package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, WrapDec, WrapInc}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

/**
 * GC 任务栈
 *
 * 模块使用片上环形栈保存活跃任务；片上任务过多时从栈底 SpillOut 到主存队列JVM queue，
 * 片上任务不足时再从主存队列JVM queue中 ReadBack。由于主存储 stack_data 是同步读 RAM，
 * 模块在栈顶设置 TopCache，为 Pop 和 PrePop 提供低延迟数据
 *
 * 数据方向：
 *   Push -> 片上栈顶；Pop/PrePop <- TopCache；
 *   SpillOut: 片上栈底 -> JVM queue；ReadBack: JVM queue -> 片上栈底。
 */
class GCTaskStack extends Module with GCTopParameters with GCParameters with HWParameters {
  val io = new Bundle {
    val toFetch        = master(new GCToFetch) // Pop and PrePop
    val toStack        = slave(new GCToStack)  // Push exclude lastPush
    val Mreq           = master(new LocalMMUIO)
    val ConfigIO       = slave(new GCTaskStackConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  // Response channel 始终保持 ready：
  //   - ReadBack response 通过 SourceID 匹配并缓存；
  //   - SpillOut 虽然 NeedResponse=False，但如果下游仍返回 write response，
  //     这里直接 drain，避免 write response 堵住后续 read response。
  io.Mreq.Response.ready := True
  io.ConfigIO.Done := False
  io.ConfigIO.config.ready := False

  // queue_bottom 使用软件/JVM 队列的逻辑索引；stack 指针使用片上环形栈索引
  val queuePtrWidth = 32
  val stackPtrWidth = log2Up(GCTaskStack_Entry)

  // stack_top 指向片上栈顶；stack_bottom 指向片上栈底边界
  // 两者相等表示片上栈为空，并牺牲一个槽位区分 full/empty
  val stack_top    = RegInit(U(0, stackPtrWidth bits))
  val stack_bottom = RegInit(U(0, stackPtrWidth bits))

  // JVM queue 元素数组基地址及当前有效元素数量/底部逻辑位置
  val queue_elems_base = RegInit(U(0, MMUAddrWidth bits))
  val queue_bottom     = RegInit(U(0, queuePtrWidth bits))

  // 片上任务主存储：同步读 RAM. Pop / PrePop 不直接读取它，而是通过 TopCache
  val stack_data = Mem(UInt(GCElementWidth bits), GCTaskStack_Entry)

  // 按物理stack entry保存PrePop标记, 物理位置被新数据覆盖时必须清零
  // 即使任务被挤出TopCache又重新Refill, 该标记仍可避免同一个任务被重复PrePop
  val prefetched = Vec.fill(GCTaskStack_Entry)(RegInit(False))

  // 环形指针辅助函数
  def stkInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, GCTaskStack_Entry, step)
  def stkDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, GCTaskStack_Entry, step)
  def queInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, GCTaskQueue_Size, step).resize(queuePtrWidth)
  def queDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, GCTaskQueue_Size, step).resize(queuePtrWidth)

  // JVM queue 的每个 GCElement 占 8 字节，将逻辑索引转换成 MMU 字节地址
  def elemAddr(idx: UInt): UInt = (queue_elems_base + (idx.resize(MMUAddrWidth) << 3)).resize(MMUAddrWidth)

  val stk_nextTop = stkInc(stack_top, U(1, stackPtrWidth bits))
  val stk_prevTop = stkDec(stack_top, U(1, stackPtrWidth bits))

  // 环形栈容量统计。保留一个槽位，因此最大可用数为 Entry-1
  val task_empty = stack_top === stack_bottom // 硬件栈队列空
  val task_usage = (stack_top - stack_bottom).resize(stackPtrWidth + 1) // 硬件栈已用项数
  val task_free  = U(GCTaskStack_Entry - 1, stackPtrWidth + 1 bits) - task_usage // 牺牲一个槽判断满

  // ReadBack 不再为所有 outstanding 提前占用 stack 空间。
  // 只在存在 outstanding 时保留 1 个 cache-line 的 commit guard，
  // 保证最老的 ReadBack response 最终一定能写回，避免 Push 把最后空间全部吃掉。
  val readbackCommitGuard = RegInit(U(0, stackPtrWidth + 1 bits))

  // 使用迟滞阈值避免 SpillOut 和 ReadBack 在边界附近来回切换
  val need_spillOut = task_usage >= U(GCTaskStack_SpillNeed + 4, task_usage.getWidth bits)
  val need_readback = (task_usage <= U(GCTaskStack_ReadNeed - 4, task_usage.getWidth bits)) && (queue_bottom =/= U(0, queuePtrWidth bits))

  // TopCache：同步 RAM 上方的栈顶缓存
  // offset 0 对应 stack_top；offset 1 对应 stack_top-1，依次向 stack_bottom 延伸。
  // topCacheIdx 保存每个缓存项对应的物理 stack_data 索引；有效项范围为
  // [0, topCacheCount)。TopCache 容量取扫描窗口的两倍，以隐藏 refill 延迟。
  val TopCacheDepth = PreFetchScanWindow << 1
  val topCacheCountWidth  = log2Up(TopCacheDepth + 1)
  val topCacheOffsetWidth = log2Up(TopCacheDepth)

  val topCacheData       = Vec.fill(TopCacheDepth)(Reg(UInt(GCElementWidth bits)))
  val topCacheIdx        = Vec.fill(TopCacheDepth)(Reg(UInt(stackPtrWidth bits)))
  val topCachePrefetched = Vec.fill(TopCacheDepth)(RegInit(False))
  val topCacheCount      = RegInit(U(0, topCacheCountWidth bits))

  def cacheOffsetValid(offset: UInt): Bool = offset.resize(topCacheCountWidth) < topCacheCount

  val topCacheEmpty = topCacheCount === U(0, topCacheCountWidth bits)
  val topCacheFull  = topCacheCount === U(TopCacheDepth, topCacheCountWidth bits)

  // Push-follow PrePop：Push burst 结束后，优先把刚 Push 的任务提供给 Fetch
  //   而普通PrePop不检查offset0 但是刚 Push 完时，希望 Fetch 尽快看到新任务，
  //   尤其是最新 Push 的栈顶任务。因此 Push-follow 模式会特殊地从 offset 0 开始
  // push_count: 记录最近一轮 Push burst 中积累的任务数量
  // not_prefetched: 仍处于Push-follow PrePop中 禁止普通PrePop
  // pushPrePopRem: 记录第一次 Push-follow PrePop 完成后，还有多少个任务需要继续 PrePop
  // pushPrePopOffset: 记录后续 Push-follow PrePop 从哪个 TopCache offset 读取
  //   第一次固定读取offset0 之后从1递增
  val push_count       = RegInit(U(0, 32 bits))
  val not_prefetch     = RegInit(False)
  val pushPrePopRem    = RegInit(U(0, 32 bits))
  val pushPrePopOffset = RegInit(U(1, topCacheOffsetWidth bits))

  // 状态机状态可见信号
  val inWork = Bool()

  // Pop / Push 接口
  // 只给 ReadBack head 保留一个 line，而不是给所有 outstanding reservation。
  val pushCanAccept = inWork && task_free > readbackCommitGuard
  io.toStack.Push.ready := pushCanAccept

  // push-follow PrePop 未处理完时禁止普通 Pop，避免新任务的观察次序混乱(Pop会让offset TopCache左移)
  val popBlockedByPushFollow = (push_count =/= U(0, 32 bits)) || (pushPrePopRem =/= U(0, 32 bits))
  val popAvailable = !topCacheEmpty // TopCache 不空 即可做Pop操作
  io.toFetch.Pop.valid := inWork && popAvailable && !popBlockedByPushFollow
  io.toFetch.Pop.payload := topCacheData(0) // 每次取bias 0 数据

  val pushFire = io.toStack.Push.fire
  val popFire  = io.toFetch.Pop.fire

  // 若本周期 Pop 消费了一个刚 Push 的任务，Fetch 看到的 PushCount 同拍减一
  val pushCountForFetch = Mux(popFire && push_count =/= U(0, 32 bits), push_count - U(1, 32 bits), push_count)
  io.toFetch.PushCount := pushCountForFetch

  // PrePop 候选选择
  // 普通模式扫描 offset 1..PreFetchScanWindow，offset 0 留给 Pop 选择离栈顶最近且从未 PrePop 的项
  // push-follow模式 按照指定的offset依次选择刚Push的任务
  val normalCandidates    = Vec(Bool(), PreFetchScanWindow) // 表示第i个候选是否有效
  val normalCandidateOffs = Vec(UInt(topCacheOffsetWidth bits), PreFetchScanWindow) // 第i个候选对应的TopCache offset
  for (i <- 1 until PreFetchScanWindow + 1) { // 在 [1, PreFetchScanWindow] 区间内扫描
    val off = U(i, topCacheOffsetWidth bits)
    // topCache entry 有效 且 没有被 PrePop过(topCache and hardware_stack)
    normalCandidates(i - 1) := cacheOffsetValid(off) && !topCachePrefetched(off) && !prefetched(topCacheIdx(off))
    normalCandidateOffs(i - 1) := off
  }
  val normalFirstOH = OHMasking.first(normalCandidates.asBits) // 从小序开始选
  val normalPrePopOffset = MuxOH(normalFirstOH, normalCandidateOffs)

  // 只要存在刚 Push 的任务，或者 Push-follow 尚未完成，就进入 Push-follow 模式
  //    第一次Push-follow PrePop pushCountForFetch != 0, offset 取 0 (完成后push_count 会 清 0)
  //    后续的Push-follow PrePop pushPrePopRem != 0, offset 取 pushPrePopoffset
  val pushFollowPrePopMode = (pushCountForFetch =/= U(0, 32 bits)) || (pushPrePopRem =/= U(0, 32 bits))
  val pushFollowOffset = Mux(pushCountForFetch =/= U(0, 32 bits), U(0, topCacheOffsetWidth bits), pushPrePopOffset)
  val prefetchOffset = Mux(pushFollowPrePopMode, pushFollowOffset, normalPrePopOffset)

  val prefetchHit = Mux(pushFollowPrePopMode, cacheOffsetValid(pushFollowOffset), normalFirstOH.orR)

  // Push/Pop 会移动缓存项，禁止与 PrePop 同拍，避免给移动前后错误的项打标记
  val prefetchBlockedByStackMove = pushFire || popFire
  io.toFetch.PrePop.valid := inWork && prefetchHit && !not_prefetch && !prefetchBlockedByStackMove
  io.toFetch.PrePop.payload := topCacheData(prefetchOffset)
  val preFire = io.toFetch.PrePop.fire

  // TopCache Refill: 当 TopCache 没满时，从同步 RAM stack_data 中读取更深的栈元素，并按顺序追加到 TopCache 尾部
  // 由于stack_data.readSync数据在下一拍才能返回, 所以refill是单级流水, 可以做到连续每周期补充一个缓存项
  //    第N拍发送refill请求 第N+1拍得到refill response并尝试加到TopCache
  val refillReq       = Bool() // 本周期是否向 stack_data 发起同步读
  val refillReqIdx    = UInt(stackPtrWidth bits) // 本周期读取的stack data索引
  val refillRespIdx   = Reg(UInt(stackPtrWidth bits)) // 本周期返回的 refillData 是否有效, 在refillReq有效时 锁存一拍refillReqIdx 这样就知道refilldata对应什么
  val refillRespValid = RegInit(False) // 本周期返回数据对应的物理栈索引

  val refillData = stack_data.readSync(refillReqIdx, refillReq) // 第二个参数是使能信号

  // 发起新的 refill request
  // 栈中有更多的数据 避免读到stack_bottom之外
  //
  // Push 同拍不发新的 refill：
  //   因为 Push 会更新 stack_top 和 TopCache 排列。
  //   下一拍再基于新的 stack_top/topCacheCount 重新计算 refill 地址。
  //
  // TopCache 满时，只有 Pop 同拍才允许预读
  // 若本拍会 append 已返回的数据，新 refill 必须再向深处多读一项，避免重复读取
  when(refillReq) {
    refillRespIdx := refillReqIdx
  }

  // readSync response valid 延迟一拍
  refillRespValid := refillReq

  val refillAppendAllowed = refillRespValid && !pushFire && (popFire || !topCacheFull) // 和refillReq有效要求差不多 都需要禁止Push改变TopCache关系 topCache不能满或者满时同周期有Pop
  val refillAppendData = refillData
  val refillAppendIdx  = refillRespIdx
  val refillAppendPrefetched = prefetched(refillRespIdx)
  val refillAppendWillHappen = refillAppendAllowed

  // refillOffset = topCacheCount + 本周期是否会 append(如果有append 会占据topCacheCount这个位置 需要往深读一个)
  val refillOffsetWide = topCacheCount.resize(stackPtrWidth + 1) + refillAppendWillHappen.asUInt.resize(stackPtrWidth + 1)
  // TopCache中的offset和stack_data对应关系是
  //   offset0 --- stack_top offset1 --- stack_top - 1 offset2 --- stack_top - 2
  // 因此refillReqIdx = stack_top - refillOffsetWide
  refillReqIdx := stkDec(stack_top, refillOffsetWide.resize(stackPtrWidth))
  val refillHasMoreData = task_usage > refillOffsetWide.resize(task_usage.getWidth)
  refillReq := inWork && !pushFire && refillHasMoreData && (popFire || !topCacheFull)

  // Push 发生时，上一拍发出的 refill response 已经不可信 直接清掉，避免旧 response 在之后被错误 append。
  when(pushFire) {
    refillRespValid := False
  }

  def dbg(msg: Seq[Any]): Unit = {
    if (DebugEnable) {
      report(Seq("[GCTaskStack<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }
  }

  // Fast path handling
  //
  // 这里统一维护：
  //   1. push_count / push-follow 状态
  //   2. TopCache shift / replace / refill append
  //   3. stack_top 更新
  def handleFastPath(): Unit = {
    // PrePop 标记
    when(preFire) {
      topCachePrefetched(prefetchOffset) := True
      prefetched(topCacheIdx(prefetchOffset)) := True

      dbg(Seq("PrePop from TopCache, offset=", prefetchOffset,
        " index=", topCacheIdx(prefetchOffset),
        " data=", topCacheData(prefetchOffset)
      ))
    }

    // push_count 从 0 开始, 每一次Push Fire都会增加 但是每一次Pop都会减少
    val pushCountAfterPush = push_count + pushFire.asUInt.resize(32)
    val pushCountAfterPop = Mux(
      popFire && push_count =/= U(0, 32 bits),
      pushCountAfterPush - U(1, 32 bits),
      pushCountAfterPush
    )

    push_count := pushCountAfterPop

    // Push 时 禁止PrePop
    when(pushFire) {
      not_prefetch := True
    }

    // LastPush 表示Push已完 可以重新PrePop
    when(io.toStack.LastPush) {
      not_prefetch := False
    }

    when(preFire) {
      // 第一次Push-follow PrePop
      when(pushCountForFetch =/= U(0, 32 bits)) {
        val takeNum = Mux(
          pushCountForFetch > U(PreFetchBufferNum, 32 bits),
          U(PreFetchBufferNum, 32 bits),
          pushCountForFetch
        )

        push_count       := U(0, 32 bits)
        pushPrePopRem    := takeNum - U(1, 32 bits)
        pushPrePopOffset := U(1, topCacheOffsetWidth bits)

      }.elsewhen(pushPrePopRem =/= U(0, 32 bits)) {
        pushPrePopRem    := pushPrePopRem - U(1, 32 bits)
        pushPrePopOffset := pushPrePopOffset + U(1, topCacheOffsetWidth bits)
      }
    }

    // TopCache update
    when(pushFire && popFire) {
      // Push + Pop 同拍： Pop 返回旧 topCacheData(0) Push 写入当前 stack_top 位置，成为新的 top
      // stack_top 不变，topCacheCount 不变
      stack_data.write(stack_top, io.toStack.Push.payload)

      topCacheData(0)       := io.toStack.Push.payload
      topCacheIdx(0)        := stack_top
      topCachePrefetched(0) := False
      prefetched(stack_top) := False

      dbg(Seq("Push+Pop replace top, index=", stack_top, " push=", io.toStack.Push.payload, " pop=", topCacheData(0)))

    }.elsewhen(pushFire) {
      // Push only： 写入 stack_top + 1 TopCache 整体右移，新数据插入 offset 0
      // 如果 TopCache 已满，最后一个 entry 会被挤出 PushFire时不存在TopCacheRefill
      val pushIndex = stk_nextTop
      stack_data.write(pushIndex, io.toStack.Push.payload)

      for (i <- TopCacheDepth - 1 downto 1) {
        topCacheData(i)       := topCacheData(i - 1)
        topCacheIdx(i)        := topCacheIdx(i - 1)
        topCachePrefetched(i) := topCachePrefetched(i - 1)
      }

      topCacheData(0)       := io.toStack.Push.payload
      topCacheIdx(0)        := pushIndex
      topCachePrefetched(0) := False
      prefetched(pushIndex) := False

      when(!topCacheFull) {
        topCacheCount := topCacheCount + U(1, topCacheCountWidth bits)
      }

      stack_top := pushIndex

      dbg(Seq("Push TopCache, index=", pushIndex, " data=", io.toStack.Push.payload))

    }.elsewhen(popFire) {
      // Pop only： 消费 offset 0 TopCache 左移
      // 如果本周期刚好有 refill response 可 append，则插到尾部
      // 没有 refill response 时，topCacheCount 减 1
      for (i <- 0 until TopCacheDepth - 1) {
        topCacheData(i)       := topCacheData(i + 1)
        topCacheIdx(i)        := topCacheIdx(i + 1)
        topCachePrefetched(i) := topCachePrefetched(i + 1)
      }

      topCachePrefetched(TopCacheDepth - 1) := False

      when(refillAppendAllowed) {
        val insertOff = (topCacheCount - U(1, topCacheCountWidth bits)).resize(topCacheOffsetWidth)

        topCacheData(insertOff)       := refillAppendData
        topCacheIdx(insertOff)        := refillAppendIdx
        topCachePrefetched(insertOff) := refillAppendPrefetched

        // Pop 减 1，refill append 加 1，净效果 count 不变
        topCacheCount := topCacheCount

        dbg(Seq("Pop with refill append, popIndex=", stack_top, " appendIndex=", refillAppendIdx, " appendData=", refillAppendData))

      }.otherwise {
        when(!topCacheEmpty) {
          topCacheCount := topCacheCount - U(1, topCacheCountWidth bits)
        }
        dbg(Seq("Pop TopCache, index=", stack_top, " data=", topCacheData(0)))
      }

      stack_top := stk_prevTop
    }.otherwise {
      // 无 Push / Pop： 如果有 refill response，并且 TopCache 未满，则 append 到尾部。
      // 如果 TopCache 已满，response 会被丢弃，不会 hold 这是为了避免旧 response 在 Push 后错序插入
      when(refillAppendAllowed) {
        val insertOff = topCacheCount.resize(topCacheOffsetWidth)

        topCacheData(insertOff)       := refillAppendData
        topCacheIdx(insertOff)        := refillAppendIdx
        topCachePrefetched(insertOff) := refillAppendPrefetched
        topCacheCount                 := topCacheCount + U(1, topCacheCountWidth bits)

        dbg(Seq("TopCache refill append, offset=", insertOff, " index=", refillAppendIdx, " data=", refillAppendData))
      }
    }
  }

  // ReadBack 独立发射游标：
  // formal queue_bottom 只在按 request 顺序 commit 时更新；
  // readbackQueueCursor 则随着连续 issue 向前推进。
  val readbackQueueCursor = RegInit(U(0, queuePtrWidth bits))

  val QueueElemBytes    = GCElementWidth / 8
  val QueueElemsPerLine = LineBytesNum / QueueElemBytes
  val QueueElemShift    = log2Up(QueueElemBytes)
  val LineReqNumWidth   = log2Up(QueueElemsPerLine + 1)

  // SpillOut Area:
  //   1. stack_data 是同步读 RAM，因此先连续发起 RAM read；
  //   2. RAM response 进入 2-entry skid buffer；
  //   3. Mreq 每拍最多发送一个完整、Line 对齐的写请求；
  //   4. NeedResponse=False，请求 fire 后立即提交 stack_bottom / queue_bottom。
  //
  // 所有写请求地址都按 cache line 对齐；不足一整行的部分用 RequestWStrb 屏蔽，
  // 因而不再需要 GCUnalignedMMUAdapter。
  val spillOutArea = new Area {
    val BufferDepth   = 2
    val BufPtrWidth   = log2Up(BufferDepth)
    val BufCountWidth = log2Up(BufferDepth + 1)

    // 从 stack_data 发出的同步读，下一拍返回。
    val readPending       = RegInit(False)
    val readLineAddr      = Reg(UInt(MMUAddrWidth bits))
    val readByteOffset    = Reg(UInt(log2Up(LineBytesNum) bits))
    val readReqNum        = Reg(UInt(LineReqNumWidth bits))

    // 两级 skid buffer，吸收 Mreq.Request.ready 的短暂 back-pressure。
    val bufHead  = RegInit(U(0, BufPtrWidth bits))
    val bufTail  = RegInit(U(0, BufPtrWidth bits))
    val bufCount = RegInit(U(0, BufCountWidth bits))

    val bufAddr   = Vec.fill(BufferDepth)(Reg(UInt(MMUAddrWidth bits)))
    val bufData   = Vec.fill(BufferDepth)(Reg(UInt(MMUDataWidth bits)))
    val bufMask   = Vec.fill(BufferDepth)(Reg(UInt(LineBytesNum bits)))
    val bufReqNum = Vec.fill(BufferDepth)(Reg(UInt(LineReqNumWidth bits)))

    // 已经从 stack_data 预留、但尚未真正发到 Mreq 的元素数。
    // formal stack_bottom / queue_bottom 只在 write request fire 时推进。
    val reserved = RegInit(U(0, stackPtrWidth + 1 bits))

    def busy: Bool = readPending || (bufCount =/= 0) || (reserved =/= 0)

    def nextBufPtr(ptr: UInt): UInt = (ptr + U(1, BufPtrWidth bits)).resized

    def run(): Unit = {
      // ---------------- Mreq write output ----------------
      val outValid  = bufCount =/= 0
      val outReqNum = bufReqNum(bufHead)

      io.Mreq.Request.valid := outValid
      when(outValid) {
        io.Mreq.Request.payload.RequestVirtualAddr := bufAddr(bufHead)
        io.Mreq.Request.payload.RequestSourceID := io.Mreq.ConherentRequsetSourceID.payload
        io.Mreq.Request.payload.RequestType_isWrite := True
        io.Mreq.Request.payload.RequestData := bufData(bufHead)
        io.Mreq.Request.payload.RequestWStrb := bufMask(bufHead)
        io.Mreq.Request.payload.RequestSize := U(LineBytesNum, LineBytesNumBitSize bits)
        io.Mreq.Request.payload.NeedResponse := False
        io.Mreq.Request.payload.NeedDoCmpxChg := False
      }

      val sendFire = io.Mreq.Request.fire && outValid

      // ---------------- stack_data read launch ----------------
      // formal pointer + reserved = 下一批尚未预留的数据起点。
      val reserveQueueBottom = queInc(queue_bottom, reserved.resize(queuePtrWidth))
      val reserveStackBottom = stkInc(stack_bottom, reserved.resize(stackPtrWidth))

      // Spill 触发后持续搬到 GCTaskStack_SpillNeed 附近；
      // reserved 计入预测值，避免 pipeline 继续超发。
      // 形成真正的 burst：
      // need_spillOut 在 SpillNeed+4 才触发；一旦触发，不只搬回 SpillNeed，
      // 而是继续搬到更低的 low watermark。这样至少可以连续形成多条 line write，
      // 避免“为了支持流水加了很多状态，最后每次仍只发 1 个 request”。
      val SpillBurstLines = 2
      val spillLowWatermarkScala =
        Math.min(
          GCTaskStack_SpillNeed,
          Math.max(
            GCTaskStack_ReadNeed + QueueElemsPerLine,
            GCTaskStack_SpillNeed - QueueElemsPerLine * SpillBurstLines
          )
        )

      // 把本拍 Push / Pop 的影响也计入，避免基于旧 task_usage 多发或少发一项。
      val usageAfterFastWide =
        task_usage.resize(task_usage.getWidth + 1) +
          pushFire.asUInt.resize(task_usage.getWidth + 1) -
          popFire.asUInt.resize(task_usage.getWidth + 1)

      val projectedUsageWide = Mux(
        usageAfterFastWide >= reserved.resize(usageAfterFastWide.getWidth),
        usageAfterFastWide - reserved.resize(usageAfterFastWide.getWidth),
        U(0, usageAfterFastWide.getWidth bits)
      )

      val spillTarget =
        U(spillLowWatermarkScala, usageAfterFastWide.getWidth bits)

      val spillBudgetWide = Mux(
        projectedUsageWide > spillTarget,
        projectedUsageWide - spillTarget,
        U(0, usageAfterFastWide.getWidth bits)
      )
      val spillBudget = spillBudgetWide.resize(task_usage.getWidth)

      val writeAddr = elemAddr(reserveQueueBottom)
      val elemOffsetInLine =
        (writeAddr(log2Up(LineBytesNum) - 1 downto 0) >> QueueElemShift)
          .resize(task_usage.getWidth)
      val remainingInLine =
        U(QueueElemsPerLine, task_usage.getWidth bits) - elemOffsetInLine

      val reqNumWide = Mux(spillBudget < remainingInLine, spillBudget, remainingInLine)
      val reqNum     = reqNumWide.resize(LineReqNumWidth)

      // 当前拍先考虑：
      //   - 已有 RAM response 入 buffer；
      //   - 当前 buffer head 被 Mreq 消费。
      // 只有处理完这两个动作后 buffer 仍至少空一个 slot，才继续发下一次同步 RAM read。
      val pendingArrive = readPending
      val bufCountAfterCurrent =
        bufCount.resize(BufCountWidth + 1) +
          pendingArrive.asUInt.resize(BufCountWidth + 1) -
          sendFire.asUInt.resize(BufCountWidth + 1)

      val bufferHasCredit =
        bufCountAfterCurrent < U(BufferDepth, BufCountWidth + 1 bits)

      val launchRead = bufferHasCredit && reqNum =/= 0

      val spillPtrs = Vec((0 until QueueElemsPerLine).map(i =>
        stkInc(reserveStackBottom, U(i + 1, stackPtrWidth bits))))
      val spillData = Vec((0 until QueueElemsPerLine).map(i =>
        stack_data.readSync(spillPtrs(i), launchRead)))

      // 本拍的 readPending 对应上一拍发出的 stack_data read。
      val pendingPacked = Cat(spillData.reverse).asUInt.resize(MMUDataWidth)
      val pendingByteCount =
        (readReqNum.resize(LineBytesNumBitSize) << QueueElemShift).resize(LineBytesNumBitSize)
      val pendingByteOffset = readByteOffset.resize(LineBytesNumBitSize)

      val pendingWriteData =
        (pendingPacked |<< (pendingByteOffset << 3)).resize(MMUDataWidth)

      val pendingWriteMask = UInt(LineBytesNum bits)
      pendingWriteMask := 0
      for (b <- 0 until LineBytesNum) {
        pendingWriteMask(b) :=
          U(b, LineBytesNumBitSize bits) >= pendingByteOffset &&
            U(b, LineBytesNumBitSize bits) < pendingByteOffset + pendingByteCount
      }

      // RAM response 入 skid buffer。
      when(pendingArrive) {
        bufAddr(bufTail)   := readLineAddr
        bufData(bufTail)   := pendingWriteData
        bufMask(bufTail)   := pendingWriteMask
        bufReqNum(bufTail) := readReqNum
        bufTail            := nextBufPtr(bufTail)
      }

      // Mreq write request 消费 skid buffer head。
      when(sendFire) {
        bufHead := nextBufPtr(bufHead)

        stack_bottom := stkInc(stack_bottom, outReqNum.resize(stackPtrWidth))
        queue_bottom := queInc(queue_bottom, outReqNum.resize(queuePtrWidth))

        dbg(Seq(
          "SpillOut write fire, moveNum=", outReqNum,
          " new queue_bottom=", queInc(queue_bottom, outReqNum.resize(queuePtrWidth))
        ))
      }

      when(pendingArrive && !sendFire) {
        bufCount := bufCount + U(1, BufCountWidth bits)
      }.elsewhen(!pendingArrive && sendFire) {
        bufCount := bufCount - U(1, BufCountWidth bits)
      }

      // 发起下一拍 stack_data read，并锁存这次 read 的元数据。
      readPending := launchRead
      when(launchRead) {
        readLineAddr   := writeAddr & ~U(LineBytesNum - 1, MMUAddrWidth bits)
        readByteOffset := writeAddr(log2Up(LineBytesNum) - 1 downto 0)
        readReqNum     := reqNum
      }

      // reservation 在 RAM read launch 时增加，在真正 write fire 时释放。
      val launchNum = Mux(
        launchRead,
        reqNum.resize(reserved.getWidth),
        U(0, reserved.getWidth bits)
      )
      val sentNum = Mux(
        sendFire,
        outReqNum.resize(reserved.getWidth),
        U(0, reserved.getWidth bits)
      )
      when(launchRead || sendFire) {
        reserved := reserved + launchNum - sentNum
      }

      // SpillOut 使用 NeedResponse=False；不等待、不匹配任何 write response。
    }

    def clear(): Unit = {
      readPending := False
      bufHead     := 0
      bufTail     := 0
      bufCount    := 0
      reserved    := 0
    }
  }

  // ReadBack Area:
  //
  // v5 性能结构：
  //   1. Read request 可以连续 issue，最多 4 outstanding；
  //   2. request 不预占 stack，只占本地 ROB slot；
  //   3. OOO response 通过 SourceID 写入对应 slotRespData；
  //   4. head slot 按 request 顺序 commit，commit 时才真正占 stack；
  //   5. 只保留一个 cache-line commit guard，显著减少对 Push 的阻塞；
  //   6. response capture / head commit / new issue 三条控制解耦；
  //   7. 当本地 stack 和 TopCache 都空时，ReadBack commit 同拍直接 seed TopCache，
  //      省掉“MMU -> stack RAM -> readSync -> TopCache”的额外 refill 延迟。
  val readBackArea = new Area {
    val EntryNum         = Math.min(4, LLCSourceMaxNum)
    val SlotWidth        = Math.max(1, log2Up(EntryNum))
    val OutstandingWidth = log2Up(EntryNum + 1)

    val slotValid  = Vec.fill(EntryNum)(RegInit(False))
    val slotDone   = Vec.fill(EntryNum)(RegInit(False))
    val slotReqNum = Vec.fill(EntryNum)(RegInit(U(0, LineReqNumWidth bits)))
    val slotLane   = Vec.fill(EntryNum)(RegInit(U(0, LineReqNumWidth bits)))

    // OOO response 完整缓存。response 到达时不抢 stack_data write port。
    val slotRespData =
      Vec.fill(EntryNum)(Reg(UInt(MMUDataWidth bits)))

    // ResponseSourceID -> request slot
    val sourceIdToSlot =
      Reg(Vec(Seq.fill(LLCSourceMaxNum)(U(0, SlotWidth bits))))
    val sourceIdValid =
      Vec.fill(LLCSourceMaxNum)(RegInit(False))

    val head        = RegInit(U(0, SlotWidth bits))
    val tail        = RegInit(U(0, SlotWidth bits))
    val outstanding = RegInit(U(0, OutstandingWidth bits))

    // 只用于控制“还要不要继续预取”，不限制 Push。
    // 它表示已经 issue、但尚未 commit 的 element 总数。
    val inflightElems =
      RegInit(U(0, stackPtrWidth + 2 bits))

    // ReadBack burst 状态只控制“是否继续发新请求”。
    // 即使 burstActive=False，已有 response 仍会继续 capture / commit。
    val burstActive = RegInit(False)

    // Critical-first ReadBack：
    // 当本地 task / TopCache 已经饥饿时，burst 的第一条 ReadBack 是 demand request。
    // 在它的 response 返回前，暂停本模块后续 prefetch read；
    // critical response 返回当拍即可重新放行后续 request。
    val criticalPending = RegInit(False)

    // Area 外部性能计数器使用的事件信号。
    val commitEvent        = Bool()
    val commitBlockedEvent = Bool()
    val topCacheSeedEvent  = Bool()
    val criticalStartEvent = Bool()
    val criticalRespEvent  = Bool()

    commitEvent        := False
    commitBlockedEvent := False
    topCacheSeedEvent  := False
    criticalStartEvent := False
    criticalRespEvent  := False

    def busy: Bool = outstanding =/= 0

    def nextSlot(ptr: UInt): UInt =
      WrapInc(ptr, EntryNum, U(1, SlotWidth bits))

    def calcReqNum(cursor: UInt): UInt = {
      val queueAddr   = elemAddr(cursor)
      val queueOffset =
        queueAddr(log2Up(LineBytesNum) - 1 downto 0)
      val queueLane =
        (queueOffset >> QueueElemShift).resize(LineReqNumWidth)

      val maxInLine = Mux(
        queueLane =/= 0,
        queueLane,
        U(QueueElemsPerLine, LineReqNumWidth bits)
      )

      Mux(
        cursor < maxInLine.resize(queuePtrWidth),
        cursor.resize(LineReqNumWidth),
        maxInLine
      )
    }

    // allowIssue 只控制 request channel。
    // response / commit 每个 WORK 周期都运行。
    def run(allowIssue: Bool): Unit = {
      // ================================================================
      // 1. OOO response capture
      // ================================================================
      val responseSource =
        io.Mreq.Response.payload.ResponseSourceID.resized
      val responseSlot =
        sourceIdToSlot(responseSource)

      // 模块顶层 Response.ready 恒为 True。
      // 只有 sourceIdValid 命中的 response 才属于当前 ReadBack；
      // 其它 response（例如 no-response write 仍被下游返回）直接 drain。
      val responseReadHit =
        io.Mreq.Response.valid &&
          sourceIdValid(responseSource) &&
          slotValid(responseSlot) &&
          !slotDone(responseSlot)

      val responseFire =
        io.Mreq.Response.fire && responseReadHit

      val responseIsHead =
        responseFire && responseSlot === head

      // critical request 是 starvation burst 的第一条 request，也就是当前 head。
      // response 返回当拍组合地解除后续 issue 阻塞。
      val criticalRespNow =
        criticalPending && responseIsHead

      criticalRespEvent := criticalRespNow

      // ================================================================
      // 2. Head commit
      // ================================================================
      val commitSlot   = head
      val commitReqNum = slotReqNum(commitSlot)
      val commitLane   = slotLane(commitSlot)

      // Head response 当拍到达可直接 bypass，省掉一拍 slot buffer latency。
      val headBypass =
        responseIsHead && !slotDone(commitSlot)

      val headDataReady =
        slotValid(commitSlot) &&
          (slotDone(commitSlot) || headBypass)

      // Push 可能写 stack_data，因此 commit 与 Push 错开。
      // Pop 不写 stack_data，可以与 ReadBack commit 同拍；
      // Pop 还会额外释放一个 stack entry。
      val commitFreeWide =
        task_free.resize(task_free.getWidth + 1) +
          popFire.asUInt.resize(task_free.getWidth + 1)

      val commitHasSpace =
        commitFreeWide >= commitReqNum.resize(commitFreeWide.getWidth)

      val commitFire =
        headDataReady &&
          commitHasSpace &&
          !pushFire

      val commitData =
        Mux(
          headBypass,
          io.Mreq.Response.payload.ResponseData,
          slotRespData(commitSlot)
        )

      val canSeedTopCache =
        task_empty &&
          topCacheEmpty &&
          !refillRespValid &&
          !pushFire

      commitEvent        := commitFire
      commitBlockedEvent := headDataReady && !commitFire
      topCacheSeedEvent  := commitFire && canSeedTopCache

      // 非 head response 一律先缓存。
      // head response 如果本拍不能 commit（例如刚好 Push），也必须缓存，不能丢。
      when(responseFire) {
        sourceIdValid(responseSource) := False

        when(!headBypass || !commitFire) {
          slotRespData(responseSlot) := io.Mreq.Response.payload.ResponseData
          slotDone(responseSlot)     := True
        }

        dbg(Seq(
          "ReadBack response, slot=", responseSlot,
          " isHead=", responseIsHead,
          " commitNow=", commitFire
        ))
      }

      when(commitFire) {
        val elems =
          commitData.subdivideIn(GCElementWidth bits)

        // commit 时才决定物理 stack 位置。
        // 因为 request/response 顺序由 head 保证，所以完全不需要 issue 时预留 stackBase。
        for (i <- 0 until QueueElemsPerLine) {
          when(U(i, LineReqNumWidth bits) < commitReqNum) {
            val lane =
              commitLane + commitReqNum -
                U(i + 1, LineReqNumWidth bits)

            val wrPtr =
              stkDec(stack_bottom, U(i, stackPtrWidth bits))

            stack_data.write(wrPtr, elems(lane.resized))
            prefetched(wrPtr) := False
          }
        }

        // ----------------------------------------------------------------
        // Fast empty-stack bypass:
        // 当 ReadBack 前本地完全空时，这批数据就是新的栈顶数据。
        // 同拍直接灌入 TopCache，下一拍 Fetch 就能看到，不必再走 stack_data.readSync refill。
        // ----------------------------------------------------------------
        when(canSeedTopCache) {
          val seedCountWide =
            Mux(
              commitReqNum.resize(topCacheCountWidth) >
                U(TopCacheDepth, topCacheCountWidth bits),
              U(TopCacheDepth, topCacheCountWidth bits),
              commitReqNum.resize(topCacheCountWidth)
            )

          topCacheCount := seedCountWide

          val SeedMax = Math.min(TopCacheDepth, QueueElemsPerLine)
          for (i <- 0 until SeedMax) {
            when(U(i, LineReqNumWidth bits) < commitReqNum) {
              val lane =
                commitLane + commitReqNum -
                  U(i + 1, LineReqNumWidth bits)
              val cacheIdx =
                stkDec(stack_top, U(i, stackPtrWidth bits))

              topCacheData(i)       := elems(lane.resized)
              topCacheIdx(i)        := cacheIdx
              topCachePrefetched(i) := False
            }
          }

          dbg(Seq(
            "ReadBack seed TopCache, count=", seedCountWide
          ))
        }

        stack_bottom :=
          stkDec(stack_bottom, commitReqNum.resize(stackPtrWidth))
        queue_bottom :=
          queDec(queue_bottom, commitReqNum.resize(queuePtrWidth))

        slotValid(commitSlot) := False
        slotDone(commitSlot)  := False
        head := nextSlot(head)

        dbg(Seq(
          "ReadBack commit, slot=", commitSlot,
          " lane=", commitLane,
          " moveNum=", commitReqNum,
          " new queue_bottom=",
          queDec(queue_bottom, commitReqNum.resize(queuePtrWidth))
        ))
      }

      // ================================================================
      // 3. Adaptive burst issue
      // ================================================================
      val issueCursor =
        Mux(outstanding === 0, queue_bottom, readbackQueueCursor)

      val naturalReqNum =
        calcReqNum(issueCursor)

      // 一次触发后允许预取到 high watermark。
      // inflightElems 只参与“是否继续 issue”，不再从 task_free 中扣掉。
      val ReadBackBurstLines = 3
      val readHighWatermarkScala =
        Math.max(
          GCTaskStack_ReadNeed,
          Math.min(
            GCTaskStack_SpillNeed - QueueElemsPerLine,
            GCTaskStack_ReadNeed + QueueElemsPerLine * ReadBackBurstLines
          )
        )

      val usageAfterFastWide =
        task_usage.resize(task_usage.getWidth + 2) +
          pushFire.asUInt.resize(task_usage.getWidth + 2) -
          popFire.asUInt.resize(task_usage.getWidth + 2)

      val projectedUsageWide =
        usageAfterFastWide +
          inflightElems.resize(usageAfterFastWide.getWidth)

      val readTarget =
        U(readHighWatermarkScala, projectedUsageWide.getWidth bits)

      val roomToTargetWide = Mux(
        projectedUsageWide < readTarget,
        readTarget - projectedUsageWide,
        U(0, projectedUsageWide.getWidth bits)
      )

      // need_readback 触发本轮 burst；之后即使 formal usage 越过 low watermark，
      // 仍可继续 issue 到 high watermark。
      val issueMode =
        burstActive || need_readback

      when(need_readback) {
        burstActive := True
      }

      // 如果 workload 反向增长到 Spill 区，立即停止发新的 ReadBack；
      // 但已有 outstanding 仍继续 response/commit。
      when(
        projectedUsageWide >= readTarget ||
          issueCursor === 0 ||
          need_spillOut
      ) {
        burstActive := False
      }

      val issueReqNum =
        Mux(
          naturalReqNum.resize(roomToTargetWide.getWidth) >
            roomToTargetWide,
          roomToTargetWide.resize(LineReqNumWidth),
          naturalReqNum
        )

      val issueReadIdx =
        queDec(issueCursor, issueReqNum.resize(queuePtrWidth))

      val issueReadAddr =
        elemAddr(issueReadIdx)

      val issueLineAddr =
        issueReadAddr & ~U(LineBytesNum - 1, MMUAddrWidth bits)

      val issueReadLane =
        (issueReadAddr(log2Up(LineBytesNum) - 1 downto 0) >>
          QueueElemShift).resize(LineReqNumWidth)

      val slotHasSpace =
        outstanding < U(EntryNum, OutstandingWidth bits) ||
          commitFire

      // burst 的第一条 request 建立时，必须确保本拍 Push/Pop 之后仍能留下
      // 一个完整 cache-line 的 commit guard。后续 outstanding request 只占 ROB，
      // 不再额外扣 task_free。
      val freeAfterFastWide =
        task_free.resize(task_free.getWidth + 1) +
          popFire.asUInt.resize(task_free.getWidth + 1) -
          pushFire.asUInt.resize(task_free.getWidth + 1)

      val firstIssueHasGuard =
        outstanding =/= 0 ||
          freeAfterFastWide >=
            U(QueueElemsPerLine, freeAfterFastWide.getWidth bits)

      // starvation 时第一条 demand read 尚未返回，则暂停本模块后续 prefetch。
      // criticalRespNow 当拍即可恢复 issue：
      // 可以做到 Resp(critical) 与下一条 Req 同拍 fire。
      val criticalBlocksIssue =
        criticalPending && !criticalRespNow

      val canIssue =
        allowIssue &&
          issueMode &&
          issueCursor =/= 0 &&
          issueReqNum =/= 0 &&
          slotHasSpace &&
          firstIssueHasGuard &&
          !criticalBlocksIssue

      io.Mreq.Request.valid := canIssue

      when(canIssue) {
        io.Mreq.Request.payload.RequestVirtualAddr := issueLineAddr
        io.Mreq.Request.payload.RequestSourceID :=
          io.Mreq.ConherentRequsetSourceID.payload
        io.Mreq.Request.payload.RequestType_isWrite := False
        io.Mreq.Request.payload.RequestData := 0
        io.Mreq.Request.payload.RequestWStrb := 0
        io.Mreq.Request.payload.RequestSize :=
          U(LineBytesNum, LineBytesNumBitSize bits)
        io.Mreq.Request.payload.NeedResponse := True
        io.Mreq.Request.payload.NeedDoCmpxChg := False
      }

      val requestFire =
        io.Mreq.Request.fire && canIssue

      when(requestFire) {
        val issueSlot =
          tail
        val sourceId =
          io.Mreq.ConherentRequsetSourceID.payload.resized

        slotValid(issueSlot)  := True
        slotDone(issueSlot)   := False
        slotReqNum(issueSlot) := issueReqNum
        slotLane(issueSlot)   := issueReadLane

        sourceIdToSlot(sourceId) := issueSlot
        sourceIdValid(sourceId)  := True

        tail := nextSlot(tail)

        readbackQueueCursor :=
          issueReadIdx

        // 只有从 0 outstanding 启动，且本地供给已经饥饿时，
        // 才把第一条 request 标成 critical。
        // 正常后台 ReadBack 仍保持最多 4 outstanding。
        when(
          outstanding === 0 &&
            (task_empty || topCacheEmpty)
        ) {
          criticalPending  := True
          criticalStartEvent := True
        }

        dbg(Seq(
          "ReadBack request, slot=", issueSlot,
          " alignedAddr=", issueLineAddr,
          " actualAddr=", issueReadAddr,
          " lane=", issueReadLane,
          " moveNum=", issueReqNum,
          " outstanding=", outstanding,
          " critical=", outstanding === 0 && (task_empty || topCacheEmpty)
        ))
      }

      when(criticalRespNow) {
        criticalPending := False
      }

      // ================================================================
      // 4. Outstanding / inflight / one-line guard bookkeeping
      // ================================================================
      when(requestFire && !commitFire) {
        outstanding :=
          outstanding + U(1, OutstandingWidth bits)
      }.elsewhen(!requestFire && commitFire) {
        outstanding :=
          outstanding - U(1, OutstandingWidth bits)
      }

      val issueNum =
        Mux(
          requestFire,
          issueReqNum.resize(inflightElems.getWidth),
          U(0, inflightElems.getWidth bits)
        )

      val commitNum =
        Mux(
          commitFire,
          commitReqNum.resize(inflightElems.getWidth),
          U(0, inflightElems.getWidth bits)
        )

      when(requestFire || commitFire) {
        inflightElems :=
          inflightElems + issueNum - commitNum
      }

      // 只要还有至少一个 ReadBack request 未 commit，就保留一个完整 line 的空间。
      // ring 从 0->1 时建立 guard；最后一个 commit 后释放。
      when(requestFire && outstanding === 0 && !commitFire) {
        readbackCommitGuard :=
          U(QueueElemsPerLine, readbackCommitGuard.getWidth bits)
      }

      when(commitFire && !requestFire && outstanding === 1) {
        readbackCommitGuard := 0
      }

      // outstanding=1 且 commit/request 同拍：旧 head 退出，新 request 进入，
      // guard 保持一个 line，不需要修改。
      when(requestFire && commitFire && outstanding === 0) {
        readbackCommitGuard :=
          U(QueueElemsPerLine, readbackCommitGuard.getWidth bits)
      }
    }

    def clear(): Unit = {
      head                := 0
      tail                := 0
      outstanding         := 0
      inflightElems       := 0
      burstActive         := False
      criticalPending     := False
      readbackCommitGuard := 0

      for (i <- 0 until EntryNum) {
        slotValid(i)    := False
        slotDone(i)     := False
        slotReqNum(i)   := 0
        slotLane(i)     := 0
        slotRespData(i) := 0
      }

      for (i <- 0 until LLCSourceMaxNum) {
        sourceIdValid(i)  := False
        sourceIdToSlot(i) := 0
      }
    }
  }

  // ------------------------------------------------------------------
  // Performance observability
  // ------------------------------------------------------------------
  // 可直接从波形/Verilog 观察：
  //   reqUtil = perfMemReqFireCycles / perfWorkCycles
  //   perfMaxOutstanding：ReadBack MLP
  //   perfCriticalReadStarts / perfCriticalReadResps：
  //     critical-first ReadBack 的触发 / 完成次数
  val perfWorkCycles       = RegInit(U(0, 64 bits))
  val perfMemReqFireCycles = RegInit(U(0, 64 bits))
  val perfReadReqs         = RegInit(U(0, 64 bits))
  val perfWriteReqs        = RegInit(U(0, 64 bits))
  val perfResponses        = RegInit(U(0, 64 bits))
  val perfPushGuardStall   = RegInit(U(0, 64 bits))
  val perfReadCommitBlocked = RegInit(U(0, 64 bits))
  val perfTopCacheSeed       = RegInit(U(0, 64 bits))
  val perfCriticalReadStarts = RegInit(U(0, 64 bits))
  val perfCriticalReadResps  = RegInit(U(0, 64 bits))
  val perfMaxOutstanding =
    RegInit(U(0, readBackArea.OutstandingWidth bits))

  when(inWork) {
    perfWorkCycles := perfWorkCycles + 1

    when(io.Mreq.Request.fire) {
      perfMemReqFireCycles := perfMemReqFireCycles + 1

      when(io.Mreq.Request.payload.RequestType_isWrite) {
        perfWriteReqs := perfWriteReqs + 1
      }.otherwise {
        perfReadReqs := perfReadReqs + 1
      }
    }

    when(io.Mreq.Response.fire) {
      perfResponses := perfResponses + 1
    }

    when(
      io.toStack.Push.valid &&
        !io.toStack.Push.ready &&
        task_free =/= 0 &&
        task_free <= readbackCommitGuard
    ) {
      perfPushGuardStall := perfPushGuardStall + 1
    }

    when(readBackArea.commitBlockedEvent) {
      perfReadCommitBlocked := perfReadCommitBlocked + 1
    }

    when(readBackArea.topCacheSeedEvent) {
      perfTopCacheSeed := perfTopCacheSeed + 1
    }

    when(readBackArea.criticalStartEvent) {
      perfCriticalReadStarts := perfCriticalReadStarts + 1
    }

    when(readBackArea.criticalRespEvent) {
      perfCriticalReadResps := perfCriticalReadResps + 1
    }

    when(readBackArea.outstanding > perfMaxOutstanding) {
      perfMaxOutstanding := readBackArea.outstanding
    }
  }

  // Task exhausted
  // 注意 TopCache / refill response 也要算进去。 否则可能 stack_top == stack_bottom 时提前结束。
  val task_exhausted = task_empty && queue_bottom === U(0) && topCacheCount === U(0) &&
    !refillRespValid && push_count === U(0) && pushPrePopRem === U(0) &&
    !spillOutArea.busy && !readBackArea.busy

  // FSM
  val fsm = new StateMachine {
    val IDLE: State         = new State with EntryPoint
    val WORK: State         = new State

    inWork := isActive(WORK)

    always {
      when(isEntering(WORK) && isExiting(IDLE)) {
        queue_bottom     := io.ConfigIO.config.payload.TaskQueue_Bottom
        queue_elems_base := io.ConfigIO.config.payload.TaskQueue_ElemsBase.resize(MMUAddrWidth)

        dbg(Seq(
          "Config JVM Queue, Bottom=", io.ConfigIO.config.payload.TaskQueue_Bottom,
          " ElemsBase=", io.ConfigIO.config.payload.TaskQueue_ElemsBase
        ))
      }

      when(isEntering(IDLE) && isExiting(WORK)) {
        io.ConfigIO.Done := True
      }
    }

    IDLE.whenIsActive {
      io.ConfigIO.config.ready := True

      for (i <- 0 until GCTaskStack_Entry) {
        prefetched(i) := False
      }

      for (i <- 0 until TopCacheDepth) {
        topCachePrefetched(i) := False
      }

      stack_top    := U(0, stackPtrWidth bits)
      stack_bottom := U(0, stackPtrWidth bits)
      queue_bottom := U(0, queuePtrWidth bits)
      readbackQueueCursor := U(0, queuePtrWidth bits)

      perfWorkCycles       := 0
      perfMemReqFireCycles := 0
      perfReadReqs         := 0
      perfWriteReqs        := 0
      perfResponses        := 0
      perfPushGuardStall   := 0
      perfReadCommitBlocked := 0
      perfTopCacheSeed       := 0
      perfCriticalReadStarts := 0
      perfCriticalReadResps  := 0
      perfMaxOutstanding     := 0

      topCacheCount := U(0, topCacheCountWidth bits)

      push_count       := U(0, 32 bits)
      not_prefetch     := False
      pushPrePopRem    := U(0, 32 bits)
      pushPrePopOffset := U(1, topCacheOffsetWidth bits)

      refillRespValid := False

      spillOutArea.clear()
      readBackArea.clear()

      when(io.ConfigIO.config.fire) {
        goto(WORK)
      }
    }

    WORK.whenIsActive {
      when(!task_exhausted || !io.toFetch.Pop.ready) {
        handleFastPath()

        // ReadBack 的 response capture / head commit 每拍都运行。
        // 是否继续发新的 Read request 单独由 allowReadIssue 控制。
        //
        // queue_bottom 同时被 SpillOut(+方向)和 ReadBack commit(-方向)维护，
        // 因此 ReadBack 仍有 outstanding 时不能启动 SpillOut；
        // 但可以立即停止继续 issue ReadBack，让旧请求尽快 drain。
        val spillCanRun =
          !readBackArea.busy &&
            (spillOutArea.busy || need_spillOut)

        val allowReadIssue =
          !spillOutArea.busy &&
            !need_spillOut

        readBackArea.run(allowReadIssue)

        when(spillCanRun) {
          spillOutArea.run()
        }
      }

      // Fetch Module idle and taskStack localBot == 0
      when(task_exhausted && io.toFetch.Pop.ready) {
        goto(IDLE)
      }
    }
  }
}

object GCTaskStackVerilog extends App {
  Config.spinal.generateVerilog(new GCTaskStack())
}