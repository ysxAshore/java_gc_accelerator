package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, WrapDec, WrapInc}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

// ============================================================================
// GCCopyReadMeta —— 向 Copy 模块发起 store-buffer 查询时携带的元数据
//
// 当某个任务标记为"来自 GCTrace 推送"（fromTracePush = true），说明该对象
// 可能正处于 Copy 过程中（从 from-space 搬到 to-space）。此时 Fetch 读取
// OOP/MarkWord 不应直接发 MMU 请求，而应先查询 Copy 的 store-buffer
//
// 字段说明：
//   needQuery       — 是否需要向 Copy 模块发起查询（仅 fromTracePush 任务为真）
//   epoch           — 当前 Copy 事务的 epoch，用于版本匹配
//   predecodedValid — 预解码信息是否有效（避免 Fetch 在关键路径上计算 beat 索引）
//   firstBeatIdx    — 如果命中 Copy buffer，数据在源端第几个 beat 中
//   firstByteOffset — 数据在 beat 内的字节偏移
// ============================================================================
case class GCCopyReadMeta() extends Bundle with GCTopParameters with GCParameters with HWParameters {
  val needQuery       = Bool()
  val epoch           = UInt(GCCopyEpochWidth bits)
  val predecodedValid = Bool()
  val firstBeatIdx    = UInt(32 bits)
  val firstByteOffset = UInt(log2Up(LineBytesNum) bits)
}

// ============================================================================
// GcFetchData —— Fetch 流水线中携带的完整任务上下文
//
// 每个任务经过 Fetch 流水线后，需要携带以下信息交给下游处理单元：
//
//   task           — 任务基地址（OOP 指针，已去除低位 tag 和 TracePushTagBit）
//   oopType        — OOP 类型：NotArrayOop(普通对象) / PartialArrayOop(部分数组)
//   fromObj        — 从 oop 中读取出的引用目标对象地址（即 oop 指向的对象的基地址）
//   markWord       — 目标对象的 Mark Word（GC 标记/转发/锁信息）
//   klassPtr       — 目标对象的 Klass 指针（类型元数据指针）
//   srcLength      — 源对象长度（32位，用于数组类型的长度信息）
//
//   fromTracePush  — 该任务是否由 GCTrace 实时推送（而非从 TaskStack 出栈）为 true 时需要查询 Copy store-buffer
//   oopCopyMeta    — OOP 读取时的 Copy 转发查询元数据
// ============================================================================
case class GcFetchData() extends Bundle with GCTopParameters with GCParameters with HWParameters {
  val task      = UInt(GCElementWidth bits)
  val oopType   = UInt(GCOopTypeWidth bits)
  val fromObj   = UInt(GCElementWidth bits)
  val markWord  = UInt(GCElementWidth bits)
  val klassPtr  = UInt(GCElementWidth bits)
  val srcLength = UInt(32 bits)

  val fromTracePush = Bool()
  val oopCopyMeta    = GCCopyReadMeta()
}

// ============================================================================
// GCFetch — 从 TaskStack 获取任务，通过 MMU 读取 OOP 和 MarkWord，
//           最终分发到 ArrayProcess 或 OopProcess 处理单元
//
// 顶层数据流：
//   TaskStack ──Pop──> preBuf(环形预取缓冲) ──> mainFsm ──> Fetch2ArrayProcess
//         │                                                      │
//         └──PrePop──> preFsm 提前完成 OOP + MW 读取 ─────────────┘
//                                                               Fetch2OopProcess
//   GCTrace ──Trace2Fetch──> pushFsm ──> mainFsm ───────────────>
//                                                               (二选一)
//
// 每条任务的处理流程（两阶段读取）：
//   阶段1: 读 OOP  — 从 task 地址读取 8 字节，解码得到 fromObj（目标对象地址）
//         特殊处理：PartialArrayOop 跳过 OOP 读取，因为 task 本身就是 fromObj
//   阶段2: 读 MW   — 从 fromObj 地址读取 MarkWord(8B) + KlassPtr(8B) [+可选 Length(4B)]
//
// 六条独立 MMU 端口（均直接连接 GCLocalMMU，不经过 UnalignedAdapter）：
//   MainMreq — mainFsm 使用，内部完成 Line 对齐/shift/跨 Line 拼接
//   PushMreq — pushFsm 使用，内部完成 Line 对齐/shift/跨 Line 拼接
//   PreMreq0..3 — 4 个 prefetch worker 独立使用
//
// OOP Line Cache：
//   只缓存 READ_OOP 的 32B Line；MW/Klass/Length 读取不进入 Cache。
//   TracePush OOP 为保证 Copy store-buffer forwarding 语义，绕过 Cache。
//   同 Line cache miss 通过 pending-line/MSHR 合并，避免多个 worker 重复读 LLC。
//
// Copy Store-Buffer 转发：
//   当任务来自 GCTrace（fromTracePush=true）时，目标对象可能正被 Copy 搬运。
//   此时通过 CopyFwd* 端口查询 Copy 模块的 store-buffer：
//     1. 完全命中 (fullFwd)    — 数据全部在 buffer 中，跳过 MMU 请求
//     2. 部分命中              — 合并 buffer 数据和 MMU 响应
//     3. stall               — 数据正在传输中，等待 Copy 完成
// ============================================================================
class GCFetch extends Module with HWParameters with GCTopParameters with GCParameters {
  val io = new Bundle {
    // Main / Push / Pre0~3 全部直接连接 GCLocalMMU。
    // GCFetch 内部负责所有 read 的 Line 对齐和 Response 恢复。
    val MainMreq = master(new LocalMMUIO)
    val PushMreq = master(new LocalMMUIO)

    // 4 条独立 Prefetch MMU 通道，每个 prefetch worker 独占一条。
    // 这四条端口直接连接 GCLocalMMU，不再经过 GCUnalignedMMUAdapter。
    // GCFetch 内部负责 Line 对齐、Response shift 和跨 Line 拼接。
    val PreMreq0 = master(new LocalMMUIO)
    val PreMreq1 = master(new LocalMMUIO)
    val PreMreq2 = master(new LocalMMUIO)
    val PreMreq3 = master(new LocalMMUIO)

    // 与 TaskStack 的接口：
    //   Pop    — 主消费端口，mainFsm 从此获取待处理任务
    //   PrePop — 预取端口，preFsm 从此提前获取未来任务
    val toFetch          = slave(new GCToFetch)
    val gcWriteSrcOopPtr = slave(new GCWriteSrcOopPtr) // Copy2Survivor 写入 srcOopPtr 的转发通知（用于 MarkWord 缓存更新）
    val Trace2Fetch      = slave Stream UInt(GCElementWidth bits) // GCTrace 实时推送的任务流（优先级高于 Pop）
    val CopyDone         = in Bool()
    val CopyState        = in(GCCopyPublicState()) // Copy 模块当前事务的公共状态（用于计算转发地址范围）

    // Copy store-buffer 转发查询端口（每条流水线一个）
    //   valid/addr/size/epoch — Fetch 发出查询
    //   stall/mask/data       — Copy 返回结果
    val CopyFwdMain = master(GCCopyForwardPort())
    val CopyFwdPush = master(GCCopyForwardPort())
    val CopyFwdPre  = master(GCCopyForwardPort())

    val Fetch2ArrayProcess = master(new GCToProcessUnit)
    val Fetch2OopProcess   = master(new GCToProcessUnit)
    val ConfigIO           = slave(new GCFetchConfigIO)

    val DebugTimeStamp     = in UInt(64 bits)
  }

  def clearMreq(m: LocalMMUIO): Unit = {
    m.Request.valid  := False
    m.Request.payload.clearAll()
    m.Response.ready := False
  }

  // GCFetch 所有 read 端口统一使用的对齐整 Line 读。
  // RequestSize 保持 0，与原 UnalignedAdapter 适配读送往 LLC 的语义一致：
  // 下游根据对齐地址返回完整 ResponseData Line。
  def driveAlignedLineReadReq(m: LocalMMUIO, alignedAddr: UInt): Unit = {
    m.Request.valid := True

    m.Request.payload.NeedResponse        := True
    m.Request.payload.NeedDoCmpxChg       := False
    m.Request.payload.RequestSize         := U(0, LineBytesNumBitSize bits)
    m.Request.payload.RequestWStrb        := U(0)
    m.Request.payload.RequestData         := U(0)
    m.Request.payload.RequestType_isWrite := False
    m.Request.payload.RequestSourceID     := m.ConherentRequsetSourceID.payload
    m.Request.payload.RequestVirtualAddr  := alignedAddr.resize(MMUAddrWidth)
  }

  val PreLineOffsetBits = log2Up(LineBytesNum)

  // 将任意逻辑地址向下对齐到 LineBytesNum 边界。
  def alignToLine(addr: UInt): UInt = {
    (addr & ~U(LineBytesNum - 1, addr.getWidth bits)).resize(MMUAddrWidth)
  }

  // 逻辑访问是否跨越 32B Line。
  def readCrossesLine(addr: UInt, sizeBytes: UInt): Bool = {
    val off = addr(PreLineOffsetBits - 1 downto 0)
    val sumWidth = LineBytesNumBitSize + 1

    off.resize(sumWidth) + sizeBytes.resize(sumWidth) >
      U(LineBytesNum, sumWidth bits)
  }

  // 单 Line 返回后，把逻辑地址对应的第 0 字节移到 ResponseData bit[7:0]。
  def shiftLineToLogical(lineData: UInt, offset: UInt): UInt = {
    lineData |>> (offset << 3)
  }

  // 跨 Line 逻辑读拼接：
  //   line0 中 offset..31 放到低位，
  //   line1 中 0.. 放到其后。
  def mergeTwoReadLines(line0: UInt, line1: UInt, offset: UInt): UInt = {
    val firstBytes =
      U(LineBytesNum, LineBytesNumBitSize + 1 bits) -
        offset.resize(LineBytesNumBitSize + 1)

    val firstPart =
      line0 |>> (offset << 3)

    val secondPart =
      line1 |<< (firstBytes << 3)

    firstPart | secondPart
  }

  // 跨 Line 两个 response 的归属判断。
  //
  // 不能假设 beat0 / beat1 一定得到不同 SourceID：
  // 当前下层可能连续给同一 LocalMMUIO 的两个 request 分配同一个 SourceID。
  //
  // 处理规则：
  //   1. SourceID 不同：按 ID 匹配，可乱序返回；
  //   2. SourceID 相同：优先填尚未收到的 beat0，下一次同 ID response 再填 beat1。
  //
  // 因此即使 src0 == src1，也不会让同一个 response 同时命中两个 beat。
  //
  // 注意：当两个 request 的 SourceID 相同时，这依赖下层对同 SourceID response
  // 保持请求顺序。如果下层允许“相同 SourceID 仍乱序返回”，仅凭当前 Response
  // 接口无法区分两个 transaction，此时必须让下层分配不同 SourceID 或改回串行。
  def classifyTwoLineResp(
      respFire: Bool,
      respSid: UInt,
      src0: UInt,
      src1: UInt,
      src1Valid: Bool,
      resp0Valid: Bool,
      resp1Valid: Bool
  ): (Bool, Bool) = {
    val match0 =
      respFire &&
        !resp0Valid &&
        respSid === src0

    val match1 =
      respFire &&
        src1Valid &&
        !resp1Valid &&
        respSid === src1

    // 如果 src0 == src1 且两个 beat 都没收到，本次 response 只归 beat0。
    val take0 =
      match0

    val take1 =
      match1 &&
        !match0

    (take0, take1)
  }

  // 辅助函数：Copy Store-Buffer 转发查询
  def driveCopyFwd(port: GCCopyForwardPort, addr: UInt, sizeBytes: UInt, meta: GCCopyReadMeta): Unit = {
    port.valid           := meta.needQuery
    port.addr            := addr.resize(MMUAddrWidth)
    port.size            := sizeBytes.resize(LineBytesNumBitSize)
    port.epoch           := meta.epoch
    port.predecodedValid := meta.predecodedValid
    port.firstBeatIdx    := meta.firstBeatIdx
    port.firstByteOffset := meta.firstByteOffset
  }

  // 根据 sizeBytes 生成字节有效掩码 例: sizeBytes=5, LineBytesNum=32 → ret[4:0]=1, ret[31:5]=0
  def requestedByteMask(sizeBytes: UInt): Bits = {
    val ret = Bits(LineBytesNum bits)
    for (i <- 0 until LineBytesNum) {
      ret(i) := U(i, LineBytesNumBitSize bits) < sizeBytes.resize(LineBytesNumBitSize)
    }
    ret
  }

  // 将字节掩码扩展为位掩码（每个有效字节对应 8 个 1 位）例: mask[0]=1, mask[1]=0 → ret[7:0]=0xFF, ret[15:8]=0x00
  def byteMaskToBits(mask: Bits): Bits = {
    val ret = Bits(MMUDataWidth bits)
    for (i <- 0 until LineBytesNum) {
      ret(i * 8 + 7 downto i * 8) := Mux(mask(i), B"8'xFF", B"8'x00")
    }
    ret
  }

  // 合并 Copy store-buffer 转发数据与 MMU 响应数 对于 Copy 已覆盖的字节用 fwdData，其余用 memoryData
  def mergeCopyForward(memoryData: UInt, fwdMask: Bits, fwdData: UInt): UInt = {
    val bitMask = byteMaskToBits(fwdMask)
    ((memoryData.asBits & ~bitMask) | (fwdData.asBits & bitMask)).asUInt
  }

  clearMreq(io.MainMreq)
  clearMreq(io.PushMreq)
  clearMreq(io.PreMreq0)
  clearMreq(io.PreMreq1)
  clearMreq(io.PreMreq2)
  clearMreq(io.PreMreq3)

  // Scala 侧静态数组，仅用于 generate-time 的 for 循环。
  // worker i 永远绑定 PreMreq{i}。
  val preMreqs = Seq(
    io.PreMreq0,
    io.PreMreq1,
    io.PreMreq2,
    io.PreMreq3
  )

  io.toFetch.Pop.ready    := False
  io.toFetch.PrePop.ready := False
  io.Trace2Fetch.ready    := False

  def clearCopyFwd(port: GCCopyForwardPort): Unit = {
    port.valid           := False
    port.addr            := 0
    port.size            := 0
    port.epoch           := 0
    port.predecodedValid := False
    port.firstBeatIdx    := 0
    port.firstByteOffset := 0
  }

  clearCopyFwd(io.CopyFwdMain)
  clearCopyFwd(io.CopyFwdPush)
  clearCopyFwd(io.CopyFwdPre)

  io.Fetch2ArrayProcess.clearOut()
  io.Fetch2OopProcess.clearOut()

  require(GCElementWidth > TracePushTagBit)

  val CopyOffBits = log2Up(LineBytesNum)
  val TracePushTagMask = U(BigInt(1) << TracePushTagBit, GCElementWidth bits) // TracePushTagBit 表示 用的哪一位表示task source

  def stripTraceTag(payload: UInt): UInt = (payload & ~TracePushTagMask).resize(GCElementWidth) // 对task去掉task source

  // 对于task 去掉 trace tag 以及 oop tag
  def taskBaseFromPayload(payload: UInt): UInt = {
    val untagged = stripTraceTag(payload)
    val lowTag = untagged(GCOopTagWidth - 1 downto 0).resize(GCElementWidth)
    (untagged - lowTag).resize(GCElementWidth)
  }

  // 提前计算Copy Forward 的请求信息 省一拍的关键路径延迟
  def prepareCopyReadMeta(addr: UInt, sizeBytes: UInt, fromTracePush: Bool, meta: GCCopyReadMeta): Unit = {
    val reqEnd = addr.resize(GCElementWidth) + sizeBytes.resize(GCElementWidth)
    val copyEnd = io.CopyState.DstPtr + io.CopyState.TotalSize.resize(GCElementWidth)
    val fullyInsideCopyRange = addr.resize(GCElementWidth) >= io.CopyState.DstPtr && reqEnd <= copyEnd // 判断边界
    val logicalOffset = addr.resize(GCElementWidth) - io.CopyState.DstPtr
    val sourceByteAddr = io.CopyState.SrcPtr + logicalOffset

    meta.needQuery := fromTracePush
    meta.epoch     := io.CopyState.Epoch
    meta.predecodedValid := fromTracePush && io.CopyState.Active && fullyInsideCopyRange
    meta.firstBeatIdx := ((sourceByteAddr - io.CopyState.SrcBase) >> CopyOffBits).resize(32) // 每次请求时 是以32B对齐的请求的 所以-SrcBase可以看到是第几个源
    meta.firstByteOffset := sourceByteAddr(CopyOffBits - 1 downto 0) // 在该源请求内的第几个偏移
  }

  // ============================================================================
  // Task 接收与解析
  //
  // receiveTask 将一个原始 task payload（来自 Pop/PrePop/Trace2Fetch）解析为
  // GcFetchData 结构：
  //   — 提取 bit[63] 判断是否来自 Trace 推送
  //   — 提取低位 OopTag 判断对象类型（普通对象 vs 部分数组）
  //   — 计算 taskBase（去除所有 tag 后的纯地址）
  //   — 为 OOP 读取阶段准备好 Copy 转发元数据
  //
  //   PartialArrayOop 特殊处理：task 本身就是 fromObj（数组片段起始地址），
  //   不需要再读 OOP
  // ============================================================================
  def receiveTask(payload: UInt, data: GcFetchData): Unit = {
    val fromTrace = payload(TracePushTagBit)
    val untagged  = stripTraceTag(payload)
    val lowTag = untagged(GCOopTagWidth - 1 downto 0)
    val taskBase = taskBaseFromPayload(payload)

    data.oopType := Mux(
      lowTag === U(PartialArrayTag, GCOopTagWidth bits),
      U(PartialArrayOop, GCOopTypeWidth bits),
      U(NotArrayOop, GCOopTypeWidth bits)
    )
    data.task          := taskBase
    data.fromTracePush := fromTrace

    prepareCopyReadMeta(
      taskBase,
      U(8, LineBytesNumBitSize bits),
      fromTrace,
      data.oopCopyMeta
    )
  }

  def decodeReadOopResp(rd: UInt): UInt = Mux(
    io.ConfigIO.UseCompressedOop,
    (io.ConfigIO.CompressedOopBase + (rd(31 downto 0).resize(GCElementWidth) << io.ConfigIO.CompressedOopShift)).resize(GCElementWidth),
    rd(GCElementWidth - 1 downto 0)
  )

  // fillKlassLen — 从读取的原始数据中提取 KlassPtr 和对象长度
  // MMU 响应数据布局（256 bit = 32 字节线）：
  //   非压缩Klass: [Length(32b)][KlassPtr(64b)][MarkWord(64b)]
  //   压缩Klass:   [KlassPtr(32b)][Length(32b)][MarkWord(64b)]
  def fillKlassLen(rd: UInt, data: GcFetchData): Unit = {
    data.klassPtr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)

    data.srcLength := Mux(
      io.ConfigIO.UseCompressedKlassPointers,
      rd(GCElementWidth * 2 - 1 downto GCElementWidth + 32),
      rd(GCElementWidth * 2 + 31 downto GCElementWidth * 2)
    )
  }

  def fillMwKlassLen(rd: UInt, data: GcFetchData): Unit = {
    data.markWord := rd(GCElementWidth - 1 downto 0)
    fillKlassLen(rd, data)
  }

  def copyFetchContextWithoutMw(dst: GcFetchData, src: GcFetchData): Unit = {
    dst.task          := src.task
    dst.oopType       := src.oopType
    dst.fromObj       := src.fromObj
    dst.fromTracePush := src.fromTracePush
    dst.oopCopyMeta   := src.oopCopyMeta
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCFetch<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }

  def driveProcessUnit(target: GCToProcessUnit, payload: GcFetchData, effectiveMarkWord: UInt): Unit = {
    target.cmd.valid             := True
    target.cmd.payload.Task      := payload.task
    target.cmd.payload.OopType   := payload.oopType
    target.cmd.payload.SrcOopPtr := payload.fromObj
    target.cmd.payload.MarkWord  := effectiveMarkWord
    target.cmd.payload.KlassPtr  := payload.klassPtr
    target.cmd.payload.SrcLength := payload.srcLength
  }

  val oopReadSize = U(8, LineBytesNumBitSize bits)
  val mwReadSize = Mux(
    io.ConfigIO.UseCompressedKlassPointers,
    U(16),
    U(20)
  ).resize(LineBytesNumBitSize)

  val main_data = RegInit(GcFetchData().getZero)
  val push_data = RegInit(GcFetchData().getZero)

  // ============================================================================
  // Main / Push direct-Line read metadata
  //
  // 一个逻辑 read 最多拆成两个 32B Line request。
  // 对 cross-line 情况，REQ0 fire 后下一拍立即尝试 REQ1，不等待 RESP0。
  // 两个 Response 通过各自 RequestSourceID 匹配，因此允许乱序返回。
  // ============================================================================
  val mainOopOffset = RegInit(U(0, PreLineOffsetBits bits))
  val mainOopCross  = RegInit(False)
  val mainMwOffset  = RegInit(U(0, PreLineOffsetBits bits))
  val mainMwCross   = RegInit(False)

  val mainLineSrc0 = RegInit(U(0, LLCSourceMaxNumBitSize bits))
  val mainLineSrc1 = RegInit(U(0, LLCSourceMaxNumBitSize bits))
  val mainLineSrc1Valid = RegInit(False)
  val mainLineResp0Valid = RegInit(False)
  val mainLineResp1Valid = RegInit(False)
  val mainLineData0 = RegInit(U(0, MMUDataWidth bits))
  val mainLineData1 = RegInit(U(0, MMUDataWidth bits))

  val pushOopOffset = RegInit(U(0, PreLineOffsetBits bits))
  val pushOopCross  = RegInit(False)
  val pushMwOffset  = RegInit(U(0, PreLineOffsetBits bits))
  val pushMwCross   = RegInit(False)

  val pushLineSrc0 = RegInit(U(0, LLCSourceMaxNumBitSize bits))
  val pushLineSrc1 = RegInit(U(0, LLCSourceMaxNumBitSize bits))
  val pushLineSrc1Valid = RegInit(False)
  val pushLineResp0Valid = RegInit(False)
  val pushLineResp1Valid = RegInit(False)
  val pushLineData0 = RegInit(U(0, MMUDataWidth bits))
  val pushLineData1 = RegInit(U(0, MMUDataWidth bits))

  // ============================================================================
  // READ_OOP-only 8-entry Line Cache
  //
  // * 全相联，tag = 32B aligned address，data = 256-bit Line。
  // * Main / Push / Pre0~3 共享 lookup。
  // * 仅普通（!fromTracePush）且单-Line 的 8B OOP read 使用 Cache。
  // * MW/Klass/Length 永远 bypass Cache。
  // * TracePush OOP 必须先走 Copy store-buffer forwarding，因此 bypass Cache。
  // ============================================================================
  val OopLineCacheEntries  = 8
  val OopLineCacheIdxWidth = log2Up(OopLineCacheEntries)

  val oopLineCacheValid = Vec.fill(OopLineCacheEntries)(RegInit(False))
  val oopLineCacheTag = Vec.fill(OopLineCacheEntries)(
    RegInit(U(0, MMUAddrWidth bits))
  )
  val oopLineCacheData = Vec.fill(OopLineCacheEntries)(
    RegInit(U(0, MMUDataWidth bits))
  )
  val oopLineCacheReplacePtr =
    RegInit(U(0, OopLineCacheIdxWidth bits))

  def lookupOopLineCache(lineAddr: UInt): (Bool, UInt) = {
    val hitVec = Bits(OopLineCacheEntries bits)

    for (i <- 0 until OopLineCacheEntries) {
      hitVec(i) :=
        oopLineCacheValid(i) &&
          oopLineCacheTag(i) === lineAddr.resize(MMUAddrWidth)
    }

    val hit = hitVec.orR
    val hitData = UInt(MMUDataWidth bits)

    hitData := U(0, MMUDataWidth bits)

    when(hit) {
      hitData :=
        PriorityMux(
          (0 until OopLineCacheEntries).map(i =>
            (hitVec(i), oopLineCacheData(i))
          )
        )
    }

    (hit, hitData)
  }

  // ============================================================================
  // OOP pending-line table / lightweight MSHR
  //
  // 每个可发 OOP Line request 的 source 固定拥有一个 pending entry：
  //   0 = Main
  //   1 = Push
  //   2 = PreWorker0
  //   3 = PreWorker1
  //   4 = PreWorker2
  //   5 = PreWorker3
  //
  // 同 Line miss coalescing：
  //   新 miss 先经过每拍一个的 issue arbiter，解决同拍重复 miss；
  //   cache miss + pending hit -> 不重复发 LLC request，停在 OOP_REQ0 重试；
  //   owner response 返回     -> pendingReady=1；
  //   fill arbiter 每拍将一个 ready entry 写入 Cache 并释放 pending；
  //   waiter 下一拍 lookup Cache 命中继续。
  //
  // 这样不需要 waiter bitmap，也不会因为同时 4 个 worker miss 同一 Line
  // 而重复产生 4 次 LLC read。
  // ============================================================================
  val OopPendingNum      = 6
  val OopPendingIdxWidth = log2Up(OopPendingNum)

  val oopPendingValid = Vec.fill(OopPendingNum)(RegInit(False))
  val oopPendingReady = Vec.fill(OopPendingNum)(RegInit(False))
  val oopPendingLineAddr = Vec.fill(OopPendingNum)(
    RegInit(U(0, MMUAddrWidth bits))
  )
  val oopPendingLineData = Vec.fill(OopPendingNum)(
    RegInit(U(0, MMUDataWidth bits))
  )

  def lookupOopPending(lineAddr: UInt): Bool = {
    val hitVec = Bits(OopPendingNum bits)

    for (i <- 0 until OopPendingNum) {
      hitVec(i) :=
        oopPendingValid(i) &&
          oopPendingLineAddr(i) === lineAddr.resize(MMUAddrWidth)
    }

    hitVec.orR
  }

  // 同一拍的多个 Cache miss 看不到彼此本拍将写入的 pendingValid。
  // 因此新 OOP Cache miss 统一经过一个 6->1 issue arbiter：
  // 每拍最多建立一个新的 pending Line，下一拍开始其它 source 就能 pending-hit。
  //
  // 这不会把 OOP MLP 压成 1：不同 Line 仍可连续每拍各发一个 request，
  // 很快形成多个 outstanding；只是避免同一拍重复 miss。
  val oopMissWant =
    Bits(OopPendingNum bits)

  oopMissWant :=
    B(0, OopPendingNum bits)

  val oopMissGrant =
    Bits(OopPendingNum bits)

  oopMissGrant(0) :=
    oopMissWant(0)

  for (i <- 1 until OopPendingNum) {
    oopMissGrant(i) :=
      oopMissWant(i) &&
        !oopMissWant(i - 1 downto 0).orR
  }

  // ============================================================================
  // Copy 部分转发寄存器
  //
  // 当 MMU 读请求发送时，采样 Copy 返回的 mask/data。
  // 每条流水线最多一个未完成的 MMU 读取，所以每条流水线一对寄存器即可。
  // MMU 响应返回时，用 mergeCopyForward() 合并转发数据。
  // ============================================================================
  val mainFwdMask = RegInit(B(0, LineBytesNum bits))
  val mainFwdData = RegInit(U(0, MMUDataWidth bits))
  val pushFwdMask = RegInit(B(0, LineBytesNum bits))
  val pushFwdData = RegInit(U(0, MMUDataWidth bits))

  // ============================================================================
  // PreFetch 环形缓冲区
  //
  // preBuf[0..PreFetchBufferNum-1] — 预取任务上下文环形缓冲区
  // preBufDone[i]                  — 第 i 个槽位的任务是否已完成 OOP+MW 读取
  //
  // 环形指针：
  //   buf_top    — 写入端（preFsm 将新预取任务追加到此位置）
  //   buf_bottom — 读取端（mainFsm 从此位置消费已完成的任务）
  //   buf_count  — 当前缓冲区中的有效条目数
  //   buf_work   — preFsm 当前正在处理的槽位索引
  //
  //  buf_capacity = PreFetchBufferNum
  //  buf_free      = buf_capacity - buf_count
  //
  // 生命周期：
  //   1. preFsm 预取任务 → 写入 buf_top，buf_count++
  //   2. preFsm 完成 OOP+MW 读取 → preBufDone[buf_work] = True
  //   3. mainFsm Pop 到已完成条目 → 从 buf_bottom 取出，buf_count--
  // ============================================================================
  val preBuf = Vec.fill(PreFetchBufferNum)(RegInit(GcFetchData().getZero))

  // preBufValid:
  //   False -> FREE，allocator 可以重新分配
  //   True  -> slot 已经属于当前 prefetch window；无论 worker 是否已经 Done，
  //            在 Main 真正消费/显式覆盖前都不能被 Normal allocator 复用
  //
  // preBufDone:
  //   valid=1, done=0 -> INFLIGHT
  //   valid=1, done=1 -> READY，等待 Main 消费
  val preBufValid = Vec.fill(PreFetchBufferNum)(RegInit(False))
  val preBufDone  = Vec.fill(PreFetchBufferNum)(RegInit(False))

  val buf_top = RegInit(U(0, PreFetchBufferWidth bits))
  val buf_bottom = RegInit(U(0, PreFetchBufferWidth bits))
  val buf_count = RegInit(U(0, PreFetchBufferWidth + 1 bits))

  val buf_capacity = U(PreFetchBufferNum, PreFetchBufferWidth + 1 bits)
  val buf_free = buf_capacity - buf_count


  // pushFollowRem — Push-follow 模式下剩余待 PrePop 的任务数
  //
  // 当 TaskStack 有一批新的 Push burst 之后，这些新任务需要通过 PrePop 输送给 Fetch
  // 这批任务应优先于缓冲区中已有的旧预取任务被消费。
  // pushFollowRem 记录了这批次中还剩多少个任务需要 PrePop
  //
  // 两种模式：
  //   Normal 模式：PushCount==0 且 pushFollowRem==0，新 PrePop 追加到 buf_top
  //   Push-follow 模式：PushCount!=0 或 pushFollowRem!=0，新 Push 的任务插入到
  //                     buf_bottom 之前，优先被 mainFsm 消费
  val pushFollowRem = RegInit(U(0, 32 bits))

  def bufInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, PreFetchBufferNum, step).resize(PreFetchBufferWidth)
  def bufDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, PreFetchBufferNum, step).resize(PreFetchBufferWidth)

  // 只有 Main 真正消费 slot 后，Normal allocator 才能重新使用它。
  def releaseSlot(idx: UInt): Unit = {
    preBufValid(idx) := False
    preBufDone(idx)  := False
  }

  val mainIsIdle     = Bool()
  val mainIsWaitDone = Bool()
  val pushIsIdle     = Bool()

  val mainGotoReadOop = Bool()
  val mainGotoSend    = Bool()

  mainGotoReadOop := False
  mainGotoSend    := False

  val targetDone = Mux(
    main_data.oopType === U(NotArrayOop),
    io.Fetch2OopProcess.Done,
    io.Fetch2ArrayProcess.Done
  )

  val targetDoneSeen = RegInit(False)
  when(targetDone && !mainIsWaitDone) {
    targetDoneSeen := True
  }

  // mainFsm Pop 到预取条目但 worker 尚未完成时，锁存“具体 slot + task”。
  // 后续不能继续用实时 buf_bottom 作为唯一依据，否则 buf_bottom/slot 被其它
  // 控制路径修改时可能消费到另一个 generation 的数据。
  val waitForPrefetch = RegInit(False)
  val waitPrefetchSlot = RegInit(U(0, PreFetchBufferWidth bits))
  val waitPrefetchTask = RegInit(U(0, GCElementWidth bits))

  // ============================================================================
  // MarkWord 转发缓存（Forwarding Cache）
  //
  // 问题场景（RAW 冒险）：
  //   1. Fetch 读取了对象 A 的 MarkWord（旧值，如 unlocked 状态）
  //   2. Copy2Survivor 将 A 拷贝到 to-space，并安装了转发 MarkWord
  //   3. Fetch 在 SEND 时应使用新 MarkWord，而非已读取的旧值
  //
  // 解决方案：一个小型全相联缓存，存储 Copy2Survivor 的转发映射. 满后会覆盖最旧的槽位
  // 查询优先级（由高到低）：
  //   1. 当前周期的 writeForward 通知
  //   2. 转发缓存中的历史条目
  //   3. fallback（之前读取的 MarkWord）
  //
  // 清理条件：所有流水线空闲 + 预取缓冲为空时，安全清空
  // ============================================================================
  val ForwardCacheEntries = 1 << log2Up(PreFetchBufferNum + 4)

  val fwdCacheValid = Vec.fill(ForwardCacheEntries)(RegInit(False))
  val fwdCacheObj = Vec.fill(ForwardCacheEntries)(RegInit(U(0, GCElementWidth bits)))
  val fwdCacheValue = Vec.fill(ForwardCacheEntries)(RegInit(U(0, GCElementWidth bits)))
  val fwdCacheReplacePtr = RegInit(U(0, log2Up(ForwardCacheEntries) bits))

  val incomingFwdValid = io.gcWriteSrcOopPtr.writeForward.valid
  val incomingFwdObj = io.gcWriteSrcOopPtr.writeForward.payload.srcOopPtr
  val incomingFwdValue = io.gcWriteSrcOopPtr.writeForward.payload.writeValue

  val incomingFwdHitVec = Bits(ForwardCacheEntries bits)

  for (i <- 0 until ForwardCacheEntries) {
    incomingFwdHitVec(i) := fwdCacheValid(i) && fwdCacheObj(i) === incomingFwdObj
  }

  val incomingFwdHit = incomingFwdHitVec.orR
  val incomingFwdHitIndex = OHToUInt(incomingFwdHitVec)

  when(incomingFwdValid) {
    when(incomingFwdHit) {
      fwdCacheValue(incomingFwdHitIndex) := incomingFwdValue
    } otherwise {
      fwdCacheValid(fwdCacheReplacePtr) := True
      fwdCacheObj(fwdCacheReplacePtr) := incomingFwdObj
      fwdCacheValue(fwdCacheReplacePtr) := incomingFwdValue
      fwdCacheReplacePtr := fwdCacheReplacePtr + U(1, fwdCacheReplacePtr.getWidth bits)
    }
  }.elsewhen(mainIsIdle && pushIsIdle && buf_count === U(0, buf_count.getWidth bits) && !waitForPrefetch) {
    for (i <- 0 until ForwardCacheEntries) {
      fwdCacheValid(i) := False
    }
    fwdCacheReplacePtr := 0
  }

  def resolveForwardMark(obj: UInt, fallback: UInt): UInt = {
    val resolved = UInt(GCElementWidth bits)

    resolved := fallback

    for (i <- 0 until ForwardCacheEntries) {
      when(fwdCacheValid(i) && fwdCacheObj(i) === obj) {
        resolved := fwdCacheValue(i)
      }
    }

    when(incomingFwdValid && incomingFwdObj === obj) {
      resolved := incomingFwdValue
    }

    resolved
  }

  // ============================================================================
  // Main StateMachine
  //
  // OOP:
  //   Cache hit -> 直接 decode
  //   Cache miss + pending hit -> 原地等待 owner fill Cache
  //   Cache miss + pending miss -> 发 Line0，并建立 pending[Main]
  //
  // Cross-line:
  //   REQ0.fire -> 下一拍 REQ1，不等待 RESP0；
  //   WAIT 中按 SourceID 收集两个 Response，可乱序返回。
  // ============================================================================
  val mainFsm = new StateMachine {
    val IDLE          = new State with EntryPoint
    val READ_OOP_REQ0 = new State
    val READ_OOP_REQ1 = new State
    val READ_OOP_WAIT = new State
    val READ_MW_REQ0  = new State
    val READ_MW_REQ1  = new State
    val READ_MW_WAIT  = new State
    val SEND          = new State
    val WAIT_DONE     = new State

    IDLE.whenIsActive {
      val fetchPushFollowActive =
        pushFollowRem =/= U(0, 32 bits)

      io.toFetch.Pop.ready :=
        pushIsIdle &&
          !io.Trace2Fetch.valid &&
          !waitForPrefetch &&
          !fetchPushFollowActive

      when(io.toFetch.Pop.fire) {
        val popBase =
          taskBaseFromPayload(io.toFetch.Pop.payload)

        val bottomCountValid =
          buf_count =/= U(0, buf_count.getWidth bits)

        val bottomValid =
          bottomCountValid &&
            preBufValid(buf_bottom)

        val bottomHit =
          bottomValid &&
            preBuf(buf_bottom).task === popBase

        when(bottomHit && preBufDone(buf_bottom)) {
          main_data :=
            preBuf(buf_bottom)

          releaseSlot(buf_bottom)

          buf_bottom :=
            bufInc(
              buf_bottom,
              U(1, PreFetchBufferWidth bits)
            )

          buf_count :=
            buf_count -
              U(1, buf_count.getWidth bits)

          goto(SEND)

        }.elsewhen(bottomHit) {
          waitForPrefetch :=
            True

          waitPrefetchSlot :=
            buf_bottom

          waitPrefetchTask :=
            popBase

        }.otherwise {
          receiveTask(
            io.toFetch.Pop.payload,
            main_data
          )

          main_data.fromObj :=
            U(0, GCElementWidth bits)

          goto(READ_OOP_REQ0)
        }

      }.elsewhen(
        waitForPrefetch &&
          preBufValid(waitPrefetchSlot) &&
          preBuf(waitPrefetchSlot).task === waitPrefetchTask &&
          preBufDone(waitPrefetchSlot)
      ) {
        waitForPrefetch :=
          False

        main_data :=
          preBuf(waitPrefetchSlot)

        releaseSlot(waitPrefetchSlot)

        buf_bottom :=
          bufInc(
            waitPrefetchSlot,
            U(1, PreFetchBufferWidth bits)
          )

        buf_count :=
          buf_count -
            U(1, buf_count.getWidth bits)

        goto(SEND)
      }
    }

    // ------------------------------------------------------------------------
    // OOP REQ0
    // ------------------------------------------------------------------------
    READ_OOP_REQ0.whenIsActive {
      when(main_data.oopType === U(PartialArrayOop)) {
        main_data.fromObj :=
          main_data.task

        goto(READ_MW_REQ0)

      }.otherwise {
        val oopAddr =
          main_data.task

        val line0Addr =
          alignToLine(oopAddr)

        val offsetNow =
          oopAddr(PreLineOffsetBits - 1 downto 0)

        val crossNow =
          readCrossesLine(
            oopAddr,
            oopReadSize
          )

        val cacheEligible =
          !main_data.oopCopyMeta.needQuery &&
            !crossNow

        val (cacheHit, cacheLine) =
          lookupOopLineCache(line0Addr)

        val pendingHit =
          lookupOopPending(line0Addr)

        val ownPendingFree =
          !oopPendingValid(0)

        val newCacheMiss =
          cacheEligible &&
            !cacheHit &&
            !pendingHit &&
            ownPendingFree

        when(newCacheMiss) {
          oopMissWant(0) :=
            True
        }

        driveCopyFwd(
          io.CopyFwdMain,
          oopAddr,
          oopReadSize,
          main_data.oopCopyMeta
        )

        val reqMask =
          requestedByteMask(oopReadSize)

        val fullFwd =
          main_data.oopCopyMeta.needQuery &&
            (io.CopyFwdMain.mask & reqMask) === reqMask

        when(!io.CopyFwdMain.stall) {
          when(fullFwd) {
            main_data.fromObj :=
              decodeReadOopResp(
                io.CopyFwdMain.data
              )

            goto(READ_MW_REQ0)

          }.elsewhen(cacheEligible && cacheHit) {
            val logicalData =
              shiftLineToLogical(
                cacheLine,
                offsetNow
              )

            main_data.fromObj :=
              decodeReadOopResp(logicalData)

            goto(READ_MW_REQ0)

          }.elsewhen(cacheEligible && pendingHit) {
            // 同一 Line 已经在飞：不重复发请求。
            // owner fill Cache 后，本状态下一拍自动 cache hit。
          }.elsewhen(
            !cacheEligible ||
              (newCacheMiss && oopMissGrant(0))
          ) {
            driveAlignedLineReadReq(
              io.MainMreq,
              line0Addr
            )

            when(io.MainMreq.Request.fire) {
              mainOopOffset :=
                offsetNow

              mainOopCross :=
                crossNow

              mainFwdMask :=
                io.CopyFwdMain.mask

              mainFwdData :=
                io.CopyFwdMain.data

              mainLineSrc0 :=
                io.MainMreq.Request.payload.RequestSourceID.resized

              mainLineSrc1Valid :=
                False

              mainLineResp0Valid :=
                False

              mainLineResp1Valid :=
                False

              when(cacheEligible) {
                oopPendingValid(0) :=
                  True

                oopPendingReady(0) :=
                  False

                oopPendingLineAddr(0) :=
                  line0Addr
              }

              when(crossNow) {
                goto(READ_OOP_REQ1)

              }.otherwise {
                goto(READ_OOP_WAIT)
              }
            }
          }
        }
      }
    }

    // ------------------------------------------------------------------------
    // OOP REQ1
    // ------------------------------------------------------------------------
    READ_OOP_REQ1.whenIsActive {
      val line1Addr =
        (
          alignToLine(main_data.task) +
            U(LineBytesNum, MMUAddrWidth bits)
        ).resize(MMUAddrWidth)

      driveAlignedLineReadReq(
        io.MainMreq,
        line1Addr
      )

      // RESP0 可能在 REQ1 尚未 fire 时提前回来。
      io.MainMreq.Response.ready :=
        True

      when(io.MainMreq.Response.fire) {
        val respSid =
          io.MainMreq.Response.payload.ResponseSourceID.resized

        when(
          !mainLineResp0Valid &&
            respSid === mainLineSrc0
        ) {
          mainLineData0 :=
            io.MainMreq.Response.payload.ResponseData

          mainLineResp0Valid :=
            True
        }
      }

      when(io.MainMreq.Request.fire) {
        mainLineSrc1 :=
          io.MainMreq.Request.payload.RequestSourceID.resized

        mainLineSrc1Valid :=
          True

        goto(READ_OOP_WAIT)
      }
    }

    // ------------------------------------------------------------------------
    // OOP WAIT
    // ------------------------------------------------------------------------
    READ_OOP_WAIT.whenIsActive {
      io.MainMreq.Response.ready :=
        True

      val respFire =
        io.MainMreq.Response.fire

      val respSid =
        io.MainMreq.Response.payload.ResponseSourceID.resized

      val (respIs0, respIs1) =
        classifyTwoLineResp(
          respFire,
          respSid,
          mainLineSrc0,
          mainLineSrc1,
          mainLineSrc1Valid,
          mainLineResp0Valid,
          mainLineResp1Valid
        )

      when(respIs0) {
        mainLineData0 :=
          io.MainMreq.Response.payload.ResponseData

        mainLineResp0Valid :=
          True
      }

      when(respIs1) {
        mainLineData1 :=
          io.MainMreq.Response.payload.ResponseData

        mainLineResp1Valid :=
          True
      }

      val got0Now =
        mainLineResp0Valid || respIs0

      val got1Now =
        !mainOopCross ||
          mainLineResp1Valid ||
          respIs1

      val line0Now =
        Mux(
          respIs0,
          io.MainMreq.Response.payload.ResponseData,
          mainLineData0
        )

      val line1Now =
        Mux(
          respIs1,
          io.MainMreq.Response.payload.ResponseData,
          mainLineData1
        )

      when(got0Now && got1Now) {
        val memoryLogical =
          Mux(
            mainOopCross,
            mergeTwoReadLines(
              line0Now,
              line1Now,
              mainOopOffset
            ),
            shiftLineToLogical(
              line0Now,
              mainOopOffset
            )
          )

        val logicalData =
          mergeCopyForward(
            memoryLogical,
            mainFwdMask,
            mainFwdData
          )

        // 只有普通单-Line OOP miss 建立了 pending[0]。
        // Response 先落 pendingReady，Cache fill arbiter 后续写 Cache。
        when(oopPendingValid(0)) {
          oopPendingLineData(0) :=
            line0Now

          oopPendingReady(0) :=
            True
        }

        main_data.fromObj :=
          decodeReadOopResp(logicalData)

        goto(READ_MW_REQ0)
      }
    }

    // ------------------------------------------------------------------------
    // MW REQ0
    // ------------------------------------------------------------------------
    READ_MW_REQ0.whenIsActive {
      val mwAddr =
        main_data.fromObj

      val line0Addr =
        alignToLine(mwAddr)

      val offsetNow =
        mwAddr(PreLineOffsetBits - 1 downto 0)

      val crossNow =
        readCrossesLine(
          mwAddr,
          mwReadSize
        )

      driveAlignedLineReadReq(
        io.MainMreq,
        line0Addr
      )

      when(io.MainMreq.Request.fire) {
        mainMwOffset :=
          offsetNow

        mainMwCross :=
          crossNow

        mainLineSrc0 :=
          io.MainMreq.Request.payload.RequestSourceID.resized

        mainLineSrc1Valid :=
          False

        mainLineResp0Valid :=
          False

        mainLineResp1Valid :=
          False

        when(crossNow) {
          goto(READ_MW_REQ1)

        }.otherwise {
          goto(READ_MW_WAIT)
        }
      }
    }

    // ------------------------------------------------------------------------
    // MW REQ1
    // ------------------------------------------------------------------------
    READ_MW_REQ1.whenIsActive {
      val line1Addr =
        (
          alignToLine(main_data.fromObj) +
            U(LineBytesNum, MMUAddrWidth bits)
        ).resize(MMUAddrWidth)

      driveAlignedLineReadReq(
        io.MainMreq,
        line1Addr
      )

      // 不阻塞提前返回的 RESP0。
      io.MainMreq.Response.ready :=
        True

      when(io.MainMreq.Response.fire) {
        val respSid =
          io.MainMreq.Response.payload.ResponseSourceID.resized

        when(
          !mainLineResp0Valid &&
            respSid === mainLineSrc0
        ) {
          mainLineData0 :=
            io.MainMreq.Response.payload.ResponseData

          mainLineResp0Valid :=
            True
        }
      }

      when(io.MainMreq.Request.fire) {
        mainLineSrc1 :=
          io.MainMreq.Request.payload.RequestSourceID.resized

        mainLineSrc1Valid :=
          True

        goto(READ_MW_WAIT)
      }
    }

    // ------------------------------------------------------------------------
    // MW WAIT
    // ------------------------------------------------------------------------
    READ_MW_WAIT.whenIsActive {
      io.MainMreq.Response.ready :=
        True

      val respFire =
        io.MainMreq.Response.fire

      val respSid =
        io.MainMreq.Response.payload.ResponseSourceID.resized

      val (respIs0, respIs1) =
        classifyTwoLineResp(
          respFire,
          respSid,
          mainLineSrc0,
          mainLineSrc1,
          mainLineSrc1Valid,
          mainLineResp0Valid,
          mainLineResp1Valid
        )

      when(respIs0) {
        mainLineData0 :=
          io.MainMreq.Response.payload.ResponseData

        mainLineResp0Valid :=
          True
      }

      when(respIs1) {
        mainLineData1 :=
          io.MainMreq.Response.payload.ResponseData

        mainLineResp1Valid :=
          True
      }

      val got0Now =
        mainLineResp0Valid || respIs0

      val got1Now =
        !mainMwCross ||
          mainLineResp1Valid ||
          respIs1

      val line0Now =
        Mux(
          respIs0,
          io.MainMreq.Response.payload.ResponseData,
          mainLineData0
        )

      val line1Now =
        Mux(
          respIs1,
          io.MainMreq.Response.payload.ResponseData,
          mainLineData1
        )

      when(got0Now && got1Now) {
        val rd =
          Mux(
            mainMwCross,
            mergeTwoReadLines(
              line0Now,
              line1Now,
              mainMwOffset
            ),
            shiftLineToLogical(
              line0Now,
              mainMwOffset
            )
          )

        fillMwKlassLen(
          rd,
          main_data
        )

        goto(SEND)
      }
    }

    SEND.whenIsActive {
      val isOop =
        main_data.oopType === U(NotArrayOop)

      val dispatchMarkWord =
        resolveForwardMark(
          main_data.fromObj,
          main_data.markWord
        )

      when(isOop) {
        driveProcessUnit(
          io.Fetch2OopProcess,
          main_data,
          dispatchMarkWord
        )

      }.otherwise {
        driveProcessUnit(
          io.Fetch2ArrayProcess,
          main_data,
          dispatchMarkWord
        )
      }

      val unitFire =
        Mux(
          isOop,
          io.Fetch2OopProcess.cmd.fire,
          io.Fetch2ArrayProcess.cmd.fire
        )

      when(unitFire) {
        goto(WAIT_DONE)

        dbg(
          Seq(
            "Dispatch Task=",
            main_data.task,
            " OopType=",
            main_data.oopType,
            " SrcOopPtr=",
            main_data.fromObj,
            " MarkWord=",
            dispatchMarkWord,
            " KlassPtr=",
            main_data.klassPtr,
            " success!"
          )
        )
      }
    }

    WAIT_DONE.whenIsActive {
      when(targetDone || targetDoneSeen) {
        targetDoneSeen :=
          False

        goto(IDLE)

        dbg(
          Seq(
            "Task=",
            main_data.task,
            " done"
          )
        )
      }
    }

    always {
      when(mainGotoSend) {
        goto(SEND)

      }.elsewhen(mainGotoReadOop) {
        goto(READ_OOP_REQ0)
      }
    }
  }

  // ============================================================================
  // Push StateMachine
  //
  // PushMreq 同样支持 cross-line 两个请求 back-to-back。
  // TracePush OOP 通常 needQuery=True，因此正常绕过 OOP Cache/MSHR。
  // ============================================================================
  val pushFsm = new StateMachine {
    val IDLE          = new State with EntryPoint
    val READ_OOP_REQ0 = new State
    val READ_OOP_REQ1 = new State
    val READ_OOP_WAIT = new State
    val READ_MW_REQ0  = new State
    val READ_MW_REQ1  = new State
    val READ_MW_WAIT  = new State
    val SEND          = new State

    IDLE.whenIsActive {
      io.Trace2Fetch.ready :=
        True

      when(io.Trace2Fetch.fire) {
        val payload =
          io.Trace2Fetch.payload

        when(mainIsIdle) {
          receiveTask(
            payload,
            main_data
          )

          main_data.fromObj :=
            U(0, GCElementWidth bits)

          mainGotoReadOop :=
            True

        }.otherwise {
          receiveTask(
            payload,
            push_data
          )

          push_data.fromObj :=
            U(0, GCElementWidth bits)

          goto(READ_OOP_REQ0)
        }
      }
    }

    READ_OOP_REQ0.whenIsActive {
      when(push_data.oopType === U(PartialArrayOop)) {
        push_data.fromObj :=
          push_data.task

        goto(READ_MW_REQ0)

      }.otherwise {
        val oopAddr =
          push_data.task

        val line0Addr =
          alignToLine(oopAddr)

        val offsetNow =
          oopAddr(PreLineOffsetBits - 1 downto 0)

        val crossNow =
          readCrossesLine(
            oopAddr,
            oopReadSize
          )

        val cacheEligible =
          !push_data.oopCopyMeta.needQuery &&
            !crossNow

        val (cacheHit, cacheLine) =
          lookupOopLineCache(line0Addr)

        val pendingHit =
          lookupOopPending(line0Addr)

        val ownPendingFree =
          !oopPendingValid(1)

        val newCacheMiss =
          cacheEligible &&
            !cacheHit &&
            !pendingHit &&
            ownPendingFree

        when(newCacheMiss) {
          oopMissWant(1) :=
            True
        }

        driveCopyFwd(
          io.CopyFwdPush,
          oopAddr,
          oopReadSize,
          push_data.oopCopyMeta
        )

        val reqMask =
          requestedByteMask(oopReadSize)

        val fullFwd =
          push_data.oopCopyMeta.needQuery &&
            (io.CopyFwdPush.mask & reqMask) === reqMask

        when(!io.CopyFwdPush.stall) {
          when(fullFwd) {
            push_data.fromObj :=
              decodeReadOopResp(
                io.CopyFwdPush.data
              )

            goto(READ_MW_REQ0)

          }.elsewhen(cacheEligible && cacheHit) {
            val logicalData =
              shiftLineToLogical(
                cacheLine,
                offsetNow
              )

            push_data.fromObj :=
              decodeReadOopResp(logicalData)

            goto(READ_MW_REQ0)

          }.elsewhen(cacheEligible && pendingHit) {
            // 等 owner fill Cache。
          }.elsewhen(
            !cacheEligible ||
              (newCacheMiss && oopMissGrant(1))
          ) {
            driveAlignedLineReadReq(
              io.PushMreq,
              line0Addr
            )

            when(io.PushMreq.Request.fire) {
              pushOopOffset :=
                offsetNow

              pushOopCross :=
                crossNow

              pushFwdMask :=
                io.CopyFwdPush.mask

              pushFwdData :=
                io.CopyFwdPush.data

              pushLineSrc0 :=
                io.PushMreq.Request.payload.RequestSourceID.resized

              pushLineSrc1Valid :=
                False

              pushLineResp0Valid :=
                False

              pushLineResp1Valid :=
                False

              when(cacheEligible) {
                oopPendingValid(1) :=
                  True

                oopPendingReady(1) :=
                  False

                oopPendingLineAddr(1) :=
                  line0Addr
              }

              when(crossNow) {
                goto(READ_OOP_REQ1)

              }.otherwise {
                goto(READ_OOP_WAIT)
              }
            }
          }
        }
      }
    }

    READ_OOP_REQ1.whenIsActive {
      val line1Addr =
        (
          alignToLine(push_data.task) +
            U(LineBytesNum, MMUAddrWidth bits)
        ).resize(MMUAddrWidth)

      driveAlignedLineReadReq(
        io.PushMreq,
        line1Addr
      )

      io.PushMreq.Response.ready :=
        True

      when(io.PushMreq.Response.fire) {
        val respSid =
          io.PushMreq.Response.payload.ResponseSourceID.resized

        when(
          !pushLineResp0Valid &&
            respSid === pushLineSrc0
        ) {
          pushLineData0 :=
            io.PushMreq.Response.payload.ResponseData

          pushLineResp0Valid :=
            True
        }
      }

      when(io.PushMreq.Request.fire) {
        pushLineSrc1 :=
          io.PushMreq.Request.payload.RequestSourceID.resized

        pushLineSrc1Valid :=
          True

        goto(READ_OOP_WAIT)
      }
    }

    READ_OOP_WAIT.whenIsActive {
      io.PushMreq.Response.ready :=
        True

      val respFire =
        io.PushMreq.Response.fire

      val respSid =
        io.PushMreq.Response.payload.ResponseSourceID.resized

      val (respIs0, respIs1) =
        classifyTwoLineResp(
          respFire,
          respSid,
          pushLineSrc0,
          pushLineSrc1,
          pushLineSrc1Valid,
          pushLineResp0Valid,
          pushLineResp1Valid
        )

      when(respIs0) {
        pushLineData0 :=
          io.PushMreq.Response.payload.ResponseData

        pushLineResp0Valid :=
          True
      }

      when(respIs1) {
        pushLineData1 :=
          io.PushMreq.Response.payload.ResponseData

        pushLineResp1Valid :=
          True
      }

      val got0Now =
        pushLineResp0Valid || respIs0

      val got1Now =
        !pushOopCross ||
          pushLineResp1Valid ||
          respIs1

      val line0Now =
        Mux(
          respIs0,
          io.PushMreq.Response.payload.ResponseData,
          pushLineData0
        )

      val line1Now =
        Mux(
          respIs1,
          io.PushMreq.Response.payload.ResponseData,
          pushLineData1
        )

      when(got0Now && got1Now) {
        val memoryLogical =
          Mux(
            pushOopCross,
            mergeTwoReadLines(
              line0Now,
              line1Now,
              pushOopOffset
            ),
            shiftLineToLogical(
              line0Now,
              pushOopOffset
            )
          )

        val logicalData =
          mergeCopyForward(
            memoryLogical,
            pushFwdMask,
            pushFwdData
          )

        when(oopPendingValid(1)) {
          oopPendingLineData(1) :=
            line0Now

          oopPendingReady(1) :=
            True
        }

        push_data.fromObj :=
          decodeReadOopResp(logicalData)

        goto(READ_MW_REQ0)
      }
    }

    READ_MW_REQ0.whenIsActive {
      val mwAddr =
        push_data.fromObj

      val line0Addr =
        alignToLine(mwAddr)

      val offsetNow =
        mwAddr(PreLineOffsetBits - 1 downto 0)

      val crossNow =
        readCrossesLine(
          mwAddr,
          mwReadSize
        )

      driveAlignedLineReadReq(
        io.PushMreq,
        line0Addr
      )

      when(io.PushMreq.Request.fire) {
        pushMwOffset :=
          offsetNow

        pushMwCross :=
          crossNow

        pushLineSrc0 :=
          io.PushMreq.Request.payload.RequestSourceID.resized

        pushLineSrc1Valid :=
          False

        pushLineResp0Valid :=
          False

        pushLineResp1Valid :=
          False

        when(crossNow) {
          goto(READ_MW_REQ1)

        }.otherwise {
          goto(READ_MW_WAIT)
        }
      }
    }

    READ_MW_REQ1.whenIsActive {
      val line1Addr =
        (
          alignToLine(push_data.fromObj) +
            U(LineBytesNum, MMUAddrWidth bits)
        ).resize(MMUAddrWidth)

      driveAlignedLineReadReq(
        io.PushMreq,
        line1Addr
      )

      io.PushMreq.Response.ready :=
        True

      when(io.PushMreq.Response.fire) {
        val respSid =
          io.PushMreq.Response.payload.ResponseSourceID.resized

        when(
          !pushLineResp0Valid &&
            respSid === pushLineSrc0
        ) {
          pushLineData0 :=
            io.PushMreq.Response.payload.ResponseData

          pushLineResp0Valid :=
            True
        }
      }

      when(io.PushMreq.Request.fire) {
        pushLineSrc1 :=
          io.PushMreq.Request.payload.RequestSourceID.resized

        pushLineSrc1Valid :=
          True

        goto(READ_MW_WAIT)
      }
    }

    READ_MW_WAIT.whenIsActive {
      io.PushMreq.Response.ready :=
        True

      val respFire =
        io.PushMreq.Response.fire

      val respSid =
        io.PushMreq.Response.payload.ResponseSourceID.resized

      val (respIs0, respIs1) =
        classifyTwoLineResp(
          respFire,
          respSid,
          pushLineSrc0,
          pushLineSrc1,
          pushLineSrc1Valid,
          pushLineResp0Valid,
          pushLineResp1Valid
        )

      when(respIs0) {
        pushLineData0 :=
          io.PushMreq.Response.payload.ResponseData

        pushLineResp0Valid :=
          True
      }

      when(respIs1) {
        pushLineData1 :=
          io.PushMreq.Response.payload.ResponseData

        pushLineResp1Valid :=
          True
      }

      val got0Now =
        pushLineResp0Valid || respIs0

      val got1Now =
        !pushMwCross ||
          pushLineResp1Valid ||
          respIs1

      val line0Now =
        Mux(
          respIs0,
          io.PushMreq.Response.payload.ResponseData,
          pushLineData0
        )

      val line1Now =
        Mux(
          respIs1,
          io.PushMreq.Response.payload.ResponseData,
          pushLineData1
        )

      when(got0Now && got1Now) {
        val rd =
          Mux(
            pushMwCross,
            mergeTwoReadLines(
              line0Now,
              line1Now,
              pushMwOffset
            ),
            shiftLineToLogical(
              line0Now,
              pushMwOffset
            )
          )

        when(mainIsIdle) {
          copyFetchContextWithoutMw(
            main_data,
            push_data
          )

          fillMwKlassLen(
            rd,
            main_data
          )

          mainGotoSend :=
            True

          goto(IDLE)

        }.otherwise {
          fillMwKlassLen(
            rd,
            push_data
          )

          goto(SEND)
        }
      }
    }

    SEND.whenIsActive {
      when(mainIsIdle) {
        main_data :=
          push_data

        mainGotoSend :=
          True

        goto(IDLE)
      }
    }
  }

  // ============================================================================
  // 4-way PreFetch workers
  //
  // worker i 固定绑定 PreMreq{i}。
  // 每个 worker 的 cross-line read:
  //   REQ0.fire -> REQ1 尽快 fire -> WAIT 按 SourceID 收集两个 Response。
  //
  // OOP Cache/MSHR:
  //   普通单-Line OOP 先查 Cache；
  //   miss 且其它 source 已有 pending 同 Line -> 原地等；
  //   miss 且无 pending -> 自己发 request，并占用 pending[2+i]。
  // ============================================================================
  val PreFetchWorkerNum      = 4
  val PreFetchWorkerIdxWidth = log2Up(PreFetchWorkerNum)

  require(PreFetchWorkerNum <= PreFetchBufferNum)
  require(preMreqs.length == PreFetchWorkerNum)

  val PRE_STAGE_IDLE      = U(0, 3 bits)
  val PRE_STAGE_OOP_REQ0  = U(1, 3 bits)
  val PRE_STAGE_OOP_REQ1  = U(2, 3 bits)
  val PRE_STAGE_OOP_WAIT  = U(3, 3 bits)
  val PRE_STAGE_MW_REQ0   = U(4, 3 bits)
  val PRE_STAGE_MW_REQ1   = U(5, 3 bits)
  val PRE_STAGE_MW_WAIT   = U(6, 3 bits)

  val preWorkerBusy = Vec.fill(PreFetchWorkerNum)(
    RegInit(False)
  )

  val preWorkerStage = Vec.fill(PreFetchWorkerNum)(
    RegInit(U(0, 3 bits))
  )

  val preWorkerSlot = Vec.fill(PreFetchWorkerNum)(
    RegInit(U(0, PreFetchBufferWidth bits))
  )

  val preWorkerFwdMask = Vec.fill(PreFetchWorkerNum)(
    RegInit(B(0, LineBytesNum bits))
  )

  val preWorkerFwdData = Vec.fill(PreFetchWorkerNum)(
    RegInit(U(0, MMUDataWidth bits))
  )

  val preWorkerOopOffset = Vec.fill(PreFetchWorkerNum)(
    RegInit(U(0, PreLineOffsetBits bits))
  )

  val preWorkerOopCross = Vec.fill(PreFetchWorkerNum)(
    RegInit(False)
  )

  val preWorkerMwOffset = Vec.fill(PreFetchWorkerNum)(
    RegInit(U(0, PreLineOffsetBits bits))
  )

  val preWorkerMwCross = Vec.fill(PreFetchWorkerNum)(
    RegInit(False)
  )

  // 每个 worker 一个逻辑 read 最多两个 SourceID / Response。
  val preWorkerLineSrc0 = Vec.fill(PreFetchWorkerNum)(
    RegInit(U(0, LLCSourceMaxNumBitSize bits))
  )

  val preWorkerLineSrc1 = Vec.fill(PreFetchWorkerNum)(
    RegInit(U(0, LLCSourceMaxNumBitSize bits))
  )

  val preWorkerLineSrc1Valid = Vec.fill(PreFetchWorkerNum)(
    RegInit(False)
  )

  val preWorkerLineResp0Valid = Vec.fill(PreFetchWorkerNum)(
    RegInit(False)
  )

  val preWorkerLineResp1Valid = Vec.fill(PreFetchWorkerNum)(
    RegInit(False)
  )

  val preWorkerLineData0 = Vec.fill(PreFetchWorkerNum)(
    RegInit(U(0, MMUDataWidth bits))
  )

  val preWorkerLineData1 = Vec.fill(PreFetchWorkerNum)(
    RegInit(U(0, MMUDataWidth bits))
  )

  val pushFollowWritePtr =
    RegInit(U(0, PreFetchBufferWidth bits))

  val preWorkerFreeVec =
    Bits(PreFetchWorkerNum bits)

  for (i <- 0 until PreFetchWorkerNum) {
    preWorkerFreeVec(i) :=
      !preWorkerBusy(i)
  }

  val preHasFreeWorker =
    preWorkerFreeVec.orR

  val preFreeWorkerIdx =
    PriorityMux(
      (0 until PreFetchWorkerNum).map(i =>
        (
          preWorkerFreeVec(i),
          U(i, PreFetchWorkerIdxWidth bits)
        )
      )
    )

  val allPreWorkersIdle =
    !preWorkerBusy.asBits.orR

  def slotOwnedByWorker(slot: UInt): Bool = {
    val hitVec =
      Bits(PreFetchWorkerNum bits)

    for (i <- 0 until PreFetchWorkerNum) {
      hitVec(i) :=
        preWorkerBusy(i) &&
          preWorkerSlot(i) === slot
    }

    hitVec.orR
  }

  def startPreWorker(
      workerIdx: UInt,
      slotIdx: UInt,
      payload: UInt
  ): Unit = {
    receiveTask(
      payload,
      preBuf(slotIdx)
    )

    preBuf(slotIdx).fromObj   := U(0, GCElementWidth bits)
    preBuf(slotIdx).markWord  := U(0, GCElementWidth bits)
    preBuf(slotIdx).klassPtr  := U(0, GCElementWidth bits)
    preBuf(slotIdx).srcLength := U(0, 32 bits)

    preBufValid(slotIdx) :=
      True

    preBufDone(slotIdx) :=
      False

    preWorkerBusy(workerIdx) :=
      True

    preWorkerStage(workerIdx) :=
      PRE_STAGE_OOP_REQ0

    preWorkerSlot(workerIdx) :=
      slotIdx

    preWorkerFwdMask(workerIdx) :=
      B(0, LineBytesNum bits)

    preWorkerFwdData(workerIdx) :=
      U(0, MMUDataWidth bits)

    preWorkerLineSrc1Valid(workerIdx) :=
      False

    preWorkerLineResp0Valid(workerIdx) :=
      False

    preWorkerLineResp1Valid(workerIdx) :=
      False
  }

  // --------------------------------------------------------------------------
  // PrePop allocation
  // --------------------------------------------------------------------------
  val stackPushFollowActive =
    io.toFetch.PushCount =/= U(0, 32 bits)

  val fetchPushFollowActive =
    pushFollowRem =/= U(0, 32 bits)

  when(
    !stackPushFollowActive &&
      !fetchPushFollowActive
  ) {
    val normalAllocSlot =
      buf_top

    val normalSlotFree =
      !preBufValid(normalAllocSlot) &&
        !slotOwnedByWorker(normalAllocSlot)

    io.toFetch.PrePop.ready :=
      preHasFreeWorker &&
        buf_free =/= U(0, buf_free.getWidth bits) &&
        normalSlotFree

    when(io.toFetch.PrePop.fire) {
      startPreWorker(
        preFreeWorkerIdx,
        normalAllocSlot,
        io.toFetch.PrePop.payload
      )

      buf_top :=
        bufInc(
          buf_top,
          U(1, PreFetchBufferWidth bits)
        )

      buf_count :=
        buf_count +
          U(1, buf_count.getWidth bits)
    }

  }.elsewhen(stackPushFollowActive) {
    val canStartPushFollow =
      preHasFreeWorker &&
        allPreWorkersIdle &&
        !waitForPrefetch

    io.toFetch.PrePop.ready :=
      canStartPushFollow

    when(io.toFetch.PrePop.fire) {
      val pushCount =
        Mux(
          io.toFetch.PushCount >
            U(PreFetchBufferNum, 32 bits),
          U(PreFetchBufferNum, 32 bits),
          io.toFetch.PushCount
        )

      val pushCountSmall =
        pushCount.resize(buf_count.getWidth)

      val firstSlot =
        bufDec(
          buf_bottom,
          pushCountSmall
        )

      when(buf_free >= pushCountSmall) {
        buf_count :=
          buf_count +
            U(1, buf_count.getWidth bits)

      }.otherwise {
        val dropNum =
          pushCountSmall - buf_free

        buf_top :=
          bufDec(
            buf_top,
            dropNum
          )

        buf_count :=
          (
            buf_count +
              buf_free -
              pushCountSmall +
              U(1, buf_count.getWidth bits)
          ).resized
      }

      buf_bottom :=
        firstSlot

      pushFollowWritePtr :=
        firstSlot

      pushFollowRem :=
        pushCount -
          U(1, 32 bits)

      startPreWorker(
        preFreeWorkerIdx,
        firstSlot,
        io.toFetch.PrePop.payload
      )
    }

  }.otherwise {
    val nextFollowSlot =
      bufInc(
        pushFollowWritePtr,
        U(1, PreFetchBufferWidth bits)
      )

    val nextFollowSlotSafe =
      !slotOwnedByWorker(nextFollowSlot) &&
        !(waitForPrefetch && waitPrefetchSlot === nextFollowSlot)

    io.toFetch.PrePop.ready :=
      preHasFreeWorker &&
        nextFollowSlotSafe

    when(io.toFetch.PrePop.fire) {
      pushFollowWritePtr :=
        nextFollowSlot

      pushFollowRem :=
        pushFollowRem -
          U(1, 32 bits)

      buf_count :=
        buf_count +
          U(1, buf_count.getWidth bits)

      startPreWorker(
        preFreeWorkerIdx,
        nextFollowSlot,
        io.toFetch.PrePop.payload
      )
    }
  }

  // --------------------------------------------------------------------------
  // Shared CopyFwdPre arbitration
  // --------------------------------------------------------------------------
  val preCopyReqVec =
    Bits(PreFetchWorkerNum bits)

  for (i <- 0 until PreFetchWorkerNum) {
    val slotIdx =
      preWorkerSlot(i)

    preCopyReqVec(i) :=
      preWorkerBusy(i) &&
        preWorkerStage(i) === PRE_STAGE_OOP_REQ0 &&
        preBuf(slotIdx).oopType =/= U(PartialArrayOop) &&
        preBuf(slotIdx).oopCopyMeta.needQuery
  }

  val criticalCopyReqVec =
    Bits(PreFetchWorkerNum bits)

  for (i <- 0 until PreFetchWorkerNum) {
    criticalCopyReqVec(i) :=
      preCopyReqVec(i) &&
        waitForPrefetch &&
        preWorkerSlot(i) === buf_bottom
  }

  val hasCriticalCopyReq =
    criticalCopyReqVec.orR

  val normalCopyGrant =
    Bits(PreFetchWorkerNum bits)

  normalCopyGrant(0) :=
    preCopyReqVec(0)

  normalCopyGrant(1) :=
    preCopyReqVec(1) &&
      !preCopyReqVec(0)

  normalCopyGrant(2) :=
    preCopyReqVec(2) &&
      !preCopyReqVec(0) &&
      !preCopyReqVec(1)

  normalCopyGrant(3) :=
    preCopyReqVec(3) &&
      !preCopyReqVec(0) &&
      !preCopyReqVec(1) &&
      !preCopyReqVec(2)

  val criticalCopyGrant =
    Bits(PreFetchWorkerNum bits)

  criticalCopyGrant(0) :=
    criticalCopyReqVec(0)

  criticalCopyGrant(1) :=
    criticalCopyReqVec(1) &&
      !criticalCopyReqVec(0)

  criticalCopyGrant(2) :=
    criticalCopyReqVec(2) &&
      !criticalCopyReqVec(0) &&
      !criticalCopyReqVec(1)

  criticalCopyGrant(3) :=
    criticalCopyReqVec(3) &&
      !criticalCopyReqVec(0) &&
      !criticalCopyReqVec(1) &&
      !criticalCopyReqVec(2)

  val preCopyGrant =
    Bits(PreFetchWorkerNum bits)

  for (i <- 0 until PreFetchWorkerNum) {
    preCopyGrant(i) :=
      Mux(
        hasCriticalCopyReq,
        criticalCopyGrant(i),
        normalCopyGrant(i)
      )
  }

  // --------------------------------------------------------------------------
  // Worker execution
  // --------------------------------------------------------------------------
  for (i <- 0 until PreFetchWorkerNum) {
    val m =
      preMreqs(i)

    val slotIdx =
      preWorkerSlot(i)

    val pendingIdx =
      2 + i

    val oopAddr =
      preBuf(slotIdx).task

    val oopLine0Addr =
      alignToLine(oopAddr)

    val oopOffsetNow =
      oopAddr(PreLineOffsetBits - 1 downto 0)

    val oopCrossNow =
      readCrossesLine(
        oopAddr,
        oopReadSize
      )

    val oopLine1Addr =
      (
        oopLine0Addr +
          U(LineBytesNum, MMUAddrWidth bits)
      ).resize(MMUAddrWidth)

    val mwAddr =
      preBuf(slotIdx).fromObj

    val mwLine0Addr =
      alignToLine(mwAddr)

    val mwOffsetNow =
      mwAddr(PreLineOffsetBits - 1 downto 0)

    val mwCrossNow =
      readCrossesLine(
        mwAddr,
        mwReadSize
      )

    val mwLine1Addr =
      (
        mwLine0Addr +
          U(LineBytesNum, MMUAddrWidth bits)
      ).resize(MMUAddrWidth)

    // ========================================================================
    // OOP REQ0
    // ========================================================================
    when(
      preWorkerBusy(i) &&
        preWorkerStage(i) === PRE_STAGE_OOP_REQ0
    ) {
      when(
        preBuf(slotIdx).oopType === U(PartialArrayOop)
      ) {
        preBuf(slotIdx).fromObj :=
          preBuf(slotIdx).task

        preWorkerStage(i) :=
          PRE_STAGE_MW_REQ0

      }.elsewhen(
        !preBuf(slotIdx).oopCopyMeta.needQuery
      ) {
        val (cacheHit, cacheLine) =
          lookupOopLineCache(oopLine0Addr)

        val pendingHit =
          lookupOopPending(oopLine0Addr)

        val ownPendingFree =
          !oopPendingValid(pendingIdx)

        val newCacheMiss =
          !oopCrossNow &&
            !cacheHit &&
            !pendingHit &&
            ownPendingFree

        when(newCacheMiss) {
          oopMissWant(pendingIdx) :=
            True
        }

        when(!oopCrossNow && cacheHit) {
          val logicalData =
            shiftLineToLogical(
              cacheLine,
              oopOffsetNow
            )

          preBuf(slotIdx).fromObj :=
            decodeReadOopResp(logicalData)

          preWorkerStage(i) :=
            PRE_STAGE_MW_REQ0

        }.elsewhen(!oopCrossNow && pendingHit) {
          // 等 owner fill Cache。
        }.elsewhen(
          oopCrossNow ||
            (newCacheMiss && oopMissGrant(pendingIdx))
        ) {
          driveAlignedLineReadReq(
            m,
            oopLine0Addr
          )

          when(m.Request.fire) {
            preWorkerOopOffset(i) :=
              oopOffsetNow

            preWorkerOopCross(i) :=
              oopCrossNow

            preWorkerFwdMask(i) :=
              B(0, LineBytesNum bits)

            preWorkerFwdData(i) :=
              U(0, MMUDataWidth bits)

            preWorkerLineSrc0(i) :=
              m.Request.payload.RequestSourceID.resized

            preWorkerLineSrc1Valid(i) :=
              False

            preWorkerLineResp0Valid(i) :=
              False

            preWorkerLineResp1Valid(i) :=
              False

            when(!oopCrossNow) {
              oopPendingValid(pendingIdx) :=
                True

              oopPendingReady(pendingIdx) :=
                False

              oopPendingLineAddr(pendingIdx) :=
                oopLine0Addr
            }

            when(oopCrossNow) {
              preWorkerStage(i) :=
                PRE_STAGE_OOP_REQ1

            }.otherwise {
              preWorkerStage(i) :=
                PRE_STAGE_OOP_WAIT
            }
          }
        }

      }.elsewhen(preCopyGrant(i)) {
        driveCopyFwd(
          io.CopyFwdPre,
          oopAddr,
          oopReadSize,
          preBuf(slotIdx).oopCopyMeta
        )

        val reqMask =
          requestedByteMask(oopReadSize)

        val fullFwd =
          (io.CopyFwdPre.mask & reqMask) === reqMask

        when(!io.CopyFwdPre.stall) {
          when(fullFwd) {
            preBuf(slotIdx).fromObj :=
              decodeReadOopResp(
                io.CopyFwdPre.data
              )

            preWorkerStage(i) :=
              PRE_STAGE_MW_REQ0

          }.otherwise {
            driveAlignedLineReadReq(
              m,
              oopLine0Addr
            )

            when(m.Request.fire) {
              preWorkerOopOffset(i) :=
                oopOffsetNow

              preWorkerOopCross(i) :=
                oopCrossNow

              preWorkerFwdMask(i) :=
                io.CopyFwdPre.mask

              preWorkerFwdData(i) :=
                io.CopyFwdPre.data

              preWorkerLineSrc0(i) :=
                m.Request.payload.RequestSourceID.resized

              preWorkerLineSrc1Valid(i) :=
                False

              preWorkerLineResp0Valid(i) :=
                False

              preWorkerLineResp1Valid(i) :=
                False

              when(oopCrossNow) {
                preWorkerStage(i) :=
                  PRE_STAGE_OOP_REQ1

              }.otherwise {
                preWorkerStage(i) :=
                  PRE_STAGE_OOP_WAIT
              }
            }
          }
        }
      }
    }

    // ========================================================================
    // OOP REQ1
    // ========================================================================
    when(
      preWorkerBusy(i) &&
        preWorkerStage(i) === PRE_STAGE_OOP_REQ1
    ) {
      driveAlignedLineReadReq(
        m,
        oopLine1Addr
      )

      m.Response.ready :=
        True

      when(m.Response.fire) {
        val respSid =
          m.Response.payload.ResponseSourceID.resized

        when(
          !preWorkerLineResp0Valid(i) &&
            respSid === preWorkerLineSrc0(i)
        ) {
          preWorkerLineData0(i) :=
            m.Response.payload.ResponseData

          preWorkerLineResp0Valid(i) :=
            True
        }
      }

      when(m.Request.fire) {
        preWorkerLineSrc1(i) :=
          m.Request.payload.RequestSourceID.resized

        preWorkerLineSrc1Valid(i) :=
          True

        preWorkerStage(i) :=
          PRE_STAGE_OOP_WAIT
      }
    }

    // ========================================================================
    // OOP WAIT
    // ========================================================================
    when(
      preWorkerBusy(i) &&
        preWorkerStage(i) === PRE_STAGE_OOP_WAIT
    ) {
      m.Response.ready :=
        True

      val respFire =
        m.Response.fire

      val respSid =
        m.Response.payload.ResponseSourceID.resized

      val (respIs0, respIs1) =
        classifyTwoLineResp(
          respFire,
          respSid,
          preWorkerLineSrc0(i),
          preWorkerLineSrc1(i),
          preWorkerLineSrc1Valid(i),
          preWorkerLineResp0Valid(i),
          preWorkerLineResp1Valid(i)
        )

      when(respIs0) {
        preWorkerLineData0(i) :=
          m.Response.payload.ResponseData

        preWorkerLineResp0Valid(i) :=
          True
      }

      when(respIs1) {
        preWorkerLineData1(i) :=
          m.Response.payload.ResponseData

        preWorkerLineResp1Valid(i) :=
          True
      }

      val got0Now =
        preWorkerLineResp0Valid(i) ||
          respIs0

      val got1Now =
        !preWorkerOopCross(i) ||
          preWorkerLineResp1Valid(i) ||
          respIs1

      val line0Now =
        Mux(
          respIs0,
          m.Response.payload.ResponseData,
          preWorkerLineData0(i)
        )

      val line1Now =
        Mux(
          respIs1,
          m.Response.payload.ResponseData,
          preWorkerLineData1(i)
        )

      when(got0Now && got1Now) {
        val memoryLogical =
          Mux(
            preWorkerOopCross(i),
            mergeTwoReadLines(
              line0Now,
              line1Now,
              preWorkerOopOffset(i)
            ),
            shiftLineToLogical(
              line0Now,
              preWorkerOopOffset(i)
            )
          )

        val logicalData =
          mergeCopyForward(
            memoryLogical,
            preWorkerFwdMask(i),
            preWorkerFwdData(i)
          )

        when(oopPendingValid(pendingIdx)) {
          oopPendingLineData(pendingIdx) :=
            line0Now

          oopPendingReady(pendingIdx) :=
            True
        }

        preBuf(slotIdx).fromObj :=
          decodeReadOopResp(logicalData)

        preWorkerStage(i) :=
          PRE_STAGE_MW_REQ0
      }
    }

    // ========================================================================
    // MW REQ0
    // ========================================================================
    when(
      preWorkerBusy(i) &&
        preWorkerStage(i) === PRE_STAGE_MW_REQ0
    ) {
      driveAlignedLineReadReq(
        m,
        mwLine0Addr
      )

      when(m.Request.fire) {
        preWorkerMwOffset(i) :=
          mwOffsetNow

        preWorkerMwCross(i) :=
          mwCrossNow

        preWorkerLineSrc0(i) :=
          m.Request.payload.RequestSourceID.resized

        preWorkerLineSrc1Valid(i) :=
          False

        preWorkerLineResp0Valid(i) :=
          False

        preWorkerLineResp1Valid(i) :=
          False

        when(mwCrossNow) {
          preWorkerStage(i) :=
            PRE_STAGE_MW_REQ1

        }.otherwise {
          preWorkerStage(i) :=
            PRE_STAGE_MW_WAIT
        }
      }
    }

    // ========================================================================
    // MW REQ1
    // ========================================================================
    when(
      preWorkerBusy(i) &&
        preWorkerStage(i) === PRE_STAGE_MW_REQ1
    ) {
      driveAlignedLineReadReq(
        m,
        mwLine1Addr
      )

      m.Response.ready :=
        True

      when(m.Response.fire) {
        val respSid =
          m.Response.payload.ResponseSourceID.resized

        when(
          !preWorkerLineResp0Valid(i) &&
            respSid === preWorkerLineSrc0(i)
        ) {
          preWorkerLineData0(i) :=
            m.Response.payload.ResponseData

          preWorkerLineResp0Valid(i) :=
            True
        }
      }

      when(m.Request.fire) {
        preWorkerLineSrc1(i) :=
          m.Request.payload.RequestSourceID.resized

        preWorkerLineSrc1Valid(i) :=
          True

        preWorkerStage(i) :=
          PRE_STAGE_MW_WAIT
      }
    }

    // ========================================================================
    // MW WAIT
    // ========================================================================
    when(
      preWorkerBusy(i) &&
        preWorkerStage(i) === PRE_STAGE_MW_WAIT
    ) {
      m.Response.ready :=
        True

      val respFire =
        m.Response.fire

      val respSid =
        m.Response.payload.ResponseSourceID.resized

      val (respIs0, respIs1) =
        classifyTwoLineResp(
          respFire,
          respSid,
          preWorkerLineSrc0(i),
          preWorkerLineSrc1(i),
          preWorkerLineSrc1Valid(i),
          preWorkerLineResp0Valid(i),
          preWorkerLineResp1Valid(i)
        )

      when(respIs0) {
        preWorkerLineData0(i) :=
          m.Response.payload.ResponseData

        preWorkerLineResp0Valid(i) :=
          True
      }

      when(respIs1) {
        preWorkerLineData1(i) :=
          m.Response.payload.ResponseData

        preWorkerLineResp1Valid(i) :=
          True
      }

      val got0Now =
        preWorkerLineResp0Valid(i) ||
          respIs0

      val got1Now =
        !preWorkerMwCross(i) ||
          preWorkerLineResp1Valid(i) ||
          respIs1

      val line0Now =
        Mux(
          respIs0,
          m.Response.payload.ResponseData,
          preWorkerLineData0(i)
        )

      val line1Now =
        Mux(
          respIs1,
          m.Response.payload.ResponseData,
          preWorkerLineData1(i)
        )

      when(got0Now && got1Now) {
        val rd =
          Mux(
            preWorkerMwCross(i),
            mergeTwoReadLines(
              line0Now,
              line1Now,
              preWorkerMwOffset(i)
            ),
            shiftLineToLogical(
              line0Now,
              preWorkerMwOffset(i)
            )
          )

        val currentFromObj =
          preBuf(slotIdx).fromObj

        val finalMw =
          resolveForwardMark(
            currentFromObj,
            rd(GCElementWidth - 1 downto 0)
          )

        preBuf(slotIdx).markWord :=
          finalMw

        fillKlassLen(
          rd,
          preBuf(slotIdx)
        )

        preBufDone(slotIdx) :=
          True

        preWorkerBusy(i) :=
          False

        preWorkerStage(i) :=
          PRE_STAGE_IDLE
      }
    }
  }

  // ============================================================================
  // OOP Line Cache fill/update
  //
  // pending response 可以同周期到达多个；它们先各自保存在 pendingLineData。
  // Cache 每拍 drain 一个 ready pending entry，因此不会丢 fill，也不会导致
  // waiter 因“owner pending 已清但 Cache 没写入”而重新重复发请求。
  //
  // 不再根据 main/push/pre 短暂 idle 自动 flush。
  // Cache 仅在 reset 显式失效。
  // ============================================================================
  val oopPendingReadyVec =
    Bits(OopPendingNum bits)

  for (i <- 0 until OopPendingNum) {
    oopPendingReadyVec(i) :=
      oopPendingValid(i) &&
        oopPendingReady(i)
  }

  val oopPendingFillAny =
    oopPendingReadyVec.orR

  val oopPendingFillIdx =
    UInt(OopPendingIdxWidth bits)

  oopPendingFillIdx :=
    PriorityMux(
      (0 until OopPendingNum).map(i =>
        (
          oopPendingReadyVec(i),
          U(i, OopPendingIdxWidth bits)
        )
      )
    )

  val selectedOopFillAddr =
    oopPendingLineAddr(oopPendingFillIdx)

  val selectedOopFillData =
    oopPendingLineData(oopPendingFillIdx)

  val oopCacheExistingHitVec =
    Bits(OopLineCacheEntries bits)

  for (i <- 0 until OopLineCacheEntries) {
    oopCacheExistingHitVec(i) :=
      oopLineCacheValid(i) &&
        oopLineCacheTag(i) === selectedOopFillAddr
  }

  val oopCacheExistingHit =
    oopCacheExistingHitVec.orR

  val oopCacheExistingIdx =
    OHToUInt(oopCacheExistingHitVec)

  when(oopPendingFillAny) {
    when(oopCacheExistingHit) {
      oopLineCacheData(oopCacheExistingIdx) :=
        selectedOopFillData

    }.otherwise {
      oopLineCacheValid(oopLineCacheReplacePtr) :=
        True

      oopLineCacheTag(oopLineCacheReplacePtr) :=
        selectedOopFillAddr

      oopLineCacheData(oopLineCacheReplacePtr) :=
        selectedOopFillData

      oopLineCacheReplacePtr :=
        oopLineCacheReplacePtr +
          U(1, OopLineCacheIdxWidth bits)
    }

    oopPendingValid(oopPendingFillIdx) :=
      False

    oopPendingReady(oopPendingFillIdx) :=
      False
  }

  // 转发通知修补已固化的 MarkWord
  // 此代码块放在所有 FSM 之后，以确保同周期的陈旧 MMU 响应不会覆盖Copy2Survivor 的转发通知。resolveForwardMark() 也在 SEND 中调用，
  // 覆盖了与分发并发到达的转发通知
  when(incomingFwdValid) {
    when(main_data.fromObj === incomingFwdObj) {
      main_data.markWord := incomingFwdValue
    }

    when(push_data.fromObj === incomingFwdObj) {
      push_data.markWord := incomingFwdValue
    }

    for (i <- 0 until PreFetchBufferNum) {
      when(preBufValid(i) && preBufDone(i) && preBuf(i).fromObj === incomingFwdObj) {
        preBuf(i).markWord := incomingFwdValue
      }
    }
  }

  mainIsIdle := mainFsm.isActive(mainFsm.IDLE)
  mainIsWaitDone := mainFsm.isActive(mainFsm.WAIT_DONE)
  pushIsIdle := pushFsm.isActive(pushFsm.IDLE)
}

object GCFetchVerilog extends App {
  Config.spinal.generateVerilog(
    new GCFetch()
  )
}