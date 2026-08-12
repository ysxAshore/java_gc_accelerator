package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, MyStateMachine}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class SlotCtx() extends Bundle with GCTopParameters {
  val task                 = UInt(GCElementWidth bits)
  val srcOopPtr            = UInt(GCElementWidth bits)
  val markWord             = UInt(GCElementWidth bits)
  val klassPtr             = UInt(GCElementWidth bits)
  val srcLength            = UInt(32 bits)

  val srcRegionAttr        = UInt(16 bits)
  val destOopPtr           = UInt(GCElementWidth bits)
  val heapRegion           = UInt(GCElementWidth bits)
  val heapRegionHumongous  = Bool()
  val heapRegionReady      = Bool()
  val heapLookupPhase      = UInt(1 bits) // 0: heap-region ptr, 1: humongous flag

  val fromMarkWord         = Bool()
  val destRegionAttr       = UInt(16 bits)
}

class GCOopProcess extends Module with HWParameters with GCTopParameters with GCParameters {
  val io = new Bundle {
    val Mreq0                = master(new LocalMMUIO)
    val Mreq1                = master(new LocalMMUIO)

    val Process2Aop          = master(new GCToAop)
    val Fetch2Process        = slave(new GCToProcessUnit)
    val gcWriteSrcOopPtr     = slave(new GCWriteSrcOopPtr)
    val Process2CopySurvivor = master(new GCToSurvivor)

    val ConfigIO             = slave(new GCOopProcessConfigIO)
    val DebugTimeStamp       = in UInt(64 bits)

    val SlotIsEmpty          = out Bool()
  }

  def clearMreq(m: LocalMMUIO): Unit = {
    m.Request.valid := False
    m.Request.payload.clearAll()
    m.Response.ready := True
  }

  clearMreq(io.Mreq0)
  clearMreq(io.Mreq1)

  io.Process2CopySurvivor.clearOut()
  io.Process2Aop.clearOut()

  // slot 状态 放前面 不然 会报错NullPointer
  val slotValid = Vec.fill(2)(RegInit(False))
  val slotCtx   = Vec.fill(2)(Reg(SlotCtx()) init SlotCtx().getZero)

  // 将更新广播到仍在 OopProcess 中、但尚未真正送入 Copy2Survivor 的任务
  val incomingFwdValid = io.gcWriteSrcOopPtr.writeForward.valid
  val incomingFwdObj   = io.gcWriteSrcOopPtr.writeForward.payload.srcOopPtr
  val incomingFwdValue = io.gcWriteSrcOopPtr.writeForward.payload.writeValue

  def isForwardedMark(mark: UInt): Bool = (mark & U(3, GCElementWidth bits)) === U(3, GCElementWidth bits)

  val slotLiveFwdHit       = Vec.fill(2)(Bool())
  val slotEffectiveMarkWord = Vec.fill(2)(UInt(GCElementWidth bits))

  for (i <- 0 until 2) {
    slotLiveFwdHit(i) := incomingFwdValid && slotValid(i) && slotCtx(i).srcOopPtr === incomingFwdObj

    slotEffectiveMarkWord(i) := Mux(
      slotLiveFwdHit(i),
      incomingFwdValue,
      slotCtx(i).markWord
    )
  }

  val slotStart        = Vec.fill(2)(Bool())
  val slotGotoIdle     = Vec.fill(2)(Bool())
  val slotReleaseFetch = Vec.fill(2)(Bool()) // 当前Slot 允许前级 Fetch 释放任务
  // 这个默认赋值放条件赋值前面 不然会 和条件 冲突 ASSIGN OVERFLEAP
  for (i <- 0 until 2) {
    slotStart(i)        := False
    slotGotoIdle(i)     := False
    slotReleaseFetch(i) := False
  }


  def slotMreq(i: Int): LocalMMUIO = if (i == 0) io.Mreq0 else io.Mreq1
  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCOopProcess<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }
  def regionAttrAddrOf(i: Int): UInt = (io.ConfigIO.RegionAttrBiasedBase + ((slotCtx(i).srcOopPtr >> io.ConfigIO.RegionAttrShiftBy) << U(1))).resize(MMUAddrWidth)
  def destRegionAttrAddrOf(i: Int): UInt = (io.ConfigIO.RegionAttrBiasedBase + ((slotCtx(i).destOopPtr >> io.ConfigIO.RegionAttrShiftBy) << U(1))).resize(MMUAddrWidth)
  def heapRegionLookupAddrOf(i: Int): UInt = (io.ConfigIO.HeapRegionBiasedBase + ((slotCtx(i).task >> io.ConfigIO.HeapRegionShiftBy) << U(3))).resize(MMUAddrWidth)
  def writeBackObjOf(i: Int): UInt = {
    Mux(io.ConfigIO.UseCompressedOop,
      ((slotCtx(i).destOopPtr - io.ConfigIO.CompressedOopBase) >> io.ConfigIO.CompressedOopShift).resize(GCElementWidth),
      slotCtx(i).destOopPtr)
  }
  def writeBackSize(): UInt = Mux(io.ConfigIO.UseCompressedOop, U(4), U(8))
  def clearSlotRuntime(i: Int): Unit = {
    slotCtx(i).srcRegionAttr        := 0
    slotCtx(i).destOopPtr           := 0
    slotCtx(i).heapRegion           := 0
    slotCtx(i).heapRegionHumongous  := False
    slotCtx(i).heapRegionReady      := False
    slotCtx(i).heapLookupPhase      := 0
    slotCtx(i).fromMarkWord         := False
    slotCtx(i).destRegionAttr       := 0

    slotCopy2SurvivorDone(i)          := False
    slotCopy2SurvivorInflight(i)      := False
    slotCopy2SurvivorBypassGranted(i) := False
    slotNeedCopyReq(i)                := False
  }
  def allocToSlot(i: Int): Unit = {
    val allocFwdHit = incomingFwdValid && io.Fetch2Process.cmd.payload.SrcOopPtr === incomingFwdObj

    slotValid(i) := True

    slotCtx(i).task := io.Fetch2Process.cmd.payload.Task
    slotCtx(i).markWord := Mux(
      allocFwdHit,
      incomingFwdValue,
      io.Fetch2Process.cmd.payload.MarkWord
    )
    slotCtx(i).klassPtr  := io.Fetch2Process.cmd.payload.KlassPtr
    slotCtx(i).srcOopPtr := io.Fetch2Process.cmd.payload.SrcOopPtr
    slotCtx(i).srcLength := io.Fetch2Process.cmd.payload.SrcLength

    clearSlotRuntime(i)
    slotStart(i) := True

    dbg(Seq("Allocate task to slot", i.toString, ", srcOopPtr=", io.Fetch2Process.cmd.payload.SrcOopPtr))
  }
  def finishSlot(i: Int): Unit = {
    slotValid(i) := False
    clearSlotRuntime(i)
    slotGotoIdle(i) := True

    dbg(Seq("Finish slot", i.toString))
  }
  def releaseFetchFromSlotDyn(i: UInt): Unit = {
    when(i === U(0)) {
      slotReleaseFetch(0) := True
    } otherwise {
      slotReleaseFetch(1) := True
    }
  }

  // shared small caches
  // 两个 slot 共用一份 cache。 cache fill 写口采用固定优先级：slot0 > slot1。
  // 当 slot0 和 slot1 同周期都 miss 且都要 fill 时，只 fill slot0。 slot1 的当前返回数据已经写入 slotCtx(1)，只是这次不更新 cache。
  val regionAttrCacheEntries = 8
  val regionAttrCacheValid   = Vec.fill(regionAttrCacheEntries)(RegInit(False))
  val regionAttrCacheTag     = Vec.fill(regionAttrCacheEntries)(RegInit(U(0, MMUAddrWidth bits)))
  val regionAttrCache        = Vec.fill(regionAttrCacheEntries)(RegInit(U(0, 16 bits)))
  val regionAttrCacheReplacePtr = RegInit(U(0, log2Up(regionAttrCacheEntries) bits))

  val heapRegionCacheEntries = 4
  val heapRegionCacheValid   = Vec.fill(heapRegionCacheEntries)(RegInit(False))
  val heapRegionCacheTag     = Vec.fill(heapRegionCacheEntries)(RegInit(U(0, MMUAddrWidth bits)))
  val heapRegionCache        = Vec.fill(heapRegionCacheEntries)(RegInit(False))
  val heapRegionCacheReplacePtr = RegInit(U(0, log2Up(heapRegionCacheEntries) bits))

  val srcRegionAttrAddr     = Vec.fill(2)(UInt(MMUAddrWidth bits))
  val destRegionAttrAddr    = Vec.fill(2)(UInt(MMUAddrWidth bits))
  val heapRegionLookupAddr  = Vec.fill(2)(UInt(MMUAddrWidth bits))
  val srcRegionAttrHit       = Vec.fill(2)(Bool())
  val srcRegionAttrHitIndex  = Vec.fill(2)(UInt(log2Up(regionAttrCacheEntries) bits))
  val destRegionAttrHit      = Vec.fill(2)(Bool())
  val destRegionAttrHitIndex = Vec.fill(2)(UInt(log2Up(regionAttrCacheEntries) bits))

  val heapRegionHit         = Vec.fill(2)(Bool())
  val heapRegionHitIndex    = Vec.fill(2)(UInt(log2Up(heapRegionCacheEntries) bits))

  for (i <- 0 until 2) {
    srcRegionAttrAddr(i)    := regionAttrAddrOf(i)
    destRegionAttrAddr(i)   := destRegionAttrAddrOf(i)
    heapRegionLookupAddr(i) := heapRegionLookupAddrOf(i)

    val srcRegionHitVec = Vec.fill(regionAttrCacheEntries)(Bool())
    val destRegionHitVec = Vec.fill(regionAttrCacheEntries)(Bool())
    for (j <- 0 until regionAttrCacheEntries) {
      srcRegionHitVec(j)  := regionAttrCacheValid(j) && regionAttrCacheTag(j) === srcRegionAttrAddr(i)
      destRegionHitVec(j) := regionAttrCacheValid(j) && regionAttrCacheTag(j) === destRegionAttrAddr(i)
    }

    srcRegionAttrHit(i)       := srcRegionHitVec.orR
    srcRegionAttrHitIndex(i)  := OHToUInt(srcRegionHitVec.asBits)
    destRegionAttrHit(i)      := destRegionHitVec.orR
    destRegionAttrHitIndex(i) := OHToUInt(destRegionHitVec.asBits)

    val heapHitVec = Vec.fill(heapRegionCacheEntries)(Bool())
    for (j <- 0 until heapRegionCacheEntries) {
      heapHitVec(j) := heapRegionCacheValid(j) && heapRegionCacheTag(j) === heapRegionLookupAddr(i)
    }

    heapRegionHit(i)      := heapHitVec.orR
    heapRegionHitIndex(i) := OHToUInt(heapHitVec.asBits)
  }

  val regionAttrFillValid = Vec.fill(2)(Bool())
  val regionAttrFillAddr  = Vec.fill(2)(UInt(MMUAddrWidth bits))
  val regionAttrFillData  = Vec.fill(2)(UInt(16 bits))

  val heapRegionFillValid = Vec.fill(2)(Bool())
  val heapRegionFillAddr  = Vec.fill(2)(UInt(MMUAddrWidth bits))
  val heapRegionFillData  = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    regionAttrFillValid(i) := False
    regionAttrFillAddr(i)  := U(0, MMUAddrWidth bits)
    regionAttrFillData(i)  := U(0, 16 bits)

    heapRegionFillValid(i) := False
    heapRegionFillAddr(i)  := U(0, MMUAddrWidth bits)
    heapRegionFillData(i)  := False
  }

    val slotCopy2SurvivorDone          = Vec.fill(2)(RegInit(False)) // CopySurvivor 已经返回 最终的 destOopPtr
  val slotCopy2SurvivorInflight      = Vec.fill(2)(RegInit(False)) // 已经向 CopySurvivor 发出请求，正在等待返回
  val slotCopy2SurvivorBypassGranted = Vec.fill(2)(RegInit(False)) // 某些 type array 可以提前释放 Fetch，避免阻塞前级
  // Copy2Survivor 请求还未 fire。该请求与 heap-region 查询并行。
  val slotNeedCopyReq                = Vec.fill(2)(RegInit(False))

  // allowSecondInFlight = True 表示已经有某个 slot 提前对 Fetch 发过 Done， 因此允许 Fetch 再送一个任务进入另一个空 slot
  val allowSecondInFlight = RegInit(False)

  val pipeEmpty = !slotValid.orR
  val hasFreeSlot = !slotValid.andR
  val fetchReleasePulse = slotReleaseFetch.orR
  val fetchAccept = io.Fetch2Process.cmd.fire

  // 如果两个 slot 都空，直接 ready; 如果 pipeline 非空, 必须 allowSecondInFlight=True， 才允许 Fetch 再发一个任务进来
  io.SlotIsEmpty := pipeEmpty
  io.Fetch2Process.cmd.ready := hasFreeSlot && (pipeEmpty || allowSecondInFlight)
  io.Fetch2Process.Done := fetchReleasePulse

  // Process2CopySurvivor isTypeArray capture (不需要复制 可以提前释放)
  when(io.Process2CopySurvivor.done.payload.isTypeArray) {
    val isTypeArrayIdx = io.Process2CopySurvivor.done.payload.DoneOwner

    when(slotCopy2SurvivorInflight(isTypeArrayIdx) && !slotCopy2SurvivorBypassGranted(isTypeArrayIdx)) {
      releaseFetchFromSlotDyn(isTypeArrayIdx)
      slotCopy2SurvivorBypassGranted(isTypeArrayIdx) := True
    }
  }

  // Process2CopySurvivor Done capture
  when(io.Process2CopySurvivor.done.valid) {
    val doneOwner = io.Process2CopySurvivor.done.payload.DoneOwner

    slotCtx(doneOwner).destOopPtr := io.Process2CopySurvivor.done.payload.DestOopPtr
    slotCopy2SurvivorDone(doneOwner) := True
    slotCopy2SurvivorInflight(doneOwner) := False

    dbg(Seq("Copy2Survivor done for slot", doneOwner))
  }


  when(fetchAccept) {
    when(!slotValid(0)) {
      allocToSlot(0)
    } otherwise {
      allocToSlot(1)
    }
  }

  // 更新已经进入 OopProcess 的任务
  //
  // slotCopy2SurvivorInflight=True 时，任务已经真正 fire 给 Copy2Survivor，
  // 此后不再尝试回退 OopProcess 状态；正确性由 Copy2Survivor 的 CAS 保证。
  // 这里更新 markWord 本身是安全的，但只会影响尚未 fire 的状态和调试可见值
  when(incomingFwdValid) {
    for (i <- 0 until 2) {
      when(slotValid(i) && slotCtx(i).srcOopPtr === incomingFwdObj) {
        slotCtx(i).markWord := incomingFwdValue
      }
    }
  }

  // allowSecondInFlight 的集中更新 (应该不会有同周期的 fetchReleasePulse 和 fetchAccept 均有效)
  // 如果本周期有 slotReleaseFetch，则允许 Fetch 后续再发一个任务
  // 如果本周期只是 Fetch 被接收，则消耗这个 token
  when(fetchReleasePulse) {
    allowSecondInFlight := True
  } elsewhen fetchAccept {
    allowSecondInFlight := False
  }

  // slot FSM visibility for shared-output arbitration
  val slotIsWaitAop = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    val m = slotMreq(i)

    val slotFsm = new MyStateMachine {
      val IDLE              = new State with EntryPoint
      val READ_SRC_ATTR     = new State
      // heap ptr / humongous / wait-copy 三段合成一个 phase-based join state。
      val RESOLVE_HEAP_COPY = new State
      val WRITE_BACK        = new State
      val READ_DEST_ATTR    = new State
      val SEND_AOP          = new State

      def resolveSrcRegionAttr(attr: UInt): Unit = {
        slotCtx(i).srcRegionAttr := attr

        val srcRegionAttrType = attr(15 downto 8).asSInt
        when(srcRegionAttrType < S(0, 8 bits)) {
          releaseFetchFromSlotDyn(U(i, 1 bits))
          finishSlot(i)

        } otherwise {
          val currentMarkWord = slotEffectiveMarkWord(i)

          when(isForwardedMark(currentMarkWord)) {
            slotCtx(i).destOopPtr   := currentMarkWord & ~U(3, GCElementWidth bits)
            slotCtx(i).fromMarkWord := True

            releaseFetchFromSlotDyn(U(i, 1 bits))
            goto(WRITE_BACK)

            dbg(Seq("slot", i.toString, " use fromMarkWord path"))

          } otherwise {
            slotCtx(i).fromMarkWord := False
            slotNeedCopyReq(i)      := True
            slotCtx(i).heapRegionReady := False
            slotCtx(i).heapLookupPhase := U(0)

            // Copy2Survivor 请求与 heap-region 查询没有数据依赖，直接并行。
            goto(RESOLVE_HEAP_COPY)

            dbg(Seq("slot", i.toString, " start copy2survivor and heap lookup in parallel"))
          }
        }
      }

      def gotoDestAttrOrAop(): Unit = {
        when(destRegionAttrHit(i)) {
          slotCtx(i).destRegionAttr := regionAttrCache(destRegionAttrHitIndex(i))
          goto(SEND_AOP)
        } otherwise {
          goto(READ_DEST_ATTR)
        }
      }

      def joinHeapAndCopy(hum: Bool): Unit = {
        slotCtx(i).heapRegionHumongous := hum
        slotCtx(i).heapRegionReady := True

        when(slotCtx(i).fromMarkWord && !slotCopy2SurvivorInflight(i) && !slotNeedCopyReq(i)) {
          // late forwarding 或 forwarded-mark 路径：heap 信息一到即可继续。
          slotCtx(i).fromMarkWord := False

          when(hum) {
            finishSlot(i)
          } otherwise {
            gotoDestAttrOrAop()
          }

        }.elsewhen(slotCopy2SurvivorDone(i)) {
          val needRelease = !slotCopy2SurvivorBypassGranted(i)

          when(needRelease) {
            releaseFetchFromSlotDyn(U(i, 1 bits))
          }

          slotCopy2SurvivorDone(i) := False
          slotCopy2SurvivorBypassGranted(i) := False
          goto(WRITE_BACK)
        }
      }

      always {
        when(slotGotoIdle(i)) {
          goto(IDLE)

        }.elsewhen(slotStart(i)) {
          goto(READ_SRC_ATTR)

        }.elsewhen(
          slotNeedCopyReq(i) &&
          !slotCopy2SurvivorInflight(i) &&
          isForwardedMark(slotEffectiveMarkWord(i))
        ) {
          // Copy2Survivor.cmd 还没有 fire 时如果收到 forwarding pointer：
          // 取消 pending copy。heap lookup 可能已经在进行，不回退该内存访问。
          slotNeedCopyReq(i)      := False
          slotCtx(i).destOopPtr   := slotEffectiveMarkWord(i) & ~U(3, GCElementWidth bits)
          slotCtx(i).fromMarkWord := True

          releaseFetchFromSlotDyn(U(i, 1 bits))

          dbg(Seq(
            "slot", i.toString,
            " receives late forwarding before Copy2Survivor fire, cancel pending copy"
          ))
        }
      }

      READ_SRC_ATTR.whenIsActive {
        when(srcRegionAttrHit(i)) {
          resolveSrcRegionAttr(regionAttrCache(srcRegionAttrHitIndex(i)))

        } otherwise {
          // 直接在 read response 中完成 DECIDE，消掉一个纯控制状态。
          issueReq(m, srcRegionAttrAddr(i), False, U(2), U(0), True, False, issued) { rd =>
            val attr = rd(15 downto 0)

            regionAttrFillValid(i) := True
            regionAttrFillAddr(i)  := srcRegionAttrAddr(i)
            regionAttrFillData(i)  := attr

            resolveSrcRegionAttr(attr)
          }
        }
      }

      RESOLVE_HEAP_COPY.whenIsActive {
        when(slotCtx(i).heapRegionReady) {
          // heap lookup 已完成，当前 state 直接作为 Copy/late-forwarding join point。
          joinHeapAndCopy(slotCtx(i).heapRegionHumongous)

        }.elsewhen(heapRegionHit(i)) {
          // cache hit 不再经过额外 WAIT state。
          joinHeapAndCopy(heapRegionCache(heapRegionHitIndex(i)))

        }.elsewhen(slotCtx(i).heapLookupPhase === U(0)) {
          issueDirectRead(m, heapRegionLookupAddr(i), U(8), RESOLVE_HEAP_COPY) { rd =>
            slotCtx(i).heapRegion := rd(GCElementWidth - 1 downto 0)
            slotCtx(i).heapLookupPhase := U(1)
          }

        } otherwise {
          val humAddr = (slotCtx(i).heapRegion.resize(MMUAddrWidth) + U"xbc").resize(MMUAddrWidth)

          issueReq(m, humAddr, False, U(4), U(0), True, False, issued) { rd =>
            val hum = (rd(31 downto 0) & U(2, 32 bits)) =/= U(0)

            heapRegionFillValid(i) := True
            heapRegionFillAddr(i)  := heapRegionLookupAddr(i)
            heapRegionFillData(i)  := hum

            joinHeapAndCopy(hum)
          }
        }
      }

      WRITE_BACK.whenIsActive {
        issueReq(m, slotCtx(i).task.resize(MMUAddrWidth), True, writeBackSize(), writeBackObjOf(i), False, False, issued) { _ =>}

        when(issued) {
          issued := False

          val sameRegion = ((slotCtx(i).task ^ slotCtx(i).destOopPtr) >> io.ConfigIO.LogOfHRGrainBytes) === U(0)

          when(slotCtx(i).fromMarkWord){
            when(sameRegion){
              finishSlot(i)
            }.otherwise{
              // forwarded-mark 路径在 write-back 后才需要 heap 信息。
              slotCtx(i).heapRegionReady := False
              slotCtx(i).heapLookupPhase := U(0)
              goto(RESOLVE_HEAP_COPY)
            }

          }.otherwise {
            when(sameRegion || slotCtx(i).heapRegionHumongous) {
              finishSlot(i)

            } otherwise {
              // destination attr cache hit 时直接去 AOP，省掉 READ_DEST_ATTR 状态周期。
              gotoDestAttrOrAop()
            }
          }
        }
      }

      READ_DEST_ATTR.whenIsActive {
        when(destRegionAttrHit(i)) {
          slotCtx(i).destRegionAttr := regionAttrCache(destRegionAttrHitIndex(i))
          goto(SEND_AOP)

        } otherwise {
          // 目的 region attr 与源 region attr 共用同一份 cache，避免重复 2-byte MMU read。
          issueDirectRead(m, destRegionAttrAddr(i), U(2), SEND_AOP) { rd =>
            slotCtx(i).destRegionAttr := rd(15 downto 0)

            regionAttrFillValid(i) := True
            regionAttrFillAddr(i)  := destRegionAttrAddr(i)
            regionAttrFillData(i)  := rd(15 downto 0)
          }
        }
      }
    }

    slotIsWaitAop(i) := slotFsm.isActive(slotFsm.SEND_AOP)
  }

  // Shared regionAttrCache fill
  // Reg Vec 可以同周期写不同 entry，因此两个 slot 同周期返回时不再丢弃 slot1 fill。
  when(regionAttrFillValid(0) && regionAttrFillValid(1)) {
    when(regionAttrFillAddr(0) === regionAttrFillAddr(1)) {
      regionAttrCacheValid(regionAttrCacheReplacePtr) := True
      regionAttrCacheTag(regionAttrCacheReplacePtr)   := regionAttrFillAddr(0)
      regionAttrCache(regionAttrCacheReplacePtr)      := regionAttrFillData(0)
      regionAttrCacheReplacePtr := regionAttrCacheReplacePtr + U(1)

    } otherwise {
      val ptr0 = regionAttrCacheReplacePtr
      val ptr1 = (regionAttrCacheReplacePtr + U(1)).resized

      regionAttrCacheValid(ptr0) := True
      regionAttrCacheTag(ptr0)   := regionAttrFillAddr(0)
      regionAttrCache(ptr0)      := regionAttrFillData(0)

      regionAttrCacheValid(ptr1) := True
      regionAttrCacheTag(ptr1)   := regionAttrFillAddr(1)
      regionAttrCache(ptr1)      := regionAttrFillData(1)

      regionAttrCacheReplacePtr := regionAttrCacheReplacePtr + U(2)
    }

  }.elsewhen(regionAttrFillValid(0)) {
    regionAttrCacheValid(regionAttrCacheReplacePtr) := True
    regionAttrCacheTag(regionAttrCacheReplacePtr)   := regionAttrFillAddr(0)
    regionAttrCache(regionAttrCacheReplacePtr)      := regionAttrFillData(0)
    regionAttrCacheReplacePtr := regionAttrCacheReplacePtr + U(1)

  }.elsewhen(regionAttrFillValid(1)) {
    regionAttrCacheValid(regionAttrCacheReplacePtr) := True
    regionAttrCacheTag(regionAttrCacheReplacePtr)   := regionAttrFillAddr(1)
    regionAttrCache(regionAttrCacheReplacePtr)      := regionAttrFillData(1)
    regionAttrCacheReplacePtr := regionAttrCacheReplacePtr + U(1)
  }

  // Shared heapRegionCache fill：同样保留两个 slot 的同周期 fill。
  when(heapRegionFillValid(0) && heapRegionFillValid(1)) {
    when(heapRegionFillAddr(0) === heapRegionFillAddr(1)) {
      heapRegionCacheValid(heapRegionCacheReplacePtr) := True
      heapRegionCacheTag(heapRegionCacheReplacePtr)   := heapRegionFillAddr(0)
      heapRegionCache(heapRegionCacheReplacePtr)      := heapRegionFillData(0)
      heapRegionCacheReplacePtr := heapRegionCacheReplacePtr + U(1)

    } otherwise {
      val ptr0 = heapRegionCacheReplacePtr
      val ptr1 = (heapRegionCacheReplacePtr + U(1)).resized

      heapRegionCacheValid(ptr0) := True
      heapRegionCacheTag(ptr0)   := heapRegionFillAddr(0)
      heapRegionCache(ptr0)      := heapRegionFillData(0)

      heapRegionCacheValid(ptr1) := True
      heapRegionCacheTag(ptr1)   := heapRegionFillAddr(1)
      heapRegionCache(ptr1)      := heapRegionFillData(1)

      heapRegionCacheReplacePtr := heapRegionCacheReplacePtr + U(2)
    }

  }.elsewhen(heapRegionFillValid(0)) {
    heapRegionCacheValid(heapRegionCacheReplacePtr) := True
    heapRegionCacheTag(heapRegionCacheReplacePtr)   := heapRegionFillAddr(0)
    heapRegionCache(heapRegionCacheReplacePtr)      := heapRegionFillData(0)
    heapRegionCacheReplacePtr := heapRegionCacheReplacePtr + U(1)

  }.elsewhen(heapRegionFillValid(1)) {
    heapRegionCacheValid(heapRegionCacheReplacePtr) := True
    heapRegionCacheTag(heapRegionCacheReplacePtr)   := heapRegionFillAddr(1)
    heapRegionCache(heapRegionCacheReplacePtr)      := heapRegionFillData(1)
    heapRegionCacheReplacePtr := heapRegionCacheReplacePtr + U(1)
  }

  // Centralized Copy2Survivor arbitration: two-slot round-robin.
  val slotWantCopySurvivor = Vec.fill(2)(Bool())

  slotWantCopySurvivor(0) := slotValid(0) && slotNeedCopyReq(0) &&
      !slotCopy2SurvivorInflight(0) && !isForwardedMark(slotEffectiveMarkWord(0))

  slotWantCopySurvivor(1) := slotValid(1) && slotNeedCopyReq(1) &&
      !slotCopy2SurvivorInflight(1) && !isForwardedMark(slotEffectiveMarkWord(1))

  // False: conflict 时优先 slot0；True: conflict 时优先 slot1
  val copyPrefer1 = RegInit(False)
  val grantCopy0 = slotWantCopySurvivor(0) && (!slotWantCopySurvivor(1) || !copyPrefer1)
  val grantCopy1 = slotWantCopySurvivor(1) && (!slotWantCopySurvivor(0) || copyPrefer1)

  when(grantCopy0 || grantCopy1) {
    io.Process2CopySurvivor.cmd.valid := True
    io.Process2CopySurvivor.cmd.payload.Owner := Mux(grantCopy0, U(0, 1 bits), U(1, 1 bits))
    io.Process2CopySurvivor.cmd.payload.MarkWord := Mux(
      grantCopy0,
      slotEffectiveMarkWord(0),
      slotEffectiveMarkWord(1)
    )
    io.Process2CopySurvivor.cmd.payload.KlassPtr := Mux(grantCopy0, slotCtx(0).klassPtr, slotCtx(1).klassPtr)
    io.Process2CopySurvivor.cmd.payload.SrcOopPtr := Mux(grantCopy0, slotCtx(0).srcOopPtr, slotCtx(1).srcOopPtr)
    io.Process2CopySurvivor.cmd.payload.SrcLength := Mux(grantCopy0, slotCtx(0).srcLength, slotCtx(1).srcLength)
    io.Process2CopySurvivor.cmd.payload.SrcRegionAttr := Mux(grantCopy0, slotCtx(0).srcRegionAttr, slotCtx(1).srcRegionAttr)
    io.Process2CopySurvivor.cmd.payload.RegionAttrPtr := Mux(grantCopy0,
      srcRegionAttrAddr(0).resize(GCElementWidth),
      srcRegionAttrAddr(1).resize(GCElementWidth))

    when(io.Process2CopySurvivor.cmd.fire) {
      when(grantCopy0) {
        slotNeedCopyReq(0)                := False
        slotCopy2SurvivorInflight(0)      := True
        slotCopy2SurvivorBypassGranted(0) := False
        copyPrefer1 := True
      }

      when(grantCopy1) {
        slotNeedCopyReq(1)                := False
        slotCopy2SurvivorInflight(1)      := True
        slotCopy2SurvivorBypassGranted(1) := False
        copyPrefer1 := False
      }
    }
  }

  // Centralized AOP arbitration: two-slot round-robin.
  val slotWantAop = Vec.fill(2)(Bool())

  slotWantAop(0) := slotValid(0) && slotIsWaitAop(0)
  slotWantAop(1) := slotValid(1) && slotIsWaitAop(1)

  val aopPrefer1 = RegInit(False)
  val grantAop0 = slotWantAop(0) && (!slotWantAop(1) || !aopPrefer1)
  val grantAop1 = slotWantAop(1) && (!slotWantAop(0) || aopPrefer1)

  when(grantAop0 || grantAop1) {
    io.Process2Aop.cmd.valid := True
    io.Process2Aop.cmd.payload.Task := Mux(grantAop0, slotCtx(0).task, slotCtx(1).task)
    io.Process2Aop.cmd.payload.RegionAttr := Mux(grantAop0, slotCtx(0).destRegionAttr, slotCtx(1).destRegionAttr)

    when(io.Process2Aop.cmd.fire) {
      when(grantAop0) {
        aopPrefer1 := True
        finishSlot(0)
      }

      when(grantAop1) {
        aopPrefer1 := False
        finishSlot(1)
      }
    }
  }
}

object GCOopProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCOopProcess())
}