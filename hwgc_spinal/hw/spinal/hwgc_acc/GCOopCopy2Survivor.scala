package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, MyStateMachine}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class CopySurvivorSlotConfig() extends Bundle with GCTopParameters {
  val owner          = UInt(1 bits)
  val markWord       = UInt(GCElementWidth bits)
  val klassPtr       = UInt(GCElementWidth bits)
  val srcLength      = UInt(32 bits)
  val srcOopPtr      = UInt(GCElementWidth bits)
  val srcRegionAttr  = UInt(16 bits)
  val regionAttrPtr  = UInt(GCElementWidth bits)
  val ageThreshold   = UInt(32 bits)
}

case class CopySurvivorSlotRuntime() extends Bundle with GCTopParameters {
  val destOopPtr     = UInt(GCElementWidth bits)

  val lh             = UInt(32 bits)
  val kid            = UInt(32 bits)
  val size           = UInt(32 bits)
  val age            = UInt(32 bits)

  val destAttrPtr    = UInt(GCElementWidth bits)
  val destRegionAttr = UInt(16 bits)

  val plabTargetIdx  = UInt(1 bits)
  val plabForceOld   = Bool()

  val monitor_mw         = UInt(GCElementWidth bits)
  val monitorReadDone    = Bool()
  val monitorUpdateDone  = Bool()

  // PLAB refill 只保留一个 FSM state，用 phase 表示 ptr/buffer/top-end 三段依赖访存。
  val plabRefillPhase    = UInt(2 bits)
  val plabBufferPtr      = UInt(GCElementWidth bits)
  val plabBuffer         = UInt(GCElementWidth bits)
  val plabCacheBottom    = UInt(GCElementWidth bits)
  val plabCacheHardEnd   = UInt(GCElementWidth bits)

  val writeDestOopPtrDone = Bool()

  val copyIssued = Bool()
  val traceIssued = Bool()
  val copyDone = Bool()
  val traceDone = Bool()
  val allocIssued = Bool()
  val allocDone = Bool()

  val plab_refill_failed = Bool()

  val forwardPtr = UInt(GCElementWidth bits)
  val forwardDecided = Bool()

  // 跨 slot PLAB 安全保护。
  val afterAllocCache = Bool()
  val usingPlabCacheBuffer = Bool()

  // 防止 TypeArray 通知在同一个状态里重复置位。
  val typeArrayNotified = Bool()
}

case class CopySurvivorSlotCtx() extends Bundle with GCTopParameters {
  val configs = CopySurvivorSlotConfig()
  val runtime = CopySurvivorSlotRuntime()
}

class GCOopCopy2Survivor extends Module with HWParameters with GCTopParameters with GCParameters {
  val io = new Bundle {
    val Mreq0 = master(new LocalMMUIO)
    val Mreq1 = master(new LocalMMUIO)

    val ToCopy = master(new GCToCopy)
    val ToFetch = master(new GCWriteSrcOopPtr)
    val ToTrace = master(new GCToTrace)
    val ToAllocate = master(new GCToAllocate)
    val ToCopySurvivor = slave(new GCToSurvivor)
    val ConfigIO = slave(new GCCopy2SurvivorConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
    val UpdateAgeThreshold = master(Flow(UInt(64 bits)))
  }

  def clearMreq(m: LocalMMUIO): Unit = {
    m.Request.valid := False
    m.Request.payload.clearAll()
    m.Response.ready := True
  }

  clearMreq(io.Mreq0)
  clearMreq(io.Mreq1)

  io.ToCopy.clearOut()
  io.ToFetch.clearOut()
  io.ToTrace.clearOut()
  io.ToAllocate.clearOut()
  io.ToCopySurvivor.done.clearAll()
  io.UpdateAgeThreshold.clearAll()

  val AGE_MASK = (U(0xF, GCElementWidth bits) << 3).resize(GCElementWidth)

  def slotMreq(i: Int): LocalMMUIO = if (i == 0) io.Mreq0 else io.Mreq1
  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCOopCopy2Survivor<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }
  def sizeBytesOf(words: UInt): UInt = (words.resize(GCElementWidth) << 3).resize(GCElementWidth)
  def nextAge(age: UInt): UInt = Mux(age < U(15, 32 bits), age + U(1, 32 bits), age)
  def replaceAge(mark: UInt, age: UInt): UInt = (mark & ~AGE_MASK) | (age(3 downto 0).resize(GCElementWidth) << 3).resize(GCElementWidth)
  def monitorMarkAddr(mark: UInt): UInt = Mux(mark(1), mark ^ U(2, GCElementWidth bits), mark)
  def forwardingMarkOf(dest: UInt): UInt = Cat(dest(GCElementWidth - 1 downto 2), U(3, 2 bits)).asUInt.resize(GCElementWidth)
  def plabAvailableWords(idx: UInt): UInt = {
    val top = plabCacheTop(idx)
    val end = plabCacheEnd(idx)
    Mux(
      end >= top,
      ((end - top) >> 3).resize(GCElementWidth),
      U(0, GCElementWidth bits))
  }
  def plabEnough(i: Int, idx: UInt): Bool = {
    val top = plabCacheTop(idx)
    val end = plabCacheEnd(idx)

    (end >= top) && (plabAvailableWords(idx) >= slotCtx(i).runtime.size.resize(GCElementWidth))
  }
  def calcSize(lh: UInt, srcLength: UInt): UInt = {
    val size = UInt(32 bits)
    val temp = ((srcLength << lh(7 downto 0)) + lh(23 downto 16)).resize(32)

    when(lh.asSInt < S(0)) {
      size := Mux(
        temp(2 downto 0) =/= U(0),
        ((temp >> U(3)) + U(1)).resize(32),
        (temp >> U(3)).resize(32))
    }.elsewhen(lh.asSInt > S(0) && !lh(0)) {
      size := (lh >> U(3)).resize(32)
    } otherwise {
      size := (lh >> U(3)).resize(32)
    }
    size
  }
  def compressedKlassPtrOf(i: Int): UInt = (io.ConfigIO.CompressedKlassPointerBase + (slotCtx(i).configs.klassPtr(31 downto 0).resize(GCElementWidth) << io.ConfigIO.CompressedKlassPointerShift)).resize(GCElementWidth)
  def lookupKlassPtrOf(i: Int): UInt = Mux(io.ConfigIO.UseCompressedKlassPointer, compressedKlassPtrOf(i), slotCtx(i).configs.klassPtr)

  // Slot 上下文
  val slotValid = Vec.fill(2)(RegInit(False))
  val slotCtx = Vec.fill(2)(Reg(CopySurvivorSlotCtx()).init(CopySurvivorSlotCtx().getZero))

  val slotStart = Vec.fill(2)(Bool())
  val slotGotoIdle = Vec.fill(2)(Bool())

  val slotAllocCacheHazard = Vec.fill(2)(Bool())
  val slotPlabSelectGrant = Vec.fill(2)(Bool())
  val slotPlabAllocGrant = Vec.fill(2)(Bool())
  val slotPlabRefillGrant = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    slotStart(i) := False
    slotGotoIdle(i) := False

    slotPlabSelectGrant(i) := False
    slotPlabRefillGrant(i) := False
  }

  val destAttrRegionValid = RegInit(False)
  val destAttrRegionCache = RegInit(U(0, 32 bits))

  val plabCacheBuffer = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))
  val plabCacheBufferPtr = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))
  val plabCacheBufferPtrValid = Vec.fill(2)(RegInit(False))
  val plabCacheBufferValid = Vec.fill(2)(RegInit(False))
  val plabCacheTop = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))
  val plabCacheEnd = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))
  val plabCacheValid = Vec.fill(2)(RegInit(False))
  val plabRefillBusy = Vec.fill(2)(RegInit(False))
  val plabRefillOwner = Vec.fill(2)(RegInit(U(0, 1 bits)))

  // ============================================================================
  // PLAB 操作锁
  // 每个 plabTargetIdx 一个 lock
  // 当 slot0 正在操作 idx=0 的 plabCacheTop/End 或 ALLOC_CACHE 时， slot1 如果也要 idx=0，必须阻塞在 PLAB_SELECT
  // ============================================================================
  val plabOpBusy = Vec.fill(2)(RegInit(False))
  val plabOpOwner = Vec.fill(2)(RegInit(U(0, 1 bits)))

  val olderSlot = RegInit(U(0, 1 bits)) // 记录两个 slot 的任务先后顺序 当两个 slot 同时到 PLAB_SELECT 时，olderSlot 优先

  // 判断 slot i 是否拥有 Plab(idx)
  def ownsPlab(i: Int, idx: UInt): Bool = plabOpBusy(idx.resize(1)) && plabOpOwner(idx.resize(1)) === U(i, 1 bits)
  // 如果plab Free 或者 slot i 拥有 plab(idx) 则可对其进行分配或者重填操作
  def plabFreeOrMine(i: Int, idx: UInt): Bool = !plabOpBusy(idx) || ownsPlab(i, idx)
  def lockPlab(i: Int, idx: UInt): Unit = {
    plabOpBusy(idx) := True
    plabOpOwner(idx) := U(i, 1 bits)
  }
  def unlockPlab(i: Int, idx: UInt): Unit = {
    when(ownsPlab(i, idx)) {
      plabOpBusy(idx.resize(1)) := False
    }
  }
  def unlockAllPlabOfSlot(i: Int): Unit = {
    for (j <- 0 until 2) {
      unlockPlab(i, j)
    }
  }

  // Klass cache lookup
  val KlassCacheEntries = 16
  val klassCacheValid = Vec.fill(KlassCacheEntries)(RegInit(False))
  val klassCachePtr = Vec.fill(KlassCacheEntries)(RegInit(U(0, GCElementWidth bits)))
  val klassCacheKidLh = Vec.fill(KlassCacheEntries)(RegInit(U(0, GCElementWidth bits)))
  val klassCacheReplacePtr = RegInit(U(0, log2Up(KlassCacheEntries) bits))

  val klassLookupPtr = Vec.fill(2)(UInt(GCElementWidth bits))
  val klassCacheHit = Vec.fill(2)(Bool())
  val klassCacheHitIndex = Vec.fill(2)(UInt(log2Up(KlassCacheEntries) bits))

  for (i <- 0 until 2) {
    klassLookupPtr(i) := lookupKlassPtrOf(i)

    val hitVec = Vec.fill(KlassCacheEntries)(Bool())
    for (j <- 0 until KlassCacheEntries) {
      hitVec(j) := klassCacheValid(j) && klassCachePtr(j) === klassLookupPtr(i)
    }

    klassCacheHit(i) := hitVec.orR
    klassCacheHitIndex(i) := OHToUInt(hitVec.asBits)
  }

  // Shared fill requests
  val klassFillValid = Vec.fill(2)(Bool())
  val klassFillPtr = Vec.fill(2)(UInt(GCElementWidth bits))
  val klassFillKidLh = Vec.fill(2)(UInt(GCElementWidth bits))

  val destAttrFillValid = Vec.fill(2)(Bool())
  val destAttrFillData = Vec.fill(2)(UInt(32 bits))

  // PlabAllocatorPtr + 0x10/0x18 是两个连续的 buffer-pointer 槽。
  // 第一次 miss 直接 16B 读取两个 pointer，顺手预取另一个 idx。
  val plabPtrPairFillValid = Vec.fill(2)(Bool())
  val plabPtrPairFillData0 = Vec.fill(2)(UInt(GCElementWidth bits))
  val plabPtrPairFillData1 = Vec.fill(2)(UInt(GCElementWidth bits))

  val plabBufFillValid = Vec.fill(2)(Bool())
  val plabBufFillIdx = Vec.fill(2)(UInt(1 bits))
  val plabBufFillData = Vec.fill(2)(UInt(GCElementWidth bits))

  val plabTopEndFillValid = Vec.fill(2)(Bool())
  val plabTopEndFillIdx = Vec.fill(2)(UInt(1 bits))
  val plabTopFillData = Vec.fill(2)(UInt(GCElementWidth bits))
  val plabEndFillData = Vec.fill(2)(UInt(GCElementWidth bits))

  for (i <- 0 until 2) {
    klassFillValid(i) := False
    klassFillPtr(i) := U(0, GCElementWidth bits)
    klassFillKidLh(i) := U(0, GCElementWidth bits)

    destAttrFillValid(i) := False
    destAttrFillData(i) := U(0, 32 bits)

    plabPtrPairFillValid(i) := False
    plabPtrPairFillData0(i) := U(0, GCElementWidth bits)
    plabPtrPairFillData1(i) := U(0, GCElementWidth bits)

    plabBufFillValid(i) := False
    plabBufFillIdx(i) := U(0, 1 bits)
    plabBufFillData(i) := U(0, GCElementWidth bits)

    plabTopEndFillValid(i) := False
    plabTopEndFillIdx(i) := U(0, 1 bits)
    plabTopFillData(i) := U(0, GCElementWidth bits)
    plabEndFillData(i) := U(0, GCElementWidth bits)
  }

  // shared next module resource busy
  val copyBusy = RegInit(False)
  val copyOwner = RegInit(U(0, 1 bits))

  val traceBusy = RegInit(False)
  val traceOwner = RegInit(U(0, 1 bits))

  val allocBusy = RegInit(False)
  val allocOwner = RegInit(U(0, 1 bits))

  when(copyBusy && io.ToCopy.Done) {
    copyBusy := False
    slotCtx(copyOwner).runtime.copyDone := True
    dbg(Seq("copy done for slot ", copyOwner))
  }

  when(traceBusy && io.ToTrace.Done) {
    traceBusy := False
    slotCtx(traceOwner).runtime.traceDone := True
    dbg(Seq("trace done for slot ", traceOwner))
  }

  when(allocBusy && io.ToAllocate.done.valid) {
    allocBusy := False
    slotCtx(allocOwner).runtime.allocDone := True
    slotCtx(allocOwner).runtime.destOopPtr := io.ToAllocate.done.payload.DestOopPtr
    slotCtx(allocOwner).runtime.plab_refill_failed := io.ToAllocate.done.payload.PlabRefillFailed

    dbg(Seq("allocate done for slot ", allocOwner))
  }

  // pending events
  val typeArrayPending = Vec.fill(2)(RegInit(False))
  val typeArrayOwner = Vec.fill(2)(RegInit(U(0, 1 bits)))

  val survivorDonePending = Vec.fill(2)(RegInit(False))
  val survivorDoneOwner = Vec.fill(2)(RegInit(U(0, 1 bits)))
  val survivorDoneDest = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))

  val toFetchPending = Vec.fill(2)(RegInit(False))
  val toFetchSrcOopPtr = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))
  val toFetchWriteValue = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))

  // slot allocation and finish
  def clearSlotRuntime(i: Int): Unit = {
    slotCtx(i).runtime.clearAll()
  }
  def allocSlot(i: Int): Unit = {
    slotValid(i) := True

    slotCtx(i).configs.owner := io.ToCopySurvivor.cmd.payload.Owner
    slotCtx(i).configs.srcOopPtr := io.ToCopySurvivor.cmd.payload.SrcOopPtr
    slotCtx(i).configs.markWord := io.ToCopySurvivor.cmd.payload.MarkWord
    slotCtx(i).configs.klassPtr := io.ToCopySurvivor.cmd.payload.KlassPtr
    slotCtx(i).configs.srcLength := io.ToCopySurvivor.cmd.payload.SrcLength
    slotCtx(i).configs.srcRegionAttr := io.ToCopySurvivor.cmd.payload.SrcRegionAttr
    slotCtx(i).configs.regionAttrPtr := io.ToCopySurvivor.cmd.payload.RegionAttrPtr
    slotCtx(i).configs.ageThreshold := io.ConfigIO.AgeThreshold

    clearSlotRuntime(i)

    slotStart(i) := True

    // 维护任务先后顺序 如果另一个 slot 为空，当前任务就是 olderSlot。
    when(!slotValid(1 - i)) {
      olderSlot := U(i, 1 bits)
    }

    dbg(Seq("Allocate task to slot", i.toString, ", src=", io.ToCopySurvivor.cmd.payload.SrcOopPtr))
  }
  def finishSlot(i: Int): Unit = {
    slotValid(i) := False
    clearSlotRuntime(i)

    slotGotoIdle(i) := True

    unlockAllPlabOfSlot(i) // slot 完成时释放自己持有的 PLAB lock，避免异常路径导致死锁。

    // 更新 olderSlot。
    when(slotValid(1 - i)) {
      olderSlot := U(1 - i, 1 bits)
    } otherwise {
      olderSlot := U(0, 1 bits)
    }

    dbg(Seq("Finish slot", i.toString))
  }

  val hasFreeSlot = !slotValid(0) || !slotValid(1)
  io.ToCopySurvivor.cmd.ready := hasFreeSlot

  when(io.ToCopySurvivor.cmd.fire) {
    when(!slotValid(0)) {
      allocSlot(0)
    } otherwise {
      allocSlot(1)
    }
  }

  // Slot Fsms
  val slotIsPlabSelect = Vec.fill(2)(Bool()) // 状态泄漏
  val slotIsAllocCache = Vec.fill(2)(Bool())
  val slotIsRunCopyTrace = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    val m = slotMreq(i)

    val slotFsm = new MyStateMachine {
      val IDLE = new State with EntryPoint
      val READ_KLASS = new State
      val SIZE_DEICIDE = new State
      val DEST_ATTR_DECIDE = new State
      val AGE_DECIDE = new State

      val PLAB_SELECT = new State
      val REFILL_PLAB = new State

      val ALLOC_CACHE = new State
      val WRITE_FORCE_OLD = new State

      val DECIDE_FORWARD_PTR = new State

      // Copy 发出后，在一个状态里并行推进 header write / Trace / completion wait。
      // Trace 的仲裁仍然由 copyIssued 硬门控，因此不会早于 Copy.cmd.fire。
      val RUN_COPY_TRACE = new State

      val READ_BOTTOM_HARD_END = new State
      val WRITE_FORWARDPTR_NOT_ZERO = new State

      def getLhKidResp(lhVal: UInt, kidVal: UInt) : Unit = {
        when(kidVal === U(InstanceMirrorKlassID, 32 bits) && (lhVal.asSInt === S(0) || (lhVal.asSInt > S(0) && lhVal(0)))) {
          goto(SIZE_DEICIDE)
        } otherwise {
          slotCtx(i).runtime.size := calcSize(lhVal, slotCtx(i).configs.srcLength)
          goto(DEST_ATTR_DECIDE)
        }
      }

      def assignDestRegionAttrByAge(newAge: UInt) : Unit = {
        when(newAge < slotCtx(i).configs.ageThreshold) {
          slotCtx(i).runtime.destRegionAttr := slotCtx(i).configs.srcRegionAttr
          slotCtx(i).runtime.destAttrPtr := slotCtx(i).configs.regionAttrPtr
          slotCtx(i).runtime.plabTargetIdx := slotCtx(i).configs.srcRegionAttr(15 downto 8).resize(1)
        }
      }

      def casForwardPtr(addr: UInt, expected: UInt, desired: UInt, successNext: State, failNext: State) : Unit = {
        // 128bits 对齐的访问 64bits
        // val casData = Mux(
        //   addr(3),
        //   Cat((desired.resize(128) << 64).resize(128), (expected.resize(128) << 64).resize(128)),
        //   Cat(desired.resize(128), expected.resize(128))
        // ).asUInt.resize(MMUDataWidth)
        val casData = desired

        issueReq(m, addr, True, U(8), casData, True, True, issued) { rd =>
          val observed = rd(GCElementWidth - 1 downto 0)
          slotCtx(i).runtime.forwardDecided := True

          when(expected === expected) {
          //when(observed === expected) {
            slotCtx(i).runtime.forwardPtr := 0

            toFetchPending(i) := True
            toFetchSrcOopPtr(i) := slotCtx(i).configs.srcOopPtr
            toFetchWriteValue(i) := desired

            goto(successNext)
          }.otherwise{
            slotCtx(i).runtime.forwardPtr := observed & ~U(3, GCElementWidth bits)
            goto(failNext)
          }
        }
      }

      def doPlabCacheAllocDirect(): Unit = {
        val idx = slotCtx(i).runtime.plabTargetIdx
        val otherIdx = U(1) - idx

        val oldTop = plabCacheTop(idx)
        val newTop = (oldTop + sizeBytesOf(slotCtx(i).runtime.size)).resize(GCElementWidth)

        slotCtx(i).runtime.destOopPtr := oldTop
        slotCtx(i).runtime.afterAllocCache := True
        slotCtx(i).runtime.usingPlabCacheBuffer := True
        slotCtx(i).runtime.plabBuffer := plabCacheBuffer(idx)

        when(plabCacheValid(otherIdx) && plabCacheBuffer(0) === plabCacheBuffer(1)) {
          plabCacheTop(otherIdx) := newTop
        }

        issueDirectWriteWithoutResp(m, plabCacheBuffer(idx) + U"x30", U(8), newTop, DECIDE_FORWARD_PTR) {
          plabCacheTop(idx) := newTop
          unlockPlab(i, idx)
        }
      }

      READ_KLASS.whenIsActive {
        when(klassCacheHit(i)) {
          val idx = klassCacheHitIndex(i)
          val lhVal = klassCacheKidLh(idx)(31 downto 0)
          val kidVal = klassCacheKidLh(idx)(63 downto 32)

          slotCtx(i).configs.klassPtr := klassLookupPtr(i)
          slotCtx(i).runtime.lh := lhVal
          slotCtx(i).runtime.kid := kidVal

          getLhKidResp(lhVal, kidVal)

        }.otherwise {
          val newKlassPtr = klassLookupPtr(i)
          val addr = (newKlassPtr + U(8)).resize(MMUAddrWidth)

          issueReq(m, addr, False, U(8), U(0), True, False, issued) { rd =>
            val lhVal = rd(31 downto 0)
            val kidVal = rd(63 downto 32)

            slotCtx(i).configs.klassPtr := newKlassPtr
            slotCtx(i).runtime.lh := lhVal
            slotCtx(i).runtime.kid := kidVal

            klassFillValid(i) := True
            klassFillPtr(i) := newKlassPtr
            klassFillKidLh(i) := rd(63 downto 0)

            getLhKidResp(lhVal, kidVal)
          }
        }
      }

      SIZE_DEICIDE.whenIsActive {
        val offset = Mux(io.ConfigIO.UseCompressedKlassPointer, U"x20", U"x24")
        issueDirectRead(m, slotCtx(i).configs.srcOopPtr + offset, U(8), DEST_ATTR_DECIDE) { rd =>
          slotCtx(i).runtime.size := rd(31 downto 0)
        }
      }

      DEST_ATTR_DECIDE.whenIsActive {
        slotCtx(i).runtime.plabForceOld := False

        // 正在执行的Oop 是 TypeArray 不需要遍历 没有子对象 所以可以 告诉OopProcess 做前递
        when(slotCtx(i).runtime.kid === U(TypeArrayKlassID, 32 bits) && !slotCtx(i).runtime.typeArrayNotified) {
          typeArrayPending(i) := True
          typeArrayOwner(i) := slotCtx(i).configs.owner
          slotCtx(i).runtime.typeArrayNotified := True
        }

        val srcType = slotCtx(i).configs.srcRegionAttr(15 downto 8)
        val highSelected = srcType === U(1)
        val destAttrBase = (io.ConfigIO.ParScanThreadStatePtr + U"x178").resize(MMUAddrWidth)

        val cachedDestAttr = Mux(highSelected, destAttrRegionCache(31 downto 16), destAttrRegionCache(15 downto 0)).resize(16)
        val cachedPlabIdx = Mux(highSelected, destAttrRegionCache(31 downto 24), destAttrRegionCache(15 downto 8)).resize(1)

        when(!destAttrRegionValid) {
          issueDirectRead(m, destAttrBase, U(4), AGE_DECIDE) { rd =>
            slotCtx(i).runtime.destAttrPtr := destAttrBase + Mux(highSelected, U(2), U(0))
            slotCtx(i).runtime.destRegionAttr := Mux(highSelected, rd(31 downto 16), rd(15 downto 0))
            slotCtx(i).runtime.plabTargetIdx := Mux(highSelected, rd(31 downto 24), rd(15 downto 8)).resize(1)

            destAttrFillValid(i) := True
            destAttrFillData(i) := rd(31 downto 0)
          }
        } otherwise {
          slotCtx(i).runtime.destAttrPtr := destAttrBase + Mux(highSelected, U(2), U(0))
          slotCtx(i).runtime.destRegionAttr := cachedDestAttr
          slotCtx(i).runtime.plabTargetIdx := cachedPlabIdx

          goto(AGE_DECIDE)
        }
      }

      AGE_DECIDE.whenIsActive {
        val src_region_attr_type = slotCtx(i).configs.srcRegionAttr(15 downto 8)

        when(src_region_attr_type =/= U(0)) {
          goto(PLAB_SELECT)
        }.elsewhen(slotCtx(i).configs.markWord(0)) {
          val new_age = (slotCtx(i).configs.markWord >> 3).resize(4).resize(32)
          slotCtx(i).runtime.age := new_age
          assignDestRegionAttrByAge(new_age)

          goto(PLAB_SELECT)
        } otherwise {
          issueDirectRead(m, monitorMarkAddr(slotCtx(i).configs.markWord), U(8), PLAB_SELECT) { rd =>
            val new_age = (rd(GCElementWidth - 1 downto 0) >> 3).resize(4).resize(32)
            slotCtx(i).runtime.age := new_age
            assignDestRegionAttrByAge(new_age)
          }
        }
      }

      PLAB_SELECT.whenIsActive {
        val idx = slotCtx(i).runtime.plabTargetIdx
        val enough = plabEnough(i, idx)

        when(slotPlabSelectGrant(i)) {
          when(plabCacheValid(idx)) {
            when(!slotAllocCacheHazard(i) && enough && slotPlabAllocGrant(i)) {
              when(!ownsPlab(i, idx)) {
                lockPlab(i, idx)
              }

              doPlabCacheAllocDirect()
            }.elsewhen(!slotAllocCacheHazard(i) && !enough) {
              // cache 有效但空间不够，需要走 ALLOC_CACHE 从这里开始锁住该 PLAB idx，直到 ToAllocate 完成或切换目标。
              when(!ownsPlab(i, idx)) {
                lockPlab(i, idx)
              }

              goto(ALLOC_CACHE)
            }

          } otherwise {
            // cache 无效，需要 refill top/end。
            // 进入一个 phase-based state，避免 ptr -> buffer -> top/end 三个 FSM state。
            when(slotPlabRefillGrant(i)) {
              when(!ownsPlab(i, idx)) {
                lockPlab(i, idx)
              }

              when(plabCacheBufferValid(idx)) {
                slotCtx(i).runtime.plabBuffer := plabCacheBuffer(idx)
                slotCtx(i).runtime.plabRefillPhase := U(2)
              }.elsewhen(plabCacheBufferPtrValid(idx)) {
                slotCtx(i).runtime.plabBufferPtr := plabCacheBufferPtr(idx)
                slotCtx(i).runtime.plabRefillPhase := U(1)
              }.otherwise {
                slotCtx(i).runtime.plabRefillPhase := U(0)
              }

              goto(REFILL_PLAB)
            }
          }
        }
      }

      REFILL_PLAB.whenIsActive {
        val idx = slotCtx(i).runtime.plabTargetIdx

        switch(slotCtx(i).runtime.plabRefillPhase) {
          is(U(0)) {
            // 两个 pointer 位于 +0x10/+0x18，一次 16B 读同时预取 idx0/idx1。
            val addr = (io.ConfigIO.PlabAllocatorPtr + U"x10").resize(MMUAddrWidth)

            issueDirectRead(m, addr, U(16), REFILL_PLAB) { rd =>
              val ptr0 = rd(GCElementWidth - 1 downto 0)
              val ptr1 = rd(GCElementWidth * 2 - 1 downto GCElementWidth)

              plabPtrPairFillValid(i) := True
              plabPtrPairFillData0(i) := ptr0
              plabPtrPairFillData1(i) := ptr1

              slotCtx(i).runtime.plabBufferPtr := Mux(idx === U(0), ptr0, ptr1)
              slotCtx(i).runtime.plabRefillPhase := U(1)
            }
          }

          is(U(1)) {
            issueDirectRead(m, slotCtx(i).runtime.plabBufferPtr, U(8), REFILL_PLAB) { rd =>
              plabBufFillValid(i) := True
              plabBufFillIdx(i) := idx
              plabBufFillData(i) := rd(GCElementWidth - 1 downto 0)

              slotCtx(i).runtime.plabBuffer := rd(GCElementWidth - 1 downto 0)
              slotCtx(i).runtime.plabRefillPhase := U(2)
            }
          }

          default {
            val addr = (slotCtx(i).runtime.plabBuffer + U"x30").resize(MMUAddrWidth)

            issueDirectRead(m, addr, U(16), PLAB_SELECT) { rd =>
              plabTopEndFillValid(i) := True
              plabTopEndFillIdx(i) := idx
              plabTopFillData(i) := rd(GCElementWidth - 1 downto 0)
              plabEndFillData(i) := rd(GCElementWidth * 2 - 1 downto GCElementWidth)

              unlockPlab(i, idx)
            }
          }
        }
      }

      ALLOC_CACHE.whenIsActive {
        val idx = slotCtx(i).runtime.plabTargetIdx
        val otherIdx = U(1) - idx

        // ALLOC_CACHE 同时承担 request + wait-done，allocIssued 本身就是 phase bit。
        // 这样等待 ToAllocate 时不会再占一个独立 FSM state。
        when(!ownsPlab(i, idx)) {
          goto(PLAB_SELECT)
        } otherwise {
          when(!slotCtx(i).runtime.allocIssued) {
            slotCtx(i).runtime.destOopPtr := U(0)

            when(plabCacheValid(otherIdx) && plabCacheBuffer(0) === plabCacheBuffer(1)) {
              plabCacheValid(otherIdx) := False
            }
            plabCacheValid(idx) := False

          } otherwise {
            slotCtx(i).runtime.afterAllocCache := True
            slotCtx(i).runtime.usingPlabCacheBuffer := False
            slotCtx(i).runtime.plabBuffer := plabCacheBuffer(idx)
          }

          when(slotCtx(i).runtime.allocDone) {
            slotCtx(i).runtime.allocIssued := False
            slotCtx(i).runtime.allocDone := False

            when(slotCtx(i).runtime.destOopPtr === U(0, GCElementWidth bits)) {
              // 当前 idx 分配失败，切到 old idx 前先释放当前 idx lock。
              unlockPlab(i, idx)
              slotCtx(i).runtime.plabTargetIdx := U(1, 1 bits)
              slotCtx(i).runtime.plabForceOld := True

              goto(PLAB_SELECT)

            } otherwise {
              // ToAllocate 已经完成，当前 idx 的本地 cache 已失效。
              unlockPlab(i, idx)

              when(slotCtx(i).runtime.plabForceOld) {
                when(slotCtx(i).runtime.plab_refill_failed) {
                  slotCtx(i).configs.ageThreshold := U(0)
                  io.UpdateAgeThreshold.valid := True
                  io.UpdateAgeThreshold.payload := U(0)

                  issueDirectWriteWithoutResp(m, io.ConfigIO.ParScanThreadStatePtr + U"x17c", U(4), U(0), WRITE_FORCE_OLD) ()
                }.otherwise{
                  goto(WRITE_FORCE_OLD)
                }
              } otherwise {
                goto(DECIDE_FORWARD_PTR)
              }
            }
          }
        }
      }

      WRITE_FORCE_OLD.whenIsActive {
        val addr = (slotCtx(i).runtime.destAttrPtr + U(1)).resize(MMUAddrWidth)

        issueDirectWriteWithoutResp(m, addr, U(1), U(1), DECIDE_FORWARD_PTR) {
          slotCtx(i).runtime.destRegionAttr(15 downto 8) := 1

          when(slotCtx(i).runtime.destAttrPtr === slotCtx(i).configs.regionAttrPtr) {
            slotCtx(i).configs.srcRegionAttr(15 downto 8) := 1
          }.elsewhen(slotCtx(i).runtime.destAttrPtr === io.ConfigIO.ParScanThreadStatePtr + U"x178") {
            destAttrRegionCache(15 downto 8) := 1
          }.elsewhen(slotCtx(i).runtime.destAttrPtr === io.ConfigIO.ParScanThreadStatePtr + U"x17a") {
            destAttrRegionCache(31 downto 24) := 1
          }
        }
      }

      DECIDE_FORWARD_PTR.whenIsActive {
        // @notice: atomic-cas
        val newMw = forwardingMarkOf(slotCtx(i).runtime.destOopPtr)
        casForwardPtr(slotCtx(i).configs.srcOopPtr, slotCtx(i).configs.markWord, newMw, RUN_COPY_TRACE, READ_BOTTOM_HARD_END)
      }

      RUN_COPY_TRACE.whenIsActive {
        val copyIssuedDone = slotCtx(i).runtime.copyIssued
        val needTrace = slotCtx(i).runtime.kid =/= U(TypeArrayKlassID, 32 bits)
        val copyFinished = slotCtx(i).runtime.copyDone
        val traceFinished = slotCtx(i).runtime.traceDone || !needTrace
        val needMonitorUpdate = slotCtx(i).runtime.destOopPtr(15 downto 0) === 0 && !slotCtx(i).configs.markWord(0)
        val memoryPostDone = slotCtx(i).runtime.writeDestOopPtrDone &&
          (!needMonitorUpdate || slotCtx(i).runtime.monitorUpdateDone)

        // 关键约束保持不变：
        // copyIssued 只在 ToCopy.cmd.fire 时置位，所以 Trace 仍只能在 Copy 真正发出后的下一拍或更晚发出。
        //
        // Mreq 优化：
        // 不再等待 Trace issued 才做 monitor 更新。Copy fire 后，Mreq 连续执行
        // dest header write -> monitor read -> monitor write，与 Trace/Copy completion 并行。
        when(copyIssuedDone) {
          when(!slotCtx(i).runtime.writeDestOopPtrDone) {
            val addr = slotCtx(i).runtime.destOopPtr.resize(MMUAddrWidth)
            val newAge = nextAge(slotCtx(i).runtime.age)
            val writeValue = Mux(
              slotCtx(i).runtime.destRegionAttr(15 downto 8) === 0 && slotCtx(i).configs.markWord(0),
              replaceAge(slotCtx(i).configs.markWord, newAge),
              slotCtx(i).configs.markWord
            )

            issueReq(m, addr, True, U(8), writeValue, False, False, issued) { _ => }

            when(issued) {
              issued := False
              slotCtx(i).runtime.writeDestOopPtrDone := True
            }

          }.elsewhen(needMonitorUpdate && !slotCtx(i).runtime.monitorReadDone) {
            issueReq(m, monitorMarkAddr(slotCtx(i).configs.markWord), False, U(8), U(0), True, False, issued) { rd =>
              slotCtx(i).runtime.monitor_mw := rd(GCElementWidth - 1 downto 0)
              slotCtx(i).runtime.monitorReadDone := True
            }

          }.elsewhen(needMonitorUpdate && !slotCtx(i).runtime.monitorUpdateDone) {
            val addr = monitorMarkAddr(slotCtx(i).configs.markWord)
            val newAge = nextAge(slotCtx(i).runtime.age)
            val writeValue = replaceAge(slotCtx(i).runtime.monitor_mw, newAge)

            issueDirectWriteWithoutResp(m, addr, U(8), writeValue, RUN_COPY_TRACE) {
              slotCtx(i).runtime.monitorUpdateDone := True
            }
          }
        }

        when(copyIssuedDone && memoryPostDone && copyFinished && traceFinished) {
          survivorDonePending(i) := True
          survivorDoneOwner(i) := slotCtx(i).configs.owner
          survivorDoneDest(i) := slotCtx(i).runtime.destOopPtr

          finishSlot(i)
        }
      }

      READ_BOTTOM_HARD_END.whenIsActive {
        val idx = slotCtx(i).runtime.plabTargetIdx
        val addr = (plabCacheBuffer(idx) + U"x28").resize(MMUAddrWidth)

        issueDirectRead(m, addr, U(32), WRITE_FORWARDPTR_NOT_ZERO) { rd =>
          slotCtx(i).runtime.plabCacheBottom := rd(GCElementWidth - 1 downto 0)
          slotCtx(i).runtime.plabCacheHardEnd := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)
        }
      }

      WRITE_FORWARDPTR_NOT_ZERO.whenIsActive {
        val idx = slotCtx(i).runtime.plabTargetIdx

        when(slotCtx(i).runtime.destOopPtr >= slotCtx(i).runtime.plabCacheBottom && slotCtx(i).runtime.destOopPtr < slotCtx(i).runtime.plabCacheHardEnd) {
          val otherIdx = U(1) - idx

          when(plabCacheValid(idx)) {
            plabCacheTop(idx) := slotCtx(i).runtime.destOopPtr
          }
          when(plabCacheValid(otherIdx) && plabCacheBuffer(0) === plabCacheBuffer(1)) {
            plabCacheTop(otherIdx) := slotCtx(i).runtime.destOopPtr
          }

          issueReq(m, plabCacheBuffer(idx) + U"x30", True, U(8), slotCtx(i).runtime.destOopPtr, False, False, issued) { _ => }
        }.otherwise {
          val words = slotCtx(i).runtime.size >> 3
          val headSize = Mux(io.ConfigIO.UseCompressedKlassPointer, U(2), U(3))
          val cond = words >= headSize
          val temp_klass_ptr = Mux(cond, io.ConfigIO.IntArrayKlassObj, io.ConfigIO.ObjectKlassObj)

          val writeOff0 = U(1, 64 bits)
          val writeOff8 = Mux(
            io.ConfigIO.UseCompressedKlassPointer,
            ((temp_klass_ptr - io.ConfigIO.CompressedKlassPointerBase) >> io.ConfigIO.CompressedKlassPointerShift).resize(64),
            temp_klass_ptr
          )
          val writeOff12_16 = ((words - headSize) * 2).resize(32)
          val writeValue = Mux(
            io.ConfigIO.UseCompressedKlassPointer,
            Cat(writeOff12_16, writeOff8.resize(32), writeOff0).resize(MMUDataWidth),
            Cat(writeOff12_16, writeOff8, writeOff0).resize(MMUDataWidth)
          ).asUInt

          val writeSize = Mux(
            io.ConfigIO.UseCompressedKlassPointer && cond,
            U(16),
            Mux(io.ConfigIO.UseCompressedKlassPointer, U(12), Mux(cond, U(20), U(16)))
          ).resize(LineBytesNumBitSize)

          issueReq(m, slotCtx(i).runtime.destOopPtr, True, writeSize, writeValue, False, False, issued) { _ => }
        }

        when(issued) {
          issued := False
          survivorDonePending(i) := True
          survivorDoneOwner(i) := slotCtx(i).configs.owner
          survivorDoneDest(i) := slotCtx(i).runtime.forwardPtr

          finishSlot(i)
        }
      }

      always {
        when(slotGotoIdle(i)) {
          goto(IDLE)
        }.elsewhen(slotStart(i)) {
          goto(READ_KLASS)
        }
      }
    }

    slotIsPlabSelect(i) := slotFsm.isActive(slotFsm.PLAB_SELECT)
    slotIsAllocCache(i) := slotFsm.isActive(slotFsm.ALLOC_CACHE)
    slotIsRunCopyTrace(i) := slotFsm.isActive(slotFsm.RUN_COPY_TRACE)
  }

  for (i <- 0 until 2) {
    val other = 1 - i
    val myIdx = slotCtx(i).runtime.plabTargetIdx

    val samePlabBuffer = slotValid(other) && slotCtx(other).runtime.usingPlabCacheBuffer &&
      plabCacheValid(myIdx) && slotCtx(other).runtime.plabBuffer === plabCacheBuffer(myIdx)
    val otherNeedBlock = slotValid(other) && slotCtx(other).runtime.afterAllocCache &&
      samePlabBuffer && (!slotCtx(other).runtime.forwardDecided || slotCtx(other).runtime.forwardPtr =/= U(0, GCElementWidth bits))

    // 同 idx 被另一个 slot lock 时，当前 slot 必须阻塞
    val sameIdxLockedByOther = plabOpBusy(myIdx) && plabOpOwner(myIdx) =/= U(i, 1 bits)

    slotAllocCacheHazard(i) := otherNeedBlock || sameIdxLockedByOther
  }

  // ============================================================================
  // PLAB_SELECT arbitration
  // 规则：
  //   1. 每拍最多一个 slot 真正执行 PLAB_SELECT 判断。
  //   2. 已经持有 PLAB lock 的 slot 优先继续执行。
  //   3. 两个 slot 同时请求时，olderSlot 优先。
  // ============================================================================
  val plabSelectReq = Vec.fill(2)(Bool())
  val plabSelectOwn = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    val idx = slotCtx(i).runtime.plabTargetIdx

    plabSelectOwn(i) := slotValid(i) && slotIsPlabSelect(i) && ownsPlab(i, idx)
    plabSelectReq(i) := slotValid(i) && slotIsPlabSelect(i) && plabFreeOrMine(i, idx)
  }

  when(plabSelectOwn(0)) {
    slotPlabSelectGrant(0) := True
  }.elsewhen(plabSelectOwn(1)) {
    slotPlabSelectGrant(1) := True
  }.otherwise {
    when(olderSlot === U(0, 1 bits)) {
      when(plabSelectReq(0)) {
        slotPlabSelectGrant(0) := True
      }.elsewhen(plabSelectReq(1)) {
        slotPlabSelectGrant(1) := True
      }
    } otherwise {
      when(plabSelectReq(1)) {
        slotPlabSelectGrant(1) := True
      }.elsewhen(plabSelectReq(0)) {
        slotPlabSelectGrant(0) := True
      }
    }
  }

  // ============================================================================
  // PLAB cache allocation grant
  // 改成跟随 slotPlabSelectGrant，而不是固定 slot0 > slot1。
  // ============================================================================
  for (i <- 0 until 2) {
    val idx = slotCtx(i).runtime.plabTargetIdx
    slotPlabAllocGrant(i) := slotPlabSelectGrant(i) && plabCacheValid(idx) &&
      plabEnough(i, idx) && !slotAllocCacheHazard(i)
  }

  // PLAB refill request generation
  val plabRefillReq = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    val idx = slotCtx(i).runtime.plabTargetIdx

    plabRefillReq(i) := slotValid(i) && slotIsPlabSelect(i) && slotPlabSelectGrant(i) &&
      !plabCacheValid(idx) && !plabRefillBusy(idx) && plabFreeOrMine(i, idx)
  }

  // Shared cache fill: klass cache
  val grantKlassFill0 = klassFillValid(0)
  val grantKlassFill1 = !klassFillValid(0) && klassFillValid(1)

  when(grantKlassFill0 || grantKlassFill1) {
    val fillPtr = Mux(grantKlassFill0, klassFillPtr(0), klassFillPtr(1))
    val fillKidLh = Mux(grantKlassFill0, klassFillKidLh(0), klassFillKidLh(1))

    klassCacheValid(klassCacheReplacePtr) := True
    klassCachePtr(klassCacheReplacePtr) := fillPtr
    klassCacheKidLh(klassCacheReplacePtr) := fillKidLh
    klassCacheReplacePtr := klassCacheReplacePtr + 1
  }

  // Shared dest attr cache fill
  val grantDestAttrFill0 = destAttrFillValid(0)
  val grantDestAttrFill1 = !destAttrFillValid(0) && destAttrFillValid(1)

  when(grantDestAttrFill0 || grantDestAttrFill1) {
    val fillData = Mux(grantDestAttrFill0, destAttrFillData(0), destAttrFillData(1))

    destAttrRegionValid := True
    destAttrRegionCache := fillData
  }

  // PLAB refill-start arbitration
  val grantPlabRefill0 = plabRefillReq(0)
  val grantPlabRefill1 = !plabRefillReq(0) && plabRefillReq(1)

  when(grantPlabRefill0) {
    val idx = slotCtx(0).runtime.plabTargetIdx

    plabRefillBusy(idx) := True
    plabRefillOwner(idx) := U(0, 1 bits)
    slotPlabRefillGrant(0) := True

    lockPlab(0, idx)

  } elsewhen grantPlabRefill1 {
    val idx = slotCtx(1).runtime.plabTargetIdx

    plabRefillBusy(idx) := True
    plabRefillOwner(idx) := U(1, 1 bits)
    slotPlabRefillGrant(1) := True

    lockPlab(1, idx)
  }

  // PLAB pointer pair fill：第一次访问任一 idx 时，同时缓存两个 pointer。
  val grantPlabPtrPairFill0 = plabPtrPairFillValid(0)
  val grantPlabPtrPairFill1 = !plabPtrPairFillValid(0) && plabPtrPairFillValid(1)

  when(grantPlabPtrPairFill0 || grantPlabPtrPairFill1) {
    plabCacheBufferPtr(0) := Mux(grantPlabPtrPairFill0, plabPtrPairFillData0(0), plabPtrPairFillData0(1))
    plabCacheBufferPtr(1) := Mux(grantPlabPtrPairFill0, plabPtrPairFillData1(0), plabPtrPairFillData1(1))
    plabCacheBufferPtrValid(0) := True
    plabCacheBufferPtrValid(1) := True
  }

  // PLAB buffer fill
  for (j <- 0 until 2) {
    when(plabBufFillValid(0) && plabBufFillIdx(0) === U(j, 1 bits)) {
      plabCacheBuffer(j) := plabBufFillData(0)
      plabCacheBufferValid(j) := True
    } elsewhen(plabBufFillValid(1) && plabBufFillIdx(1) === U(j, 1 bits)) {
      plabCacheBuffer(j) := plabBufFillData(1)
      plabCacheBufferValid(j) := True
    }
  }

  // PLAB top/end fill
  for (j <- 0 until 2) {
    when(plabTopEndFillValid(0) && plabTopEndFillIdx(0) === U(j, 1 bits)) {
      plabCacheTop(j) := plabTopFillData(0)
      plabCacheEnd(j) := plabEndFillData(0)
      plabCacheValid(j) := True
      plabRefillBusy(j) := False
    } elsewhen(plabTopEndFillValid(1) && plabTopEndFillIdx(1) === U(j, 1 bits)) {
      plabCacheTop(j) := plabTopFillData(1)
      plabCacheEnd(j) := plabEndFillData(1)
      plabCacheValid(j) := True
      plabRefillBusy(j) := False
    }
  }

  // ToCopySurvivor event arbitration
  // 每拍只发一种事件，避免 isTypeArray 和 normal done 的 owner 覆盖。
  val grantDone0 = survivorDonePending(0)
  val grantDone1 = !survivorDonePending(0) && survivorDonePending(1)
  val hasDoneEvent = grantDone0 || grantDone1
  val grantTypeArray0 = !hasDoneEvent && typeArrayPending(0)
  val grantTypeArray1 = !hasDoneEvent && !typeArrayPending(0) && typeArrayPending(1)

  when(hasDoneEvent){
    io.ToCopySurvivor.done.valid := True
    io.ToCopySurvivor.done.payload.DoneOwner := Mux(grantDone0, survivorDoneOwner(0), survivorDoneOwner(1))
    io.ToCopySurvivor.done.payload.DestOopPtr := Mux(grantDone0, survivorDoneDest(0), survivorDoneDest(1))

    when(grantDone0) {
      survivorDonePending(0) := False
    } otherwise {
      survivorDonePending(1) := False
    }
  }.elsewhen(grantTypeArray0 || grantTypeArray1) {
    io.ToCopySurvivor.done.payload.isTypeArray := True
    io.ToCopySurvivor.done.payload.DoneOwner := Mux(grantTypeArray0, typeArrayOwner(0), typeArrayOwner(1))

    when(grantTypeArray0) {
      typeArrayPending(0) := False
    } otherwise {
      typeArrayPending(1) := False
    }
  }

  // ToFetch forwarding pulse arbitration
  val grantFetch0 = toFetchPending(0)
  val grantFetch1 = !toFetchPending(0) && toFetchPending(1)

  when(grantFetch0 || grantFetch1) {
    io.ToFetch.writeForward.valid := True

    io.ToFetch.writeForward.payload.srcOopPtr := Mux(grantFetch0, toFetchSrcOopPtr(0), toFetchSrcOopPtr(1))
    io.ToFetch.writeForward.payload.writeValue := Mux(grantFetch0, toFetchWriteValue(0), toFetchWriteValue(1))

    when(grantFetch0) {
      toFetchPending(0) := False
    } otherwise {
      toFetchPending(1) := False
    }
  }

  // Copy arbitration

  def driveCopy(i: Int): Unit = {
    val totalBytes = (slotCtx(i).runtime.size * U(8)).resize(32)
    val objectArrayHeaderBytes = Mux(
      io.ConfigIO.UseCompressedKlassPointer,
      U(16, 32 bits),
      U(20, 32 bits)
    )
    val copyOffsetBytes = Mux(
      slotCtx(i).runtime.kid === U(ObjectArrayKlassID, 32 bits),
      objectArrayHeaderBytes,
      U(8, 32 bits)
    )
    val copySize = Mux(
      slotCtx(i).runtime.kid === U(ObjectArrayKlassID, 32 bits),
      totalBytes - objectArrayHeaderBytes,
      totalBytes - U(8, 32 bits)
    )
    io.ToCopy.cmd.valid := True
    io.ToCopy.cmd.payload.Size := copySize
    io.ToCopy.cmd.payload.SrcOopPtr := slotCtx(i).configs.srcOopPtr + copyOffsetBytes.resize(GCElementWidth)
    io.ToCopy.cmd.payload.DestOopPtr := slotCtx(i).runtime.destOopPtr + copyOffsetBytes.resize(GCElementWidth)

    when(io.ToCopy.cmd.fire) {
      slotCtx(i).runtime.copyIssued := True
      copyBusy := True
      copyOwner := U(i, 1 bits)
    }
  }

  val wantCopy = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    wantCopy(i) := slotValid(i) && slotIsRunCopyTrace(i) && !slotCtx(i).runtime.copyIssued
  }

  val grantCopy0 = !copyBusy && wantCopy(0) && (!wantCopy(1) || olderSlot === U(0, 1 bits))
  val grantCopy1 = !copyBusy && wantCopy(1) && (!wantCopy(0) || olderSlot === U(1, 1 bits))

  when(grantCopy0) {
    driveCopy(0)
  }.elsewhen(grantCopy1) {
    driveCopy(1)
  }

  // Trace arbitration
  def driveTrace(i: Int): Unit = {
    val chunkRem = slotCtx(i).configs.srcLength % io.ConfigIO.ChunkSize

    io.ToTrace.cmd.valid := True
    io.ToTrace.cmd.payload.OopType := U(NotArrayOop)
    io.ToTrace.cmd.payload.KlassPtr := slotCtx(i).configs.klassPtr
    io.ToTrace.cmd.payload.SrcOopPtr := slotCtx(i).configs.srcOopPtr
    io.ToTrace.cmd.payload.DestOopPtr := slotCtx(i).runtime.destOopPtr
    io.ToTrace.cmd.payload.Kid := slotCtx(i).runtime.kid
    io.ToTrace.cmd.payload.ScanningInYoung := slotCtx(i).runtime.destRegionAttr(15 downto 8) === U(0, 8 bits)
    io.ToTrace.cmd.payload.ArrayLength := slotCtx(i).configs.srcLength
    io.ToTrace.cmd.payload.PartialArrayStart := U(0)
    io.ToTrace.cmd.payload.StepIndex := chunkRem.resize(32)
    io.ToTrace.cmd.payload.StepNCreate := Mux(slotCtx(i).configs.srcLength > chunkRem, U(1), U(0)).resize(32)

    when(io.ToTrace.cmd.fire) {
      slotCtx(i).runtime.traceIssued := True
      traceBusy := True
      traceOwner := U(i, 1 bits)
    }
  }

  val wantTrace = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    // copyIssued 只在 ToCopy.cmd.fire 时置位，因此 Trace 绝不会早于 Copy 请求真正发出。
    wantTrace(i) := slotValid(i) && slotIsRunCopyTrace(i) && slotCtx(i).runtime.copyIssued &&
        !slotCtx(i).runtime.traceIssued && slotCtx(i).runtime.kid =/= U(TypeArrayKlassID, 32 bits)
  }

  val grantTrace0 = !traceBusy && wantTrace(0) && (!wantTrace(1) || olderSlot === U(0, 1 bits))
  val grantTrace1 = !traceBusy && wantTrace(1) && (!wantTrace(0) || olderSlot === U(1, 1 bits))

  when(grantTrace0) {
    driveTrace(0)
  }.elsewhen(grantTrace1) {
    driveTrace(1)
  }

  // Allocate arbitration
  def driveAllocate(i: Int): Unit = {
    io.ToAllocate.cmd.valid := True
    io.ToAllocate.cmd.payload.Size := slotCtx(i).runtime.size
    io.ToAllocate.cmd.payload.DestAttrType := Mux(
      slotCtx(i).runtime.plabForceOld,
      U(1, 8 bits),
      slotCtx(i).runtime.destRegionAttr(15 downto 8)
    )

    when(io.ToAllocate.cmd.fire) {
      slotCtx(i).runtime.allocIssued := True
      allocBusy := True
      allocOwner := U(i, 1 bits)
    }
  }

  val wantAlloc = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    val idx = slotCtx(i).runtime.plabTargetIdx
    wantAlloc(i) := slotValid(i) && slotIsAllocCache(i) &&
      !slotCtx(i).runtime.allocIssued && !slotAllocCacheHazard(i) && ownsPlab(i, idx)
  }

  when(!allocBusy) {
    when(wantAlloc(0)) {
      driveAllocate(0)
    } .elsewhen(wantAlloc(1)) {
      driveAllocate(1)
    }
  }
}

object GCOopCopy2SurvivorVerilog extends App {
  Config.spinal.generateVerilog(new GCOopCopy2Survivor())
}