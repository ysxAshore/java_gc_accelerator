package hwgc

import spinal.core._
import spinal.lib._
import scala.language.postfixOps

trait GCParameters {
  val GCTaskStack_Entry = 64
  val GCTaskStack_SpillNeed = 63
  val GCTaskStack_ReadNeed = 8

  val PartialArrayTag = 2

  val GCOopTypeWidth = 2
  val CommonOop = 0
  val CompressedOop = 1
  val PartialArrayOop = 2

  val InstanceKlassID = 0
  val InstanceRefKlassID = 1
  val InstanceMirrorKlassID = 2
  val InstanceClassLoaderKlassID = 3
  val TypeArrayKlassID = 4
  val ObjectArrayKlassID = 5

  // OopDesc
  // --- Common Oop
  val MarkWordOff = 0
  val KlassOff = 8
  val ElementOff = 16
  // --- Array Oop
  val ArrayLenOff = 16
  val ArrayElementOff = 24
  // --- Ref Oop
  val REFERENT_OFFSET = 16
  val DISCOVERED_OFFSET = 40
  // --- mirror Oop
  val OopSizeOff = 36
  val staticOopFieldCountOff = 40

  val LhKidOff = 8
  val VTableLenOff = 160
  val NonstaticOopMapSizeOff = 296
  val ITableLenOff = 300
  val REFERENCE_TYPE = 315
  val VTableOff = 464
  val StaticFieldOff = 184

  val TypeOffSet = 1
  val Type_Young = 0
  val Type_Humongous = -2
  val Type_NoInCset = -1

  /*-------------- G1ParScanThreadState ------------*/
  val QSET_OFFSET = 24
  val PSS_CARD_TABLE_OFFSET = 96
  val PLAB_ALLOCATOR_OFFSET = 112
  val AGE_TABLE_OFFSET = 120
  val REGION_ATTR_DEST_OFFSET = 376
  val TENURING_THRESHOLD_OFFSET = 380
  val OBJCLOSURE_OFFSET = 384
  val LAST_ENQUEUED_CARD_OFFSET = 432
  val SURVIVING_YOUNG_WORDS_BASE_OFFSET = 464
  val OLD_GEN_IS_FULL_OFFSET = 488
  val PARTIAL_ARRAY_CHUNK_SIZE_OFFSET = 492
  val PARTIAL_ARRAY_STEPPER_OFFSET = 496
  val OBJ_ALLOC_STAT_OFFSET = 560

  /*-------------------- ObjClosure -----------------*/
  val SCANNING_IN_YOUNG_OFFSET = 32


  /*--------------------- CardTable -----------------*/
  val BYTE_MAP_OFFSET = 56
  val BYTE_MAP_BASE_OFFSET = 64

  val LogOfHRGrainBytes = 22
  val GCTaskQueue_Size = 1 << 17
  val GCScannerTask_Size = 8
  val GCObjectPtr_Size = 8
  val GCHeapRegionAttr_Size = 2
}

trait HWParameters {
  //true is boolean, True is Bool
  val DebugEnable = true

  val IntWidth = 32
  val MMUAddrWidth = 64
  val MMUDataWidth = 64

  val LLCSourceMaxNum = 64
  val LLCSourceMaxNumBitSize = log2Up(LLCSourceMaxNum) + 1
}

class DebugInfoIO() extends Bundle with HWParameters{
  val DebugTimeStampe = UInt(64 bits)
}

class GCParse2Trace extends Bundle with HWParameters with GCParameters with IMasterSlave{
  val Valid = in Bool()
  val Ready = out Bool()
  // some config parameters
  val RegionAttrBase = in UInt(MMUAddrWidth bits)
  val RegionAttrBiasedBase = in UInt(MMUAddrWidth bits)
  val RegionAttrShiftBy = in UInt(IntWidth bits)
  val HeapRegionBias = in UInt(IntWidth bits)
  val HeapRegionShiftBy = in UInt(IntWidth bits)
  val HumongousReclaimCandidatesBoolBase = in UInt(MMUAddrWidth bits)
  val ParScanThreadStatePtr = in UInt(MMUAddrWidth bits)

  // some parse module caculate parameters
  val OopType = in UInt(GCOopTypeWidth bits)
  val KlassPtr = in UInt(MMUAddrWidth bits)
  val SrcOopPtr = in UInt(MMUAddrWidth bits)
  val DestOopPtr = in UInt(MMUAddrWidth bits)
  val Kid = in UInt(IntWidth bits)
  val ArrayLength = in UInt(IntWidth bits)
  val PartialArrayStart = in UInt(IntWidth bits)
  val StepIndex = in UInt(IntWidth bits)
  val StepNCreate = in UInt(IntWidth bits)

  override def asMaster(): Unit = {
    in(Valid, RegionAttrBiasedBase, RegionAttrShiftBy, HeapRegionBias, HeapRegionShiftBy, HumongousReclaimCandidatesBoolBase, ParScanThreadStatePtr, OopType, KlassPtr, SrcOopPtr, DestOopPtr, Kid, ArrayLength, PartialArrayStart, StepIndex, StepNCreate)
    out(Ready)
  }
}

class GCTaskStackConfigIO extends Bundle with HWParameters with IMasterSlave{
  val TaskQueue_BottomAddr = in UInt(MMUAddrWidth bits)
  val TaskQueue_AgeTopAddr = in UInt(MMUAddrWidth bits)
  val TaskQueue_ElemsBase = in UInt(MMUAddrWidth bits)

  val TaskReady = out Bool()
  val TaskValid = in Bool()
  val TaskEndValid = out Bool()
  val TaskEndReady = in Bool()

  override def asMaster(): Unit = {
    in(TaskQueue_BottomAddr, TaskQueue_AgeTopAddr, TaskQueue_AgeTopAddr, TaskValid, TaskEndReady)
    out(TaskReady,TaskEndValid)
  }
}

class LocalMMUIO extends Bundle with HWParameters with IMasterSlave{
  //发出的访存请求
  val Request = master Stream(new Bundle{
    val RequestVirtualAddr = UInt(MMUAddrWidth bits)
    val RequestData = UInt(MMUDataWidth bits)
    val RequestSourceID = UInt(LLCSourceMaxNumBitSize bits)
    val RequestType_isWrite = Bool()
  })

  //读请求分发到的TL Link的事务编号
  val ConherentRequsetSourceID    = slave Flow(UInt(LLCSourceMaxNumBitSize bits))

  //Memoryloader一定能保证收回！
  val Response = slave Stream(new Bundle{
    val ResponseData = UInt(MMUDataWidth bits)
    val ResponseSourceID = UInt(LLCSourceMaxNumBitSize bits)
  })
  override def asMaster(): Unit = {
    master(Request)
    slave(Response,ConherentRequsetSourceID)
  }
}