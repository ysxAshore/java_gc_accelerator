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

  val ArrayElementOff = 24

  val LhKidOff = 8
  val VTableLenOff = 160
  val NonstaticOopMapSizeOff = 296
  val ITableLenOff = 300
  val REFERENCE_TYPE = 315
  val VTableOff = 464
  val StaticFieldOff = 184

  val GCTaskQueue_Size = 1 << 17
  val GCScannerTask_Size = 8
  val GCObjectPtr_Size = 8
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