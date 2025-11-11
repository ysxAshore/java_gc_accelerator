package hwgc

import spinal.core._
import spinal.lib._
import scala.language.postfixOps

trait GCParameters {
  val GCTaskStack_Entry = 64
}

trait HWParameters {
  //true is boolean, True is Bool
  val DebugEnable = true

  val MMUAddrWidth = 64
  val MMUDataWidth = 64

  val LLCSourceMaxNum = 64
  val LLCSourceMaxNumBitSize = log2Up(LLCSourceMaxNum) + 1
}

class DebugInfoIO() extends Bundle with HWParameters{
  val DebugTimeStampe = UInt(64 bits)
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
    val ReseponseData = UInt(MMUDataWidth bits)
    val ReseponseSourceID = UInt(LLCSourceMaxNumBitSize bits)
  })
  override def asMaster(): Unit = {
    master(Request)
    slave(Response,ConherentRequsetSourceID)
  }
}