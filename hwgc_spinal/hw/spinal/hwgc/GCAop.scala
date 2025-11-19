package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class StageData() extends Bundle with HWParameters{
  val dest = UInt(MMUAddrWidth bits)
  val pss = UInt(MMUAddrWidth bits)
  val ct_ptr = UInt(MMUAddrWidth bits)
  val byte_map = UInt(MMUAddrWidth bits)
  val card_index = UInt(MMUDataWidth bits)
  val index = UInt(MMUDataWidth bits)
  val buffer = UInt(MMUAddrWidth bits)
  val res = UInt(MMUDataWidth bits)
}

class GCAop extends Bundle with GCParameters with HWParameters {
  val io = new Bundle{
    val Aop = slave (new AopParameters)
    val aopMReqs = Vec.fill(GCaopWorkStages)(new LocalMMUIO)
    aopMReqs.foreach(master(_))
  }

  // default value
  for(i <- 0 until GCaopWorkStages){
    io.aopMReqs(i).Request.valid := False
    io.aopMReqs(i).Request.payload.clearAll()
    io.aopMReqs(i).Response.ready := False
  }

  val stages = Seq.tabulate(GCaopWorkStages) { _ =>
    new Area {
      val valid = RegInit(False)
      val reg = Reg(StageData().getZero)
      val reqDone = RegInit(False)
      val reqIssued = RegInit(False)
      val responseData = RegInit(U(0, MMUDataWidth bits))
    }
  }

  val allBytesOnes = U((BigInt(1) << (MMUDataWidth / 8)) - 1, MMUDataWidth / 8 bits)

  // @todo : Aop backpressure -> (!stages(0).valid) && (!stages(1).valid || stages(1).reqDone)
  io.Aop.Ready := !stages(0).valid
  io.Aop.Done := !stages.map(_.valid).reduce(_ || _)

  when(io.Aop.Valid && io.Aop.Ready){
    stages(0).valid := True
    stages(0).reg.pss := io.Aop.ParScanThreadStatePtr
    stages(0).reg.dest := io.Aop.DestOopPtr
  }

  def mmuOpForIndex(i: Int, s: StageData): (Bool, UInt, Bool, UInt, UInt) = {
    i match {
      case 0 => (True, s.pss + PSS_CARD_TABLE_OFFSET, False, U(0), U(0))
      case 1 => (True, s.ct_ptr + BYTE_MAP_OFFSET, False, U(0), U(0))
      case 2 => (True, s.ct_ptr + BYTE_MAP_BASE_OFFSET, False, U(0), U(0))
      case 3 => (True, s.pss + LAST_ENQUEUED_CARD_OFFSET, False, U(0), U(0))
      case 4 => (True, s.pss + QSET_OFFSET + QSET_QUEUE_OFFSET + INDEX_OFFSET, False, U(0), U(0))
      case 5 => (True, s.pss + QSET_OFFSET + QSET_QUEUE_OFFSET + BUFFER_OFFSET, False, U(0), U(0))
      case 7 => (True, s.buffer + (s.index - U(1)) * GCObjectPtr_Size, True, allBytesOnes, s.res)
      case 8 => (True, s.pss + QSET_OFFSET + QSET_QUEUE_OFFSET + INDEX_OFFSET, True, allBytesOnes, (s.index - U(1)) * GCObjectPtr_Size)
      case 9 => (True, s.pss + LAST_ENQUEUED_CARD_OFFSET, True, allBytesOnes, s.card_index)
      case _ => (False, U(0), False, U(0), U(0))
    }
  }
  def resetStage(i: Int): Unit = {
    stages(i).valid := False
    stages(i).reqDone := False
  }
  def transferStage(i: Int , j: Int) : Unit = {
    resetStage(i)
    stages(j).valid := True
    stages(j).reg := stages(i).reg
  }

  // send and save response
  for(i <- 0 until GCaopWorkStages){
    val s = stages(i)
    val mreq = io.aopMReqs(i)
    val (want, addr, isWrite, wmask, wdata) = mmuOpForIndex(i, s.reg)
    when(s.valid && !s.reqDone && want) {
      issueReq(mreq, addr, isWrite, wmask, wdata, s.reqIssued) { rd =>
        s.reqDone := True
        s.responseData := rd
      }
    }
  }

  // reg handler logic
  for(i <- 0 until GCaopWorkStages){
    val s = stages(i)
    when(s.valid){
      i match {
        case 0 =>
          when(s.reqDone){
            when(!stages(1).valid){
              transferStage(0, 1)
              stages(1).reg.ct_ptr := s.responseData
            }
          }
        case 1 =>
          when(s.reqDone){
            when(!stages(2).valid){
              transferStage(1, 2)
              stages(2).reg.byte_map := s.responseData
            }
          }
        case 2 =>
          when(s.reqDone){
            when(!stages(3).valid){
              transferStage(2, 3)
              stages(3).reg.res := s.responseData + (s.reg.dest >> 9)
              stages(3).reg.card_index := s.responseData + (s.reg.dest >> 9) - s.reg.byte_map
            }
          }
        case 3 =>
          when(s.reqDone){
            when(s.responseData === s.reg.card_index){
              resetStage(3)
            }.elsewhen(!stages(4).valid){
              transferStage(3, 4)
            }
          }
        case 4 =>
          when(s.reqDone){
            when(!stages(5).valid){
              transferStage(4, 5)
              stages(5).reg.index := s.responseData / U(8)
            }
          }
        case 5 =>
          when(s.reqDone){
            when(s.reg.index === U(0)){
              when(!stages(6).valid){
                transferStage(5, 6)
                stages(6).reg.buffer := s.responseData
              }
            }.elsewhen(!stages(7).valid){
              transferStage(5, 7)
              stages(7).reg.buffer := s.responseData
            }
          }
        case 6 =>
          // @todo: interrupt
        case 9 =>
          when(s.reqDone){
            resetStage(9)
          }
        case _ =>
          when(s.reqDone){
            when(!stages(i+1).valid){
              transferStage(i, i+1)
            }
          }

      }
    }
  }
}
