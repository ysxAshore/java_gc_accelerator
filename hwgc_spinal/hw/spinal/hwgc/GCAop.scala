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
    val aopMReqs = Vec.fill(9)(new LocalMMUIO)
    aopMReqs.foreach(master(_))
  }

  val sReqIssued = Vec(RegInit(False), 9)
  val sValid = Vec(RegInit(False), 6)
  val sReg = Vec(RegInit(StageData().getZero), 6)

  io.Aop.Ready := !sValid(0)
  io.Aop.Done := !sValid.orR

  when(io.Aop.Valid && io.Aop.Ready){
    sValid(0) := True
    sReg(0).pss := io.Aop.ParScanThreadStatePtr
    sReg(0).dest := io.Aop.DestOopPtr
  }

  when(sValid(0) && !sValid(1)){
    // read ct_ptr
    issueReq(io.aopMReqs(0), sReg(0).pss + PSS_CARD_TABLE_OFFSET, False, U(0), U(0), sReqIssued(0)){ rd =>
      sValid(0) := False
      sValid(1) := True
      sReg(1) := sReg(0)
      sReg(1).ct_ptr := rd
    }
  }

  when(sValid(1) && !sValid(2)){
    // read byte_map and byte_map_base
    val completed = Vec.fill(2)(RegInit(False))
    when(!completed(0)) {
      issueReq(io.aopMReqs(1), sReg(1).ct_ptr + BYTE_MAP_OFFSET, False, U(0), U(0), sReqIssued(1)) { rd =>
        completed(0) := True
        sReg(1).byte_map := rd
      }
    }
    when(!completed(1)) {
      issueReq(io.aopMReqs(2), sReg(1).ct_ptr + BYTE_MAP_BASE_OFFSET, False, U(0), U(0), sReqIssued(2)) { rd =>
        completed(1) := True
        sReg(1).res := rd + (sReg(1).dest >> 9)
      }
    }
    when(completed.andR){
      sValid(1) := False
      sValid(2) := True
      sReg(2) := sReg(1)
      sReg(2).card_index := sReg(1).res - sReg(1).byte_map
    }
  }

  when(sValid(2) && !sValid(3)){
    // read last enqueued card offset
    issueReq(io.aopMReqs(3), sReg(2).pss + LAST_ENQUEUED_CARD_OFFSET, False, U(0), U(0), sReqIssued(3)) { rd =>
      sValid(2) := False
      when(rd =/= sReg(2).card_index){
        sValid(3) := True
        sReg(3) := sReg(2)
      }
    }
  }

  when(sValid(3)){
    // read index and buffer
    val completed = Vec.fill(2)(RegInit(False))
    when(!completed(0)) {
      issueReq(io.aopMReqs(4), sReg(3).pss + QSET_OFFSET + QSET_QUEUE_OFFSET + INDEX_OFFSET, False, U(0), U(0), sReqIssued(4)) { rd =>
        completed(0) := True
        sReg(3).index := rd / U(8)
      }
    }
    when(!completed(1)) {
      issueReq(io.aopMReqs(5), sReg(3).pss + QSET_OFFSET + QSET_QUEUE_OFFSET + BUFFER_OFFSET, False, U(0), U(0), sReqIssued(2)) { rd =>
        completed(1) := True
        sReg(3).buffer := rd
      }
    }
    when(completed.andR){
      sValid(3) := False
      when(sReg(3).index === U(0) && !sValid(4)){
        sValid(4) := True
        sReg(4) := sReg(3)
      }.elsewhen(sReg(3).index =/= U(0) && !sValid(5)) {
        sValid(5) := True
        sReg(5) := sReg(3)
      }
    }
  }

  when(sValid(4) && !sValid(5)){
    // index === 0
    // interrupt
  }

  when(sValid(5)){
    // write buffer queue and last enqueued card offset
    val completed = Vec(RegInit(False), 3)
    val addr_buffer = sReg(5).buffer + (sReg(5).index - U(1)) * GCObjectPtr_Size
    when(!completed(0)) {
      issueReq(io.aopMReqs(6), addr_buffer, True, U(63), sReg(5).res, sReqIssued(6)) { rd =>
        completed(0) := True
      }
    }
    val addr_index = sReg(5).pss + QSET_OFFSET + QSET_QUEUE_OFFSET + INDEX_OFFSET
    when(!completed(1)) {
      issueReq(io.aopMReqs(7), addr_index, True, U(63), (sReg(5).index - 1) * GCObjectPtr_Size, sReqIssued(7)) { rd =>
        completed(1) := True
      }
    }
    val addr_cardIndex = sReg(5).pss + LAST_ENQUEUED_CARD_OFFSET
    when(!completed(2)) {
      issueReq(io.aopMReqs(8), addr_cardIndex, True, U(63), sReg(5).card_index, sReqIssued(8)) { rd =>
        completed(2) := True
      }
    }
    when(completed.andR){
      sValid(5) := False
    }
  }
}
