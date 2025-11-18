package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/* GCFetch read task from TaskStack, the dispatch to arrayProcess and oopProcess */
class GCFetch extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Stack2Fetch = slave Stream UInt(MMUAddrWidth bits)
    val Fetch2ArrayProcess = master(new ProcessUnit)
    val Fetch2OopProcess = master(new ProcessUnit)
    val FetchMReq = master(new LocalMMUIO)
  }

  // default value
  io.Stack2Fetch.ready := False
  io.Fetch2ArrayProcess.Valid := False
  io.Fetch2OopProcess.Valid := False
  io.FetchMReq.Request.valid := False
  io.FetchMReq.Request.payload.clearAll()
  io.FetchMReq.Response.payload.clearAll()
  io.FetchMReq.Response.ready := False

  object overall_state extends SpinalEnum {
    val s_idle, s_loadMem, s_send, s_waitDone, s_end = newElement()
  }

  val state = RegInit(overall_state.s_idle)

  val oopType = RegInit(U(0, GCOopTypeWidth bits))
  val task = RegInit(U(0, MMUAddrWidth bits))

  // mem related regs
  val issued = RegInit(False)
  val memDone = RegInit(False)
  val memData = RegInit(U(0, MMUAddrWidth bits))

  // idle: take task from stack
  when(state === overall_state.s_idle){
    io.Stack2Fetch.ready := True
    when(io.Stack2Fetch.fire){
      val payload = io.Stack2Fetch.payload
      when(payload(GCOopTypeWidth - 1 downto 0) === U(OopTag)){
        oopType := OopTag
        task := payload - OopTag
      }.otherwise{
        oopType := PartialArrayTag
        task := payload - PartialArrayTag
      }

      // reset mem flags for next task
      issued := False
      memDone := False
      memData := U(0)

      state := overall_state.s_loadMem
    }
  }

  // loadMem: oopTag -> send mreq
  when(state === overall_state.s_loadMem){
    when(oopType === OopTag){
     issueReq(io.FetchMReq, task, False, U(0), U(0), issued){ rd =>
       memData := rd
       memDone := True
     }

      when(memDone) {
        state := overall_state.s_send
      }
    }.otherwise{
      memData := task
      memDone := True
      state := overall_state.s_send
    }
  }

  // send
  when(state === overall_state.s_send){
    when(oopType === OopTag){
      io.Fetch2OopProcess.SrcOopPtr := memData
      io.Fetch2OopProcess.OopType := oopType
      io.Fetch2OopProcess.Valid := True

      when(io.Fetch2OopProcess.Valid && io.Fetch2OopProcess.Ready){
        state := overall_state.s_waitDone
      }
    }.otherwise{
      io.Fetch2ArrayProcess.SrcOopPtr := memData
      io.Fetch2ArrayProcess.OopType := oopType
      io.Fetch2ArrayProcess.Valid := True

      when(io.Fetch2ArrayProcess.Valid && io.Fetch2ArrayProcess.Ready){
        state := overall_state.s_waitDone
      }
    }
  }

  // waitDone
  when(state === overall_state.s_waitDone){
    when(oopType === OopTag){
      when(io.Fetch2OopProcess.Done){
        state := overall_state.s_end
      }
    }.otherwise{
      when(io.Fetch2ArrayProcess.Done){
        state := overall_state.s_end
      }
    }
  }

  when(state === overall_state.s_end){
    state := overall_state.s_idle
  }
}
