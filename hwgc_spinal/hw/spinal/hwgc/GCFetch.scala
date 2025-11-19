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
    val s_idle, s_readOop, s_readMW, s_send, s_waitDone = newElement()
  }

  val state = RegInit(overall_state.s_idle)

  val oopType = RegInit(U(0, GCOopTypeWidth bits))
  val task = RegInit(U(0, MMUAddrWidth bits))

  // mem related regs
  val issued = RegInit(False)
  val memData = RegInit(U(0, MMUAddrWidth bits))

  // idle: take task from stack
  when(state === overall_state.s_idle){
    io.Stack2Fetch.ready := True
    when(io.Stack2Fetch.fire){
      val payload = io.Stack2Fetch.payload
      when(payload(GCOopTypeWidth - 1 downto 0) === U(OopTag)){
        oopType := U(OopTag)
        task := payload - U(OopTag)
      }.otherwise{
        oopType := U(PartialArrayTag)
        task := payload - U(PartialArrayTag)
      }

      // reset mem flags for next task
      issued := False
      memData := U(0)

      state := overall_state.s_loadMem
    }
  }

  // readOop: oopTag -> send mreq
  when(state === overall_state.s_readOop){
    when(oopType === U(OopTag)){
     issueReq(io.FetchMReq, task, False, U(0), U(0), issued){ rd =>
       memData := rd
       state := overall_state.s_readMW
     }
    }.otherwise{
      memData := task
      state := overall_state.s_readMW
    }
  }

  val markWord = RegInit(U(0, MMUDataWidth bits))
  when(state === overall_state.s_readMW){
    issueReq(io.FetchMReq, memData + MarkWordOff, False, U(0), U(0), issued){ rd =>
      markWord := rd
      state := overall_state.s_send
    }
  }


  // send
  when(state === overall_state.s_send){
    val processUnit = Mux(oopType === U(OopTag), io.Fetch2OopProcess, io.Fetch2ArrayProcess)
    processUnit.Valid := True

    processUnit.OopType := oopType
    processUnit.SrcOopPtr := memData
    processUnit.MarkWord := markWord

    when(processUnit.Valid && processUnit.Ready){
      state := overall_state.s_waitDone
    }
  }

  // waitDone
  when(state === overall_state.s_waitDone){
    when(oopType === U(OopTag)){
      when(io.Fetch2OopProcess.Done){
        state := overall_state.s_idle
      }
    }.otherwise{
      when(io.Fetch2ArrayProcess.Done){
        state := overall_state.s_idle
      }
    }
  }

}
