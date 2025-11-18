package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/* GCFetch read task from TaskStack, the dispath to arrayProcess and oopProcess */
class GCFetch extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Stack2Fetch = slave Stream UInt(MMUAddrWidth bits)
    val Fetch2ArrayProcess = master(new ProcessUnit)
    val Fetch2OopProcess = master(new ProcessUnit)
    val FetchMReq = master(new LocalMMUIO)
  }

  object overall_state extends SpinalEnum {
    val s_idle, s_doWork, s_end = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  val oopType = RegInit(U(0, GCOopTypeWidth bits))
  val task = RegInit(U(0, MMUAddrWidth bits))

  when(state === overall_state.s_idle){
    io.Stack2Fetch.ready := True
    when(io.Stack2Fetch.fire){
      state := overall_state.s_doWork
      when(io.Stack2Fetch.payload(GCOopTypeWidth - 1 downto 0) === U(OopTag)){
        oopType := OopTag
        task := io.Stack2Fetch.payload - OopTag
      }.otherwise{
        oopType := PartialArrayTag
        task := io.Stack2Fetch.payload - OopTag
      }
    }
  }

  val hasSent = RegInit(False)
  when(state === overall_state.s_doWork){
    when(oopType === OopTag){
      when(!hasSent){
        val issued = RegInit(False)
        issueReq(io.FetchMReq, task, False, U(0), U(0), issued){ rd =>
          io.Fetch2ArrayProcess.SrcOopPtr := rd
          io.Fetch2ArrayProcess.OopType := oopType
          io.Fetch2ArrayProcess.Valid := True
          when(io.Fetch2ArrayProcess.Valid && io.Fetch2ArrayProcess.Ready){
            hasSent := True
          }
        }
      }
      when(io.Fetch2OopProcess.Done){
        hasSent := False
        state := overall_state.s_end
      }
    }.otherwise{
      when(!hasSent){
        io.Fetch2ArrayProcess.SrcOopPtr := task
        io.Fetch2ArrayProcess.OopType := oopType
        io.Fetch2ArrayProcess.Valid := True
        when(io.Fetch2ArrayProcess.Valid && io.Fetch2ArrayProcess.Ready){
          hasSent := True
        }
      }
      when(io.Fetch2ArrayProcess.Done){
        hasSent := False
        state := overall_state.s_end
      }
    }
  }

  when(state === overall_state.s_end){
    state := overall_state.s_idle
  }
}
