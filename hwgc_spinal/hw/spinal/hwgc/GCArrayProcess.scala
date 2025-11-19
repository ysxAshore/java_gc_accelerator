package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCArrayProcess extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Fetch2Process = slave(new ProcessUnit)
    val Process2Trace = master(new GCProcess2Trace)
    val Mreq = master(new LocalMMUIO)
    val ConfigIO = slave(new GCArrayProcessConfigIO)
  }

  // default value
  io.Fetch2Process.Ready := False
  io.Fetch2Process.Done := False
  io.Process2Trace.Valid := False

  object overall_state extends SpinalEnum {
    val s_idle, s_readSrcLen, s_readDestLen, s_readChunk, s_writeDestLen, s_readStepper, s_doTrace, s_waitDone = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  val pss = RegInit(U(0, MMUAddrWidth bits))
  val oopType = RegInit(U(0, GCOopTypeWidth bits))
  val srcOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val destOopPtr = RegInit(U(0,MMUAddrWidth bits))
  val markWord = RegInit(U(0, MMUDataWidth bits))

  when(state === overall_state.s_idle){
    io.Fetch2Process.Ready := True
    when(io.Fetch2Process.Valid && io.Fetch2Process.Ready){
      pss := io.ConfigIO.ParScanThreadStatePtr
      oopType := io.Fetch2Process.OopType
      srcOopPtr := io.Fetch2Process.SrcOopPtr
      markWord := io.Fetch2Process.MarkWord
      destOopPtr := io.Fetch2Process.MarkWord & ~LOCK_MASK_IN_PLACE

      state := overall_state.s_readSrcLen
    }
  }

  val srcLength = RegInit(U(0, IntWidth bits))
  val reqIssued = RegInit(False)
  when(state === overall_state.s_readSrcLen){
    issueReq(io.Mreq, srcOopPtr + ArrayLenOff, False, U(0), U(0), reqIssued) { rd =>
      srcLength := rd(IntWidth-1 downto 0)
      state := overall_state.s_readDestLen
    }
  }

  val destLength = RegInit(U(0, IntWidth bits))
  when(state === overall_state.s_readDestLen){
    issueReq(io.Mreq, destOopPtr + ArrayLenOff, False, U(0), U(0), reqIssued) { rd =>
      destLength := rd(IntWidth-1 downto 0)
      state := overall_state.s_readChunk
    }
  }

  val chunk_size = RegInit(U(0, IntWidth bits))
  when(state === overall_state.s_readChunk){
    issueReq(io.Mreq, pss + PARTIAL_ARRAY_CHUNK_SIZE_OFFSET, False, U(0), U(0), reqIssued) { rd =>
      chunk_size := rd(IntWidth-1 downto 0)
      state := overall_state.s_writeDestLen
    }
  }


  val halfBytesOnes = U((BigInt(1) << (MMUDataWidth / 8 / 2)) - 1, MMUDataWidth / 8 bits)
  when(state === overall_state.s_writeDestLen){
    issueReq(io.Mreq, destOopPtr + ArrayLenOff, True, halfBytesOnes, destLength + chunk_size, reqIssued) { rd =>
      state := overall_state.s_readStepper
    }
  }

  val stepIndex = RegInit(U(0, IntWidth bits))
  val stepNCreate = RegInit(U(0, IntWidth bits))
  when(state === overall_state.s_readStepper){
    issueReq(io.Mreq, pss + PARTIAL_ARRAY_STEPPER_OFFSET, False, U(0), U(0), reqIssued) { rd =>
      val task_num = destLength / chunk_size
      val remaining_tasks = (srcLength - destLength) / chunk_size
      val task_limit = rd(IntWidth - 1 downto 0)
      val task_fanout = rd(63 downto IntWidth)
      val max_pending = (task_fanout - U(1)) * task_num + U(1)
      val pending = max_pending.min(remaining_tasks).min(task_limit)
      stepNCreate := task_fanout.min(remaining_tasks.min(task_limit + U(1)) - pending)
      stepIndex := destLength + chunk_size

      state := overall_state.s_doTrace
    }
  }

  when(state === overall_state.s_doTrace){
    io.Process2Trace.Valid := True
    io.Process2Trace.OopType := PartialArrayOop
    io.Process2Trace.SrcOopPtr := srcOopPtr
    io.Process2Trace.DestOopPtr := destOopPtr
    io.Process2Trace.ArrayLength := srcLength
    io.Process2Trace.PartialArrayStart := destLength
    io.Process2Trace.StepIndex := stepIndex
    io.Process2Trace.StepNCreate := stepNCreate

    when(io.Process2Trace.Valid && io.Process2Trace.Ready){
      state := overall_state.s_waitDone
    }
  }

  when(state === overall_state.s_waitDone){
    when(io.Process2Trace.Done){
      io.Fetch2Process.Done := True
      state := overall_state.s_idle
    }
  }
}
