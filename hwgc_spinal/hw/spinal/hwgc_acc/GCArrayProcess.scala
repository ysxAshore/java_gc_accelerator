package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, MyStateMachine}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

class GCArrayProcess extends Module with HWParameters with GCTopParameters with GCParameters {
  val io = new Bundle {
    val Mreq              = master(new LocalMMUIO)
    val Fetch2Process     = slave(new GCToProcessUnit)
    val Process2Trace     = master(new GCToTrace)
    val gcWriteSrcOopPtr  = slave(new GCWriteSrcOopPtr)
    val ConfigIO          = slave(new GCArrayProcessConfigIO)
    val DebugTimeStamp    = in UInt(64 bits)
  }

  // defaults
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := True

  io.Fetch2Process.clearIn()
  io.Process2Trace.clearOut()

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCArrayProcess<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }

  // task context registers
  val oopType    = RegInit(U(0, GCOopTypeWidth bits))
  val srcOopPtr  = RegInit(U(0, GCElementWidth bits))
  val destOopPtr = RegInit(U(0, GCElementWidth bits))
  val markWord   = RegInit(U(0, GCElementWidth bits))
  val srcLength  = RegInit(U(0, 32 bits))

  val incomingFwdValid = io.gcWriteSrcOopPtr.writeForward.valid
  val incomingFwdObj   = io.gcWriteSrcOopPtr.writeForward.payload.srcOopPtr
  val incomingFwdValue = io.gcWriteSrcOopPtr.writeForward.payload.writeValue

  // the oop has marked
  def isForwardedMark(mark: UInt): Bool = (mark & U(3, GCElementWidth bits)) === U(3, GCElementWidth bits)

  val step_index  = RegInit(U(0, 32 bits))
  val dest_length = RegInit(U(0, 32 bits))
  val step_ncreate = RegInit(U(0, 32 bits))
  val heap_region = RegInit(U(0, GCElementWidth bits))

  val task_limit  = io.ConfigIO.StepperOffset(31 downto 0)
  val task_fanout = io.ConfigIO.StepperOffset(63 downto 32)

  // heap region cache
  val heapRegionCacheEntries = 4
  val heapRegionCache = Vec.fill(heapRegionCacheEntries)(RegInit(False))
  val heapRegionCacheTag = Vec.fill(heapRegionCacheEntries)(RegInit(U(0, MMUAddrWidth bits)))
  val heapRegionCacheValid = Vec.fill(heapRegionCacheEntries)(RegInit(False))
  val heapRegionCacheReplacePtr = RegInit(U(0, log2Up(heapRegionCacheEntries) bits))

  val heapRegionAddrLookup = (io.ConfigIO.HeapRegionBiasedBase + ((destOopPtr >> io.ConfigIO.HeapRegionShiftBy) << U(3))).resize(MMUAddrWidth)
  val heapRegionHitVec = Vec.fill(heapRegionCacheEntries)(Bool())
  for (i <- 0 until heapRegionCacheEntries) {
    heapRegionHitVec(i) := heapRegionCacheValid(i) && heapRegionCacheTag(i) === heapRegionAddrLookup
  }

  val heapRegionHit = heapRegionHitVec.orR
  val heapRegionHitIndex = OHToUInt(heapRegionHitVec.asBits)

  // Main StateMachine
  val fsm = new MyStateMachine {
    val IDLE            = new State with EntryPoint
    val WAIT_FORWARD    = new State
    val READ_DEST_LEN   = new State // WRITE_DEST_LEN in TRACE module(DISPATCH)
    val CALC_STEP       = new State
    val LOOKUP_HEAP_REG = new State
    val READ_HUMONGOUS  = new State
    val SEND_TRACE      = new State
    val WAIT_TRACE_DONE = new State

    IDLE.whenIsActive {
      io.Fetch2Process.cmd.ready := True

      when(io.Fetch2Process.cmd.fire) {
        val inputFwdHit = incomingFwdValid && io.Fetch2Process.cmd.payload.SrcOopPtr === incomingFwdObj
        val inputMarkWord = Mux(
          inputFwdHit,
          incomingFwdValue,
          io.Fetch2Process.cmd.payload.MarkWord
        )

        oopType    := io.Fetch2Process.cmd.payload.OopType
        srcOopPtr  := io.Fetch2Process.cmd.payload.SrcOopPtr
        markWord   := inputMarkWord
        destOopPtr := inputMarkWord & ~U(3, GCElementWidth bits)
        srcLength  := io.Fetch2Process.cmd.payload.SrcLength

        issued := False

        // PartialArray 只有在 MarkWord 已经变成 forwarding pointer 后，才能把 markWord & ~3 当作目标对象地址
        when(isForwardedMark(inputMarkWord)) {
          goto(READ_DEST_LEN)
        } otherwise {
          goto(WAIT_FORWARD)
        }

        dbg(Seq("Receive task from Fetch Module, srcOopPtr=", io.Fetch2Process.cmd.payload.SrcOopPtr,", markWord=", inputMarkWord))
      }
    }

    WAIT_FORWARD.whenIsActive {
      val currentFwdHit = incomingFwdValid && srcOopPtr === incomingFwdObj

      when(currentFwdHit) {
        // 对已经 fire 到 ArrayProcess、但携带旧 MarkWord 的任务进行修正
        markWord   := incomingFwdValue
        destOopPtr := incomingFwdValue & ~U(3, GCElementWidth bits)
        goto(READ_DEST_LEN)

        dbg(Seq("Receive late forwarding in ArrayProcess, srcOopPtr=", srcOopPtr, ", newMarkWord=", incomingFwdValue))

      }.elsewhen(isForwardedMark(markWord)) {
        goto(READ_DEST_LEN)
      }
    }

    READ_DEST_LEN.whenIsActive {
      val addr = (destOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U(12), U(16))).resize(MMUAddrWidth)
      val offset = addr(log2Up(LineBytesNum) - 1 downto 0)
      val alignedAddr = addr & ~U(LineBytesNum - 1, MMUAddrWidth bits)

      // ArrayProcess only does reads here.  Do the former Unaligned-adapter
      // line alignment locally, then extract the requested word from the line.
      issueDirectRead(io.Mreq, alignedAddr, U(4), CALC_STEP) { rd =>
        val shifted = rd |>> (offset << 3)
        dest_length := shifted(31 downto 0)
      }
    }

    CALC_STEP.whenIsActive {
      val task_num = (dest_length / io.ConfigIO.ChunkSize).resize(32)
      val remaining_tasks = ((srcLength - dest_length) / io.ConfigIO.ChunkSize).resize(32)
      val max_pending = ((task_fanout - U(1)) * task_num + U(1)).resize(32)
      val pending = max_pending.min(remaining_tasks).min(task_limit)
      step_ncreate := task_fanout.min(remaining_tasks.min(task_fanout + U(1)) - pending).resize(32)
      step_index := dest_length + io.ConfigIO.ChunkSize

      goto(LOOKUP_HEAP_REG)
    }

    LOOKUP_HEAP_REG.whenIsActive {
      when(heapRegionHit) {
        goto(SEND_TRACE)

      } otherwise {
        val addr = heapRegionAddrLookup.resize(MMUAddrWidth)
        val offset = addr(log2Up(LineBytesNum) - 1 downto 0)
        val alignedAddr = addr & ~U(LineBytesNum - 1, MMUAddrWidth bits)

        issueDirectRead(io.Mreq, alignedAddr, U(8), READ_HUMONGOUS) { rd =>
          val shifted = rd |>> (offset << 3)
          heap_region := shifted(GCElementWidth - 1 downto 0)
        }
      }
    }

    READ_HUMONGOUS.whenIsActive {
      val addr = (heap_region.resize(MMUAddrWidth) + U"xbc").resize(MMUAddrWidth)
      val offset = addr(log2Up(LineBytesNum) - 1 downto 0)
      val alignedAddr = addr & ~U(LineBytesNum - 1, MMUAddrWidth bits)

      issueDirectRead(io.Mreq, alignedAddr, U(4), SEND_TRACE) { rd =>
        val shifted = rd |>> (offset << 3)
        heapRegionCacheValid(heapRegionCacheReplacePtr) := True
        heapRegionCacheTag(heapRegionCacheReplacePtr)   := heapRegionAddrLookup
        heapRegionCache(heapRegionCacheReplacePtr)      := (shifted(31 downto 0) & U(2, 32 bits)) =/= U(0)
        heapRegionCacheReplacePtr := heapRegionCacheReplacePtr + 1
      }
    }

    SEND_TRACE.whenIsActive {
      io.Process2Trace.cmd.valid := True

      io.Process2Trace.cmd.payload.OopType := oopType
      io.Process2Trace.cmd.payload.SrcOopPtr := srcOopPtr
      io.Process2Trace.cmd.payload.DestOopPtr := destOopPtr
      io.Process2Trace.cmd.payload.ScanningInYoung := heapRegionCache(heapRegionHitIndex)
      io.Process2Trace.cmd.payload.StepIndex := step_index
      io.Process2Trace.cmd.payload.StepNCreate := step_ncreate
      io.Process2Trace.cmd.payload.ArrayLength := dest_length + io.ConfigIO.ChunkSize
      io.Process2Trace.cmd.payload.PartialArrayStart := dest_length

      when(io.Process2Trace.cmd.fire) {
        goto(WAIT_TRACE_DONE)

        dbg(Seq("This task has sent to Trace Module"))
      }
    }

    WAIT_TRACE_DONE.whenIsActive {
      // 这里不需要缓存 Process2Trace.Done 信号
      // 因为在当前设计中，Process2Trace.Done 信号只会在 Trace Module 完成当前任务后发出一次，并且在 FSM 中，我们已经确保只有在发送了有效的 Process2Trace 信号后才会进入 WAIT_TRACE_DONE 状态。因此，在 WAIT_TRACE_DONE 状态中，我们可以直接监测 Process2Trace.Done 信号，而不需要担心它会被重复触发或者丢失
      when(io.Process2Trace.Done) {
        io.Fetch2Process.Done := True

        goto(IDLE)

        dbg(Seq("This task done"))
      }
    }
  }
}

object GCArrayProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCArrayProcess())
}