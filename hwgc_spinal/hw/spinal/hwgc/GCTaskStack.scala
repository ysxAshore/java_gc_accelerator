package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/* GCTaskStack
 * @description: As a hardware buffer for the task queue in the JVM Hotspot
 *
 */
class GCTaskStack extends Module with GCParameters with HWParameters {
  val io = new Bundle {
    val Push = slave Stream UInt(MMUAddrWidth bits)
    val Pop = master Stream UInt(MMUAddrWidth bits)
    val PreFetch = master Stream UInt(MMUAddrWidth bits)
    val LocalMMUIO = master(new LocalMMUIO)
    val ConfigIO = slave(new GCTaskStackConfigIO)
    val DebugInfo = in(new DebugInfoIO)
  }

  // TaskStack config
  val data = RegInit(Vec.fill(GCTaskStack_Entry)(U(0, MMUAddrWidth bits)))
  val stack_top = RegInit(U(0, log2Up(GCTaskStack_Entry) bits))
  val stack_bottom = RegInit(U(0, log2Up(GCTaskStack_Entry) bits))

  // TaskQueue
  val queue_bottom_addr = RegInit(U(0, MMUAddrWidth bits))
  val queue_ageTop_addr = RegInit(U(0, MMUAddrWidth bits))
  val queue_elems_base = RegInit(U(0, MMUAddrWidth bits))
  val queue_bottom = RegInit(U(0, MMUAddrWidth bits))
  val queue_ageTop = RegInit(U(0, MMUAddrWidth bits))

  // State Machine
  // 0: Idle 1: Read 2: Work 3: End
  object overall_state extends SpinalEnum {
    val s_idle, s_read, s_work, s_end = newElement()
  }
  object read_state extends SpinalEnum {
    val s_read_bottom, s_read_top, s_done = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  val read_sub_state = RegInit(read_state.s_read_bottom)

  // initial
  io.LocalMMUIO.Request.payload.RequestData := U(0)
  io.LocalMMUIO.Request.payload.RequestSourceID := U(0)
  io.LocalMMUIO.Request.payload.RequestType_isWrite := False
  io.LocalMMUIO.Request.payload.RequestVirtualAddr.clearAll()
  io.LocalMMUIO.Response.ready := False
  io.ConfigIO.TaskEndValid := False
  io.ConfigIO.TaskReady := False

  when(state === overall_state.s_idle){
    io.ConfigIO.TaskReady := True
    when(io.ConfigIO.TaskValid && io.ConfigIO.TaskReady){
      state := overall_state.s_read
      queue_bottom_addr := io.ConfigIO.TaskQueue_BottomAddr
      queue_ageTop_addr := io.ConfigIO.TaskQueue_AgeTopAddr
      queue_elems_base := io.ConfigIO.TaskQueue_ElemsBase

      if(DebugEnable){
        report(Seq(
          "[GCTaskStack<", io.DebugInfo.DebugTimeStampe,
          ">]Config JVM HotSpot Queue, Bottom Addr is ", io.ConfigIO.TaskQueue_BottomAddr,
          ", AgeTop Addr is ", io.ConfigIO.TaskQueue_AgeTopAddr,
          ", Elems Base is", io.ConfigIO.TaskQueue_ElemsBase,
          "\n"
        ))
      }
    }
  }

  when(state === overall_state.s_read){
    switch(read_sub_state){
      is(read_state.s_read_bottom){
        io.LocalMMUIO.Request.valid := True
        io.LocalMMUIO.Request.payload.RequestVirtualAddr := queue_bottom_addr
        io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.payload
        io.LocalMMUIO.Request.payload.RequestType_isWrite := False
        io.LocalMMUIO.Response.ready := True

        when(io.LocalMMUIO.Response.fire){
          queue_bottom := io.LocalMMUIO.Response.payload.ReseponseData
          read_sub_state := read_state.s_read_top
          io.LocalMMUIO.Request.valid := False
        }
      }
      is(read_state.s_read_top) {
        io.LocalMMUIO.Request.valid := True
        io.LocalMMUIO.Request.payload.RequestVirtualAddr := queue_ageTop_addr
        io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.payload
        io.LocalMMUIO.Request.payload.RequestType_isWrite := False
        io.LocalMMUIO.Response.ready := True

        when(io.LocalMMUIO.Response.fire){
          queue_ageTop := io.LocalMMUIO.Response.payload.ReseponseData
          read_sub_state := read_state.s_done
          io.LocalMMUIO.Request.valid := False
        }
      }
      is(read_state.s_done){
        read_sub_state := read_state.s_read_bottom
        state := overall_state.s_work

        if(DebugEnable){
          report(Seq(
            "[GCTaskStack<", io.DebugInfo.DebugTimeStampe,
            ">]Getched JVM HotSpot Queue, Bottom is ", queue_bottom,
            ", AgeTop is ", queue_ageTop,
            "\n"
          ))
        }
      }
    }
  }

  io.Push.ready := state === overall_state.s_work && !(((stack_top > stack_bottom) && ((stack_top - stack_bottom) === (GCTaskStack_Entry - U(1)))) || ((stack_bottom > stack_top) && ((stack_bottom - stack_top) === U(1))))
  io.Pop.valid := stack_bottom =/= stack_top
  io.Pop.payload := data(stack_top)
  io.PreFetch.valid := stack_bottom =/= stack_top
  io.PreFetch.payload := data(stack_top)

  val dispatch_done = state === overall_state.s_work && stack_top === stack_bottom && queue_bottom === queue_ageTop

  when(state === overall_state.s_work){
    // push to stack or pop from stack
    when(io.Push.fire){
      when(stack_bottom + U(1) === GCTaskStack_Entry){
        stack_bottom := U(0)
      }.otherwise{
        stack_bottom := stack_bottom + U(1)
      }
    }.elsewhen(io.Pop.fire){
      when(stack_top === U(0)) {
        stack_top := GCTaskStack_Entry - U(1)
      }.otherwise{
        stack_top := stack_top - U(1)
      }
    }
    // spill or readBack
  }
}
