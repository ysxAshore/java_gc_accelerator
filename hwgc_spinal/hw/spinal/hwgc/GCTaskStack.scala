package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/* GCTaskStack
 * @description: As a hardware buffer for the task queue in the JVM Hotspot
 * in HWGC pop and push operate on stack_top
 */
class GCTaskStack extends Module with GCParameters with HWParameters {
  val io = new Bundle {
    val Push = slave Stream UInt(MMUAddrWidth bits)
    val Pop = master Stream UInt(MMUAddrWidth bits)
    val LocalMMUIO = master(new LocalMMUIO)
    val ConfigIO = slave(new GCTaskStackConfigIO)
    val DebugInfo = in(new DebugInfoIO)
  }

  // TaskStack config
  val stack_mask = U(GCTaskStack_Entry) - U(1)
  val stack_data = RegInit(Vec.fill(GCTaskStack_Entry)(U(0, MMUAddrWidth bits)))
  val stack_top = RegInit(U(0, log2Up(GCTaskStack_Entry) bits))
  val stack_bottom = RegInit(U(0, log2Up(GCTaskStack_Entry) bits))

  // TaskQueue
  val queue_mask = U(GCTaskQueue_Size) - U(1)
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
  object sub_state extends SpinalEnum {
    val s0, s1 = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  val readQueue_sub_state = RegInit(sub_state.s0)
  val spillOut_sub_state = RegInit(sub_state.s0)
  val readBack_sub_state = RegInit(sub_state.s0)

  // default value
  io.LocalMMUIO.Request.valid := False
  io.LocalMMUIO.Request.payload.clearAll()
  io.LocalMMUIO.Response.ready := False
  io.ConfigIO.TaskEndValid := False
  io.ConfigIO.TaskReady := False

  val issued_readBottom = RegInit(False)
  val issued_readAgeTop = RegInit(False)
  val issued_spill_elems = RegInit(False)
  val issued_spill_bottom = RegInit(False)
  val issued_readback_elems = RegInit(False)
  val issued_readback_bottom = RegInit(False)

  val dispatch_done = stack_top === stack_bottom && queue_bottom === queue_ageTop
  val task_capacity = (stack_top - stack_bottom) & stack_mask

  val need_spillOut = task_capacity >= U(GCTaskStack_SpillNeed)
  val need_readback = task_capacity <= U(GCTaskStack_ReadNeed)

  io.Push.ready := state === overall_state.s_work && task_capacity < stack_mask
  io.Pop.valid := stack_bottom =/= stack_top
  io.Pop.payload := stack_data(stack_top)

  when(state === overall_state.s_idle){
    //reset issued
    issued_readBottom := False
    issued_readAgeTop := False
    issued_spill_elems := False
    issued_spill_bottom := False
    issued_readback_bottom := False
    issued_readback_elems := False

    stack_top := U(0)
    stack_bottom := U(0)

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
          ", Elems Base is", io.ConfigIO.TaskQueue_ElemsBase, "\n"
        ))
      }
    }
  }

  when(state === overall_state.s_read){
    switch(readQueue_sub_state){
      is(sub_state.s0){
        issueReq(io.LocalMMUIO, queue_bottom_addr, False, U(0), U(0), issued_readBottom) { rd =>
          queue_bottom := io.LocalMMUIO.Response.payload.ResponseData
          readQueue_sub_state := sub_state.s1
        }
      }
      is(sub_state.s1) {
        issueReq(io.LocalMMUIO, queue_ageTop_addr, False, U(0), U(0), issued_readAgeTop) { rd =>
          queue_ageTop := io.LocalMMUIO.Response.payload.ResponseData
          readQueue_sub_state := sub_state.s0
          state := overall_state.s_work
          if(DebugEnable){
            report(Seq(
              "[GCTaskStack<", io.DebugInfo.DebugTimeStampe,
              ">]Getched JVM HotSpot Queue, Bottom is ", queue_bottom,
              ", AgeTop is ", io.LocalMMUIO.Response.payload.ResponseData,
              "\n"
            ))
          }
        }
      }
    }
  }


  when(state === overall_state.s_work){
    // push to stack or pop from stack --- operate on stack_top
    // push > pop
    when(dispatch_done){
      state := overall_state.s_end
    }.otherwise{
       when(io.Push.fire){
         stack_top := (stack_top + U(1)) & stack_mask
         stack_data((stack_top + U(1)) & stack_mask) := io.Push.payload
       }.elsewhen(io.Pop.fire){
         stack_top := (stack_top - U(1)) & stack_mask
       }

       // spill or readBack from stack --- operate on stack_bottom
       // spill > readBack
       when(need_spillOut){
         switch(spillOut_sub_state){
           is(sub_state.s0){
             issueReq(io.LocalMMUIO, queue_elems_base + queue_bottom * GCScannerTask_Size, True, U(63), stack_data((stack_bottom + U(1)) & (GCTaskStack_Entry - U(1))), issued_spill_elems){ rd =>
               spillOut_sub_state := sub_state.s1
             }
           }
           is(sub_state.s1){
             issueReq(io.LocalMMUIO, queue_bottom_addr, True, U(63), (queue_bottom + U(1)) & (GCTaskQueue_Size - U(1)), issued_spill_bottom) { rd =>
               queue_bottom := (queue_bottom + U(1)) & (GCTaskQueue_Size - U(1))
               stack_bottom := (stack_bottom + U(1)) & (GCTaskStack_Entry - U(1))
               spillOut_sub_state := sub_state.s0
             }
           }
         }
       }.elsewhen(need_readback){
         switch(readBack_sub_state){
           is(sub_state.s0){
             issueReq(io.LocalMMUIO, queue_elems_base + ((queue_bottom - U(1)) & (GCTaskQueue_Size - U(1))) * GCScannerTask_Size, False, U(0), U(0), issued_readback_elems) { rd =>
               when(!(io.Push.fire && task_capacity === GCTaskStack_Entry - U(2))){
                 stack_data(stack_bottom) := io.LocalMMUIO.Response.payload.ResponseData
               }
               readBack_sub_state := sub_state.s1
             }
           }
           is(sub_state.s1){
             issueReq(io.LocalMMUIO, queue_bottom_addr, True, U(63), (queue_bottom - U(1)) & (GCTaskQueue_Size - U(1)), issued_readback_bottom){ rd =>
               queue_bottom := (queue_bottom - U(1)) & (GCTaskQueue_Size - U(1))
               stack_bottom := (stack_bottom + U(1)) & (GCTaskStack_Entry - U(1))
               readBack_sub_state := sub_state.s0
             }
           }
         }
       }
    }
  }

  when(state === overall_state.s_end){
    state := overall_state.s_idle
    io.ConfigIO.TaskEndValid := True
  }
}
