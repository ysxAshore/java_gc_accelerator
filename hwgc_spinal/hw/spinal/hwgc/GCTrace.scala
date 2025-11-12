package hwgc

import spinal.core._
import spinal.lib._
import scala.language.postfixOps

class GCTrace extends Module with GCParameters with HWParameters{
  val io = new Bundle {
    val LocalMMUIO = master(new LocalMMUIO)
    val DebugInfo = in(new DebugInfoIO)
    val Parse2Trace = slave(new GCParse2Trace)
    val Trace2Stack = master Stream UInt(MMUAddrWidth bits)
  }

  // State Machine
  // 0: Idle 1: Read 2: Work 3: End
  object overall_state extends SpinalEnum {
    val s_idle, s_arrayPush, s_arrayTrace, s_refKlassTrace, s_staticTrace, s_oopTrace, s_doOopWork, s_end = newElement()
  }
  object sub_state extends SpinalEnum{
    val s_0, s_1, s_2 = newElement()
  }

  val state = RegInit(overall_state.s_idle)

  val RegionAttrBiasedBase = RegInit(U(0, MMUAddrWidth bits))
  val RegionAttrShiftBy = RegInit(U(0, IntWidth bits))
  val HeapRegionBias = RegInit(U(0, IntWidth bits))
  val HeapRegionShiftBy = RegInit(U(0, IntWidth bits))
  val HumongousReclaimCandidatesBoolBase = RegInit(U(0, MMUAddrWidth bits))
  val ParScanThreadStatePtr = RegInit(U(0, MMUAddrWidth bits))
  val KlassPtr = RegInit(U(0, MMUAddrWidth bits))
  val SrcOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val DestOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val Kid = RegInit(U(0, IntWidth bits))
  val ArrayLength = RegInit(U(0, IntWidth bits))
  val PartialArrayStart = RegInit(U(0, IntWidth bits))
  val StepIndex = RegInit(U(0, IntWidth bits))
  val StepNCreate = RegInit(U(0, IntWidth bits))

  when(state === overall_state.s_idle){
    io.Parse2Trace.Ready := True
    when(io.Parse2Trace.Valid && io.Parse2Trace.Ready){
      RegionAttrBiasedBase:= io.Parse2Trace.RegionAttrBiasedBase
      RegionAttrShiftBy := io.Parse2Trace.RegionAttrShiftBy
      HeapRegionBias := io.Parse2Trace.HeapRegionBias
      HeapRegionShiftBy := io.Parse2Trace.HeapRegionShiftBy
      HumongousReclaimCandidatesBoolBase := io.Parse2Trace.HumongousReclaimCandidatesBoolBase
      ParScanThreadStatePtr := io.Parse2Trace.ParScanThreadStatePtr
      KlassPtr := io.Parse2Trace.KlassPtr
      SrcOopPtr := io.Parse2Trace.SrcOopPtr
      DestOopPtr := io.Parse2Trace.DestOopPtr
      Kid := io.Parse2Trace.Kid
      ArrayLength := io.Parse2Trace.ArrayLength
      PartialArrayStart := Mux(io.Parse2Trace.OopType === PartialArrayOop, io.Parse2Trace.PartialArrayStart, U(0))
      StepIndex := io.Parse2Trace.StepIndex
      StepNCreate := io.Parse2Trace.StepNCreate

      when(io.Parse2Trace.OopType === PartialArrayOop || io.Parse2Trace.Kid === ObjectArrayKlassID){
        state := overall_state.s_arrayPush
      }.elsewhen(io.Parse2Trace.Kid === InstanceRefKlassID){
        state := overall_state.s_refKlassTrace
      }.elsewhen(io.Parse2Trace.Kid === InstanceMirrorKlassID){
        state := overall_state.s_staticTrace
      }.otherwise{
        state := overall_state.s_oopTrace
      }

      if(DebugEnable){
        if(DebugEnable){
          report(Seq(
            "[GCTrace<", io.DebugInfo.DebugTimeStampe,
            ">] GCParse to GCTrace",
            ", RegionAttrBiasedBase = ", io.Parse2Trace.RegionAttrBiasedBase,
            ", RegionAttrShiftBy = ", io.Parse2Trace.RegionAttrShiftBy,
            ", HeapRegionBias = ", io.Parse2Trace.HeapRegionBias,
            ", HeapRegionShiftBy = ", io.Parse2Trace.HeapRegionShiftBy,
            ", HumongousReclaimCandidatesBoolBase = ", io.Parse2Trace.HumongousReclaimCandidatesBoolBase,
            ", ParScanThreadStatePtr = ", io.Parse2Trace.ParScanThreadStatePtr,
            ", OopType = ", io.Parse2Trace.OopType,
            ", KlassPtr = ", io.Parse2Trace.KlassPtr,
            ", SrcOopPtr = ", io.Parse2Trace.SrcOopPtr,
            ", DestOopPtr = ", io.Parse2Trace.DestOopPtr,
            ", Kid = ", io.Parse2Trace.Kid, "\n",
            ", ArrayLength = ", io.Parse2Trace.ArrayLength,
            ", PartialArrayStart = ", io.Parse2Trace.PartialArrayStart,
            ", StepIndex = ", io.Parse2Trace.StepIndex,
            ", StepNCreate = ", io.Parse2Trace.StepNCreate, "\n"
          ))
        }
      }
    }
  }

  val for_counter = RegInit(U(0, IntWidth bits))
  when(state === overall_state.s_arrayPush){
    when(for_counter === StepNCreate){
      state := overall_state.s_arrayTrace
    }.otherwise{
      io.Trace2Stack.valid := True
      io.Trace2Stack.payload := SrcOopPtr + PartialArrayTag
      when(io.Trace2Stack.fire){
        for_counter := for_counter + U(1)
      }
    }
  }

  val p = RegInit(U(0, IntWidth bits))
  val q = RegInit(U(0, IntWidth bits))
  val prev_state = RegInit(overall_state.s_idle)

  val arrayTrace_subState = RegInit(sub_state.s_0)
  when(state === overall_state.s_arrayTrace){
    switch(arrayTrace_subState){
      is(sub_state.s_0){
        //assign p, q and prev_state
        prev_state := overall_state.s_arrayTrace
        val low = DestOopPtr + ArrayElementOff + PartialArrayStart * GCObjectPtr_Size
        val high = DestOopPtr + ArrayElementOff + StepIndex * GCObjectPtr_Size
        p := Mux(DestOopPtr + ArrayElementOff < low, low, DestOopPtr + ArrayElementOff)
        q := Mux(DestOopPtr + ArrayElementOff + ArrayLength * GCObjectPtr_Size > high, high, DestOopPtr + ArrayElementOff + ArrayLength * GCObjectPtr_Size)

        arrayTrace_subState := sub_state.s_1
      }
      is(sub_state.s_1){
        when(p < q){
          state := overall_state.s_doOopWork
        }.otherwise{
          arrayTrace_subState := sub_state.s_0
          state := overall_state.s_end
        }
      }
    }
  }

  when(state === overall_state.s_refKlassTrace){

  }

  when(state === overall_state.s_staticTrace){

  }

  val oopTrace_subState = RegInit(sub_state.s_0)
  val vtable_len = RegInit(U(0, IntWidth bits))
  val itable_len = RegInit(U(0, IntWidth bits))
  val nonstaticOopMapSize = RegInit(U(0, IntWidth bits))
  when(state === overall_state.s_oopTrace){
    switch(oopTrace_subState){
      is(sub_state.s_0){
        // access (KlassPtr + VTableLenOff) to get the vtableLen
        io.LocalMMUIO.Request.valid := True
        io.LocalMMUIO.Request.payload.RequestVirtualAddr := KlassPtr + VTableLenOff
        io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID
        io.LocalMMUIO.Request.payload.RequestType_isWrite := False
        io.LocalMMUIO.Response.ready := True
        when(io.LocalMMUIO.Response.fire){
          vtable_len := io.LocalMMUIO.Response.payload.ResponseData(31 downto 0)
          oopTrace_subState := sub_state.s_1
          io.LocalMMUIO.Request.valid := False
        }
      }
      is(sub_state.s_1){
        // access (KlassPtr + NonStaticOopMapSizeOff) to get the itableLen and nonStaticOopMapSize
        io.LocalMMUIO.Request.valid := True
        io.LocalMMUIO.Request.payload.RequestVirtualAddr := KlassPtr + ITableLenOff
        io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID
        io.LocalMMUIO.Request.payload.RequestType_isWrite := False
        io.LocalMMUIO.Response.ready := True
        when(io.LocalMMUIO.Response.fire){
          nonstaticOopMapSize := io.LocalMMUIO.Response.payload.ResponseData(31 downto 0)
          itable_len := io.LocalMMUIO.Response.payload.ResponseData(63 downto 32)
          oopTrace_subState := sub_state.s_2
          io.LocalMMUIO.Request.valid := False
        }
      }
    }
  }

  when(state === overall_state.s_doOopWork){

    when(prev_state === overall_state.s_arrayTrace){
      p := p + GCObjectPtr_Size
    }
  }

  when(state === overall_state.s_end){
    state := overall_state.s_idle
  }
}
