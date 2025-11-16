package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Stage0Data() extends Bundle with GCParameters with HWParameters {
  val src = UInt(MMUAddrWidth bits)
  val dest = UInt(MMUAddrWidth bits)
}

case class Stage1Data() extends Bundle with GCParameters with HWParameters {
  val src = UInt(MMUAddrWidth bits)
  val dest = UInt(MMUAddrWidth bits)
  val obj = UInt(MMUAddrWidth bits)
}

case class Stage2Data() extends Bundle with GCParameters with HWParameters {
  val src = UInt(MMUAddrWidth bits)
  val dest = UInt(MMUAddrWidth bits)
  val obj = UInt(MMUAddrWidth bits)
  val regionAttr = UInt(16 bits)
}

case class Stage6Data() extends Bundle with GCParameters with HWParameters {
  val src = UInt(MMUAddrWidth bits)
  val dest = UInt(MMUAddrWidth bits)
  val obj = UInt(MMUAddrWidth bits)
  val regionAttr = UInt(16 bits)
  val cardIndex = UInt(MMUAddrWidth bits)
}

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
    val s_0, s_1, s_2, s_3, s_4 = newElement()
  }

  val state = RegInit(overall_state.s_idle)

  val RegionAttrBase = RegInit(U(0, MMUAddrWidth bits))
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
      RegionAttrBase := io.Parse2Trace.RegionAttrBase
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
            ", RegionAttrBase = ", io.Parse2Trace.RegionAttrBase,
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
        io.Trace2Stack.valid := False
        for_counter := for_counter + U(1)
      }
    }
  }

  val p = RegInit(U(0, IntWidth bits))
  val q = RegInit(U(0, IntWidth bits))
  val prev_state = RegInit(overall_state.s_idle)

  val traceTaskFifo = StreamFifo(
    dataType = Stage0Data(),
    depth = 8
  )

  val doOopWork = new Area{
    //The parameters such as pssptr are the same in a batch

    val s0Valid = RegInit(False)
    val s1Valid = RegInit(False)
    val s2Valid = RegInit(False)
    val s3Valid = RegInit(False)
    val s4Valid = RegInit(False)
    val s5Valid = RegInit(False)
    val s6Valid = RegInit(False)
    val s7Valid = RegInit(False)
    val s8Valid = RegInit(False)
    val s9Valid = RegInit(False)

    val s0Reg = Reg(Stage0Data())
    val s1Reg = Reg(Stage1Data())
    val s2Reg = Reg(Stage2Data())
    val s3Reg = Reg(Stage2Data())
    val s4Reg = Reg(Stage2Data())
    val s5Reg = Reg(Stage2Data())
    val s6Reg = Reg(Stage6Data())
    val s7Reg = Reg(Stage6Data())
    val s9Reg = Reg(Stage6Data())

    traceTaskFifo.io.pop.ready := !s0Valid
    when(traceTaskFifo.io.pop.fire){
      s0Reg := traceTaskFifo.io.pop.payload
      s0Valid := True
    }

    when(s0Valid && !s1Valid){
      io.LocalMMUIO.Request.valid := True
      io.LocalMMUIO.Request.payload.RequestVirtualAddr := s0Reg.src
      io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.payload
      io.LocalMMUIO.Request.payload.RequestType_isWrite := False
      io.LocalMMUIO.Response.ready := True
      when(io.LocalMMUIO.Response.fire){
        io.LocalMMUIO.Request.valid := False
        s0Valid := False
        when(io.LocalMMUIO.Response.payload.ResponseData =/= 0){
          s1Valid := True
          s1Reg.src := s0Reg.src
          s1Reg.dest := s0Reg.dest
          s1Reg.obj := io.LocalMMUIO.Response.ResponseData
        }
      }
    }

    when(s1Valid && !s2Valid){
      io.LocalMMUIO.Request.valid := True
      io.LocalMMUIO.Request.payload.RequestVirtualAddr := RegionAttrBiasedBase + (s1Reg.obj >> RegionAttrShiftBy) * GCHeapRegionAttr_Size
      io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.payload
      io.LocalMMUIO.Request.payload.RequestType_isWrite := False
      io.LocalMMUIO.Response.ready := True
      when(io.LocalMMUIO.Response.fire){
        io.LocalMMUIO.Request.valid := False
        s1Valid := False
        when(!(io.LocalMMUIO.Response.payload.ResponseData(15 downto 8).asSInt < Type_Young && (s1Reg.dest ^ s1Reg.obj) >> LogOfHRGrainBytes === 0)){
          s2Valid := True
          s2Reg.src := s1Reg.src
          s2Reg.dest := s1Reg.dest
          s2Reg.obj := s1Reg.obj
          s2Reg.regionAttr := io.LocalMMUIO.Response.ResponseData(15 downto 0)
        }
      }
    }

    when(s2Valid && !s3Valid){
      when(s2Reg.regionAttr(15 downto 8).asSInt >= Type_Young){
        io.Trace2Stack.valid := True
        io.Trace2Stack.payload := s2Reg.dest
        when(io.Trace2Stack.fire){
          io.Trace2Stack.valid := False
          s2Valid := False
        }
      }.otherwise{
        s3Valid := True
        s3Reg := s2Reg
      }
    }

    val s3_subState = RegInit(sub_state.s_0)
    when(s3Valid && !s4Valid){
      when(s3Reg.regionAttr(15 downto 8).asSInt === Type_Humongous){
        switch(s3_subState){
          is(sub_state.s_0){
            io.LocalMMUIO.Request.valid := True
            io.LocalMMUIO.Request.payload.RequestVirtualAddr := HumongousReclaimCandidatesBoolBase + ((s3Reg.obj - (HeapRegionBias << HeapRegionShiftBy)) >> LogOfHRGrainBytes)
            io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.payload
            io.LocalMMUIO.Request.payload.RequestType_isWrite := False
            io.LocalMMUIO.Response.ready := True
            when(io.LocalMMUIO.Response.fire) {
              io.LocalMMUIO.Request.valid := False
              when(io.LocalMMUIO.Response.payload.ResponseData(0) === U(0)){
                s4Valid := True
                s4Reg := s3Reg
                s3Valid := False
              }.otherwise{
                s3_subState := sub_state.s_1
              }
            }
          }
          is(sub_state.s_1){
            io.LocalMMUIO.Request.valid := True
            io.LocalMMUIO.Request.payload.RequestVirtualAddr := HumongousReclaimCandidatesBoolBase + ((s3Reg.obj - (HeapRegionBias << HeapRegionShiftBy)) >> LogOfHRGrainBytes)
            io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.payload
            io.LocalMMUIO.Request.payload.RequestType_isWrite := True
            io.LocalMMUIO.Request.payload.RequestData := 0 // 只写1字节
            io.LocalMMUIO.Response.ready := True
            when(io.LocalMMUIO.Response.fire) {
              io.LocalMMUIO.Request.valid := False
              s3_subState := sub_state.s_2
            }
          }
          is(sub_state.s_2){
            io.LocalMMUIO.Request.valid := True
            io.LocalMMUIO.Request.payload.RequestVirtualAddr := RegionAttrBase + ((s3Reg.obj - (HeapRegionBias << HeapRegionShiftBy)) >> LogOfHRGrainBytes) * GCHeapRegionAttr_Size
            io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.payload
            io.LocalMMUIO.Request.payload.RequestType_isWrite := True
            io.LocalMMUIO.Request.payload.RequestData := U(Type_NoInCset) // 只写1字节
            io.LocalMMUIO.Response.ready := True
            when(io.LocalMMUIO.Response.fire) {
              io.LocalMMUIO.Request.valid := False
              s3_subState := sub_state.s_0
              s4Valid := True
              s4Reg := s3Reg
              s3Valid := False
            }
          }
        }
      }.otherwise{
        s4Valid := True
        s4Reg := s3Reg
        s3Valid := False
      }
    }
  }

  val arrayTrace_subState = RegInit(sub_state.s_0)
  when(state === overall_state.s_arrayTrace){
    prev_state := overall_state.s_arrayTrace
    switch(arrayTrace_subState){
      is(sub_state.s_0){
        //assign p, q and prev_state
        val low = DestOopPtr + ArrayElementOff + PartialArrayStart * GCObjectPtr_Size
        val high = DestOopPtr + ArrayElementOff + StepIndex * GCObjectPtr_Size
        p := Mux(DestOopPtr + ArrayElementOff < low, low, DestOopPtr + ArrayElementOff)
        q := Mux(DestOopPtr + ArrayElementOff + ArrayLength * GCObjectPtr_Size > high, high, DestOopPtr + ArrayElementOff + ArrayLength * GCObjectPtr_Size)

        arrayTrace_subState := sub_state.s_1
      }
      is(sub_state.s_1){
        when(p < q){
          traceTaskFifo.io.push.valid := True
          traceTaskFifo.io.push.payload.src := p - DestOopPtr + SrcOopPtr
          traceTaskFifo.io.push.payload.dest := p
          when(traceTaskFifo.io.push.fire){
            traceTaskFifo.io.push.valid := False
            p := p + GCObjectPtr_Size
          }
        }.otherwise{
          when(traceTaskFifo.io.occupancy === 0 && doOopWork.done){
            arrayTrace_subState := sub_state.s_0
            state := overall_state.s_end
          }
        }
      }
    }
  }

  val refTrace_subState = RegInit(sub_state.s_0)
  when(state === overall_state.s_refKlassTrace){
    prev_state := overall_state.s_refKlassTrace
    switch(refTrace_subState){
      is(sub_state.s_0){
        // do_oop_work(DISCOVERED)
        p := DestOopPtr + DISCOVERED_OFFSET
        state := overall_state.s_doOopWork
        refTrace_subState := sub_state.s_1
      }
      is(sub_state.s_1){
        // do_oop_work(DISCOVERED)
        p := DestOopPtr + REFERENT_OFFSET
        state := overall_state.s_doOopWork
        refTrace_subState := sub_state.s_2
      }
      is(sub_state.s_2){
        // do_oop_work(DISCOVERED)
        p := DestOopPtr + DISCOVERED_OFFSET
        state := overall_state.s_doOopWork
        refTrace_subState := sub_state.s_3
      }
      is(sub_state.s_3){
        state := overall_state.s_oopTrace
        refTrace_subState := sub_state.s_0
      }
    }
  }

  val staticTrace_subState = RegInit(sub_state.s_0)
  when(state === overall_state.s_staticTrace){
    prev_state := overall_state.s_staticTrace
    switch(staticTrace_subState){
      is(sub_state.s_0){
        //access (SrcOopPtr + staticOopFieldOff)
        io.LocalMMUIO.Request.valid := True
        io.LocalMMUIO.Request.payload.RequestVirtualAddr := SrcOopPtr + staticOopFieldCountOff
        io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.payload
        io.LocalMMUIO.Request.payload.RequestType_isWrite := False
        io.LocalMMUIO.Response.ready := True
        when(io.LocalMMUIO.Response.fire){
          p := SrcOopPtr + StaticFieldOff
          q := SrcOopPtr + StaticFieldOff + io.LocalMMUIO.Response.payload.ResponseData(31 downto 0) * GCObjectPtr_Size
          staticTrace_subState := sub_state.s_1
          io.LocalMMUIO.Request.valid := False
        }
      }
      is(sub_state.s_1){
        when(p < q){
          state := overall_state.s_doOopWork
        }.otherwise{
          state := overall_state.s_oopTrace
          staticTrace_subState := sub_state.s_0
        }
      }
    }
  }

  val oopTrace_subState = RegInit(sub_state.s_0)
  val vtable_len = RegInit(U(0, IntWidth bits))
  val itable_len = RegInit(U(0, IntWidth bits))
  val nonstaticOopMapSize = RegInit(U(0, IntWidth bits))

  val start_map = RegInit(U(0, MMUAddrWidth bits))
  val end_map = RegInit(U(0, MMUAddrWidth bits))
  when(state === overall_state.s_oopTrace){
    prev_state := overall_state.s_oopTrace
    switch(oopTrace_subState){
      is(sub_state.s_0){
        // access (KlassPtr + VTableLenOff) to get the vtableLen
        io.LocalMMUIO.Request.valid := True
        io.LocalMMUIO.Request.payload.RequestVirtualAddr := KlassPtr + VTableLenOff
        io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.payload
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
        io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.payload
        io.LocalMMUIO.Request.payload.RequestType_isWrite := False
        io.LocalMMUIO.Response.ready := True
        when(io.LocalMMUIO.Response.fire){
          nonstaticOopMapSize := io.LocalMMUIO.Response.payload.ResponseData(31 downto 0)
          itable_len := io.LocalMMUIO.Response.payload.ResponseData(63 downto 32)
          oopTrace_subState := sub_state.s_2
          io.LocalMMUIO.Request.valid := False
        }
      }
      is(sub_state.s_2){
        io.LocalMMUIO.Request.valid := True
        io.LocalMMUIO.Request.payload.RequestVirtualAddr := KlassPtr + VTableOff
        io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.payload
        io.LocalMMUIO.Request.payload.RequestType_isWrite := False
        io.LocalMMUIO.Response.ready := True
        when(io.LocalMMUIO.Response.fire){
          start_map := io.LocalMMUIO.Response.payload.ResponseData + vtable_len + itable_len
          end_map := io.LocalMMUIO.Response.payload.ResponseData + vtable_len + itable_len + nonstaticOopMapSize * GCObjectPtr_Size
          oopTrace_subState := sub_state.s_3
          io.LocalMMUIO.Request.valid := False
        }
      }
      is(sub_state.s_3){
        when(start_map < end_map){
          io.LocalMMUIO.Request.valid := True
          io.LocalMMUIO.Request.payload.RequestVirtualAddr := end_map - GCObjectPtr_Size
          io.LocalMMUIO.Request.payload.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.payload
          io.LocalMMUIO.Request.payload.RequestType_isWrite := False
          io.LocalMMUIO.Response.ready := True
          when(io.LocalMMUIO.Response.fire){
            p := DestOopPtr + io.LocalMMUIO.Response.payload.ResponseData(31 downto 0)
            q := DestOopPtr + io.LocalMMUIO.Response.payload.ResponseData(31 downto 0) + io.LocalMMUIO.Response.payload.ResponseData(63 downto 32) * GCObjectPtr_Size - GCObjectPtr_Size
            oopTrace_subState := sub_state.s_4
            io.LocalMMUIO.Request.valid := False
          }
        }.otherwise{
          oopTrace_subState := sub_state.s_0
          state := overall_state.s_end
        }

      }
      is(sub_state.s_4) {
        when(p < q + GCObjectPtr_Size) {
          state := overall_state.s_doOopWork
        }.otherwise {
          oopTrace_subState := sub_state.s_3
          end_map := end_map - GCObjectPtr_Size
        }
      }
    }
  }

  when(state === overall_state.s_end){
    state := overall_state.s_idle
  }

}