package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class TaskData() extends Bundle with GCParameters with HWParameters{
  val src = UInt(MMUAddrWidth bits)
  val dest = UInt(MMUAddrWidth bits)
}

case class StageData() extends Bundle with GCParameters with HWParameters {
  val src = UInt(MMUAddrWidth bits)
  val dest = UInt(MMUAddrWidth bits)
  val obj = UInt(MMUAddrWidth bits)
  val regionAttr = UInt(16 bits)
}

class GCTrace extends Module with GCParameters with HWParameters{
  val io = new Bundle {
    val TraceMMUIO = master(new TraceMReq2MMU(GCoopWorkStages, 4))
    val DebugInfo = in(new DebugInfoIO)
    val ToTrace = slave(new GCProcess2Trace)
    val Trace2Stack = master Stream UInt(MMUAddrWidth bits)
    val Trace2Aop = master(new AopParameters)
  }

  // default value
  for(i <- 0 until GCoopWorkStages){
    io.TraceMMUIO.oopWorkMReqs(i).Request.valid := False
    io.TraceMMUIO.oopWorkMReqs(i).Request.payload.clearAll()
    io.TraceMMUIO.oopWorkMReqs(i).Response.ready := False
  }
  for(i <- 0 until 4){
    io.TraceMMUIO.oopTraceMReqs(i).Request.valid := False
    io.TraceMMUIO.oopTraceMReqs(i).Request.payload.clearAll()
    io.TraceMMUIO.oopTraceMReqs(i).Response.ready := False
  }
  io.TraceMMUIO.staticMReq.Request.valid := False
  io.TraceMMUIO.staticMReq.Request.payload.clearAll()
  io.TraceMMUIO.staticMReq.Response.ready := False

  io.ToTrace.Ready := False
  io.ToTrace.Done := False

  io.Trace2Stack.valid := False
  io.Trace2Stack.payload.clearAll()

  // State Machine
  object overall_state extends SpinalEnum {
    val s_idle, s_arrayPush, s_arrayTrace, s_refKlassTrace, s_staticTrace, s_oopTrace, s_end = newElement()
  }
  object sub_state extends SpinalEnum{
    val s_0, s_1, s_2, s_3, s_4 = newElement()
  }

  // helper: pushStack
  def pushFifo(src: UInt, dest: UInt, issued: Bool): Bool = {
    traceTaskFifo.io.push.valid := False
    when(!issued) {
      traceTaskFifo.io.push.valid := True
      traceTaskFifo.io.push.payload.src := src
      traceTaskFifo.io.push.payload.dest := dest
      issued := True
    }
    val pushed = traceTaskFifo.io.push.fire
    pushed
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
    io.ToTrace.Ready := True
    when(io.ToTrace.Valid && io.ToTrace.Ready){
      io.ToTrace.Done := False

      RegionAttrBase := io.ToTrace.RegionAttrBase
      RegionAttrBiasedBase:= io.ToTrace.RegionAttrBiasedBase
      RegionAttrShiftBy := io.ToTrace.RegionAttrShiftBy
      HeapRegionBias := io.ToTrace.HeapRegionBias
      HeapRegionShiftBy := io.ToTrace.HeapRegionShiftBy
      HumongousReclaimCandidatesBoolBase := io.ToTrace.HumongousReclaimCandidatesBoolBase
      ParScanThreadStatePtr := io.ToTrace.ParScanThreadStatePtr
      KlassPtr := io.ToTrace.KlassPtr
      SrcOopPtr := io.ToTrace.SrcOopPtr
      DestOopPtr := io.ToTrace.DestOopPtr
      Kid := io.ToTrace.Kid
      ArrayLength := io.ToTrace.ArrayLength
      PartialArrayStart := Mux(io.ToTrace.OopType === PartialArrayOop, io.ToTrace.PartialArrayStart, U(0))
      StepIndex := io.ToTrace.StepIndex
      StepNCreate := io.ToTrace.StepNCreate

      when(io.ToTrace.OopType === PartialArrayOop || io.ToTrace.Kid === ObjectArrayKlassID){
        state := overall_state.s_arrayPush
      }.elsewhen(io.ToTrace.Kid === InstanceRefKlassID){
        state := overall_state.s_refKlassTrace
      }.elsewhen(io.ToTrace.Kid === InstanceMirrorKlassID){
        state := overall_state.s_staticTrace
      }.otherwise{
        state := overall_state.s_oopTrace
      }

      if(DebugEnable){
        if(DebugEnable){
          report(Seq(
            "[GCTrace<", io.DebugInfo.DebugTimeStampe,
            ">] GCParse to GCTrace",
            ", RegionAttrBase = ", io.ToTrace.RegionAttrBase,
            ", RegionAttrBiasedBase = ", io.ToTrace.RegionAttrBiasedBase,
            ", RegionAttrShiftBy = ", io.ToTrace.RegionAttrShiftBy,
            ", HeapRegionBias = ", io.ToTrace.HeapRegionBias,
            ", HeapRegionShiftBy = ", io.ToTrace.HeapRegionShiftBy,
            ", HumongousReclaimCandidatesBoolBase = ", io.ToTrace.HumongousReclaimCandidatesBoolBase,
            ", ParScanThreadStatePtr = ", io.ToTrace.ParScanThreadStatePtr,
            ", OopType = ", io.ToTrace.OopType,
            ", KlassPtr = ", io.ToTrace.KlassPtr,
            ", SrcOopPtr = ", io.ToTrace.SrcOopPtr,
            ", DestOopPtr = ", io.ToTrace.DestOopPtr,
            ", Kid = ", io.ToTrace.Kid, "\n",
            ", ArrayLength = ", io.ToTrace.ArrayLength,
            ", PartialArrayStart = ", io.ToTrace.PartialArrayStart,
            ", StepIndex = ", io.ToTrace.StepIndex,
            ", StepNCreate = ", io.ToTrace.StepNCreate, "\n"
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

  val traceTaskFifo = StreamFifo(
    dataType = TaskData(),
    depth = 8
  )
  traceTaskFifo.io.push.valid := False
  traceTaskFifo.io.push.clearAll()
  traceTaskFifo.io.pop.ready := False

  val doOopWork = new Area{
    // MMU req and resp
    val stages = Seq.tabulate(GCoopWorkStages){ _ =>
      new Area{
        val valid = RegInit(False)
        val reg = Reg(StageData().getZero)
        val reqIssued = RegInit(False)
        val reqDone = RegInit(False)
        val responseData = RegInit(U(0, MMUDataWidth bits))
      }
    }

    traceTaskFifo.io.pop.ready := !stages(0).valid
    when(traceTaskFifo.io.pop.fire){
      stages(0).reg.src := traceTaskFifo.io.pop.payload.src
      stages(0).reg.dest := traceTaskFifo.io.pop.payload.dest
      stages(0).valid := True
    }

    // helper to compute MMU op properties per stage index (pure combinational)
    // return (want, addr, isWrite, wmask, wdata)
    def mmuOpForIndex(i: Int, s: StageData): (Bool, UInt, Bool, UInt, UInt) = {
      i match {
        case 0 => (True, s.src, False, U(0), U(0))
        case 1 => (True, RegionAttrBiasedBase + (s.obj >> RegionAttrShiftBy) * GCHeapRegionAttr_Size, False, U(0), U(0))
        case 3 => (True, HumongousReclaimCandidatesBoolBase + ((s.obj - (HeapRegionBias << HeapRegionShiftBy)) >> LogOfHRGrainBytes), False, U(0), U(0))
        case 4 => (True, HumongousReclaimCandidatesBoolBase + ((s.obj - (HeapRegionBias << HeapRegionShiftBy)) >> LogOfHRGrainBytes), True, U(1), U(0))
        case 5 => (True, RegionAttrBase + ((s.obj - (HeapRegionBias << HeapRegionShiftBy)) >> LogOfHRGrainBytes) * GCHeapRegionAttr_Size + TypeOffSet, True, U(1), U(Type_NoInCset))
        case 6 => (True, ParScanThreadStatePtr + OBJCLOSURE_OFFSET + SCANNING_IN_YOUNG_OFFSET, False, U(0), U(0))
        case _ =>  (False, U(0), False, U(0), U(0))
      }
    }
    def resetStage(i: Int): Unit = {
      stages(i).valid := False
      stages(i).reqDone := False
    }
    def transferStage(i: Int , j: Int) : Unit = {
      resetStage(i)
      stages(j).valid := True
      stages(j).reg := stages(i).reg
    }

    // send and save response
    for(i <- 0 until GCoopWorkStages){
      val s = stages(i)
      val mreq = io.TraceMMUIO.oopWorkMReqs(i)
      val (want, addr, isWrite, wmask, wdata) = mmuOpForIndex(i, s.reg)
      when(s.valid && !s.reqDone && want) {
        issueReq(mreq, addr, isWrite, wmask, wdata, s.reqIssued) { rd =>
          s.reqDone := True
          s.responseData := rd
        }
      }
    }

    // reg handler logic
    for(i <- 0 until GCoopWorkStages){
      val s = stages(i)
      when(s.valid){
        i match {
          case 0 =>
            when(s.reqDone){
              when(s.responseData === U(0)){
                resetStage(0)
              }.elsewhen(!stages(1).valid){
                transferStage(0, 1)
                stages(1).reg.obj := s.responseData
              }
            }
          case 1 =>
            when(s.reqDone){
              val regionAttrType = s.responseData(15 downto 8)
              when(regionAttrType.asSInt >= Type_Young){
                when(!stages(2).valid){
                  transferStage(1, 2)
                  stages(2).reg.regionAttr := s.responseData(15 downto 0)
                }
              }.elsewhen((s.reg.dest ^ s.reg.obj) >> LogOfHRGrainBytes =/= 0){
                when(regionAttrType === U(Type_Humongous)){
                  when(!stages(3).valid){
                    transferStage(1, 3)
                    stages(3).reg.regionAttr := s.responseData(15 downto 0)
                  }
                }.elsewhen(!stages(6).valid){
                  transferStage(1, 6)
                  stages(6).reg.regionAttr := s.responseData(15 downto 0)
                }
              }.otherwise{
                resetStage(1)
              }
            }
          case 2 =>
            io.Trace2Stack.valid := True
            io.Trace2Stack.payload := s.reg.dest
            when(io.Trace2Stack.fire) {
              resetStage(2)
            }
          case 3 =>
            when(s.reqDone){
              when(s.responseData(0)){
                when(!stages(4).valid){
                  transferStage(3, 4)
                }
              }.otherwise{
                when(!stages(6).valid){
                  transferStage(3, 6)
                }
              }
            }
          case 6 =>
            when(s.reqDone){
              when(s.responseData(7 downto 0) === U(1) || s.reg.regionAttr(7 downto 0) === U(0)){
                resetStage(6)
              }.otherwise{
                when(!stages(7).valid) {
                  transferStage(6, 7)
                }
              }
            }
          case 7 =>
            io.Trace2Aop.Valid := True
            io.Trace2Aop.ParScanThreadStatePtr := ParScanThreadStatePtr
            io.Trace2Aop.DestOopPtr := s.reg.dest
            when(io.Trace2Aop.Valid && io.Trace2Aop.Ready){
              resetStage(7)
            }
          case _ =>
            when(s.reqDone){
              when(!stages(i+1).valid){
                transferStage(i, i+1)
              }
            }
        }
      }
    }

    val anyValid = stages.map(_.valid).reduce(_ || _)
    val done = traceTaskFifo.io.occupancy === U(0) && !anyValid && io.Trace2Aop.Done
  }

  val p = RegInit(U(0, IntWidth bits))
  val q = RegInit(U(0, IntWidth bits))
  val arrayTrace_subState = RegInit(sub_state.s_0)
  val issued_arrayPush = RegInit(False)
  when(state === overall_state.s_arrayTrace){
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
          when(pushFifo(p - DestOopPtr + SrcOopPtr, p, issued_arrayPush)){
            p := p + GCObjectPtr_Size
            issued_arrayPush := False
          }
        }.otherwise{
          when(doOopWork.done){
            arrayTrace_subState := sub_state.s_0
            state := overall_state.s_end
          }
        }
      }
    }
  }

  val refTrace_subState = RegInit(sub_state.s_0)
  val issued_refTrace = RegInit(False)
  when(state === overall_state.s_refKlassTrace){
    switch(refTrace_subState){
      is(sub_state.s_0){
        // do_oop_work(DISCOVERED)
        when(pushFifo(SrcOopPtr + DISCOVERED_OFFSET, DestOopPtr + DISCOVERED_OFFSET, issued_refTrace)){
          refTrace_subState := sub_state.s_1
          issued_refTrace := False
        }
      }
      is(sub_state.s_1){
        // do_oop_work(DISCOVERED)
        when(pushFifo(SrcOopPtr + REFERENT_OFFSET, DestOopPtr + REFERENT_OFFSET, issued_refTrace)){
          refTrace_subState := sub_state.s_2
          issued_refTrace := False
        }
      }
      is(sub_state.s_2){
        // do_oop_work(DISCOVERED)
        when(pushFifo(SrcOopPtr + DISCOVERED_OFFSET, DestOopPtr + DISCOVERED_OFFSET, issued_refTrace)){
          refTrace_subState := sub_state.s_3
          issued_refTrace := False
        }
      }
      is(sub_state.s_3){
        state := overall_state.s_oopTrace
        refTrace_subState := sub_state.s_0
      }
    }
  }

  val staticTrace_subState = RegInit(sub_state.s_0)
  val staticReqIssue = RegInit(False)
  val issued_staticTrace = RegInit(False)
  when(state === overall_state.s_staticTrace){
    switch(staticTrace_subState){
      is(sub_state.s_0){
        //access (SrcOopPtr + staticOopFieldOff)
        val addr = SrcOopPtr + staticOopFieldCountOff
        issueReq(io.TraceMMUIO.staticMReq, addr, False, U(0), U(0), staticReqIssue) {rd =>
          p := SrcOopPtr + StaticFieldOff
          q := SrcOopPtr + StaticFieldOff + rd(31 downto 0) * GCObjectPtr_Size
          staticTrace_subState := sub_state.s_1
        }
      }
      is(sub_state.s_1){
        when(p < q){
          when(pushFifo(p - DestOopPtr + SrcOopPtr, p, issued_staticTrace)){
            p := p + GCObjectPtr_Size
            issued_staticTrace := False
          }
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
  val oopReq = Vec(RegInit(False), 4)
  val issued_oopTrace = RegInit(False)
  when(state === overall_state.s_oopTrace){
    switch(oopTrace_subState){
      is(sub_state.s_0){
        // access (KlassPtr + VTableLenOff) to get the vtableLen
        val addr = KlassPtr + VTableLenOff
        issueReq(io.TraceMMUIO.oopTraceMReqs(0), addr, False, U(0), U(0), oopReq(0)) {rd =>
          vtable_len := rd(31 downto 0)
          oopTrace_subState := sub_state.s_1
        }
      }
      is(sub_state.s_1){
        // access (KlassPtr + NonStaticOopMapSizeOff) to get the itableLen and nonStaticOopMapSize
        val addr = KlassPtr + ITableLenOff
        issueReq(io.TraceMMUIO.oopTraceMReqs(1), addr, False, U(0), U(0), oopReq(1)) {rd =>
          nonstaticOopMapSize := rd(31 downto 0)
          itable_len := rd(63 downto 32)
          oopTrace_subState := sub_state.s_2
        }
      }
      is(sub_state.s_2){
        // access (KlassPtr + VTableOff)
        val addr = KlassPtr + VTableOff
        issueReq(io.TraceMMUIO.oopTraceMReqs(2), addr, False, U(0), U(0), oopReq(2)) {rd =>
          start_map := rd + vtable_len + itable_len
          end_map := rd + vtable_len + itable_len + nonstaticOopMapSize * GCObjectPtr_Size
          oopTrace_subState := sub_state.s_3
        }
      }
      is(sub_state.s_3){
        when(start_map < end_map){
          // access p and q
          val addr = end_map - GCObjectPtr_Size
          issueReq(io.TraceMMUIO.oopTraceMReqs(3), addr, False, U(0), U(0), oopReq(2)) {rd =>
            p := DestOopPtr + rd(31 downto 0)
            q := DestOopPtr + rd(31 downto 0) + rd(63 downto 32) * GCObjectPtr_Size - GCObjectPtr_Size
            oopTrace_subState := sub_state.s_4
          }
        }.otherwise{
          when(doOopWork.done){
            oopTrace_subState := sub_state.s_0
            state := overall_state.s_end
          }
        }

      }
      is(sub_state.s_4) {
        when(p < q + GCObjectPtr_Size) {
          when(pushFifo(q - DestOopPtr + SrcOopPtr, q, issued_oopTrace)){
            q := q - GCObjectPtr_Size
            issued_oopTrace := False
          }
        }.otherwise {
          oopTrace_subState := sub_state.s_3
          end_map := end_map - GCObjectPtr_Size
        }
      }
    }
  }

  when(state === overall_state.s_end){
    state := overall_state.s_idle
    io.ToTrace.Done := True
  }
}