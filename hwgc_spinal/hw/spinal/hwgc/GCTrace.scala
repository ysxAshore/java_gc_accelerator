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

class GCTrace(oopWorkStages:Int) extends Module with GCParameters with HWParameters{
  val io = new Bundle {
    val TraceMMUIO = master(new TraceMReq2MMU(oopWorkStages - 1, 4))
    val DebugInfo = in(new DebugInfoIO)
    val ToTrace = slave(new GCParse2Trace)
    val Trace2Stack = master Stream UInt(MMUAddrWidth bits)
    val Trace2Aop = master(new AopParameters)
  }

  // default value
  for(i <- 0 until oopWorkStages){
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
  def pushFifo(src: UInt, dest: UInt): Bool = {
    traceTaskFifo.io.push.valid := True
    traceTaskFifo.io.push.payload.src := src
    traceTaskFifo.io.push.payload.dest := dest

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
    val sReqIssued = Vec(RegInit(False), oopWorkStages - 1)
    val sValid = Vec(RegInit(False), oopWorkStages)
    val sReg = Vec(RegInit(StageData().getZero), oopWorkStages)

    traceTaskFifo.io.pop.ready := !sValid(0)
    when(traceTaskFifo.io.pop.fire){
      sReg(0).src := traceTaskFifo.io.pop.payload.src
      sReg(0).dest := traceTaskFifo.io.pop.payload.dest
      sValid(0) := True
    }

    // process every stage mmu req 0~13
    for(i <- 0 until oopWorkStages - 1){
      val want = Bool()
      val addr = UInt(MMUAddrWidth bits)
      val isWrite = Bool()
      val wdata = UInt(MMUAddrWidth bits)
      val wmask = UInt(MMUDataWidth / 8 bits)

      want := False
      addr := U(0)
      isWrite := False
      wdata := U(0)
      wmask := U(0)

      when(sValid(i) && !sReqIssued(i)){
        want := True
        isWrite := False
        switch(i){
          is(0) {
            addr := sReg(0).src
          }
          is(1) {
            addr := RegionAttrBiasedBase + (sReg(1).obj >> RegionAttrShiftBy) * GCHeapRegionAttr_Size
          }
          is(3) {
            addr := HumongousReclaimCandidatesBoolBase + ((sReg(3).obj - (HeapRegionBias << HeapRegionShiftBy)) >> LogOfHRGrainBytes)
          }
          is(4) {
            addr := HumongousReclaimCandidatesBoolBase + ((sReg(4).obj - (HeapRegionBias << HeapRegionShiftBy)) >> LogOfHRGrainBytes)
            isWrite := True
            wdata := U(0)
            wmask := U(1)
          }
          is(5) {
            addr :=  RegionAttrBase + ((sReg(5).obj - (HeapRegionBias << HeapRegionShiftBy)) >> LogOfHRGrainBytes) * GCHeapRegionAttr_Size + TypeOffSet
            isWrite := True
            wdata := U(Type_NoInCset)
            wmask := U(1)
          }
          is(6) {
            addr := ParScanThreadStatePtr + OBJCLOSURE_OFFSET + SCANNING_IN_YOUNG_OFFSET
          }
          default{
            want := False
          }
        }
      }
      when(want){
        io.TraceMMUIO.oopWorkMReqs(i).Request.valid := True
        io.TraceMMUIO.oopWorkMReqs(i).Request.payload.RequestVirtualAddr := addr
        io.TraceMMUIO.oopWorkMReqs(i).Request.payload.RequestType_isWrite := isWrite
        io.TraceMMUIO.oopWorkMReqs(i).Request.payload.RequestWStrb := wmask
        io.TraceMMUIO.oopWorkMReqs(i).Request.payload.RequestData := wdata
        io.TraceMMUIO.oopWorkMReqs(i).Request.payload.RequestSourceID := io.TraceMMUIO.oopWorkMReqs(i).ConherentRequsetSourceID.payload
        io.TraceMMUIO.oopWorkMReqs(i).Response.ready := True
        when(io.TraceMMUIO.oopWorkMReqs(i).Request.fire){
          sReqIssued(i) := True
        }
      }
    }

    // Response fire
    for(i <- 0 until oopWorkStages - 1){
      when(io.TraceMMUIO.oopWorkMReqs(i).Response.fire && sValid(i)) {
        val data = io.TraceMMUIO.oopWorkMReqs(i).Response.payload.ResponseData
        sReqIssued(i) := False
        sReg(i + 1) := sReg(i)
        switch(i) {
          is(0) {
            when(data =/= U(0) && !sValid(1)) {
              sValid(0) := False
              sValid(1) := True
              sReg(1).obj := data
            }
          }
          is(1) {
            when(!(data(7 downto 0).asSInt < Type_Young && (sReg(1).dest ^ sReg(1).obj) >> LogOfHRGrainBytes === 0) && !sValid(2)) {
              sValid(1) := False
              sValid(2) := True
              sReg(2).regionAttr := data(15 downto 0)
            }
          }
          is(3) {
            when(data(0) && !sValid(4)) {
              sValid(3) := False
              sValid(4) := True
            }.elsewhen(!data(0) && !sValid(6)) {
              sValid(3) := False
              sValid(6) := True
              sReg(6) := sReg(3)
            }
          }
          is(6) {
            when(data(7 downto 0) =/= U(1) && sReg(6).regionAttr(7 downto 0) === U(1) && !sValid(7)) {
              sValid(6) := False
              sValid(7) := True
            }
          }

          default {
            when(!sValid(i + 1)){
              sValid(i) := False
              sValid(i + 1) := True
              sReg(i + 1) := sReg(i)
            }
          }
        }
      }.elsewhen(U(i) === U(2) && sValid(2)){
          when(sReg(2).regionAttr(15 downto 8).asSInt >= Type_Young) {
            io.Trace2Stack.valid := True
            io.Trace2Stack.payload := sReg(2).dest
            when(io.Trace2Stack.fire) {
              sValid(2) := False
            }
          }.elsewhen(sReg(2).regionAttr(15 downto 8).asSInt === Type_Humongous && !sValid(3)){
            sValid(2) := False
            sValid(3) := True
            sReg(3) := sReg(2)
          }.elsewhen(!sValid(6)){
            sValid(2) := False
            sValid(6) := True
            sReg(6) := sReg(2)
          }
      }.elsewhen(sValid(7) && U(i) === 7){
        // send GCAop
        io.Trace2Aop.Valid := True
        io.Trace2Aop.ParScanThreadStatePtr := ParScanThreadStatePtr
        io.Trace2Aop.DestOopPtr := sReg(7).dest
        when(io.Trace2Aop.Valid && io.Trace2Aop.Ready){
          sValid(7) := False
        }
      }
    }
    val done = traceTaskFifo.io.occupancy === U(0) && !sValid.orR && io.Trace2Aop.Done
  }

  val p = RegInit(U(0, IntWidth bits))
  val q = RegInit(U(0, IntWidth bits))
  val arrayTrace_subState = RegInit(sub_state.s_0)
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
          when(pushFifo(p - DestOopPtr + SrcOopPtr, p)){
            p := p + GCObjectPtr_Size
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
  when(state === overall_state.s_refKlassTrace){
    switch(refTrace_subState){
      is(sub_state.s_0){
        // do_oop_work(DISCOVERED)
        when(pushFifo(SrcOopPtr + DISCOVERED_OFFSET, DestOopPtr + DISCOVERED_OFFSET)){
          refTrace_subState := sub_state.s_1
        }
      }
      is(sub_state.s_1){
        // do_oop_work(DISCOVERED)
        when(pushFifo(SrcOopPtr + REFERENT_OFFSET, DestOopPtr + REFERENT_OFFSET)){
          refTrace_subState := sub_state.s_2
        }
      }
      is(sub_state.s_2){
        // do_oop_work(DISCOVERED)
        when(pushFifo(SrcOopPtr + DISCOVERED_OFFSET, DestOopPtr + DISCOVERED_OFFSET)){
          refTrace_subState := sub_state.s_3
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
          when(pushFifo(p - DestOopPtr + SrcOopPtr, p)){
            p := p + GCObjectPtr_Size
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
          oopTrace_subState := sub_state.s_0
          state := overall_state.s_end
        }

      }
      is(sub_state.s_4) {
        when(p < q + GCObjectPtr_Size) {
          when(pushFifo(q - DestOopPtr + SrcOopPtr, q)){
            q := q - GCObjectPtr_Size
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
  }
}