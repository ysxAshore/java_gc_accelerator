package hwgc_acc

import hwgc_top.{Config, GCTopParameters}
import spinal.core._
import spinal.lib.StreamArbiterFactory

import scala.language.postfixOps

class GCAccTop extends Module with GCTopParameters {
  val io  = new GCAccTopIO

  // 19 = 1(TaskStack) + 3(Fetch:PrePop,Push,Main) + 1(ArrayProcess) + 2(OopProcess) + 2(OopCopy2Survivor) + 1(Allocate) + 
  //      3(ParAllocate) + 1(NewGCAlloc) + 1(AllocFreeRegion) + 1(Aop) + 1(Trace) +
  //      2(Copy)
  val LocalMMUIOsNum = 19

  val gcAop = new GCAop
  val gcCopy = new GCCopy
  val gcFetch = new GCFetch
  val gcTrace = new GCTrace
  val gcAllocate = new GCAllocate
  val gcTaskStack = new GCTaskStack
  val gcNewGCAlloc = new GCNewGCAlloc
  val gcOopProcess = new GCOopProcess
  val gcParAllocate = new GCParAllocate
  val gcArrayProcess = new GCArrayProcess
  val gcAllocFreeRegion = new GCAllocFreeRegion
  val gcOopCopy2Survivor = new GCOopCopy2Survivor
  val gcUnalignedMMUAdapter = Array.fill(LocalMMUIOsNum - 2)(new GCUnalignedMMUAdapter) // - 2(exclude Copy readMreq and writeMreq)

  val gcLocalMMU = new GCLocalMMU(LocalMMUIOsNum)
  gcUnalignedMMUAdapter.zipWithIndex.foreach{ case(adapter, i) =>
    gcLocalMMU.io.localMMUIOs(i) <> adapter.io.out
  }

  val task_valid = RegInit(False) // 当前有任务在进行
  val taskStackDone = RegInit(False) // TaskStack 完成缓存
  val taskStackStart = RegInit(False) // TaskStack 握手Valid, 在握手之后复位

  val ChunkSize = RegInit(U(0, 32 bits))
  val AgeThreshold = RegInit(U(0, 32 bits))
  val HeapRegionBias = RegInit(U(0, 32 bits))
  val RegionAttrShiftBy = RegInit(U(0, 32 bits))
  val HeapRegionShiftBy = RegInit(U(0, 32 bits))
  val LogOfHRGrainBytes = RegInit(U(0, 32 bits))
  val StepperOffset = RegInit(U(0, GCElementWidth bits))
  val YoungWordsBase = RegInit(U(0, GCElementWidth bits))
  val RegionAttrBase = RegInit(U(0, GCElementWidth bits))
  val PlabAllocatorPtr = RegInit(U(0, GCElementWidth bits))
  val RegionAttrBiasedBase = RegInit(U(0, GCElementWidth bits))
  val HeapRegionBiasedBase = RegInit(U(0, GCElementWidth bits))
  val ParScanThreadStatePtr = RegInit(U(0, GCElementWidth bits))
  val TaskQueue_Bottom = RegInit(U(0, 32 bits))
  val TaskQueue_ElemsBase = RegInit(U(0, GCElementWidth bits))
  val HumongousReclaimCandidatesBoolBase = RegInit(U(0, GCElementWidth bits))
  val CardTablePtr = RegInit(U(0, GCElementWidth bits))
  val G1h = RegInit(U(0, GCElementWidth bits))
  val Thread = RegInit(U(0, GCElementWidth bits))
  val LockPtr = RegInit(U(0, GCElementWidth bits))
  val DummyRegion = RegInit(U(0, GCElementWidth bits))
  val IntArrayKlassObj = RegInit(U(0, GCElementWidth bits))
  val ObjectKlass = RegInit(U(0, GCElementWidth bits))
  val CompressedOopBase = RegInit(U(0, GCElementWidth bits))
  val CompressedKlassPointerBase = RegInit(U(0, GCElementWidth bits))
  val CompressedFlag = RegInit(U(0, 32 bits))

  val DebugTimeStamp = RegInit(U(0,64 bits))
  DebugTimeStamp := DebugTimeStamp + U(1)

  io.config.cmd.ready := !task_valid
  io.config.Done := task_valid && (gcTaskStack.io.ConfigIO.Done || taskStackDone) && gcOopProcess.io.SlotIsEmpty

  when(task_valid && gcTaskStack.io.ConfigIO.Done){
    taskStackDone := True
  }

  when(io.config.Done){
    task_valid := False
    taskStackDone := False
  }

  when(io.config.cmd.fire){
    task_valid := True
    taskStackStart := True

    ChunkSize                         := io.config.cmd.payload.ChunkSize
    AgeThreshold                      := io.config.cmd.payload.AgeThreshold
    HeapRegionBias                    := io.config.cmd.payload.HeapRegionBias
    RegionAttrShiftBy                 := io.config.cmd.payload.RegionAttrShiftBy
    HeapRegionShiftBy                 := io.config.cmd.payload.HeapRegionShiftBy
    LogOfHRGrainBytes                 := io.config.cmd.payload.LogOfHRGrainBytes
    StepperOffset                     := io.config.cmd.payload.StepperOffset
    YoungWordsBase                    := io.config.cmd.payload.YoungWordsBase
    RegionAttrBase                    := io.config.cmd.payload.RegionAttrBase
    PlabAllocatorPtr                  := io.config.cmd.payload.PlabAllocatorPtr
    RegionAttrBiasedBase              := io.config.cmd.payload.RegionAttrBiasedBase
    HeapRegionBiasedBase              := io.config.cmd.payload.HeapRegionBiasedBase
    ParScanThreadStatePtr             := io.config.cmd.payload.ParScanThreadStatePtr
    TaskQueue_Bottom                  := io.config.cmd.payload.TaskQueue_Bottom
    TaskQueue_ElemsBase               := io.config.cmd.payload.TaskQueue_ElemsBase
    HumongousReclaimCandidatesBoolBase:= io.config.cmd.payload.HumongousReclaimCandidatesBoolBase
    CardTablePtr                      := io.config.cmd.payload.CardTablePtr
    G1h                               := io.config.cmd.payload.G1h
    Thread                            := io.config.cmd.payload.Thread
    LockPtr                           := io.config.cmd.payload.LockPtr
    DummyRegion                       := io.config.cmd.payload.DummyRegion
    IntArrayKlassObj                  := io.config.cmd.payload.IntArrayKlassObj
    ObjectKlass                       := io.config.cmd.payload.ObjectKlass
    CompressedOopBase                 := io.config.cmd.payload.CompressedOopBase
    CompressedKlassPointerBase        := io.config.cmd.payload.CompressedKlassPointerBase
    CompressedFlag                    := io.config.cmd.payload.CompressedFlag
  }

  // 内部可能会更新AgeThreshold
  when(gcOopCopy2Survivor.io.UpdateAgeThreshold.valid){
    AgeThreshold := gcOopCopy2Survivor.io.UpdateAgeThreshold.payload.resized
  }

  // gcTaskStack 握手成功 则 复位 taskStackStart
  when(gcTaskStack.io.ConfigIO.config.fire){
    taskStackStart := False
  }

  // GCTaskStack
  gcTaskStack.io.toFetch <> gcFetch.io.toFetch
  gcTaskStack.io.toStack <> gcTrace.io.ToStack

  gcTaskStack.io.Mreq <> gcUnalignedMMUAdapter(0).io.in

  gcTaskStack.io.ConfigIO.config.payload.TaskQueue_Bottom := TaskQueue_Bottom
  gcTaskStack.io.ConfigIO.config.payload.TaskQueue_ElemsBase := TaskQueue_ElemsBase
  gcTaskStack.io.ConfigIO.config.valid := taskStackStart
  gcTaskStack.io.DebugTimeStamp := DebugTimeStamp

  // GCFetch
  gcFetch.io.MainMreq <> gcUnalignedMMUAdapter(1).io.in
  gcFetch.io.PushMreq <> gcUnalignedMMUAdapter(2).io.in
  gcFetch.io.PreMreq <> gcUnalignedMMUAdapter(3).io.in

  gcFetch.io.Fetch2OopProcess <> gcOopProcess.io.Fetch2Process
  gcFetch.io.Fetch2ArrayProcess <> gcArrayProcess.io.Fetch2Process
  gcFetch.io.Trace2Fetch <> gcTrace.io.Trace2Fetch

  gcFetch.io.gcWriteSrcOopPtr.writeForward.valid := gcOopCopy2Survivor.io.ToFetch.writeForward.valid
  gcFetch.io.gcWriteSrcOopPtr.writeForward.payload := gcOopCopy2Survivor.io.ToFetch.writeForward.payload

  gcFetch.io.CopyFwdMain <> gcCopy.io.FwdMain
  gcFetch.io.CopyFwdPush <> gcCopy.io.FwdPush
  gcFetch.io.CopyFwdPre  <> gcCopy.io.FwdPre
  gcFetch.io.CopyState := gcCopy.io.State
  gcFetch.io.CopyDone := gcCopy.io.ToCopy.Done

  gcFetch.io.ConfigIO.UseCompressedOop := CompressedFlag(0)
  gcFetch.io.ConfigIO.CompressedOopBase := CompressedOopBase
  gcFetch.io.ConfigIO.CompressedOopShift := CompressedFlag(15 downto 8)
  gcFetch.io.ConfigIO.UseCompressedKlassPointers := CompressedFlag(1)
  gcFetch.io.DebugTimeStamp := DebugTimeStamp

  // GCOopProcess
  gcOopProcess.io.Mreq0 <> gcUnalignedMMUAdapter(4).io.in
  gcOopProcess.io.Mreq1 <> gcUnalignedMMUAdapter(5).io.in

  gcOopProcess.io.gcWriteSrcOopPtr.writeForward.valid := gcOopCopy2Survivor.io.ToFetch.writeForward.valid
  gcOopProcess.io.gcWriteSrcOopPtr.writeForward.payload := gcOopCopy2Survivor.io.ToFetch.writeForward.payload

  gcOopProcess.io.Process2CopySurvivor <> gcOopCopy2Survivor.io.ToCopySurvivor

  gcOopProcess.io.ConfigIO.RegionAttrShiftBy := RegionAttrShiftBy
  gcOopProcess.io.ConfigIO.RegionAttrBiasedBase := RegionAttrBiasedBase
  gcOopProcess.io.ConfigIO.LogOfHRGrainBytes := LogOfHRGrainBytes
  gcOopProcess.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy
  gcOopProcess.io.ConfigIO.HeapRegionBiasedBase := HeapRegionBiasedBase
  gcOopProcess.io.ConfigIO.UseCompressedOop := CompressedFlag(0)
  gcOopProcess.io.ConfigIO.CompressedOopBase := CompressedOopBase
  gcOopProcess.io.ConfigIO.CompressedOopShift := CompressedFlag(15 downto 8)
  gcOopProcess.io.DebugTimeStamp := DebugTimeStamp

  // GCArrayProcess
  gcArrayProcess.io.Mreq <> gcUnalignedMMUAdapter(6).io.in

  gcArrayProcess.io.gcWriteSrcOopPtr.writeForward.valid := gcOopCopy2Survivor.io.ToFetch.writeForward.valid
  gcArrayProcess.io.gcWriteSrcOopPtr.writeForward.payload := gcOopCopy2Survivor.io.ToFetch.writeForward.payload

  gcArrayProcess.io.ConfigIO.ChunkSize := ChunkSize
  gcArrayProcess.io.ConfigIO.StepperOffset := StepperOffset
  gcArrayProcess.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy
  gcArrayProcess.io.ConfigIO.HeapRegionBiasedBase := HeapRegionBiasedBase
  gcArrayProcess.io.ConfigIO.UseCompressedKlassPointers := CompressedFlag(1)
  gcArrayProcess.io.DebugTimeStamp := DebugTimeStamp

  // GCOopCopy2Survivor
  gcOopCopy2Survivor.io.Mreq0 <> gcUnalignedMMUAdapter(7).io.in
  gcOopCopy2Survivor.io.Mreq1 <> gcUnalignedMMUAdapter(8).io.in

  gcOopCopy2Survivor.io.ToCopy <> gcCopy.io.ToCopy

  gcOopCopy2Survivor.io.ToAllocate <> gcAllocate.io.ToAllocate

  gcOopCopy2Survivor.io.ConfigIO.ChunkSize := ChunkSize
  gcOopCopy2Survivor.io.ConfigIO.AgeThreshold := AgeThreshold
  gcOopCopy2Survivor.io.ConfigIO.YoungWordsBase := YoungWordsBase
  gcOopCopy2Survivor.io.ConfigIO.PlabAllocatorPtr := PlabAllocatorPtr
  gcOopCopy2Survivor.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy
  gcOopCopy2Survivor.io.ConfigIO.HeapRegionBiasedBase := HeapRegionBiasedBase
  gcOopCopy2Survivor.io.ConfigIO.ParScanThreadStatePtr := ParScanThreadStatePtr
  gcOopCopy2Survivor.io.ConfigIO.UseCompressedKlassPointer := CompressedFlag(1)
  gcOopCopy2Survivor.io.ConfigIO.CompressedKlassPointerBase := CompressedKlassPointerBase
  gcOopCopy2Survivor.io.ConfigIO.CompressedKlassPointerShift := CompressedFlag(23 downto 16)
  gcOopCopy2Survivor.io.ConfigIO.IntArrayKlassObj := IntArrayKlassObj
  gcOopCopy2Survivor.io.ConfigIO.ObjectKlassObj := ObjectKlass
  gcOopCopy2Survivor.io.DebugTimeStamp := DebugTimeStamp

  // gcAllocate(ToParAllocate)
  gcAllocate.io.Mreq <> gcUnalignedMMUAdapter(9).io.in

  gcAllocate.io.ToParAllocate <> gcParAllocate.io.ToParAllocate

  gcAllocate.io.ConfigIO.G1h := G1h
  gcAllocate.io.ConfigIO.ObjectKlassObj := ObjectKlass
  gcAllocate.io.ConfigIO.IntArrayKlassObj := IntArrayKlassObj
  gcAllocate.io.ConfigIO.PlabAllocatorPtr := PlabAllocatorPtr
  gcAllocate.io.ConfigIO.UseCompressedKlassPointers := CompressedFlag(1)
  gcAllocate.io.ConfigIO.CompressedKlassPointerBase := CompressedKlassPointerBase
  gcAllocate.io.ConfigIO.CompressedKlassPointerShift := CompressedFlag(23 downto 16)
  gcAllocate.io.DebugTimeStamp := DebugTimeStamp

  // gcParAllocate
  gcParAllocate.io.MreqPar <> gcUnalignedMMUAdapter(11).io.in
  gcParAllocate.io.MreqMainIml <> gcUnalignedMMUAdapter(10).io.in
  gcParAllocate.io.MreqAttempt <> gcUnalignedMMUAdapter(12).io.in

  gcParAllocate.io.ToNewGCAlloc <> gcNewGCAlloc.io.ToNewGCAlloc

  gcParAllocate.io.CacheUpdateOut <> io.CacheUpdateOut
  gcParAllocate.io.CacheUpdateIn <> io.CacheUpdateIn

  gcParAllocate.io.Irq.clearIn()

  gcParAllocate.io.ConfigIO.G1h := G1h
  gcParAllocate.io.ConfigIO.Thread := Thread
  gcParAllocate.io.ConfigIO.LockPtr := LockPtr
  gcParAllocate.io.ConfigIO.DummyRegion := DummyRegion
  gcParAllocate.io.DebugTimeStamp := DebugTimeStamp

  // gcNewAlloc
  gcNewGCAlloc.io.Mreq <> gcUnalignedMMUAdapter(13).io.in

  gcNewGCAlloc.io.ToAllocFreeRegion <> gcAllocFreeRegion.io.ToAllocFreeRegion

  gcNewGCAlloc.io.Irq.clearIn()

  gcNewGCAlloc.io.ConfigIO.G1h := G1h
  gcNewGCAlloc.io.ConfigIO.DummyRegion := DummyRegion

  // gcAllocFreeRegion
  gcAllocFreeRegion.io.Mreq <> gcUnalignedMMUAdapter(14).io.in

  gcAllocFreeRegion.io.ConfigIO.G1h := G1h

  // gcTrace
  gcTrace.io.Mreq <> gcUnalignedMMUAdapter(15).io.in

  gcTrace.io.ConfigIO.ChunkSize                          := ChunkSize
  gcTrace.io.ConfigIO.RegionAttrBase                     := RegionAttrBase
  gcTrace.io.ConfigIO.RegionAttrShiftBy                  := RegionAttrShiftBy
  gcTrace.io.ConfigIO.RegionAttrBiasedBase               := RegionAttrBiasedBase
  gcTrace.io.ConfigIO.HeapRegionBias                     := HeapRegionBias
  gcTrace.io.ConfigIO.HeapRegionShiftBy                  := HeapRegionShiftBy
  gcTrace.io.ConfigIO.LogOfHRGrainBytes                  := LogOfHRGrainBytes
  gcTrace.io.ConfigIO.UseCompressedOops                  := CompressedFlag(0)
  gcTrace.io.ConfigIO.CompressedOopBase                  := CompressedOopBase
  gcTrace.io.ConfigIO.CompressedOopShift                 := CompressedFlag(15 downto 8)
  gcTrace.io.ConfigIO.UseCompressedKlassPointers         := CompressedFlag(1)
  gcTrace.io.ConfigIO.HumongousReclaimCandidatesBoolBase := HumongousReclaimCandidatesBoolBase
  gcTrace.io.DebugTimeStamp                              := DebugTimeStamp

  // gcAop
  gcAop.io.Mreq <> gcUnalignedMMUAdapter(16).io.in

  gcAop.io.Irq.clearIn()

  gcAop.io.ConfigIO.CardTablePtr := CardTablePtr
  gcAop.io.ConfigIO.ParScanThreadStatePtr := ParScanThreadStatePtr
  gcAop.io.DebugTimeStamp := DebugTimeStamp

  gcAop.io.NoAopSrc := (gcTaskStack.io.ConfigIO.Done || taskStackDone) && gcOopProcess.io.SlotIsEmpty // 不会再传递新的Aop任务

  // gcCopy Copy的Mreq直接连LocalMMU 不需要经过对齐(其实现在的Unaligned对于对齐的也是直连的 也可以连接Unaligned, 这里为了省部件就不连了)
  gcCopy.io.readMReq <> gcLocalMMU.io.localMMUIOs(17)
  gcCopy.io.writeMReq <> gcLocalMMU.io.localMMUIOs(18)


  // gcLocalMMU
  gcLocalMMU.io.LastLevelCacheTLIO <> io.mmu2llc

  // gcTrace source arbiter
  val traceInputs = Seq(
    gcArrayProcess.io.Process2Trace.cmd,      // source 0
    gcOopCopy2Survivor.io.ToTrace.cmd         // source 1
  )
  val traceArb = StreamArbiterFactory().roundRobin.transactionLock.buildOn(traceInputs)
  val traceOwner = Reg(UInt(1 bits)) init(0)

  gcTrace.io.ToTrace.cmd << traceArb.io.output

  when(traceArb.io.output.fire) {
    traceOwner := traceArb.io.chosen
  }

  gcArrayProcess.io.Process2Trace.Done := False
  gcOopCopy2Survivor.io.ToTrace.Done := False

  when(gcTrace.io.ToTrace.Done) {
    switch(traceOwner) {
      is(U(0)) {
        gcArrayProcess.io.Process2Trace.Done := True
      }

      is(U(1)) {
        gcOopCopy2Survivor.io.ToTrace.Done := True
      }
    }
  }

  // gcAop source arbiter
  val aopInputs = Seq(
    gcOopProcess.io.Process2Aop.cmd,        // source 0
    gcTrace.io.ToAop.cmd,                   // source 1
  )
  val aopArb = StreamArbiterFactory().roundRobin.transactionLock.buildOn(aopInputs)
  val aopOwner = Reg(UInt(1 bits)) init(0)

  gcAop.io.Aop.cmd << aopArb.io.output

  when(aopArb.io.output.fire) {
    aopOwner := aopArb.io.chosen
  }

  gcOopProcess.io.Process2Aop.Done := False
  gcTrace.io.ToAop.Done := False

  when(gcAop.io.Aop.Done) {
    switch(aopOwner) {
      is(U(0)) {
        gcOopProcess.io.Process2Aop.Done := True
      }

      is(U(1)) {
        gcTrace.io.ToAop.Done := True
      }
    }
  }
}

object GCAccTopVerilog extends App{
  Config.spinal.generateVerilog(new GCAccTop())
}