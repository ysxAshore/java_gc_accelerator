package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCArrayProcess extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Fetch2Process = slave(new ProcessUnit)
    val Process2Trace = master(new GCProcess2Trace)
  }

  object overall_state extends SpinalEnum {
    val s_idle, s_readSrcLen, s_readDestLen, s_readChunk, s_writeDestLen, s_readStepper, s_doTrace, s_waitDone = newElement()
  }
}
