package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam, StringParam}
import freechips.rocketchip.config.{Parameters}

/**
 * Dromajo bridge to input instruction streams and check with Dromajo
 */
class SimDromajoBridge(traceProto: Seq[Vec[DeclockedTracedInstruction]]) extends Module
{
  val io = IO(Flipped(new TraceOutputTop(traceProto)))

  // constants
  val xLen = 64
  val instBits = 32
  // constants

  // only supports 1 core/instr. stream
  require(io.traces.size == 1)

  val traces = io.traces(0)

  val dromajo = Module(new SimDromajoCosimBlackBox(traces.size, xLen))

  dromajo.io.clock := clock
  dromajo.io.reset := reset.asBool

  dromajo.io.valid := Cat(traces.map(t => t.valid).reverse)
  dromajo.io.hartid := 0.U
  dromajo.io.pc := Cat(traces.map(t => t.iaddr.pad(xLen)).reverse)
  dromajo.io.inst := Cat(traces.map(t => t.insn.pad(instBits)).reverse)
  dromajo.io.wdata := Cat(traces.map(t => t.wdata.pad(xLen)).reverse)
  dromajo.io.mstatus := 0.U
  dromajo.io.check := ((1 << traces.size) - 1).U

  // assumes that all interrupt/exception signals are the same throughout all committed instructions
  dromajo.io.int_xcpt := traces(0).interrupt || traces(0).exception
  dromajo.io.cause := traces(0).cause.pad(xLen) | (traces(0).interrupt << xLen-1)
}

/**
 * Helper function to connect Dromajo bridge.
 * Mirrors the Dromajo bridge in FireSim.
 */
object SimDromajoBridge
{
  def apply(port: TraceOutputTop)(implicit p: Parameters): Seq[SimDromajoBridge] = {
    val dbridge = Module(new SimDromajoBridge(port.getProto))
    dbridge.io <> port

    Seq(dbridge)
  }
}

/**
 * Connect to the Dromajo Cosimulation Tool through a BB
 */
class SimDromajoCosimBlackBox(
  commitWidth: Int,
  xLen: Int)
  extends BlackBox(Map(
    "COMMIT_WIDTH" -> IntParam(commitWidth),
    "XLEN" -> IntParam(xLen)
  ))
  with HasBlackBoxResource
{
  val instBits = 32
  val maxHartIdBits = 32
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())

    val valid   = Input(UInt(         (commitWidth).W))
    val hartid  = Input(UInt(       (maxHartIdBits).W))
    val pc      = Input(UInt(    (xLen*commitWidth).W))
    val inst    = Input(UInt((instBits*commitWidth).W))
    val wdata   = Input(UInt(    (xLen*commitWidth).W))
    val mstatus = Input(UInt(    (xLen*commitWidth).W))
    val check   = Input(UInt(         (commitWidth).W))

    val int_xcpt = Input(      Bool())
    val cause    = Input(UInt(xLen.W))
  })

  addResource("/testchipip/vsrc/SimDromajoCosimBlackBox.v")
  addResource("/testchipip/csrc/SimDromajoCosim.cc")
  addResource("/testchipip/csrc/dromajo_wrapper.cc")
  addResource("/testchipip/csrc/dromajo_wrapper.h")
}
