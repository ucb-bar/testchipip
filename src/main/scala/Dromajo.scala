package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam, StringParam}
import freechips.rocketchip.config.{Parameters}

/**
 * Dromajo bridge to input instruction streams and check with Dromajo
 */
class SimDromajoBridge(insnWidths: TracedInstructionWidths, numInsns: Int) extends Module
{
  val io = IO(new Bundle {
    val trace = Input(new TileTraceIO(insnWidths, numInsns))
  })

  // constants
  val xLen = 64
  val instBits = 32
  // constants

  val traces = io.trace.insns

  val dromajo = Module(new SimDromajoCosimBlackBox(numInsns, xLen))

  dromajo.io.clock := clock
  dromajo.io.reset := reset.asBool

  // TODO: convert to use SInts?
  def signextPad(in: UInt, padLen: Int): UInt = {
    require(in.getWidth <= padLen)
    if (in.getWidth == padLen) {
      in
    } else {
      Mux(in(in.getWidth - 1) === 1.U, Cat(0.U((padLen - in.getWidth).W) - 1.U, in), in.pad(padLen))
    }
  }

  dromajo.io.valid := Cat(traces.map(t => t.valid).reverse)
  dromajo.io.hartid := 0.U
  dromajo.io.pc := Cat(traces.map(t => signextPad(t.iaddr, xLen)).reverse)
  dromajo.io.inst := Cat(traces.map(t => t.insn.pad(instBits)).reverse)
  dromajo.io.wdata := Cat(traces.map(t => signextPad(t.wdata, xLen)).reverse)
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
  def apply(tracedInsns: TileTraceIO)(implicit p: Parameters): SimDromajoBridge = {
    val dbridge = Module(new SimDromajoBridge(tracedInsns.insnWidths, tracedInsns.numInsns))

    dbridge.io.trace := tracedInsns

    dbridge
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
