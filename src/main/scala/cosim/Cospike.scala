package testchipip.cosim

import chisel3._
import chisel3.experimental.{IntParam, StringParam}
import chisel3.util._

import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util._

case class SpikeCosimConfig(
  isa: String,
  priv: String,
  pmpregions: Int,
  maxpglevels: Int,
  nharts: Int,
  bootrom: String,
  has_dtm: Boolean,
  mems: Seq[(BigInt, BigInt)],
  // Optional target facts; the defaults reproduce the previous behaviour.
  devices: Seq[(BigInt, BigInt)] = Nil,
  customCSRs: Seq[(Int, BigInt, BigInt)] = Nil,
  paddrBits: Int = 0,
  vaddrBitsExtended: Int = 0,
  npmpcsrs: Int = 0,
  // Legacy APIs
  mem0_base: BigInt = 0,
  mem0_size: BigInt = 0,
  mem1_base: BigInt = 0,
  mem1_size: BigInt = 0,
  mem2_base: BigInt = 0,
  mem2_size: BigInt = 0,
)

class SpikeCosim(cfg: SpikeCosimConfig) extends BlackBox(Map(
  "ISA" -> StringParam(cfg.isa),
  "PRIV" -> StringParam(cfg.priv),
  "PMPREGIONS" -> IntParam(cfg.pmpregions),
  "MAXPGLEVELS" -> IntParam(cfg.maxpglevels),
  "MEM0_BASE" -> IntParam(cfg.mem0_base),
  "MEM0_SIZE" -> IntParam(cfg.mem0_size),
  "MEM1_BASE" -> IntParam(cfg.mem1_base),
  "MEM1_SIZE" -> IntParam(cfg.mem1_size),
  "MEM2_BASE" -> IntParam(cfg.mem2_base),
  "MEM2_SIZE" -> IntParam(cfg.mem2_size),
  "NHARTS" -> IntParam(cfg.nharts),
  "BOOTROM" -> StringParam(cfg.bootrom)
)) with HasBlackBoxResource
{
  addResource("/testchipip/csrc/cospike.cc")
  addResource("/testchipip/csrc/cospike_impl.cc")
  addResource("/testchipip/csrc/cospike_impl.h")
  addResource("/testchipip/vsrc/cospike.v")
  if (cfg.has_dtm) addResource("/testchipip/csrc/cospike_dtm.h")
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val cycle = Input(UInt(64.W))
    val hartid = Input(UInt(64.W))
    val trace = Input(Vec(4, new Bundle {
      val valid = Bool()
      val iaddr = UInt(64.W)
      val insn = UInt(32.W)
      val exception = Bool()
      val interrupt = Bool()
      val cause = UInt(64.W)
      val has_wdata = Bool()
      val wdata = UInt(64.W)
      val priv = UInt(3.W)
    }))
  })
}

class SpikeCosimRegisterMemory(base: BigInt, size: BigInt) extends BlackBox(Map(
  "BASE" -> IntParam(base),
  "SIZE" -> IntParam(size)))
{
  val io = IO(new Bundle {})
}

class SpikeCosimRegisterDevice(base: BigInt, size: BigInt) extends BlackBox(Map(
  "BASE" -> IntParam(base),
  "SIZE" -> IntParam(size)))
{
  val io = IO(new Bundle {})
}

class SpikeCosimRegisterCSR(addr: Int, mask: BigInt, init: BigInt) extends BlackBox(Map(
  "ADDR" -> IntParam(addr),
  "MASK" -> IntParam(mask),
  "INIT" -> IntParam(init)))
{
  val io = IO(new Bundle {})
}

class SpikeCosimTargetParams(paddrBits: Int, vaddrBitsExtended: Int, npmpcsrs: Int)
  extends BlackBox(Map(
    "PADDRBITS" -> IntParam(paddrBits),
    "VADDRBITSEXTENDED" -> IntParam(vaddrBitsExtended),
    "NPMPCSRS" -> IntParam(npmpcsrs)))
{
  val io = IO(new Bundle {})
}

object SpikeCosim
{
  def apply(trace: TileTraceIO, hartid: Int, cfg: SpikeCosimConfig) = {
    val cosim = Module(new SpikeCosim(cfg))
    for ((base, size) <- cfg.mems) {
      val reg = Module(new SpikeCosimRegisterMemory(base, size))
    }
    for ((base, size) <- cfg.devices) {
      val dev = Module(new SpikeCosimRegisterDevice(base, size))
    }
    for ((addr, mask, init) <- cfg.customCSRs) {
      val csr = Module(new SpikeCosimRegisterCSR(addr, mask, init))
    }
    if (cfg.paddrBits > 0 || cfg.vaddrBitsExtended > 0 || cfg.npmpcsrs > 0) {
      val params = Module(new SpikeCosimTargetParams(cfg.paddrBits, cfg.vaddrBitsExtended, cfg.npmpcsrs))
    }
    val cycle = withClockAndReset(trace.clock, trace.reset) {
      val r = RegInit(0.U(64.W))
      r := r + 1.U
      r
    }
    cosim.io.clock := trace.clock
    cosim.io.reset := trace.reset
    require(trace.numInsns <= 4)
    cosim.io.cycle := cycle
    cosim.io.trace.map(t => {
      t := DontCare
      t.valid := false.B
    })
    cosim.io.hartid := hartid.U
    for (i <- 0 until trace.numInsns) {
      val insn = trace.trace.insns(i)
      cosim.io.trace(i).valid := insn.valid
      val signed = Wire(SInt(64.W))
      signed := insn.iaddr.asSInt
      cosim.io.trace(i).iaddr := signed.asUInt
      cosim.io.trace(i).insn := insn.insn
      cosim.io.trace(i).exception := insn.exception
      cosim.io.trace(i).interrupt := insn.interrupt
      cosim.io.trace(i).cause := insn.cause
      cosim.io.trace(i).has_wdata := insn.wdata.isDefined.B
      cosim.io.trace(i).wdata := insn.wdata.getOrElse(0.U)
      cosim.io.trace(i).priv := insn.priv
    }
  }
}

