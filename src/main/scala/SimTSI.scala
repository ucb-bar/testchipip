package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.{IO, DataMirror}
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.debug.HasPeripheryDebug
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._
import scala.math.min
import freechips.rocketchip.amba.axi4._
import sifive.blocks.devices.uart._

object SimTSI {
  def connect(tsi: Option[TSIIO], clock: Clock, reset: Reset): Bool = {
    val exit = tsi.map { s =>
      val sim = Module(new SimTSI)
      sim.io.clock := clock
      sim.io.reset := reset
      sim.io.tsi <> s
      sim.io.exit
    }.getOrElse(0.U)

    val success = exit === 1.U
    val error = exit >= 2.U
    assert(!error, "*** FAILED *** (exit code = %d)\n", exit >> 1.U)
    success
  }
}

class SimTSI extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val tsi = Flipped(new TSIIO)
    val exit = Output(UInt(32.W))
  })

  addResource("/testchipip/vsrc/SimTSI.v")
  addResource("/testchipip/csrc/SimTSI.cc")
  addResource("/testchipip/csrc/testchip_tsi.cc")
  addResource("/testchipip/csrc/testchip_tsi.h")
}
