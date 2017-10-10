package testchipip

import chisel3._
import chisel3.core.IntParam
import chisel3.util._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.coreplex.CacheBlockBytes
import freechips.rocketchip.coreplex.{HasSystemBus, HasPeripheryBus}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RegisterReadIO,RegisterWriteIO ,RegField, HasRegMap}
import freechips.rocketchip.rocket.PAddrBits
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{ParameterizedBundle, DecoupledHelper, UIntIsOneOf}
import scala.math.max

class WallClockIO extends Bundle {
  val req = Decoupled(UInt(32.W))
  val resp = Flipped(Decoupled(UInt(32.W)))
}

case class WallClockControllerParams(address: BigInt, beatBytes: Int)

trait WallClockControllerBundle extends Bundle {
  val ext = new WallClockIO
}

trait WallClockControllerModule extends Module with HasRegMap {
  val io: WallClockControllerBundle

  //val query = Reg(Bool())
  //val query = Reg(UInt(32.W))
  val requestQueue = Module(new Queue(UInt(32.W),10))
  val responseQueue = Module(new Queue(UInt(32.W),10))

  //val data = Reg(UInt(32.W))
  //val allocRead = Wire(new RegisterReadIO(UInt(32.W)))
  
  io.ext.req <> requestQueue.io.deq
  responseQueue.io.enq <> io.ext.resp

  regmap(
    0x00 -> Seq(RegField.w(32, requestQueue.io.enq)),
    0x04 -> Seq(RegField.r(32, responseQueue.io.deq)))

}

class WallClockController(c: WallClockControllerParams)(implicit p: Parameters)
  extends TLRegisterRouter(
    c.address, "wallclock", Seq("ucbbar,wallclock"),
    interrupts = 0, beatBytes = c.beatBytes)(
      new TLRegBundle(c, _)     with WallClockControllerBundle)(
      new TLRegModule(c, _, _)  with WallClockControllerModule)

class SimWallClock extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val wclk = Flipped(new WallClockIO)
  })
}

trait HasPeripheryWallClock extends HasSystemBus {
  private val address = BigInt(0x10017000)
  val wallclock = LazyModule(new WallClockController(WallClockControllerParams(address, sbus.beatBytes)))

  wallclock.node := sbus.toVariableWidthSlaves
}

trait HasPeripheryWallClockModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryWallClock
  val clock: Clock
  val reset: Bool
  val wclk = IO(new WallClockIO)

  wclk <> outer.wallclock.module.io.ext
  
  
  def connectSimWallClock() {
    val sim = Module(new SimWallClock)
    sim.io.clock := clock
    sim.io.reset := reset
    sim.io.wclk <> wclk
  }
}
