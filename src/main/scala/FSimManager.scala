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

class FSimManagerIO extends Bundle {
  val req = Decoupled(UInt(32.W))
  val resp = Flipped(Decoupled(UInt(32.W)))
}

case class FSimManagerControllerParams(address: BigInt, beatBytes: Int)

trait FSimManagerControllerBundle extends Bundle {
  val ext = new FSimManagerIO
}

trait FSimManagerControllerModule extends Module with HasRegMap {
  val io: FSimManagerControllerBundle

  //val query = Reg(Bool())
  //val query = Reg(UInt(32.W))
  val requestQueue = Module(new Queue(UInt(32.W),10))
  val responseQueue = Module(new Queue(UInt(32.W),10))

  //val data = Reg(UInt(32.W))
  val allocRead = Wire(new RegisterReadIO(UInt(32.W)))
  val requestReg = Reg(UInt(32.W))

  io.ext.req <> requestQueue.io.deq
  responseQueue.io.enq <> io.ext.resp
 
  allocRead.request.ready := true.B

  when(!(responseQueue.io.count > 0.U)) {
    allocRead.response.valid := true.B
    allocRead.response.bits := 0.U
  } .otherwise {
    allocRead.response <> responseQueue.io.deq
  }

  regmap(
    0x00 -> Seq(RegField.w(32, requestQueue.io.enq)),
    0x04 -> Seq(RegField.r(32, allocRead))
  )

}

class FSimManagerController(c: FSimManagerControllerParams)(implicit p: Parameters)
  extends TLRegisterRouter(
    c.address, "fsim-manager", Seq("ucbbar,fsim-manager"),
    interrupts = 0, beatBytes = c.beatBytes, concurrency = 1)(
      new TLRegBundle(c, _)     with FSimManagerControllerBundle)(
      new TLRegModule(c, _, _)  with FSimManagerControllerModule)

class SimFSimManager extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val fsim = Flipped(new FSimManagerIO)
  })
}

trait HasPeripheryFSimManager extends HasSystemBus {
  private val address = BigInt(0x10017000)
  val fsimman = LazyModule(new FSimManagerController(FSimManagerControllerParams(address, sbus.beatBytes)))

  fsimman.node := sbus.toVariableWidthSlaves
}

trait HasPeripheryFSimManagerModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryFSimManager
  val clock: Clock
  val reset: Bool
  val fsim = IO(new FSimManagerIO)

  fsim <> outer.fsimman.module.io.ext
  
  
  def connectSimFSimManager() {
    val sim = Module(new SimFSimManager)
    sim.io.clock := clock
    sim.io.reset := reset
    sim.io.fsim <> fsim
  }
}
