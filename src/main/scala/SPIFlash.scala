package testchipip

import chisel3._
import chisel3.util.{HasBlackBoxResource}
import chisel3.experimental.{Analog, IntParam}

import freechips.rocketchip.config.{Parameters}
import sifive.blocks.devices.spi.{PeripherySPIFlashKey}

class SPIChipIO(csWidth: Int = 1) extends Bundle {
  val sck = Output(Bool())
  val cs = Vec(csWidth, Output(Bool()))
  val dq = Vec(4, Analog(1.W)) // Not using Analog(4.W) because we can't connect these to IO cells
}

class SPIFlashIO extends SPIChipIO(1) {
  val reset = Output(Reset()) // This is Output because we're going to flip it in the BlackBox
}

class SimSPIFlashModel(capacityBytes: BigInt, id: Int) extends BlackBox(Map(
  "CAPACITY_BYTES" -> IntParam(capacityBytes),
  "ID" -> IntParam(id))
) with HasBlackBoxResource {
  val io = IO(Flipped(new SPIFlashIO()))

  addResource("/testchipip/vsrc/SimSPIFlashModel.sv")
}

object SimSPIFlashModel {
  def connect(spi: Seq[SPIChipIO], reset: Reset)(implicit p: Parameters) {
    spi.zip(p(PeripherySPIFlashKey)).zipWithIndex.foreach { case ((port, params), i) =>
      val spi_mem = Module(new SimSPIFlashModel(params.fSize, i))
      spi_mem.suggestName(s"spi_mem_${i}")
      spi_mem.io.sck := port.sck
      require(params.csWidth == 1, "I don't know what to do with your extra CS bits. Fix me please.")
      spi_mem.io.cs(0) := port.cs(0)
      spi_mem.io.dq.zip(port.dq).foreach { case (x, y) => x <> y }
      spi_mem.io.reset := reset.asBool
    }
  }
}
