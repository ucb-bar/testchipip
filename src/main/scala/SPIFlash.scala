package testchipip

import chisel3._
import chisel3.util.{HasBlackBoxResource}
import chisel3.experimental.{Analog, IntParam}

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.util.{PlusArgArtefacts}
import sifive.blocks.devices.spi.{PeripherySPIFlashKey}

class SPIChipIO(val csWidth: Int = 1) extends Bundle {
  val sck = Output(Bool())
  val cs = Vec(csWidth, Output(Bool()))
  val dq = Vec(4, Analog(1.W)) // Not using Analog(4.W) because we can't connect these to IO cells
}

class SPIFlashIO extends SPIChipIO(1) {
  val reset = Output(Reset()) // This is Output because we're going to flip it in the BlackBox
}

class SimSPIFlashModel(capacityBytes: BigInt, id: Int) extends BlackBox(Map(
  "MAX_ADDR" -> IntParam(capacityBytes-1),
  "ID" -> IntParam(id))
) with HasBlackBoxResource {
  val io = IO(Flipped(new SPIFlashIO()))

  // The model adds a +spiflash<ID>=<path> plusarg. It's not a numeric, so we can't use
  // plusarg_reader, but we can still add to the PlusArgArtefacts
  // Unfortunately it requires a numeric default, but that's really just a docstring issue
  PlusArgArtefacts.append(s"spiflash${id}", 0, s"Binary image to mount to SPI flash memory ${id}")

  require(capacityBytes < 0x100000000L, "SimSPIFlashModel only supports 32-bit addressing")

  addResource("/testchipip/vsrc/SimSPIFlashModel.sv")
  addResource("/testchipip/csrc/SimSPIFlashModel.cc")
  addResource("/testchipip/csrc/SPIFlashMem.h")
  addResource("/testchipip/csrc/SPIFlashMem.cc")
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
