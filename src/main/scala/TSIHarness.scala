package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.{IO, DataMirror}
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._
import freechips.rocketchip.amba.axi4._

import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}

object TSIHarness {
  def connectRAM(serdesser: TLSerdesser, port: SerialIO, reset: Reset): SerialRAM = {
    implicit val p: Parameters = serdesser.p

    val ram = LazyModule(new SerialRAM(serdesser))

    val module = Module(ram.module)
    module.io.ser <> port

    require(ram.serdesser.module.mergedParams == serdesser.module.mergedParams,
      "Mismatch between chip-side diplomatic params and harness-side diplomatic params:\n" +
      s"Harness-side params: ${ram.serdesser.module.mergedParams}\n" +
      s"Chip-side params: ${serdesser.module.mergedParams}")

    ram
  }

  def tieoff(serial: Option[SerialIO]) {
    serial.foreach { s =>
      s.in.valid := false.B
      s.in.bits := DontCare
      s.out.ready := true.B
    }
  }

  def tieoff(serial: SerialIO) { tieoff(Some(serial)) }
}

object SerialTLROM {
  def apply(romParams: SerialTLROMParams, beatBytes: Int)(implicit p: Parameters): TLROM = {
    lazy val romContents = {
      val romData = romParams.contentFileName.map(n => Files.readAllBytes(Paths.get(n))).getOrElse(
        Array(
          0x1b, 0x05, 0x10, 0x00, // 0010051b     addiw    a0,zero,1
          0x13, 0x15, 0xf5, 0x01, // 01f51513     slli     a0,a0,0x1f (li a0, 0x8000_0000)
          0x73, 0x10, 0x15, 0x34, // 34151073     csrw     mepc,a0
          0x37, 0x25, 0x00, 0x00, // 00002537     lui      a0,0x2
          0x1b, 0x05, 0x05, 0x80, // 8005051b     addiw    a0,a0,-2048
          0x73, 0x20, 0x05, 0x30, // 30052073     csrs     mstatus,a0
          0x73, 0x25, 0x40, 0xf1, // f1402573     csrr     a0,mhartid
          0x73, 0x00, 0x20, 0x30  // 30200073     mret
        ).map(_.toByte)
      )
      val rom = ByteBuffer.wrap(romData)
      rom.array()
    }
    val rom = LazyModule(new TLROM(romParams.address, romParams.size, romContents, true, beatBytes))
    rom
  }
}

class SerialRAM(tl_serdesser: TLSerdesser)(implicit p: Parameters) extends LazyModule {
  val managerParams = tl_serdesser.module.client_edge.map(_.slave) // the managerParams are the chip-side clientParams
  val clientParams = tl_serdesser.module.manager_edge.map(_.master) // The clientParams are the chip-side managerParams
  val tsi2tl = LazyModule(new TSIToTileLink)
  val serdesser = LazyModule(new TLSerdesser(
    tl_serdesser.w,
    clientParams,
    managerParams
  ))

  serdesser.clientNode.foreach { clientNode =>
    val memParams = p(SerialTLKey).get.serialTLManagerParams.get.memParams
    val romParams = p(SerialTLKey).get.serialTLManagerParams.get.romParams
    val srams = AddressSet.misaligned(memParams.base, memParams.size).map { aset =>
      LazyModule(new TLRAM(
        aset,
        beatBytes = memParams.beatBytes
      ))
    }

    val xbar = TLXbar()
    srams.foreach { s => s.node := TLBuffer() := TLFragmenter(memParams.beatBytes, p(CacheBlockBytes)) := xbar }

    romParams.map { romParams =>
      val rom = SerialTLROM(romParams, memParams.beatBytes)
      rom.node := TLFragmenter(memParams.beatBytes, p(CacheBlockBytes)) := xbar
    }

    xbar := clientNode
  }

  serdesser.managerNode.get := TLBuffer() := tsi2tl.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ser = Flipped(new SerialIO(tl_serdesser.w))
      val tsi = new TSIIO
      val tsi2tl_state = Output(UInt())
    })

    serdesser.module.io.ser.in <> io.ser.out
    io.ser.in <> serdesser.module.io.ser.out
    io.tsi <> tsi2tl.module.io.tsi
    io.tsi2tl_state := tsi2tl.module.io.state
  }
}
