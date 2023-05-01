package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam}

import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._

import sifive.blocks.devices.uart._
import testchipip.{SerialIO}

// This should NEVER be taped out, it should only be used on bringup FPGAs
case class UARTTSITLClientParams(
  uartParams: UARTParams = UARTParams(0),
  tlbus: TLBusWrapperLocation = FBUS
)

case object UARTTSITLClientKey extends Field[Option[UARTTSITLClientParams]](None)
trait CanHavePeripheryUARTTSITLClient { this: BaseSubsystem =>
  val uart_tsi = p(UARTTSITLClientKey).map { params =>
    val tlbus = locateTLBusWrapper(params.tlbus)
    val uartParams = params.uartParams
    val uart_bus_io = tlbus {
      val tsi2tl = LazyModule(new TSIToTileLink)
      tlbus.coupleFrom("uart_tsi") { _ := tsi2tl.node }
      InModuleBody {
        val uart_to_serial = Module(new UARTToSerial(tlbus.dtsFrequency.get, uartParams))
        val width_adapter = Module(new SerialWidthAdapter(8, TSI.WIDTH))
        tsi2tl.module.io.tsi.flipConnect(width_adapter.io.wide)
        width_adapter.io.narrow.flipConnect(uart_to_serial.io.serial)
        val uart_tsi_io = IO(new UARTPortIO(uartParams))
        uart_tsi_io <> uart_to_serial.io.uart
        uart_tsi_io
      }
    }
    InModuleBody {
      val uart_tsi_io = IO(new UARTPortIO(uartParams))
      uart_tsi_io <> uart_bus_io
      uart_tsi_io
    }
  }
}
