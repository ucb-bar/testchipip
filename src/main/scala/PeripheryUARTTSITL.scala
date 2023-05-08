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
  val (uart_tsi, tsi2tl_state) = p(UARTTSITLClientKey).map { params =>
    val tlbus = locateTLBusWrapper(params.tlbus)
    val uartParams = params.uartParams
    val tsi2tl = tlbus { LazyModule(new TSIToTileLink) }
    tlbus.coupleFrom("uart_tsi") { _ := tsi2tl.node }
    val uart_bus_io = tlbus { InModuleBody {
      val uart_to_serial = Module(new UARTToSerial(tlbus.dtsFrequency.get, uartParams))
      val width_adapter = Module(new SerialWidthAdapter(8, TSI.WIDTH))
      tsi2tl.module.io.tsi.flipConnect(width_adapter.io.wide)
      width_adapter.io.narrow.flipConnect(uart_to_serial.io.serial)
      val uart_tsi_io = IO(new UARTPortIO(uartParams))
      uart_tsi_io <> uart_to_serial.io.uart
      uart_tsi_io
    } }
    val tsi2tl_state_bus_io = tlbus { InModuleBody {
      val tsi2tl_state = IO(Output(UInt()))
      tsi2tl_state := tsi2tl.module.io.state
      tsi2tl_state
    } }

    val uart_tsi_io = InModuleBody {
      val uart_tsi_io = IO(new UARTPortIO(uartParams))
      uart_tsi_io <> uart_bus_io
      uart_tsi_io
    }

    val tsi2tl_state_io = InModuleBody {
      val tsi2tl_state_io = IO(Output(UInt()))
      tsi2tl_state_io := tsi2tl_state_bus_io
      tsi2tl_state_io
    }

    (uart_tsi_io, tsi2tl_state_io)
  }.unzip
}
