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

// "off-chip" bus, bus which connects to serialized-tl port
case object OBUS extends TLBusWrapperLocation("subsystem_obus")
case object OffchipBusKey extends Field[SystemBusParams]


case class OffchipBusTopologyParams(
  location: TLBusWrapperLocation,
  obus: SystemBusParams
) extends TLBusWrapperTopology(
  instantiations = List((OBUS, obus)),
  connections = List((location, OBUS, TLBusWrapperConnection(driveClockFromMaster = Some(true))()))
)

