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

// "off-chip" bus, TL bus which connects off-chip tilelink memories/devices
case object OBUS extends TLBusWrapperLocation("subsystem_obus")
case object OffchipBusKey extends Field[SystemBusParams]

case class OffchipBusTopologyParams(
  obus: SystemBusParams
) extends TLBusWrapperTopology(
  instantiations = List((OBUS, obus)),
  connections = Nil
)

// location: bus which masters this bus
// offchipOffset: add this to the address before sending it off-chip
case class OffchipBusTopologyConnectionParams(
  location: TLBusWrapperLocation,
  blockRange: Seq[AddressSet] = Nil, // offchip addresses which will not be accessible through this port
  replicationBase: Option[BigInt] = None // the offchip address region will be replicated above this address
) extends TLBusWrapperTopology(
  instantiations = Nil,
  connections = List((location, OBUS, TLBusWrapperConnection(driveClockFromMaster = Some(true))(
    inject = (q: Parameters) => {
      implicit val p: Parameters = q
      val filter = if (blockRange.isEmpty) { TLTempNode() } else {
        TLFilter(TLFilter.mSubtract(blockRange))(p)
      }
      val replicator = replicationBase.map { base =>
        val baseRegion = AddressSet(0, base-1)
        val offsetRegion = AddressSet(base, base-1)
        val replicator = LazyModule(new RegionReplicator(ReplicatedRegion(baseRegion, baseRegion.widen(base))))
        val prefixSource = BundleBridgeSource[UInt](() => UInt(1.W))
        replicator.prefix := prefixSource
        // prefix is unused for TL uncached, so this is ok
        InModuleBody { prefixSource.bundle := 0.U(1.W) }
        replicator.node
      }.getOrElse { TLTempNode() }
      replicator := filter
    }
  )))
)
