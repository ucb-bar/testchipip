package testchipip.soc

import chisel3._
import chisel3.util._
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
case object OffchipBusKey extends Field[SystemBusParams](SystemBusParams(1, 1)) // default settings are non-sensical

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


/*
 * This series of TL widgets works to map the innate
 * address range of a given node to another address range.
 * Consequent to whatever bus this unit is attached,
 * the diplomatic node is address by new new address region
 *
 * Exact operation is node's address + base, region size is maintained
 */
case class InwardAddressTranslator(blockRange : AddressSet, replicationBase : Option[BigInt] = None)(implicit p: Parameters) extends LazyModule {
  val module_side = replicationBase.map { base =>
    val baseRegion   = AddressSet(0, base-1)
    val replicator   = LazyModule(new RegionReplicator(ReplicatedRegion(baseRegion, baseRegion.widen(base))))
    val prefixSource = BundleBridgeSource[UInt](() => UInt(1.W))
    replicator.prefix := prefixSource
    InModuleBody { prefixSource.bundle := 0.U(1.W) } // prefix is unused for TL uncached, so this is ok
    replicator.node
  }.getOrElse { TLTempNode() }

  // val bus_side = TLFilter(TLFilter.mSelectIntersect(blockRange))(p)
  val bus_side = TLFilter(TLFilter.mSubtract(blockRange))(p)

  // module_side := bus_side

  def apply(node : TLNode) : TLNode = {
    node := module_side := bus_side
  }

  lazy val module = new LazyModuleImp(this) {}
}
