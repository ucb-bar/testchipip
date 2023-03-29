package testchipip

import chisel3._

import freechips.rocketchip.subsystem._
import org.chipsalliance.cde.config.{Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class WithMbusScratchpad(base: BigInt = 0x80000000L, size: BigInt = (4 << 20), stripes: Int = 1, partitions: Int = 1) extends Config((site, here, up) => {
  case BankedScratchpadKey => up(BankedScratchpadKey) :+ BankedScratchpadParams(
    base, size, busWhere = MBUS, name = "mbus-scratchpad", stripes = stripes, partitions = partitions)
})

class WithSbusScratchpad(base: BigInt = 0x80000000L, size: BigInt = (4 << 20), stripes: Int = 1, partitions: Int = 1) extends Config((site, here, up) => {
  case BankedScratchpadKey => up(BankedScratchpadKey) :+ BankedScratchpadParams(
    base, size, busWhere = SBUS, name = "sbus-scratchpad", stripes = stripes, partitions = partitions)
})

// "stripes" are banks by lower bits
// "partitions" are bakns by upper bits
// total banks is stripes * partitions
case class BankedScratchpadParams(
  base: BigInt,
  size: BigInt,
  busWhere: TLBusWrapperLocation = SBUS,
  stripes: Int = 1,
  partitions: Int = 1,
  name: String = "banked-scratchpad")

case object BankedScratchpadKey extends Field[Seq[BankedScratchpadParams]](Nil)

trait CanHaveBankedScratchpad { this: BaseSubsystem =>
  p(BankedScratchpadKey).zipWithIndex.foreach { case (params, si) =>
    val bus = locateTLBusWrapper(params.busWhere)
    val name = params.name
    val mask = (params.stripes-1)*p(CacheBlockBytes) + (params.partitions-1)*(params.size / params.partitions)

    val banker = BankBinder(mask)
    val device = new MemoryDevice
    (0 until params.partitions).map { partition =>
      (0 until params.stripes).map { stripe =>
        val local = AddressSet(params.base, (params.size / (params.partitions * params.stripes)) - 1)
        val ram = LazyModule(new TLRAM(
          address = local,
          beatBytes = bus.beatBytes,
          devOverride = Some(device)
        ))
        val replicator = LazyModule(new RegionReplicator(ReplicatedRegion(local, AddressSet(params.base, params.size-1))))
        val prefix = BundleBridgeSource[UInt](() => mask.U)
        replicator.prefix := prefix
        ram.node := replicator.node := banker
      }
    }
    bus.coupleTo(Some(s"$name-$i")) { banker }
  }
}


