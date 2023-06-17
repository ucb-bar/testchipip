package testchipip

import chisel3._

import freechips.rocketchip.subsystem._
import org.chipsalliance.cde.config.{Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

case class BankedScratchpadParams(
  base: BigInt,
  size: BigInt,
  busWhere: TLBusWrapperLocation = SBUS,
  banks: Int = 1,
  name: String = "banked-scratchpad")

case object BankedScratchpadKey extends Field[Seq[BankedScratchpadParams]](Nil)

trait CanHaveBankedScratchpad { this: BaseSubsystem =>
  p(BankedScratchpadKey).zipWithIndex.foreach { case (params, si) =>
    val bus = locateTLBusWrapper(params.busWhere)
    val name = params.name
    val banks = params.banks
    val mask = (params.banks-1)*p(CacheBlockBytes)
    val device = new MemoryDevice
    (0 until banks).map { bank =>
      val ram = LazyModule(new TLRAM(
        address = AddressSet(params.base + mask * bank, params.size - 1 - mask),
        beatBytes = bus.beatBytes,
        devOverride = Some(device)
      ))
      bus.coupleTo(s"$name-$si-$bank") { ram.node := TLFragmenter(bus.beatBytes, p(CacheBlockBytes)) := _ }
    }
  }
}

