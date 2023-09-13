package testchipip

import chisel3._

import freechips.rocketchip.subsystem._
import org.chipsalliance.cde.config.{Field, Config, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

case class BankedScratchpadParams(
  base: BigInt,
  size: BigInt,
  busWhere: TLBusWrapperLocation = SBUS,
  banks: Int = 1,
  name: String = "banked-scratchpad",
  disableMonitors: Boolean = false)

case object BankedScratchpadKey extends Field[Seq[BankedScratchpadParams]](Nil)

trait CanHaveBankedScratchpad { this: BaseSubsystem =>
  p(BankedScratchpadKey).zipWithIndex.foreach { case (params, si) =>
    val bus = locateTLBusWrapper(params.busWhere)
    val name = params.name
    val banks = params.banks
    val mask = (params.banks-1)*p(CacheBlockBytes)
    val device = new MemoryDevice

    def genBanks()(implicit p: Parameters) = (0 until banks).map { bank =>
      val ram = LazyModule(new TLRAM(
        address = AddressSet(params.base + p(CacheBlockBytes) * bank, params.size - 1 - mask),
        beatBytes = bus.beatBytes,
        devOverride = Some(device)
      ))
      bus.coupleTo(s"$name-$si-$bank") { ram.node := TLFragmenter(bus.beatBytes, p(CacheBlockBytes)) := _ }
    }

    if (params.disableMonitors) DisableMonitors { implicit p => genBanks()(p) } else genBanks()
  }
}

