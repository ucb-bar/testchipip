package testchipip

import chisel3._

import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.config.{Field, Config}
import freechips.rocketchip.diplomacy.{LazyModule, AddressSet}
import freechips.rocketchip.tilelink.{TLRAM}

case class BackingScratchpadParams(
  base: BigInt,
  mask: BigInt)

case object BackingScratchpadKey extends Field[Option[BackingScratchpadParams]](None)

/**
 * Trait to add a scratchpad on the mbus
 */
trait CanHaveBackingScratchpad { this: BaseSubsystem =>
  private val portName = "Backing-Scratchpad"

  val spadOpt = p(BackingScratchpadKey).map { param =>
    val spad = mbus { LazyModule(new TLRAM(address=AddressSet(param.base, param.mask), beatBytes=mbus.beatBytes, devName=Some("backing-scratchpad"))) }
    mbus.toVariableWidthSlave(Some(portName)) { spad.node }
    spad
  }
}

class WithBackingScratchpad(base: BigInt = 0x80000000L, mask: BigInt = ((4 << 20) - 1)) extends Config((site, here, up) => {
  case BackingScratchpadKey => Some(BackingScratchpadParams(base, mask))
})
