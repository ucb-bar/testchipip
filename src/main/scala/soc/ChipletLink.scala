package testchipip.soc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._
import freechips.rocketchip.diplomacy.BundleBridgeSource

case object MaxOffchipAddressRange extends Field[Seq[AddressSet]](AddressSet.misaligned(0x100000000L, 0x1000000000L))

// Link params should extend this trait
trait ChipletLinkParams{
    //def managerRegion: Seq[AddressSet]
    def managerBusWhere: TLBusWrapperLocation // Where the link client node is connected
    def controlManagerBusWhere: Option[TLBusWrapperLocation] // Where the link control node is connected
    //def instantiationFn: _ => (TLInwardNode, TLOutwardNode, Option[TLRegisterNode])
}

case class OffchipSubsystemParams(
  val managerRegion: Seq[AddressSet],
  val clientBeatBytes: Int,
  val clientBlockBytes: Int,
  val managerBeatBytes: Int,
  val managerBlockBytes: Int
)

abstract class ChipletLinkWrapper(implicit p: Parameters) extends LazyModule {
  //val link_name: String 
  val client_node: TLClientNode
  val manager_node: TLManagerNode
  val control_manager_node: Option[TLRegisterNode]
  val clock_node: Option[ClockNode]
  val top_IO: BundleBridgeSource[ChipletIO]
}

abstract class ChipletLinkWrapperImpl(outer: ChipletLinkWrapper) extends LazyModuleImp(outer) {
  val io: ChipletIO
}

// Links should "with" this trait
trait ChipletLinkWrapperInstantiationLike {
  def instantiate(params: OffchipSubsystemParams, id: Int)(implicit p: Parameters): ChipletLinkWrapper
}

// Should this be called D2D IO instead?
trait ChipletIO extends Bundle {
    def tieoff: Unit
    def connect(io: ChipletIO): Unit
    def loopback: Unit
}

