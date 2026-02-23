package testchipip.soc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._
import freechips.rocketchip.diplomacy.BundleBridgeSource

// Link params should extend this trait
trait ChipletLinkParams{
    def managerRegion: Seq[AddressSet]
    def managerBusWhere: TLBusWrapperLocation // Where the link client node is connected
    def controlManagerBusWhere: Option[TLBusWrapperLocation] // Where the link control node is connected
    //def instantiationFn: _ => (TLInwardNode, TLOutwardNode, Option[TLRegisterNode])
}

abstract class ChipletLinkWrapper(implicit p: Parameters) extends LazyModule {
  //val link_name: String 
  val client_node: TLClientNode
  val manager_node: TLManagerNode
  val control_manager_node: Option[TLRegisterNode]
  val top_IO: BundleBridgeSource[ChipletIO]
}

abstract class ChipletLinkWrapperImpl(outer: ChipletLinkWrapper) extends LazyModuleImp(outer) {
  val io: ChipletIO
}

// Links should "with" this trait
trait ChipletLinkWrapperInstantiationLike {
  def instantiate(name: String, id: Int)(implicit p: Parameters): ChipletLinkWrapper
}

// Should this be called D2D IO instead?
trait ChipletIO extends Bundle {
    def tieoff: Unit
    def connect(io: ChipletIO): Unit
    def loopback: Unit
}

