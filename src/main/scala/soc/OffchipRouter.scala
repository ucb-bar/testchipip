package testchipip.soc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.debug.HasPeripheryDebug
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._
import freechips.rocketchip.regmapper._
import testchipip.ctc.CTCBridgeIO // TODO: remove this later
import scala.math.min
import testchipip.util.{TLSwitch}

case class ChipletRoutingParams(
    clientBusWhere: TLBusWrapperLocation = SBUS,
    controlBusWhere: TLBusWrapperLocation = CBUS,
    routerParams: OffchipRouterParams,
    translationParams: InwardAddressTranslatorParams,
    ports: Seq[ChipletLinkParams]
)

case object ChipletRoutingKey extends Field[Option[ChipletRoutingParams]](None)

case class OffchipRouterParams(
  // Need to think about what I want here
  tableAddress: BigInt = 0x4000L,
  tableEntries: Int,
  beatBytes: Int // maybe not here
)

class OffchipRouter(val params: OffchipRouterParams, val nPorts: Int, val offset: BigInt)(implicit p: Parameters) extends LazyModule {

  // BORROWED FROM TLSwitch.scala START
  // This function can handle simple cases only
  def unifyManagers(mgrs: Seq[Seq[TLSlaveParameters]]): Seq[TLSlaveParameters] = {
    mgrs.flatten.groupBy(_.sortedAddress.head).map { case (_, m) =>
      require(m.forall(_.address == m.head.address), "Require homogeneous address ranges")
      require(m.forall(_.regionType == m.head.regionType), "Require homogeneous regionType")
      require(m.forall(_.supports == m.head.supports), "Require homogeneous supported operations")
      m.head
    }.toSeq
  }

  val node = new TLNexusNode(
    clientFn = { c =>
      require(c.size == 1, s"Only one ClientPort supported in TLSwitch, not $c")
      c.head
    },
    managerFn = { m =>
      // unifies all the managers, its up to the user to be careful here
      // TODO: Use bus error device to report problems?
      require(m.flatMap(_.responseFields).size == 0, "ResponseFields not supported in TLSwitch")
      require(m.flatMap(_.requestKeys).size == 0, "RequestKeys not supported in TLSwitch")
      require(m.forall(_.beatBytes == m.head.beatBytes), "Homogeneous beatBytes required")
      TLSlavePortParameters.v1(
        beatBytes = m.head.beatBytes,
        managers = unifyManagers(m.map(_.sortedSlaves)),
        endSinkId = m.map(_.endSinkId).max,
        minLatency = m.map(_.minLatency).min,
        responseFields = Nil,
        requestKeys = Nil,
      )
    }
  )
  // BORROWED FROM TLSwitch.scala END

  // Register node for setting the routing table
  val routing_table_node = TLRegisterNode(
    address = AddressSet.misaligned(params.tableAddress, 0x1000), // TODO: fix size
    device = new SimpleDevice("routing-table", Nil),
    beatBytes = params.beatBytes
  )

  override lazy val module = new OffchipRouterImpl(this)
}

class OffchipRouterImpl(outer: OffchipRouter) extends LazyModuleImp(outer) {

  val io = IO(new Bundle {
    val chip_id = Output(UInt(log2Ceil(outer.params.tableEntries).W))
  })

  val (bundleIn, edgeIn) = outer.node.in(0)
  val bundlesOut = outer.node.out.map(_._1)

  val portWidth = if (outer.nPorts == 1) 1 else log2Ceil(outer.nPorts)
  val idWidth = log2Ceil(outer.params.tableEntries + 1) // But maybe we want to allow this to be configurable, in case chip ids don't count up from 0
  val addressWidth = bundleIn.a.bits.address.getWidth // TODO: check
  val totalWidth = idWidth + addressWidth + portWidth + 1
  assert(totalWidth <= 64, "Total width of routing table entry must be less than or equal to 64 bits") // for now :)

  val chipIdReg = RegInit(0.U(idWidth.W))
  io.chip_id := chipIdReg

  // Create a memory mapped table of offchip addresses which store chip id, base address, and port to route to
  // and initialize it to all 0s using RegInit
  //val routing_table = VecInit(Seq.fill(outer.params.tableEntries)(RegInit(0.U.asTypeOf(new RoutingTableEntry(idWidth, addressWidth, portWidth)))))

  val routing_table = RegInit(VecInit(Seq.fill(outer.params.tableEntries)(0.U.asTypeOf(new RoutingTableEntry(idWidth, addressWidth, portWidth)))))
  
  // One entry = 4 separate regs (valid, chipID, baseAddress, port)
  val regsPerEntry  = 4 // TODO: fixy

  val mapped_entries = (0 until outer.params.tableEntries).flatMap { i =>
    val base = i * regsPerEntry
    Seq(
      (base + 0) -> Seq(RegField(1,           routing_table(i).valid)),
      (base + 1) -> Seq(RegField(idWidth,     routing_table(i).chipID)),
      (base + 2) -> Seq(RegField(portWidth,   routing_table(i).port)),
    )
  }
  
  val chip_id = Seq((outer.params.tableEntries * regsPerEntry) -> Seq(RegField(idWidth, chipIdReg)))
  outer.routing_table_node.regmap(mapped_entries ++ chip_id :_*)

  // Select offchip port from routing table
  val addr_top_bits = bundleIn.a.bits.address(addressWidth-1, log2Ceil(outer.offset)) 
  val matches = Wire(Vec(outer.params.tableEntries, Bool()))
  val sel = Wire(UInt(log2Ceil(outer.nPorts).W))
  sel := 0.U
  matches := VecInit(Seq.fill(outer.params.tableEntries)(false.B))
  for (i <- 0 until outer.params.tableEntries) {
    when (routing_table(i).valid && (addr_top_bits === routing_table(i).chipID)) { 
      sel := routing_table(i).port
      matches(i) := true.B
    }
  }

  // Assert that only one match is found
  assert(!bundleIn.a.valid || ((matches.asUInt & (matches.asUInt - 1.U)) === 0.U), "Multiple matches found in routing table") // TODO: check

  // AGENT OUTPUT START ~~~~~~~~~~~~~~~~
  // Decide legality per-request. Address is stable across beats.
  val illegal = Wire(Bool()) // <- your programmable table miss / tag mismatch
  illegal := bundleIn.a.valid && !matches.reduce(_ || _)

  val errActive = RegInit(false.B)
  val errOpcode = Reg(bundleIn.a.bits.opcode.cloneType)
  val errSize   = Reg(bundleIn.a.bits.size.cloneType)
  val errSource = Reg(bundleIn.a.bits.source.cloneType)

  // Helpers
  val a_last = edgeIn.last(bundleIn.a)

  // Start an error response when we accept the LAST beat of an illegal request
  when (!errActive && bundleIn.a.fire && a_last && illegal) {
    errActive := true.B
    errOpcode := bundleIn.a.bits.opcode
    errSize   := bundleIn.a.bits.size
    errSource := bundleIn.a.bits.source
  }

  // Local D generator
  val dErr = Wire(chiselTypeOf(bundleIn.d))
  val (_, d_last, _) = edgeIn.firstlast(dErr)

  dErr.valid := errActive
  dErr.bits.opcode  := TLMessages.adResponse(errOpcode)
  dErr.bits.param   := 0.U
  dErr.bits.size    := errSize
  dErr.bits.source  := errSource
  dErr.bits.sink    := 0.U
  dErr.bits.denied  := true.B
  dErr.bits.data    := 0.U
  dErr.bits.corrupt := edgeIn.hasData(dErr.bits)

  // Done when last D beat fires
  when (dErr.fire && d_last) { errActive := false.B }

  // AGENT OUTPUT END ~~~~~~~~~~~~~~~~

  // BORROWED FROM TLSwitch.scala START
  bundlesOut.zipWithIndex.foreach { case (out, i) =>
    val selected = i.U === sel

    out.a.valid := bundleIn.a.valid && selected
    out.a.bits := bundleIn.a.bits

    out.c.valid := bundleIn.c.valid && selected
    out.c.bits  := bundleIn.c.bits

    out.e.valid := bundleIn.e.valid && selected
    out.e.bits  := bundleIn.e.bits
  }

  bundleIn.a.ready := VecInit(bundlesOut.map(_.a.ready))(sel) && !errActive
  bundleIn.c.ready := VecInit(bundlesOut.map(_.c.ready))(sel)
  bundleIn.e.ready := VecInit(bundlesOut.map(_.e.ready))(sel)
  // BORROWED FROM TLSwitch.scala END

  // Arbitrate responses from d channels
  val response_arbiter_d = Module(new RRArbiter(chiselTypeOf(bundleIn.d.bits), outer.nPorts + 1))
  for (i <- 0 until outer.nPorts) {
    response_arbiter_d.io.in(i) <> bundlesOut(i).d
  }
  response_arbiter_d.io.in(outer.nPorts) <> dErr
  bundleIn.d <> Queue(response_arbiter_d.io.out, outer.nPorts + 1) // I think this queue is necessary??

  // Arbitrate responses from b channels
  // TODO ?? : Error checking for coherent requests
  val response_arbiter_b = Module(new RRArbiter(chiselTypeOf(bundleIn.b.bits), outer.nPorts))
  for (i <- 0 until outer.nPorts) {
    response_arbiter_b.io.in(i) <> bundlesOut(i).b
  }
  bundleIn.b <> Queue(response_arbiter_b.io.out, outer.nPorts)

}

// TODO: fix widths
class RoutingTableEntry(idWidth: Int, addressWidth: Int, portWidth: Int) extends Bundle {
  val valid = Bool()
  val chipID = UInt(idWidth.W) // This is probably determined by the number of entries
  val port = UInt(portWidth.W)
}

// Mostly borrowed from the Radiance AddressRewriterNode
case class ChipletAddressTranslator(val params: InwardAddressTranslatorParams, val idWidth: Int)(implicit p: Parameters) extends LazyModule {

  val node = TLAdapterNode(clientFn = c => c, managerFn = m => m)

  assert(params.offset.bitCount == 1, "Offset must only have 1 bit set") // TODO: should we enforce this in the spec?

  override lazy val module = new ChipletAddressTranslatorImpl(this)
}

class ChipletAddressTranslatorImpl(outer: ChipletAddressTranslator) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val chip_id = Input(UInt(outer.idWidth.W))
  })

  // If the top bits of the address match the chip id, then trim those bits off
  // Top bits are determined by the offset in the params
  def trimTag(address: UInt) = {
    val topBits = log2Ceil(outer.params.offset)
    val addressWidth = address.getWidth
    val tagMatch = address(addressWidth-1, topBits) === io.chip_id
    val mask = (1.U(addressWidth.W) << topBits) - 1.U
    Mux(tagMatch, address & mask, address)
  }

  (outer.node.in.map(_._1) zip outer.node.out.map(_._1)).foreach { case (in, out) =>
    out.a <> in.a
    out.a.bits.address := trimTag(in.a.bits.address)
    in.b <> out.b
    in.b.bits.address := trimTag(out.b.bits.address)
    out.c <> in.c
    out.c.bits.address := trimTag(in.c.bits.address)
    in.d <> out.d
    out.e <> in.e
  }
}  

trait CanHaveChipletRouting { this: BaseSubsystem =>
  val d2d_port_ios = p(ChipletRoutingKey).map { params => 

    val cbus = locateTLBusWrapper(params.controlBusWhere)
    val client_bus = locateTLBusWrapper(params.clientBusWhere)

    val all_freqs = { 
      Seq(client_bus.dtsFrequency) ++ 
      params.ports.map { pP => locateTLBusWrapper(pP.managerBusWhere).dtsFrequency } ++ 
      Seq(cbus.dtsFrequency) ++ 
      params.ports.flatMap { pP =>
        pP.controlManagerBusWhere match {
          case Some(where) => Some(locateTLBusWrapper(where).dtsFrequency)
          case None => None
        }
      }
    }
    require(all_freqs.forall(_.isDefined), "All buses must provide a frequency")
    require(all_freqs.forall(_ == all_freqs.head), "All buses must have the same frequency")

    val router_domain = LazyModule(new ClockSinkDomain(name=Some("offchip_router")))
    router_domain.clockNode := client_bus.fixedClockNode

    val router = router_domain { LazyModule(new OffchipRouter(params.routerParams, params.ports.size, params.translationParams.offset)) }

    // The bus drives the router's manager node
    client_bus.coupleTo(s"offchip_router") { router.node := TLBuffer() := _ }

    router.routing_table_node := cbus.coupleTo(s"offchip_router_mmio") { TLBuffer() := TLFragmenter(cbus) := _ }

    val port_ios = params.ports.zipWithIndex.map { case (pP, id) =>
      val link_manager_bus = locateTLBusWrapper(pP.managerBusWhere)

      val port = router_domain { pP.asInstanceOf[ChipletLinkWrapperInstantiationLike].instantiate(s"d2d${id}", id)(p) }

      // TODO: Translator should take in chip ID as an IO
      val translator = router_domain {
        LazyModule(ChipletAddressTranslator(params.translationParams, log2Ceil(params.routerParams.tableEntries))(p))
      }
      
      router_domain {
        InModuleBody {
          translator.module.io.chip_id := router.module.io.chip_id
        }
      }

      port.manager_node :*= router.node
      port.control_manager_node.foreach { node =>
        cbus.coupleTo(s"${port.name}_control") { node := TLBuffer() := _ }
      }
      link_manager_bus.coupleFrom(s"${port.name}") { _ := TLBuffer() := translator.node :=* port.client_node }

      val port_ioSink = BundleBridgeSink[ChipletIO]()
      port_ioSink := port.top_IO

      val outer_io = InModuleBody {
        // port_ioSink.makeIO(s"d2d${id}_port")
        val outer_io = IO(chiselTypeOf(port_ioSink.in(0)._1)).suggestName(s"d2d${id}_port")
        outer_io <> port_ioSink.in(0)._1
        outer_io
      }
      outer_io

    }
    port_ios
  }
}

class WithChipletRouting(params: ChipletRoutingParams) extends Config((site, here, up) => {
  case ChipletRoutingKey => Some(params)
})
