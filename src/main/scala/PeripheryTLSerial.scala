package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.{IO}
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._

case class SerialTLROMParams(
  address: BigInt = 0x20000,
  size: Int = 0x10000,
  contentFileName: Option[String] = None) // If unset, generates a JALR to DRAM_BASE

case class SerialTLManagerParams(
  memParams: MasterPortParams,
  romParams: Option[SerialTLROMParams] = None,
  isMemoryDevice: Boolean = false,
  slaveWhere: TLBusWrapperLocation = OBUS
)

case class SerialTLClientParams(
  idBits: Int = 8,
  masterWhere: TLBusWrapperLocation = FBUS)

// The SerialTL can be configured to be bidirectional if serialTLManagerParams is set
case class SerialTLParams(
  client: Option[SerialTLClientParams] = None,
  manager: Option[SerialTLManagerParams] = None,
  width: Int = 4,
  provideClockFreqMHz: Option[Int] = None,
  bundleParams: TLBundleParameters = TLSerdesser.STANDARD_TLBUNDLE_PARAMS)

case object SerialTLKey extends Field[Option[SerialTLParams]](None)

trait CanHavePeripheryTLSerial { this: BaseSubsystem =>
  private val portName = "serial-tl"
  val (serdesser, serial_tl, serial_tl_debug) = p(SerialTLKey).map { params =>
    lazy val manager_bus = params.manager.map(m => locateTLBusWrapper(m.slaveWhere))
    lazy val client_bus = params.client.map(c => locateTLBusWrapper(c.masterWhere))
    val clientPortParams = params.client.map { c => TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = "serial-tl",
        sourceId = IdRange(0, 1 << c.idBits)
      ))
    ) }

    val managerPortParams = params.manager.map { m =>
      val memParams = m.memParams
      val romParams = m.romParams
      val memDevice = if (m.isMemoryDevice) new MemoryDevice else new SimpleDevice("lbwif-readwrite", Nil)
      val romDevice = new SimpleDevice("lbwif-readonly", Nil)
      val blockBytes = manager_bus.get.blockBytes
      TLSlavePortParameters.v1(
        managers = Seq(
          TLSlaveParameters.v1(
            address            = AddressSet.misaligned(memParams.base, memParams.size),
            resources          = memDevice.reg,
            regionType         = RegionType.UNCACHED, // cacheable
            executable         = true,
            supportsGet        = TransferSizes(1, blockBytes),
            supportsPutFull    = TransferSizes(1, blockBytes),
            supportsPutPartial = TransferSizes(1, blockBytes)
          )
        ) ++ romParams.map { romParams =>
          TLSlaveParameters.v1(
            address            = List(AddressSet(romParams.address, romParams.size-1)),
            resources          = romDevice.reg,
            regionType         = RegionType.UNCACHED, // cacheable
            executable         = true,
            supportsGet        = TransferSizes(1, blockBytes),
            fifoId             = Some(0)
          )
        },
        beatBytes = memParams.beatBytes
      )
    }

    val serial_tl_domain = LazyModule(new ClockSinkDomain(name=Some("serial_tl")))
    serial_tl_domain.clockNode := manager_bus.getOrElse(client_bus.get).fixedClockNode
    if (manager_bus.isDefined && client_bus.isDefined)
      require(manager_bus.get.dtsFrequency.get == client_bus.get.dtsFrequency.get)

    val serdesser = serial_tl_domain { LazyModule(new TLSerdesser(
      w = params.width,
      clientPortParams = clientPortParams,
      managerPortParams = managerPortParams,
      bundleParams = params.bundleParams
    )) }
    serdesser.managerNode.foreach { managerNode =>
      manager_bus.get.coupleTo(s"port_named_serial_tl_mem") {
        TLSourceShrinker(1 << params.manager.get.memParams.idBits) := TLWidthWidget(manager_bus.get.beatBytes) := _
      }
    }
    serdesser.clientNode.foreach { clientNode =>
      client_bus.get.coupleFrom(s"port_named_serial_tl_ctrl") { _ := TLBuffer() := clientNode }
    }


    // If we provide a clock, generate a clock domain for the outgoing clock
    val serial_tl_clock_node = params.provideClockFreqMHz.map { f =>
      serial_tl_domain { ClockSinkNode(Seq(ClockSinkParameters(take=Some(ClockParameters(f))))) }
    }
    serial_tl_clock_node.foreach(_ := ClockGroup()(p, ValName("serial_tl_clock")) := asyncClockGroupsNode)

    def serialType = params.provideClockFreqMHz.map { f =>
      new ClockedIO(new SerialIO(params.width))
    }.getOrElse {
      Flipped(new ClockedIO(Flipped(new SerialIO(params.width))))
    }

    val inner_io = serial_tl_domain { InModuleBody {
      val inner_io = IO(serialType).suggestName("serial_tl")

      serial_tl_clock_node.map { n =>
        inner_io.clock := n.in.head._1.clock
      }

      // Handle async crossing here, the off-chip clock should only drive part of the Async Queue
      // The inner reset is the same as the serializer reset
      // The outer reset is the inner reset sync'd to the outer clock
      val outer_reset = ResetCatchAndSync(inner_io.clock, serdesser.module.reset.asBool)
      val out_async = Module(new AsyncQueue(UInt(params.width.W)))
      out_async.io.enq <> BlockDuringReset(serdesser.module.io.ser.out, 4)
      out_async.io.enq_clock := serdesser.module.clock
      out_async.io.enq_reset := serdesser.module.reset
      out_async.io.deq_clock := inner_io.clock
      out_async.io.deq_reset := outer_reset

      val in_async = Module(new AsyncQueue(UInt(params.width.W)))
      in_async.io.enq <> BlockDuringReset(inner_io.bits.in, 4)
      in_async.io.enq_clock := inner_io.clock
      in_async.io.enq_reset := outer_reset
      in_async.io.deq_clock := serdesser.module.clock
      in_async.io.deq_reset := serdesser.module.reset

      inner_io.bits.out          <> out_async.io.deq
      serdesser.module.io.ser.in <> in_async.io.deq

      inner_io
    } }
    val outer_io = InModuleBody {
      val outer_io = IO(serialType).suggestName("serial_tl")
      outer_io <> inner_io
      outer_io
    }

    val inner_debug_io = serial_tl_domain { InModuleBody {
      val inner_debug_io = IO(new SerdesDebugIO).suggestName("serial_tl_debug")
      inner_debug_io := serdesser.module.io.debug
      inner_debug_io
    }}
    val outer_debug_io = InModuleBody {
      val outer_debug_io = IO(new SerdesDebugIO).suggestName("serial_tl_debug")
      outer_debug_io := inner_debug_io
      outer_debug_io
    }
    (Some(serdesser), Some(outer_io), Some(outer_debug_io))
  }.getOrElse(None, None, None)
}
