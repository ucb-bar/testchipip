package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.{IO, DataMirror}
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.debug.HasPeripheryDebug
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.prci.{ClockSourceNode, ClockSourceParameters, ClockSinkDomain, ClockBundle, ClockBundleParameters}
import scala.math.min
import freechips.rocketchip.amba.axi4.{AXI4Bundle, AXI4SlaveNode, AXI4SlavePortParameters, AXI4SlaveParameters, AXI4UserYanker, AXI4IdIndexer}

import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}

case object SerialAdapter {
  val SERIAL_TSI_WIDTH = 32 // hardcoded in FESVR

  def asyncQueue(port: ClockedIO[SerialIO], clock: Clock, reset: Reset): SerialIO = {
    val w = port.bits.w
    // AsyncQueue needs an implicit clock/reset, but they are unused
    withClockAndReset (false.B.asClock, false.B) {
      val out_queue = Module(new AsyncQueue(UInt(w.W)))
      out_queue.io.enq <> port.bits.out
      out_queue.io.enq_clock := port.clock
      out_queue.io.enq_reset := reset.asBool
      out_queue.io.deq_clock := clock
      out_queue.io.deq_reset := reset.asBool
      val in_queue = Module(new AsyncQueue(UInt(w.W)))
      port.bits.in <> in_queue.io.deq
      in_queue.io.deq_clock := port.clock
      in_queue.io.deq_reset := reset.asBool
      in_queue.io.enq_clock := clock
      in_queue.io.enq_reset := reset.asBool
      val crossed = Wire(new SerialIO(w))
      in_queue.io.enq <> crossed.in
      crossed.out <> out_queue.io.deq
      crossed
    }
  }

  def asyncResetQueue(port: SerialIO, clock: Clock, reset: Reset): SerialIO = {
    val clocked = Wire(new ClockedIO(new SerialIO(port.w)))
    clocked.bits <> port
    clocked.clock := clock
    asyncQueue(clocked, clock, reset)
  }

  def connectHarnessRAM(serdesser: TLSerdesser, port: SerialIO, reset: Reset): SerialRAM = {
    implicit val p: Parameters = serdesser.p

    val ram = LazyModule(new SerialRAM(
      p(SerialTLKey).get.width,
      p(SerialTLKey).get.memParams,
      p(SerialTLKey).get.romParams,
      managerEdge = serdesser.managerNode.edges.in(0),
      clientEdge = serdesser.clientNode.edges.out(0)
    ))

    val module = Module(ram.module)
    module.io.ser <> port

    require(ram.serdesser.module.mergedParams == serdesser.module.mergedParams,
      "Mismatch between chip-side diplomatic params and harness-side diplomatic params:\n" +
      s"Harness-side params: ${ram.serdesser.module.mergedParams}\n" +
      s"Chip-side params: ${serdesser.module.mergedParams}")

    ram
  }

  def connectHarnessMultiClockAXIRAM(serdesser: TLSerdesser, serial_port: SerialIO, mem_clock_port: ClockBundle, reset: Reset): MultiClockSerialAXIRAM = {
    implicit val p: Parameters = serdesser.p

    val ram = LazyModule(new MultiClockSerialAXIRAM(
      p(SerialTLKey).get.width,
      p(SerialTLKey).get.memParams,
      p(SerialTLKey).get.romParams,
      p(SerialTLKey).get.axiMemOverSerialTLParams.get,
      managerEdge = serdesser.managerNode.edges.in(0),
      clientEdge = serdesser.clientNode.edges.out(0)
    ))

    val module = Module(ram.module)
    module.io.ser <> serial_port
    module.io.passthrough_clock_reset <> mem_clock_port

    require(ram.serdesser.module.mergedParams == serdesser.module.mergedParams,
      "Mismatch between chip-side diplomatic params and harness-side diplomatic params:\n" +
      s"Harness-side params: ${ram.serdesser.module.mergedParams}\n" +
      s"Chip-side params: ${serdesser.module.mergedParams}")

    ram
  }

  def connectSimSerial(serial: Option[SerialIO], clock: Clock, reset: Reset): Bool = {
    serial.map { s =>
      val sim = Module(new SimSerial(s.w))
      sim.io.clock := clock
      sim.io.reset := reset
      sim.io.serial <> s
      sim.io.exit
    }.getOrElse(false.B)
  }

  def connectSimSerial(serial: SerialIO, clock: Clock, reset: Reset): Bool = connectSimSerial(Some(serial), clock, reset)

  def tieoff(serial: Option[SerialIO]) {
    serial.foreach { s =>
      s.in.valid := false.B
      s.in.bits := DontCare
      s.out.ready := true.B
    }
  }

  def tieoff(serial: SerialIO) { tieoff(Some(serial)) }
}
import SerialAdapter._

class SerialAdapter(sourceIds: Int = 1)(implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "serial", sourceId = IdRange(0, sourceIds))

  lazy val module = new SerialAdapterModule(this)
}

class SerialAdapterModule(outer: SerialAdapter) extends LazyModuleImp(outer) {
  val w = SERIAL_TSI_WIDTH
  val io = IO(new Bundle {
    val serial = new SerialIO(w)
  })

  val (mem, edge) = outer.node.out(0)

  require (edge.manager.minLatency > 0)

  val pAddrBits = edge.bundle.addressBits
  val wordLen = 64
  val nChunksPerWord = wordLen / w
  val dataBits = mem.params.dataBits
  val beatBytes = dataBits / 8
  val nChunksPerBeat = dataBits / w
  val byteAddrBits = log2Ceil(beatBytes)

  require(nChunksPerWord > 0, s"Serial interface width must be <= $wordLen")

  val cmd = Reg(UInt(w.W))
  val addr = Reg(UInt(wordLen.W))
  val len = Reg(UInt(wordLen.W))
  val body = Reg(Vec(nChunksPerBeat, UInt(w.W)))
  val bodyValid = Reg(UInt(nChunksPerBeat.W))
  val idx = Reg(UInt(log2Up(nChunksPerBeat).W))

  val (cmd_read :: cmd_write :: Nil) = Enum(2)
  val (s_cmd :: s_addr :: s_len ::
       s_read_req  :: s_read_data :: s_read_body ::
       s_write_body :: s_write_data :: s_write_ack :: Nil) = Enum(9)
  val state = RegInit(s_cmd)

  io.serial.in.ready := state.isOneOf(s_cmd, s_addr, s_len, s_write_body)
  io.serial.out.valid := state === s_read_body
  io.serial.out.bits := body(idx)

  val beatAddr = addr(pAddrBits - 1, byteAddrBits)
  val nextAddr = Cat(beatAddr + 1.U, 0.U(byteAddrBits.W))

  val wmask = FillInterleaved(w/8, bodyValid)
  val addr_size = nextAddr - addr
  val len_size = Cat(len + 1.U, 0.U(log2Ceil(w/8).W))
  val raw_size = Mux(len_size < addr_size, len_size, addr_size)
  val rsize = MuxLookup(raw_size, byteAddrBits.U,
    (0 until log2Ceil(beatBytes)).map(i => ((1 << i).U -> i.U)))

  val pow2size = PopCount(raw_size) === 1.U
  val byteAddr = Mux(pow2size, addr(byteAddrBits - 1, 0), 0.U)

  val put_acquire = edge.Put(
    0.U, beatAddr << byteAddrBits.U, log2Ceil(beatBytes).U,
    body.asUInt, wmask)._2

  val get_acquire = edge.Get(
    0.U, Cat(beatAddr, byteAddr), rsize)._2

  mem.a.valid := state.isOneOf(s_write_data, s_read_req)
  mem.a.bits := Mux(state === s_write_data, put_acquire, get_acquire)
  mem.b.ready := false.B
  mem.c.valid := false.B
  mem.d.ready := state.isOneOf(s_write_ack, s_read_data)
  mem.e.valid := false.B

  def shiftBits(bits: UInt, idx: UInt): UInt =
    if (nChunksPerWord > 1)
      bits << Cat(idx(log2Ceil(nChunksPerWord) - 1, 0), 0.U(log2Up(w).W))
    else bits

  def addrToIdx(addr: UInt): UInt =
    if (nChunksPerBeat > 1) addr(byteAddrBits - 1, log2Up(w/8)) else 0.U

  when (state === s_cmd && io.serial.in.valid) {
    cmd := io.serial.in.bits
    idx := 0.U
    addr := 0.U
    len := 0.U
    state := s_addr
  }

  when (state === s_addr && io.serial.in.valid) {
    addr := addr | shiftBits(io.serial.in.bits, idx)
    idx := idx + 1.U
    when (idx === (nChunksPerWord - 1).U) {
      idx := 0.U
      state := s_len
    }
  }

  when (state === s_len && io.serial.in.valid) {
    len := len | shiftBits(io.serial.in.bits, idx)
    idx := idx + 1.U
    when (idx === (nChunksPerWord - 1).U) {
      idx := addrToIdx(addr)
      when (cmd === cmd_write) {
        bodyValid := 0.U
        state := s_write_body
      } .elsewhen (cmd === cmd_read) {
        state := s_read_req
      } .otherwise {
        assert(false.B, "Bad TSI command")
      }
    }
  }

  when (state === s_read_req && mem.a.ready) {
    state := s_read_data
  }

  when (state === s_read_data && mem.d.valid) {
    body := mem.d.bits.data.asTypeOf(body)
    idx := addrToIdx(addr)
    addr := nextAddr
    state := s_read_body
  }

  when (state === s_read_body && io.serial.out.ready) {
    idx := idx + 1.U
    len := len - 1.U
    when (len === 0.U) { state := s_cmd }
    .elsewhen (idx === (nChunksPerBeat - 1).U) { state := s_read_req }
  }

  when (state === s_write_body && io.serial.in.valid) {
    body(idx) := io.serial.in.bits
    bodyValid := bodyValid | UIntToOH(idx)
    when (idx === (nChunksPerBeat - 1).U || len === 0.U) {
      state := s_write_data
    } .otherwise {
      idx := idx + 1.U
      len := len - 1.U
    }
  }

  when (state === s_write_data && mem.a.ready) {
    state := s_write_ack
  }

  when (state === s_write_ack && mem.d.valid) {
    when (len === 0.U) {
      state := s_cmd
    } .otherwise {
      addr := nextAddr
      len := len - 1.U
      idx := 0.U
      bodyValid := 0.U
      state := s_write_body
    }
  }
}

class SimSerial(w: Int) extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val serial = Flipped(new SerialIO(w))
    val exit = Output(Bool())
  })

  addResource("/testchipip/vsrc/SimSerial.v")
  addResource("/testchipip/csrc/SimSerial.cc")
  addResource("/testchipip/csrc/testchip_tsi.cc")
  addResource("/testchipip/csrc/testchip_tsi.h")
}

case class AXIClockParams(
  clockFreqMHz: Double = 1000.0, // Match FireSim's 1GHz MBUS freq.
  crossingType: ClockCrossingType = AsynchronousCrossing() // Default to async crossing
)
case class AXIMemOverSerialTLClockParams(
  axiClockParams: Option[AXIClockParams] = Some(AXIClockParams()) // if set, axi port in different clk domain
) {
  def getMemFrequency(system: HasTileLinkLocations)(implicit p: Parameters): Double = {
    axiClockParams match {
      case Some(clkParams) => clkParams.clockFreqMHz * (1000 * 1000)
      case None => {
        // get the freq. from what the serial link masters
        system.locateTLBusWrapper(p(SerialTLAttachKey).masterWhere).dtsFrequency.get.toDouble
      }
    }
  }
}
case class SerialTLROMParams(
  address: BigInt = 0x20000,
  size: Int = 0x10000,
  contentFileName: Option[String] = None) // If unset, generates a JALR to DRAM_BASE

case class SerialTLParams(
  memParams: MasterPortParams,
  romParams: SerialTLROMParams = SerialTLROMParams(),
  isMemoryDevice: Boolean = false,
  width: Int = 4,
  asyncResetQueue: Boolean = false,
  axiMemOverSerialTLParams: Option[AXIMemOverSerialTLClockParams] = Some(AXIMemOverSerialTLClockParams()) // if enabled, expose axi port instead of TL RAM
)
case object SerialTLKey extends Field[Option[SerialTLParams]](None)

case class SerialTLAttachParams(
  masterWhere: TLBusWrapperLocation = FBUS,
  slaveWhere: TLBusWrapperLocation = MBUS,
  slaveCrossingType: ClockCrossingType = SynchronousCrossing()
)
case object SerialTLAttachKey extends Field[SerialTLAttachParams](SerialTLAttachParams())

trait CanHavePeripheryTLSerial { this: BaseSubsystem =>
  private val portName = "serial-tl"
  val (serdesser, serial_tl) = p(SerialTLKey).map { params =>
    val memParams = params.memParams
    val romParams = params.romParams
    val manager = locateTLBusWrapper(p(SerialTLAttachKey).slaveWhere) // The bus for which this acts as a manager
    val client = locateTLBusWrapper(p(SerialTLAttachKey).masterWhere) // The bus for which this acts as a client
    val memDevice = if (params.isMemoryDevice) new MemoryDevice else new SimpleDevice("lbwif-ram", Nil)
    val romDevice = new SimpleDevice("lbwif-rom", Nil)
    val clientPortParams = TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = "serial-tl",
        sourceId = IdRange(0, 1)
      ))
    )
    val managerPortParams = TLSlavePortParameters.v1(
      managers = Seq(
        TLSlaveParameters.v1(
          address            = AddressSet.misaligned(memParams.base, memParams.size),
          resources          = memDevice.reg,
          regionType         = RegionType.UNCACHED, // cacheable
          executable         = true,
          supportsGet        = TransferSizes(1, manager.blockBytes),
          supportsPutFull    = TransferSizes(1, manager.blockBytes),
          supportsPutPartial = TransferSizes(1, manager.blockBytes)
        ),
        TLSlaveParameters.v1(
          address            = List(AddressSet(romParams.address, romParams.size-1)),
          resources          = romDevice.reg,
          regionType         = RegionType.UNCACHED, // cacheable
          executable         = true,
          supportsGet        = TransferSizes(1, manager.blockBytes),
          fifoId             = Some(0)
        )
      ),
      beatBytes = memParams.beatBytes
    )

    // Assume we are in the same domain as our client-side binding.
    val domain = LazyModule(new ClockSinkDomain(name=Some(portName)))
    domain.clockNode := client.fixedClockNode

    val serdesser = domain { LazyModule(new TLSerdesser(
      w = params.width,
      clientPortParams = clientPortParams,
      managerPortParams = managerPortParams
    )) }
    manager.coupleTo(s"port_named_serial_tl_mem") {
      ((domain.crossIn(serdesser.managerNode)(ValName("TLSerialManagerCrossing")))(p(SerialTLAttachKey).slaveCrossingType)
        := TLSourceShrinker(1 << memParams.idBits)
        := TLWidthWidget(manager.beatBytes)
        := _ )
    }
    client.coupleFrom(s"port_named_serial_tl_ctrl") {
      ( _
        := TLBuffer()
        := serdesser.clientNode
      )
    }

    val inner_io = domain { InModuleBody {
      val inner_io = IO(new SerialIO(params.width)).suggestName("serial_tl")
      inner_io.out <> serdesser.module.io.ser.out
      serdesser.module.io.ser.in <> inner_io.in
      inner_io
    } }
    val outer_io = InModuleBody {
      val outer_io = IO(new ClockedIO(new SerialIO(params.width))).suggestName("serial_tl")
      val ser: SerialIO = if (params.asyncResetQueue) {
        SerialAdapter.asyncResetQueue(inner_io, domain.module.clock, domain.module.reset)
      } else {
        inner_io
      }
      outer_io.bits <> ser
      outer_io.clock := domain.module.clock
      outer_io
    }
    (Some(serdesser), Some(outer_io))
  }.getOrElse(None, None)
}

object SerialTLROM {
  def apply(romParams: SerialTLROMParams, beatBytes: Int)(implicit p: Parameters): TLROM = {
    lazy val romContents = {
      val romData = romParams.contentFileName.map(n => Files.readAllBytes(Paths.get(n))).getOrElse(
        Array(
          0x1b, 0x05, 0x10, 0x00, // 0010051b     addiw    a0,zero,1
          0x13, 0x15, 0xf5, 0x01, // 01f51513     slli     a0,a0,0x1f (li a0, 0x8000_0000)
          0x73, 0x10, 0x15, 0x34, // 34151073     csrw     mepc,a0
          0x37, 0x25, 0x00, 0x00, // 00002537     lui      a0,0x2
          0x1b, 0x05, 0x05, 0x80, // 8005051b     addiw    a0,a0,-2048
          0x73, 0x20, 0x05, 0x30, // 30052073     csrs     mstatus,a0
          0x73, 0x25, 0x40, 0xf1, // f1402573     csrr     a0,mhartid
          0x73, 0x00, 0x20, 0x30  // 30200073     mret
        ).map(_.toByte)
      )
      val rom = ByteBuffer.wrap(romData)
      rom.array()
    }
    val rom = LazyModule(new TLROM(romParams.address, romParams.size, romContents, true, beatBytes))
    rom
  }
}

class SerialRAM(
  w: Int,
  memParams: MasterPortParams,
  romParams: SerialTLROMParams,
  managerEdge: TLEdgeParameters,
  clientEdge: TLEdgeParameters)(implicit p: Parameters) extends LazyModule {
  val managerParams = clientEdge.slave // the managerParams are the chip-side clientParams
  val clientParams = managerEdge.master // The clientParams are the chip-side managerParams
  val adapter = LazyModule(new SerialAdapter)
  val serdesser = LazyModule(new TLSerdesser(
    w,
    clientParams,
    managerParams
  ))

  val srams = AddressSet.misaligned(memParams.base, memParams.size).map { aset =>
    LazyModule(new TLRAM(
      aset,
      beatBytes = memParams.beatBytes
    ))
  }

  val rom = SerialTLROM(romParams, memParams.beatBytes)

  val xbar = TLXbar()
  srams.foreach { s => s.node := TLBuffer() := TLFragmenter(memParams.beatBytes, p(CacheBlockBytes)) := xbar }
  rom.node := TLFragmenter(memParams.beatBytes, p(CacheBlockBytes)) := xbar
  xbar := serdesser.clientNode

  serdesser.managerNode := TLBuffer() := adapter.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ser = Flipped(new SerialIO(w))
      val tsi_ser = new SerialIO(SERIAL_TSI_WIDTH)
    })

    serdesser.module.io.ser.in <> io.ser.out
    io.ser.in <> serdesser.module.io.ser.out
    io.tsi_ser <> adapter.module.io.serial
  }
}

class MultiClockSerialAXIRAM(
  w: Int,
  memParams: MasterPortParams,
  romParams: SerialTLROMParams,
  axiMemOverSerialTLParams: AXIMemOverSerialTLClockParams,
  managerEdge: TLEdgeParameters,
  clientEdge: TLEdgeParameters)(implicit p: Parameters) extends LazyModule {

  // setup serdes and serial adapter
  val managerParams = clientEdge.slave // the managerParams are the chip-side clientParams
  val clientParams = managerEdge.master // The clientParams are the chip-side managerParams
  val adapter = LazyModule(new SerialAdapter)
  val serdesser = LazyModule(new TLSerdesser(
    w,
    clientParams,
    managerParams
  ))

  // connect the serial adapter to serdes manager
  serdesser.managerNode := TLBuffer() := adapter.node

  val memClkRstDomain = LazyModule(new ClockSinkDomain(name=Some("mem-over-serialtl-domain")))
  val memClkRstSource = ClockSourceNode(Seq(ClockSourceParameters()))
  memClkRstDomain.clockNode := memClkRstSource

  val memCrossing = axiMemOverSerialTLParams.axiClockParams match {
    case Some(params) => {
      params.crossingType
    }
    case None => {
      SynchronousCrossing()
    }
  }

  val memXbar = memClkRstDomain { TLXbar() }
  val rom = memClkRstDomain { SerialTLROM(romParams, memParams.beatBytes) }
  memClkRstDomain {
    rom.node := TLFragmenter(memParams.beatBytes, p(CacheBlockBytes)) := memXbar
  }
  (memClkRstDomain.crossIn(memXbar)(ValName("MemPortCrossing")))(memCrossing) := serdesser.clientNode

  // TODO: Currently only supports single-channel memory
  val memPortParamsOpt = Some(MemoryPortParams(memParams, 1))
  val portName = "axi4"
  val device = new MemoryDevice
  val idBits = memPortParamsOpt.map(_.master.idBits).getOrElse(1)
  val memBusParams = p(MemoryBusKey)

  val memNode = memClkRstDomain {
    AXI4SlaveNode(memPortParamsOpt.map({ case MemoryPortParams(memPortParams, nMemoryChannels) =>
      Seq.tabulate(nMemoryChannels) { channel =>
        val base = AddressSet.misaligned(memPortParams.base, memPortParams.size)
        val filter = AddressSet(channel * memBusParams.blockBytes, ~((nMemoryChannels-1) * memBusParams.blockBytes))

        AXI4SlavePortParameters(
          slaves = Seq(AXI4SlaveParameters(
            address       = base.flatMap(_.intersect(filter)),
            resources     = device.reg,
            regionType    = RegionType.UNCACHED, // cacheable
            executable    = true,
            supportsWrite = TransferSizes(1, memBusParams.blockBytes),
            supportsRead  = TransferSizes(1, memBusParams.blockBytes),
            interleavedId = Some(0))), // slave does not interleave read responses
          beatBytes = memPortParams.beatBytes)
      }
    }).toList.flatten)
  }

  val memPort = memClkRstDomain {
    // connect axi mem to axi widgets to mem xbar
    (memNode
      :*= AXI4UserYanker()
      :*= AXI4IdIndexer(idBits)
      :*= TLToAXI4()
      :*= TLWidthWidget(memBusParams.beatBytes)
      :*= memXbar)

    InModuleBody { memNode.makeIOs() }
  }

  val mem_axi4 = InModuleBody {
    val ports: Seq[ClockedAndResetIO[AXI4Bundle]] = memPort.zipWithIndex.map({ case (m, i) =>
      val port = IO(new ClockedAndResetIO(DataMirror.internal.chiselTypeClone[AXI4Bundle](m))).suggestName(s"axi4_mem_${i}")
      port.bits <> m
      port.clock := memClkRstSource.out.head._1.clock
      port.reset := memClkRstSource.out.head._1.reset
      port
    })
    ports
  }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ser = Flipped(new SerialIO(w))
      val tsi_ser = new SerialIO(SERIAL_TSI_WIDTH)
      val passthrough_clock_reset = Flipped(new ClockBundle(ClockBundleParameters()))
    })

    // setup clock domain
    axiMemOverSerialTLParams.axiClockParams match {
      case Some(params) => {
        // setup the clock domain to be the passthrough clock
        memClkRstSource.out.head._1 <> io.passthrough_clock_reset
      }
      case None => {
        // connect to implicit clock/reset
        memClkRstSource.out.head._1.clock <> clock
        memClkRstSource.out.head._1.reset <> reset
      }
    }

    // connect the serdes and serial adapter in the serdes clock domain
    serdesser.module.io.ser.in <> io.ser.out
    io.ser.in <> serdesser.module.io.ser.out
    io.tsi_ser <> adapter.module.io.serial
  }
}
