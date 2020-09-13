package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.IO
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.debug.HasPeripheryDebug
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.prci.{ClockSinkDomain}
import scala.math.min

case object SerialAdapter {
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

class SerialAdapter(val width: Int, sourceIds: Int = 1)(implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "serial", sourceId = IdRange(0, sourceIds))

  lazy val module = new SerialAdapterModule(this)
}

class SerialAdapterModule(outer: SerialAdapter) extends LazyModuleImp(outer) {
  val w = outer.width
  require (w > 8)
  require (w % 8 == 0)
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
}

case class SerialTSIParams(width: Int = 32)
case object SerialTSIKey extends Field[Option[SerialTSIParams]](None)

case class SerialTSIAttachParams(masterWhere: TLBusWrapperLocation = FBUS)
case object SerialTSIAttachKey extends Field[SerialTSIAttachParams](SerialTSIAttachParams())

trait CanHavePeripheryTSISerial extends HasPeripheryDebug { this: BaseSubsystem =>
  private val portName = "serial-tsi"

  val serial_tsi = p(SerialTSIKey) .map { params =>
    val tlbus = locateTLBusWrapper(p(SerialTSIAttachKey).masterWhere)

    val domain = LazyModule(new ClockSinkDomain(name=Some(portName)))
    domain.clockNode := tlbus.fixedClockNode

    val inner_io = domain {
      val adapter = LazyModule(new SerialAdapter(params.width))
      tlbus.fromPort(Some(portName))() := adapter.node
      InModuleBody {
        val inner_io = IO(new SerialIO(params.width)).suggestName("serial_tsi")
        inner_io.out <> Queue(adapter.module.io.serial.out)
        adapter.module.io.serial.in <> Queue(inner_io.in)
        inner_io
      }
    }
    val outer_io = InModuleBody {
      val outer_io = IO(new ClockedIO(new SerialIO(params.width))).suggestName("serial_tsi")
      outer_io.bits <> inner_io
      outer_io.clock := domain.module.clock
      outer_io
    }
    outer_io
  }
}

case class SerialTLParams(memParams: MemoryPortParams, width: Int = 4)
case object SerialTLKey extends Field[Option[SerialTLParams]](None)

case class SerialTLAttachParams(
  masterWhere: TLBusWrapperLocation = FBUS,
  slaveWhere: TLBusWrapperLocation = MBUS
)
case object SerialTLAttachKey extends Field[SerialTLAttachParams](SerialTLAttachParams())


trait CanHavePeripheryTLSerial { this: BaseSubsystem =>
  private val portName = "serial-tl"
  val (serdesser, serial_tl) = p(SerialTLKey).map { params =>
    val memParams = params.memParams
    val manager = locateTLBusWrapper(p(SerialTLAttachKey).slaveWhere) // The bus for which this acts as a manager
    val client = locateTLBusWrapper(p(SerialTLAttachKey).masterWhere) // The bus for which this acts as a client
    val device = new MemoryDevice
    val clientPortParams = TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = "serial-tl",
        sourceId = IdRange(0, 1)
      ))
    )
    val managerPortParams = TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address            = AddressSet.misaligned(memParams.master.base, memParams.master.size),
        resources          = device.reg,
        regionType         = RegionType.UNCACHED, // cacheable
        executable         = true,
        supportsGet        = TransferSizes(1, manager.blockBytes),
        supportsPutFull    = TransferSizes(1, manager.blockBytes),
        supportsPutPartial = TransferSizes(1, manager.blockBytes))),
      beatBytes = memParams.master.beatBytes
    )

    val domain = LazyModule(new ClockSinkDomain(name=Some(portName)))

    // TODO: currently assume manager and client buses have same clock
    domain.clockNode := manager.fixedClockNode
    val serdesser = domain { LazyModule(new TLSerdesser(
      w = params.width,
      clientPortParams = clientPortParams,
      managerPortParams = managerPortParams
    )) }
    manager.coupleTo(s"port_named_serial_tl_mem") {
      (serdesser.managerNode
        := TLBuffer()
        := TLSourceShrinker(1 << memParams.master.idBits)
        := TLWidthWidget(manager.beatBytes)
        := _ )
    }
    client.coupleFrom(s"port_named_serial_tl_ctrl") {
      ( _
        := TLBuffer(BufferParams.default)
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
      outer_io.bits <> inner_io
      outer_io.clock := domain.module.clock
      outer_io
    }
    (Some(serdesser), Some(outer_io))
  }.getOrElse(None, None)
}

class SerialRAM(w: Int, size: BigInt, base: BigInt, managerEdge: TLEdgeParameters, clientEdge: TLEdgeParameters)(implicit p: Parameters) extends LazyModule {
  val managerParams = clientEdge.slave // the managerParams are the chip-side clientParams
  val clientParams = managerEdge.master // The clientParams are the chip-side managerParams
  val adapter = LazyModule(new SerialAdapter(width=32))
  val serdesser = LazyModule(new TLSerdesser(
    w,
    clientParams,
    managerParams
  ))

  val beatBytes = p(SerialTLKey).get.memParams.master.beatBytes
  val srams = AddressSet.misaligned(base, size).map { aset =>
    LazyModule(new TLRAM(
      aset,
      beatBytes = beatBytes
    ))
  }
  val xbar = TLXbar()
  srams.foreach { s => s.node := TLBuffer() := TLFragmenter(beatBytes, p(CacheBlockBytes)) := xbar }
  xbar := serdesser.clientNode

  serdesser.managerNode := TLBuffer() := adapter.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ser = Flipped(new SerialIO(w))
      val tsi_ser = new SerialIO(32)
    })

    serdesser.module.io.ser.in <> io.ser.out
    io.ser.in <> serdesser.module.io.ser.out
    io.tsi_ser <> adapter.module.io.serial
  }
}
