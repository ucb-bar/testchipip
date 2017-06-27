package testchipip

import chisel3._
import chisel3.util._
import coreplex.NTiles
import config.Parameters
import uncore.tilelink._
import uncore.tilelink2._
import uncore.coherence.{MESICoherence, NullRepresentation}
import tile.XLen
import junctions._
import diplomacy._
import regmapper.{HasRegMap, RegField}
import rocketchip._
import coreplex._
import _root_.util.UIntIsOneOf

/* 
 * Top-level off-chip "transport" interface for SimpleNIC.
 *
 * To simplify the current implementation, we use a very simple
 * "transport" encoding that reserves one bit in each message to indicate
 * the last 64-bit chunk of a frame.
 *
 * A message carries 64 bits of data + 1 bit end of frame marker
 */
class SimpleNicExtBundle extends Bundle {
  val bitsout = Decoupled(UInt(65.W))
  val bitsin = Flipped(Decoupled(UInt(65.W)))
}

trait SimpleNicControllerBundle extends Bundle {
  val send_info = Decoupled(UInt(64.W))
  val recv_info = Decoupled(UInt(64.W))
}

trait SimpleNicControllerModule extends Module with HasRegMap {
  val io: SimpleNicControllerBundle

  // hold (len, addr) of packets that we need to send out
  val sendQueue = Module(new Queue(UInt(64.W), 10))
  // hold addr of buffers we can write received packets into
  val recvQueue = Module(new Queue(UInt(64.W), 10))

  io.send_info <> sendQueue.io.deq
  io.recv_info <> recvQueue.io.deq

  regmap(
    0x00 -> Seq(RegField.w(64, sendQueue.io.enq)),
    0x08 -> Seq(RegField.w(64, recvQueue.io.enq)))
}

case class SimpleNicControllerParams(address: BigInt, beatBytes: Int)

/*
 * Take commands from the CPU over TL2, expose as Queues
 */
class SimpleNicController(c: SimpleNicControllerParams)(implicit p: Parameters)
  extends TLRegisterRouter(
    c.address, "simple-nic", Seq("ucbbar,simple-nic"),
    beatBytes = c.beatBytes)(
      new TLRegBundle(c, _)    with SimpleNicControllerBundle)(
      new TLRegModule(c, _, _) with SimpleNicControllerModule)

/*
 * Send frames out
 */
class SimpleNicSendPath(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    // tl
    val tl = new ClientUncachedTileLinkIO
    val send_info = Flipped(Decoupled(Bits(width=64.W)))
    val bitsout = Decoupled(Bits(width=65.W))
  })

  val lastQueue = Module(new Queue(Bool(), 10))
  val sq = io.send_info

  val dat = sq.bits
  val packlen = dat(63, 48)
  val packaddr = dat(47, 0)

  // read out the packet based on given info, send it
  // packlen is the length (in bytes)
  // packaddr is the addr (byte addr) (TODO: alignment?)

  // for dealing with TL, we want all addresses/lengths as 64 bit chunks:
  val num_tl_blocks = packlen(15, 3)
  val addr_tl = packaddr(47, 3)

  // offset into the packet as the 64bit chunk number
  val block_req_num = Reg(UInt(width=32.W), init=UInt(0))

  // computed address for use by Acquires
  val curr_addr = addr_tl + block_req_num

  // we allow one TL request at a time to avoid tracking
  val outstanding = Reg(init=Bool(false))

  val counter_reset = sq.ready
  val counter_increment = io.tl.acquire.valid & io.tl.acquire.ready

  block_req_num := Mux(counter_reset,
                       UInt(0),
                       Mux(counter_increment,
                         block_req_num + UInt(1),
                         block_req_num))

  //done
  io.tl.acquire.valid := sq.valid & !outstanding & lastQueue.io.enq.ready
  sq.ready := (block_req_num === (num_tl_blocks-UInt(1))) & !outstanding & io.tl.acquire.ready & lastQueue.io.enq.ready

  lastQueue.io.enq.bits := block_req_num === (num_tl_blocks-UInt(1))
  lastQueue.io.enq.valid := io.tl.acquire.valid & io.tl.acquire.ready

  when(io.tl.acquire.valid && io.tl.acquire.ready) {
    outstanding := Bool(true)
//    printf("Sending request for block %x, beat %x\n", curr_addr(44, 3), curr_addr(2, 0))
  }

  io.tl.acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = Acquire.getType,
    client_xact_id = UInt(0),
    addr_block = curr_addr(44, 3),
    addr_beat = curr_addr(2, 0),
    data = UInt(0),
    union = UInt(0))

  val grantqueue = Queue(io.tl.grant, 4)

  io.bitsout.valid := grantqueue.valid & lastQueue.io.deq.valid
  io.bitsout.bits := Cat(lastQueue.io.deq.bits, grantqueue.bits.data)
  when(grantqueue.valid & io.bitsout.ready & lastQueue.io.deq.valid) {
    printf("got grant data: %x\n", grantqueue.bits.data)
    printf("mark as end of frame?: %x\n", lastQueue.io.deq.bits)
    printf("encoded: %x\n", io.bitsout.bits)
    outstanding := Bool(false)
  }
  grantqueue.ready := io.bitsout.ready & lastQueue.io.deq.valid //UInt(1)
  lastQueue.io.deq.ready := io.bitsout.ready & grantqueue.valid
}

class SimpleNicDoubleBuffer extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Bits(65.W)))
    val out = Decoupled(Bits(65.W))
  })

  val buffers = Seq.fill(2) { Mem(200, Bits(64.W)) }

  val inIdx = Reg(init = 0.U(8.W))
  val outIdx = Reg(init = 0.U(8.W))
  val outLen = Reg(init = 0.U(8.W))
  val outDone = outLen === 0.U

  val phase = Reg(init = 0.U(1.W))

  io.in.ready := true.B

  when (io.in.fire()) {
    inIdx := inIdx + 1.U
    buffers.zipWithIndex.foreach { case (buffer, i) =>
      when (~phase === i.U) {
        buffer(inIdx) := io.in.bits(63, 0)
      }
    }
    when (io.in.bits(64)) {
      inIdx := 0.U
      // If writing out isn't finished, we just lose the whole frame
      when (outDone) {
        outIdx := 0.U
        outLen := inIdx + 1.U
        phase := ~phase
      }
    }
  }

  when (io.out.fire()) {
    outIdx := outIdx + 1.U
    outLen := outLen - 1.U
  }

  val last = outLen === 1.U
  val data = Vec(buffers.map(_.read(outIdx)))(phase)

  io.out.valid := !outDone
  io.out.bits := Cat(last, data)
}

class SimpleNicWriter(implicit p: Parameters) extends TLModule()(p) {
  val io = IO(new Bundle {
    val tl = new ClientUncachedTileLinkIO // dma mem port
    val recv_info = Flipped(Decoupled(Bits(64.W))) // contains recv buffers
    val bitsin = Flipped(Decoupled(Bits(65.W))) // input stream 
  })

  require(tlDataBits == 64)

  val addrBits = tlBlockAddrBits + tlBeatAddrBits

  val s_idle :: s_data :: s_zero :: s_sentinel :: Nil = Enum(4)
  val state = Reg(init = s_idle)

  val base_addr = Reg(init = 0.U(addrBits.W))
  val idx = Reg(init = 0.U(addrBits.W))
  val addr_merged = base_addr + idx
  val addr_block = addr_merged(addrBits - 1, tlBeatAddrBits)
  val addr_beat = addr_merged(tlBeatAddrBits - 1, 0)
  val put_data = MuxLookup(state, 0.U(64.W), Seq(
    s_data -> io.bitsin.bits(63, 0),
    s_sentinel -> "h10000000000000001".U)) // hacky polling sentinel

  val xact_busy = Reg(init = 0.U(tlMaxClientXacts.W))
  val xact_onehot = PriorityEncoderOH(~xact_busy)
  val gnt_xact_id = io.tl.grant.bits.client_xact_id
  val can_send = !xact_busy.andR

  xact_busy := (xact_busy | Mux(io.tl.acquire.fire(), xact_onehot, 0.U)) &
                  ~Mux(io.tl.grant.fire(), UIntToOH(gnt_xact_id), 0.U)

  io.recv_info.ready := state === s_idle
  io.tl.acquire.valid := ((state === s_data && io.bitsin.valid) ||
                          state.isOneOf(s_zero, s_sentinel)) && can_send
  io.tl.acquire.bits := Put(
    client_xact_id = OHToUInt(xact_onehot),
    addr_block = addr_block,
    addr_beat = addr_beat,
    data = put_data)
  io.tl.grant.ready := xact_busy.orR
  io.bitsin.ready := state === s_data && can_send && io.tl.acquire.ready

  when (io.recv_info.fire()) {
    idx := 0.U
    base_addr := io.recv_info.bits >> tlByteAddrBits.U
    state := s_data
  }

  when (io.tl.acquire.fire()) {
    idx := idx + 1.U
    switch (state) {
      is (s_data) {
        when (io.bitsin.bits(64)) { state := s_zero }
      }
      is (s_zero) {
        when (idx === 199.U) { state := s_sentinel }
      }
      is (s_sentinel) { state := s_idle }
    }
  }
}

/*
 * Recv frames
 */
class SimpleNicRecvPath(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    // tl
    val tl = new ClientUncachedTileLinkIO // dma mem port
    val recv_info = Flipped(Decoupled(Bits(width=64.W))) // contains recv buffers
    val bitsin = Flipped(Decoupled(Bits(width=65.W))) // input stream 
  })

  val buffer = Module(new SimpleNicDoubleBuffer)
  buffer.io.in <> io.bitsin

  val writer = Module(new SimpleNicWriter)
  writer.io.bitsin <> SeqQueue(buffer.io.out, 2000)
  writer.io.recv_info <> io.recv_info
  io.tl <> writer.io.tl
}

/* 
 * A simple NIC
 *
 * Expects ethernet frames (see below), but uses a custom transport 
 * (see ExtBundle)
 * 
 * Ethernet Frame format:
 *   8 bytes    |  6 bytes  |  6 bytes    | 2 bytes  | 46-1500B | 4 bytes
 * Preamble/SFD | Dest Addr | Source Addr | Type/Len | Data     | CRC
 * Gen by NIC   | ------------- from/to CPU --------------------| Gen by NIC
 *
 * For now, we elide the Gen by NIC components since we're talking to a 
 * custom network.
 */
class SimpleNIC(address: BigInt, beatBytes: Int = 8)(implicit p: Parameters)
    extends LazyModule {

  val control = LazyModule(new SimpleNicController(
    SimpleNicControllerParams(address, beatBytes)))
  val node = TLInputNode()
  control.node := node

  lazy val module = new LazyModuleImp(this) {

    val io = IO(new Bundle {
      val tlpacketmem = new ClientUncachedTileLinkIO // move packets in/out of mem
      val tlin = node.bundleIn  // commands from cpu
      val ext = new SimpleNicExtBundle
    })

    val tlarb = Module(new ClientUncachedTileLinkIOArbiter(2))
    io.tlpacketmem <> tlarb.io.out

    val sendPath = Module(new SimpleNicSendPath)
    tlarb.io.in(1) <> sendPath.io.tl

    val recvPath = Module(new SimpleNicRecvPath)
    tlarb.io.in(0) <> recvPath.io.tl

    sendPath.io.send_info <> control.module.io.send_info
    recvPath.io.recv_info <> control.module.io.recv_info

    // connect externally
    recvPath.io.bitsin <> io.ext.bitsin
    io.ext.bitsout <> sendPath.io.bitsout
  }
}

trait HasPeripherySimpleNIC extends HasSystemNetworks {
  private val address = BigInt(0x10016000)
  private val lineBytes = 64
  private val legacyParams = p.alterPartial({
    case TLId => "NICtoL2"
    case CacheBlockOffsetBits => log2Up(cacheBlockBytes)
    case AmoAluOperandBits => p(XLen)
    case TLKey("NICtoL2") =>
      TileLinkParameters(
        coherencePolicy = new MESICoherence(new NullRepresentation(p(NTiles))),
        nManagers = p(BankedL2Config).nBanks + 1 /* MMIO */,
        nCachingClients = 1,
        nCachelessClients = 1,
        maxClientXacts = 8,
        maxClientsPerPort = 2,
        maxManagerXacts = 8,
        dataBeats = (8 * cacheBlockBytes) / p(XLen),
        dataBits = cacheBlockBytes * 8)
  })

  val nicLegacy = diplomacy.LazyModule(new TLLegacy()(legacyParams))
  fsb.node := TLHintHandler()(nicLegacy.node)

  val simplenic = LazyModule(new SimpleNIC(
    address, socBusConfig.beatBytes)(legacyParams))
  simplenic.node := TLFragmenter(
    socBusConfig.beatBytes, cacheBlockBytes)(socBus.node)
}

trait HasPeripherySimpleNICModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripherySimpleNIC
  val ext = IO(new SimpleNicExtBundle)

  ext <> outer.simplenic.module.io.ext
  outer.nicLegacy.module.io.legacy <> outer.simplenic.module.io.tlpacketmem

  def connectNicLoopback(qDepth: Int = 64) {
    ext.bitsin <> Queue(ext.bitsout, qDepth)
  }
}

