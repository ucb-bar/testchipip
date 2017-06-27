package testchipip

import chisel3._
import chisel3.util._
import config.Parameters
import diplomacy._
import junctions.{StreamIO, StreamChannel}
import regmapper.{HasRegMap, RegField}
import rocket.PAddrBits
import rocketchip.HasSystemNetworks
import uncore.tilelink2._

/* 
 * Top-level off-chip "transport" interface for SimpleNIC.
 *
 * To simplify the current implementation, we use a very simple
 * "transport" encoding that reserves one bit in each message to indicate
 * the last 64-bit chunk of a frame.
 *
 * A message carries 64 bits of data + 1 bit end of frame marker
 */
class SimpleNicExtBundle extends StreamIO(64)

trait SimpleNicControllerBundle extends Bundle {
  val send_info = Decoupled(UInt(64.W))
  val recv_info = Decoupled(UInt(64.W))
  val comp_info = Flipped(Decoupled(UInt(16.W)))
}

trait SimpleNicControllerModule extends Module with HasRegMap {
  val io: SimpleNicControllerBundle

  val qDepth = 10
  // hold (len, addr) of packets that we need to send out
  val sendQueue = Module(new Queue(UInt(64.W), qDepth))
  // hold addr of buffers we can write received packets into
  val recvQueue = Module(new Queue(UInt(64.W), qDepth))
  // hold length of received packets
  val compQueue = Module(new Queue(UInt(16.W), qDepth))

  io.send_info <> sendQueue.io.deq
  io.recv_info <> recvQueue.io.deq
  compQueue.io.enq <> io.comp_info

  interrupts(0) := compQueue.io.deq.valid

  val sendAvail = (qDepth.U - sendQueue.io.count)
  val recvAvail = (qDepth.U - recvQueue.io.count)

  regmap(
    0x00 -> Seq(RegField.w(64, sendQueue.io.enq)),
    0x08 -> Seq(RegField.w(64, recvQueue.io.enq)),
    0x10 -> Seq(RegField.r(16, compQueue.io.deq)),
    0x12 -> Seq(
      RegField.r(4, sendAvail),
      RegField.r(4, recvAvail),
      RegField.r(4, compQueue.io.count)))
}

case class SimpleNicControllerParams(address: BigInt, beatBytes: Int)

/*
 * Take commands from the CPU over TL2, expose as Queues
 */
class SimpleNicController(c: SimpleNicControllerParams)(implicit p: Parameters)
  extends TLRegisterRouter(
    c.address, "simple-nic", Seq("ucbbar,simple-nic"),
    interrupts = 1, beatBytes = c.beatBytes)(
      new TLRegBundle(c, _)    with SimpleNicControllerBundle)(
      new TLRegModule(c, _, _) with SimpleNicControllerModule)

/*
 * Send frames out
 */
class SimpleNicSendPath(implicit p: Parameters)
    extends LazyModule {
  val node = TLClientNode(TLClientParameters(
    name = "simple-nic-send", sourceId = IdRange(0, 1)))
  lazy val module = new SimpleNicSendPathModule(this)
}

class SimpleNicSendPathModule(outer: SimpleNicSendPath)
    extends LazyModuleImp(outer) {

  val io = IO(new Bundle {
    val tl = outer.node.bundleOut
    val send_info = Flipped(Decoupled(UInt(64.W)))
    val out = Decoupled(new StreamChannel(64))
  })

  val tl = io.tl(0)
  val beatBytes = tl.params.dataBits / 8
  val byteAddrBits = log2Ceil(beatBytes)
  val addrBits = p(PAddrBits) - byteAddrBits
  val lenBits = 16 - byteAddrBits
  val packlen = io.send_info.bits(63, 48)
  val packaddr = io.send_info.bits(47, 0)

  // we allow one TL request at a time to avoid tracking
  val s_idle :: s_read :: s_send :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val sendaddr = Reg(UInt(addrBits.W))
  val sendlen  = Reg(UInt(lenBits.W))

  val edge = outer.node.edgesOut(0)
  val grantqueue = Queue(tl.d, 1)

  io.send_info.ready := state === s_idle
  tl.a.valid := state === s_read
  tl.a.bits := edge.Get(
    fromSource = 0.U,
    toAddress = sendaddr << byteAddrBits.U,
    lgSize = byteAddrBits.U)._2
  io.out.valid := grantqueue.valid && state === s_send
  io.out.bits.data := grantqueue.bits.data
  io.out.bits.last := sendlen === 0.U
  grantqueue.ready := io.out.ready && state === s_send

  when (io.send_info.fire()) {
    sendaddr := packaddr >> byteAddrBits.U
    sendlen  := packlen  >> byteAddrBits.U
    state := s_read

    assert(packaddr(byteAddrBits-1,0) === 0.U &&
           packlen(byteAddrBits-1,0)  === 0.U,
           s"NIC send address and length must be aligned to ${beatBytes} bytes")
  }

  when (tl.a.fire()) {
    sendaddr := sendaddr + 1.U
    sendlen  := sendlen - 1.U
    state := s_send
  }

  when (io.out.fire()) {
    state := Mux(sendlen === 0.U, s_idle, s_read)
  }
}

class SimpleNicDoubleBuffer extends Module {
  val io = IO(new StreamIO(64))

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
        buffer(inIdx) := io.in.bits.data
      }
    }
    when (io.in.bits.last) {
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

  io.out.valid := !outDone
  io.out.bits.data := Vec(buffers.map(_.read(outIdx)))(phase)
  io.out.bits.last := outLen === 1.U
}

class SimpleNicWriter(val nXacts: Int)(implicit p: Parameters)
    extends LazyModule {
  val node = TLClientNode(TLClientParameters(
    name = "simple-nic-recv", sourceId = IdRange(0, nXacts)))
  lazy val module = new SimpleNicWriterModule(this)
}

class SimpleNicWriterModule(outer: SimpleNicWriter)
    extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val tl = outer.node.bundleOut // dma mem port
    val recv_info = Flipped(Decoupled(Bits(64.W))) // contains recv buffers
    val comp_info = Decoupled(UInt(16.W)) // finished recv lengths
    val in = Flipped(Decoupled(new StreamChannel(64)))
  })

  val tl = io.tl(0)
  val edge = outer.node.edgesOut(0)
  val beatBytes = tl.params.dataBits / 8
  val byteAddrBits = log2Ceil(beatBytes)
  val addrBits = p(PAddrBits) - byteAddrBits

  val s_idle :: s_data :: s_complete :: Nil = Enum(3)
  val state = Reg(init = s_idle)

  val base_addr = Reg(init = 0.U(addrBits.W))
  val idx = Reg(init = 0.U(addrBits.W))
  val addr_merged = base_addr + idx

  val xact_busy = Reg(init = 0.U(outer.nXacts.W))
  val xact_onehot = PriorityEncoderOH(~xact_busy)
  val can_send = !xact_busy.andR

  xact_busy := (xact_busy | Mux(tl.a.fire(), xact_onehot, 0.U)) &
                  ~Mux(tl.d.fire(), UIntToOH(tl.d.bits.source), 0.U)

  io.recv_info.ready := state === s_idle
  tl.a.valid := (state === s_data && io.in.valid) && can_send
  tl.a.bits := edge.Put(
    fromSource = OHToUInt(xact_onehot),
    toAddress = addr_merged << byteAddrBits.U,
    lgSize = byteAddrBits.U,
    data = io.in.bits.data)._2
  tl.d.ready := xact_busy.orR
  io.in.ready := state === s_data && can_send && tl.a.ready
  io.comp_info.valid := state === s_complete
  io.comp_info.bits := idx << byteAddrBits.U

  when (io.recv_info.fire()) {
    idx := 0.U
    base_addr := io.recv_info.bits >> byteAddrBits.U
    state := s_data
  }

  when (tl.a.fire()) {
    idx := idx + 1.U
    when (io.in.bits.last) { state := s_complete }
  }

  when (io.comp_info.fire()) { state := s_idle }
}

/*
 * Recv frames
 */
class SimpleNicRecvPath(nXacts: Int)(implicit p: Parameters)
    extends LazyModule {
  val writer = LazyModule(new SimpleNicWriter(nXacts))
  val node = TLOutputNode()
  node := writer.node
  lazy val module = new SimpleNicRecvPathModule(this)
}

class SimpleNicRecvPathModule(outer: SimpleNicRecvPath)
    extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val tl = outer.node.bundleOut // dma mem port
    val recv_info = Flipped(Decoupled(UInt(64.W))) // contains recv buffers
    val comp_info = Decoupled(UInt(16.W)) // finished recv lengths
    val in = Flipped(Decoupled(new StreamChannel(64))) // input stream 
  })

  val buffer = Module(new SimpleNicDoubleBuffer)
  buffer.io.in <> io.in

  val writer = outer.writer.module
  writer.io.in <> SeqQueue(buffer.io.out, 2000)
  writer.io.recv_info <> io.recv_info
  io.comp_info <> writer.io.comp_info
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
class SimpleNIC(address: BigInt, beatBytes: Int = 8, nXacts: Int = 8)
    (implicit p: Parameters) extends LazyModule {

  val control = LazyModule(new SimpleNicController(
    SimpleNicControllerParams(address, beatBytes)))
  val sendPath = LazyModule(new SimpleNicSendPath)
  val recvPath = LazyModule(new SimpleNicRecvPath(nXacts))

  val mmionode = TLInputNode()
  val dmanode = TLOutputNode()
  val intnode = IntOutputNode()

  control.node := mmionode
  dmanode := sendPath.node
  dmanode := recvPath.node
  intnode := control.intnode

  lazy val module = new LazyModuleImp(this) {

    val io = IO(new Bundle {
      val tlout = dmanode.bundleOut // move packets in/out of mem
      val tlin = mmionode.bundleIn  // commands from cpu
      val ext = new SimpleNicExtBundle
      val interrupt = intnode.bundleOut
    })

    sendPath.module.io.send_info <> control.module.io.send_info
    recvPath.module.io.recv_info <> control.module.io.recv_info
    control.module.io.comp_info <> recvPath.module.io.comp_info

    // connect externally
    recvPath.module.io.in <> io.ext.in
    io.ext.out <> sendPath.module.io.out
  }
}

trait HasPeripherySimpleNIC extends HasSystemNetworks {
  private val address = BigInt(0x10016000)

  val simplenic = LazyModule(new SimpleNIC(address, socBusConfig.beatBytes))
  simplenic.mmionode := TLFragmenter(
    socBusConfig.beatBytes, cacheBlockBytes)(socBus.node)
  fsb.node :=* simplenic.dmanode
  intBus.intnode := simplenic.intnode
}

trait HasPeripherySimpleNICModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripherySimpleNIC
  val ext = IO(new SimpleNicExtBundle)

  ext <> outer.simplenic.module.io.ext

  def connectNicLoopback(qDepth: Int = 64) {
    ext.in <> Queue(ext.out, qDepth)
  }
}

