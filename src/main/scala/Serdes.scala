package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import freechips.rocketchip.util.HellaPeekingArbiter
import freechips.rocketchip.tilelink._

class SerialIO(w: Int) extends Bundle {
  val in = Flipped(Decoupled(UInt(w.W)))
  val out = Decoupled(UInt(w.W))

  def flipConnect(other: SerialIO) {
    in <> other.out
    other.in <> out
  }

  override def cloneType = new SerialIO(w).asInstanceOf[this.type]
}

class ValidSerialIO(w: Int) extends Bundle {
  val in = Flipped(Valid(UInt(w.W)))
  val out = Valid(UInt(w.W))

  def flipConnect(other: ValidSerialIO) {
    in <> other.out
    other.in <> out
  }

  override def cloneType = new ValidSerialIO(w).asInstanceOf[this.type]
}

class StreamChannel(val w: Int) extends Bundle {
  val data = UInt(w.W)
  val keep = UInt((w/8).W)
  val last = Bool()

  override def cloneType = new StreamChannel(w).asInstanceOf[this.type]
}

class StreamIO(w: Int) extends Bundle {
  val in = Flipped(Decoupled(new StreamChannel(w)))
  val out = Decoupled(new StreamChannel(w))

  def flipConnect(other: StreamIO) {
    in <> other.out
    other.in <> out
  }

  override def cloneType = new StreamIO(w).asInstanceOf[this.type]
}

class StreamNarrower(inW: Int, outW: Int) extends Module {
  require(inW > outW)
  require(inW % outW == 0)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StreamChannel(inW)))
    val out = Decoupled(new StreamChannel(outW))
  })

  val outBytes = outW / 8
  val outBeats = inW / outW

  val bits = Reg(new StreamChannel(inW))
  val count = Reg(UInt(log2Ceil(outBeats).W))

  val s_recv :: s_send :: Nil = Enum(2)
  val state = RegInit(s_recv)

  val nextData = bits.data >> outW.U
  val nextKeep = bits.keep >> outBytes.U

  io.in.ready := state === s_recv
  io.out.valid := state === s_send
  io.out.bits.data := bits.data(outW - 1, 0)
  io.out.bits.keep := bits.keep(outBytes - 1, 0)
  io.out.bits.last := bits.last && !nextKeep.orR

  when (io.in.fire()) {
    count := (outBeats - 1).U
    bits := io.in.bits
    state := s_send
  }

  when (io.out.fire()) {
    count := count - 1.U
    bits.data := nextData
    bits.keep := nextKeep
    when (io.out.bits.last || count === 0.U) {
      state := s_recv
    }
  }
}

class StreamWidener(inW: Int, outW: Int) extends Module {
  require(outW > inW)
  require(outW % inW == 0)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StreamChannel(inW)))
    val out = Decoupled(new StreamChannel(outW))
  })

  val inBytes = inW / 8
  val inBeats = outW / inW

  val data = Reg(Vec(inBeats, UInt(inW.W)))
  val keep = RegInit(Vec(Seq.fill(inBeats)(0.U(inBytes.W))))
  val last = Reg(Bool())

  val idx = RegInit(0.U(log2Ceil(inBeats).W))

  val s_recv :: s_send :: Nil = Enum(2)
  val state = RegInit(s_recv)

  io.in.ready := state === s_recv
  io.out.valid := state === s_send
  io.out.bits.data := data.asUInt
  io.out.bits.keep := keep.asUInt
  io.out.bits.last := last

  when (io.in.fire()) {
    idx := idx + 1.U
    data(idx) := io.in.bits.data
    keep(idx) := io.in.bits.keep
    when (io.in.bits.last || idx === (inBeats - 1).U) {
      last := io.in.bits.last
      state := s_send
    }
  }

  when (io.out.fire()) {
    idx := 0.U
    keep.foreach(_ := 0.U)
    state := s_recv
  }
}

object StreamWidthAdapter {
  def apply(out: DecoupledIO[StreamChannel], in: DecoupledIO[StreamChannel]) {
    if (out.bits.w > in.bits.w) {
      val widener = Module(new StreamWidener(in.bits.w, out.bits.w))
      widener.io.in <> in
      out <> widener.io.out
    } else if (out.bits.w < in.bits.w) {
      val narrower = Module(new StreamNarrower(in.bits.w, out.bits.w))
      narrower.io.in <> in
      out <> narrower.io.out
    } else {
      out <> in
    }
  }

  def apply(a: StreamIO, b: StreamIO) {
    apply(a.out, b.out)
    apply(b.in, a.in)
  }
}

class ValidStreamIO(w: Int) extends Bundle {
  val in = Flipped(Valid(new StreamChannel(w)))
  val out = Valid(new StreamChannel(w))

  def flipConnect(other: ValidStreamIO) {
    in <> other.out
    other.in <> out
  }

  override def cloneType =
    new ValidStreamIO(w).asInstanceOf[this.type]
}

class GenericSerializer[T <: Data](t: T, w: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(t))
    val out = Decoupled(UInt(w.W))
  })

  val dataBits = t.getWidth
  val dataBeats = (dataBits - 1) / w + 1
  val data = Reg(UInt(dataBits.W))

  val sending = RegInit(false.B)
  val (sendCount, sendDone) = Counter(io.out.fire(), dataBeats)

  io.in.ready := !sending
  io.out.valid := sending
  io.out.bits := data(w-1, 0)

  when (io.in.fire()) {
    data := io.in.bits.asUInt
    sending := true.B
  }

  when (io.out.fire()) { data := data >> w.U }

  when (sendDone) { sending := false.B }
}

class GenericDeserializer[T <: Data](t: T, w: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(w.W)))
    val out = Decoupled(t)
  })

  val dataBits = t.getWidth
  val dataBeats = (dataBits - 1) / w + 1
  val data = Reg(Vec(dataBeats, UInt(w.W)))

  val receiving = RegInit(true.B)
  val (recvCount, recvDone) = Counter(io.in.fire(), dataBeats)

  io.in.ready := receiving
  io.out.valid := !receiving
  io.out.bits := t.fromBits(data.asUInt)

  when (io.in.fire()) {
    data(recvCount) := io.in.bits
  }

  when (recvDone) { receiving := false.B }

  when (io.out.fire()) { receiving := true.B }
}

class TLMergedBundle(params: TLBundleParameters) extends TLBundleBase(params) {
  val chanId = UInt(3.W)
  val opcode = UInt(3.W)
  val param = UInt(Seq(
    TLAtomics.width, TLHints.width,
    TLPermissions.aWidth, TLPermissions.bdWidth, TLPermissions.cWidth).max.W)
  val size = UInt(params.sizeBits.W)
  val source = UInt(params.sourceBits.W)
  val address = UInt(params.addressBits.W)
  val data = UInt(params.dataBits.W)
  // either mask or sink+error
  val union = UInt(Seq(params.dataBits/8, params.sinkBits + 1).max.W)
  val last = Bool()

  def isA(dummy: Int = 0) = (chanId === TLMergedBundle.TL_CHAN_ID_A)
  def isB(dummy: Int = 0) = (chanId === TLMergedBundle.TL_CHAN_ID_B)
  def isC(dummy: Int = 0) = (chanId === TLMergedBundle.TL_CHAN_ID_C)
  def isD(dummy: Int = 0) = (chanId === TLMergedBundle.TL_CHAN_ID_D)
  def isE(dummy: Int = 0) = (chanId === TLMergedBundle.TL_CHAN_ID_E)
}

object TLMergedBundle {
  val TL_CHAN_ID_A = 0.U
  val TL_CHAN_ID_B = 1.U
  val TL_CHAN_ID_C = 2.U
  val TL_CHAN_ID_D = 3.U
  val TL_CHAN_ID_E = 4.U

  def apply(a: TLBundleA): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(a.params))
    merged.chanId  := TL_CHAN_ID_A
    merged.opcode  := a.opcode
    merged.param   := a.param
    merged.size    := a.size
    merged.source  := a.source
    merged.address := a.address
    merged.data    := a.data
    merged.union   := a.mask
    merged.last    := true.B
    merged
  }

  def apply(b: TLBundleB): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(b.params))
    merged.chanId  := TL_CHAN_ID_B
    merged.opcode  := b.opcode
    merged.param   := b.param
    merged.size    := b.size
    merged.source  := b.source
    merged.address := b.address
    merged.data    := b.data
    merged.union   := b.mask
    merged.last    := true.B
    merged
  }

  def apply(c: TLBundleC): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(c.params))
    merged.chanId  := TL_CHAN_ID_C
    merged.opcode  := c.opcode
    merged.param   := c.param
    merged.size    := c.size
    merged.source  := c.source
    merged.address := c.address
    merged.data    := c.data
    merged.union   := c.error
    merged.last    := true.B
    merged
  }

  def apply(d: TLBundleD): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(d.params))
    merged.chanId  := TL_CHAN_ID_D
    merged.opcode  := d.opcode
    merged.param   := d.param
    merged.size    := d.size
    merged.source  := d.source
    merged.address := DontCare
    merged.data    := d.data
    merged.union   := Cat(d.sink, d.error)
    merged.last    := true.B
    merged
  }

  def apply(e: TLBundleE): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(e.params))
    merged.chanId  := TL_CHAN_ID_E
    merged.opcode  := 0.U
    merged.param   := 0.U
    merged.size    := 0.U
    merged.source  := 0.U
    merged.address := 0.U
    merged.data    := 0.U
    merged.union   := Cat(e.sink, false.B)
    merged.last    := true.B
    merged
  }

  def apply(chan: DecoupledIO[TLChannel])(implicit edge: TLEdge): DecoupledIO[TLMergedBundle] = {
    val merged = Wire(Decoupled(new TLMergedBundle(chan.bits.params)))
    merged.valid := chan.valid
    merged.bits := (chan.bits match {
      case (a: TLBundleA) => apply(a)
      case (b: TLBundleB) => apply(b)
      case (c: TLBundleC) => apply(c)
      case (d: TLBundleD) => apply(d)
      case (e: TLBundleE) => apply(e)
    })
    merged.bits.last := edge.last(chan)
    chan.ready := merged.ready
    merged
  }

  def toA(chan: TLMergedBundle): TLBundleA = {
    val a = Wire(new TLBundleA(chan.params))
    a.opcode  := chan.opcode
    a.param   := chan.param
    a.size    := chan.size
    a.source  := chan.source
    a.address := chan.address
    a.data    := chan.data
    a.mask    := chan.union
    a
  }

  def toA(chan: DecoupledIO[TLMergedBundle]): DecoupledIO[TLBundleA] = {
    val a = Wire(Decoupled(new TLBundleA(chan.bits.params)))
    a.valid := chan.valid
    a.bits  := apply(a.bits)
    chan.ready := a.ready
    a
  }

  def toB(chan: TLMergedBundle): TLBundleB = {
    val b = Wire(new TLBundleB(chan.params))
    b.opcode  := chan.opcode
    b.param   := chan.param
    b.size    := chan.size
    b.source  := chan.source
    b.address := chan.address
    b.data    := chan.data
    b.mask    := chan.union
    b
  }

  def toB(chan: DecoupledIO[TLMergedBundle]): DecoupledIO[TLBundleB] = {
    val b = Wire(Decoupled(new TLBundleB(chan.bits.params)))
    b.valid := chan.valid
    b.bits  := apply(b.bits)
    chan.ready := b.ready
    b
  }

  def toC(chan: TLMergedBundle): TLBundleC = {
    val c = Wire(new TLBundleC(chan.params))
    c.opcode  := chan.opcode
    c.param   := chan.param
    c.size    := chan.size
    c.source  := chan.source
    c.address := chan.address
    c.data    := chan.data
    c.error   := chan.union(0)
    c
  }

  def toC(chan: DecoupledIO[TLMergedBundle]): DecoupledIO[TLBundleC] = {
    val c = Wire(Decoupled(new TLBundleC(chan.bits.params)))
    c.valid := chan.valid
    c.bits  := apply(c.bits)
    chan.ready := c.ready
    c
  }

  def toD(chan: TLMergedBundle): TLBundleD = {
    val d = Wire(new TLBundleD(chan.params))
    d.opcode  := chan.opcode
    d.param   := chan.param
    d.size    := chan.size
    d.source  := chan.source
    d.data    := chan.data
    d.sink    := chan.union >> 1.U
    d.error   := chan.union(0)
    d
  }

  def toD(chan: DecoupledIO[TLMergedBundle]): DecoupledIO[TLBundleD] = {
    val d = Wire(Decoupled(new TLBundleD(chan.bits.params)))
    d.valid := chan.valid
    d.bits  := apply(d.bits)
    chan.ready := d.ready
    d
  }

  def toE(chan: TLMergedBundle): TLBundleE = {
    val e = Wire(new TLBundleE(chan.params))
    e.sink := chan.union >> 1.U
    e
  }

  def toE(chan: DecoupledIO[TLMergedBundle]): DecoupledIO[TLBundleE] = {
    val e = Wire(Decoupled(new TLBundleE(chan.bits.params)))
    e.valid := chan.valid
    e.bits  := apply(e.bits)
    chan.ready := e.ready
    e
  }
}

class TLSerdes(w: Int, params: Seq[TLManagerParameters], beatBytes: Int = 8)
    (implicit p: Parameters) extends LazyModule {

  val node = TLManagerNode(params.map(
    manager =>
      TLManagerPortParameters(
        managers = Seq(manager),
        beatBytes = beatBytes)))

  lazy val module = new LazyModuleImp(this) {
    val nChannels = params.size
    val io = IO(new Bundle {
      val ser = Vec(nChannels, new SerialIO(w))
    })

    val mergeTypes = new Array[TLMergedBundle](nChannels)

    node.in.zip(io.ser).zipWithIndex.foreach { case (((tl, edge), ser), i) =>
      val mergeType = new TLMergedBundle(tl.params)

      val outChannels = Seq(tl.e, tl.c, tl.a).map(TLMergedBundle(_)(edge))
      val outArb = Module(new HellaPeekingArbiter(
        mergeType, outChannels.size, (b: TLMergedBundle) => b.last))
      val outSer = Module(new GenericSerializer(mergeType, w))
      outArb.io.in <> outChannels
      outSer.io.in <> outArb.io.out
      ser.out <> outSer.io.out

      val inDes = Module(new GenericDeserializer(mergeType, w))
      inDes.io.in <> ser.in
      tl.b.valid := inDes.io.out.valid && inDes.io.out.bits.isB()
      tl.b.bits := TLMergedBundle.toB(inDes.io.out.bits)
      tl.d.valid := inDes.io.out.valid && inDes.io.out.bits.isD()
      tl.d.bits := TLMergedBundle.toD(inDes.io.out.bits)
      inDes.io.out.ready := MuxLookup(inDes.io.out.bits.chanId, false.B, Seq(
        TLMergedBundle.TL_CHAN_ID_B -> tl.b.ready,
        TLMergedBundle.TL_CHAN_ID_D -> tl.d.ready))

      mergeTypes(i) = mergeType
    }
  }
}

class TLDesser(w: Int, params: Seq[TLClientParameters])
    (implicit p: Parameters) extends LazyModule {

  val node = TLClientNode(params.map(client =>
      TLClientPortParameters(Seq(client))))

  lazy val module = new LazyModuleImp(this) {
    val nChannels = params.size
    val io = IO(new Bundle {
      val ser = Vec(nChannels, new SerialIO(w))
    })

    val mergeTypes = new Array[TLMergedBundle](nChannels)

    node.out.zip(io.ser).zipWithIndex.foreach { case (((tl, edge), ser), i) =>
      val mergeType = new TLMergedBundle(tl.params)

      val outChannels = Seq(tl.d, tl.b).map(TLMergedBundle(_)(edge))
      val outArb = Module(new HellaPeekingArbiter(
        mergeType, outChannels.size, (b: TLMergedBundle) => b.last))
      val outSer = Module(new GenericSerializer(mergeType, w))
      outArb.io.in <> outChannels
      outSer.io.in <> outArb.io.out
      ser.out <> outSer.io.out

      val inDes = Module(new GenericDeserializer(mergeType, w))
      inDes.io.in <> ser.in
      tl.a.valid := inDes.io.out.valid && inDes.io.out.bits.isA()
      tl.a.bits := TLMergedBundle.toA(inDes.io.out.bits)
      tl.c.valid := inDes.io.out.valid && inDes.io.out.bits.isC()
      tl.c.bits := TLMergedBundle.toC(inDes.io.out.bits)
      tl.e.valid := inDes.io.out.valid && inDes.io.out.bits.isE()
      tl.e.bits := TLMergedBundle.toE(inDes.io.out.bits)
      inDes.io.out.ready := MuxLookup(inDes.io.out.bits.chanId, false.B, Seq(
        TLMergedBundle.TL_CHAN_ID_A -> tl.a.ready,
        TLMergedBundle.TL_CHAN_ID_C -> tl.c.ready,
        TLMergedBundle.TL_CHAN_ID_E -> tl.e.ready))

      mergeTypes(i) = mergeType
    }
  }
}

class TLSerdesser(
    w: Int,
    clientParams: TLClientParameters,
    managerParams: TLManagerParameters,
    beatBytes: Int = 8)
    (implicit p: Parameters) extends LazyModule {

  val clientNode = TLClientNode(
    Seq(TLClientPortParameters(Seq(clientParams))))

  val managerNode = TLManagerNode(
    Seq(TLManagerPortParameters(
      managers = Seq(managerParams),
      beatBytes = beatBytes)))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ser = new SerialIO(w)
    })

    val (client_tl, client_edge) = clientNode.out(0)
    val (manager_tl, manager_edge) = managerNode.in(0)

    require (client_tl.params.sizeBits    == manager_tl.params.sizeBits)
    require (client_tl.params.sourceBits  == manager_tl.params.sourceBits)
    require (client_tl.params.addressBits == manager_tl.params.addressBits)
    require (client_tl.params.dataBits    == manager_tl.params.dataBits)

    val mergeType = new TLMergedBundle(manager_tl.params)

    val outChannels = Seq(
      manager_tl.e, client_tl.d, manager_tl.c, client_tl.b, manager_tl.a)
    val outArb = Module(new HellaPeekingArbiter(
      mergeType, outChannels.size, (b: TLMergedBundle) => b.last))
    val outSer = Module(new GenericSerializer(mergeType, w))
    outArb.io.in <> outChannels.map(TLMergedBundle(_)(client_edge))
    outSer.io.in <> outArb.io.out
    io.ser.out <> outSer.io.out

    val inDes = Module(new GenericDeserializer(mergeType, w))
    inDes.io.in <> io.ser.in
    client_tl.a.valid := inDes.io.out.valid && inDes.io.out.bits.isA()
    client_tl.a.bits := TLMergedBundle.toA(inDes.io.out.bits)
    manager_tl.b.valid := inDes.io.out.valid && inDes.io.out.bits.isB()
    manager_tl.b.bits := TLMergedBundle.toB(inDes.io.out.bits)
    client_tl.c.valid := inDes.io.out.valid && inDes.io.out.bits.isC()
    client_tl.c.bits := TLMergedBundle.toC(inDes.io.out.bits)
    manager_tl.d.valid := inDes.io.out.valid && inDes.io.out.bits.isD()
    manager_tl.d.bits := TLMergedBundle.toD(inDes.io.out.bits)
    client_tl.e.valid := inDes.io.out.valid && inDes.io.out.bits.isE()
    client_tl.e.bits := TLMergedBundle.toE(inDes.io.out.bits)
    inDes.io.out.ready := MuxLookup(inDes.io.out.bits.chanId, false.B, Seq(
      TLMergedBundle.TL_CHAN_ID_A -> client_tl.a.ready,
      TLMergedBundle.TL_CHAN_ID_B -> manager_tl.b.ready,
      TLMergedBundle.TL_CHAN_ID_C -> client_tl.c.ready,
      TLMergedBundle.TL_CHAN_ID_D -> manager_tl.d.ready,
      TLMergedBundle.TL_CHAN_ID_E -> client_tl.e.ready))
  }
}
