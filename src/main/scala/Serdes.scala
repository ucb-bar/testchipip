package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import freechips.rocketchip.util.HellaPeekingArbiter
import freechips.rocketchip.tilelink._

class SerialIO(val w: Int) extends Bundle {
  val in = Flipped(Decoupled(UInt(w.W)))
  val out = Decoupled(UInt(w.W))

  def flipConnect(other: SerialIO) {
    in <> other.out
    other.in <> out
  }
}

class ValidSerialIO(val w: Int) extends Bundle {
  val in = Flipped(Valid(UInt(w.W)))
  val out = Valid(UInt(w.W))

  def flipConnect(other: ValidSerialIO) {
    in <> other.out
    other.in <> out
  }
}

class StreamChannel(val w: Int) extends Bundle {
  val data = UInt(w.W)
  val keep = UInt((w/8).W)
  val last = Bool()
}

class StreamIO(val w: Int) extends Bundle {
  val in = Flipped(Decoupled(new StreamChannel(w)))
  val out = Decoupled(new StreamChannel(w))

  def flipConnect(other: StreamIO) {
    in <> other.out
    other.in <> out
  }
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

  when (io.in.fire) {
    count := (outBeats - 1).U
    bits := io.in.bits
    state := s_send
  }

  when (io.out.fire) {
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
  val keep = RegInit(VecInit(Seq.fill(inBeats)(0.U(inBytes.W))))
  val last = Reg(Bool())

  val idx = RegInit(0.U(log2Ceil(inBeats).W))

  val s_recv :: s_send :: Nil = Enum(2)
  val state = RegInit(s_recv)

  io.in.ready := state === s_recv
  io.out.valid := state === s_send
  io.out.bits.data := data.asUInt
  io.out.bits.keep := keep.asUInt
  io.out.bits.last := last

  when (io.in.fire) {
    idx := idx + 1.U
    data(idx) := io.in.bits.data
    keep(idx) := io.in.bits.keep
    when (io.in.bits.last || idx === (inBeats - 1).U) {
      last := io.in.bits.last
      state := s_send
    }
  }

  when (io.out.fire) {
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
  val (sendCount, sendDone) = Counter(io.out.fire, dataBeats)

  io.in.ready := !sending
  io.out.valid := sending
  io.out.bits := data(w-1, 0)

  when (io.in.fire) {
    data := io.in.bits.asUInt
    sending := true.B
  }

  when (io.out.fire) { data := data >> w.U }

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
  val (recvCount, recvDone) = Counter(io.in.fire, dataBeats)

  io.in.ready := receiving
  io.out.valid := !receiving
  io.out.bits := data.asUInt.asTypeOf(t)

  when (io.in.fire) {
    data(recvCount) := io.in.bits
  }

  when (recvDone) { receiving := false.B }

  when (io.out.fire) { receiving := true.B }
}

// If hasCorruptDenied is false we revert to earlier TL2 bundles which have an error signal on C and D in the same position as denied in D
class TLMergedBundle(params: TLBundleParameters, hasCorruptDenied: Boolean = true) extends TLBundleBase(params) {
  val chanId = UInt(3.W)
  val opcode = UInt(3.W)
  val param = UInt(Seq(
    TLAtomics.width, TLHints.width,
    TLPermissions.aWidth, TLPermissions.bdWidth, TLPermissions.cWidth).max.W)
  val size = UInt(params.sizeBits.W)
  val source = UInt(params.sourceBits.W)
  val address = UInt(params.addressBits.W)
  val data = UInt(params.dataBits.W)
  val corrupt = if(hasCorruptDenied) Some(Bool()) else None
  // either mask or sink+denied (or sink+error if !hasCorruptDenied)
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

  def apply(a: TLBundleA, hasCorruptDenied: Boolean): TLMergedBundle = apply(a, a.params, hasCorruptDenied)

  def apply(a: TLBundleA, params: TLBundleParameters, hasCorruptDenied: Boolean): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(params, hasCorruptDenied))
    merged.chanId  := TL_CHAN_ID_A
    merged.opcode  := a.opcode
    merged.param   := a.param
    merged.size    := a.size
    merged.source  := a.source
    merged.address := a.address
    merged.data    := a.data
    if(hasCorruptDenied)
      merged.corrupt.get := a.corrupt
    merged.union   := a.mask
    merged.last    := true.B
    merged
  }

  def apply(b: TLBundleB, hasCorruptDenied: Boolean): TLMergedBundle = apply(b, b.params, hasCorruptDenied)

  def apply(b: TLBundleB, params: TLBundleParameters, hasCorruptDenied: Boolean): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(params, hasCorruptDenied))
    merged.chanId  := TL_CHAN_ID_B
    merged.opcode  := b.opcode
    merged.param   := b.param
    merged.size    := b.size
    merged.source  := b.source
    merged.address := b.address
    merged.data    := b.data
    if(hasCorruptDenied)
      merged.corrupt.get := b.corrupt
    merged.union   := b.mask
    merged.last    := true.B
    merged
  }

  def apply(c: TLBundleC, hasCorruptDenied: Boolean): TLMergedBundle = apply(c, c.params, hasCorruptDenied)

  def apply(c: TLBundleC, params: TLBundleParameters, hasCorruptDenied: Boolean): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(params, hasCorruptDenied))
    merged.chanId  := TL_CHAN_ID_C
    merged.opcode  := c.opcode
    merged.param   := c.param
    merged.size    := c.size
    merged.source  := c.source
    merged.address := c.address
    merged.data    := c.data
    if(hasCorruptDenied) {
      merged.corrupt.get := c.corrupt
      merged.union   := DontCare
    } else {
      merged.union   := 0.U //error
    }
    merged.last    := true.B
    merged
  }

  def apply(d: TLBundleD, hasCorruptDenied: Boolean): TLMergedBundle = apply(d, d.params, hasCorruptDenied)

  def apply(d: TLBundleD, params: TLBundleParameters, hasCorruptDenied: Boolean): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(params, hasCorruptDenied))
    merged.chanId  := TL_CHAN_ID_D
    merged.opcode  := d.opcode
    merged.param   := d.param
    merged.size    := d.size
    merged.source  := d.source
    merged.address := DontCare
    merged.data    := d.data
    if(hasCorruptDenied) {
      merged.corrupt.get := d.corrupt
      merged.union   := Cat(d.sink, d.denied)
    } else {
      merged.union   := Cat(d.sink, 0.U) //error
    }
    merged.last    := true.B
    merged
  }

  def apply(e: TLBundleE, hasCorruptDenied: Boolean): TLMergedBundle = apply(e, e.params, hasCorruptDenied)

  def apply(e: TLBundleE, params: TLBundleParameters, hasCorruptDenied: Boolean): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(params, hasCorruptDenied))
    merged.chanId  := TL_CHAN_ID_E
    merged.opcode  := 0.U
    merged.param   := 0.U
    merged.size    := 0.U
    merged.source  := 0.U
    merged.address := 0.U
    merged.data    := 0.U
    if(hasCorruptDenied) {
      merged.corrupt.get := DontCare
      merged.union   := Cat(e.sink, false.B)
    } else {
      merged.union   := Cat(e.sink)
    }
    merged.last    := true.B
    merged
  }

  def apply(chan: DecoupledIO[TLChannel], hasCorruptDenied: Boolean)
      (implicit edge: TLEdge): DecoupledIO[TLMergedBundle] =
    apply(chan, chan.bits.params, hasCorruptDenied)

  def apply(chan: DecoupledIO[TLChannel], params: TLBundleParameters, hasCorruptDenied: Boolean)
      (implicit edge: TLEdge): DecoupledIO[TLMergedBundle] = {
    val merged = Wire(Decoupled(new TLMergedBundle(params)))
    merged.valid := chan.valid
    merged.bits := (chan.bits match {
      case (a: TLBundleA) => apply(a, params, hasCorruptDenied)
      case (b: TLBundleB) => apply(b, params, hasCorruptDenied)
      case (c: TLBundleC) => apply(c, params, hasCorruptDenied)
      case (d: TLBundleD) => apply(d, params, hasCorruptDenied)
      case (e: TLBundleE) => apply(e, params, hasCorruptDenied)
    })
    merged.bits.last := edge.last(chan)
    chan.ready := merged.ready
    merged
  }

  def toA(chan: TLMergedBundle, hasCorruptDenied: Boolean): TLBundleA = toA(chan, chan.params, hasCorruptDenied)

  def toA(chan: TLMergedBundle, params: TLBundleParameters, hasCorruptDenied: Boolean): TLBundleA = {
    val a = Wire(new TLBundleA(params))
    a.opcode  := chan.opcode
    a.param   := chan.param
    a.size    := chan.size
    a.source  := chan.source
    a.address := chan.address
    a.data    := chan.data
    if(hasCorruptDenied)
      a.corrupt := chan.corrupt.get
    else
      a.corrupt := false.B
    a.mask    := chan.union
    a
  }

  def toA(chan: DecoupledIO[TLMergedBundle], hasCorruptDenied: Boolean): DecoupledIO[TLBundleA] =
    toA(chan, chan.bits.params, hasCorruptDenied)

  def toA(chan: DecoupledIO[TLMergedBundle], params: TLBundleParameters, hasCorruptDenied: Boolean): DecoupledIO[TLBundleA] = {
    val a = Wire(Decoupled(new TLBundleA(params)))
    a.valid := chan.valid
    a.bits  := apply(a.bits, params, hasCorruptDenied)
    chan.ready := a.ready
    a
  }

  def toB(chan: TLMergedBundle, hasCorruptDenied: Boolean): TLBundleB = toB(chan, chan.params, hasCorruptDenied)

  def toB(chan: TLMergedBundle, params: TLBundleParameters, hasCorruptDenied: Boolean): TLBundleB = {
    val b = Wire(new TLBundleB(params))
    b.opcode  := chan.opcode
    b.param   := chan.param
    b.size    := chan.size
    b.source  := chan.source
    b.address := chan.address
    b.data    := chan.data
    if(hasCorruptDenied)
      b.corrupt := chan.corrupt.get
    else
      b.corrupt := false.B
    b.mask    := chan.union
    b
  }

  def toB(chan: DecoupledIO[TLMergedBundle], hasCorruptDenied: Boolean): DecoupledIO[TLBundleB] =
    toB(chan, chan.bits.params, hasCorruptDenied)

  def toB(chan: DecoupledIO[TLMergedBundle], params: TLBundleParameters, hasCorruptDenied: Boolean): DecoupledIO[TLBundleB] = {
    val b = Wire(Decoupled(new TLBundleB(params)))
    b.valid := chan.valid
    b.bits  := apply(b.bits, hasCorruptDenied)
    chan.ready := b.ready
    b
  }

  def toC(chan: TLMergedBundle, hasCorruptDenied: Boolean): TLBundleC = toC(chan, chan.params, hasCorruptDenied)

  def toC(chan: TLMergedBundle, params: TLBundleParameters, hasCorruptDenied: Boolean): TLBundleC = {
    val c = Wire(new TLBundleC(params))
    c.opcode  := chan.opcode
    c.param   := chan.param
    c.size    := chan.size
    c.source  := chan.source
    c.address := chan.address
    c.data    := chan.data
    if(hasCorruptDenied)
      c.corrupt := chan.corrupt.get
    else
      c.corrupt := false.B
    c
  }

  def toC(chan: DecoupledIO[TLMergedBundle], hasCorruptDenied: Boolean): DecoupledIO[TLBundleC] =
    toC(chan, chan.bits.params, hasCorruptDenied)

  def toC(chan: DecoupledIO[TLMergedBundle], params: TLBundleParameters, hasCorruptDenied: Boolean): DecoupledIO[TLBundleC] = {
    val c = Wire(Decoupled(new TLBundleC(params)))
    c.valid := chan.valid
    c.bits  := apply(c.bits, hasCorruptDenied)
    chan.ready := c.ready
    c
  }

  def toD(chan: TLMergedBundle, hasCorruptDenied: Boolean): TLBundleD = toD(chan, chan.params, hasCorruptDenied)

  def toD(chan: TLMergedBundle, params: TLBundleParameters, hasCorruptDenied: Boolean): TLBundleD = {
    val d = Wire(new TLBundleD(params))
    d.opcode  := chan.opcode
    d.param   := chan.param
    d.size    := chan.size
    d.source  := chan.source
    d.data    := chan.data
    if(hasCorruptDenied) {
      d.corrupt := chan.corrupt.get
      d.sink    := chan.union >> 1.U
      d.denied  := chan.union(0)
    } else {
      d.corrupt := false.B
      d.sink    := chan.union >> 1.U // error
      d.denied  := false.B
    }
    d
  }

  def toD(chan: DecoupledIO[TLMergedBundle], hasCorruptDenied: Boolean): DecoupledIO[TLBundleD] =
    toD(chan, chan.bits.params, hasCorruptDenied)

  def toD(chan: DecoupledIO[TLMergedBundle], params: TLBundleParameters, hasCorruptDenied: Boolean): DecoupledIO[TLBundleD] = {
    val d = Wire(Decoupled(new TLBundleD(params)))
    d.valid := chan.valid
    d.bits  := apply(d.bits, hasCorruptDenied)
    chan.ready := d.ready
    d
  }

  def toE(chan: TLMergedBundle, hasCorruptDenied: Boolean): TLBundleE = toE(chan, chan.params, hasCorruptDenied)

  def toE(chan: TLMergedBundle, params: TLBundleParameters, hasCorruptDenied: Boolean): TLBundleE = {
    val e = Wire(new TLBundleE(params))
    if(hasCorruptDenied)
      e.sink := chan.union >> 1.U
    else
      e.sink := chan.union
    e
  }

  def toE(chan: DecoupledIO[TLMergedBundle], hasCorruptDenied: Boolean): DecoupledIO[TLBundleE] =
    toE(chan, chan.bits.params, hasCorruptDenied)

  def toE(chan: DecoupledIO[TLMergedBundle], params: TLBundleParameters, hasCorruptDenied: Boolean): DecoupledIO[TLBundleE] = {
    val e = Wire(Decoupled(new TLBundleE(params)))
    e.valid := chan.valid
    e.bits  := apply(e.bits, hasCorruptDenied)
    chan.ready := e.ready
    e
  }
}

class TLSerdes(w: Int, params: Seq[TLManagerParameters], beatBytes: Int = 8, hasCorruptDenied: Boolean = true)
    (implicit p: Parameters) extends LazyModule {

  val node = TLManagerNode(params.map(
    manager =>
      TLSlavePortParameters.v1(
        managers = Seq(manager),
        beatBytes = beatBytes)))

  lazy val module = new LazyModuleImp(this) {
    val nChannels = params.size
    val io = IO(new Bundle {
      val ser = Vec(nChannels, new SerialIO(w))
    })

    val mergeTypes = new Array[TLMergedBundle](nChannels)

    node.in.zip(io.ser).zipWithIndex.foreach { case (((tl, edge), ser), i) =>
      val mergeType = new TLMergedBundle(tl.params, hasCorruptDenied)

      val outChannels = Seq(tl.e, tl.c, tl.a).map(TLMergedBundle(_, hasCorruptDenied)(edge))
      val outArb = Module(new HellaPeekingArbiter(
        mergeType, outChannels.size, (b: TLMergedBundle) => b.last))
      val outSer = Module(new GenericSerializer(mergeType, w))
      outArb.io.in <> outChannels
      outSer.io.in <> outArb.io.out
      ser.out <> outSer.io.out

      val inDes = Module(new GenericDeserializer(mergeType, w))
      inDes.io.in <> ser.in
      tl.b.valid := inDes.io.out.valid && inDes.io.out.bits.isB()
      tl.b.bits := TLMergedBundle.toB(inDes.io.out.bits, hasCorruptDenied)
      tl.d.valid := inDes.io.out.valid && inDes.io.out.bits.isD()
      tl.d.bits := TLMergedBundle.toD(inDes.io.out.bits, hasCorruptDenied)
      inDes.io.out.ready := MuxLookup(inDes.io.out.bits.chanId, false.B, Seq(
        TLMergedBundle.TL_CHAN_ID_B -> tl.b.ready,
        TLMergedBundle.TL_CHAN_ID_D -> tl.d.ready))

      mergeTypes(i) = mergeType
    }
  }
}

class TLDesser(w: Int, params: Seq[TLClientParameters], hasCorruptDenied: Boolean = true)
    (implicit p: Parameters) extends LazyModule {

  val node = TLClientNode(params.map(client =>
      TLMasterPortParameters.v1(Seq(client))))

  lazy val module = new LazyModuleImp(this) {
    val nChannels = params.size
    val io = IO(new Bundle {
      val ser = Vec(nChannels, new SerialIO(w))
    })

    val mergeTypes = new Array[TLMergedBundle](nChannels)

    node.out.zip(io.ser).zipWithIndex.foreach { case (((tl, edge), ser), i) =>
      val mergeType = new TLMergedBundle(tl.params, hasCorruptDenied)

      val outChannels = Seq(tl.d, tl.b).map(TLMergedBundle(_, hasCorruptDenied)(edge))
      val outArb = Module(new HellaPeekingArbiter(
        mergeType, outChannels.size, (b: TLMergedBundle) => b.last))
      val outSer = Module(new GenericSerializer(mergeType, w))
      outArb.io.in <> outChannels
      outSer.io.in <> outArb.io.out
      ser.out <> outSer.io.out

      val inDes = Module(new GenericDeserializer(mergeType, w))
      inDes.io.in <> ser.in
      tl.a.valid := inDes.io.out.valid && inDes.io.out.bits.isA()
      tl.a.bits := TLMergedBundle.toA(inDes.io.out.bits, hasCorruptDenied)
      tl.c.valid := inDes.io.out.valid && inDes.io.out.bits.isC()
      tl.c.bits := TLMergedBundle.toC(inDes.io.out.bits, hasCorruptDenied)
      tl.e.valid := inDes.io.out.valid && inDes.io.out.bits.isE()
      tl.e.bits := TLMergedBundle.toE(inDes.io.out.bits, hasCorruptDenied)
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
  clientPortParams: TLMasterPortParameters,
  managerPortParams: TLSlavePortParameters,
  hasCorruptDenied: Boolean = true)
  (implicit p: Parameters) extends LazyModule {
  val clientNode = TLClientNode(Seq(clientPortParams))
  val managerNode = TLManagerNode(Seq(managerPortParams))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ser = new SerialIO(w)
    })

    val (client_tl, client_edge) = clientNode.out(0)
    val (manager_tl, manager_edge) = managerNode.in(0)

    val clientParams = client_edge.bundle
    val managerParams = manager_edge.bundle
    val mergedParams = clientParams.union(managerParams)
    val mergeType = new TLMergedBundle(mergedParams, hasCorruptDenied)

    val outChannels = Seq(
      manager_tl.e, client_tl.d, manager_tl.c, client_tl.b, manager_tl.a)
    val outArb = Module(new HellaPeekingArbiter(
      mergeType, outChannels.size, (b: TLMergedBundle) => b.last))
    val outSer = Module(new GenericSerializer(mergeType, w))
    outArb.io.in <> outChannels.map(TLMergedBundle(_, mergedParams, hasCorruptDenied)(client_edge))
    outSer.io.in <> outArb.io.out
    io.ser.out <> outSer.io.out

    val inDes = Module(new GenericDeserializer(mergeType, w))
    inDes.io.in <> io.ser.in
    client_tl.a.valid := inDes.io.out.valid && inDes.io.out.bits.isA()
    client_tl.a.bits := TLMergedBundle.toA(inDes.io.out.bits, clientParams, hasCorruptDenied)
    manager_tl.b.valid := inDes.io.out.valid && inDes.io.out.bits.isB()
    manager_tl.b.bits := TLMergedBundle.toB(inDes.io.out.bits, managerParams, hasCorruptDenied)
    client_tl.c.valid := inDes.io.out.valid && inDes.io.out.bits.isC()
    client_tl.c.bits := TLMergedBundle.toC(inDes.io.out.bits, clientParams, hasCorruptDenied)
    manager_tl.d.valid := inDes.io.out.valid && inDes.io.out.bits.isD()
    manager_tl.d.bits := TLMergedBundle.toD(inDes.io.out.bits, managerParams, hasCorruptDenied)
    client_tl.e.valid := inDes.io.out.valid && inDes.io.out.bits.isE()
    client_tl.e.bits := TLMergedBundle.toE(inDes.io.out.bits, clientParams, hasCorruptDenied)
    inDes.io.out.ready := MuxLookup(inDes.io.out.bits.chanId, false.B, Seq(
      TLMergedBundle.TL_CHAN_ID_A -> client_tl.a.ready,
      TLMergedBundle.TL_CHAN_ID_B -> manager_tl.b.ready,
      TLMergedBundle.TL_CHAN_ID_C -> client_tl.c.ready,
      TLMergedBundle.TL_CHAN_ID_D -> manager_tl.d.ready,
      TLMergedBundle.TL_CHAN_ID_E -> client_tl.e.ready))
  }
}
