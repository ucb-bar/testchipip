package testchipip.serdes

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util.HellaPeekingArbiter
import freechips.rocketchip.tilelink._

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
  val corrupt = Bool()
  // either mask or sink+denied
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

  def apply(a: TLBundleA): TLMergedBundle = apply(a, a.params)

  def apply(a: TLBundleA, params: TLBundleParameters): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(params))
    merged.chanId  := TL_CHAN_ID_A
    merged.opcode  := a.opcode
    merged.param   := a.param
    merged.size    := a.size
    merged.source  := a.source
    merged.address := a.address
    merged.data    := a.data
    merged.corrupt := a.corrupt
    merged.union   := a.mask
    merged.last    := true.B
    merged
  }

  def apply(b: TLBundleB): TLMergedBundle = apply(b, b.params)

  def apply(b: TLBundleB, params: TLBundleParameters): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(params))
    merged.chanId  := TL_CHAN_ID_B
    merged.opcode  := b.opcode
    merged.param   := b.param
    merged.size    := b.size
    merged.source  := b.source
    merged.address := b.address
    merged.data    := b.data
    merged.corrupt := b.corrupt
    merged.union   := b.mask
    merged.last    := true.B
    merged
  }

  def apply(c: TLBundleC): TLMergedBundle = apply(c, c.params)

  def apply(c: TLBundleC, params: TLBundleParameters): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(params))
    merged.chanId  := TL_CHAN_ID_C
    merged.opcode  := c.opcode
    merged.param   := c.param
    merged.size    := c.size
    merged.source  := c.source
    merged.address := c.address
    merged.data    := c.data
    merged.corrupt := c.corrupt
    merged.union   := DontCare
    merged.last    := true.B
    merged
  }

  def apply(d: TLBundleD): TLMergedBundle = apply(d, d.params)

  def apply(d: TLBundleD, params: TLBundleParameters): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(params))
    merged.chanId  := TL_CHAN_ID_D
    merged.opcode  := d.opcode
    merged.param   := d.param
    merged.size    := d.size
    merged.source  := d.source
    merged.address := DontCare
    merged.data    := d.data
    merged.corrupt := d.corrupt
    merged.union   := Cat(d.sink, d.denied)
    merged.last    := true.B
    merged
  }

  def apply(e: TLBundleE): TLMergedBundle = apply(e, e.params)

  def apply(e: TLBundleE, params: TLBundleParameters): TLMergedBundle = {
    val merged = Wire(new TLMergedBundle(params))
    merged.chanId  := TL_CHAN_ID_E
    merged.opcode  := 0.U
    merged.param   := 0.U
    merged.size    := 0.U
    merged.source  := 0.U
    merged.address := 0.U
    merged.data    := 0.U
    merged.corrupt := DontCare
    merged.union   := Cat(e.sink, false.B)
    merged.last    := true.B
    merged
  }

  def apply(chan: DecoupledIO[TLChannel], last: DecoupledIO[TLChannel] => Bool): DecoupledIO[TLMergedBundle] =
    apply(chan, chan.bits.params, last)

  def apply(chan: DecoupledIO[TLChannel], params: TLBundleParameters, last: DecoupledIO[TLChannel] => Bool): DecoupledIO[TLMergedBundle] = {
    val merged = Wire(Decoupled(new TLMergedBundle(params)))
    merged.valid := chan.valid
    merged.bits := (chan.bits match {
      case (a: TLBundleA) => apply(a, params)
      case (b: TLBundleB) => apply(b, params)
      case (c: TLBundleC) => apply(c, params)
      case (d: TLBundleD) => apply(d, params)
      case (e: TLBundleE) => apply(e, params)
    })
    merged.bits.last := last(chan)
    chan.ready := merged.ready
    merged
  }

  def toA(chan: TLMergedBundle): TLBundleA = toA(chan, chan.params)

  def toA(chan: TLMergedBundle, params: TLBundleParameters): TLBundleA = {
    val a = Wire(new TLBundleA(params))
    a.opcode  := chan.opcode
    a.param   := chan.param
    a.size    := chan.size
    a.source  := chan.source
    a.address := chan.address
    a.data    := chan.data
    a.corrupt := chan.corrupt
    a.mask    := chan.union
    a
  }

  def toA(chan: DecoupledIO[TLMergedBundle]): DecoupledIO[TLBundleA] =
    toA(chan, chan.bits.params)

  def toA(chan: DecoupledIO[TLMergedBundle], params: TLBundleParameters): DecoupledIO[TLBundleA] = {
    val a = Wire(Decoupled(new TLBundleA(params)))
    a.valid := chan.valid
    a.bits  := apply(a.bits, params)
    chan.ready := a.ready
    a
  }

  def toB(chan: TLMergedBundle): TLBundleB = toB(chan, chan.params)

  def toB(chan: TLMergedBundle, params: TLBundleParameters): TLBundleB = {
    val b = Wire(new TLBundleB(params))
    b.opcode  := chan.opcode
    b.param   := chan.param
    b.size    := chan.size
    b.source  := chan.source
    b.address := chan.address
    b.data    := chan.data
    b.corrupt := chan.corrupt
    b.mask    := chan.union
    b
  }

  def toB(chan: DecoupledIO[TLMergedBundle]): DecoupledIO[TLBundleB] =
    toB(chan, chan.bits.params)

  def toB(chan: DecoupledIO[TLMergedBundle], params: TLBundleParameters): DecoupledIO[TLBundleB] = {
    val b = Wire(Decoupled(new TLBundleB(params)))
    b.valid := chan.valid
    b.bits  := apply(b.bits)
    chan.ready := b.ready
    b
  }

  def toC(chan: TLMergedBundle): TLBundleC = toC(chan, chan.params)

  def toC(chan: TLMergedBundle, params: TLBundleParameters): TLBundleC = {
    val c = Wire(new TLBundleC(params))
    c.opcode  := chan.opcode
    c.param   := chan.param
    c.size    := chan.size
    c.source  := chan.source
    c.address := chan.address
    c.data    := chan.data
    c.corrupt := chan.corrupt
    c
  }

  def toC(chan: DecoupledIO[TLMergedBundle]): DecoupledIO[TLBundleC] =
    toC(chan, chan.bits.params)

  def toC(chan: DecoupledIO[TLMergedBundle], params: TLBundleParameters): DecoupledIO[TLBundleC] = {
    val c = Wire(Decoupled(new TLBundleC(params)))
    c.valid := chan.valid
    c.bits  := apply(c.bits)
    chan.ready := c.ready
    c
  }

  def toD(chan: TLMergedBundle): TLBundleD = toD(chan, chan.params)

  def toD(chan: TLMergedBundle, params: TLBundleParameters): TLBundleD = {
    val d = Wire(new TLBundleD(params))
    d.opcode  := chan.opcode
    d.param   := chan.param
    d.size    := chan.size
    d.source  := chan.source
    d.data    := chan.data
    d.corrupt := chan.corrupt
    d.sink    := chan.union >> 1.U
    d.denied  := chan.union(0)
    d
  }

  def toD(chan: DecoupledIO[TLMergedBundle]): DecoupledIO[TLBundleD] =
    toD(chan, chan.bits.params)

  def toD(chan: DecoupledIO[TLMergedBundle], params: TLBundleParameters): DecoupledIO[TLBundleD] = {
    val d = Wire(Decoupled(new TLBundleD(params)))
    d.valid := chan.valid
    d.bits  := apply(d.bits)
    chan.ready := d.ready
    d
  }

  def toE(chan: TLMergedBundle): TLBundleE = toE(chan, chan.params)

  def toE(chan: TLMergedBundle, params: TLBundleParameters): TLBundleE = {
    val e = Wire(new TLBundleE(params))
    e.sink := chan.union >> 1.U
    e
  }

  def toE(chan: DecoupledIO[TLMergedBundle]): DecoupledIO[TLBundleE] =
    toE(chan, chan.bits.params)

  def toE(chan: DecoupledIO[TLMergedBundle], params: TLBundleParameters): DecoupledIO[TLBundleE] = {
    val e = Wire(Decoupled(new TLBundleE(params)))
    e.valid := chan.valid
    e.bits  := apply(e.bits)
    chan.ready := e.ready
    e
  }
}

object TLSerdesser {
  // This should be the standard bundle type for TLSerdesser
  val STANDARD_TLBUNDLE_PARAMS = TLBundleParameters(
    addressBits=64, dataBits=64,
    sourceBits=8, sinkBits=8, sizeBits=8,
    echoFields=Nil, requestFields=Nil, responseFields=Nil,
    hasBCE=false)
}

class SerdesDebugIO extends Bundle {
  val ser_busy = Output(Bool())
  val des_busy = Output(Bool())
}

class TLSerdesser(
  val flitWidth: Int,
  clientPortParams: Option[TLMasterPortParameters],
  managerPortParams: Option[TLSlavePortParameters],
  val bundleParams: TLBundleParameters = TLSerdesser.STANDARD_TLBUNDLE_PARAMS)
  (implicit p: Parameters) extends LazyModule {
  require (clientPortParams.isDefined || managerPortParams.isDefined)
  val clientNode = clientPortParams.map { c => TLClientNode(Seq(c)) }
  val managerNode = managerPortParams.map { m => TLManagerNode(Seq(m)) }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ser = new DecoupledFlitIO(flitWidth)
      val debug = new SerdesDebugIO
    })

    val client_tl = clientNode.map(_.out(0)._1).getOrElse(0.U.asTypeOf(new TLBundle(bundleParams)))
    val client_edge = clientNode.map(_.out(0)._2)
    val manager_tl = managerNode.map(_.in(0)._1).getOrElse(0.U.asTypeOf(new TLBundle(bundleParams)))
    val manager_edge = managerNode.map(_.in(0)._2)

    val clientParams = client_edge.map(_.bundle).getOrElse(bundleParams)
    val managerParams = manager_edge.map(_.bundle).getOrElse(bundleParams)
    val mergedParams = clientParams.union(managerParams).union(bundleParams)
    require(mergedParams.echoFields.isEmpty, "TLSerdesser does not support TileLink with echo fields")
    require(mergedParams.requestFields.isEmpty, "TLSerdesser does not support TileLink with request fields")
    require(mergedParams.responseFields.isEmpty, "TLSerdesser does not support TileLink with response fields")
    require(mergedParams == bundleParams, s"TLSerdesser is misconfigured, the combined inwards/outwards parameters cannot be serialized using the provided bundle params\n$mergedParams > $bundleParams")

    val mergeType = new TLMergedBundle(mergedParams)

    val outChannels = Seq(
      manager_tl.e, client_tl.d, manager_tl.c, client_tl.b, manager_tl.a)
    val outArb = Module(new HellaPeekingArbiter(
      mergeType, outChannels.size, (b: TLMergedBundle) => b.last))
    val outSer = Module(new GenericSerializer(mergeType, flitWidth))
    outArb.io.in <> outChannels.map(o => TLMergedBundle(o, mergedParams, c => client_edge.map(_.last(c)).getOrElse(false.B)))
    outSer.io.in <> outArb.io.out
    io.ser.out <> outSer.io.out
    io.debug.ser_busy := outSer.io.busy

    val inDes = Module(new GenericDeserializer(mergeType, flitWidth))
    inDes.io.in <> io.ser.in
    io.debug.des_busy := inDes.io.busy
    client_tl.a.valid := inDes.io.out.valid && inDes.io.out.bits.isA()
    client_tl.a.bits := TLMergedBundle.toA(inDes.io.out.bits, clientParams)
    manager_tl.b.valid := inDes.io.out.valid && inDes.io.out.bits.isB()
    manager_tl.b.bits := TLMergedBundle.toB(inDes.io.out.bits, managerParams)
    client_tl.c.valid := inDes.io.out.valid && inDes.io.out.bits.isC()
    client_tl.c.bits := TLMergedBundle.toC(inDes.io.out.bits, clientParams)
    manager_tl.d.valid := inDes.io.out.valid && inDes.io.out.bits.isD()
    manager_tl.d.bits := TLMergedBundle.toD(inDes.io.out.bits, managerParams)
    client_tl.e.valid := inDes.io.out.valid && inDes.io.out.bits.isE()
    client_tl.e.bits := TLMergedBundle.toE(inDes.io.out.bits, clientParams)
    inDes.io.out.ready := MuxLookup(inDes.io.out.bits.chanId, false.B, Seq(
      TLMergedBundle.TL_CHAN_ID_A -> client_tl.a.ready,
      TLMergedBundle.TL_CHAN_ID_B -> manager_tl.b.ready,
      TLMergedBundle.TL_CHAN_ID_C -> client_tl.c.ready,
      TLMergedBundle.TL_CHAN_ID_D -> manager_tl.d.ready,
      TLMergedBundle.TL_CHAN_ID_E -> client_tl.e.ready))
  }
}
