package testchipip

import chisel3._
import chisel3.util._
import junctions._
import uncore.tilelink._
import scala.math.max
import config.{Parameters, Field}
import _root_.util.{ParameterizedBundle, HellaPeekingArbiter}

trait HasTileLinkSerializerParameters extends HasTileLinkParameters {
  val nChannels = 5
  val tlChannelIdBits = log2Up(nChannels)
  def tlSerialDataBits = {
    Seq(Wire(new AcquireFromSrc),
        Wire(new ReleaseFromSrc),
        Wire(new FinishToDst),
        Wire(new GrantFromSrc),
        Wire(new GrantToDst),
        Wire(new ProbeToDst))
      .map(ch => ch.asUInt.getWidth)
      .reduce(max(_, _))
  }
}

abstract class TLSerBundle(implicit val p: Parameters)
    extends ParameterizedBundle()(p)
    with HasTileLinkSerializerParameters

abstract class TLSerModule(_clock: Clock = null, _reset: Bool = null)(implicit val p: Parameters)
    extends Module(Option(_clock), Option(_reset)) with HasTileLinkSerializerParameters

class TLSerChannel(implicit p: Parameters)
    extends TLSerBundle()(p) {
  val chan = UInt(tlChannelIdBits.W)
  val data = UInt(tlSerialDataBits.W)
  val last = Bool()
}

class TLSerializedIO(implicit p: Parameters) extends TLSerBundle()(p) {
  val ctom = Decoupled(new TLSerChannel)
  val mtoc = Flipped(Decoupled(new TLSerChannel))
}

trait HasTileLinkSerializers {
  val SER_ACQ = 4.U
  val SER_PRB = 3.U
  val SER_REL = 2.U
  val SER_GNT = 1.U
  val SER_FIN = 0.U

  def serialize(in: Acquire)(implicit p: Parameters): TLSerChannel = {
    val out = Wire(new TLSerChannel)
    out.chan := SER_ACQ
    out.data := in.asUInt
    out.last := in.last()
    out
  }

  def serialize(in: Probe)(implicit p: Parameters): TLSerChannel = {
    val out = Wire(new TLSerChannel)
    out.chan := SER_PRB
    out.data := in.asUInt
    out.last := true.B
    out
  }


  def serialize(in: Release)(implicit p: Parameters): TLSerChannel = {
    val out = Wire(new TLSerChannel)
    out.chan := SER_REL
    out.data := in.asUInt
    out.last := in.last()
    out
  }

  def serialize(in: Grant)(implicit p: Parameters): TLSerChannel = {
      val out = Wire(new TLSerChannel)
    out.chan := SER_GNT
    out.data := in.asUInt
    out.last := in.last()
    out
  }

  def serialize(in: Finish)(implicit p: Parameters): TLSerChannel = {
    val out = Wire(new TLSerChannel)
    out.chan := SER_FIN
    out.data := in.asUInt
    out.last := true.B
    out
  }
}

class ClientTileLinkIOSerdes(w: Int, _clock: Clock = null, _reset: Bool = null)(implicit p: Parameters)
    extends TLSerModule(_clock, _reset)(p) with HasTileLinkSerializers {
  val io = IO(new Bundle {
    val tl = Flipped(new ClientTileLinkIO)
    val serial = new SerialIO(w)
  })

  val ctomArb = Module(new HellaPeekingArbiter(
    new TLSerChannel, 3, (b: TLSerChannel) => b.last))
  ctomArb.io.in(0).valid := io.tl.finish.valid
  io.tl.finish.ready := ctomArb.io.in(0).ready
  ctomArb.io.in(0).bits := serialize(io.tl.finish.bits)
  ctomArb.io.in(1).valid := io.tl.release.valid
  io.tl.release.ready := ctomArb.io.in(1).ready
  ctomArb.io.in(1).bits := serialize(io.tl.release.bits)
  ctomArb.io.in(2).valid := io.tl.acquire.valid
  io.tl.acquire.ready := ctomArb.io.in(2).ready
  ctomArb.io.in(2).bits := serialize(io.tl.acquire.bits)

  val ser = Module(new Serializer(w, new TLSerChannel))
  ser.io.in <> ctomArb.io.out
  io.serial.out <> ser.io.out

  val des = Module(new Deserializer(w, new TLSerChannel))
  des.io.in <> io.serial.in
  io.tl.grant.valid := des.io.out.valid && des.io.out.bits.chan === SER_GNT
  io.tl.grant.bits := io.tl.grant.bits.fromBits(des.io.out.bits.data)
  io.tl.probe.valid := des.io.out.valid && des.io.out.bits.chan === SER_PRB
  io.tl.probe.bits := io.tl.probe.bits.fromBits(des.io.out.bits.data)
  des.io.out.ready := MuxLookup(des.io.out.bits.chan, false.B, Seq(
    SER_GNT -> io.tl.grant.ready,
    SER_PRB -> io.tl.probe.ready))
}

class ClientTileLinkIODesser(w: Int, _clock: Clock = null, _reset: Bool = null)(implicit p: Parameters)
    extends TLSerModule(_clock, _reset)(p) with HasTileLinkSerializers {
  val io = IO(new Bundle {
    val serial = new SerialIO(w)
    val tl = new ClientTileLinkIO
  })

  val mtocArb = Module(new HellaPeekingArbiter(
    new TLSerChannel, 2, (b: TLSerChannel) => b.last))
  mtocArb.io.in(0).valid := io.tl.grant.valid
  io.tl.grant.ready := mtocArb.io.in(0).ready
  mtocArb.io.in(0).bits := serialize(io.tl.grant.bits)
  mtocArb.io.in(1).valid := io.tl.probe.valid
  io.tl.probe.ready := mtocArb.io.in(1).ready
  mtocArb.io.in(1).bits := serialize(io.tl.probe.bits)

  val ser = Module(new Serializer(w, new TLSerChannel))
  ser.io.in <> mtocArb.io.out
  io.serial.out <> ser.io.out

  val des = Module(new Deserializer(w, new TLSerChannel))
  des.io.in <> io.serial.in
  io.tl.finish.valid := des.io.out.valid && des.io.out.bits.chan === SER_FIN
  io.tl.finish.bits := io.tl.finish.bits.fromBits(des.io.out.bits.data)
  io.tl.release.valid := des.io.out.valid && des.io.out.bits.chan === SER_REL
  io.tl.release.bits := io.tl.release.bits.fromBits(des.io.out.bits.data)
  io.tl.acquire.valid := des.io.out.valid && des.io.out.bits.chan === SER_ACQ
  io.tl.acquire.bits := io.tl.acquire.bits.fromBits(des.io.out.bits.data)
  des.io.out.ready := MuxLookup(des.io.out.bits.chan, false.B, Seq(
    SER_FIN -> io.tl.finish.ready,
    SER_REL -> io.tl.release.ready,
    SER_ACQ -> io.tl.acquire.ready))
}

class ClientUncachedTileLinkIOSerdes(w: Int, _clock: Clock = null, _reset: Bool = null)(implicit p: Parameters)
    extends TLSerModule(_clock, _reset)(p) with HasTileLinkSerializers {

  val io = IO(new Bundle {
    val tl = Flipped(new ClientUncachedTileLinkIO)
    val serial = new SerialIO(w)
  })

  val ser = Module(new Serializer(w, new TLSerChannel))
  ser.io.in.valid := io.tl.acquire.valid
  io.tl.acquire.ready := ser.io.in.ready
  ser.io.in.bits := serialize(io.tl.acquire.bits)
  io.serial.out <> ser.io.out

  val des = Module(new Deserializer(w, new TLSerChannel))
  des.io.in <> io.serial.in
  io.tl.grant.valid := des.io.out.valid
  des.io.out.ready := io.tl.grant.ready
  io.tl.grant.bits := io.tl.grant.bits.fromBits(des.io.out.bits.data)
}

class ClientUncachedTileLinkIODesser(w: Int, _clock: Clock = null, _reset: Bool = null)(implicit p: Parameters)
    extends TLSerModule(_clock, _reset)(p) with HasTileLinkSerializers {
  val io = IO(new Bundle {
    val serial = new SerialIO(w)
    val tl = new ClientUncachedTileLinkIO
  })

  val ser = Module(new Serializer(w, new TLSerChannel))
  ser.io.in.valid := io.tl.grant.valid
  io.tl.grant.ready := ser.io.in.ready
  ser.io.in.bits := serialize(io.tl.grant.bits)
  io.serial.out <> ser.io.out

  val des = Module(new Deserializer(w, new TLSerChannel))
  des.io.in <> io.serial.in
  io.tl.acquire.valid := des.io.out.valid
  des.io.out.ready := io.tl.acquire.ready
  io.tl.acquire.bits := io.tl.acquire.bits.fromBits(des.io.out.bits.data)
}

class ClientUncachedTileLinkIOBidirectionalSerdes(
    w: Int, _clock: Clock = null, _reset: Bool = null)(implicit p: Parameters)
    extends TLSerModule(_clock, _reset)(p) with HasTileLinkSerializers {
  val io = IO(new Bundle {
    val serial = new SerialIO(w)
    val tl_client = new ClientUncachedTileLinkIO
    val tl_manager = Flipped(new ClientUncachedTileLinkIO())
  })

  val serArb = Module(new HellaPeekingArbiter(
    new TLSerChannel, 2, (b: TLSerChannel) => b.last))
  serArb.io.in(0).valid := io.tl_client.grant.valid
  io.tl_client.grant.ready := serArb.io.in(0).ready
  serArb.io.in(0).bits := serialize(io.tl_client.grant.bits)
  serArb.io.in(1).valid := io.tl_manager.acquire.valid
  io.tl_manager.acquire.ready := serArb.io.in(1).ready
  serArb.io.in(1).bits := serialize(io.tl_manager.acquire.bits)

  val ser = Module(new Serializer(w, new TLSerChannel))
  ser.io.in <> serArb.io.out
  io.serial.out <> ser.io.out

  val des = Module(new Deserializer(w, new TLSerChannel))
  des.io.in <> io.serial.in
  io.tl_manager.grant.valid := des.io.out.valid && des.io.out.bits.chan === SER_GNT
  io.tl_manager.grant.bits := io.tl_manager.grant.bits.fromBits(des.io.out.bits.data)
  io.tl_client.acquire.valid := des.io.out.valid && des.io.out.bits.chan === SER_ACQ
  io.tl_client.acquire.bits := io.tl_client.acquire.bits.fromBits(des.io.out.bits.data)
  des.io.out.ready := MuxLookup(des.io.out.bits.chan, false.B, Seq(
    SER_GNT -> io.tl_manager.grant.ready,
    SER_ACQ -> io.tl_client.acquire.ready))
}
