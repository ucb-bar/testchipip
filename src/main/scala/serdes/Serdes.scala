package testchipip.serdes

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

class GenericSerializer[T <: Data](t: T, flitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(t))
    val out = Decoupled(new Flit(flitWidth))
    val busy = Output(Bool())
  })

  val dataBits = t.getWidth.max(flitWidth)
  val dataBeats = (dataBits - 1) / flitWidth + 1
  require(dataBeats > 1)
  val data = Reg(Vec(dataBeats-1, UInt(flitWidth.W)))
  val beat = RegInit(0.U(log2Ceil(dataBeats).W))

  io.in.ready := io.out.ready && beat === 0.U
  io.out.valid := io.in.valid || beat =/= 0.U
  io.out.bits.flit := Mux(beat === 0.U, io.in.bits.asUInt, data(beat-1.U))

  when (io.out.fire) {
    beat := Mux(beat === (dataBeats-1).U, 0.U, beat + 1.U)
    when (beat === 0.U) {
      data := io.in.bits.asTypeOf(Vec(dataBeats, UInt(flitWidth.W))).tail
    }
  }

  io.busy := io.out.valid
}

class GenericDeserializer[T <: Data](t: T, flitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Flit(flitWidth)))
    val out = Decoupled(t)
    val busy = Output(Bool())
  })

  val dataBits = t.getWidth.max(flitWidth)
  val dataBeats = (dataBits - 1) / flitWidth + 1
  require(dataBeats > 1)
  val data = Reg(Vec(dataBeats-1, UInt(flitWidth.W)))
  val beat = RegInit(0.U(log2Ceil(dataBeats).W))

  io.in.ready := io.out.ready || beat =/= (dataBeats-1).U
  io.out.valid := io.in.valid && beat === (dataBeats-1).U
  io.out.bits := Cat(io.in.bits.flit, data.asUInt).asTypeOf(t)

  when (io.in.fire) {
    beat := Mux(beat === (dataBeats-1).U, 0.U, beat + 1.U)
    when (beat =/= (dataBeats-1).U) {
      data(beat) := io.in.bits.flit
    }
  }

  io.busy := beat =/= 0.U
}

class FlitToPhit(flitWidth: Int, phitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Flit(flitWidth)))
    val out = Decoupled(new Phit(phitWidth))
  })
  require(flitWidth >= phitWidth)

  val dataBeats = (flitWidth - 1) / phitWidth + 1
  val data = Reg(Vec(dataBeats-1, UInt(phitWidth.W)))
  val beat = RegInit(0.U(log2Ceil(dataBeats).W))

  io.in.ready := io.out.ready && beat === 0.U
  io.out.valid := io.in.valid || beat =/= 0.U
  io.out.bits.phit := Mux(beat === 0.U, io.in.bits.flit, data(beat-1.U))

  when (io.out.fire) {
    beat := Mux(beat === (dataBeats-1).U, 0.U, beat + 1.U)
    when (beat === 0.U) {
      data := io.in.bits.asTypeOf(Vec(dataBeats, UInt(phitWidth.W))).tail
    }
  }
}

object FlitToPhit {
  def apply(flit: DecoupledIO[Flit], phitWidth: Int): DecoupledIO[Phit] = {
    val flit2phit = Module(new FlitToPhit(flit.bits.flitWidth, phitWidth))
    flit2phit.io.in <> flit
    flit2phit.io.out
  }
}

class PhitToFlit(flitWidth: Int, phitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Phit(phitWidth)))
    val out = Decoupled(new Flit(flitWidth))
  })
  require(flitWidth >= phitWidth)

  val dataBeats = (flitWidth - 1) / phitWidth + 1
  val data = Reg(Vec(dataBeats-1, UInt(phitWidth.W)))
  val beat = RegInit(0.U(log2Ceil(dataBeats).W))

  io.in.ready := io.out.ready || beat =/= (dataBeats-1).U
  io.out.valid := io.in.valid && beat === (dataBeats-1).U
  io.out.bits.flit := (if (dataBeats == 1) io.in.bits.phit else Cat(io.in.bits.phit, data.asUInt))

  when (io.in.fire) {
    beat := Mux(beat === (dataBeats-1).U, 0.U, beat + 1.U)
    when (beat =/= (dataBeats-1).U) {
      data(beat) := io.in.bits.phit
    }
  }
}

object PhitToFlit {
  def apply(phit: DecoupledIO[Phit], flitWidth: Int): DecoupledIO[Flit] = {
    val phit2flit = Module(new PhitToFlit(flitWidth, phit.bits.phitWidth))
    phit2flit.io.in <> phit
    phit2flit.io.out
  }
  def apply(phit: ValidIO[Phit], flitWidth: Int): ValidIO[Flit] = {
    val phit2flit = Module(new PhitToFlit(flitWidth, phit.bits.phitWidth))
    phit2flit.io.in.valid := phit.valid
    phit2flit.io.in.bits := phit.bits
    when (phit.valid) { assert(phit2flit.io.in.ready) }
    val out = Wire(Valid(new Flit(flitWidth)))
    out.valid := phit2flit.io.out.valid
    out.bits := phit2flit.io.out.bits
    phit2flit.io.out.ready := true.B
    out
  }
}

