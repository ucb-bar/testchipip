package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, ValName}
import freechips.rocketchip.util.AsyncResetReg
import freechips.rocketchip.tilelink._

class ResetSync(c: Clock, lat: Int = 2) extends Module(_clock = c) {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val reset_sync = Output(Bool())
  })
  io.reset_sync := ShiftRegister(io.reset,lat)
}

object ResetSync {
  def apply(r: Bool, c: Clock): Bool = {
    val sync = Module(new ResetSync(c,2))
    sync.suggestName("resetSyncInst")
    sync.io.reset := r
    sync.io.reset_sync
  }
}

// a counter that clock gates most of its MSBs using the LSB carry-out
// uses asyncresetregs to make it easy for cross-clock domain work
case class AsyncWideCounter(width: Int, inc: UInt = 1.U, reset: Boolean = true)
{
  private val isWide = width > 2*inc.getWidth
  private val smallWidth = if (isWide) inc.getWidth max log2Up(width) else width
  private val widerNextSmall = Wire(UInt((smallWidth + 1).W))
  private val nextSmall = Wire(UInt(smallWidth.W))
  private val small = if (reset) AsyncResetReg(nextSmall, 0, "smallReg") else AsyncResetReg(nextSmall, "smallReg")
  widerNextSmall := small +& inc
  nextSmall := widerNextSmall

  private val large = if (isWide) {
    val nextR = Wire(UInt((width - smallWidth).W))
    val r = if (reset) AsyncResetReg(nextR, 0, "rReg") else AsyncResetReg(nextR, "rReg")
    when (widerNextSmall(smallWidth)) {
      nextR := r +& 1.U
    }.otherwise {
      nextR := r
    }
    r
  } else null

  val value = if (isWide) large ## small else small
  lazy val carryOut = {
    val lo = (small ^ widerNextSmall) >> 1
    if (!isWide) lo else {
      val hi = Mux(widerNextSmall(smallWidth), large ^ (large +& 1.U), 0.U) >> 1
      hi ## lo
    }
  }
}

// As WideCounter, but it's a module so it can take arbitrary clocks
class WideCounterModule(w: Int, inc: UInt = 1.U, reset: Boolean = true, clockSignal: Clock = null, resetSignal: Bool = null)
    extends Module(Option(clockSignal), Option(resetSignal)) {
  val io = IO(new Bundle {
    val value = Output(UInt(w.W))
  })
  io.value := AsyncWideCounter(w, inc, reset).value
}

object WideCounterModule {
  def apply(w: Int, c: Clock, r: Bool) = {
    val counter = Module(new WideCounterModule(w, clockSignal = c, resetSignal = r))
    counter.suggestName("wideCounterInst")
    counter.io.value
  }
  def apply(w: Int, c: Clock) = {
    val counter = Module(new WideCounterModule(w, clockSignal = c))
    counter.suggestName("wideCounterInst")
    counter.io.value
  }
}

// Use gray coding to safely synchronize a word across a clock crossing.
// This should be placed in the receiver's clock domain.
class WordSync[T <: Data](gen: T, lat: Int = 2) extends Module {
  val size = gen.getWidth
  val io = IO(new Bundle {
    val in = Flipped(gen.chiselCloneType)
    val out = gen.chiselCloneType
    val tx_clock = Input(Clock())
  })
  val bin2gray = Module(new BinToGray(gen,io.tx_clock))
  bin2gray.io.bin := io.in
  val out_gray = ShiftRegister(bin2gray.io.gray, lat)
  io.out := gen.cloneType.fromBits(
    (0 until size)
      .map(out_gray.asUInt >> _.U)
      .reduceLeft((a: UInt, b: UInt) => a^b))
}

class BinToGray[T <: Data](gen: T, c: Clock) extends Module(_clock = c) {
  val io = IO(new Bundle {
    val bin = Flipped(gen.chiselCloneType)
    val gray = UInt(gen.getWidth.W)
  })
  io.gray := Reg(next=(io.bin.asUInt ^ (io.bin.asUInt >> 1.U)))
}

object WordSync {
  def apply[T <: Data](word: T, c: Clock) = {
    val sync = Module(new WordSync(word))
    sync.suggestName("wordSyncInst")
    sync.io.tx_clock := c
    sync.io.in := word
    sync.io.out
  }
  def apply[T <: Data](gen: T, word: Data, c: Clock, lat: Int = 2) = {
    val sync = Module(new WordSync(gen,lat))
    sync.suggestName("wordSyncInst")
    sync.io.tx_clock := c
    sync.io.in := word
    sync.io.out
  }
}

object TLHelper {
  def makeClientNode(name: String, sourceId: IdRange)
                    (implicit valName: ValName): TLClientNode =
    makeClientNode(TLClientParameters(name, sourceId))

  def makeClientNode(params: TLClientParameters)
                    (implicit valName: ValName): TLClientNode =
    TLClientNode(Seq(TLClientPortParameters(Seq(params))))

  def makeManagerNode(beatBytes: Int, params: TLManagerParameters)
                     (implicit valName: ValName): TLManagerNode =
    TLManagerNode(Seq(TLManagerPortParameters(Seq(params), beatBytes)))

  def latency(lat: Int, node: TLOutwardNode)(implicit p: Parameters): TLOutwardNode =
    TLBuffer.chain(lat).foldRight(node)(_ :=* _)
}

class DecoupledMux[T <: Data](typ: T, n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(n, Decoupled(typ)))
    val out = Decoupled(typ)
    val sel = Input(UInt(log2Ceil(n).W))
  })

  if (n > 1) {
    io.out.valid := io.in(io.sel).valid
    io.out.bits := io.in(io.sel).bits
    io.in.zipWithIndex.foreach { case (in, i) =>
      in.ready := io.out.ready && io.sel === i.U
    }
  } else { io.out <> io.in.head }
}

object DecoupledMux {
  def apply[T <: Data](sel: UInt, in: Seq[DecoupledIO[T]]): DecoupledIO[T] = {
    val mux = Module(new DecoupledMux(in(0).bits.cloneType, 2))
    mux.io.sel := sel
    mux.io.in <> in
    mux.io.out
  }

  def apply[T <: Data](sel: Bool, a: DecoupledIO[T], b: DecoupledIO[T]): DecoupledIO[T] =
    apply(sel, Seq(b, a))
}
