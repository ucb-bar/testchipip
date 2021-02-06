package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._

object ArbiterCtrl {
  def apply(request: Seq[Bool]): Seq[Bool] = request.length match {
    case 0 => Seq()
    case 1 => Seq(true.B)
    case _ => true.B +: request.tail.init.scanLeft(request.head)(_ || _).map(!_)
  }
}

class DynamicRRArbiter[T <: Data](gen: T, n: Int)
    extends LockingArbiterLike[T](gen, n, 1, None) {
  lazy val lastGrant = RegEnable(io.chosen, io.out.fire() || io.in.map(_.valid).reduce(_||_))
  lazy val grantMask = (0 until n).map(_.asUInt > lastGrant)
  lazy val validMask = io.in zip grantMask map { case (in, g) => in.valid && g }

  override def grant: Seq[Bool] = {
    val ctrl = ArbiterCtrl((0 until n).map(i => validMask(i)) ++ io.in.map(_.valid))
    (0 until n).map(i => ctrl(i) && grantMask(i) || ctrl(i + n))
  }

  override lazy val choice = WireDefault((n-1).asUInt)
  for (i <- n-2 to 0 by -1)
    when (io.in(i).valid) { choice := i.asUInt }
  for (i <- n-1 to 1 by -1)
    when (validMask(i)) { choice := i.asUInt }
}

class TLBankedBuffer(
  a: BufferParams,
  b: BufferParams,
  c: BufferParams,
  d: BufferParams,
  e: BufferParams)(implicit p: Parameters) extends LazyModule
{
  def this(ace: BufferParams, bd: BufferParams)(implicit p: Parameters) = this(ace, bd, ace, bd, ace)
  def this(abcde: BufferParams)(implicit p: Parameters) = this(abcde, abcde)
  def this()(implicit p: Parameters) = this(BufferParams.default)

  val node = new TLBufferNode(a, b, c, d, e)
  val nBanks = p(BankedL2Key).nBanks
  println("Creating TLBankedBuffer")

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      in.a.ready := false.B
      val aArb = Module(new DynamicRRArbiter(new TLBundleA(in.params), nBanks))
      out.a <> aArb.io.out
      val outD = d(out.d)
      in.d.valid := outD.valid
      in.d.bits  := outD.bits
      outD.ready := in.d.ready

      if (edgeOut.manager.anySupportAcquireB && edgeOut.client.anySupportProbe) {
        val outB = b(out.b)
        in.b.valid := outB.valid
        in.b.bits  := outB.bits
        outB.ready := in.b.ready
        val inC = c(in.c)
        out.c.valid := inC.valid
        out.c.bits  := inC.bits
        inC.ready   := out.c.ready
        val inE = e(in.e)
        out.e.valid := inE.valid
        out.e.bits  := inE.bits
        inE.ready   := out.e.ready
      } else {
        in.b.valid := false.B
        in.c.ready := true.B
        in.e.ready := true.B
        out.b.ready := true.B
        out.c.valid := false.B
        out.e.valid := false.B
      }

      for (w <- 0 until nBanks) {
        def isThisBank(addr: UInt) = (addr >> log2Ceil(p(CacheBlockBytes)))(log2Ceil(nBanks)-1,0) === w.U
        val inA = Wire(Decoupled(new TLBundleA(in.params)))
        inA.valid := in.a.valid && isThisBank(in.a.bits.address)
        inA.bits  := in.a.bits
        when (inA.fire()) {
          in.a.ready := true.B
        }
        aArb.io.in(w) <> a(inA)
      }
    }
  }
}

object TLBankedBuffer
{
  def apply()                                   (implicit p: Parameters): TLNode = apply(BufferParams.default)
  def apply(abcde: BufferParams)                (implicit p: Parameters): TLNode = apply(abcde, abcde)
  def apply(ace: BufferParams, bd: BufferParams)(implicit p: Parameters): TLNode = apply(ace, bd, ace, bd, ace)
  def apply(
      a: BufferParams,
      b: BufferParams,
      c: BufferParams,
      d: BufferParams,
      e: BufferParams)(implicit p: Parameters): TLNode =
  {
    if (p(BankedL2Key).nBanks > 1) {
      val buffer = LazyModule(new TLBankedBuffer(a, b, c, d, e))
      buffer.node
    } else {
      TLBuffer(a, b, c, d, e)
    }
  }
}

case class TileMasterPortParamsWithBankedBuffers(
  depth: Int = 0,
  cork: Option[Boolean] = None,
  where: TLBusWrapperLocation = SBUS
) extends TilePortParamsLike {
  def injectNode(context: Attachable)(implicit p: Parameters): TLNode = {
    (TLBankedBuffer(BufferParams(depth)) :=* cork.map { u => TLCacheCork(unsafe = u) } .getOrElse { TLTempNode() })
  }
}

