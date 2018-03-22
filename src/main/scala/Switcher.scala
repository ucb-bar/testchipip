package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class TLSplitter(n: Int, params: TLBundleParameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new TLBundle(params))
    val out = Vec(n, new TLBundle(params))
    val sel = Input(UInt(log2Ceil(n).W))
  })

  io.out.zipWithIndex.foreach { case (out, i) =>
    val selected = io.sel === i.U
    out.a.valid := io.in.a.valid && selected
    out.a.bits := io.in.a.bits
    out.d.ready := io.in.d.ready && selected

    out.b.ready := false.B
    out.c.valid := false.B
    out.c.bits  := DontCare
    out.e.valid := false.B
    out.e.bits  := DontCare
  }

  io.in.a.ready := Vec(io.out.map(_.a.ready))(io.sel)
  io.in.d.valid := Vec(io.out.map(_.d.valid))(io.sel)
  io.in.d.bits  := Vec(io.out.map(_.d.bits))(io.sel)

  io.in.b.valid := false.B
  io.in.b.bits  := DontCare
  io.in.c.ready := false.B
  io.in.e.ready := false.B
}

class TLSwitchArbiter(n: Int, edge: TLEdge) extends Module {
  val params = edge.bundle
  val io = IO(new Bundle {
    val in = Flipped(Vec(n, new TLBundle(params)))
    val out = new TLBundle(params)
  })

  val inA = io.in.zipWithIndex.map { case (in, i) =>
    val a = Wire(Decoupled(new TLBundleA(params)))
    a.valid := in.a.valid
    a.bits := in.a.bits
    a.bits.source := Cat(in.a.bits.source, i.U(log2Ceil(n).W))
    in.a.ready := a.ready
    a
  }

  TLArbiter.robin(edge, io.out.a, inA:_*)
  io.out.d.ready := false.B
  io.out.b.ready := false.B
  io.out.c.valid := false.B
  io.out.c.bits  := DontCare
  io.out.e.valid := false.B
  io.out.e.bits  := DontCare

  io.in.zipWithIndex.foreach { case (in, i) =>
    val dId = io.out.d.bits.source(log2Ceil(n)-1, 0)

    in.d.valid := io.out.d.valid && dId === i.U
    in.d.bits := io.out.d.bits
    in.d.bits.source := io.out.d.bits.source >> log2Ceil(n).U
    when (dId === i.U) { io.out.d.ready := in.d.ready }

    in.b.valid := false.B
    in.b.bits  := DontCare
    in.c.ready := false.B
    in.e.ready := false.B
  }
}

class TLSwitcher(
    inPortN: Int,
    outPortN: Seq[Int],
    address: Seq[AddressSet],
    cacheable: Boolean = true,
    executable: Boolean = true,
    beatBytes: Int = 4,
    lineBytes: Int = 64,
    idBits: Int = 6)(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("switcher", Seq("ucb-bar,switcher"))

  val innode = TLManagerNode(Seq.fill(inPortN) {
    TLManagerPortParameters(
      Seq(TLManagerParameters(
        address    = address,
        resources  = device.reg("mem"),
        regionType = if (cacheable) RegionType.UNCACHED
                     else RegionType.UNCACHEABLE,
        executable = executable,
        supportsGet        = TransferSizes(1, lineBytes),
        supportsPutPartial = TransferSizes(1, lineBytes),
        supportsPutFull    = TransferSizes(1, lineBytes))),
      beatBytes = beatBytes)
  })

  val outnodes = outPortN.map(n =>
    TLClientNode(Seq.tabulate(n) { i =>
      TLClientPortParameters(Seq(TLClientParameters(
        name = s"switch_$i", sourceId = IdRange(0, 1 << idBits))))
    })): Seq[TLClientNode]

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val sel = Input(UInt(log2Ceil(outPortN.size).W))
    })

    val splitTL = innode.in.map { case (in, edge) =>
      val splitter = Module(new TLSplitter(outPortN.size, edge.bundle))
      splitter.io.in <> in
      splitter.io.sel := io.sel
      splitter.io.out
    }

    outnodes.zipWithIndex.foreach { case (outnode, i) =>
      val inputs = splitTL.map(all => all(i))
      val nOutputs = outnode.out.size
      val inputsPerOutput = inputs.size / nOutputs
      val arbIdBits = log2Ceil(inputsPerOutput)
      val inParams = innode.in(0)._2.bundle
      val outParams = outnode.out(0)._2.bundle

      require(inputs.size >= nOutputs)
      require((inputs.size % nOutputs) == 0)
      require(outParams.sourceBits >= (inParams.sourceBits + arbIdBits))
      require(outParams.dataBits == inParams.dataBits)

      outnode.out.zipWithIndex.foreach { case ((out, edge), j) =>
        if (inputsPerOutput == 1) {
          out <> inputs(j)
        } else {
          val arbInputs = (j until inputs.size by nOutputs).map(k => inputs(k))
          val arb = Module(new TLSwitchArbiter(arbInputs.size, edge))
          arb.io.in <> arbInputs
          out <> arb.io.out
        }
      }
    }
  }
}
