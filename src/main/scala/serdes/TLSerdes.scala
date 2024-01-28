package testchipip.serdes

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._

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
      val ser = Vec(5, new DecoupledFlitIO(flitWidth))
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

    val out_channels = Seq(
      (manager_tl.e, new TLBundleE(mergedParams)),
      (client_tl.d,  new TLBundleD(mergedParams)),
      (manager_tl.c, new TLBundleC(mergedParams)),
      (client_tl.b,  new TLBundleB(mergedParams)),
      (manager_tl.a, new TLBundleA(mergedParams))
    )
    val out_sers = out_channels.zipWithIndex.map { case ((c,t),i) =>
      val ser = Module(new GenericSerializer(t, flitWidth))
      ser.io.in <> c
      io.ser(i).out <> ser.io.out
      ser
    }
    io.debug.ser_busy := out_sers.map(_.io.busy).orR

    val in_channels = Seq(
      (client_tl.e,  new TLBundleE(mergedParams)),
      (manager_tl.d, new TLBundleD(mergedParams)),
      (client_tl.c,  new TLBundleC(mergedParams)),
      (manager_tl.b, new TLBundleB(mergedParams)),
      (client_tl.a,  new TLBundleA(mergedParams))
    )
    val in_desers = in_channels.zipWithIndex.map { case ((c,t),i) =>
      val des = Module(new GenericDeserializer(t, flitWidth))
      des.io.in <> io.ser(i).in
      c <> des.io.out
      des
    }
    io.debug.des_busy := in_desers.map(_.io.busy).orR
  }
}
