package testchipip

import chisel3._
import chisel3.util_
import freechips.rocketchip.diplomacy.TLBundleParameters
import org.json4s.jackson.Serialization

case class LbwifParams(
  serialWidth: Int,
  sourceId: Int,
  clientParams: TLBundleParameters,
  clientBeatBytes: Int,
  clientTLUH: Boolean,
  clientTLC: Boolean,
  managerParams: TLBundleParameters,
  managerBeatBytes: Int,
  managerTLUH: Boolean,
  managerTLC: Boolean
) {

  def serialize(): String = Serialization.write(this)

  def createManagerParams(): TLManagerParameters

}

class LbwifNarrowIO(val lp: LbwifParams) extends SerialIO(lp.serialWidth) {

  def makeAndConnectHarness(): HarnessLbwif = {
    val lbwif = Module(new HarnessLbwif(lp))
    lbwif.io.narrow <> this
    lbwif
  }

}

object LbwifParams {

  def deserialize(str: String): LbwifParams = Serialization.read[LbwifParams](str)

}

/*
class DiplomaticChipLbwif extends LazyModule {

  // TODO

}
*/

class HarnessLbwif(lp: LbwifParams) extends Module {

  val io = IO(new Bundle {
    val clientTL = TLBundle(lp.clientParams)
    // We are intentionally not using the name "serial" because it is often ambiguous which protocol it's talking about
    val tether = new SerialIO(SerialAdapter.SERIAL_IF_WIDTH.W)
    val narrow = new LbwifNarrowIO(lp)
  })

  require(lp.managerBeatBytes == lp.clientBeatBytes, "This implementation currently requires the manager and client beatBytes to be the same.")

  val serdesser = Module(new TLSerdesser(lp.serialWidth, lp.clientParams, lp.managerParams, lp.managerBeatBytes))
  // Note we are passing lp.managerParams to the clientParams of the SerialAdapterModule so that it matches the serdesser manager port params
  val adapter = Module(new SerialAdapterModule(lp.managerParams, lp.sourceId))

  serdesser.io.managerTL <> adapter.io.clientTL
  adapter.io.serial <> io.tether
  serdesser.io.clientTL <> io.clientTL
  serdesser.io.narrow <> io.narrow

}

/*
class DiplomaticHarnessLbwif(lp: LbwifParams) extends LazyModule {
  // TODO
}
*/
