package testchipip

import chisel3._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.{CacheBlockBytes, ExtMem}
import freechips.rocketchip.diplomacy.{TransferSizes, MemoryDevice, IdRange}
import freechips.rocketchip.tilelink.{TLManagerParameters, TLClientParameters}
import freechips.rocketchip.util.{FastToSlow, SlowToFast}

case class TSIParams(
  width: Int = 8,
  minFreqHz: BigInt = 50000,
  initFreqHz: BigInt = 500000,
  configurable: Boolean = true
) {
  require(minFreqHz > 0, "Minimum clock frequency must be a positive integer")
  require(initFreqHz > minFreqHz, "Initial clock frequency must be larger than the minimum")
}

case object TSITargetKey extends Field[TSIParams](TSIParams())

trait HasPeripheryTargetTSI { this: BaseSubsystem =>

  val ckDivBits = ClockDivider.divisorBits(mbus.dtsFrequency, p(TSITargetKey).minFreqHz)
  val ckDivInit = mbus.dtsFrequency / p(TSITargetKey).initFreqHz
  if (mbus.dtsFrequency % p(TSITargetKey).initFreqMHz != 0) {
    println(s"[${Console.YELLOW}warn${Console.RESET}] the mbus frequency ${mbus.dtsFrequency} is not an integer multiple of the TSI init frequency ${p(TSITargetKey).minFreqHz}")
  }

  require(p(ExtMem).isDefined, "LBWIF requires external memory")

  val tsiManagerParams = TLManagerParameters(
    address = AddressSet.misaligned(p(ExtMem).get.master.base, p(ExtMem).get.master.size),
    resources = (new MemoryDevice).reg,
    regionType = RegionType.UNCACHED, // TODO support off-chip coherency?
    executable = true,
    fifoId = Some(0),
    supportsGet        = TransferSizes(1, p(CacheBlockBytes)),
    supportsPutFull    = TransferSizes(1, p(CacheBlockBytes)),
    supportsAcquireT   = TransferSizes(1, p(CacheBlockBytes)), // TODO - do we want to support this for TLU?
    supportsAcquireB   = TransferSizes(1, p(CacheBlockBytes)), // TODO - do we want to support this for TLU?
    supportsArithmetic = TransferSizes(1, p(CacheBlockBytes)), // TODO - do we want this?
    supportsLogical    = TransferSizes(1, p(CacheBlockBytes)), // TODO - do we want this?
    supportsPutPartial = TransferSizes(1, p(CacheBlockBytes)), // TODO - do we want this?
    supportsHint       = TransferSizes(1, p(CacheBlockBytes))) // TODO - do we want this?
  val tsiClientParams = TLClientParameters(
    name = "tsi_control",
    sourceId = IdRange(0, 1),
    requestFifo = true)

  val serdes = LazyModule(new TLSerdesser(p(TSITargetKey), tsiClientParams, tsiManagerParams, p(ExtMem).get.master.beatBytes, 0) // TODO what should endSinkId be?
  val tsiClientSource = TLRationalCrossingSource(SlowToFast)
  val tsiManagerSink = TLRationalCrossingSink(FastToSlow)

  // TODO paramterize TLBuffering
  (tsi.managerNode
    := tsiManagerSink.node
    := TLRationalCrossingSource()
    := mbus.coupleTo("tsi_memory") { TLBuffer() := _ })

  // TODO paramterize TLBuffering
  (fbus.fromMaster()()
    := TLBuffer()
    := TLWidthWidget(p(ExtMem).get.master.beatBytes)
    := TLRationalCrossingSink()
    := tsiClientSource.node
    := tsi.clientNode)

  InModuleBody {
    val tsiClockDivider = Module(new ClockDivider(ckDivBits)).suggestName("tsiClockDivider")
    if (p(TSITargetKey).configurable) {
      val tsiClockDivisorReg = RegInit(ckDivInit.U(ckDivBits.W)).suggestName("tsiClockDivisorReg")
      tsiClockDivider.io.divisor := tsiClockDivisorReg
    } else {
      tsiClockDivider.io.divisor := ckDivInit.U(ckDivBits.W)
    }

    val tsiClock = tsiClockDivider.io.clockOut
    val tsiReset = ResetCatchAndSync(tsiClock, reset.toBool) // TODO does this handle AsyncReset correctly?

    tsi.clock := tsiClock
    tsi.reset := tsiReset
    tsiClientSource.clock := tsiClock
    tsiClientSource.reset := tsiReset
    tsiManagerSink.clock := tsiClock
    tsiManagerSink.reset := tsiReset

  }

}
