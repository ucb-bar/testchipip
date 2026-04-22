package testchipip.clocking

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._

/** TileLink memory-mapped clock divider for a serial TL clock domain.
  *
  * Uses ClockDivider (register-based, no clock-gate cells) rather than
  * ClockDivideOrPass so it is safe for FPGA targets that have no EICG_wrapper.
  *
  * The divisor register semantics (matching ClockDivider):
  *   divisor == 0  => divide by 1 (pass-through)
  *   divisor == N  => divide by (N+1)
  *
  * @param address   Base address of the 4 KiB MMIO register region.
  * @param beatBytes Beat (bus data) width in bytes of the attached TL bus.
  * @param divBits   Width of the divisor register (default 8 bits).
  * @param enable    When false the divider is bypassed; for RTL simulation only.
  */
class TLSerialClockDivider(
  address:   BigInt,
  beatBytes: Int,
  divBits:   Int     = 8,
  enable:    Boolean = true)(implicit p: Parameters) extends LazyModule {

  val device    = new SimpleDevice("serial-clk-div-ctrl", Nil)
  val clockNode = ClockGroupIdentityNode()
  val tlNode    = TLRegisterNode(Seq(AddressSet(address, 4096 - 1)), device, "reg/control", beatBytes = beatBytes)

  if (!enable) println(Console.RED + s"""

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

WARNING:

YOU ARE USING TLSerialClockDivider IN
"DISABLED" MODE. THIS SHOULD ONLY BE DONE
FOR RTL SIMULATION.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
""" + Console.RESET)

  lazy val module = new LazyModuleImp(this) {
    require(clockNode.out.size == 1,
      "TLSerialClockDivider: clockNode must have exactly 1 output group")
    val sources = clockNode.in.head._1.member.data.toSeq
    val sinks   = clockNode.out.head._1.member.elements.toSeq
    require(sources.size == sinks.size,
      s"TLSerialClockDivider: source/sink count mismatch: ${sources.size} vs ${sinks.size}")

    val nSinks   = sinks.size
    val busReset = reset

    val regs = (0 until nSinks).map { i =>
      // AsyncResetRegVec runs on the bus (implicit) clock; its reset is busReset.
      val reg = Module(new AsyncResetRegVec(w = divBits, init = 0))

      println(s"  0x${(address + i * 4).toString(16)}: serial TL clock domain '${sinks(i)._1}' divider")

      if (enable) {
        // ClockDivider is register-based (uses ClockFlop only, no EICG_wrapper).
        // Run it synchronously with the source clock.
        val div = withClockAndReset(sources(i).clock, sources(i).reset.asAsyncReset) {
          Module(new testchipip.clocking.ClockDivider(divBits, initDiv = 0))
        }
        // While busReset is asserted the register reads back 0 → divisor=0 → divide-by-1.
        div.io.divisor    := Mux(busReset.asBool, 0.U, reg.io.q)
        sinks(i)._2.clock := div.io.clockOut
        // Reset: identity-node passthrough drives sinks(i)._2.reset; no second driver needed.
      }
      // !enable: identity node passes clock and reset through unchanged.
      reg
    }

    tlNode.regmap((0 until nSinks).map { i =>
      i * 4 -> Seq(RegField.rwReg(divBits, regs(i).io))
    }: _*)
  }
}
