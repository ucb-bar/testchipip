package testchipip

import chisel3._
import chisel3.util._
import chisel3.core.IntParam
import chisel3.experimental.{Analog, withClock, withClockAndReset}

import freechips.rocketchip.util.AsyncResetSynchronizerShiftReg

// A clock flip-flop
class ClockFlop extends BlackBox with HasBlackBoxResource {
    val io = IO(new Bundle {
        val clockIn = Input(Clock())
        val d = Input(Bool())
        val clockOut = Output(Clock())
    })

    setResource("/testchipip/vsrc/ClockUtil.v")
}

// An integrated clock gating cell, clock is enabled when enable is high
class ClockGater extends BlackBox with HasBlackBoxResource {
    val io = IO(new Bundle {
        val clockIn = Input(Clock())
        val enable = Input(Bool())
        val clockGated = Output(Clock())
    })

    setResource("/testchipip/vsrc/ClockUtil.v")
}

// A clock OR module. It does what you'd think.
class ClockOr2 extends BlackBox with HasBlackBoxResource {
    val io = IO(new Bundle {
        val clocksIn = Input(Vec(2, Clock()))
        val clockOut = Output(Clock())
    })

    setResource("/testchipip/vsrc/ClockUtil.v")
}

class ClockInverter extends BlackBox with HasBlackBoxResource {
    val io = IO(new Bundle {
        val clockIn = Input(Clock())
        val clockOut = Output(Clock())
    })

    setResource("/testchipip/vsrc/ClockUtil.v")
}

object ClockInverter {
    def apply(in: Clock): Clock = {
        val x = Module(new ClockInverter)
        x.io.clockIn := in
        x.io.clockOut
    }
}

class ClockSignalNor2 extends BlackBox with HasBlackBoxResource {
    val io = IO(new Bundle {
        val clockIn = Input(Clock())
        val signalIn = Input(Bool())
        val clockOut = Output(Clock())
    })

    setResource("/testchipip/vsrc/ClockUtil.v")
}

object ClockSignalNor2 {
    def apply(in: Clock, sig: Bool): Clock = {
        val x = Module(new ClockSignalNor2)
        x.io.clockIn := in
        x.io.signalIn := sig
        x.io.clockOut
    }
}

// XXX A clock multiplexer that does NOT safely transition between clocks
// Be very careful using this!
class ClockMux2 extends BlackBox with HasBlackBoxResource {

    val io = IO(new Bundle {
        val clocksIn = Input(Vec(2, Clock()))
        val sel = Input(Bool())
        val clockOut = Output(Clock())
    })

    setResource("/testchipip/vsrc/ClockUtil.v")
}

// A clock mux that's safe to switch during execution
// n: Number of inputs
// depth: Synchronizer depth
class ClockMutexMux(val n: Int, depth: Int) extends RawModule {

    val io = IO(new Bundle {
        val clocksIn = Input(Vec(n, Clock()))
        val clockOut = Output(Clock())
        val resetAsync = Input(Bool())
        val sel = Input(UInt(log2Ceil(n).W))
    })

    val andClocks = io.clocksIn.map(x => ClockSignalNor2(ClockInverter(x), io.resetAsync))

    val syncs  = andClocks.map { c => withClockAndReset(c, io.resetAsync) { Module(new AsyncResetSynchronizerShiftReg(1, 3, 0)) } }
    val gaters = andClocks.map { c =>
        val g = Module(new ClockGater)
        g.io.clockIn := c
        g
    }

    syncs.zip(gaters).foreach { case (s, g) => g.io.enable := s.io.q }

    syncs.zipWithIndex.foreach { case (s, i) => s.io.d := (io.sel === i.U) && !(syncs.zipWithIndex.filter(_._2 != i).map(_._1.io.q.toBool).reduce(_||_)) }

    io.clockOut := clockOrTree(gaters.map(_.io.clockGated))(0)

    def clockOrTree(in: Seq[Clock]): Seq[Clock] = {
        if (in.length == 1) {
            return in
        } else {
            return clockOrTree(Seq.fill(in.length / 2)(Module(new ClockOr2))
                .zipWithIndex.map({ case (or, i) =>
                    or.io.clocksIn(0) := in(2*i)
                    or.io.clocksIn(1) := in(2*i+1)
                    or.io.clockOut
                }) ++ (if(in.length % 2 == 1) Seq(in.last) else Seq()))
        }
    }

}

object ClockMutexMux {

    def apply(clocks: Seq[Clock], depth: Int = 3) = {
        val mux = Module(new ClockMutexMux(clocks.length, depth))
        mux.io.clocksIn := VecInit(clocks)
        mux
    }

}

// Programmable clock divider which divides by (N+1) (0 stops clock)
// This is fully synchronous and doesn't need any async resets
// The implicit clock of this thing is the fast one
class ClockDivider(width: Int) extends Module {

    val io = IO(new Bundle {
        val divisor = Input(UInt(width.W))
        val clockOut = Output(Clock())
    })

    val clockReg = Module(new ClockFlop)

    val divisorReg = RegInit(0.U(width.W))
    val count = RegInit(0.U(width.W))

    clockReg.io.clockIn := this.clock
    clockReg.io.d := count < ((divisorReg >> 1) +& 1.U)
    io.clockOut := clockReg.io.clockOut

    when (count === divisorReg) {
        count := 0.U
        // Only change divisorReg when we're done with a full period
        divisorReg := io.divisor
    } .otherwise {
        count := count + 1.U
    }

}

object withGatedClock {
    def apply[T](clock: Clock, enable: Bool)(block: => T): T = {
        val gater = Module(new ClockGater)
        gater.io.enable := enable
        gater.io.clockIn := clock
        withClock(gater.io.clockGated)(block)
    }
}

object withGatedClockAndReset {
    def apply[T](clock: Clock, enable: Bool, reset: Bool)(block: => T): T = {
        val gater = Module(new ClockGater)
        gater.io.enable := enable
        gater.io.clockIn := clock
        withClockAndReset(gater.io.clockGated, reset)(block)
    }
}
