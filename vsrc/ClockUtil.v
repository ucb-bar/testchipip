// Used to build a programmable clock divider, phase detector, etc
module ClockFlop (
    input clockIn,
    input d,
    output reg clockOut
);

    // REPLACE ME WITH A CLOCK CELL IF DESIRED

    always @(posedge clockIn)
        clockOut <= d;

endmodule

module ClockSignalNor2 (
    input clockIn,
    input signalIn,
    output clockOut
);

    // REPLACE ME WITH A CLOCK CELL IF DESIRED

    assign clockOut = !(signalIn || clockIn);

endmodule

module ClockInverter (
    input clockIn,
    output clockOut
);

    // REPLACE ME WITH A CLOCK CELL IF DESIRED

    assign clockOut = !clockIn;

endmodule

module ClockGater (
    input enable,
    input clockIn,
    output clockGated
);

    // REPLACE ME WITH A CLOCK CELL IF DESIRED

    reg qd;

    assign clockGated = qd & clockIn;

    always @(*) begin
        if (!clockIn) begin
            qd <= enable;
        end
    end

endmodule

module ClockMux2 (
    input clocksIn_0,
    input clocksIn_1,
    input sel,
    output clockOut
);

    // REPLACE ME WITH A CLOCK CELL IF DESIRED

    // XXX be careful with this! You can get really nasty short edges if you
    // don't switch carefully
    assign clockOut = sel ? clocksIn_1 : clocksIn_0;

endmodule


module ClockOr2 (
    input clocksIn_0,
    input clocksIn_1,
    output clockOut
);

    // REPLACE ME WITH A CLOCK CELL IF DESIRED

    assign clockOut = clocksIn_0 | clocksIn_1;

endmodule

// Testbench-only stuff
`ifndef SYNTHESIS
`timescale 1ps/1ps
module PeriodMonitor #(
    parameter minperiodps = 1000,
    parameter maxperiodps = 1000    // Set to 0 to ignore
) (
    input clock,
    input enable
);

    time edgetime = 1;
    integer period;

    always @(posedge clock) begin
        period = $time - edgetime;
        edgetime = $time;
        if (period > 0) begin
            if (enable && (period < minperiodps)) begin
                $fatal("PeriodMonitor detected a small period of %d ps at time %0t", period, $time);
            end
            if (enable && (maxperiodps > 0) && (period > maxperiodps)) begin
                $fatal("PeriodMonitor detected a large period of %d ps at time %0t", period, $time);
            end
        end
    end

endmodule

module ClockGenerator #(
    parameter periodps = 1000
) (
    output reg clock
);

    initial begin
        clock = 1'b0;
        forever #(periodps / 2) clock = ~clock;
    end

endmodule

`endif
