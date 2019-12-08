`define ADDR_SZ 32
`define DATA_SZ 64
`define DATA_SZ_B (`DATA_SZ / 8)
`define DATA_SZ_LOG2 3 // should be log2(`DATA_SZ_B)

import "DPI-C" function int dromajo_override_mem(
    input int     dut_addr,
    input byte    dut_size,
    input longint dut_wdata
);

module SimDromajoWriter
(
    input clock,
    input reset,

    input                     valid,
    input [`ADDR_SZ-1:0]      addr,
    input [`DATA_SZ_LOG2-1:0] size,
    input [`DATA_SZ-1:0]      data
);

    int __fail;

    always @(posedge clock) begin
        if (!reset) begin
            if (valid) begin
                __fail = dromajo_override_mem(
                    addr,
                    size,
                    data);
                if (__fail != 0) begin
                    $display("FAIL: Dromajo Simulation Failed Writing To Mem with exit code: %d", __fail);
                    $fatal;
                end
            end
        end
    end

endmodule
