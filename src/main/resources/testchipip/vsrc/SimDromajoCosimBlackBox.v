import "DPI-C" function int dromajo_init(
    input string bootrom_file,
    input string dtb_file,
    input string binary_file
);

import "DPI-C" function int dromajo_step(
    input bit     valid,
    input int     hartid,
    input longint dut_pc,
    input int     dut_insn,
    input longint dut_wdata,
    input longint mstatus,
    input bit     check,
    input bit     wdata_valid,
    input int     wdata_dest,
    input bit     insn_writes_back,
    input int     insn_wdata_dest);

import "DPI-C" function void dromajo_raise_trap(
    input int     hartid,
    input longint cause
);

module SimDromajoCosimBlackBox
    #(parameter COMMIT_WIDTH, RD, XLEN, INST_BITS, HARTID_LEN)
(
    input clock,
    input reset,

    input [          (COMMIT_WIDTH) - 1:0] valid  ,
    input [            (HARTID_LEN) - 1:0] hartid ,
    input [     (XLEN*COMMIT_WIDTH) - 1:0] pc     ,
    input [(INST_BITS*COMMIT_WIDTH) - 1:0] inst   ,
    input [     (XLEN*COMMIT_WIDTH) - 1:0] wdata  ,
    input [     (XLEN*COMMIT_WIDTH) - 1:0] mstatus,
    input [          (COMMIT_WIDTH) - 1:0] check  ,

    input [          (COMMIT_WIDTH) - 1:0] wdata_valid,
    input [       (RD*COMMIT_WIDTH) - 1:0] wdata_dest,
    input [          (COMMIT_WIDTH) - 1:0] insn_writes_back,
    input [       (RD*COMMIT_WIDTH) - 1:0] insn_wdata_dest,

    input           int_xcpt,
    input [XLEN - 1:0] cause
);
    string __dtb_file, __rom_file, __bin_file, __mmio_start, __mmio_end;
    int __itr, __fail;

    initial begin
        // optional dtb param
        if (!$value$plusargs("drj_dtb=%s", __dtb_file)) begin
            __dtb_file = "";
        end
        if (!$value$plusargs("drj_rom=%s", __rom_file)) begin
            __rom_file = "";
        end
        if (!$value$plusargs("drj_bin=%s", __bin_file)) begin
            __bin_file = "";
        end
        __fail = dromajo_init(
            __rom_file,
            __dtb_file,
            __bin_file);
        if (__fail != 0) begin
            $display("FAIL: Dromajo Simulation Failed");
            $fatal;
        end
    end

    always @(posedge clock) begin
        if (!reset) begin
            for (__itr=0; __itr<COMMIT_WIDTH; __itr=__itr+1) begin
                if (valid[__itr]) begin
                    __fail = dromajo_step(
                        valid[__itr],
                        hartid,
                        pc[((__itr+1)*XLEN - 1)-:XLEN],
                        inst[((__itr+1)*INST_BITS - 1)-:INST_BITS],
                        wdata[((__itr+1)*XLEN - 1)-:XLEN],
                        mstatus[((__itr+1)*XLEN - 1)-:XLEN],
                        check[__itr],
                        wdata_valid[__itr],
                        wdata_dest[((__itr+1)*RD - 1)-:RD],
                        insn_writes_back[__itr],
                        insn_wdata_dest[((__itr+1)*RD - 1)-:RD]);
                    if (__fail != 0) begin
                        $display("FAIL: Dromajo Simulation Failed with exit code: %d", __fail);
                        $fatal;
                    end
                end
            end

            if (int_xcpt) begin
                dromajo_raise_trap(
                    hartid,
                    cause
                );
            end
        end
    end

endmodule
