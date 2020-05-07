
import "DPI-C" function void spi_flash_tick(
    input longint ptr,
    input bit sck,
    input bit cs,
    input bit reset,
    input byte dq_in,
    output byte dq_out,
    output byte dq_drive
);

import "DPI-C" function longint spi_flash_init(
    input string filename,
    input int max_addr
);

module SimSPIFlashModel #(
    parameter MAX_ADDR,
    parameter ID
) (
    input sck,
    input cs_0,
    input reset,
    inout dq_0,
    inout dq_1,
    inout dq_2,
    inout dq_3
);

    // Stores the file name for the binary memory contents of the flash
    string filename;

    // This is a pointer to the C SPIFlashMem object
    longint dev_ptr;

    initial begin
        if (MAX_ADDR > 33'h0_ffff_ffff) begin /* verilator lint_off CMPCONST */
            // Workaround for verilator to write to STDERR
            $fwrite(32'h80000002, "SimSPIFlashModel supports only 32-bit memory addressing.\n");
            $fatal;
        end
        // +spiflash0=/path/to/file for SPI flash 0, etc.
        if ($value$plusargs($sformatf("spiflash%0d=%%s", ID), filename)) begin
            dev_ptr = spi_flash_init(filename, MAX_ADDR);
        end else begin
            // Workaround for verilator to write to STDERR
            $fwrite(32'h80000002, "No memory image provided for SPI flash %0d. Use +spiflash%0d=<file> to specify.\n", ID, ID);
            $fatal;
        end
    end

    logic [7:0] dpi_dq_in;    // 4 MSBs are unused
    logic [7:0] dpi_dq_out;   // 4 MSBs are unused
    logic [7:0] dpi_dq_drive; // 4 MSBs are unused

    assign dq_3 = dpi_dq_drive[3] ? dpi_dq_out[3] : 1'bz;
    assign dq_2 = dpi_dq_drive[2] ? dpi_dq_out[2] : 1'bz;
    assign dq_1 = dpi_dq_drive[1] ? dpi_dq_out[1] : 1'bz;
    assign dq_0 = dpi_dq_drive[0] ? dpi_dq_out[0] : 1'bz;

    always @(posedge sck or negedge sck or posedge reset or posedge cs_0) begin
        spi_flash_tick(dev_ptr, sck, cs_0, reset, dpi_dq_in, dpi_dq_out, dpi_dq_drive);
    end

endmodule
