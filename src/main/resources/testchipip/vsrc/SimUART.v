`define DATA_WIDTH 8

import "DPI-C" function void uart_init(
    input  string  filename,
    input  int     uartno
);

import "DPI-C" function void uart_tick
(
    input  bit     serial_out_valid,
    output bit     serial_out_ready,
    input  byte    serial_out_bits,

    output bit     serial_in_valid,
    input  bit     serial_in_ready,
    output byte    serial_in_bits
);

module SimUART #(UARTNO) (
    input              clock,
    input              reset,

    input                    serial_out_valid,
    output                   serial_out_ready,
    input  [`DATA_WIDTH-1:0] serial_out_bits,

    output                   serial_in_valid,
    input                    serial_in_ready,
    output [`DATA_WIDTH-1:0] serial_in_bits
);

    bit __in_valid;
    bit __out_ready;
    byte __in_bits;
    string __uartlog;
    int __uartno;

    initial begin
        $value$plusargs("uartlog=%s", __uartlog);
        uart_init(__uartlog, __uartno);
    end

    reg __in_valid_reg;
    reg __out_ready_reg;
    reg [`DATA_WIDTH-1:0] __in_bits_reg;

    assign serial_in_valid  = __in_valid_reg;
    assign serial_in_bits   = __in_bits_reg;
    assign serial_out_ready = __out_ready_reg;

    // Evaluate the signals on the positive edge
    always @(posedge clock) begin
        if (reset) begin
            __in_valid = 0;
            __out_ready = 0;

            __in_valid_reg <= 0;
            __in_bits_reg <= 0;
            __out_ready_reg <= 0;
            __uartno = UARTNO;
        end else begin
            uart_tick(
                serial_out_valid,
                __out_ready,
                serial_out_bits,
                __in_valid,
                serial_in_ready,
                __in_bits
            );

            __out_ready_reg <= __out_ready;
            __in_valid_reg  <= __in_valid;
            __in_bits_reg   <= __in_bits;
        end
    end

endmodule
