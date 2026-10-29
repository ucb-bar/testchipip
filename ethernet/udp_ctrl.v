// udp_ctrl.v
//
// Placeholder receiver for UDP_PORT+1 control words.
// Replace the body with real control logic as needed.
// Currently drains words immediately (ctrl_in_ready = 1 always).

`resetall
`timescale 1ns / 1ps
`default_nettype none

module udp_ctrl #(
    parameter SERIAL_WIDTH = 32
)(
    input  wire                    clk,
    input  wire                    rst,

    // Control word stream (from udp_payload_to_tsi_serial ctrl_out_*)
    input  wire [SERIAL_WIDTH-1:0] ctrl_in_bits,
    input  wire                    ctrl_in_valid,
    output wire                    ctrl_in_ready
);

    // Drain words immediately — replace with real logic.
    assign ctrl_in_ready = 1'b1;

endmodule // udp_ctrl

`resetall
