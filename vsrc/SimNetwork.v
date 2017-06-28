import "DPI-C" function void network_tick
(
    input  bit     out_valid,
    output bit     out_ready,
    input  longint out_data,
    input  bit     out_last,

    output bit     in_valid,
    input  bit     in_ready,
    output longint in_data,
    output bit     in_last
);

module SimNetwork(
    input         clock,
    input         reset,

    input         net_out_valid,
    output        net_out_ready,
    input  [63:0] net_out_bits_data,
    input         net_out_bits_last,

    output        net_in_valid,
    input         net_in_ready,
    output [63:0] net_in_bits_data,
    output        net_in_bits_last
);

    bit __out_ready;
    bit __in_valid;
    longint __in_data;
    bit __in_last;

    reg        __out_ready_reg;
    reg        __in_valid_reg;
    reg [63:0] __in_data_reg;
    reg        __in_last_reg;

    always @(posedge clock) begin
        if (reset) begin
            __out_ready = 0;
            __in_valid = 0;
            __in_data = 0;
            __in_last = 0;

            __out_ready_reg <= 1'b0;
            __in_valid_reg <= 1'b0;
            __in_data_reg <= 64'b0;
            __in_last_reg <= 1'b0;
        end else begin
            network_tick(
                net_out_valid,
                __out_ready,
                net_out_bits_data,
                net_out_bits_last,

                __in_valid,
                net_in_ready,
                __in_data,
                __in_last);

            __out_ready_reg <= __out_ready;
            __in_valid_reg <= __in_valid;
            __in_data_reg <= __in_data;
            __in_last_reg <= __in_last;
        end
    end

    assign net_out_ready = __out_ready_reg;
    assign net_in_valid = __in_valid_reg;
    assign net_in_bits_data = __in_data_reg;
    assign net_in_bits_last = __in_last_reg;

endmodule
