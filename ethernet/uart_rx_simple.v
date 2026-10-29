///////////////////////////////////////////////////////////////////////////////
// uart_rx_simple.v — 8N1 UART RX, simple half-bit start + full-bit sampling
///////////////////////////////////////////////////////////////////////////////

module uart_rx_simple #(
    parameter integer CLK_FREQ  = 125000000,
    parameter integer BAUD_RATE = 9600
)(
    input  wire       clk,
    input  wire       rst,
    input  wire       rx,
    output reg  [7:0] data  = 8'h00,
    output reg        valid = 1'b0
);

    localparam CLKS_PER_BIT  = CLK_FREQ / BAUD_RATE;
    localparam CLKS_HALF_BIT = CLKS_PER_BIT / 2;

    localparam S_IDLE  = 2'd0;
    localparam S_START = 2'd1;
    localparam S_DATA  = 2'd2;
    localparam S_STOP  = 2'd3;

    reg [1:0]  state   = S_IDLE;
    reg [15:0] cnt     = 0;
    reg [2:0]  bit_idx = 0;
    reg [7:0]  shift   = 0;

    always @(posedge clk) begin
        valid <= 1'b0;
        if (rst) begin
            state   <= S_IDLE;
            cnt     <= 0;
            bit_idx <= 0;
            data    <= 0;
        end else begin
            case (state)
                S_IDLE: begin
                    cnt <= 0;
                    if (!rx)
                        state <= S_START;
                end

                S_START: begin
                    if (cnt == CLKS_HALF_BIT - 1) begin
                        cnt   <= 0;
                        state <= !rx ? S_DATA : S_IDLE;
                        bit_idx <= 0;
                    end else
                        cnt <= cnt + 1;
                end

                S_DATA: begin
                    if (cnt == CLKS_PER_BIT - 1) begin
                        cnt            <= 0;
                        shift[bit_idx] <= rx;
                        if (bit_idx == 7)
                            state <= S_STOP;
                        else
                            bit_idx <= bit_idx + 1;
                    end else
                        cnt <= cnt + 1;
                end

                S_STOP: begin
                    if (cnt == CLKS_PER_BIT - 1) begin
                        cnt   <= 0;
                        state <= S_IDLE;
                        if (rx) begin
                            data  <= shift;
                            valid <= 1'b1;
                        end
                    end else
                        cnt <= cnt + 1;
                end
            endcase
        end
    end

endmodule
