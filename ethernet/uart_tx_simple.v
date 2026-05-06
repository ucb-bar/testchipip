///////////////////////////////////////////////////////////////////////////////
// uart_tx_simple.v
//
// Minimal UART transmitter with valid/ready byte interface.
///////////////////////////////////////////////////////////////////////////////

module uart_tx_simple #(
    parameter integer CLK_FREQ  = 125000000,
    parameter integer BAUD_RATE = 115200
)(
    input  wire       clk,
    input  wire       rst,
    input  wire [7:0] data,
    input  wire       valid,
    output wire       ready,
    output wire       tx
);

    localparam integer CLKS_PER_BIT = (CLK_FREQ + (BAUD_RATE/2)) / BAUD_RATE;

    localparam [1:0]
        ST_IDLE  = 2'd0,
        ST_START = 2'd1,
        ST_DATA  = 2'd2,
        ST_STOP  = 2'd3;

    reg [1:0] state_reg = ST_IDLE;
    reg [7:0] data_reg = 8'd0;
    reg [15:0] clk_cnt_reg = 16'd0;
    reg [2:0] bit_idx_reg = 3'd0;
    reg tx_reg = 1'b1;

    assign ready = (state_reg == ST_IDLE);
    assign tx = tx_reg;

    always @(posedge clk) begin
        if (rst) begin
            state_reg <= ST_IDLE;
            data_reg <= 8'd0;
            clk_cnt_reg <= 16'd0;
            bit_idx_reg <= 3'd0;
            tx_reg <= 1'b1;
        end else begin
            case (state_reg)
                ST_IDLE: begin
                    tx_reg <= 1'b1;
                    clk_cnt_reg <= 16'd0;
                    bit_idx_reg <= 3'd0;
                    if (valid) begin
                        data_reg <= data;
                        tx_reg <= 1'b0; // start bit
                        state_reg <= ST_START;
                    end
                end

                ST_START: begin
                    if (clk_cnt_reg == CLKS_PER_BIT-1) begin
                        clk_cnt_reg <= 16'd0;
                        tx_reg <= data_reg[0];
                        bit_idx_reg <= 3'd0;
                        state_reg <= ST_DATA;
                    end else begin
                        clk_cnt_reg <= clk_cnt_reg + 1'b1;
                    end
                end

                ST_DATA: begin
                    if (clk_cnt_reg == CLKS_PER_BIT-1) begin
                        clk_cnt_reg <= 16'd0;
                        if (bit_idx_reg == 3'd7) begin
                            tx_reg <= 1'b1; // stop bit
                            state_reg <= ST_STOP;
                        end else begin
                            bit_idx_reg <= bit_idx_reg + 1'b1;
                            tx_reg <= data_reg[bit_idx_reg + 1'b1];
                        end
                    end else begin
                        clk_cnt_reg <= clk_cnt_reg + 1'b1;
                    end
                end

                default: begin // ST_STOP
                    if (clk_cnt_reg == CLKS_PER_BIT-1) begin
                        clk_cnt_reg <= 16'd0;
                        state_reg <= ST_IDLE;
                        tx_reg <= 1'b1;
                    end else begin
                        clk_cnt_reg <= clk_cnt_reg + 1'b1;
                    end
                end
            endcase
        end
    end

endmodule

