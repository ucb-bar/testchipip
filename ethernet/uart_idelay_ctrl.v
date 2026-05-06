///////////////////////////////////////////////////////////////////////////////
// uart_idelay_ctrl.v
//
// UART RX+TX controller for runtime IDELAYE2 tap adjustment.
//
// RX: receive one byte
//   - 0xFF        : reply with phy_link_up status byte (0x00 or 0x01) via TX
//   - anything else: update tap_reg[4:0] = data[4:0], pulse tap_ld
//
// TX: send one byte in response to 0xFF query.
//
// 8N1, CLK_FREQ/BAUD_RATE configurable.
///////////////////////////////////////////////////////////////////////////////

module uart_idelay_ctrl #(
    parameter CLK_FREQ  = 125000000,
    parameter BAUD_RATE = 115200,
    parameter [4:0] DEFAULT_TAPS = 5'd0
)(
    input  wire       clk,
    input  wire       rst,

    input  wire       rx,
    output wire       tx,

    input  wire       phy_link_up,

    output reg  [4:0] tap_reg,
    output reg        tap_ld
);

    localparam CLKS_PER_BIT  = CLK_FREQ / BAUD_RATE;
    localparam CLKS_HALF_BIT = CLKS_PER_BIT / 2;

    // =========================================================================
    // UART RX
    // =========================================================================

    localparam RX_IDLE  = 2'd0;
    localparam RX_START = 2'd1;
    localparam RX_DATA  = 2'd2;
    localparam RX_STOP  = 2'd3;

    reg [1:0]  rx_state;
    reg [15:0] rx_cnt;
    reg [2:0]  rx_bit;
    reg [7:0]  rx_shift;
    reg [7:0]  rx_data;
    reg        rx_valid;

    always @(posedge clk) begin
        rx_valid <= 1'b0;
        if (rst) begin
            rx_state <= RX_IDLE;
            rx_cnt   <= 0;
            rx_bit   <= 0;
        end else begin
            case (rx_state)
                RX_IDLE: begin
                    if (!rx) begin
                        rx_cnt   <= 0;
                        rx_state <= RX_START;
                    end
                end
                RX_START: begin
                    if (rx_cnt == CLKS_HALF_BIT - 1) begin
                        rx_cnt   <= 0;
                        rx_state <= !rx ? RX_DATA : RX_IDLE;
                        rx_bit   <= 0;
                    end else rx_cnt <= rx_cnt + 1;
                end
                RX_DATA: begin
                    if (rx_cnt == CLKS_PER_BIT - 1) begin
                        rx_cnt           <= 0;
                        rx_shift[rx_bit] <= rx;
                        if (rx_bit == 7)
                            rx_state <= RX_STOP;
                        else
                            rx_bit <= rx_bit + 1;
                    end else rx_cnt <= rx_cnt + 1;
                end
                RX_STOP: begin
                    if (rx_cnt == CLKS_PER_BIT - 1) begin
                        rx_cnt   <= 0;
                        rx_state <= RX_IDLE;
                        if (rx) begin
                            rx_data  <= rx_shift;
                            rx_valid <= 1'b1;
                        end
                    end else rx_cnt <= rx_cnt + 1;
                end
            endcase
        end
    end

    // =========================================================================
    // Command decode
    // =========================================================================

    reg       tx_req;
    reg [7:0] tx_byte;

    always @(posedge clk) begin
        tap_ld <= 1'b0;
        tx_req <= 1'b0;
        if (rst) begin
            tap_reg <= DEFAULT_TAPS;
            tap_ld  <= 1'b1;
        end else if (rx_valid) begin
            if (rx_data == 8'hFF) begin
                tx_byte <= {7'b0, phy_link_up};
                tx_req  <= 1'b1;
            end else begin
                tap_reg <= rx_data[4:0];
                tap_ld  <= 1'b1;
            end
        end
    end

    // =========================================================================
    // UART TX
    // =========================================================================

    localparam TX_IDLE  = 2'd0;
    localparam TX_START = 2'd1;
    localparam TX_DATA  = 2'd2;
    localparam TX_STOP  = 2'd3;

    reg [1:0]  tx_state;
    reg [15:0] tx_cnt;
    reg [2:0]  tx_bit;
    reg [7:0]  tx_shift;
    reg        tx_reg;

    assign tx = tx_reg;

    always @(posedge clk) begin
        if (rst) begin
            tx_state <= TX_IDLE;
            tx_cnt   <= 0;
            tx_bit   <= 0;
            tx_reg   <= 1'b1;
        end else begin
            case (tx_state)
                TX_IDLE: begin
                    tx_reg <= 1'b1;
                    if (tx_req) begin
                        tx_shift <= tx_byte;
                        tx_cnt   <= 0;
                        tx_reg   <= 1'b0;   // start bit
                        tx_state <= TX_START;
                    end
                end
                TX_START: begin
                    if (tx_cnt == CLKS_PER_BIT - 1) begin
                        tx_cnt   <= 0;
                        tx_bit   <= 0;
                        tx_reg   <= tx_shift[0];
                        tx_state <= TX_DATA;
                    end else tx_cnt <= tx_cnt + 1;
                end
                TX_DATA: begin
                    if (tx_cnt == CLKS_PER_BIT - 1) begin
                        tx_cnt <= 0;
                        if (tx_bit == 7) begin
                            tx_reg   <= 1'b1;   // stop bit
                            tx_state <= TX_STOP;
                        end else begin
                            tx_bit <= tx_bit + 1;
                            tx_reg <= tx_shift[tx_bit + 1];
                        end
                    end else tx_cnt <= tx_cnt + 1;
                end
                TX_STOP: begin
                    if (tx_cnt == CLKS_PER_BIT - 1) begin
                        tx_cnt   <= 0;
                        tx_state <= TX_IDLE;
                    end else tx_cnt <= tx_cnt + 1;
                end
            endcase
        end
    end

endmodule
