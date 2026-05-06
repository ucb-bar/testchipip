///////////////////////////////////////////////////////////////////////////////
// udp_payload_to_tsi_serial.v
//
// The sole piece of custom logic in the UDP-TSI bridge.
// Everything else (MAC, IP, ARP, UDP) is handled by verilog-ethernet.
//
// RX path: UDP payload bytes -> pack into SERIAL_WIDTH-bit words -> serial_out
//   Bytes are packed LSB-first. Partial words at end-of-packet are
//   zero-padded and forwarded.
//
// TX path: serial_in words from TSI -> unpack into bytes -> UDP TX payload
//   Also sends an ACK packet after each received UDP packet.
//
// ACK protocol:
//   After each received UDP packet (rx tlast), this module sends back
//   a short UDP response containing:
//     [31:0] ACK_PAYLOAD  (placeholder encoding)
//     [31:0] byte count of received payload
//   Total ACK packet = 8 bytes of UDP payload.
//
//   TSI read responses are sent as separate UDP packets.
///////////////////////////////////////////////////////////////////////////////

module udp_payload_to_tsi_serial #(
    parameter SERIAL_WIDTH = 32,
    parameter [31:0] ACK_PAYLOAD = 32'hAC01_0001
)(
    input  wire        clk,
    input  wire        rst,

    // ---- UDP RX payload (from udp_complete) ----
    input  wire [7:0]  rx_payload_tdata,
    input  wire        rx_payload_tvalid,
    input  wire        rx_payload_tlast,
    output wire        rx_payload_tready,

    // ---- TSI serial output (to TSIToTileLink) ----
    output reg  [SERIAL_WIDTH-1:0] serial_out_bits,
    output reg                     serial_out_valid,
    input  wire                    serial_out_ready,

    // ---- TSI serial input (from TSIToTileLink) ----
    input  wire [SERIAL_WIDTH-1:0] serial_in_bits,
    input  wire                    serial_in_valid,
    output reg                     serial_in_ready,

    // ---- UDP TX payload (to udp_complete) ----
    output reg  [7:0]  tx_payload_tdata,
    output reg         tx_payload_tvalid,
    output reg         tx_payload_tlast,
    input  wire        tx_payload_tready,

    // ---- UDP TX header control ----
    output reg         tx_hdr_valid,
    input  wire        tx_hdr_ready,
    output reg  [15:0] tx_length
);

    localparam BYTES_PER_WORD = SERIAL_WIDTH / 8;
    localparam BYTE_CNT_W    = $clog2(BYTES_PER_WORD);

    // =====================================================================
    // RX: UDP payload bytes -> serial words (LSB first)
    // =====================================================================

    reg [SERIAL_WIDTH-1:0] rx_shift;
    reg [BYTE_CNT_W:0]    rx_byte_cnt;  // 0 to BYTES_PER_WORD
    reg                    rx_word_ready; // a complete word is pending
    reg [15:0]             rx_total_bytes; // total bytes in current packet

    // Watchdog: if TSI doesn't consume a word within ~10 ms (1,250,000 cycles
    // at 125 MHz), force-clear rx_word_ready so the RX path unblocks.
    localparam WATCHDOG_CYCLES = 1_250_000;
    reg [20:0] rx_watchdog;

    // Back-pressure: stop accepting payload when a serial word is pending
    assign rx_payload_tready = !rx_word_ready;

    always @(posedge clk) begin
        if (rst) begin
            rx_byte_cnt      <= 0;
            rx_word_ready    <= 1'b0;
            serial_out_valid <= 1'b0;
            rx_total_bytes   <= 0;
            rx_watchdog      <= 0;
        end else begin
            // TSI consumed the word
            if (serial_out_valid && serial_out_ready) begin
                serial_out_valid <= 1'b0;
                rx_word_ready    <= 1'b0;
                rx_watchdog      <= 0;
                rx_shift         <= {SERIAL_WIDTH{1'b0}};
            end

            // Watchdog: unblock RX if TSI stalls
            if (rx_word_ready) begin
                if (rx_watchdog == WATCHDOG_CYCLES - 1) begin
                    rx_word_ready    <= 1'b0;
                    serial_out_valid <= 1'b0;
                    rx_watchdog      <= 0;
                    rx_shift         <= {SERIAL_WIDTH{1'b0}};
                end else begin
                    rx_watchdog <= rx_watchdog + 1;
                end
            end else begin
                rx_watchdog <= 0;
            end

            // Accept payload bytes
            if (rx_payload_tvalid && rx_payload_tready) begin
                // Shift byte in (LSB first)
                rx_shift[rx_byte_cnt*8 +: 8] <= rx_payload_tdata;
                rx_byte_cnt    <= rx_byte_cnt + 1;
                rx_total_bytes <= rx_total_bytes + 1;

                // Word complete or end of packet
                if (rx_byte_cnt == BYTES_PER_WORD - 1 || rx_payload_tlast) begin
                    // Zero-pad if partial
                    if (rx_payload_tlast && rx_byte_cnt < BYTES_PER_WORD - 1) begin
                        // Remaining bytes already zero from shift reg init
                        // (handled by writing only specific byte lanes above)
                    end
                    serial_out_bits  <= rx_shift | ({SERIAL_WIDTH{1'b0}} | rx_payload_tdata) << (rx_byte_cnt * 8);
                    serial_out_valid <= 1'b1;
                    rx_word_ready    <= 1'b1;
                    rx_byte_cnt      <= 0;

                    // On last byte, clear shift register and byte counter
                    if (rx_payload_tlast) begin
                        rx_shift       <= {SERIAL_WIDTH{1'b0}};
                        rx_total_bytes <= 0;
                    end
                end
            end
        end
    end

    // =====================================================================
    // ACK + TX response state machine
    //
    // States:
    //   IDLE     : waiting for something to send
    //   ACK_HDR  : assert UDP TX header for ACK packet
    //   ACK_DATA : send 8 bytes (ACK_PAYLOAD + byte count)
    //   RESP_HDR : assert UDP TX header for TSI response
    //   RESP_DATA: stream serial_in words as bytes
    // =====================================================================

    localparam TX_IDLE      = 3'd0;
    localparam TX_ACK_HDR   = 3'd1;
    localparam TX_ACK_DATA  = 3'd2;
    localparam TX_RESP_HDR  = 3'd3;
    localparam TX_RESP_DATA = 3'd4;

    reg [2:0] tx_state;
    reg [3:0] tx_byte_cnt;  // byte counter within current TX payload
    reg       ack_pending;  // set when rx_payload_tlast seen

    // Latch ACK pending on end of received packet
    always @(posedge clk) begin
        if (rst)
            ack_pending <= 1'b0;
        else if (rx_payload_tvalid && rx_payload_tready && rx_payload_tlast)
            ack_pending <= 1'b1;
        else if (tx_state == TX_ACK_HDR && tx_hdr_ready)
            ack_pending <= 1'b0;
    end

    // Latched byte count for ACK
    reg [15:0] ack_byte_count;

    always @(posedge clk) begin
        if (rst)
            ack_byte_count <= 0;
        else if (rx_payload_tvalid && rx_payload_tready && rx_payload_tlast)
            ack_byte_count <= rx_total_bytes + 1;
    end

    // ACK payload: 8 bytes = ACK_PAYLOAD[31:0] + byte_count[15:0] zero-padded to 32
    wire [7:0] ack_bytes [0:7];
    assign ack_bytes[0] = ACK_PAYLOAD[31:24];
    assign ack_bytes[1] = ACK_PAYLOAD[23:16];
    assign ack_bytes[2] = ACK_PAYLOAD[15:8];
    assign ack_bytes[3] = ACK_PAYLOAD[7:0];
    assign ack_bytes[4] = ack_byte_count[15:8];
    assign ack_bytes[5] = ack_byte_count[7:0];
    assign ack_bytes[6] = 8'h00;
    assign ack_bytes[7] = 8'h00;

    // TSI response serialization
    reg [SERIAL_WIDTH-1:0] tx_resp_shift;
    reg [BYTE_CNT_W:0]    tx_resp_cnt;
    reg                    tx_resp_active;

    always @(posedge clk) begin
        if (rst) begin
            tx_state        <= TX_IDLE;
            tx_hdr_valid    <= 1'b0;
            tx_payload_tvalid <= 1'b0;
            tx_payload_tlast  <= 1'b0;
            serial_in_ready <= 1'b0;
            tx_byte_cnt     <= 0;
            tx_resp_active  <= 1'b0;
            tx_length       <= 0;
        end else begin
            tx_hdr_valid      <= 1'b0;
            tx_payload_tvalid <= 1'b0;
            tx_payload_tlast  <= 1'b0;
            serial_in_ready   <= 1'b0;

            case (tx_state)
                TX_IDLE: begin
                    tx_byte_cnt <= 0;
                    // Priority: ACK first, then TSI responses.
                    // Do NOT assert tx_hdr_valid here — only assert it once
                    // inside TX_ACK_HDR/TX_RESP_HDR where tx_hdr_ready is
                    // actually checked. Asserting it here races with
                    // tx_hdr_ready already being high: udp_ip_tx would latch
                    // the header this cycle while the state machine is still
                    // in IDLE, causing it to stall in TX_ACK_HDR waiting for
                    // a tx_hdr_ready that already fired.
                    if (ack_pending) begin
                        tx_length    <= 16'd16;   // 8-byte UDP header + 8-byte ACK payload
                        tx_state     <= TX_ACK_HDR;
                        tx_hdr_valid <= 1'b1;
                    end
                    else if (serial_in_valid) begin
                        // TSI has a response word to send
                        tx_resp_shift  <= serial_in_bits;
                        serial_in_ready <= 1'b1;
                        tx_resp_cnt    <= 0;
                        tx_length      <= BYTES_PER_WORD + 16'd8; // UDP header + one-word payload
                        tx_state       <= TX_RESP_HDR;
                        tx_hdr_valid <= 1'b1;

                    end
                end

                // ---- ACK ----
                TX_ACK_HDR: begin
                    if (tx_hdr_ready) begin
                        tx_hdr_valid      <= 1'b0;
                        tx_state          <= TX_ACK_DATA;
                        tx_byte_cnt       <= 0;
                        tx_payload_tdata  <= ack_bytes[0];
                        tx_payload_tvalid <= 1'b1;
                    end
                end

                TX_ACK_DATA: begin
                    tx_payload_tvalid <= 1'b1;
                    if (tx_byte_cnt == 7)
                        tx_payload_tlast <= 1'b1;

                    if (tx_payload_tready) begin
                        if (tx_byte_cnt == 7) begin
                            tx_state          <= TX_IDLE;
                            tx_payload_tvalid <= 1'b0;
                        end else begin
                            tx_byte_cnt      <= tx_byte_cnt + 1;
                            tx_payload_tdata <= ack_bytes[tx_byte_cnt + 1];
                            if (tx_byte_cnt == 6)
                                tx_payload_tlast <= 1'b1;
                        end
                    end
                end

                // ---- TSI Response ----
                TX_RESP_HDR: begin
                    if (tx_hdr_ready) begin
                        tx_hdr_valid      <= 1'b0;
                        tx_state          <= TX_RESP_DATA;
                        tx_resp_cnt       <= 0;
                        tx_payload_tdata  <= tx_resp_shift[7:0];
                        tx_payload_tvalid <= 1'b1;
                    end
                end

                TX_RESP_DATA: begin
                    tx_payload_tvalid <= 1'b1;
                    if (tx_resp_cnt == BYTES_PER_WORD - 1)
                        tx_payload_tlast <= 1'b1;

                    if (tx_payload_tready) begin
                        if (tx_resp_cnt == BYTES_PER_WORD - 1) begin
                            tx_state          <= TX_IDLE;
                            tx_payload_tvalid <= 1'b0;
                        end else begin
                            tx_resp_shift    <= tx_resp_shift >> 8;
                            tx_resp_cnt      <= tx_resp_cnt + 1;
                            tx_payload_tdata <= tx_resp_shift[15:8];
                            if (tx_resp_cnt == BYTES_PER_WORD - 2)
                                tx_payload_tlast <= 1'b1;
                        end
                    end
                end

                default: tx_state <= TX_IDLE;
            endcase
        end
    end

    // rx_total_bytes reset is handled inside the main RX always block above.

`ifdef ENABLE_DEBUG_ILA
    // RX path is counter/flag-driven (no explicit FSM). Expose a derived state:
    //   0: idle/no partial word buffered
    //   1: collecting payload bytes into current serial word
    //   2: word pending on serial_out (back-pressuring RX)
    wire [1:0] rx_dbg_state =
        rx_word_ready ? 2'd2 :
        (rx_byte_cnt != 0) ? 2'd1 :
        2'd0;

    // Single ILA: all RX, TX, and serial TL signals
    ila_6 udp_payload_tsi_ila (
        .clk    (clk),
        .probe0 (rst),
        .probe1 (rx_dbg_state),
        .probe2 (rx_payload_tvalid),
        .probe3 (rx_payload_tready),
        .probe4 (rx_payload_tlast),
        .probe5 (rx_payload_tdata),
        .probe6 (rx_word_ready),
        .probe7 (rx_byte_cnt),
        .probe8 (rx_watchdog),
        .probe9 (ack_pending),
        .probe10(ack_byte_count),
        .probe11(rx_total_bytes),
        .probe12(tx_state),
        .probe13(tx_hdr_valid),
        .probe14(tx_hdr_ready),
        .probe15(tx_length),
        .probe16(tx_payload_tvalid),
        .probe17(tx_payload_tready),
        .probe18(tx_payload_tlast),
        .probe19(tx_payload_tdata),
        .probe20(tx_byte_cnt),
        .probe21(tx_resp_cnt),
        .probe22(serial_out_valid),
        .probe23(serial_out_ready),
        .probe24(serial_out_bits),
        .probe25(serial_in_valid),
        .probe26(serial_in_ready),
        .probe27(serial_in_bits)
    );
`endif

endmodule
