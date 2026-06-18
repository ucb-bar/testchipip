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

    // ---- UDP RX payload (from udp_complete, port-filtered) ----
    // rx_port_is_tsi: held high for the duration of a TSI-port packet,
    // low for control-port packets. Gates serial_out so ctrl packets
    // trigger ACK but are not forwarded to TileLink.
    input  wire        rx_port_is_tsi,
    input  wire [7:0]  rx_payload_tdata,
    input  wire        rx_payload_tvalid,
    input  wire        rx_payload_tlast,
    output wire        rx_payload_tready,

    // ---- TSI serial output (to TSIToTileLink) ----
    output reg  [SERIAL_WIDTH-1:0] serial_out_bits,
    output reg                     serial_out_valid,
    input  wire                    serial_out_ready,

    // ---- Ctrl serial output (to udp_ctrl_placeholder, non-TSI port) ----
    output reg  [SERIAL_WIDTH-1:0] ctrl_out_bits,
    output reg                     ctrl_out_valid,
    input  wire                    ctrl_out_ready,

    // ---- TSI serial input (from TSIToTileLink) ----
    input  wire [SERIAL_WIDTH-1:0] serial_in_bits,
    input  wire                    serial_in_valid,
    output wire                    serial_in_ready,

    // ---- UDP TX payload (to udp_complete) ----
    output reg  [7:0]  tx_payload_tdata,
    output reg         tx_payload_tvalid,
    output reg         tx_payload_tlast,
    input  wire        tx_payload_tready,

    // ---- UDP TX header control ----
    output reg         tx_hdr_valid,
    input  wire        tx_hdr_ready,
    output reg  [15:0] tx_length,

    // ---- Chip-select invert (latched via CTRL_CMD_SET_SELECT_INVERT) ----
    output wire        select_invert,

    // ---- Chip reset (latched via CTRL_CMD_SET_CHIP_RESET) ----
    // chip_reset[0] = chip 0, chip_reset[1] = chip 1.  1 = held in reset, 0 = running.
    output wire  [1:0] chip_reset,

    // ---- Fast-path window configuration ----
    output wire [63:0] fastpath_base,
    output wire [63:0] fastpath_size
);

    localparam BYTES_PER_WORD = SERIAL_WIDTH / 8;
    localparam BYTE_CNT_W    = $clog2(BYTES_PER_WORD);

    // Ctrl-port command word: read back the watchdog sticky bit + fire count.
    // Send this exact 32-bit word as the payload of a UDP packet to
    // UDP_PORT+1; the response ACK's bytes[6:7] will carry
    // {watchdog_fired, watchdog_fire_cnt[14:0]}.
    localparam [31:0] CTRL_CMD_READ_WATCHDOG = 32'h57444F47; // "WDOG"

    // Ctrl-port command word: set the RX watchdog timeout, in clk cycles.
    // Send a 2-word packet to UDP_PORT+1: [CTRL_CMD_SET_WATCHDOG_TIMEOUT, cycles].
    // `cycles` is treated as an unsigned 32-bit value (truncated to 32 bits).
    localparam [31:0] CTRL_CMD_SET_WATCHDOG_TIMEOUT = 32'h57444F54; // "WDOT"

    // Ctrl-port command word: set the chip-select invert latch.
    // Send a 2-word packet to UDP_PORT+1: [CTRL_CMD_SET_SELECT_INVERT, value].
    // value[0] = 0 -> do not invert io_select, value[0] = 1 -> invert io_select.
    localparam [31:0] CTRL_CMD_SET_SELECT_INVERT  = 32'h53454C49; // "SELI"
    // Ctrl-port command word: read back the chip-select invert latch.
    // Send a 1-word packet to UDP_PORT+1: [CTRL_CMD_READ_SELECT_INVERT].
    // Response ACK byte[6] bit[0] carries the current select_invert_reg value.
    localparam [31:0] CTRL_CMD_READ_SELECT_INVERT = 32'h53454C52; // "SELR"

    // Ctrl-port command word: set the chip reset latch.
    // Send a 2-word packet to UDP_PORT+1: [CTRL_CMD_SET_CHIP_RESET, value].
    // value[0] = chip 0 reset, value[1] = chip 1 reset.  1 = held in reset, 0 = running.
    localparam [31:0] CTRL_CMD_SET_CHIP_RESET = 32'h52535443; // "RSTC"
    localparam [31:0] CTRL_CMD_SET_FASTPATH_BASE_LO  = 32'h4650424C; // "FPBL"
    localparam [31:0] CTRL_CMD_SET_FASTPATH_BASE_HI  = 32'h46504248; // "FPBH"
    localparam [31:0] CTRL_CMD_SET_FASTPATH_SIZE_LO  = 32'h4650534C; // "FPSL"
    localparam [31:0] CTRL_CMD_SET_FASTPATH_SIZE_HI  = 32'h46505348; // "FPSH"

    // =====================================================================
    // RX: UDP payload bytes -> serial words (LSB first)
    // =====================================================================

    reg [SERIAL_WIDTH-1:0] rx_shift;
    reg [BYTE_CNT_W:0]    rx_byte_cnt;  // 0 to BYTES_PER_WORD
    reg                    rx_word_ready; // a complete word is pending
    reg [15:0]             rx_total_bytes; // total bytes in current packet

    // Watchdog: if TSI doesn't consume a word within WATCHDOG_CYCLES cycles
    // (default 12,500,000 ~= 100ms at 125 MHz), force-clear rx_word_ready so
    // the RX path unblocks. The timeout is runtime-configurable via the
    // CTRL_CMD_SET_WATCHDOG_TIMEOUT ctrl-port command (see watchdog_cycles_reg).
    localparam [31:0] WATCHDOG_CYCLES = 32'd12_500_000;
    reg [31:0] rx_watchdog;

    // Sticky "watchdog ever fired" flag + saturating fire counter, readable
    // via the ctrl-port CTRL_CMD_READ_WATCHDOG command (see ack_bytes below).
    reg        watchdog_fired;
    reg [14:0] watchdog_fire_cnt;

    // Back-pressure: stop accepting payload when a serial word is pending
    assign rx_payload_tready = !rx_word_ready;

    // Word formed by appending the current incoming byte to the shift register
    wire [SERIAL_WIDTH-1:0] rx_word_in =
        rx_shift | (({SERIAL_WIDTH{1'b0}} | rx_payload_tdata) << (rx_byte_cnt * 8));

    always @(posedge clk) begin
        if (rst) begin
            rx_byte_cnt      <= 0;
            rx_word_ready    <= 1'b0;
            serial_out_valid <= 1'b0;
            ctrl_out_valid   <= 1'b0;
            rx_total_bytes   <= 0;
            rx_watchdog      <= 0;
            watchdog_fired   <= 1'b0;
            watchdog_fire_cnt <= 15'd0;
        end else begin
            // Word consumed by TSI
            if (serial_out_valid && serial_out_ready) begin
                serial_out_valid <= 1'b0;
                rx_word_ready    <= 1'b0;
                rx_watchdog      <= 0;
                rx_shift         <= {SERIAL_WIDTH{1'b0}};
            end

            // Word consumed by ctrl placeholder
            if (ctrl_out_valid && ctrl_out_ready) begin
                ctrl_out_valid <= 1'b0;
                rx_word_ready  <= 1'b0;
                rx_watchdog    <= 0;
                rx_shift       <= {SERIAL_WIDTH{1'b0}};
            end

            // Watchdog: unblock RX if downstream stalls
            if (rx_word_ready) begin
                if (rx_watchdog == watchdog_cycles_reg - 1) begin
                    rx_word_ready    <= 1'b0;
                    serial_out_valid <= 1'b0;
                    ctrl_out_valid   <= 1'b0;
                    rx_watchdog      <= 0;
                    rx_shift         <= {SERIAL_WIDTH{1'b0}};
                    watchdog_fired   <= 1'b1;
                    if (watchdog_fire_cnt != {15{1'b1}})
                        watchdog_fire_cnt <= watchdog_fire_cnt + 15'd1;
                end else begin
                    rx_watchdog <= rx_watchdog + 1;
                end
            end else begin
                rx_watchdog <= 0;
            end

            // Accept payload bytes — shared shift register for both ports
            if (rx_payload_tvalid && rx_payload_tready) begin
                rx_shift[rx_byte_cnt*8 +: 8] <= rx_payload_tdata;
                rx_byte_cnt    <= rx_byte_cnt + 1;
                rx_total_bytes <= rx_total_bytes + 1;

                // Word complete or end of packet
                if (rx_byte_cnt == BYTES_PER_WORD - 1 || rx_payload_tlast) begin
                    ctrl_out_bits    <= rx_word_in;
                    serial_out_bits  <= rx_word_in;
                    serial_out_valid <= rx_port_is_tsi;
                    ctrl_out_valid   <= !rx_port_is_tsi;
                    rx_word_ready    <= 1'b1;
                    rx_byte_cnt      <= 0;

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

    // Latch whether this ACK is a response to a ctrl read command, so the
    // ACK payload can carry the queried value instead of zero padding.
    reg ack_watchdog_query;
    reg ack_select_query;
    always @(posedge clk) begin
        if (rst) begin
            ack_watchdog_query <= 1'b0;
            ack_select_query   <= 1'b0;
        end else if (rx_payload_tvalid && rx_payload_tready && rx_payload_tlast) begin
            ack_watchdog_query <= !rx_port_is_tsi && (rx_word_in == CTRL_CMD_READ_WATCHDOG);
            ack_select_query   <= !rx_port_is_tsi && (rx_word_in == CTRL_CMD_READ_SELECT_INVERT);
        end
    end

    // Capture a runtime-configurable RX watchdog timeout via the ctrl port.
    // Two-word command: [CTRL_CMD_SET_WATCHDOG_TIMEOUT, cycles]. `cycles` is
    // an unsigned value, truncated to 32 bits.
    reg        ctrl_expect_watchdog_timeout_value;
    reg [31:0] watchdog_cycles_reg;
    always @(posedge clk) begin
        if (rst) begin
            ctrl_expect_watchdog_timeout_value <= 1'b0;
            watchdog_cycles_reg <= WATCHDOG_CYCLES;
        end else if (rx_payload_tvalid && rx_payload_tready &&
                      (rx_byte_cnt == BYTES_PER_WORD - 1 || rx_payload_tlast) &&
                      !rx_port_is_tsi) begin
            if (ctrl_expect_watchdog_timeout_value) begin
                watchdog_cycles_reg <= rx_word_in[31:0];
                ctrl_expect_watchdog_timeout_value <= 1'b0;
            end else if (rx_word_in == CTRL_CMD_SET_WATCHDOG_TIMEOUT) begin
                ctrl_expect_watchdog_timeout_value <= 1'b1;
            end else begin
                ctrl_expect_watchdog_timeout_value <= 1'b0;
            end
        end
    end

    // Capture a runtime-configurable chip-select invert via the ctrl port.
    // Two-word command: [CTRL_CMD_SET_SELECT_INVERT, value]. value[0] is
    // latched and XOR'd with io_select in UDPTSIStreamMuxShim.
    reg        ctrl_expect_select_invert_value;
    reg        select_invert_reg;
    assign select_invert = select_invert_reg;
    always @(posedge clk) begin
        if (rst) begin
            ctrl_expect_select_invert_value <= 1'b0;
            select_invert_reg <= 1'b0;
        end else if (rx_payload_tvalid && rx_payload_tready &&
                      (rx_byte_cnt == BYTES_PER_WORD - 1 || rx_payload_tlast) &&
                      !rx_port_is_tsi) begin
            if (ctrl_expect_select_invert_value) begin
                select_invert_reg <= rx_word_in[0];
                ctrl_expect_select_invert_value <= 1'b0;
            end else if (rx_word_in == CTRL_CMD_SET_SELECT_INVERT) begin
                ctrl_expect_select_invert_value <= 1'b1;
            end else begin
                ctrl_expect_select_invert_value <= 1'b0;
            end
        end
    end

    // Chip reset latch — set via CTRL_CMD_SET_CHIP_RESET.
    reg        ctrl_expect_chip_reset_value;
    reg  [1:0] chip_reset_reg;
    assign chip_reset = chip_reset_reg;
    always @(posedge clk) begin
        if (rst) begin
            ctrl_expect_chip_reset_value <= 1'b0;
            chip_reset_reg <= 2'b00;
        end else if (rx_payload_tvalid && rx_payload_tready &&
                      (rx_byte_cnt == BYTES_PER_WORD - 1 || rx_payload_tlast) &&
                      !rx_port_is_tsi) begin
            if (ctrl_expect_chip_reset_value) begin
                chip_reset_reg <= rx_word_in[1:0];
                ctrl_expect_chip_reset_value <= 1'b0;
            end else if (rx_word_in == CTRL_CMD_SET_CHIP_RESET) begin
                ctrl_expect_chip_reset_value <= 1'b1;
            end else begin
                ctrl_expect_chip_reset_value <= 1'b0;
            end
        end
    end

    reg        ctrl_expect_fastpath_base_lo_value;
    reg        ctrl_expect_fastpath_base_hi_value;
    reg        ctrl_expect_fastpath_size_lo_value;
    reg        ctrl_expect_fastpath_size_hi_value;
    reg [63:0] fastpath_base_reg;
    reg [63:0] fastpath_size_reg;
    assign fastpath_base = fastpath_base_reg;
    assign fastpath_size = fastpath_size_reg;

    always @(posedge clk) begin
        if (rst) begin
            ctrl_expect_fastpath_base_lo_value <= 1'b0;
            ctrl_expect_fastpath_base_hi_value <= 1'b0;
            ctrl_expect_fastpath_size_lo_value <= 1'b0;
            ctrl_expect_fastpath_size_hi_value <= 1'b0;
            fastpath_base_reg <= 64'd0;
            fastpath_size_reg <= 64'd0;
        end else if (rx_payload_tvalid && rx_payload_tready &&
                      (rx_byte_cnt == BYTES_PER_WORD - 1 || rx_payload_tlast) &&
                      !rx_port_is_tsi) begin
            if (ctrl_expect_fastpath_base_lo_value) begin
                fastpath_base_reg[31:0] <= rx_word_in[31:0];
                ctrl_expect_fastpath_base_lo_value <= 1'b0;
            end else if (ctrl_expect_fastpath_base_hi_value) begin
                fastpath_base_reg[63:32] <= rx_word_in[31:0];
                ctrl_expect_fastpath_base_hi_value <= 1'b0;
            end else if (ctrl_expect_fastpath_size_lo_value) begin
                fastpath_size_reg[31:0] <= rx_word_in[31:0];
                ctrl_expect_fastpath_size_lo_value <= 1'b0;
            end else if (ctrl_expect_fastpath_size_hi_value) begin
                fastpath_size_reg[63:32] <= rx_word_in[31:0];
                ctrl_expect_fastpath_size_hi_value <= 1'b0;
            end else if (rx_word_in == CTRL_CMD_SET_FASTPATH_BASE_LO) begin
                ctrl_expect_fastpath_base_lo_value <= 1'b1;
            end else if (rx_word_in == CTRL_CMD_SET_FASTPATH_BASE_HI) begin
                ctrl_expect_fastpath_base_hi_value <= 1'b1;
            end else if (rx_word_in == CTRL_CMD_SET_FASTPATH_SIZE_LO) begin
                ctrl_expect_fastpath_size_lo_value <= 1'b1;
            end else if (rx_word_in == CTRL_CMD_SET_FASTPATH_SIZE_HI) begin
                ctrl_expect_fastpath_size_hi_value <= 1'b1;
            end
        end
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
    // bytes[6:7] are zero-padding, except in response to
    // CTRL_CMD_READ_WATCHDOG where they carry the watchdog status:
    //   bit15      = watchdog_fired (sticky)
    //   bits[14:0] = watchdog_fire_cnt (saturating)
    assign ack_bytes[6] = ack_watchdog_query ? {watchdog_fired, watchdog_fire_cnt[14:8]} :
                          ack_select_query   ? {7'h00, select_invert_reg}                :
                                               8'h00;
    assign ack_bytes[7] = ack_watchdog_query ? watchdog_fire_cnt[7:0] : 8'h00;

    // TSI response serialization
    reg [SERIAL_WIDTH-1:0] tx_resp_shift;
    reg [BYTE_CNT_W:0]    tx_resp_cnt;
    reg                    tx_resp_active;

    // serial_in_ready: combinational, asserted whenever we're idle and not
    // about to send an ACK. Per ready/valid semantics, ready must not wait
    // on valid — otherwise a single-cycle valid pulse from TSIToTileLink
    // (not held until acknowledged) would be missed entirely, since a
    // registered ready (asserted only after observing valid) would always
    // be one cycle late.
    assign serial_in_ready = (tx_state == TX_IDLE) && !ack_pending;

    always @(posedge clk) begin
        if (rst) begin
            tx_state        <= TX_IDLE;
            tx_hdr_valid    <= 1'b0;
            tx_payload_tvalid <= 1'b0;
            tx_payload_tlast  <= 1'b0;
            tx_byte_cnt     <= 0;
            tx_resp_active  <= 1'b0;
            tx_length       <= 0;
        end else begin
            tx_hdr_valid      <= 1'b0;
            tx_payload_tvalid <= 1'b0;
            tx_payload_tlast  <= 1'b0;

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
                        // TSI has a response word to send. serial_in_ready
                        // is already asserted combinationally (see assign
                        // above), so the handshake completes this cycle.
                        tx_resp_shift  <= serial_in_bits;
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

`ifdef ENABLE_MAC_DEBUG_ILA
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
        .probe27(serial_in_bits),
        .probe28(watchdog_fired),
        .probe29(watchdog_fire_cnt),
        .probe30(watchdog_cycles_reg),
        .probe31(rx_shift)
    );
`endif

endmodule
