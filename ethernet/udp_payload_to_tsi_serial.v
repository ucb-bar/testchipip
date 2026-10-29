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
    parameter [31:0] ACK_PAYLOAD = 32'hAC01_0001,
    parameter integer MAX_OUTSTANDING = 8
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
    output wire [SERIAL_WIDTH-1:0] serial_out_bits,
    output wire                    serial_out_valid,
    input  wire                    serial_out_ready,

    // ---- Ctrl serial output (to udp_ctrl_placeholder, non-TSI port) ----
    output wire [SERIAL_WIDTH-1:0] ctrl_out_bits,
    output wire                    ctrl_out_valid,
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

    // ---- Absolute chip-select value written by host (CTRL_CMD_SET_SELECT_VALUE) ----
    // select_value    : last absolute select value latched from the host (data word bit 0)
    // select_value_wr : 1-cycle strobe, high the cycle a new select_value is latched
    output wire        select_value,
    output wire        select_value_wr,

    // ---- Chip reset (latched via CTRL_CMD_SET_CHIP_RESET) ----
    // chip_reset[0] = chip 0, chip_reset[1] = chip 1.  1 = held in reset, 0 = running.
    output wire  [1:0] chip_reset,

    // ---- Fast-path window configuration ----
    output wire [63:0] fastpath_base,
    output wire [63:0] fastpath_size,

    // ---- FPGA SW reset pulse (CTRL_CMD_FPGA_RESET) ----
    // Auto-release: goes high for FPGA_RESET_CYCLES cycles after the host sends
    // a 1-word CTRL_CMD_FPGA_RESET ctrl packet, then self-clears. Generated in
    // this always-up MAC domain so it is NOT cleared by the reset it drives; the
    // harness fans it into the router/chip/fabric resets (NOT the MAC/PHY, NOT
    // the MIG).
    output wire        fpga_sw_reset
);

    localparam BYTES_PER_WORD = SERIAL_WIDTH / 8;
    localparam BYTE_CNT_W    = $clog2(BYTES_PER_WORD);
    localparam RX_WORD_FIFO_DEPTH = 256;
    localparam RX_FIFO_PTR_W = (RX_WORD_FIFO_DEPTH <= 1) ? 1 : $clog2(RX_WORD_FIFO_DEPTH);

    // Ctrl-port command word: read back the watchdog sticky bit + fire count.
    // Send this exact 32-bit word as the payload of a UDP packet to
    // UDP_PORT+1; the response ACK's bytes[6:7] will carry
    // {watchdog_fired, watchdog_fire_cnt[14:0]}.
    localparam [31:0] CTRL_CMD_READ_WATCHDOG = 32'h57444F47; // "WDOG"
    localparam [31:0] CTRL_CMD_READ_MAX_OUTSTANDING = 32'h4D584F54; // "MXOT"
    localparam [31:0] CTRL_CMD_READ_ACK_COUNT = 32'h41434B43; // "ACKC"

    // Ctrl-port command word: set the RX watchdog timeout, in clk cycles.
    // Send a 2-word packet to UDP_PORT+1: [CTRL_CMD_SET_WATCHDOG_TIMEOUT, cycles].
    // `cycles` is treated as an unsigned 32-bit value (truncated to 32 bits).
    localparam [31:0] CTRL_CMD_SET_WATCHDOG_TIMEOUT = 32'h57444F54; // "WDOT"

    // Ctrl-port command word: set the absolute chip-select value.
    // Send a 2-word packet to UDP_PORT+1: [CTRL_CMD_SET_SELECT_VALUE, value].
    // value[0] is latched into select_value and a 1-cycle select_value_wr
    // strobe is asserted; the recency mux in udp_tsi_top then holds the
    // register value until the board switch changes.
    localparam [31:0] CTRL_CMD_SET_SELECT_VALUE   = 32'h53454C56; // "SELV"

    // Ctrl-port command word: set the chip reset latch.
    // Send a 2-word packet to UDP_PORT+1: [CTRL_CMD_SET_CHIP_RESET, value].
    // value[0] = chip 0 reset, value[1] = chip 1 reset.  1 = held in reset, 0 = running.
    localparam [31:0] CTRL_CMD_SET_CHIP_RESET = 32'h52535443; // "RSTC"
    // 1-word command: [CTRL_CMD_FPGA_RESET]. Pulses fpga_sw_reset for
    // FPGA_RESET_CYCLES cycles (auto-release) to reboot router/chip/fabric.
    localparam [31:0] CTRL_CMD_FPGA_RESET     = 32'h46525354; // "FRST"
    localparam [31:0] FPGA_RESET_CYCLES       = 32'd4096;     // pulse length (cycles)
    localparam [31:0] CTRL_CMD_SET_FASTPATH_BASE_LO  = 32'h4650424C; // "FPBL"
    localparam [31:0] CTRL_CMD_SET_FASTPATH_BASE_HI  = 32'h46504248; // "FPBH"
    localparam [31:0] CTRL_CMD_SET_FASTPATH_SIZE_LO  = 32'h4650534C; // "FPSL"
    localparam [31:0] CTRL_CMD_SET_FASTPATH_SIZE_HI  = 32'h46505348; // "FPSH"

    // Ctrl-port command word: set the TX UDP payload batch size, in bytes.
    // Send a 2-word packet to UDP_PORT+1: [CTRL_CMD_SET_TX_BATCH, bytes].
    // Read responses are then split into ceil(total_bytes / bytes) UDP packets.
    localparam [31:0] CTRL_CMD_SET_TX_BATCH          = 32'h54584253; // "TXBS"

    // =====================================================================
    // RX: UDP payload bytes -> serial words (LSB first)
    // =====================================================================

    reg [SERIAL_WIDTH-1:0] tsi_rx_shift;
    reg [SERIAL_WIDTH-1:0] ctrl_rx_shift;
    reg [BYTE_CNT_W:0]     tsi_rx_byte_cnt;
    reg [BYTE_CNT_W:0]     ctrl_rx_byte_cnt;
    reg [15:0]             rx_total_bytes; // total bytes in current packet
    reg [31:0]             rx_packet_word0;
    reg                    rx_capture_word0;

    reg [SERIAL_WIDTH-1:0] tsi_fifo_mem [0:RX_WORD_FIFO_DEPTH-1];
    reg [SERIAL_WIDTH-1:0] ctrl_fifo_mem [0:RX_WORD_FIFO_DEPTH-1];
    reg [RX_FIFO_PTR_W-1:0] tsi_fifo_wr_ptr;
    reg [RX_FIFO_PTR_W-1:0] tsi_fifo_rd_ptr;
    reg [RX_FIFO_PTR_W-1:0] ctrl_fifo_wr_ptr;
    reg [RX_FIFO_PTR_W-1:0] ctrl_fifo_rd_ptr;
    reg [RX_FIFO_PTR_W:0] tsi_fifo_count;
    reg [RX_FIFO_PTR_W:0] ctrl_fifo_count;

    // Watchdog: if TSI doesn't consume a word within WATCHDOG_CYCLES cycles
    // (default 12,500,000 ~= 100ms at 125 MHz), force-clear rx_word_ready so
    // the RX path unblocks. The timeout is runtime-configurable via the
    // CTRL_CMD_SET_WATCHDOG_TIMEOUT ctrl-port command (see watchdog_cycles_reg).
    localparam [31:0] WATCHDOG_CYCLES = 32'd12_500_000;
    reg [31:0] rx_watchdog;
    reg        ctrl_expect_watchdog_timeout_value;
    reg [31:0] watchdog_cycles_reg;

    // Sticky "watchdog ever fired" flag + saturating fire counter, readable
    // via the ctrl-port CTRL_CMD_READ_WATCHDOG command (see ack_bytes below).
    reg        watchdog_fired;
    reg [14:0] watchdog_fire_cnt;
    reg [31:0] ack_sent_count;
    // ack byte_count / aux / word0 are now carried per-packet in the ACK FIFO
    // (below), not single shared registers.

    wire tsi_fifo_full = (tsi_fifo_count == RX_WORD_FIFO_DEPTH);
    wire tsi_fifo_empty = (tsi_fifo_count == 0);
    wire ctrl_fifo_full = (ctrl_fifo_count == RX_WORD_FIFO_DEPTH);
    wire ctrl_fifo_empty = (ctrl_fifo_count == 0);

    assign serial_out_valid = !tsi_fifo_empty;
    assign serial_out_bits = tsi_fifo_mem[tsi_fifo_rd_ptr];
    assign ctrl_out_valid = !ctrl_fifo_empty;
    assign ctrl_out_bits = ctrl_fifo_mem[ctrl_fifo_rd_ptr];

    assign rx_payload_tready = rx_port_is_tsi ? !tsi_fifo_full : !ctrl_fifo_full;

    wire rx_accept = rx_payload_tvalid && rx_payload_tready;
    wire [SERIAL_WIDTH-1:0] tsi_word_in =
        tsi_rx_shift | (({SERIAL_WIDTH{1'b0}} | rx_payload_tdata) << (tsi_rx_byte_cnt * 8));
    wire [SERIAL_WIDTH-1:0] ctrl_word_in =
        ctrl_rx_shift | (({SERIAL_WIDTH{1'b0}} | rx_payload_tdata) << (ctrl_rx_byte_cnt * 8));
    wire tsi_word_done = rx_accept && rx_port_is_tsi &&
                         ((tsi_rx_byte_cnt == BYTES_PER_WORD - 1) || rx_payload_tlast);
    wire ctrl_word_done = rx_accept && !rx_port_is_tsi &&
                          ((ctrl_rx_byte_cnt == BYTES_PER_WORD - 1) || rx_payload_tlast);
    wire tsi_fifo_pop = serial_out_valid && serial_out_ready;
    wire ctrl_fifo_pop = ctrl_out_valid && ctrl_out_ready;

    always @(posedge clk) begin
        if (rst) begin
            tsi_rx_shift       <= {SERIAL_WIDTH{1'b0}};
            ctrl_rx_shift      <= {SERIAL_WIDTH{1'b0}};
            tsi_rx_byte_cnt    <= 0;
            ctrl_rx_byte_cnt   <= 0;
            tsi_fifo_wr_ptr    <= {RX_FIFO_PTR_W{1'b0}};
            tsi_fifo_rd_ptr    <= {RX_FIFO_PTR_W{1'b0}};
            ctrl_fifo_wr_ptr   <= {RX_FIFO_PTR_W{1'b0}};
            ctrl_fifo_rd_ptr   <= {RX_FIFO_PTR_W{1'b0}};
            tsi_fifo_count     <= {(RX_FIFO_PTR_W+1){1'b0}};
            ctrl_fifo_count    <= {(RX_FIFO_PTR_W+1){1'b0}};
            rx_total_bytes     <= 0;
            rx_packet_word0    <= 32'd0;
            rx_capture_word0   <= 1'b1;
            rx_watchdog        <= 0;
            watchdog_fired     <= 1'b0;
            watchdog_fire_cnt <= 15'd0;
        end else begin
            if (tsi_word_done) begin
                tsi_fifo_mem[tsi_fifo_wr_ptr] <= tsi_word_in;
                if (rx_capture_word0)
                    rx_packet_word0 <= tsi_word_in[31:0];
            end

            if (ctrl_word_done) begin
                ctrl_fifo_mem[ctrl_fifo_wr_ptr] <= ctrl_word_in;
                if (rx_capture_word0)
                    rx_packet_word0 <= ctrl_word_in[31:0];
            end

            if ((tsi_word_done || ctrl_word_done) && rx_capture_word0)
                rx_capture_word0 <= 1'b0;

            case ({tsi_word_done, tsi_fifo_pop})
                2'b10: begin
                    tsi_fifo_wr_ptr <= tsi_fifo_wr_ptr + 1'b1;
                    tsi_fifo_count <= tsi_fifo_count + 1'b1;
                end
                2'b01: begin
                    tsi_fifo_rd_ptr <= tsi_fifo_rd_ptr + 1'b1;
                    tsi_fifo_count <= tsi_fifo_count - 1'b1;
                end
                2'b11: begin
                    tsi_fifo_wr_ptr <= tsi_fifo_wr_ptr + 1'b1;
                    tsi_fifo_rd_ptr <= tsi_fifo_rd_ptr + 1'b1;
                end
                default: begin end
            endcase

            case ({ctrl_word_done, ctrl_fifo_pop})
                2'b10: begin
                    ctrl_fifo_wr_ptr <= ctrl_fifo_wr_ptr + 1'b1;
                    ctrl_fifo_count <= ctrl_fifo_count + 1'b1;
                end
                2'b01: begin
                    ctrl_fifo_rd_ptr <= ctrl_fifo_rd_ptr + 1'b1;
                    ctrl_fifo_count <= ctrl_fifo_count - 1'b1;
                end
                2'b11: begin
                    ctrl_fifo_wr_ptr <= ctrl_fifo_wr_ptr + 1'b1;
                    ctrl_fifo_rd_ptr <= ctrl_fifo_rd_ptr + 1'b1;
                end
                default: begin end
            endcase

            // Watchdog: if the TSI FIFO stays full too long, drop queued TSI
            // words so ingress can recover instead of deadlocking on backpressure.
            if (tsi_fifo_full) begin
                if (rx_watchdog == watchdog_cycles_reg - 1) begin
                    tsi_fifo_wr_ptr  <= {RX_FIFO_PTR_W{1'b0}};
                    tsi_fifo_rd_ptr  <= {RX_FIFO_PTR_W{1'b0}};
                    tsi_fifo_count   <= {(RX_FIFO_PTR_W+1){1'b0}};
                    tsi_rx_shift     <= {SERIAL_WIDTH{1'b0}};
                    tsi_rx_byte_cnt  <= 0;
                    // NOTE: do NOT reset rx_total_bytes here. The RX path
                    // backpressures rather than dropping mid-stream, so a stalled
                    // packet resumes after this FIFO clear and still reaches its
                    // tlast with the full byte count. Zeroing it here would
                    // truncate the count for a watchdog-affected packet.
                    rx_watchdog      <= 0;
                    watchdog_fired   <= 1'b1;
                    if (watchdog_fire_cnt != {15{1'b1}})
                        watchdog_fire_cnt <= watchdog_fire_cnt + 15'd1;
                end else begin
                    rx_watchdog <= rx_watchdog + 1;
                end
            end else begin
                rx_watchdog <= 0;
            end

            if (rx_accept) begin
                rx_total_bytes <= rx_total_bytes + 1;
                if (rx_port_is_tsi) begin
                    if (tsi_word_done) begin
                        tsi_rx_shift <= {SERIAL_WIDTH{1'b0}};
                        tsi_rx_byte_cnt <= 0;
                    end else begin
                        tsi_rx_shift[tsi_rx_byte_cnt*8 +: 8] <= rx_payload_tdata;
                        tsi_rx_byte_cnt <= tsi_rx_byte_cnt + 1'b1;
                    end
                end else begin
                    if (ctrl_word_done) begin
                        ctrl_rx_shift <= {SERIAL_WIDTH{1'b0}};
                        ctrl_rx_byte_cnt <= 0;
                    end else begin
                        ctrl_rx_shift[ctrl_rx_byte_cnt*8 +: 8] <= rx_payload_tdata;
                        ctrl_rx_byte_cnt <= ctrl_rx_byte_cnt + 1'b1;
                    end
                end

                if (rx_payload_tlast) begin
                    rx_total_bytes <= 0;
                    rx_capture_word0 <= 1'b1;
                end
            end
        end
    end

    // =====================================================================
    // Read-command snoop + TX batching
    //
    // To pack a read's multi-word response into larger UDP packets, the MAC
    // snoops each TSI-port read command on the RX path and records the total
    // number of SERIAL_WIDTH-bit response words = (length + 1) (length is the
    // command's 64-bit length field; len_hi assumed 0 for supported sizes).
    // The TX side then emits ceil(total_words / words_per_chunk) packets,
    // where words_per_chunk = batch_bytes_reg / BYTES_PER_WORD.
    // =====================================================================
    localparam RESP_W = 21;  // up to 2M response words (8 MB) per read

    reg [2:0]        rx_word_idx;       // word index within current TSI packet (saturates)
    reg              rd_is_read;        // command word0 indicated a read (bit0 == 0)
    reg [31:0]       rd_len_lo;         // command length field, low 32 bits
    reg [RESP_W-1:0] resp_words_total;  // total response words for the pending read

    always @(posedge clk) begin
        if (rst) begin
            rx_word_idx      <= 3'd0;
            rd_is_read       <= 1'b0;
            rd_len_lo        <= 32'd0;
            resp_words_total <= {RESP_W{1'b0}};
        end else if (tsi_word_done) begin
            // A TSI-port serial word just completed; word layout is
            // [0]=cmd [1]=addr_lo [2]=addr_hi [3]=len_lo [4]=len_hi (then data).
            if (rx_word_idx == 3'd0) rd_is_read <= ~tsi_word_in[0];
            if (rx_word_idx == 3'd3) rd_len_lo  <= tsi_word_in;
            if (rx_word_idx == 3'd4 && rd_is_read)
                resp_words_total <= rd_len_lo[RESP_W-1:0] + 1'b1;
            if (rx_payload_tlast)         rx_word_idx <= 3'd0;
            else if (rx_word_idx != 3'd7) rx_word_idx <= rx_word_idx + 1'b1;
        end
    end

    // Runtime-configurable TX UDP payload batch size, in bytes (default 512).
    // Two-word ctrl command: [CTRL_CMD_SET_TX_BATCH, bytes]. The value is hard-
    // clipped to MAX_TX_BATCH so one response packet stays within an Ethernet
    // frame (no IP fragmentation in the UDP/IP TX path).
    localparam [15:0] MAX_TX_BATCH = 16'd1400;
    reg        ctrl_expect_tx_batch_value;
    reg [15:0] batch_bytes_reg;
    wire [RESP_W-1:0] words_per_chunk_raw = batch_bytes_reg >> BYTE_CNT_W;
    wire [RESP_W-1:0] words_per_chunk =
        (words_per_chunk_raw == 0) ? {{(RESP_W-1){1'b0}}, 1'b1} : words_per_chunk_raw;
    always @(posedge clk) begin
        if (rst) begin
            ctrl_expect_tx_batch_value <= 1'b0;
            batch_bytes_reg <= 16'd512;
        end else if (ctrl_word_done) begin
            if (ctrl_expect_tx_batch_value) begin
                // Hard-clip the batch size to MAX_TX_BATCH bytes so a single
                // response packet always fits in one Ethernet frame (the UDP/IP
                // TX path does not fragment). Larger written values are clamped.
                batch_bytes_reg <= (ctrl_word_in[15:0] > MAX_TX_BATCH) ? MAX_TX_BATCH
                                                                       : ctrl_word_in[15:0];
                ctrl_expect_tx_batch_value <= 1'b0;
            end else if (ctrl_word_in == CTRL_CMD_SET_TX_BATCH) begin
                ctrl_expect_tx_batch_value <= 1'b1;
            end else begin
                ctrl_expect_tx_batch_value <= 1'b0;
            end
        end
    end

    // =====================================================================
    // ACK + TX response state machine
    //
    // States:
    //   IDLE      : waiting for something to send
    //   ACK_HDR   : assert UDP TX header for ACK packet
    //   ACK_DATA  : send 8 bytes (ACK_PAYLOAD + byte count)
    //   RESP_HDR  : assert UDP TX header for a response packet (chunk)
    //   RESP_DATA : stream the current serial_in word as bytes
    //   RESP_NEXT : fetch the next word of the current chunk (same packet)
    //   RESP_CHUNK: start the next packet for a multi-packet response
    // =====================================================================

    localparam TX_IDLE       = 3'd0;
    localparam TX_ACK_HDR    = 3'd1;
    localparam TX_ACK_DATA   = 3'd2;
    localparam TX_RESP_HDR   = 3'd3;
    localparam TX_RESP_DATA  = 3'd4;
    localparam TX_RESP_NEXT  = 3'd5;
    localparam TX_RESP_CHUNK = 3'd6;

    reg [2:0] tx_state;
    reg [3:0] tx_byte_cnt;  // byte counter within current TX payload
    // ack_pending is derived from the ACK FIFO below (= !ack_fifo_empty).

    reg [RESP_W-1:0] tx_resp_left;   // response words remaining to fetch (after current)
    reg [RESP_W-1:0] tx_chunk_left;  // current-chunk words remaining to fetch (after current)

    // Chunk sizing: min(remaining words, words_per_chunk)
    wire [RESP_W-1:0] resp_total_eff =
        (resp_words_total != 0) ? resp_words_total : {{(RESP_W-1){1'b0}}, 1'b1};
    wire [RESP_W-1:0] chunk0 =
        (resp_total_eff < words_per_chunk) ? resp_total_eff : words_per_chunk;
    wire [RESP_W-1:0] chunkN =
        (tx_resp_left  < words_per_chunk) ? tx_resp_left  : words_per_chunk;

    // (ACK-pending and the per-packet aux/word0 fields are now handled by the
    // ACK FIFO defined further below — each completed packet pushes its full ACK
    // payload, so pipelined packets under backpressure can't clobber each other.)

    // Capture a runtime-configurable RX watchdog timeout via the ctrl port.
    // Two-word command: [CTRL_CMD_SET_WATCHDOG_TIMEOUT, cycles]. `cycles` is
    // an unsigned value, truncated to 32 bits.
    always @(posedge clk) begin
        if (rst) begin
            ctrl_expect_watchdog_timeout_value <= 1'b0;
            watchdog_cycles_reg <= WATCHDOG_CYCLES;
        end else if (ctrl_word_done) begin
            if (ctrl_expect_watchdog_timeout_value) begin
                watchdog_cycles_reg <= ctrl_word_in[31:0];
                ctrl_expect_watchdog_timeout_value <= 1'b0;
            end else if (ctrl_word_in == CTRL_CMD_SET_WATCHDOG_TIMEOUT) begin
                ctrl_expect_watchdog_timeout_value <= 1'b1;
            end else begin
                ctrl_expect_watchdog_timeout_value <= 1'b0;
            end
        end
    end

    // Capture an absolute chip-select value via the ctrl port.
    // Two-word command: [CTRL_CMD_SET_SELECT_VALUE, value]. value[0] is latched
    // into select_value_reg and select_value_wr_reg pulses for one cycle so the
    // recency mux in udp_tsi_top can switch to (and hold) the register value.
    reg        ctrl_expect_select_value;
    reg        select_value_reg;
    reg        select_value_wr_reg;
    assign select_value    = select_value_reg;
    assign select_value_wr = select_value_wr_reg;
    always @(posedge clk) begin
        if (rst) begin
            ctrl_expect_select_value <= 1'b0;
            select_value_reg    <= 1'b0;
            select_value_wr_reg <= 1'b0;
        end else begin
            select_value_wr_reg <= 1'b0; // default: strobe low
            if (ctrl_word_done) begin
                if (ctrl_expect_select_value) begin
                    select_value_reg    <= ctrl_word_in[0];
                    select_value_wr_reg <= 1'b1;
                    ctrl_expect_select_value <= 1'b0;
                end else if (ctrl_word_in == CTRL_CMD_SET_SELECT_VALUE) begin
                    ctrl_expect_select_value <= 1'b1;
                end else begin
                    ctrl_expect_select_value <= 1'b0;
                end
            end
        end
    end

    // FPGA SW reset pulse: a 1-word CTRL_CMD_FPGA_RESET loads a down-counter;
    // fpga_sw_reset stays high while it counts down, then auto-releases. Lives in
    // the always-up MAC domain so it survives the reset it drives.
    reg [31:0] fpga_reset_cnt;
    assign fpga_sw_reset = (fpga_reset_cnt != 32'd0);
    always @(posedge clk) begin
        if (rst)
            fpga_reset_cnt <= 32'd0;
        else if (ctrl_word_done && rx_payload_tlast && ctrl_word_in == CTRL_CMD_FPGA_RESET)
            fpga_reset_cnt <= FPGA_RESET_CYCLES;
        else if (fpga_reset_cnt != 32'd0)
            fpga_reset_cnt <= fpga_reset_cnt - 32'd1;
    end

    // Chip reset latch — set via CTRL_CMD_SET_CHIP_RESET.
    reg        ctrl_expect_chip_reset_value;
    reg  [1:0] chip_reset_reg;
    assign chip_reset = chip_reset_reg;
    always @(posedge clk) begin
        if (rst) begin
            ctrl_expect_chip_reset_value <= 1'b0;
            chip_reset_reg <= 2'b00;
        end else if (ctrl_word_done) begin
            if (ctrl_expect_chip_reset_value) begin
                chip_reset_reg <= ctrl_word_in[1:0];
                ctrl_expect_chip_reset_value <= 1'b0;
            end else if (ctrl_word_in == CTRL_CMD_SET_CHIP_RESET) begin
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
        end else if (ctrl_word_done) begin
            if (ctrl_expect_fastpath_base_lo_value) begin
                fastpath_base_reg[31:0] <= ctrl_word_in[31:0];
                ctrl_expect_fastpath_base_lo_value <= 1'b0;
            end else if (ctrl_expect_fastpath_base_hi_value) begin
                fastpath_base_reg[63:32] <= ctrl_word_in[31:0];
                ctrl_expect_fastpath_base_hi_value <= 1'b0;
            end else if (ctrl_expect_fastpath_size_lo_value) begin
                fastpath_size_reg[31:0] <= ctrl_word_in[31:0];
                ctrl_expect_fastpath_size_lo_value <= 1'b0;
            end else if (ctrl_expect_fastpath_size_hi_value) begin
                fastpath_size_reg[63:32] <= ctrl_word_in[31:0];
                ctrl_expect_fastpath_size_hi_value <= 1'b0;
            end else if (ctrl_word_in == CTRL_CMD_SET_FASTPATH_BASE_LO) begin
                ctrl_expect_fastpath_base_lo_value <= 1'b1;
            end else if (ctrl_word_in == CTRL_CMD_SET_FASTPATH_BASE_HI) begin
                ctrl_expect_fastpath_base_hi_value <= 1'b1;
            end else if (ctrl_word_in == CTRL_CMD_SET_FASTPATH_SIZE_LO) begin
                ctrl_expect_fastpath_size_lo_value <= 1'b1;
            end else if (ctrl_word_in == CTRL_CMD_SET_FASTPATH_SIZE_HI) begin
                ctrl_expect_fastpath_size_hi_value <= 1'b1;
            end
        end
    end

    // Per-packet ACK FIFO. A single shared ACK register can't survive pipelined
    // packets under upstream backpressure — a later packet's tlast would clobber
    // the pending ACK's fields (the "1420 != 528" corruption). Instead, on every
    // completed packet's tlast (guaranteed for packets that reach this module,
    // since the RX path backpressures rather than dropping mid-stream) push the
    // full ACK payload {byte_count, aux, echoed word0} into a FIFO, and pop it
    // when that ACK is fully transmitted. Depth = MAX_OUTSTANDING pending ACKs.
    localparam ACK_FIFO_DEPTH = (MAX_OUTSTANDING < 2) ? 2 : MAX_OUTSTANDING;
    localparam ACK_FIFO_PTR_W = $clog2(ACK_FIFO_DEPTH);
    reg  [63:0] ack_fifo_mem [0:ACK_FIFO_DEPTH-1];
    reg  [ACK_FIFO_PTR_W-1:0] ack_fifo_wr;
    reg  [ACK_FIFO_PTR_W-1:0] ack_fifo_rd;
    reg  [ACK_FIFO_PTR_W:0]   ack_fifo_count;
    wire ack_fifo_empty = (ack_fifo_count == 0);
    wire ack_fifo_full  = (ack_fifo_count == ACK_FIFO_DEPTH);

    // ACK content captured combinationally at tlast for THIS packet.
    wire        ack_push_ctrl_last  = ctrl_word_done && rx_payload_tlast;
    wire [15:0] ack_push_byte_count = rx_total_bytes + 16'd1;
    wire [15:0] ack_push_aux =
        (ack_push_ctrl_last && (ctrl_word_in == CTRL_CMD_READ_WATCHDOG))        ? {watchdog_fired, watchdog_fire_cnt} :
        (ack_push_ctrl_last && (ctrl_word_in == CTRL_CMD_READ_MAX_OUTSTANDING)) ? MAX_OUTSTANDING[15:0] :
        16'd0;
    wire [31:0] ack_push_word0 =
        (ack_push_ctrl_last && (ctrl_word_in == CTRL_CMD_READ_ACK_COUNT)) ? ack_sent_count : rx_packet_word0;

    wire ack_push = rx_accept && rx_payload_tlast;
    wire ack_pop  = (tx_state == TX_ACK_DATA) && tx_payload_tready && (tx_byte_cnt == 4'd11);

    always @(posedge clk) begin
        if (rst) begin
            ack_fifo_wr    <= {ACK_FIFO_PTR_W{1'b0}};
            ack_fifo_rd    <= {ACK_FIFO_PTR_W{1'b0}};
            ack_fifo_count <= {(ACK_FIFO_PTR_W+1){1'b0}};
        end else begin
            if (ack_push && !ack_fifo_full) begin
                ack_fifo_mem[ack_fifo_wr] <= {ack_push_byte_count, ack_push_aux, ack_push_word0};
                ack_fifo_wr <= ack_fifo_wr + 1'b1;
            end
            if (ack_pop && !ack_fifo_empty)
                ack_fifo_rd <= ack_fifo_rd + 1'b1;
            case ({(ack_push && !ack_fifo_full), (ack_pop && !ack_fifo_empty)})
                2'b10: ack_fifo_count <= ack_fifo_count + 1'b1;
                2'b01: ack_fifo_count <= ack_fifo_count - 1'b1;
                default: ; // both same cycle, or neither: no net change
            endcase
        end
    end

    // ACK currently being transmitted = FIFO head (stable until popped at the end
    // of TX_ACK_DATA, so it holds across the whole HDR+DATA send).
    wire [63:0] ack_head        = ack_fifo_mem[ack_fifo_rd];
    wire [15:0] ack_byte_count  = ack_head[63:48];
    wire [15:0] ack_aux_value   = ack_head[47:32];
    wire [31:0] ack_word0_value = ack_head[31:0];
    wire        ack_pending     = !ack_fifo_empty;

    // ACK payload: 12 bytes = ACK_PAYLOAD[31:0] + byte_count[15:0] +
    // aux/status[15:0] + echoed packet word0[31:0].
    wire [7:0] ack_bytes [0:11];
    assign ack_bytes[0] = ACK_PAYLOAD[31:24];
    assign ack_bytes[1] = ACK_PAYLOAD[23:16];
    assign ack_bytes[2] = ACK_PAYLOAD[15:8];
    assign ack_bytes[3] = ACK_PAYLOAD[7:0];
    assign ack_bytes[4] = ack_byte_count[15:8];
    assign ack_bytes[5] = ack_byte_count[7:0];
    // bytes[6:7] carry aux/status for ctrl query commands:
    //   CTRL_CMD_READ_WATCHDOG        -> {watchdog_fired, watchdog_fire_cnt[14:0]}
    //   CTRL_CMD_READ_MAX_OUTSTANDING -> MAX_OUTSTANDING
    assign ack_bytes[6] = ack_aux_value[15:8];
    assign ack_bytes[7] = ack_aux_value[7:0];
    // ack_byte_count / ack_aux_value / ack_word0_value are the ACK-FIFO head
    // fields (defined above), so ack_bytes reflects THIS packet's ACK.
    assign ack_bytes[8]  = ack_word0_value[31:24];
    assign ack_bytes[9]  = ack_word0_value[23:16];
    assign ack_bytes[10] = ack_word0_value[15:8];
    assign ack_bytes[11] = ack_word0_value[7:0];

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
    assign serial_in_ready = ((tx_state == TX_IDLE) && !ack_pending) ||
                             (tx_state == TX_RESP_NEXT) || (tx_state == TX_RESP_CHUNK);

    always @(posedge clk) begin
        if (rst) begin
            tx_state        <= TX_IDLE;
            tx_hdr_valid    <= 1'b0;
            tx_payload_tvalid <= 1'b0;
            tx_payload_tlast  <= 1'b0;
            tx_byte_cnt     <= 0;
            tx_resp_active  <= 1'b0;
            tx_length       <= 0;
            ack_sent_count  <= 32'd0;
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
                        tx_length    <= 16'd20;   // 8-byte UDP header + 12-byte ACK payload
                        tx_state     <= TX_ACK_HDR;
                        tx_hdr_valid <= 1'b1;
                    end
                    else if (serial_in_valid) begin
                        // Start of a read response. serial_in_ready is asserted
                        // combinationally in IDLE, so this first word is
                        // consumed now. Size the first packet for up to
                        // words_per_chunk words of the total response.
                        tx_resp_shift  <= serial_in_bits;
                        tx_resp_cnt    <= 0;
                        tx_resp_left   <= resp_total_eff - 1'b1;
                        tx_chunk_left  <= chunk0 - 1'b1;
                        tx_length      <= (chunk0 << BYTE_CNT_W) + 16'd8;
                        tx_state       <= TX_RESP_HDR;
                        tx_hdr_valid   <= 1'b1;
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
                    if (tx_byte_cnt == 11)
                        tx_payload_tlast <= 1'b1;

                    if (tx_payload_tready) begin
                        if (tx_byte_cnt == 11) begin
                            ack_sent_count    <= ack_sent_count + 32'd1;
                            tx_state          <= TX_IDLE;
                            tx_payload_tvalid <= 1'b0;
                        end else begin
                            tx_byte_cnt      <= tx_byte_cnt + 1;
                            tx_payload_tdata <= ack_bytes[tx_byte_cnt + 1];
                            if (tx_byte_cnt == 10)
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
                    // tlast only on the last byte of the last word of the chunk
                    if (tx_resp_cnt == BYTES_PER_WORD - 1 && tx_chunk_left == 0)
                        tx_payload_tlast <= 1'b1;

                    if (tx_payload_tready) begin
                        if (tx_resp_cnt == BYTES_PER_WORD - 1) begin
                            // current word fully sent
                            tx_payload_tvalid <= 1'b0;
                            if (tx_chunk_left == 0) begin
                                // this chunk (UDP packet) is complete
                                if (tx_resp_left == 0)
                                    tx_state <= TX_IDLE;        // whole response done
                                else
                                    tx_state <= TX_RESP_CHUNK;  // start next packet
                            end else begin
                                tx_state <= TX_RESP_NEXT;       // next word, same packet
                            end
                        end else begin
                            tx_resp_shift    <= tx_resp_shift >> 8;
                            tx_resp_cnt      <= tx_resp_cnt + 1;
                            tx_payload_tdata <= tx_resp_shift[15:8];
                            if (tx_resp_cnt == BYTES_PER_WORD - 2 && tx_chunk_left == 0)
                                tx_payload_tlast <= 1'b1;
                        end
                    end
                end

                // Fetch the next word of the current chunk (same UDP packet).
                TX_RESP_NEXT: begin
                    if (serial_in_valid) begin
                        tx_resp_shift     <= serial_in_bits;
                        tx_resp_cnt       <= 0;
                        tx_chunk_left     <= tx_chunk_left - 1'b1;
                        tx_resp_left      <= tx_resp_left  - 1'b1;
                        tx_payload_tdata  <= serial_in_bits[7:0];
                        tx_payload_tvalid <= 1'b1;
                        tx_state          <= TX_RESP_DATA;
                    end
                end

                // Start the next UDP packet of a multi-packet response.
                TX_RESP_CHUNK: begin
                    if (serial_in_valid) begin
                        tx_resp_shift <= serial_in_bits;
                        tx_resp_cnt   <= 0;
                        tx_chunk_left <= chunkN - 1'b1;
                        tx_resp_left  <= tx_resp_left - 1'b1;
                        tx_length     <= (chunkN << BYTE_CNT_W) + 16'd8;
                        tx_hdr_valid  <= 1'b1;
                        tx_state      <= TX_RESP_HDR;
                    end
                end

                default: tx_state <= TX_IDLE;
            endcase
        end
    end

    // rx_total_bytes reset is handled inside the main RX always block above.

`ifdef ENABLE_WORD0_DEBUG_ILA
    // Dedicated first-word capture debug:
    //   probe0 : pulses while the current packet is still eligible to capture word 0
    //   probe1 : latched first 32-bit word of the current RX packet
    //   probe2 : {rx_accept, rx_port_is_tsi, 14'd0} for capture context
    wire [15:0] rx_word0_dbg_flags = {rx_accept, rx_port_is_tsi, 14'd0};

    ila_2 udp_payload_word0_ila (
        .clk    (clk),
        .probe0 (rx_capture_word0),
        .probe1 (rx_packet_word0),
        .probe2 (rx_word0_dbg_flags)
    );
`endif

`ifdef ENABLE_DEBUG_MAC_ILA
`define UDP_PAYLOAD_HAVE_MAC_DEBUG_ILA
`endif
`ifdef ENABLE_DEBUG_MAX_ILA
`define UDP_PAYLOAD_HAVE_MAC_DEBUG_ILA
`endif
`ifdef ENABLE_MAC_DEBUG_ILA
`define UDP_PAYLOAD_HAVE_MAC_DEBUG_ILA
`endif

`ifdef UDP_PAYLOAD_HAVE_MAC_DEBUG_ILA
    // RX path is counter/FIFO-driven (no explicit FSM). Expose a derived state:
    //   0: idle/no partial word buffered
    //   1: collecting payload bytes into current serial word
    //   2: FIFO contains completed words
    wire [1:0] rx_dbg_state =
        (rx_port_is_tsi ? !tsi_fifo_empty : !ctrl_fifo_empty) ? 2'd2 :
        ((rx_port_is_tsi ? tsi_rx_byte_cnt : ctrl_rx_byte_cnt) != 0) ? 2'd1 :
        2'd0;

    // TX-side debug split by payload source:
    //   ack_tx_*      : UDP ACK payload bytes emitted by this module
    //   resp_tx_*     : UDP read-response payload bytes emitted by this module
    //   tsi_resp_*    : SERIAL_WIDTH response words arriving from TSIToTileLink
    wire        ack_tx_valid  = tx_payload_tvalid && (tx_state == TX_ACK_DATA);
    wire        ack_tx_ready  = tx_payload_tready && (tx_state == TX_ACK_DATA);
    wire [7:0]  ack_tx_data   = tx_payload_tdata;
    wire        resp_tx_valid = tx_payload_tvalid && (tx_state == TX_RESP_DATA);
    wire        resp_tx_ready = tx_payload_tready && (tx_state == TX_RESP_DATA);
    wire [7:0]  resp_tx_data  = tx_payload_tdata;
    wire        tsi_resp_valid = serial_in_valid;
    wire        tsi_resp_ready = serial_in_ready;
    wire [SERIAL_WIDTH-1:0] tsi_resp_data = serial_in_bits;

    // Single ILA: all RX, TX, and serial TL signals
    ila_6 udp_payload_tsi_ila (
        .clk    (clk),
        .probe0 (rst),
        .probe1 (rx_dbg_state),
        .probe2 (rx_payload_tvalid),
        .probe3 (rx_payload_tready),
        .probe4 (rx_payload_tlast),
        .probe5 (rx_payload_tdata),
        .probe6 (rx_port_is_tsi ? !tsi_fifo_empty : !ctrl_fifo_empty),
        .probe7 (rx_port_is_tsi ? tsi_rx_byte_cnt : ctrl_rx_byte_cnt),
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
        .probe20(ack_tx_valid),
        .probe21(ack_tx_ready),
        .probe22(ack_tx_data),
        .probe23(resp_tx_valid),
        .probe24(resp_tx_ready),
        .probe25(resp_tx_data),
        .probe26(tsi_resp_valid),
        .probe27(tsi_resp_ready),
        .probe28(tsi_resp_data),
        .probe29(serial_out_valid),
        .probe30(serial_out_ready),
        .probe31(serial_out_bits)
    );
`endif

`ifdef UDP_PAYLOAD_HAVE_MAC_DEBUG_ILA
`undef UDP_PAYLOAD_HAVE_MAC_DEBUG_ILA
`endif

endmodule
