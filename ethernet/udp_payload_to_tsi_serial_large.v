///////////////////////////////////////////////////////////////////////////////
// udp_payload_to_tsi_serial_large.v
//
// Variant of udp_payload_to_tsi_serial that collects all TSI read-response
// words into a single UDP packet instead of one packet per word.
//
// RX path: identical to original — pack UDP payload bytes into serial words.
//   Also parses the first two serial words (64-bit TSI command header) to
//   extract:
//     is_read  = !cmd[0]
//     length   = cmd[47:32]  (num 64-bit dwords - 1)
//   From these: resp_words_total = (length+1) * 2  (each 64-bit dword = 2
//   SERIAL_WIDTH=32 words).
//
// TX path: waits for serial_in words, then sends ONE UDP packet containing
//   all resp_words_total words before asserting tlast.
//   UDP payload length = resp_words_total * BYTES_PER_WORD.
//
// ACK protocol: unchanged from original.
///////////////////////////////////////////////////////////////////////////////

module udp_payload_to_tsi_serial_large #(
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
    // RX: UDP payload bytes -> serial words + command header parsing
    // =====================================================================

    reg [SERIAL_WIDTH-1:0] rx_shift;
    reg [BYTE_CNT_W:0]    rx_byte_cnt;
    reg                    rx_word_ready;
    reg [15:0]             rx_total_bytes;

    // Track which serial word within the current packet (for cmd parsing)
    reg [15:0] rx_word_idx;

    // Latched command fields
    reg        cmd_is_read;
    reg [15:0] cmd_length;     // num_dwords - 1

    // resp_words_total = (cmd_length+1)*2 (set when read command parsed)
    reg [16:0] resp_words_remaining; // words left to send in current response

    assign rx_payload_tready = !rx_word_ready;

    always @(posedge clk) begin
        if (rst) begin
            rx_byte_cnt          <= 0;
            rx_word_ready        <= 1'b0;
            serial_out_valid     <= 1'b0;
            rx_total_bytes       <= 0;
            rx_word_idx          <= 0;
            cmd_is_read          <= 1'b0;
            cmd_length           <= 0;
            resp_words_remaining <= 0;
        end else begin
            if (serial_out_valid && serial_out_ready) begin
                serial_out_valid <= 1'b0;
                rx_word_ready    <= 1'b0;
            end

            if (rx_payload_tvalid && rx_payload_tready) begin
                rx_shift[rx_byte_cnt*8 +: 8] <= rx_payload_tdata;
                rx_byte_cnt    <= rx_byte_cnt + 1;
                rx_total_bytes <= rx_total_bytes + 1;

                if (rx_byte_cnt == BYTES_PER_WORD - 1 || rx_payload_tlast) begin
                    serial_out_bits                      <= rx_shift;
                    serial_out_bits[rx_byte_cnt*8 +: 8] <= rx_payload_tdata;
                    serial_out_valid <= 1'b1;
                    rx_word_ready    <= 1'b1;
                    rx_byte_cnt      <= 0;

                    // Parse command header words
                    if (rx_word_idx == 0) begin
                        // bits[0] = write flag
                        cmd_is_read <= !(rx_shift[0] | rx_payload_tdata[0]); // bit0 of word0
                    end else if (rx_word_idx == 1) begin
                        // bits[15:0] of word1 = length (num_dwords - 1)
                        cmd_length <= {rx_payload_tdata[7:0],
                                       rx_shift[rx_byte_cnt*8 +: 8]};
                        // Will be captured cleanly below via registered path;
                        // actual latch happens in separate always block
                    end

                    rx_word_idx <= rx_word_idx + 1;

                    if (rx_payload_tlast) begin
                        rx_shift    <= {SERIAL_WIDTH{1'b0}};
                        rx_word_idx <= 0;
                    end
                end
            end
        end
    end

    // Latch resp_words_remaining when word 1 (length field) is complete
    // and the command is a read.  Each 64-bit TSI dword = 2 serial words.
    wire rx_word1_complete = rx_payload_tvalid && rx_payload_tready &&
                             (rx_byte_cnt == BYTES_PER_WORD - 1) &&
                             (rx_word_idx == 1);

    // Build the length from the incoming bytes correctly:
    // word1 bytes arrive LSB-first into rx_shift, last byte is rx_payload_tdata
    // rx_shift already has bytes 0..2, rx_payload_tdata is byte 3
    // cmd bits[47:32] = word1[15:0] = bytes 4..5 of the 8-byte header
    // With SERIAL_WIDTH=32: word1 byte0=cmd[32..39], byte1=cmd[40..47],
    //   byte2=cmd[48..55], byte3=cmd[56..63]
    // length = cmd[47:32] = word1[15:0] = {byte1, byte0}
    wire [15:0] rx_word1_length = {rx_shift[15:8], rx_shift[7:0]};

    always @(posedge clk) begin
        if (rst) begin
            resp_words_remaining <= 0;
        end else if (rx_word1_complete && cmd_is_read) begin
            // (rx_word1_length + 1) dwords * 2 serial words each
            resp_words_remaining <= (rx_word1_length + 16'd1) << 1;
        end else if (serial_in_ready && serial_in_valid) begin
            if (resp_words_remaining != 0)
                resp_words_remaining <= resp_words_remaining - 1;
        end
    end

    // =====================================================================
    // ACK + TX response state machine
    //
    // States:
    //   IDLE      : waiting
    //   ACK_HDR   : send UDP header for ACK
    //   ACK_DATA  : send 8-byte ACK payload
    //   RESP_HDR  : send UDP header for response (sized for all words)
    //   RESP_DATA : stream bytes of current word
    //   RESP_NEXT : fetch next serial_in word (between words, same packet)
    // =====================================================================

    localparam TX_IDLE      = 3'd0;
    localparam TX_ACK_HDR   = 3'd1;
    localparam TX_ACK_DATA  = 3'd2;
    localparam TX_RESP_HDR  = 3'd3;
    localparam TX_RESP_DATA = 3'd4;
    localparam TX_RESP_NEXT = 3'd5;

    reg [2:0]  tx_state;
    reg [3:0]  tx_byte_cnt;
    reg        ack_pending;

    // Words remaining to send in the current response packet
    reg [16:0] tx_words_left;

    always @(posedge clk) begin
        if (rst)
            ack_pending <= 1'b0;
        else if (rx_payload_tvalid && rx_payload_tready && rx_payload_tlast)
            ack_pending <= 1'b1;
        else if (tx_state == TX_ACK_HDR && tx_hdr_ready)
            ack_pending <= 1'b0;
    end

    reg [15:0] ack_byte_count;
    always @(posedge clk) begin
        if (rst)
            ack_byte_count <= 0;
        else if (rx_payload_tvalid && rx_payload_tready && rx_payload_tlast)
            ack_byte_count <= rx_total_bytes;
    end

    wire [7:0] ack_bytes [0:7];
    assign ack_bytes[0] = ACK_PAYLOAD[31:24];
    assign ack_bytes[1] = ACK_PAYLOAD[23:16];
    assign ack_bytes[2] = ACK_PAYLOAD[15:8];
    assign ack_bytes[3] = ACK_PAYLOAD[7:0];
    assign ack_bytes[4] = ack_byte_count[15:8];
    assign ack_bytes[5] = ack_byte_count[7:0];
    assign ack_bytes[6] = 8'h00;
    assign ack_bytes[7] = 8'h00;

    reg [SERIAL_WIDTH-1:0] tx_resp_shift;
    reg [BYTE_CNT_W:0]     tx_resp_cnt;

    always @(posedge clk) begin
        if (rst) begin
            tx_state          <= TX_IDLE;
            tx_hdr_valid      <= 1'b0;
            tx_payload_tvalid <= 1'b0;
            tx_payload_tlast  <= 1'b0;
            serial_in_ready   <= 1'b0;
            tx_byte_cnt       <= 0;
            tx_words_left     <= 0;
            tx_length         <= 0;
        end else begin
            tx_hdr_valid      <= 1'b0;
            tx_payload_tvalid <= 1'b0;
            tx_payload_tlast  <= 1'b0;
            serial_in_ready   <= 1'b0;

            case (tx_state)
                TX_IDLE: begin
                    tx_byte_cnt <= 0;
                    if (ack_pending) begin
                        tx_hdr_valid <= 1'b1;
                        tx_length    <= 16'd16;
                        tx_state     <= TX_ACK_HDR;
                    end else if (serial_in_valid && resp_words_remaining > 0) begin
                        // Latch first word; size packet for all response words
                        tx_resp_shift   <= serial_in_bits;
                        serial_in_ready <= 1'b1;
                        tx_resp_cnt     <= 0;
                        tx_words_left   <= resp_words_remaining - 1;
                        // UDP payload length = total words * bytes/word
                        tx_length    <= resp_words_remaining * BYTES_PER_WORD + 16'd8;
                        tx_hdr_valid <= 1'b1;
                        tx_state     <= TX_RESP_HDR;
                    end
                end

                TX_ACK_HDR: begin
                    tx_hdr_valid <= 1'b1;
                    if (tx_hdr_ready) begin
                        tx_hdr_valid <= 1'b0;
                        tx_state     <= TX_ACK_DATA;
                        tx_byte_cnt  <= 0;
                    end
                end

                TX_ACK_DATA: begin
                    tx_payload_tdata  <= ack_bytes[tx_byte_cnt];
                    tx_payload_tvalid <= 1'b1;
                    if (tx_byte_cnt == 7)
                        tx_payload_tlast <= 1'b1;
                    if (tx_payload_tready) begin
                        tx_byte_cnt <= tx_byte_cnt + 1;
                        if (tx_byte_cnt == 7)
                            tx_state <= TX_IDLE;
                    end
                end

                TX_RESP_HDR: begin
                    tx_hdr_valid <= 1'b1;
                    if (tx_hdr_ready) begin
                        tx_hdr_valid <= 1'b0;
                        tx_resp_cnt  <= 0;
                        tx_state     <= TX_RESP_DATA;
                    end
                end

                TX_RESP_DATA: begin
                    tx_payload_tdata  <= tx_resp_shift[7:0];
                    tx_payload_tvalid <= 1'b1;
                    // Assert tlast only on last byte of last word
                    if (tx_resp_cnt == BYTES_PER_WORD - 1 && tx_words_left == 0)
                        tx_payload_tlast <= 1'b1;

                    if (tx_payload_tready) begin
                        tx_resp_shift <= tx_resp_shift >> 8;
                        tx_resp_cnt   <= tx_resp_cnt + 1;
                        if (tx_resp_cnt == BYTES_PER_WORD - 1) begin
                            tx_resp_cnt <= 0;
                            if (tx_words_left == 0) begin
                                tx_state <= TX_IDLE;
                            end else begin
                                tx_words_left   <= tx_words_left - 1;
                                serial_in_ready <= 1'b1;
                                tx_state        <= TX_RESP_NEXT;
                            end
                        end
                    end
                end

                TX_RESP_NEXT: begin
                    // Wait for next serial_in word
                    if (serial_in_valid) begin
                        tx_resp_shift   <= serial_in_bits;
                        serial_in_ready <= 1'b1;
                        tx_resp_cnt     <= 0;
                        tx_state        <= TX_RESP_DATA;
                    end
                end

                default: tx_state <= TX_IDLE;
            endcase
        end
    end

    // Reset total byte counter at start of each new packet
    always @(posedge clk) begin
        if (rst)
            rx_total_bytes <= 0;
        else if (rx_payload_tvalid && rx_payload_tready && rx_payload_tlast)
            rx_total_bytes <= 0;
    end

endmodule
