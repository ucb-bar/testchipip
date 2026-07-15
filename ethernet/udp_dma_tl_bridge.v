///////////////////////////////////////////////////////////////////////////////
// udp_dma_tl_bridge.v
//
// Simple packet-buffered UDP DMA writer.
//
// Protocol on UDP_PORT+2:
//   bytes[0:3]   = 32-bit little-endian destination address
//   bytes[4:N-1] = payload bytes to write
//
// The bridge buffers one full UDP payload, then emits TileLink PutFullData or
// PutPartialData writes to memory.  A short ACK packet is returned after the
// packet commits:
//   word0 = 0xD04D0001 on success, 0xD04D0000 on error
//   word1 = committed payload byte count
///////////////////////////////////////////////////////////////////////////////

module udp_dma_tl_bridge #(
    parameter integer DMA_PKT_MAX_BYTES = 2048
) (
    input  wire        clk,
    input  wire        rst,

    input  wire [7:0]  rx_payload_tdata,
    input  wire        rx_payload_tvalid,
    input  wire        rx_payload_tlast,
    output wire        rx_payload_tready,

    output reg         tl_a_valid,
    input  wire        tl_a_ready,
    output reg  [2:0]  tl_a_opcode,
    output reg  [2:0]  tl_a_size,
    output reg  [31:0] tl_a_address,
    output reg  [7:0]  tl_a_mask,
    output reg  [63:0] tl_a_data,
    output reg         tl_a_corrupt,

    output wire        tl_d_ready,
    input  wire        tl_d_valid,
    input  wire        tl_d_denied,
    input  wire        tl_d_corrupt,

    output reg         tx_hdr_valid,
    input  wire        tx_hdr_ready,
    output reg  [15:0] tx_length,
    output reg  [7:0]  tx_payload_tdata,
    output reg         tx_payload_tvalid,
    output reg         tx_payload_tlast,
    input  wire        tx_payload_tready
);

    localparam [2:0] ST_IDLE      = 3'd0;
    localparam [2:0] ST_RX        = 3'd1;
    localparam [2:0] ST_SEND_A    = 3'd2;
    localparam [2:0] ST_WAIT_D    = 3'd3;
    localparam [2:0] ST_ACK_HDR   = 3'd4;
    localparam [2:0] ST_ACK_DATA  = 3'd5;

    localparam [31:0] DMA_ACK_OK  = 32'hD04D_0001;
    localparam [31:0] DMA_ACK_ERR = 32'hD04D_0000;

    reg [2:0] state;
    reg [7:0] pkt_mem [0:DMA_PKT_MAX_BYTES-1];
    reg [31:0] base_addr;
    reg [11:0] pkt_total_bytes;
    reg [11:0] payload_bytes;
    reg [11:0] proc_index;
    reg [31:0] proc_addr;
    reg [31:0] ack_word0;
    reg [31:0] ack_word1;
    reg [2:0]  ack_byte_idx;
    reg        packet_error;
    reg [2:0]  hdr_byte_idx;

    integer i;
    reg [63:0] next_beat_data;
    reg [7:0]  next_beat_mask;
    reg [3:0]  next_beat_count;
    reg [31:0] next_beat_addr;

    assign rx_payload_tready = (state == ST_IDLE) || (state == ST_RX && pkt_total_bytes < DMA_PKT_MAX_BYTES);
    assign tl_d_ready = (state == ST_WAIT_D);

    always @(*) begin
        next_beat_data  = 64'd0;
        next_beat_mask  = 8'd0;
        next_beat_count = 4'd0;
        next_beat_addr  = {proc_addr[31:3], 3'b000};

        if (proc_index < payload_bytes) begin
            for (i = 0; i < 8; i = i + 1) begin
                if ((proc_addr[2:0] + i) < 8 && (proc_index + i) < payload_bytes) begin
                    next_beat_data[(proc_addr[2:0] + i) * 8 +: 8] = pkt_mem[proc_index + i];
                    next_beat_mask[proc_addr[2:0] + i] = 1'b1;
                    next_beat_count = next_beat_count + 1'b1;
                end
            end
        end
    end

    always @(posedge clk) begin
        if (rst) begin
            state            <= ST_IDLE;
            base_addr        <= 32'd0;
            pkt_total_bytes  <= 12'd0;
            payload_bytes    <= 12'd0;
            proc_index       <= 12'd0;
            proc_addr        <= 32'd0;
            tl_a_valid       <= 1'b0;
            tl_a_opcode      <= 3'd0;
            tl_a_size        <= 3'd3;
            tl_a_address     <= 32'd0;
            tl_a_mask        <= 8'd0;
            tl_a_data        <= 64'd0;
            tl_a_corrupt     <= 1'b0;
            tx_hdr_valid     <= 1'b0;
            tx_length        <= 16'd8;
            tx_payload_tdata <= 8'd0;
            tx_payload_tvalid<= 1'b0;
            tx_payload_tlast <= 1'b0;
            ack_word0        <= DMA_ACK_ERR;
            ack_word1        <= 32'd0;
            ack_byte_idx     <= 3'd0;
            packet_error     <= 1'b0;
            hdr_byte_idx     <= 3'd0;
        end else begin
            if (tx_payload_tvalid && tx_payload_tready) begin
                tx_payload_tvalid <= 1'b0;
                tx_payload_tlast  <= 1'b0;
            end

            case (state)
                ST_IDLE: begin
                    tl_a_valid      <= 1'b0;
                    tx_hdr_valid    <= 1'b0;
                    pkt_total_bytes <= 12'd0;
                    payload_bytes   <= 12'd0;
                    proc_index      <= 12'd0;
                    proc_addr       <= 32'd0;
                    packet_error    <= 1'b0;
                    hdr_byte_idx    <= 3'd0;
                    if (rx_payload_tvalid && rx_payload_tready) begin
                        state           <= ST_RX;
                        pkt_total_bytes <= 12'd1;
                        if (!rx_payload_tlast) begin
                            base_addr[7:0] <= rx_payload_tdata;
                            hdr_byte_idx   <= 3'd1;
                        end else begin
                            base_addr[7:0] <= rx_payload_tdata;
                            ack_word0      <= DMA_ACK_ERR;
                            ack_word1      <= 32'd0;
                            tx_hdr_valid   <= 1'b1;
                            tx_length      <= 16'd8;
                            ack_byte_idx   <= 3'd0;
                            state          <= ST_ACK_HDR;
                        end
                    end
                end

                ST_RX: begin
                    if (rx_payload_tvalid && rx_payload_tready) begin
                        pkt_total_bytes <= pkt_total_bytes + 1'b1;
                        if (hdr_byte_idx < 4) begin
                            case (hdr_byte_idx)
                                3'd0: base_addr[7:0]   <= rx_payload_tdata;
                                3'd1: base_addr[15:8]  <= rx_payload_tdata;
                                3'd2: base_addr[23:16] <= rx_payload_tdata;
                                3'd3: base_addr[31:24] <= rx_payload_tdata;
                            endcase
                            hdr_byte_idx <= hdr_byte_idx + 1'b1;
                        end else begin
                            pkt_mem[payload_bytes] <= rx_payload_tdata;
                            payload_bytes          <= payload_bytes + 1'b1;
                        end

                        if (rx_payload_tlast) begin
                            proc_addr <= base_addr;
                            state     <= (hdr_byte_idx < 3 || (hdr_byte_idx == 3 && payload_bytes == 0)) ? ST_ACK_HDR : ST_SEND_A;
                            if (hdr_byte_idx < 3 || (hdr_byte_idx == 3 && payload_bytes == 0)) begin
                                ack_word0    <= DMA_ACK_ERR;
                                ack_word1    <= 32'd0;
                                tx_hdr_valid <= 1'b1;
                                tx_length    <= 16'd8;
                                ack_byte_idx <= 3'd0;
                            end
                        end
                    end
                end

                ST_SEND_A: begin
                    tl_a_valid   <= 1'b1;
                    tl_a_opcode  <= (next_beat_mask == 8'hFF) ? 3'd0 : 3'd1;
                    tl_a_size    <= 3'd3;
                    tl_a_address <= next_beat_addr;
                    tl_a_mask    <= next_beat_mask;
                    tl_a_data    <= next_beat_data;
                    tl_a_corrupt <= 1'b0;
                    if (tl_a_valid && tl_a_ready) begin
                        tl_a_valid <= 1'b0;
                        state      <= ST_WAIT_D;
                    end
                end

                ST_WAIT_D: begin
                    if (tl_d_valid) begin
                        if (tl_d_denied || tl_d_corrupt)
                            packet_error <= 1'b1;

                        proc_index <= proc_index + next_beat_count;
                        proc_addr  <= proc_addr + next_beat_count;

                        if (proc_index + next_beat_count >= payload_bytes) begin
                            ack_word0    <= (packet_error || tl_d_denied || tl_d_corrupt) ? DMA_ACK_ERR : DMA_ACK_OK;
                            ack_word1    <= {20'd0, payload_bytes};
                            tx_hdr_valid <= 1'b1;
                            tx_length    <= 16'd8;
                            ack_byte_idx <= 3'd0;
                            state        <= ST_ACK_HDR;
                        end else begin
                            state <= ST_SEND_A;
                        end
                    end
                end

                ST_ACK_HDR: begin
                    if (tx_hdr_valid && tx_hdr_ready) begin
                        tx_hdr_valid     <= 1'b0;
                        tx_payload_tvalid<= 1'b1;
                        tx_payload_tlast <= 1'b0;
                        tx_payload_tdata <= ack_word0[7:0];
                        ack_byte_idx     <= 3'd0;
                        state            <= ST_ACK_DATA;
                    end
                end

                ST_ACK_DATA: begin
                    if (tx_payload_tvalid && tx_payload_tready) begin
                        ack_byte_idx <= ack_byte_idx + 1'b1;
                        if (ack_byte_idx == 3'd7) begin
                            state <= ST_IDLE;
                        end else begin
                            tx_payload_tvalid <= 1'b1;
                            tx_payload_tlast  <= (ack_byte_idx == 3'd6);
                            case (ack_byte_idx + 1'b1)
                                3'd0: tx_payload_tdata <= ack_word0[7:0];
                                3'd1: tx_payload_tdata <= ack_word0[15:8];
                                3'd2: tx_payload_tdata <= ack_word0[23:16];
                                3'd3: tx_payload_tdata <= ack_word0[31:24];
                                3'd4: tx_payload_tdata <= ack_word1[7:0];
                                3'd5: tx_payload_tdata <= ack_word1[15:8];
                                3'd6: tx_payload_tdata <= ack_word1[23:16];
                                default: tx_payload_tdata <= ack_word1[31:24];
                            endcase
                        end
                    end
                end

                default: begin
                    state <= ST_IDLE;
                end
            endcase
        end
    end

endmodule
