///////////////////////////////////////////////////////////////////////////////
// tsi_fastpath_router.v
//
// TSI transaction-level router with a programmable fast window.
//
// Behavior:
//   - Parse one TSI command at a time from the shared TSI stream.
//   - If the full transaction address range is inside
//       [fastpath_base, fastpath_base + fastpath_size)
//     then service it locally on the direct fast TileLink port.
//   - Otherwise replay the original TSI transaction to the legacy backend
//     unchanged and proxy its TSI response back to the host.
//
// This module is parameterized by TSI stream width and fast TileLink beat width.
// Defaults match the current design point: 32-bit TSI and 64-bit TileLink.
///////////////////////////////////////////////////////////////////////////////

module tsi_fastpath_router #(
    parameter integer TSI_WIDTH    = 32,
    parameter integer TL_DATA_BITS = 64,
    parameter integer ADDR_BITS    = 37
) (
    input  wire                     clock,
    input  wire                     reset,

    input  wire [63:0]              fastpath_base,
    input  wire [63:0]              fastpath_size,

    input  wire                     tsi_in_valid,
    output wire                     tsi_in_ready,
    input  wire [TSI_WIDTH-1:0]     tsi_in_bits,

    output wire                     tsi_out_valid,
    input  wire                     tsi_out_ready,
    output wire [TSI_WIDTH-1:0]     tsi_out_bits,
    output wire                     fast_active,

    output wire                     legacy_tsi_in_valid,
    input  wire                     legacy_tsi_in_ready,
    output wire [TSI_WIDTH-1:0]     legacy_tsi_in_bits,

    input  wire                     legacy_tsi_out_valid,
    output wire                     legacy_tsi_out_ready,
    input  wire [TSI_WIDTH-1:0]     legacy_tsi_out_bits,

    output wire                     fast_a_valid,
    input  wire                     fast_a_ready,
    output wire [2:0]               fast_a_opcode,
    output wire [2:0]               fast_a_size,
    output wire [ADDR_BITS-1:0]     fast_a_address,
    output wire [(TL_DATA_BITS/8)-1:0] fast_a_mask,
    output wire [TL_DATA_BITS-1:0]  fast_a_data,
    output wire                     fast_a_corrupt,

    output wire                     fast_d_ready,
    input  wire                     fast_d_valid,
    input  wire [TL_DATA_BITS-1:0]  fast_d_data,
    input  wire                     fast_d_denied,
    input  wire                     fast_d_corrupt
);

    localparam integer TSI_BYTES            = TSI_WIDTH / 8;
    localparam integer TL_BYTES             = TL_DATA_BITS / 8;
    localparam integer TL_CHUNKS_PER_BEAT   = TL_DATA_BITS / TSI_WIDTH;
    localparam integer HDR_CHUNKS_64B       = 64 / TSI_WIDTH;
    localparam integer HDR_IDX_W            = (HDR_CHUNKS_64B <= 1) ? 1 : $clog2(HDR_CHUNKS_64B);
    localparam integer TL_CHUNK_IDX_W       = (TL_CHUNKS_PER_BEAT <= 1) ? 1 : $clog2(TL_CHUNKS_PER_BEAT);
    localparam integer BYTEOFF_W            = (TL_BYTES <= 1) ? 1 : $clog2(TL_BYTES);

    localparam [TSI_WIDTH-1:0] CMD_READ  = {{(TSI_WIDTH-1){1'b0}}, 1'b0};
    localparam [TSI_WIDTH-1:0] CMD_WRITE = {{(TSI_WIDTH-1){1'b0}}, 1'b1};

    localparam [4:0]
        S_CMD            = 5'd0,
        S_ADDR           = 5'd1,
        S_LEN            = 5'd2,
        S_CLASSIFY       = 5'd3,
        S_LEGACY_HDR     = 5'd4,
        S_LEGACY_WRITE   = 5'd5,
        S_LEGACY_READ    = 5'd6,
        S_FAST_READ_REQ  = 5'd7,
        S_FAST_READ_WAIT = 5'd8,
        S_FAST_READ_RESP = 5'd9,
        S_FAST_WRITE_CAP = 5'd10,
        S_FAST_WRITE_REQ = 5'd11,
        S_FAST_WRITE_ACK = 5'd12;

    reg [4:0] state;

    reg [TSI_WIDTH-1:0] cmd_reg;
    reg [63:0] addr_reg;
    reg [63:0] len_reg;

    reg [HDR_IDX_W-1:0] hdr_chunk_idx_reg;
    reg [63:0] words_left_reg;
    reg [63:0] legacy_words_left_reg;
    reg [63:0] curr_addr_reg;

    reg [TL_DATA_BITS-1:0] beat_data_reg;
    reg [TL_DATA_BITS-1:0] write_src_data_reg;
    reg [(TL_DATA_BITS/8)-1:0] beat_mask_reg;
    reg [TL_CHUNK_IDX_W-1:0] beat_chunks_reg;
    reg [TL_CHUNK_IDX_W-1:0] resp_chunk_idx_reg;
    reg [TL_CHUNK_IDX_W-1:0] cap_chunk_idx_reg;

    reg [TL_DATA_BITS-1:0] read_data_reg;
    reg [TSI_WIDTH-1:0] out_word_reg;
    reg                  out_valid_reg;

    reg                  legacy_tx_valid_reg;
    reg [TSI_WIDTH-1:0]  legacy_tx_bits_reg;

    reg                  fast_a_valid_reg;
    reg [2:0]            fast_a_opcode_reg;
    reg [2:0]            fast_a_size_reg;
    reg [ADDR_BITS-1:0]  fast_a_address_reg;
    reg [(TL_DATA_BITS/8)-1:0] fast_a_mask_reg;
    reg [TL_DATA_BITS-1:0] fast_a_data_reg;

    reg [63:0] total_bytes_reg;
    reg        use_fast_reg;

    integer i;
    reg [TL_CHUNK_IDX_W:0] chunks_this_beat_tmp;
    reg [TL_DATA_BITS-1:0] capture_data_tmp;
    reg [(TL_DATA_BITS/8)-1:0] capture_mask_tmp;
    reg [2:0] beat_size_tmp;
    reg [ADDR_BITS-1:0] beat_addr_tmp;
    reg [TL_CHUNK_IDX_W-1:0] first_resp_chunk_tmp;
    reg [TL_CHUNK_IDX_W:0] words_this_beat_tmp;

    initial begin
        if ((TSI_WIDTH % 8) != 0) begin
            $error("tsi_fastpath_router: TSI_WIDTH must be byte-aligned");
            $fatal;
        end
        if ((TL_DATA_BITS % 8) != 0) begin
            $error("tsi_fastpath_router: TL_DATA_BITS must be byte-aligned");
            $fatal;
        end
        if ((64 % TSI_WIDTH) != 0) begin
            $error("tsi_fastpath_router: 64-bit header fields must divide evenly by TSI_WIDTH");
            $fatal;
        end
        if ((TL_DATA_BITS % TSI_WIDTH) != 0) begin
            $error("tsi_fastpath_router: TL_DATA_BITS must divide evenly by TSI_WIDTH");
            $fatal;
        end
        if (TL_DATA_BITS < TSI_WIDTH) begin
            $error("tsi_fastpath_router: TL_DATA_BITS must be >= TSI_WIDTH");
            $fatal;
        end
    end

    task automatic compute_current_beat;
        input  [63:0] addr_in;
        input  [63:0] words_left_in;
        input  [TL_DATA_BITS-1:0] src_data_in;
        output [TL_CHUNK_IDX_W:0] chunks_out;
        output [TL_DATA_BITS-1:0] data_out;
        output [(TL_DATA_BITS/8)-1:0] mask_out;
        output [2:0] size_out;
        output [ADDR_BITS-1:0] addr_out;
        output [TL_CHUNK_IDX_W-1:0] first_chunk_out;
        integer j;
        integer max_chunks_int;
        integer bytes_int;
        integer aligned_idx_int;
        integer first_chunk_int;
        reg [TL_CHUNK_IDX_W:0] chunks_reg;
        reg [TL_DATA_BITS-1:0] data_reg;
        reg [(TL_DATA_BITS/8)-1:0] mask_reg;
        reg [BYTEOFF_W-1:0] byte_off_reg;
        begin
            byte_off_reg = addr_in[BYTEOFF_W-1:0];
            first_chunk_int = byte_off_reg / TSI_BYTES;
            aligned_idx_int = TL_CHUNKS_PER_BEAT - first_chunk_int;
            if (words_left_in < aligned_idx_int)
                chunks_reg = words_left_in[TL_CHUNK_IDX_W:0];
            else
                chunks_reg = aligned_idx_int[TL_CHUNK_IDX_W:0];

            max_chunks_int = chunks_reg;
            bytes_int = max_chunks_int * TSI_BYTES;

            case (bytes_int)
                1: size_out = 3'd0;
                2: size_out = 3'd1;
                4: size_out = 3'd2;
                8: size_out = 3'd3;
                16: size_out = 3'd4;
                32: size_out = 3'd5;
                64: size_out = 3'd6;
                default: size_out = 3'd0;
            endcase

            data_reg = {TL_DATA_BITS{1'b0}};
            mask_reg = {(TL_DATA_BITS/8){1'b0}};
            
            for (j = 0; j < TL_CHUNKS_PER_BEAT; j = j + 1) begin
                 if (j < max_chunks_int) begin
                     data_reg[(first_chunk_int + j) * TSI_WIDTH +: TSI_WIDTH] =
                         src_data_in[j * TSI_WIDTH +: TSI_WIDTH];
                     mask_reg[((first_chunk_int + j) * TSI_BYTES) +: TSI_BYTES] =
                         {TSI_BYTES{1'b1}};
                 end                 
            end       

            addr_out = {addr_in[ADDR_BITS-1:BYTEOFF_W], {BYTEOFF_W{1'b0}}};
            chunks_out = chunks_reg;
            data_out = data_reg;
            mask_out = mask_reg;
            first_chunk_out = first_chunk_int[TL_CHUNK_IDX_W-1:0];
        end
    endtask

    assign tsi_in_ready =
        (state == S_CMD) ||
        (state == S_ADDR) ||
        (state == S_LEN) ||
        (state == S_FAST_WRITE_CAP) ||
        (state == S_LEGACY_WRITE && legacy_tsi_in_ready);

    assign tsi_out_valid = (state == S_LEGACY_READ) ? legacy_tsi_out_valid :
                           (state == S_FAST_READ_RESP) ? out_valid_reg : 1'b0;
    assign tsi_out_bits  = (state == S_LEGACY_READ) ? legacy_tsi_out_bits : out_word_reg;

    assign legacy_tsi_out_ready = (state == S_LEGACY_READ) ? tsi_out_ready : 1'b0;
    assign legacy_tsi_in_valid  = legacy_tx_valid_reg ||
                                  ((state == S_LEGACY_WRITE) && tsi_in_valid);
    assign legacy_tsi_in_bits   = legacy_tx_valid_reg ? legacy_tx_bits_reg : tsi_in_bits;
    assign fast_active          = (state == S_FAST_READ_REQ)  ||
                                  (state == S_FAST_READ_WAIT) ||
                                  (state == S_FAST_READ_RESP) ||
                                  (state == S_FAST_WRITE_CAP) ||
                                  (state == S_FAST_WRITE_REQ) ||
                                  (state == S_FAST_WRITE_ACK);

    assign fast_a_valid   = fast_a_valid_reg;
    assign fast_a_opcode  = fast_a_opcode_reg;
    assign fast_a_size    = fast_a_size_reg;
    assign fast_a_address = fast_a_address_reg;
    assign fast_a_mask    = fast_a_mask_reg;
    assign fast_a_data    = fast_a_data_reg;
    assign fast_a_corrupt = 1'b0;
    assign fast_d_ready   = (state == S_FAST_READ_WAIT) || (state == S_FAST_WRITE_ACK);

    always @(posedge clock) begin
        if (reset) begin
            state                <= S_CMD;
            cmd_reg              <= {TSI_WIDTH{1'b0}};
            addr_reg             <= 64'd0;
            len_reg              <= 64'd0;
            hdr_chunk_idx_reg    <= {HDR_IDX_W{1'b0}};
            words_left_reg       <= 64'd0;
            legacy_words_left_reg <= 64'd0;
            curr_addr_reg        <= 64'd0;
            beat_data_reg        <= {TL_DATA_BITS{1'b0}};
            write_src_data_reg   <= {TL_DATA_BITS{1'b0}};
            beat_mask_reg        <= {(TL_DATA_BITS/8){1'b0}};
            beat_chunks_reg      <= {TL_CHUNK_IDX_W{1'b0}};
            resp_chunk_idx_reg   <= {TL_CHUNK_IDX_W{1'b0}};
            cap_chunk_idx_reg    <= {TL_CHUNK_IDX_W{1'b0}};
            read_data_reg        <= {TL_DATA_BITS{1'b0}};
            out_word_reg         <= {TSI_WIDTH{1'b0}};
            out_valid_reg        <= 1'b0;
            legacy_tx_valid_reg  <= 1'b0;
            legacy_tx_bits_reg   <= {TSI_WIDTH{1'b0}};
            fast_a_valid_reg     <= 1'b0;
            fast_a_opcode_reg    <= 3'd0;
            fast_a_size_reg      <= 3'd0;
            fast_a_address_reg   <= {ADDR_BITS{1'b0}};
            fast_a_mask_reg      <= {(TL_DATA_BITS/8){1'b0}};
            fast_a_data_reg      <= {TL_DATA_BITS{1'b0}};
            total_bytes_reg      <= 64'd0;
            use_fast_reg         <= 1'b0;
        end else begin
            if (out_valid_reg && tsi_out_ready)
                out_valid_reg <= 1'b0;

            if (legacy_tx_valid_reg && legacy_tsi_in_ready)
                legacy_tx_valid_reg <= 1'b0;

            if (fast_a_valid_reg && fast_a_ready)
                fast_a_valid_reg <= 1'b0;

            case (state)
                S_CMD: begin
                    addr_reg          <= 64'd0;
                    len_reg           <= 64'd0;
                    hdr_chunk_idx_reg <= {HDR_IDX_W{1'b0}};
                    if (tsi_in_valid) begin
                        cmd_reg <= tsi_in_bits;
                        state   <= S_ADDR;
                    end
                end

                S_ADDR: begin
                    if (tsi_in_valid) begin
                        addr_reg <= addr_reg | ({32'd0, tsi_in_bits} << (hdr_chunk_idx_reg * TSI_WIDTH));
                        if (hdr_chunk_idx_reg == HDR_CHUNKS_64B-1) begin
                            hdr_chunk_idx_reg <= {HDR_IDX_W{1'b0}};
                            state <= S_LEN;
                        end else begin
                            hdr_chunk_idx_reg <= hdr_chunk_idx_reg + 1'b1;
                        end
                    end
                end

                S_LEN: begin
                    if (tsi_in_valid) begin
                        len_reg <= len_reg | ({32'd0, tsi_in_bits} << (hdr_chunk_idx_reg * TSI_WIDTH));
                        if (hdr_chunk_idx_reg == HDR_CHUNKS_64B-1) begin
                            hdr_chunk_idx_reg <= {HDR_IDX_W{1'b0}};
                            state <= S_CLASSIFY;
                        end else begin
                            hdr_chunk_idx_reg <= hdr_chunk_idx_reg + 1'b1;
                        end
                    end
                end

                S_CLASSIFY: begin
                    words_left_reg        <= len_reg + 64'd1;
                    legacy_words_left_reg <= len_reg + 64'd1;
                    curr_addr_reg         <= addr_reg;
                    total_bytes_reg       <= (len_reg + 64'd1) * TSI_BYTES;
                    use_fast_reg          <= ((cmd_reg == CMD_READ) || (cmd_reg == CMD_WRITE)) &&
                                             (fastpath_size != 64'd0) &&
                                             (addr_reg >= fastpath_base) &&
                                             ((addr_reg + ((len_reg + 64'd1) * TSI_BYTES)) <= (fastpath_base + fastpath_size));
                    if (((cmd_reg == CMD_READ) || (cmd_reg == CMD_WRITE)) &&
                        (fastpath_size != 64'd0) &&
                        (addr_reg >= fastpath_base) &&
                        ((addr_reg + ((len_reg + 64'd1) * TSI_BYTES)) <= (fastpath_base + fastpath_size))) begin
                        if (cmd_reg == CMD_READ)
                            state <= S_FAST_READ_REQ;
                        else begin
                            cap_chunk_idx_reg <= {TL_CHUNK_IDX_W{1'b0}};
                            beat_data_reg     <= {TL_DATA_BITS{1'b0}};
                            write_src_data_reg <= {TL_DATA_BITS{1'b0}};
                            state <= S_FAST_WRITE_CAP;
                        end
                    end else begin
                        hdr_chunk_idx_reg <= {HDR_IDX_W{1'b0}};
                        state <= S_LEGACY_HDR;
                    end
                end

                S_LEGACY_HDR: begin
                    if (!legacy_tx_valid_reg) begin
                        if (hdr_chunk_idx_reg == {HDR_IDX_W{1'b0}})
                            legacy_tx_bits_reg <= cmd_reg;
                        else if (hdr_chunk_idx_reg <= HDR_CHUNKS_64B)
                            legacy_tx_bits_reg <= addr_reg[(hdr_chunk_idx_reg-1'b1) * TSI_WIDTH +: TSI_WIDTH];
                        else
                            legacy_tx_bits_reg <= len_reg[(hdr_chunk_idx_reg-HDR_CHUNKS_64B-1'b1) * TSI_WIDTH +: TSI_WIDTH];
                        legacy_tx_valid_reg <= 1'b1;
                    end

                    if (legacy_tx_valid_reg && legacy_tsi_in_ready) begin
                        if (hdr_chunk_idx_reg == (2*HDR_CHUNKS_64B)) begin
                            if (cmd_reg == CMD_WRITE)
                                state <= S_LEGACY_WRITE;
                            else
                                state <= S_LEGACY_READ;
                        end else begin
                            hdr_chunk_idx_reg <= hdr_chunk_idx_reg + 1'b1;
                        end
                    end
                end

                S_LEGACY_WRITE: begin
                    if (tsi_in_valid && legacy_tsi_in_ready) begin
                        if (legacy_words_left_reg == 64'd1)
                            state <= S_CMD;
                        legacy_words_left_reg <= legacy_words_left_reg - 64'd1;
                    end
                end

                S_LEGACY_READ: begin
                    if (legacy_tsi_out_valid && tsi_out_ready) begin
                        if (legacy_words_left_reg == 64'd1)
                            state <= S_CMD;
                        legacy_words_left_reg <= legacy_words_left_reg - 64'd1;
                    end
                end

                S_FAST_READ_REQ: begin
                    if (!fast_a_valid_reg) begin
                        compute_current_beat(
                            curr_addr_reg,
                            words_left_reg,
                            beat_data_reg,
                            chunks_this_beat_tmp,
                            capture_data_tmp,
                            capture_mask_tmp,
                            beat_size_tmp,
                            beat_addr_tmp,
                            first_resp_chunk_tmp
                        );
                        beat_chunks_reg    <= chunks_this_beat_tmp[TL_CHUNK_IDX_W-1:0];
                        resp_chunk_idx_reg <= first_resp_chunk_tmp;
                        fast_a_opcode_reg  <= 3'd4;
                        fast_a_size_reg    <= beat_size_tmp;
                        fast_a_address_reg <= beat_addr_tmp;
                        fast_a_mask_reg    <= capture_mask_tmp;
                        fast_a_data_reg    <= {TL_DATA_BITS{1'b0}};
                        fast_a_valid_reg   <= 1'b1;
                    end
                    if (fast_a_valid_reg && fast_a_ready)
                        state <= S_FAST_READ_WAIT;
                end

                S_FAST_READ_WAIT: begin
                    if (fast_d_valid) begin
                        read_data_reg      <= fast_d_data;
                        out_word_reg       <= fast_d_data[resp_chunk_idx_reg * TSI_WIDTH +: TSI_WIDTH];
                        out_valid_reg      <= 1'b1;
                        state              <= S_FAST_READ_RESP;
                    end
                end

                S_FAST_READ_RESP: begin
                    if (out_valid_reg && tsi_out_ready) begin
                        if (resp_chunk_idx_reg + 1'b1 == beat_chunks_reg) begin
                            curr_addr_reg  <= curr_addr_reg + (beat_chunks_reg * TSI_BYTES);
                            words_left_reg <= words_left_reg - beat_chunks_reg;
                            if (words_left_reg == beat_chunks_reg)
                                state <= S_CMD;
                            else
                                state <= S_FAST_READ_REQ;
                        end else begin
                            resp_chunk_idx_reg <= resp_chunk_idx_reg + 1'b1;
                            out_word_reg       <= read_data_reg[(resp_chunk_idx_reg + 1'b1) * TSI_WIDTH +: TSI_WIDTH];
                            out_valid_reg      <= 1'b1;
                        end
                    end
                end

                S_FAST_WRITE_CAP: begin
                    if (tsi_in_valid) begin
                        write_src_data_reg = beat_data_reg;
                        write_src_data_reg[cap_chunk_idx_reg * TSI_WIDTH +: TSI_WIDTH] = tsi_in_bits;
                        beat_data_reg[cap_chunk_idx_reg * TSI_WIDTH +: TSI_WIDTH] <= tsi_in_bits;
                        cap_chunk_idx_reg <= cap_chunk_idx_reg + 1'b1;

                        compute_current_beat(
                            curr_addr_reg,
                            words_left_reg,
                            write_src_data_reg,
                            chunks_this_beat_tmp,
                            capture_data_tmp,
                            capture_mask_tmp,
                            beat_size_tmp,
                            beat_addr_tmp,
                            first_resp_chunk_tmp
                        );

                        if (cap_chunk_idx_reg + 1'b1 == chunks_this_beat_tmp[TL_CHUNK_IDX_W-1:0]) begin
                            beat_chunks_reg    <= chunks_this_beat_tmp[TL_CHUNK_IDX_W-1:0];
                            beat_mask_reg      <= capture_mask_tmp;
                            fast_a_opcode_reg  <= (capture_mask_tmp == {(TL_DATA_BITS/8){1'b1}}) ? 3'd0 : 3'd1;
                            fast_a_size_reg    <= beat_size_tmp;
                            fast_a_address_reg <= beat_addr_tmp;
                            fast_a_mask_reg    <= capture_mask_tmp;
                            fast_a_data_reg    <= capture_data_tmp;
                            state              <= S_FAST_WRITE_REQ;
                        end
                    end
                end

                S_FAST_WRITE_REQ: begin
                    if (!fast_a_valid_reg)
                        fast_a_valid_reg <= 1'b1;
                    if (fast_a_valid_reg && fast_a_ready)
                        state <= S_FAST_WRITE_ACK;
                end

                S_FAST_WRITE_ACK: begin
                    if (fast_d_valid) begin
                        if (fast_d_denied || fast_d_corrupt) begin
                            state <= S_CMD;
                        end else begin
                            curr_addr_reg    <= curr_addr_reg + (beat_chunks_reg * TSI_BYTES);
                            words_left_reg   <= words_left_reg - beat_chunks_reg;
                            cap_chunk_idx_reg <= {TL_CHUNK_IDX_W{1'b0}};
                            beat_data_reg    <= {TL_DATA_BITS{1'b0}};
                            write_src_data_reg <= {TL_DATA_BITS{1'b0}};
                            if (words_left_reg == beat_chunks_reg)
                                state <= S_CMD;
                            else
                                state <= S_FAST_WRITE_CAP;
                        end
                    end
                end

                default: state <= S_CMD;
            endcase
        end
    end

endmodule
