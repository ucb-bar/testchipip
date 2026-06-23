module tsi_fastpath_write_router #(
  parameter integer TSI_WIDTH = 32,
  parameter integer TL_DATA_BITS = 64,
  parameter integer ADDR_BITS = 37,
  parameter integer RX_FIFO_DEPTH = 128,
  parameter integer MAX_BURST_BEATS = 8,
  parameter integer MAX_OUTSTANDING = 8,
  parameter integer SOURCE_BITS = 5
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
  output wire [SOURCE_BITS-1:0]   fast_a_source,
  output wire [ADDR_BITS-1:0]     fast_a_address,
  output wire [(TL_DATA_BITS/8)-1:0] fast_a_mask,
  output wire [TL_DATA_BITS-1:0]  fast_a_data,
  output wire                     fast_a_corrupt,
  output wire                     fast_d_ready,
  input  wire                     fast_d_valid,
  input  wire [SOURCE_BITS-1:0]   fast_d_source,
  input  wire [TL_DATA_BITS-1:0]  fast_d_data,
  input  wire                     fast_d_denied,
  input  wire                     fast_d_corrupt
);

  localparam integer TSI_BYTES = TSI_WIDTH / 8;
  localparam integer TL_BYTES = TL_DATA_BITS / 8;
  localparam integer TL_CHUNKS_PER_BEAT = TL_DATA_BITS / TSI_WIDTH;
  localparam integer HDR64_WORDS = 64 / TSI_WIDTH;
  localparam integer HDR_WORDS = 1 + (2 * HDR64_WORDS);
  localparam integer FIFO_PTR_W = (RX_FIFO_DEPTH <= 1) ? 1 : $clog2(RX_FIFO_DEPTH);
  localparam integer BYTEOFF_W = (TL_BYTES <= 1) ? 1 : $clog2(TL_BYTES);
  localparam integer BURST_CNT_W = (MAX_BURST_BEATS <= 1) ? 1 : $clog2(MAX_BURST_BEATS + 1);
  localparam integer BURST_IDX_W = (MAX_BURST_BEATS <= 1) ? 1 : $clog2(MAX_BURST_BEATS);
  localparam integer OUT_CNT_W = (MAX_OUTSTANDING <= 1) ? 1 : $clog2(MAX_OUTSTANDING + 1);
  localparam integer TL_SIZE_BITS = $clog2(TL_BYTES);

  localparam [TSI_WIDTH-1:0] CMD_READ  = {{(TSI_WIDTH-1){1'b0}}, 1'b0};
  localparam [TSI_WIDTH-1:0] CMD_WRITE = {{(TSI_WIDTH-1){1'b0}}, 1'b1};
  localparam [63:0] LEGACY_FORCE_ADDR0_REG_ADDR = 64'h00000001FFFFFE00;
  localparam [63:0] LEGACY_FORCE_ADDR1_REG_ADDR = 64'h00000001FFFFFE08;

  localparam [3:0]
    S_HDR          = 4'd0,
    S_DECODE       = 4'd1,
    S_CLASSIFY     = 4'd2,
    S_LEGACY_HDR   = 4'd3,
    S_LEGACY_WRITE = 4'd4,
    S_LEGACY_READ  = 4'd5,
    S_FAST_PLAN    = 4'd6,
    S_FAST_FILL    = 4'd7,
    S_FAST_ISSUE   = 4'd8,
    S_FAST_WAIT    = 4'd9,
    S_CTRL_WRITE   = 4'd10,
    // Fast read path: Get a burst over fast TL, then serialize beats to tsi_out.
    S_FREAD_PLAN   = 4'd11,  // size a read burst (full beats covering the words)
    S_FREAD_ISSUE  = 4'd12,  // issue one TL Get (opcode 4, full mask, source 0)
    S_FREAD_RECV   = 4'd13,  // capture fast_d data beats (single outstanding, in order)
    S_FREAD_SEND   = 4'd14;  // serialize captured words to tsi_out

  reg [TSI_WIDTH-1:0] fifo_mem [0:RX_FIFO_DEPTH-1];
  reg [FIFO_PTR_W-1:0] fifo_wr_ptr;
  reg [FIFO_PTR_W-1:0] fifo_rd_ptr;
  reg [FIFO_PTR_W:0] fifo_count;

  reg [TSI_WIDTH-1:0] hdr_regs [0:HDR_WORDS-1];
  reg [2:0] hdr_idx_reg;
  reg [2:0] legacy_hdr_idx_reg;
  reg [3:0] state;

  reg [TSI_WIDTH-1:0] cmd_reg;
  reg [63:0] words_left_reg;
  reg [63:0] hdr_addr_reg;
  reg [63:0] hdr_len_reg;
  reg        hdr_fast_route_reg;
  reg        hdr_fast_read_route_reg;
  reg        hdr_ctrl_route_reg;

  // Fast read path state
  reg [2:0]            read_size_reg;     // TL size for the Get (log2 of burst bytes)
  reg [BURST_IDX_W-1:0] fread_recv_idx_reg; // beat index while capturing fast_d
  reg [63:0]           fread_send_idx_reg;  // word index while serializing to tsi_out

  reg legacy_hdr_valid_reg;
  reg [TSI_WIDTH-1:0] legacy_hdr_bits_reg;
  reg [63:0] legacy_force_addr0_reg;
  reg [63:0] legacy_force_addr1_reg;
  reg        ctrl_write_is_addr0_reg;
  reg        ctrl_write_word_idx_reg;
  reg [63:0] ctrl_write_data_reg;

  reg [TL_DATA_BITS-1:0] burst_data_mem [0:MAX_BURST_BEATS-1];
  reg [(TL_DATA_BITS/8)-1:0] burst_mask_mem [0:MAX_BURST_BEATS-1];
  reg [BURST_CNT_W-1:0] burst_beats_reg;
  reg [BURST_IDX_W-1:0] fill_beat_idx_reg;
  reg [BURST_IDX_W-1:0] issue_beat_idx_reg;
  reg [0:0]             fill_chunk_idx_reg;
  reg [0:0]             burst_first_chunk_reg;
  reg [63:0]            burst_capture_words_left_reg;
  reg [63:0]            burst_word_count_reg;
  reg [63:0]            fill_addr_reg;
  reg [63:0]            fill_words_left_reg;
  reg [ADDR_BITS-1:0]   burst_addr_reg;
  reg [2:0]             burst_size_reg;
  reg [2:0]             burst_opcode_reg;
  reg [SOURCE_BITS-1:0] burst_source_reg;
  reg                   burst_source_valid_reg;
  reg                   transaction_done_reg;
  reg [TL_DATA_BITS-1:0] fill_data_reg;
  reg [(TL_DATA_BITS/8)-1:0] fill_mask_reg;

  reg [MAX_OUTSTANDING-1:0] free_sources_reg;
  reg [OUT_CNT_W-1:0] outstanding_reg;

  reg                   fast_a_valid_reg;
  reg [2:0]             fast_a_opcode_reg;
  reg [2:0]             fast_a_size_reg;
  reg [SOURCE_BITS-1:0] fast_a_source_reg;
  reg [ADDR_BITS-1:0]   fast_a_address_reg;
  reg [(TL_DATA_BITS/8)-1:0] fast_a_mask_reg;
  reg [TL_DATA_BITS-1:0] fast_a_data_reg;

  wire fifo_out_ready_reg;
  wire fifo_full = (fifo_count == RX_FIFO_DEPTH);
  wire fifo_empty = (fifo_count == 0);
  wire fifo_out_valid = !fifo_empty;
  wire [TSI_WIDTH-1:0] fifo_out_bits = fifo_mem[fifo_rd_ptr];
  wire do_push = tsi_in_valid && tsi_in_ready;
  wire do_pop = fifo_out_valid && fifo_out_ready_reg;

  integer i;
  integer j;
  wire [63:0] hdr_addr_tmp = {hdr_regs[2], hdr_regs[1]};
  wire [63:0] hdr_len_tmp = {hdr_regs[4], hdr_regs[3]};
  wire [63:0] total_bytes_tmp = (hdr_len_tmp + 64'd1) * TSI_BYTES;
  wire       hdr_force_legacy_tmp = (((hdr_addr_tmp + total_bytes_tmp) > legacy_force_addr0_reg) &&
                                     (hdr_addr_tmp < (legacy_force_addr0_reg + 64'd8))) ||
                                    (((hdr_addr_tmp + total_bytes_tmp) > legacy_force_addr1_reg) &&
                                     (hdr_addr_tmp < (legacy_force_addr1_reg + 64'd8)));
  wire [63:0] planned_beats_tmp =
    ((fill_addr_reg[BYTEOFF_W-1:0] == {BYTEOFF_W{1'b0}}) &&
     (fill_words_left_reg >= TL_CHUNKS_PER_BEAT) &&
     (MAX_BURST_BEATS >= 8) &&
     (fill_words_left_reg >= (8 * TL_CHUNKS_PER_BEAT)) &&
     ((fill_addr_reg % (8 * TL_BYTES)) == 0)) ? 64'd8 :
    ((fill_addr_reg[BYTEOFF_W-1:0] == {BYTEOFF_W{1'b0}}) &&
     (fill_words_left_reg >= TL_CHUNKS_PER_BEAT) &&
     (MAX_BURST_BEATS >= 4) &&
     (fill_words_left_reg >= (4 * TL_CHUNKS_PER_BEAT)) &&
     ((fill_addr_reg % (4 * TL_BYTES)) == 0)) ? 64'd4 :
    ((fill_addr_reg[BYTEOFF_W-1:0] == {BYTEOFF_W{1'b0}}) &&
     (fill_words_left_reg >= TL_CHUNKS_PER_BEAT) &&
     (MAX_BURST_BEATS >= 2) &&
     (fill_words_left_reg >= (2 * TL_CHUNKS_PER_BEAT)) &&
     ((fill_addr_reg % (2 * TL_BYTES)) == 0)) ? 64'd2 : 64'd1;
  wire [63:0] planned_first_chunk_tmp = fill_addr_reg[BYTEOFF_W-1:0] / TSI_BYTES;
  wire [63:0] planned_words_aligned_tmp = TL_CHUNKS_PER_BEAT - planned_first_chunk_tmp;
  wire [63:0] planned_words_capped_tmp = (fill_words_left_reg < planned_words_aligned_tmp) ? fill_words_left_reg : planned_words_aligned_tmp;
  wire [63:0] planned_words_tmp = (planned_beats_tmp > 64'd1) ? (planned_beats_tmp * TL_CHUNKS_PER_BEAT) : planned_words_capped_tmp;
  reg [SOURCE_BITS-1:0] selected_source_tmp;
  reg                   selected_source_valid_tmp;
  wire have_free_source = |free_sources_reg[MAX_OUTSTANDING-1:0];
  wire [63:0] first_chunk_this_beat_tmp = (fill_beat_idx_reg == 0) ? burst_first_chunk_reg : 64'd0;
  wire [63:0] beat_word_limit_tmp = TL_CHUNKS_PER_BEAT - first_chunk_this_beat_tmp;
  wire [63:0] chunk_slot_tmp = first_chunk_this_beat_tmp + fill_chunk_idx_reg;
  wire [TL_DATA_BITS-1:0] next_fill_data =
    fill_data_reg | ({{(TL_DATA_BITS-TSI_WIDTH){1'b0}}, fifo_out_bits} << (chunk_slot_tmp * TSI_WIDTH));
  wire [(TL_DATA_BITS/8)-1:0] next_fill_mask =
    fill_mask_reg | ({{((TL_DATA_BITS/8)-TSI_BYTES){1'b0}}, {TSI_BYTES{1'b1}}} << (chunk_slot_tmp * TSI_BYTES));
  wire [63:0] curr_transfer_bytes_tmp = burst_word_count_reg * TSI_BYTES;
  wire [2:0] curr_transfer_size_tmp =
    (curr_transfer_bytes_tmp == 64'd1) ? 3'd0 :
    (curr_transfer_bytes_tmp == 64'd2) ? 3'd1 :
    (curr_transfer_bytes_tmp == 64'd4) ? 3'd2 :
    (curr_transfer_bytes_tmp == 64'd8) ? 3'd3 :
    (curr_transfer_bytes_tmp == 64'd16) ? 3'd4 :
    (curr_transfer_bytes_tmp == 64'd32) ? 3'd5 :
    (curr_transfer_bytes_tmp == 64'd64) ? 3'd6 : TL_SIZE_BITS[2:0];

  // Fast read: a burst reads `planned_beats_tmp` full TL beats; the Get size
  // is log2(beats * TL_BYTES).
  wire [63:0] read_bytes_tmp = planned_beats_tmp * TL_BYTES;
  wire [2:0]  read_size_tmp =
    (read_bytes_tmp == 64'd8)  ? 3'd3 :
    (read_bytes_tmp == 64'd16) ? 3'd4 :
    (read_bytes_tmp == 64'd32) ? 3'd5 :
    (read_bytes_tmp == 64'd64) ? 3'd6 : TL_SIZE_BITS[2:0];
  // Serialize captured beats: word k of the burst lives at global chunk
  // (first_chunk + k) -> beat (/TL_CHUNKS_PER_BEAT), chunk (%TL_CHUNKS_PER_BEAT).
  wire [63:0] fread_global_chunk = {{63{1'b0}}, burst_first_chunk_reg} + fread_send_idx_reg;
  wire [63:0] fread_send_beat  = fread_global_chunk / TL_CHUNKS_PER_BEAT;
  wire [63:0] fread_send_chunk = fread_global_chunk % TL_CHUNKS_PER_BEAT;
  wire [TL_DATA_BITS-1:0] fread_send_beat_data = burst_data_mem[fread_send_beat[BURST_IDX_W-1:0]];
  wire [TSI_WIDTH-1:0] fread_send_word =
    fread_send_beat_data[fread_send_chunk[BYTEOFF_W-1:0]*TSI_WIDTH +: TSI_WIDTH];
  wire fread_sending = (state == S_FREAD_SEND) && (fread_send_idx_reg < burst_word_count_reg);

  assign tsi_in_ready = !fifo_full;
  assign tsi_out_valid = (state == S_LEGACY_READ) ? legacy_tsi_out_valid :
                         fread_sending                              ? 1'b1 : 1'b0;
  assign tsi_out_bits = (state == S_FREAD_SEND) ? fread_send_word : legacy_tsi_out_bits;
  assign legacy_tsi_out_ready = (state == S_LEGACY_READ) ? tsi_out_ready : 1'b0;
  assign legacy_tsi_in_valid = legacy_hdr_valid_reg || ((state == S_LEGACY_WRITE) && fifo_out_valid);
  assign legacy_tsi_in_bits = legacy_hdr_valid_reg ? legacy_hdr_bits_reg : fifo_out_bits;
  assign fast_active = (state == S_FAST_FILL) || (state == S_FAST_ISSUE) || (state == S_FAST_WAIT) || (outstanding_reg != 0) ||
                       (state == S_FREAD_ISSUE) || (state == S_FREAD_RECV) || (state == S_FREAD_SEND);
  assign fast_a_valid = fast_a_valid_reg;
  assign fast_a_opcode = fast_a_opcode_reg;
  assign fast_a_size = fast_a_size_reg;
  assign fast_a_source = fast_a_source_reg;
  assign fast_a_address = fast_a_address_reg;
  assign fast_a_mask = fast_a_mask_reg;
  assign fast_a_data = fast_a_data_reg;
  assign fast_a_corrupt = 1'b0;
  assign fast_d_ready = (state == S_FAST_FILL) || (state == S_FAST_ISSUE) || (state == S_FAST_WAIT) || (outstanding_reg != 0) ||
                        (state == S_FREAD_RECV);

  // A D response is consumed (a request retires) this cycle.
  wire fast_d_consume = fast_d_valid && (fast_d_source < MAX_OUTSTANDING) && !free_sources_reg[fast_d_source];
  // A new burst is committed (its final beat accepted) this cycle.
  wire burst_issue_commit = (state == S_FAST_ISSUE) && fast_a_valid_reg && fast_a_ready &&
                            ((issue_beat_idx_reg + 1'b1) == burst_beats_reg);

`ifdef ENABLE_DEBUG_ILA
  // Single consolidated debug ILA for the fast-path write router.
  // The `ila_fastpath` IP core must be generated with these 21 probes and
  // matching widths:
  //   probe0=4, probe1=64, probe2=1, probe3=1, probe4=32, probe5=1,
  //   probe6=1, probe7=1, probe8=32, probe9=1, probe10=1, probe11=115,
  //   probe12=1, probe13=1, probe14=64, probe15=64, probe16=64,
  //   probe17=64, probe18=64, probe19=64, probe20=64
  ila_fastpath udp_tsi_fastpath_ila (
    .clk     (clock),
    .probe0  (state),
    .probe1  (hdr_len_reg),
    .probe2  (tsi_in_valid),
    .probe3  (tsi_in_ready),
    .probe4  (tsi_in_bits),
    .probe5  (fast_active),
    .probe6  (do_pop),
    .probe7  (do_push),
    .probe8  (cmd_reg),
    .probe9  (fast_a_valid),
    .probe10 (fast_a_ready),
    .probe11 ({fast_a_opcode, fast_a_size, fast_a_address, fast_a_mask, fast_a_data}),
    .probe12 (fifo_out_valid),
    .probe13 (fifo_out_ready_reg),
    .probe14 (words_left_reg),
    .probe15 (burst_capture_words_left_reg),
    .probe16 (fill_words_left_reg),
    .probe17 (planned_first_chunk_tmp),
    .probe18 (planned_words_aligned_tmp),
    .probe19 (planned_words_capped_tmp),
    .probe20 (planned_words_tmp)
  );
`endif

  assign fifo_out_ready_reg =
    (state == S_HDR) ? 1'b1 :
    (state == S_LEGACY_WRITE) ? (!legacy_hdr_valid_reg && legacy_tsi_in_ready) :
    (state == S_FAST_FILL) ? (fill_words_left_reg != 64'd0) :
    (state == S_CTRL_WRITE) ? 1'b1 : 1'b0;

  always @* begin
    selected_source_tmp = {SOURCE_BITS{1'b0}};
    selected_source_valid_tmp = 1'b0;
    for (i = 0; i < MAX_OUTSTANDING; i = i + 1)
      if (free_sources_reg[i] && !selected_source_valid_tmp) begin
        selected_source_tmp = i[SOURCE_BITS-1:0];
        selected_source_valid_tmp = 1'b1;
      end
  end

  always @(posedge clock) begin
    if (reset) begin
      fifo_wr_ptr <= {FIFO_PTR_W{1'b0}};
      fifo_rd_ptr <= {FIFO_PTR_W{1'b0}};
      fifo_count <= {(FIFO_PTR_W+1){1'b0}};
      hdr_idx_reg <= 3'd0;
      legacy_hdr_idx_reg <= 3'd0;
      state <= S_HDR;
      cmd_reg <= {TSI_WIDTH{1'b0}};
      words_left_reg <= 64'd0;
      hdr_addr_reg <= 64'd0;
      hdr_len_reg <= 64'd0;
      hdr_fast_route_reg <= 1'b0;
      hdr_fast_read_route_reg <= 1'b0;
      hdr_ctrl_route_reg <= 1'b0;
      read_size_reg <= 3'd0;
      fread_recv_idx_reg <= {BURST_IDX_W{1'b0}};
      fread_send_idx_reg <= 64'd0;
      legacy_hdr_valid_reg <= 1'b0;
      legacy_hdr_bits_reg <= {TSI_WIDTH{1'b0}};
      legacy_force_addr0_reg <= 64'd0;
      legacy_force_addr1_reg <= 64'd0;
      ctrl_write_is_addr0_reg <= 1'b0;
      ctrl_write_word_idx_reg <= 1'b0;
      ctrl_write_data_reg <= 64'd0;
      burst_beats_reg <= {BURST_CNT_W{1'b0}};
      fill_beat_idx_reg <= {BURST_IDX_W{1'b0}};
      issue_beat_idx_reg <= {BURST_IDX_W{1'b0}};
      fill_chunk_idx_reg <= 1'b0;
      burst_first_chunk_reg <= 1'b0;
      burst_capture_words_left_reg <= 64'd0;
      burst_word_count_reg <= 64'd0;
      fill_addr_reg <= 64'd0;
      fill_words_left_reg <= 64'd0;
      burst_addr_reg <= {ADDR_BITS{1'b0}};
      burst_size_reg <= 3'd0;
      burst_opcode_reg <= 3'd0;
      burst_source_reg <= {SOURCE_BITS{1'b0}};
      burst_source_valid_reg <= 1'b0;
      transaction_done_reg <= 1'b0;
      fill_data_reg <= {TL_DATA_BITS{1'b0}};
      fill_mask_reg <= {(TL_DATA_BITS/8){1'b0}};
      free_sources_reg <= {MAX_OUTSTANDING{1'b0}};
      outstanding_reg <= {OUT_CNT_W{1'b0}};
      fast_a_valid_reg <= 1'b0;
      fast_a_opcode_reg <= 3'd0;
      fast_a_size_reg <= 3'd0;
      fast_a_source_reg <= {SOURCE_BITS{1'b0}};
      fast_a_address_reg <= {ADDR_BITS{1'b0}};
      fast_a_mask_reg <= {(TL_DATA_BITS/8){1'b0}};
      fast_a_data_reg <= {TL_DATA_BITS{1'b0}};
      for (i = 0; i < HDR_WORDS; i = i + 1)
        hdr_regs[i] <= {TSI_WIDTH{1'b0}};
      for (i = 0; i < MAX_OUTSTANDING; i = i + 1)
        free_sources_reg[i] <= 1'b1;
      for (i = 0; i < MAX_BURST_BEATS; i = i + 1) begin
        burst_data_mem[i] <= {TL_DATA_BITS{1'b0}};
        burst_mask_mem[i] <= {(TL_DATA_BITS/8){1'b0}};
      end
    end else begin
      if (do_push)
        fifo_mem[fifo_wr_ptr] <= tsi_in_bits;

      case ({do_push, do_pop})
        2'b10: begin
          fifo_wr_ptr <= fifo_wr_ptr + 1'b1;
          fifo_count <= fifo_count + 1'b1;
        end
        2'b01: begin
          fifo_rd_ptr <= fifo_rd_ptr + 1'b1;
          fifo_count <= fifo_count - 1'b1;
        end
        2'b11: begin
          fifo_wr_ptr <= fifo_wr_ptr + 1'b1;
          fifo_rd_ptr <= fifo_rd_ptr + 1'b1;
        end
        default: begin end
      endcase

      if (legacy_hdr_valid_reg && legacy_tsi_in_ready)
        legacy_hdr_valid_reg <= 1'b0;

      if (fast_a_valid_reg && fast_a_ready)
        fast_a_valid_reg <= 1'b0;

      if (fast_d_consume)
        free_sources_reg[fast_d_source] <= 1'b1;

      // Single coherent update of the outstanding-request counter. Issue
      // (increment) and D-response (decrement) can land in the same cycle;
      // updating outstanding_reg from two separate statements would drop one
      // and hang in S_FAST_WAIT. 2'b11 is a net no-op; 2'b00 leaves it alone.
      case ({burst_issue_commit, fast_d_consume})
        2'b10: outstanding_reg <= outstanding_reg + 1'b1;
        2'b01: if (outstanding_reg != 0) outstanding_reg <= outstanding_reg - 1'b1;
        default: ;
      endcase

      case (state)
        S_HDR: begin
          transaction_done_reg <= 1'b0;
          if (do_pop) begin
            hdr_regs[hdr_idx_reg] <= fifo_out_bits;
            if (hdr_idx_reg == HDR_WORDS-1) begin
              hdr_idx_reg <= 3'd0;
              state <= S_DECODE;
            end else begin
              hdr_idx_reg <= hdr_idx_reg + 1'b1;
            end
          end
        end

        S_DECODE: begin
          cmd_reg <= {{(TSI_WIDTH-1){1'b0}}, hdr_regs[0][0]};
          hdr_addr_reg <= hdr_addr_tmp;
          hdr_len_reg <= hdr_len_tmp;
          hdr_ctrl_route_reg <= hdr_regs[0][0] &&
                                (hdr_len_tmp == 64'd1) &&
                                ((hdr_addr_tmp == LEGACY_FORCE_ADDR0_REG_ADDR) ||
                                 (hdr_addr_tmp == LEGACY_FORCE_ADDR1_REG_ADDR));
          hdr_fast_route_reg <= hdr_regs[0][0] &&
                                (fastpath_size != 64'd0) &&
                                !hdr_force_legacy_tmp &&
                                (hdr_addr_tmp >= fastpath_base) &&
                                ((hdr_addr_tmp + total_bytes_tmp) <= (fastpath_base + fastpath_size));
          hdr_fast_read_route_reg <= !hdr_regs[0][0] &&
                                (fastpath_size != 64'd0) &&
                                !hdr_force_legacy_tmp &&
                                (hdr_addr_tmp >= fastpath_base) &&
                                ((hdr_addr_tmp + total_bytes_tmp) <= (fastpath_base + fastpath_size));
          state <= S_CLASSIFY;
        end

        S_CLASSIFY: begin
          words_left_reg <= hdr_len_reg + 64'd1;
          if (hdr_ctrl_route_reg) begin
            ctrl_write_is_addr0_reg <= (hdr_addr_reg == LEGACY_FORCE_ADDR0_REG_ADDR);
            ctrl_write_word_idx_reg <= 1'b0;
            ctrl_write_data_reg <= 64'd0;
            state <= S_CTRL_WRITE;
          end else if (hdr_fast_route_reg) begin
            fill_addr_reg <= hdr_addr_reg;
            fill_words_left_reg <= hdr_len_reg + 64'd1;
            fill_beat_idx_reg <= {BURST_IDX_W{1'b0}};
            issue_beat_idx_reg <= {BURST_IDX_W{1'b0}};
            fill_chunk_idx_reg <= 1'b0;
            burst_capture_words_left_reg <= 64'd0;
            burst_word_count_reg <= 64'd0;
            fill_data_reg <= {TL_DATA_BITS{1'b0}};
            fill_mask_reg <= {(TL_DATA_BITS/8){1'b0}};
            burst_source_valid_reg <= 1'b0;
            transaction_done_reg <= 1'b0;
            state <= S_FAST_PLAN;
          end else if (hdr_fast_read_route_reg) begin
            fill_addr_reg <= hdr_addr_reg;
            fill_words_left_reg <= hdr_len_reg + 64'd1;
            state <= S_FREAD_PLAN;
          end else begin
            legacy_hdr_idx_reg <= 3'd0;
            state <= S_LEGACY_HDR;
          end
        end

        S_FAST_PLAN: begin
          if (fill_words_left_reg != 0) begin
            burst_capture_words_left_reg <= planned_words_tmp;
            burst_word_count_reg <= planned_words_tmp;
            burst_first_chunk_reg <= planned_first_chunk_tmp[0:0];
            fill_data_reg <= {TL_DATA_BITS{1'b0}};
            fill_mask_reg <= {(TL_DATA_BITS/8){1'b0}};
          end
          state <= S_FAST_FILL;
        end

        S_LEGACY_HDR: begin
          if (!legacy_hdr_valid_reg) begin
            legacy_hdr_bits_reg <= (legacy_hdr_idx_reg == 0) ?
                                   {{(TSI_WIDTH-1){1'b0}}, hdr_regs[0][0]} :
                                   hdr_regs[legacy_hdr_idx_reg];
            legacy_hdr_valid_reg <= 1'b1;
          end
          if (legacy_hdr_valid_reg && legacy_tsi_in_ready) begin
            if (legacy_hdr_idx_reg == HDR_WORDS-1) begin
              if (hdr_regs[0][0])
                state <= S_LEGACY_WRITE;
              else
                state <= S_LEGACY_READ;
            end
            legacy_hdr_idx_reg <= legacy_hdr_idx_reg + 1'b1;
          end
        end

        S_LEGACY_WRITE: begin
          if (do_pop && legacy_tsi_in_ready) begin
            if (words_left_reg == 64'd1)
              state <= S_HDR;
            words_left_reg <= words_left_reg - 64'd1;
          end
        end

        S_LEGACY_READ: begin
          if (legacy_tsi_out_valid && tsi_out_ready) begin
            if (words_left_reg == 64'd1)
              state <= S_HDR;
            words_left_reg <= words_left_reg - 64'd1;
          end
        end

        S_FAST_FILL: begin
          if (do_pop) begin
            fill_words_left_reg <= fill_words_left_reg - 64'd1;
            words_left_reg <= fill_words_left_reg - 64'd1;

            if ((burst_capture_words_left_reg == 64'd1) || ((fill_chunk_idx_reg + 1'b1) == beat_word_limit_tmp)) begin
              burst_data_mem[fill_beat_idx_reg] <= next_fill_data;
              burst_mask_mem[fill_beat_idx_reg] <= next_fill_mask;
              fill_chunk_idx_reg <= 1'b0;
              fill_data_reg <= {TL_DATA_BITS{1'b0}};
              fill_mask_reg <= {(TL_DATA_BITS/8){1'b0}};

              if (burst_capture_words_left_reg == 64'd1) begin
                burst_beats_reg <= fill_beat_idx_reg + 1'b1;
                burst_addr_reg <= {fill_addr_reg[ADDR_BITS-1:BYTEOFF_W], {BYTEOFF_W{1'b0}}};
                if ((burst_word_count_reg > TL_CHUNKS_PER_BEAT) && (burst_first_chunk_reg == 0)) begin
                  burst_opcode_reg <= 3'd0;
                  burst_size_reg <= curr_transfer_size_tmp;
                end else begin
                  burst_opcode_reg <= (next_fill_mask == {(TL_DATA_BITS/8){1'b1}}) ? 3'd0 : 3'd1;
                  burst_size_reg <= TL_SIZE_BITS[2:0];
                end
                issue_beat_idx_reg <= {BURST_IDX_W{1'b0}};
                burst_source_valid_reg <= 1'b0;
                transaction_done_reg <= (fill_words_left_reg == 64'd1);
                burst_capture_words_left_reg <= 64'd0;
                state <= S_FAST_ISSUE;
              end else begin
                fill_beat_idx_reg <= fill_beat_idx_reg + 1'b1;
                burst_capture_words_left_reg <= burst_capture_words_left_reg - 64'd1;
              end
            end else begin
              fill_data_reg <= next_fill_data;
              fill_mask_reg <= next_fill_mask;
              fill_chunk_idx_reg <= fill_chunk_idx_reg + 1'b1;
              burst_capture_words_left_reg <= burst_capture_words_left_reg - 64'd1;
            end
          end
        end

        S_FAST_ISSUE: begin
          if (!burst_source_valid_reg && have_free_source) begin
            burst_source_reg <= selected_source_tmp;
            burst_source_valid_reg <= 1'b1;
          end else if (burst_source_valid_reg && !fast_a_valid_reg) begin
            fast_a_valid_reg <= 1'b1;
            fast_a_opcode_reg <= burst_opcode_reg;
            fast_a_size_reg <= burst_size_reg;
            fast_a_source_reg <= burst_source_reg;
            fast_a_address_reg <= burst_addr_reg;
            fast_a_mask_reg <= burst_mask_mem[issue_beat_idx_reg];
            fast_a_data_reg <= burst_data_mem[issue_beat_idx_reg];
          end

          if (fast_a_valid_reg && fast_a_ready) begin
            if (issue_beat_idx_reg + 1'b1 == burst_beats_reg) begin
              free_sources_reg[burst_source_reg] <= 1'b0;
              fill_addr_reg <= fill_addr_reg + (burst_word_count_reg * TSI_BYTES);
              fill_beat_idx_reg <= {BURST_IDX_W{1'b0}};
              issue_beat_idx_reg <= {BURST_IDX_W{1'b0}};
              burst_source_valid_reg <= 1'b0;
              if (transaction_done_reg)
                state <= S_FAST_WAIT;
              else
                state <= S_FAST_PLAN;
            end else begin
              issue_beat_idx_reg <= issue_beat_idx_reg + 1'b1;
            end
          end
        end

        S_FAST_WAIT: begin
          if ((outstanding_reg == 0) ||
              ((outstanding_reg == 1) && fast_d_valid && (fast_d_source < MAX_OUTSTANDING) && !free_sources_reg[fast_d_source])) begin
            state <= S_HDR;
          end
        end

        S_CTRL_WRITE: begin
          if (do_pop) begin
            if (!ctrl_write_word_idx_reg) begin
              ctrl_write_data_reg[31:0] <= fifo_out_bits;
              ctrl_write_word_idx_reg <= 1'b1;
            end else begin
              ctrl_write_data_reg[63:32] <= fifo_out_bits;
              if (ctrl_write_is_addr0_reg)
                legacy_force_addr0_reg <= {fifo_out_bits, ctrl_write_data_reg[31:0]};
              else
                legacy_force_addr1_reg <= {fifo_out_bits, ctrl_write_data_reg[31:0]};
              state <= S_HDR;
            end
          end
        end

        // ---- Fast read: plan a burst, Get it, capture beats, serialize ----
        S_FREAD_PLAN: begin
          // Read full beats covering up to planned_words_tmp words of the
          // request; extract the requested words on serialize.
          burst_word_count_reg  <= planned_words_tmp;
          burst_first_chunk_reg <= planned_first_chunk_tmp[0:0];
          burst_beats_reg       <= planned_beats_tmp[BURST_CNT_W-1:0];
          burst_addr_reg        <= {fill_addr_reg[ADDR_BITS-1:BYTEOFF_W], {BYTEOFF_W{1'b0}}};
          read_size_reg         <= read_size_tmp;
          state                 <= S_FREAD_ISSUE;
        end

        S_FREAD_ISSUE: begin
          // Single outstanding read: use source 0 and leave the write source
          // pool / outstanding counter untouched (so fast_d_consume ignores it).
          if (!fast_a_valid_reg) begin
            fast_a_valid_reg   <= 1'b1;
            fast_a_opcode_reg  <= 3'd4;                       // Get
            fast_a_size_reg    <= read_size_reg;
            fast_a_source_reg  <= {SOURCE_BITS{1'b0}};
            fast_a_address_reg <= burst_addr_reg;
            fast_a_mask_reg    <= {(TL_DATA_BITS/8){1'b1}};   // Get reads the full beat
            fast_a_data_reg    <= {TL_DATA_BITS{1'b0}};
          end
          if (fast_a_valid_reg && fast_a_ready) begin
            fread_recv_idx_reg <= {BURST_IDX_W{1'b0}};
            state              <= S_FREAD_RECV;
          end
        end

        S_FREAD_RECV: begin
          // fast_d_ready is asserted in this state; D beats for our single Get
          // arrive in order. Capture them into burst_data_mem.
          if (fast_d_valid) begin
            burst_data_mem[fread_recv_idx_reg] <= fast_d_data;
            if (fread_recv_idx_reg + 1'b1 == burst_beats_reg) begin
              fread_send_idx_reg <= 64'd0;
              state              <= S_FREAD_SEND;
            end else begin
              fread_recv_idx_reg <= fread_recv_idx_reg + 1'b1;
            end
          end
        end

        S_FREAD_SEND: begin
          // Serialize burst_word_count words to tsi_out (combinational mux on
          // fread_send_word); advance on each accepted word.
          if (fread_send_idx_reg < burst_word_count_reg) begin
            if (tsi_out_ready)
              fread_send_idx_reg <= fread_send_idx_reg + 64'd1;
          end else begin
            fill_addr_reg       <= fill_addr_reg + (burst_word_count_reg * TSI_BYTES);
            fill_words_left_reg <= fill_words_left_reg - burst_word_count_reg;
            if (fill_words_left_reg == burst_word_count_reg)
              state <= S_HDR;            // whole read done
            else
              state <= S_FREAD_PLAN;     // more words remain
          end
        end

        default: state <= S_HDR;
      endcase
    end
  end
endmodule
