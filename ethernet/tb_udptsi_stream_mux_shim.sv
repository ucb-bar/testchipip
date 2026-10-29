`timescale 1ns / 1ps

module tb_udptsi_stream_mux_shim;

  localparam time CLK_PERIOD_NS     = 8ns;  // 125 MHz
  localparam time CLK200_PERIOD_NS  = 5ns;  // 200 MHz

  localparam [47:0] FPGA_MAC = 48'h00_0A_35_00_01_02;
  localparam [31:0] FPGA_IP  = {8'd192, 8'd168, 8'd1, 8'd10};
  localparam [31:0] HOST_IP  = {8'd192, 8'd168, 8'd1, 8'd1};
  localparam [47:0] HOST_MAC = 48'h02_00_00_00_00_01;

  localparam [15:0] ETH_TYPE_ARP = 16'h0806;
  localparam [15:0] ETH_TYPE_IP  = 16'h0800;
  localparam [15:0] UDP_PORT_TSI = 16'd7000;
  localparam [15:0] HOST_UDP_SRC = 16'd12345;

  localparam int UDP_PAYLOAD_BYTES = 1400;
  localparam int TSI_HEADER_BYTES  = 20;
  localparam int TSI_DATA_BYTES    = UDP_PAYLOAD_BYTES - TSI_HEADER_BYTES;
  localparam longint unsigned TSI_ADDR = 64'h0000_0000_8000_0000;

  reg         clock = 1'b0;
  reg         reset = 1'b1;
  reg         io_select = 1'b0;
  wire [3:0]  io_phyRgmii_txd;
  wire        io_phyRgmii_tx_ctl;
  wire        io_phyRgmii_txc;
  reg  [3:0]  io_phyRgmii_rxd = 4'd0;
  reg         io_phyRgmii_rx_ctl = 1'b0;
  reg         io_phyRgmii_rxc = 1'b0;
  wire        io_phyResetN;
  wire        io_phyLinkUp;
  wire        io_phyMdc;
  tri1        io_phyMdio;
  reg         io_gtx_clk = 1'b0;
  reg         io_gtx_clk90 = 1'b0;
  reg         io_clk_200 = 1'b0;
  reg         io_uart_rx = 1'b1;
  wire        io_uart_tx;
  wire        io_fastActive;
  reg         io_fast_tl_a_ready = 1'b1;
  wire        io_fast_tl_a_valid;
  wire [2:0]  io_fast_tl_a_bits_opcode;
  wire [2:0]  io_fast_tl_a_bits_size;
  wire [4:0]  io_fast_tl_a_bits_source;
  wire [36:0] io_fast_tl_a_bits_address;
  wire [7:0]  io_fast_tl_a_bits_mask;
  wire [63:0] io_fast_tl_a_bits_data;
  wire        io_fast_tl_a_bits_corrupt;
  wire        io_fast_tl_d_ready;
  reg         io_fast_tl_d_valid = 1'b0;
  reg  [4:0]  io_fast_tl_d_bits_source = 5'd0;
  reg  [63:0] io_fast_tl_d_bits_data = 64'd0;
  reg         io_fast_tl_d_bits_denied = 1'b0;
  reg         io_fast_tl_d_bits_corrupt = 1'b0;
  wire        io_chip0Serial_out_ready;
  reg         io_chip0Serial_out_valid = 1'b0;
  reg  [31:0] io_chip0Serial_out_bits = 32'd0;
  reg         io_chip0Serial_in_ready = 1'b1;
  wire        io_chip0Serial_in_valid;
  wire [31:0] io_chip0Serial_in_bits;
  wire        io_chip1Serial_out_ready;
  reg         io_chip1Serial_out_valid = 1'b0;
  reg  [31:0] io_chip1Serial_out_bits = 32'd0;
  reg         io_chip1Serial_in_ready = 1'b1;
  wire        io_chip1Serial_in_valid;
  wire [31:0] io_chip1Serial_in_bits;
  wire [1:0]  io_chipReset;

  integer rx_frame_count = 0;
  integer tx_frame_count = 0;
  reg rx_ctl_q = 1'b0;
  reg tx_ctl_q = 1'b0;
  bit measure_next_rx = 1'b0;
  bit measure_next_tx = 1'b0;
  time measured_rx_start = 0;
  time measured_tx_start = 0;

`ifdef ENABLE_FSDB
  initial begin : waveform_dump_fsdb
    $fsdbDumpfile("tb_udptsi_stream_mux_shim.fsdb");
    $fsdbDumpvars(0, tb_udptsi_stream_mux_shim, "+all");
  end
`else
  initial begin : waveform_dump_vcd
    $dumpfile("tb_udptsi_stream_mux_shim.vcd");
    $dumpvars(0, tb_udptsi_stream_mux_shim);
  end
`endif

  always #(CLK_PERIOD_NS/2) clock = ~clock;
  always #(CLK_PERIOD_NS/2) io_gtx_clk = ~io_gtx_clk;
  initial begin
    #2ns;
    forever #(CLK_PERIOD_NS/2) io_gtx_clk90 = ~io_gtx_clk90;
  end
  always #(CLK200_PERIOD_NS/2) io_clk_200 = ~io_clk_200;
  always #(CLK_PERIOD_NS/2) io_phyRgmii_rxc = ~io_phyRgmii_rxc;

  UDPTSIStreamMuxShim dut (
    .clock                    (clock),
    .reset                    (reset),
    .io_select                (io_select),
    .io_phyRgmii_txd          (io_phyRgmii_txd),
    .io_phyRgmii_tx_ctl       (io_phyRgmii_tx_ctl),
    .io_phyRgmii_txc          (io_phyRgmii_txc),
    .io_phyRgmii_rxd          (io_phyRgmii_rxd),
    .io_phyRgmii_rx_ctl       (io_phyRgmii_rx_ctl),
    .io_phyRgmii_rxc          (io_phyRgmii_rxc),
    .io_phyResetN             (io_phyResetN),
    .io_phyLinkUp             (io_phyLinkUp),
    .io_phyMdc                (io_phyMdc),
    .io_phyMdio               (io_phyMdio),
    .io_gtx_clk               (io_gtx_clk),
    .io_gtx_clk90             (io_gtx_clk90),
    .io_clk_200               (io_clk_200),
    .io_uart_rx               (io_uart_rx),
    .io_uart_tx               (io_uart_tx),
    .io_fastActive            (io_fastActive),
    .io_fast_tl_a_ready       (io_fast_tl_a_ready),
    .io_fast_tl_a_valid       (io_fast_tl_a_valid),
    .io_fast_tl_a_bits_opcode (io_fast_tl_a_bits_opcode),
    .io_fast_tl_a_bits_size   (io_fast_tl_a_bits_size),
    .io_fast_tl_a_bits_source (io_fast_tl_a_bits_source),
    .io_fast_tl_a_bits_address(io_fast_tl_a_bits_address),
    .io_fast_tl_a_bits_mask   (io_fast_tl_a_bits_mask),
    .io_fast_tl_a_bits_data   (io_fast_tl_a_bits_data),
    .io_fast_tl_a_bits_corrupt(io_fast_tl_a_bits_corrupt),
    .io_fast_tl_d_ready       (io_fast_tl_d_ready),
    .io_fast_tl_d_valid       (io_fast_tl_d_valid),
    .io_fast_tl_d_bits_source (io_fast_tl_d_bits_source),
    .io_fast_tl_d_bits_data   (io_fast_tl_d_bits_data),
    .io_fast_tl_d_bits_denied (io_fast_tl_d_bits_denied),
    .io_fast_tl_d_bits_corrupt(io_fast_tl_d_bits_corrupt),
    .io_chip0Serial_out_ready (io_chip0Serial_out_ready),
    .io_chip0Serial_out_valid (io_chip0Serial_out_valid),
    .io_chip0Serial_out_bits  (io_chip0Serial_out_bits),
    .io_chip0Serial_in_ready  (io_chip0Serial_in_ready),
    .io_chip0Serial_in_valid  (io_chip0Serial_in_valid),
    .io_chip0Serial_in_bits   (io_chip0Serial_in_bits),
    .io_chip1Serial_out_ready (io_chip1Serial_out_ready),
    .io_chip1Serial_out_valid (io_chip1Serial_out_valid),
    .io_chip1Serial_out_bits  (io_chip1Serial_out_bits),
    .io_chip1Serial_in_ready  (io_chip1Serial_in_ready),
    .io_chip1Serial_in_valid  (io_chip1Serial_in_valid),
    .io_chip1Serial_in_bits   (io_chip1Serial_in_bits),
    .io_chipReset             (io_chipReset)
  );

  always @(posedge io_phyRgmii_rxc) begin
    rx_ctl_q <= io_phyRgmii_rx_ctl;
    if (!rx_ctl_q && io_phyRgmii_rx_ctl) begin
      rx_frame_count <= rx_frame_count + 1;
      if (measure_next_rx) begin
        measured_rx_start <= $time;
        measure_next_rx <= 1'b0;
        $display("[%0t] Measured RX frame start", $time);
      end
    end
  end

  always @(posedge io_phyRgmii_txc) begin
    tx_ctl_q <= io_phyRgmii_tx_ctl;
    if (!tx_ctl_q && io_phyRgmii_tx_ctl) begin
      tx_frame_count <= tx_frame_count + 1;
      if (measure_next_tx) begin
        measured_tx_start <= $time;
        measure_next_tx <= 1'b0;
        $display("[%0t] Measured TX frame start", $time);
      end
    end
  end

  function automatic [31:0] crc32_ether_byte(input [31:0] crc_in, input byte unsigned data);
    reg [31:0] crc;
    integer i;
    begin
      crc = crc_in ^ data;
      for (i = 0; i < 8; i = i + 1) begin
        if (crc[0]) crc = (crc >> 1) ^ 32'hEDB88320;
        else        crc = (crc >> 1);
      end
      crc32_ether_byte = crc;
    end
  endfunction

  function automatic [15:0] ip_checksum20(input byte unsigned hdr[0:19]);
    integer i;
    integer sum;
    begin
      sum = 0;
      for (i = 0; i < 20; i = i + 2) begin
        sum = sum + {hdr[i], hdr[i+1]};
      end
      while (sum >> 16) begin
        sum = (sum & 16'hFFFF) + (sum >> 16);
      end
      ip_checksum20 = ~sum[15:0];
    end
  endfunction

  task automatic push_u16_be(ref byte unsigned bytes[$], input [15:0] value);
    begin
      bytes.push_back(value[15:8]);
      bytes.push_back(value[7:0]);
    end
  endtask

  task automatic push_u32_be(ref byte unsigned bytes[$], input [31:0] value);
    begin
      bytes.push_back(value[31:24]);
      bytes.push_back(value[23:16]);
      bytes.push_back(value[15:8]);
      bytes.push_back(value[7:0]);
    end
  endtask

  task automatic push_u32_le(ref byte unsigned bytes[$], input [31:0] value);
    begin
      bytes.push_back(value[7:0]);
      bytes.push_back(value[15:8]);
      bytes.push_back(value[23:16]);
      bytes.push_back(value[31:24]);
    end
  endtask

  task automatic push_mac(ref byte unsigned bytes[$], input [47:0] mac);
    integer i;
    begin
      for (i = 5; i >= 0; i = i - 1) begin
        bytes.push_back(mac[i*8 +: 8]);
      end
    end
  endtask

  task automatic push_ipv4(ref byte unsigned bytes[$], input [31:0] ip);
    begin
      bytes.push_back(ip[31:24]);
      bytes.push_back(ip[23:16]);
      bytes.push_back(ip[15:8]);
      bytes.push_back(ip[7:0]);
    end
  endtask

  task automatic append_fcs(ref byte unsigned frame[$]);
    reg [31:0] crc;
    integer i;
    begin
      crc = 32'hFFFF_FFFF;
      for (i = 0; i < frame.size(); i = i + 1) begin
        crc = crc32_ether_byte(crc, frame[i]);
      end
      crc = ~crc;
      frame.push_back(crc[7:0]);
      frame.push_back(crc[15:8]);
      frame.push_back(crc[23:16]);
      frame.push_back(crc[31:24]);
    end
  endtask

  task automatic build_arp_request(output byte unsigned frame[$]);
    begin
      frame = {};
      push_mac(frame, 48'hFF_FF_FF_FF_FF_FF);
      push_mac(frame, HOST_MAC);
      push_u16_be(frame, ETH_TYPE_ARP);
      push_u16_be(frame, 16'h0001);
      push_u16_be(frame, ETH_TYPE_IP);
      frame.push_back(8'd6);
      frame.push_back(8'd4);
      push_u16_be(frame, 16'h0001);
      push_mac(frame, HOST_MAC);
      push_ipv4(frame, HOST_IP);
      push_mac(frame, 48'd0);
      push_ipv4(frame, FPGA_IP);
      append_fcs(frame);
    end
  endtask

  task automatic build_udp_write_frame(output byte unsigned frame[$]);
    byte unsigned ip_hdr[0:19];
    byte unsigned payload[$];
    int i;
    int tsi_len_words;
    reg [15:0] ip_total_len;
    reg [15:0] udp_total_len;
    reg [15:0] ip_csum;
    begin
      frame = {};
      payload = {};

      tsi_len_words = TSI_DATA_BYTES / 4 - 1;
      ip_total_len = 20 + 8 + UDP_PAYLOAD_BYTES;
      udp_total_len = 8 + UDP_PAYLOAD_BYTES;
      push_u32_le(payload, 32'd1);
      push_u32_le(payload, TSI_ADDR[31:0]);
      push_u32_le(payload, TSI_ADDR[63:32]);
      push_u32_le(payload, tsi_len_words[31:0]);
      push_u32_le(payload, 32'd0);
      for (i = 0; i < TSI_DATA_BYTES / 4; i = i + 1) begin
        push_u32_le(payload, 32'h5A5A5A5A);
      end

      push_mac(frame, FPGA_MAC);
      push_mac(frame, HOST_MAC);
      push_u16_be(frame, ETH_TYPE_IP);

      ip_hdr[0]  = 8'h45;
      ip_hdr[1]  = 8'h00;
      ip_hdr[2]  = ip_total_len[15:8];
      ip_hdr[3]  = ip_total_len[7:0];
      ip_hdr[4]  = 8'h12;
      ip_hdr[5]  = 8'h34;
      ip_hdr[6]  = 8'h40;
      ip_hdr[7]  = 8'h00;
      ip_hdr[8]  = 8'h40;
      ip_hdr[9]  = 8'h11;
      ip_hdr[10] = 8'h00;
      ip_hdr[11] = 8'h00;
      ip_hdr[12] = HOST_IP[31:24];
      ip_hdr[13] = HOST_IP[23:16];
      ip_hdr[14] = HOST_IP[15:8];
      ip_hdr[15] = HOST_IP[7:0];
      ip_hdr[16] = FPGA_IP[31:24];
      ip_hdr[17] = FPGA_IP[23:16];
      ip_hdr[18] = FPGA_IP[15:8];
      ip_hdr[19] = FPGA_IP[7:0];

      ip_csum = ip_checksum20(ip_hdr);
      ip_hdr[10] = ip_csum[15:8];
      ip_hdr[11] = ip_csum[7:0];

      for (i = 0; i < 20; i = i + 1) begin
        frame.push_back(ip_hdr[i]);
      end

      push_u16_be(frame, HOST_UDP_SRC);
      push_u16_be(frame, UDP_PORT_TSI);
      push_u16_be(frame, udp_total_len);
      push_u16_be(frame, 16'h0000);

      for (i = 0; i < payload.size(); i = i + 1) begin
        frame.push_back(payload[i]);
      end

      append_fcs(frame);
    end
  endtask

  task automatic drive_rgmii_byte(input byte unsigned data_byte, input bit dv);
    begin
      @(negedge io_phyRgmii_rxc);
      io_phyRgmii_rxd    <= data_byte[3:0];
      io_phyRgmii_rx_ctl <= dv;
      @(posedge io_phyRgmii_rxc);
      io_phyRgmii_rxd    <= data_byte[7:4];
      io_phyRgmii_rx_ctl <= dv;
    end
  endtask

  task automatic drive_idle_cycles(input integer num_bytes);
    integer i;
    begin
      for (i = 0; i < num_bytes; i = i + 1) begin
        drive_rgmii_byte(8'h00, 1'b0);
      end
    end
  endtask

  task automatic send_rgmii_frame(input byte unsigned frame[$]);
    integer i;
    begin
      for (i = 0; i < 7; i = i + 1) begin
        drive_rgmii_byte(8'h55, 1'b1);
      end
      drive_rgmii_byte(8'hD5, 1'b1);
      for (i = 0; i < frame.size(); i = i + 1) begin
        drive_rgmii_byte(frame[i], 1'b1);
      end
      drive_idle_cycles(12);
    end
  endtask

  initial begin : test_main
    byte unsigned arp_req[$];
    byte unsigned udp_write[$];
    time latency_ns;

    $display("Building ARP warmup and 1400-byte UDP TSI write frame");
    build_arp_request(arp_req);
    build_udp_write_frame(udp_write);
    $display("ARP frame bytes (without preamble): %0d", arp_req.size());
    $display("UDP write frame bytes (without preamble): %0d", udp_write.size());

    repeat (20) @(posedge clock);
    reset = 1'b0;
    repeat (50) @(posedge clock);

    $display("[%0t] Sending ARP request warmup", $time);
    send_rgmii_frame(arp_req);

    wait (tx_frame_count >= 1);
    $display("[%0t] Observed first TX frame start (expected ARP reply)", $time);

    repeat (100) @(posedge clock);

    measure_next_rx = 1'b1;
    measure_next_tx = 1'b1;
    $display("[%0t] Sending measured UDP TSI write frame", $time);
    send_rgmii_frame(udp_write);

    wait (measured_tx_start != 0);
    latency_ns = measured_tx_start - measured_rx_start;
    $display("Measured RX->TX latency: %0t ns", latency_ns);
    $display("Measured RX->TX latency: %.3f us", latency_ns / 1000.0);

    repeat (200) @(posedge clock);
    $finish;
  end

  initial begin : timeout_watchdog
    #5ms;
    $fatal(1, "Timeout waiting for ARP reply or ACK frame");
  end

endmodule
