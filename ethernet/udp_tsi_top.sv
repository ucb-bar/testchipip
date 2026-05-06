///////////////////////////////////////////////////////////////////////////////
// udp_tsi_top.v
//
// Top-level UDP-to-TSI bridge for Nexys Video using verilog-ethernet.
//
// Uses from verilog-ethernet:
//   - eth_mac_1g_rgmii_fifo : RGMII MAC with async FIFOs (clock domain crossing)
//   - udp_complete           : Full UDP/IP/ARP stack with AXI-Stream payload I/O
//
// Custom module:
//   - udp_payload_to_tsi_serial : payload bytes <-> TSI serial words
//
// Clock domains:
//   - clk          : your logic clock (any frequency, e.g. 125 MHz)
//   - gtx_clk      : 125 MHz for gigabit RGMII TX (from clk_wiz)
//   - gtx_clk90    : 125 MHz 90° shifted (for RGMII TX timing)
//   - PHY RX clock : recovered from rgmii_rxc by the MAC
//
// Data flow:
//   RGMII PHY <-> eth_mac_1g_rgmii_fifo <-> udp_complete
//       <-> udp_payload_to_tsi_serial <-> TSIToTileLink
///////////////////////////////////////////////////////////////////////////////

module udp_tsi_top #(
    parameter [47:0] FPGA_MAC     = 48'h00_0A_35_00_01_02,
    parameter [31:0] FPGA_IP      = {8'd192, 8'd168, 8'd1, 8'd10},
    parameter [31:0] FPGA_GATEWAY = {8'd192, 8'd168, 8'd1, 8'd1},
    parameter [31:0] SUBNET_MASK  = {8'd255, 8'd255, 8'd255, 8'd0},
    parameter [15:0] UDP_PORT     = 16'd7000,
    parameter        SERIAL_WIDTH = 32,
    parameter [4:0]  RGMII_RX_IDELAY_TAPS = 5'd0,
    parameter [4:0]  PHY_MDIO_ADDR = 5'd1,
    parameter [15:0] PHY_BMCR_FORCE = 16'h0100,
    parameter [7:0]  PHY_MDIO_PRESCALE = 8'd24,
    parameter [31:0] PHY_MDIO_WAIT_CYCLES = 32'd5000000
)(
    // ---- Clocks and Reset ----
    input  wire        clk,           // Logic clock (any freq)
    input  wire        rst,           // Logic reset (active high)
    input  wire        gtx_clk,       // 125 MHz for GbE TX
    input  wire        gtx_clk90,     // 125 MHz 90° phase shifted
    input  wire        gtx_rst,       // Reset in GTX domain
    input  wire        clk_200,       // 200 MHz reference for IDELAYCTRL

    // ---- RGMII PHY interface (directly to Nexys Video PHY pins) ----
    output wire [3:0]  rgmii_txd,
    output wire        rgmii_tx_ctl,
    output wire        rgmii_txc,
    input  wire [3:0]  rgmii_rxd,
    input  wire        rgmii_rx_ctl,
    input  wire        rgmii_rxc,

    // ---- PHY management ----
    output wire        phy_reset_n,
    output wire        phy_mdc,
    inout  wire        phy_mdio,

    // ---- TSI serial interface (to Chipyard TSIToTileLink) ----
    output wire [SERIAL_WIDTH-1:0] serial_out_bits,
    output wire                    serial_out_valid,
    input  wire                    serial_out_ready,

    input  wire [SERIAL_WIDTH-1:0] serial_in_bits,
    input  wire                    serial_in_valid,
    output wire                    serial_in_ready,

    // ---- Status ----
    output wire        phy_link_up,

    // ---- IDELAY tap control (UART, used with ENABLE_RGMII_RX_IDELAY_VAR) ----
    input  wire        uart_rx,        // tie to 1 if not used
    output wire        uart_tx
);

    // Hold PHY out of reset
    assign phy_reset_n = ~rst;

    // UART command protocol:
    //   Byte0: target address
    //     0x01 -> IDELAY command
    //     0x02 -> MDIO command
    //   IDELAY command payload:
    //     Byte1: tap value [4:0]
    //   MDIO command payload:
    //     Byte1: opcode [1:0] (01=write, 10=read, 11=read)
    //     Byte2: reg addr [4:0]
    //     Byte3: data[15:8]
    //     Byte4: data[7:0]
    localparam [7:0] UART_ADDR_IDELAY = 8'h01;
    localparam [7:0] UART_ADDR_MDIO   = 8'h02;

    localparam [2:0]
        UART_ST_ADDR       = 3'd0,
        UART_ST_IDELAY_VAL = 3'd1,
        UART_ST_MDIO_OP    = 3'd2,
        UART_ST_MDIO_REG   = 3'd3,
        UART_ST_MDIO_DATAH = 3'd4,
        UART_ST_MDIO_DATAL = 3'd5;

    wire [7:0] uart_rx_data;
    wire       uart_rx_valid;

    reg [2:0] uart_state_reg = UART_ST_ADDR;
    reg [4:0] idelay_uart_tap_reg = 5'd0;
    reg       idelay_uart_tap_valid_reg = 1'b0;

    reg [1:0] mdio_uart_opcode_reg = 2'b01;
    reg [4:0] mdio_uart_reg_addr_reg = 5'd0;
    reg [15:0] mdio_uart_data_reg = 16'd0;
    reg        mdio_uart_cmd_valid_reg = 1'b0;
    reg        mdio_uart_pending_reg = 1'b0;
    reg        mdio_uart_pending_pop_reg = 1'b0;
    reg [1:0]  mdio_uart_pending_opcode_reg = 2'b01;
    reg [4:0]  mdio_uart_pending_reg_addr_reg = 5'd0;
    reg [15:0] mdio_uart_pending_data_reg = 16'd0;

    wire       idelay_uart_tap_valid;
    wire [4:0] idelay_uart_tap;
    wire       mdio_uart_cmd_valid;
    wire [1:0] mdio_uart_opcode;
    wire [4:0] mdio_uart_reg_addr;
    wire [15:0] mdio_uart_data;

    assign idelay_uart_tap_valid = idelay_uart_tap_valid_reg;
    assign idelay_uart_tap = idelay_uart_tap_reg;
    assign mdio_uart_cmd_valid = mdio_uart_cmd_valid_reg;
    assign mdio_uart_opcode = mdio_uart_opcode_reg;
    assign mdio_uart_reg_addr = mdio_uart_reg_addr_reg;
    assign mdio_uart_data = mdio_uart_data_reg;

    // 2-FF synchronizer on async uart_rx input
    (* ASYNC_REG = "TRUE" *) reg uart_rx_s1   = 1'b1;
    (* ASYNC_REG = "TRUE" *) reg uart_rx_sync = 1'b1;
    always @(posedge clk) begin
        uart_rx_s1   <= uart_rx;
        uart_rx_sync <= uart_rx_s1;
    end

    uart_rx_simple #(
        .CLK_FREQ (125000000),
        .BAUD_RATE(9600)
    ) u_uart_rx (
        .clk  (clk),
        .rst  (rst),
        .rx   (uart_rx_sync),
        .data (uart_rx_data),
        .valid(uart_rx_valid)
    );

    always @(posedge clk) begin
        idelay_uart_tap_valid_reg <= 1'b0;
        mdio_uart_cmd_valid_reg <= 1'b0;
        if (mdio_uart_pending_pop_reg) begin
            mdio_uart_pending_reg <= 1'b0;
        end

        if (rst) begin
            uart_state_reg <= UART_ST_ADDR;
            mdio_uart_pending_reg <= 1'b0;
        end else if (uart_rx_valid) begin
            case (uart_state_reg)
                UART_ST_ADDR: begin
                    if (uart_rx_data == UART_ADDR_IDELAY) begin
                        uart_state_reg <= UART_ST_IDELAY_VAL;
                    end else if (uart_rx_data == UART_ADDR_MDIO) begin
                        uart_state_reg <= UART_ST_MDIO_OP;
                    end else begin
                        uart_state_reg <= UART_ST_ADDR;
                    end
                end
                UART_ST_IDELAY_VAL: begin
                    idelay_uart_tap_reg <= uart_rx_data[4:0];
                    idelay_uart_tap_valid_reg <= 1'b1;
                    uart_state_reg <= UART_ST_ADDR;
                end
                UART_ST_MDIO_OP: begin
                    mdio_uart_opcode_reg <= uart_rx_data[1:0];
                    uart_state_reg <= UART_ST_MDIO_REG;
                end
                UART_ST_MDIO_REG: begin
                    mdio_uart_reg_addr_reg <= uart_rx_data[4:0];
                    uart_state_reg <= UART_ST_MDIO_DATAH;
                end
                UART_ST_MDIO_DATAH: begin
                    mdio_uart_data_reg[15:8] <= uart_rx_data;
                    uart_state_reg <= UART_ST_MDIO_DATAL;
                end
                UART_ST_MDIO_DATAL: begin
                    mdio_uart_data_reg[7:0] <= uart_rx_data;
                    mdio_uart_cmd_valid_reg <= 1'b1;
                    mdio_uart_pending_reg <= 1'b1;
                    mdio_uart_pending_opcode_reg <= mdio_uart_opcode_reg;
                    mdio_uart_pending_reg_addr_reg <= mdio_uart_reg_addr_reg;
                    mdio_uart_pending_data_reg <= {mdio_uart_data_reg[15:8], uart_rx_data};
                    uart_state_reg <= UART_ST_ADDR;
                end
                default: begin
                    uart_state_reg <= UART_ST_ADDR;
                end
            endcase
        end
    end

    // UART response path (ACK / readback)
    reg [31:0] uart_tx_buf_reg = 32'd0;
    reg [2:0]  uart_tx_len_reg = 3'd0;
    reg [2:0]  uart_tx_idx_reg = 3'd0;
    reg        uart_tx_pending_reg = 1'b0;
    reg [7:0]  uart_tx_data_reg = 8'h00;
    reg        uart_tx_valid_reg = 1'b0;
    wire       uart_tx_ready;
`ifdef ENABLE_PHY_MDIO_CFG
    reg [15:0] mdio_rsp_data_reg = 16'd0;
    reg        mdio_rsp_pending_reg = 1'b0;
`endif
    reg        mdio_rsp_take_pulse_reg = 1'b0;

    uart_tx_simple #(
        .CLK_FREQ (125000000),
        .BAUD_RATE(9600)
    ) u_uart_tx (
        .clk   (clk),
        .rst   (rst),
        .data  (uart_tx_data_reg),
        .valid (uart_tx_valid_reg),
        .ready (uart_tx_ready),
        .tx    (uart_tx)
    );

    always @(posedge clk) begin
        mdio_rsp_take_pulse_reg <= 1'b0;
        if (rst) begin
            uart_tx_pending_reg <= 1'b0;
            uart_tx_valid_reg <= 1'b0;
            uart_tx_len_reg <= 3'd0;
            uart_tx_idx_reg <= 3'd0;
            uart_tx_buf_reg <= 32'd0;
        end else begin
            // enqueue ACK/readback frames when idle
            if (!uart_tx_pending_reg && !uart_tx_valid_reg) begin
                if (idelay_uart_tap_valid) begin
                    uart_tx_pending_reg <= 1'b1;
                    uart_tx_len_reg <= 3'd2;
                    uart_tx_idx_reg <= 3'd0;
                    uart_tx_buf_reg <= {8'h00, 8'h00, 8'h81, {3'b000, idelay_uart_tap}};
                end
`ifdef ENABLE_PHY_MDIO_CFG
                else if (mdio_cmd_accepted_pulse) begin
                    uart_tx_pending_reg <= 1'b1;
                    uart_tx_len_reg <= 3'd3;
                    uart_tx_idx_reg <= 3'd0;
                    uart_tx_buf_reg <= {8'h82, {3'b000, mdio_cmd_accepted_reg_addr}, {6'b000000, mdio_cmd_accepted_opcode}, 8'hA2};
                end else if (mdio_rsp_pending_reg) begin
                    uart_tx_pending_reg <= 1'b1;
                    uart_tx_len_reg <= 3'd3;
                    uart_tx_idx_reg <= 3'd0;
                    uart_tx_buf_reg <= {mdio_rsp_data_reg, 8'hB2};
                    mdio_rsp_take_pulse_reg <= 1'b1;
                end
`endif
            end

            if (uart_tx_valid_reg && uart_tx_ready) begin
                uart_tx_valid_reg <= 1'b0;
            end

            if (uart_tx_pending_reg && !uart_tx_valid_reg && uart_tx_ready) begin
                uart_tx_data_reg <= uart_tx_buf_reg[uart_tx_idx_reg*8 +: 8];
                uart_tx_valid_reg <= 1'b1;
                if (uart_tx_idx_reg + 1'b1 >= uart_tx_len_reg) begin
                    uart_tx_pending_reg <= 1'b0;
                    uart_tx_idx_reg <= 3'd0;
                end else begin
                    uart_tx_idx_reg <= uart_tx_idx_reg + 1'b1;
                end
            end
        end
    end

`ifdef ENABLE_PHY_MDIO_CFG
    // Optional PHY MDIO configuration (force speed/duplex at startup).
    wire mdio_i;
    wire mdio_o;
    wire mdio_t;
    wire mdio_mdc;

    assign phy_mdc = mdio_mdc;
    assign mdio_i = phy_mdio;
    assign phy_mdio = mdio_t ? 1'bz : mdio_o;

    localparam [1:0]
        PHY_CFG_WAIT = 2'd0,
        PHY_CFG_SEND = 2'd1,
        PHY_CFG_DONE = 2'd2;

    reg [1:0] phy_cfg_state_reg = PHY_CFG_WAIT;
    reg [31:0] phy_cfg_wait_ctr_reg = 32'd0;
    reg [4:0]  mdio_cmd_phy_addr_reg = 5'd0;
    reg [4:0]  mdio_cmd_reg_addr_reg = 5'd0;
    reg [15:0] mdio_cmd_data_reg = 16'd0;
    reg [1:0]  mdio_cmd_opcode_reg = 2'b01;
    reg        mdio_cmd_valid_reg = 1'b0;
    reg [1:0]  mdio_cmd_accepted_opcode = 2'b00;
    reg [4:0]  mdio_cmd_accepted_reg_addr = 5'd0;
    reg        mdio_cmd_accepted_pulse = 1'b0;
    wire       mdio_cmd_ready;
    wire [15:0] mdio_data_out;
    wire        mdio_data_out_valid;

    always @(posedge clk) begin
        mdio_uart_pending_pop_reg <= 1'b0;
        mdio_cmd_accepted_pulse <= 1'b0;
        if (rst) begin
            phy_cfg_state_reg <= PHY_CFG_WAIT;
            phy_cfg_wait_ctr_reg <= 32'd0;
            mdio_cmd_phy_addr_reg <= PHY_MDIO_ADDR;
            mdio_cmd_reg_addr_reg <= 5'h00;
            mdio_cmd_data_reg <= 16'd0;
            mdio_cmd_opcode_reg <= 2'b01;
            mdio_cmd_valid_reg <= 1'b0;
            mdio_rsp_data_reg <= 16'd0;
            mdio_rsp_pending_reg <= 1'b0;
        end else begin
            if (mdio_rsp_take_pulse_reg) begin
                mdio_rsp_pending_reg <= 1'b0;
            end
            if (mdio_data_out_valid) begin
                mdio_rsp_data_reg <= mdio_data_out;
                mdio_rsp_pending_reg <= 1'b1;
            end

            if (mdio_cmd_valid_reg && mdio_cmd_ready) begin
                mdio_cmd_valid_reg <= 1'b0;
            end

            if (mdio_uart_pending_reg && !mdio_cmd_valid_reg && mdio_cmd_ready) begin
                mdio_cmd_phy_addr_reg <= PHY_MDIO_ADDR;
                mdio_cmd_reg_addr_reg <= mdio_uart_pending_reg_addr_reg;
                mdio_cmd_data_reg <= mdio_uart_pending_data_reg;
                mdio_cmd_opcode_reg <= mdio_uart_pending_opcode_reg;
                mdio_cmd_valid_reg <= 1'b1;
                mdio_uart_pending_pop_reg <= 1'b1;
                mdio_cmd_accepted_opcode <= mdio_uart_pending_opcode_reg;
                mdio_cmd_accepted_reg_addr <= mdio_uart_pending_reg_addr_reg;
                mdio_cmd_accepted_pulse <= 1'b1;
                // Any explicit UART MDIO command disables the one-shot default write.
                phy_cfg_state_reg <= PHY_CFG_DONE;
            end else begin
                case (phy_cfg_state_reg)
                    PHY_CFG_WAIT: begin
                        if (phy_cfg_wait_ctr_reg < PHY_MDIO_WAIT_CYCLES) begin
                            phy_cfg_wait_ctr_reg <= phy_cfg_wait_ctr_reg + 1'b1;
                        end else if (!mdio_cmd_valid_reg && mdio_cmd_ready) begin
                            mdio_cmd_phy_addr_reg <= PHY_MDIO_ADDR;
                            mdio_cmd_reg_addr_reg <= 5'h00; // BMCR
                            mdio_cmd_data_reg <= PHY_BMCR_FORCE;
                            mdio_cmd_opcode_reg <= 2'b01; // write
                            mdio_cmd_valid_reg <= 1'b1;
                            phy_cfg_state_reg <= PHY_CFG_SEND;
                        end
                    end
                    PHY_CFG_SEND: begin
                        if (!mdio_cmd_valid_reg) begin
                            phy_cfg_state_reg <= PHY_CFG_DONE;
                        end
                    end
                    default: begin
                        phy_cfg_state_reg <= PHY_CFG_DONE;
                    end
                endcase
            end
        end
    end

    mdio_master mdio_master_inst (
        .clk(clk),
        .rst(rst),
        .cmd_phy_addr(mdio_cmd_phy_addr_reg),
        .cmd_reg_addr(mdio_cmd_reg_addr_reg),
        .cmd_data(mdio_cmd_data_reg),
        .cmd_opcode(mdio_cmd_opcode_reg),
        .cmd_valid(mdio_cmd_valid_reg),
        .cmd_ready(mdio_cmd_ready),
        .data_out(mdio_data_out),
        .data_out_valid(mdio_data_out_valid),
        .data_out_ready(1'b1),
        .mdc_o(mdio_mdc),
        .mdio_i(mdio_i),
        .mdio_o(mdio_o),
        .mdio_t(mdio_t),
        .busy(),
        .prescale(PHY_MDIO_PRESCALE)
    );
`else
    assign phy_mdc = 1'b0;
    assign phy_mdio = 1'bz;
`endif

    // =====================================================================
    // Wires between MAC and UDP stack
    // =====================================================================

    // MAC RX -> UDP stack (AXI-Stream, in logic clock domain after FIFO)
    wire [7:0]  mac_rx_axis_tdata;
    wire        mac_rx_axis_tkeep;
    wire        mac_rx_axis_tvalid;
    wire        mac_rx_axis_tlast;
    wire        mac_rx_axis_tuser;
    wire        mac_rx_axis_tready;

    // UDP stack -> MAC TX (AXI-Stream, in logic clock domain before FIFO)
    wire [7:0]  mac_tx_axis_tdata;
    wire        mac_tx_axis_tkeep;
    wire        mac_tx_axis_tvalid;
    wire        mac_tx_axis_tlast;
    wire        mac_tx_axis_tuser;
    wire        mac_tx_axis_tready;

    // MAC status (unused)
    wire        mac_tx_error_underflow;
    wire        mac_tx_fifo_overflow;
    wire        mac_tx_fifo_bad_frame;
    wire        mac_tx_fifo_good_frame;
    wire        mac_rx_error_bad_frame;
    wire        mac_rx_error_bad_fcs;
    wire        mac_rx_fifo_overflow;
    wire        mac_rx_fifo_bad_frame;
    wire        mac_rx_fifo_good_frame;

    // Speed from MAC (for status)
    wire [1:0]  speed;

    // RGMII RX signals after optional IDELAY.
    wire [3:0] rgmii_rxd_mac;
    wire       rgmii_rx_ctl_mac;

`ifdef ENABLE_RGMII_RX_IDELAY

    (* IODELAY_GROUP = "RGMII_RX_IDELAY_GRP" *)
    IDELAYCTRL u_rgmii_idelayctrl (
        .REFCLK(clk_200),
        .RST(rst),
        .RDY()
    );

  `ifdef ENABLE_RGMII_RX_IDELAY_VAR

    // ---- Variable IDELAYE2: tap count set via UART addressed command ----

    reg [4:0] idelay_tap_reg;
    reg       idelay_ld;

    always @(posedge clk) begin
        idelay_ld <= 1'b0;
        if (rst) begin
            idelay_tap_reg <= RGMII_RX_IDELAY_TAPS;
            idelay_ld      <= 1'b1;
        end else if (idelay_uart_tap_valid) begin
            idelay_tap_reg <= idelay_uart_tap;
            idelay_ld      <= 1'b1;
        end
    end

    genvar g_rgmii_rx_idelay;
    generate
        for (g_rgmii_rx_idelay = 0; g_rgmii_rx_idelay < 4; g_rgmii_rx_idelay = g_rgmii_rx_idelay + 1) begin : rgmii_rxd_idelay_gen
            (* IODELAY_GROUP = "RGMII_RX_IDELAY_GRP" *)
            IDELAYE2 #(
                .CINVCTRL_SEL("FALSE"),
                .DELAY_SRC("IDATAIN"),
                .HIGH_PERFORMANCE_MODE("TRUE"),
                .IDELAY_TYPE("VAR_LOAD"),
                .IDELAY_VALUE(RGMII_RX_IDELAY_TAPS),
                .PIPE_SEL("FALSE"),
                .REFCLK_FREQUENCY(200.0),
                .SIGNAL_PATTERN("DATA")
            ) rgmii_rxd_idelay_inst (
                .IDATAIN(rgmii_rxd[g_rgmii_rx_idelay]),
                .DATAOUT(rgmii_rxd_mac[g_rgmii_rx_idelay]),
                .DATAIN(1'b0),
                .C(clk),
                .CE(1'b0),
                .INC(1'b0),
                .CINVCTRL(1'b0),
                .CNTVALUEIN(idelay_tap_reg),
                .CNTVALUEOUT(),
                .LD(idelay_ld),
                .LDPIPEEN(1'b0),
                .REGRST(1'b0)
            );
        end
    endgenerate

    (* IODELAY_GROUP = "RGMII_RX_IDELAY_GRP" *)
    IDELAYE2 #(
        .CINVCTRL_SEL("FALSE"),
        .DELAY_SRC("IDATAIN"),
        .HIGH_PERFORMANCE_MODE("TRUE"),
        .IDELAY_TYPE("VAR_LOAD"),
        .IDELAY_VALUE(RGMII_RX_IDELAY_TAPS),
        .PIPE_SEL("FALSE"),
        .REFCLK_FREQUENCY(200.0),
        .SIGNAL_PATTERN("DATA")
    ) rgmii_rx_ctl_idelay_inst (
        .IDATAIN(rgmii_rx_ctl),
        .DATAOUT(rgmii_rx_ctl_mac),
        .DATAIN(1'b0),
        .C(clk),
        .CE(1'b0),
        .INC(1'b0),
        .CINVCTRL(1'b0),
        .CNTVALUEIN(idelay_tap_reg),
        .CNTVALUEOUT(),
        .LD(idelay_ld),
        .LDPIPEEN(1'b0),
        .REGRST(1'b0)
    );

  `else

    // ---- Fixed IDELAYE2 ----
    genvar g_rgmii_rx_idelay;
    generate
        for (g_rgmii_rx_idelay = 0; g_rgmii_rx_idelay < 4; g_rgmii_rx_idelay = g_rgmii_rx_idelay + 1) begin : rgmii_rxd_idelay_gen
            (* IODELAY_GROUP = "RGMII_RX_IDELAY_GRP" *)
            IDELAYE2 #(
                .CINVCTRL_SEL("FALSE"),
                .DELAY_SRC("IDATAIN"),
                .HIGH_PERFORMANCE_MODE("TRUE"),
                .IDELAY_TYPE("FIXED"),
                .IDELAY_VALUE(RGMII_RX_IDELAY_TAPS),
                .PIPE_SEL("FALSE"),
                .REFCLK_FREQUENCY(200.0),
                .SIGNAL_PATTERN("DATA")
            ) rgmii_rxd_idelay_inst (
                .IDATAIN(rgmii_rxd[g_rgmii_rx_idelay]),
                .DATAOUT(rgmii_rxd_mac[g_rgmii_rx_idelay]),
                .DATAIN(1'b0),
                .C(1'b0),
                .CE(1'b0),
                .INC(1'b0),
                .CINVCTRL(1'b0),
                .CNTVALUEIN(5'd0),
                .CNTVALUEOUT(),
                .LD(1'b0),
                .LDPIPEEN(1'b0),
                .REGRST(1'b0)
            );
        end
    endgenerate

    (* IODELAY_GROUP = "RGMII_RX_IDELAY_GRP" *)
    IDELAYE2 #(
        .CINVCTRL_SEL("FALSE"),
        .DELAY_SRC("IDATAIN"),
        .HIGH_PERFORMANCE_MODE("TRUE"),
        .IDELAY_TYPE("FIXED"),
        .IDELAY_VALUE(RGMII_RX_IDELAY_TAPS),
        .PIPE_SEL("FALSE"),
        .REFCLK_FREQUENCY(200.0),
        .SIGNAL_PATTERN("DATA")
    ) rgmii_rx_ctl_idelay_inst (
        .IDATAIN(rgmii_rx_ctl),
        .DATAOUT(rgmii_rx_ctl_mac),
        .DATAIN(1'b0),
        .C(1'b0),
        .CE(1'b0),
        .INC(1'b0),
        .CINVCTRL(1'b0),
        .CNTVALUEIN(5'd0),
        .CNTVALUEOUT(),
        .LD(1'b0),
        .LDPIPEEN(1'b0),
        .REGRST(1'b0)
    );

  `endif

`else
    assign rgmii_rxd_mac     = rgmii_rxd;
    assign rgmii_rx_ctl_mac  = rgmii_rx_ctl;
`endif

    // =====================================================================
    // Ethernet MAC with RGMII interface and async FIFOs
    //
    // This handles:
    //   - RGMII DDR (IDDR/ODDR) for Xilinx 7-series
    //   - Preamble/SFD generation and stripping
    //   - FCS (CRC-32) computation and checking
    //   - Frame padding to minimum 64 bytes
    //   - Clock domain crossing via async FIFOs
    //   - Automatic speed detection (10/100/1000)
    // =====================================================================

    eth_mac_1g_rgmii_fifo #(
        .TARGET            ("XILINX"),
        .IODDR_STYLE       ("IODDR"),
        .CLOCK_INPUT_STYLE ("BUFR"),
        .USE_CLK90         ("TRUE"),
        .ENABLE_PADDING    (1),
        .MIN_FRAME_LENGTH  (64),
        .TX_FIFO_DEPTH     (4096),
        .TX_FRAME_FIFO     (1),
        .RX_FIFO_DEPTH     (4096),
        .RX_FRAME_FIFO     (1)
    ) u_mac (
        // RGMII PHY
        .gtx_clk           (gtx_clk),
        .gtx_clk90         (gtx_clk90),
        .gtx_rst           (gtx_rst),
        .rgmii_txd          (rgmii_txd),
        .rgmii_tx_ctl       (rgmii_tx_ctl),
        .rgmii_tx_clk       (rgmii_txc),
        .rgmii_rxd          (rgmii_rxd_mac),
        .rgmii_rx_ctl       (rgmii_rx_ctl_mac),
        .rgmii_rx_clk       (rgmii_rxc),

        // Logic-side clock (your fabric clock)
        .logic_clk          (clk),
        .logic_rst          (rst),

        // AXI-Stream TX (logic clock domain -> MAC -> PHY)
        .tx_axis_tdata      (mac_tx_axis_tdata),
        .tx_axis_tkeep      (mac_tx_axis_tkeep),
        .tx_axis_tvalid     (mac_tx_axis_tvalid),
        .tx_axis_tready     (mac_tx_axis_tready),
        .tx_axis_tlast      (mac_tx_axis_tlast),
        .tx_axis_tuser      (mac_tx_axis_tuser),

        // AXI-Stream RX (PHY -> MAC -> logic clock domain)
        .rx_axis_tdata      (mac_rx_axis_tdata),
        .rx_axis_tkeep      (mac_rx_axis_tkeep),
        .rx_axis_tvalid     (mac_rx_axis_tvalid),
        .rx_axis_tready     (mac_rx_axis_tready),
        .rx_axis_tlast      (mac_rx_axis_tlast),
        .rx_axis_tuser      (mac_rx_axis_tuser),

        // Status
        .tx_error_underflow (mac_tx_error_underflow),
        .tx_fifo_overflow   (mac_tx_fifo_overflow),
        .tx_fifo_bad_frame  (mac_tx_fifo_bad_frame),
        .tx_fifo_good_frame (mac_tx_fifo_good_frame),
        .rx_error_bad_frame (mac_rx_error_bad_frame),
        .rx_error_bad_fcs   (mac_rx_error_bad_fcs),
        .rx_fifo_overflow   (mac_rx_fifo_overflow),
        .rx_fifo_bad_frame  (mac_rx_fifo_bad_frame),
        .rx_fifo_good_frame (mac_rx_fifo_good_frame),
        .speed              (speed),

        // Configuration
        .cfg_ifg            (8'd12),
        .cfg_tx_enable      (1'b1),
        .cfg_rx_enable      (1'b1)
    );

    assign phy_link_up = (speed != 2'b00);

    // =====================================================================
    // eth_axis_rx: decode raw MAC RX byte stream into Ethernet header + payload
    // =====================================================================

    wire        rx_eth_hdr_valid;
    wire        rx_eth_hdr_ready;
    wire [47:0] rx_eth_dest_mac;
    wire [47:0] rx_eth_src_mac;
    wire [15:0] rx_eth_type;
    wire [7:0]  rx_eth_payload_tdata;
    wire        rx_eth_payload_tvalid;
    wire        rx_eth_payload_tready;
    wire        rx_eth_payload_tlast;
    wire        rx_eth_payload_tuser;

    eth_axis_rx eth_axis_rx_inst (
        .clk                        (clk),
        .rst                        (rst),
        .s_axis_tdata               (mac_rx_axis_tdata),
        .s_axis_tkeep               (mac_rx_axis_tkeep),
        .s_axis_tvalid              (mac_rx_axis_tvalid),
        .s_axis_tready              (mac_rx_axis_tready),
        .s_axis_tlast               (mac_rx_axis_tlast),
        .s_axis_tuser               (mac_rx_axis_tuser),
        .m_eth_hdr_valid            (rx_eth_hdr_valid),
        .m_eth_hdr_ready            (rx_eth_hdr_ready),
        .m_eth_dest_mac             (rx_eth_dest_mac),
        .m_eth_src_mac              (rx_eth_src_mac),
        .m_eth_type                 (rx_eth_type),
        .m_eth_payload_axis_tdata   (rx_eth_payload_tdata),
        .m_eth_payload_axis_tkeep   (),
        .m_eth_payload_axis_tvalid  (rx_eth_payload_tvalid),
        .m_eth_payload_axis_tready  (rx_eth_payload_tready),
        .m_eth_payload_axis_tlast   (rx_eth_payload_tlast),
        .m_eth_payload_axis_tuser   (rx_eth_payload_tuser),
        .busy                       (),
        .error_header_early_termination ()
    );

    // =====================================================================
    // eth_axis_tx: encode Ethernet header + payload into MAC TX byte stream
    // =====================================================================

    wire        tx_eth_hdr_valid;
    wire        tx_eth_hdr_ready;
    wire [47:0] tx_eth_dest_mac;
    wire [47:0] tx_eth_src_mac;
    wire [15:0] tx_eth_type;
    wire [7:0]  tx_eth_payload_tdata;
    wire        tx_eth_payload_tvalid;
    wire        tx_eth_payload_tready;
    wire        tx_eth_payload_tlast;
    wire        tx_eth_payload_tuser;

    eth_axis_tx eth_axis_tx_inst (
        .clk                        (clk),
        .rst                        (rst),
        .s_eth_hdr_valid            (tx_eth_hdr_valid),
        .s_eth_hdr_ready            (tx_eth_hdr_ready),
        .s_eth_dest_mac             (tx_eth_dest_mac),
        .s_eth_src_mac              (tx_eth_src_mac),
        .s_eth_type                 (tx_eth_type),
        .s_eth_payload_axis_tdata   (tx_eth_payload_tdata),
        .s_eth_payload_axis_tkeep   (1'b1),
        .s_eth_payload_axis_tvalid  (tx_eth_payload_tvalid),
        .s_eth_payload_axis_tready  (tx_eth_payload_tready),
        .s_eth_payload_axis_tlast   (tx_eth_payload_tlast),
        .s_eth_payload_axis_tuser   (tx_eth_payload_tuser),
        .m_axis_tdata               (mac_tx_axis_tdata),
        .m_axis_tkeep               (),
        .m_axis_tvalid              (mac_tx_axis_tvalid),
        .m_axis_tready              (mac_tx_axis_tready),
        .m_axis_tlast               (mac_tx_axis_tlast),
        .m_axis_tuser               (mac_tx_axis_tuser),
        .busy                       ()
    );

    // =====================================================================
    // UDP/IP/ARP stack (udp_complete from verilog-ethernet)
    //
    // This handles:
    //   - Ethernet frame parsing/building
    //   - ARP request/reply with cache
    //   - IPv4 header parsing/building with checksum
    //   - UDP header parsing/building
    //   - Exposes clean AXI-Stream interfaces for UDP payload
    // =====================================================================

    // UDP RX payload (from network -> our logic)
    wire [7:0]  udp_rx_payload_tdata;
    wire        udp_rx_payload_tvalid;
    wire        udp_rx_payload_tlast;
    wire        udp_rx_payload_tready;

    // UDP RX header fields
    wire        udp_rx_hdr_valid;
    wire        udp_rx_hdr_ready;
    wire [47:0] udp_rx_eth_src_mac;
    wire [31:0] udp_rx_ip_src_ip;
    wire [31:0] udp_rx_ip_dst_ip;
    wire [15:0] udp_rx_src_port;
    wire [15:0] udp_rx_dst_port;
    wire [15:0] udp_rx_length;

    // UDP TX payload (our logic -> network)
    wire [7:0]  udp_tx_payload_tdata;
    wire        udp_tx_payload_tvalid;
    wire        udp_tx_payload_tlast;
    wire        udp_tx_payload_tready;

    // UDP TX header fields
    wire        udp_tx_hdr_valid;
    wire        udp_tx_hdr_ready;
    wire        ip_tx_error_payload_early_termination;
    wire        ip_tx_error_arp_failed;

    // Latched source info for replies
    reg [47:0]  reply_eth_dst_mac;
    reg [31:0]  reply_ip_dst_ip;
    reg [15:0]  reply_dst_port;
    reg [15:0]  reply_length;

    // Latch sender info on each received packet
    always @(posedge clk) begin
        if (rst) begin
            reply_eth_dst_mac <= 48'd0;
            reply_ip_dst_ip   <= 32'd0;
            reply_dst_port    <= 16'd0;
        end else if (udp_rx_hdr_valid && udp_rx_hdr_ready) begin
            reply_eth_dst_mac <= udp_rx_eth_src_mac;
            reply_ip_dst_ip   <= udp_rx_ip_src_ip;
            reply_dst_port    <= udp_rx_src_port;
        end
    end

    udp_complete #(
        .ARP_CACHE_ADDR_WIDTH (2),
        .ARP_REQUEST_RETRY_COUNT (4),
        .ARP_REQUEST_RETRY_INTERVAL (125000000),  // 1 sec at 125 MHz
        .ARP_REQUEST_TIMEOUT (125000000 * 5),
        .UDP_CHECKSUM_GEN_ENABLE (0),
        .UDP_CHECKSUM_PAYLOAD_FIFO_DEPTH (2048),
        .UDP_CHECKSUM_HEADER_FIFO_DEPTH (8)
    ) u_udp_stack (
        .clk                (clk),
        .rst                (rst),

        // ---- Ethernet frame interface (via eth_axis_rx / eth_axis_tx) ----
        // RX from eth_axis_rx
        .s_eth_hdr_valid           (rx_eth_hdr_valid),
        .s_eth_hdr_ready           (rx_eth_hdr_ready),
        .s_eth_dest_mac            (rx_eth_dest_mac),
        .s_eth_src_mac             (rx_eth_src_mac),
        .s_eth_type                (rx_eth_type),
        .s_eth_payload_axis_tdata  (rx_eth_payload_tdata),
        .s_eth_payload_axis_tvalid (rx_eth_payload_tvalid),
        .s_eth_payload_axis_tready (rx_eth_payload_tready),
        .s_eth_payload_axis_tlast  (rx_eth_payload_tlast),
        .s_eth_payload_axis_tuser  (rx_eth_payload_tuser),

        // TX to eth_axis_tx
        .m_eth_hdr_valid           (tx_eth_hdr_valid),
        .m_eth_hdr_ready           (tx_eth_hdr_ready),
        .m_eth_dest_mac            (tx_eth_dest_mac),
        .m_eth_src_mac             (tx_eth_src_mac),
        .m_eth_type                (tx_eth_type),
        .m_eth_payload_axis_tdata  (tx_eth_payload_tdata),
        .m_eth_payload_axis_tvalid (tx_eth_payload_tvalid),
        .m_eth_payload_axis_tready (tx_eth_payload_tready),
        .m_eth_payload_axis_tlast  (tx_eth_payload_tlast),
        .m_eth_payload_axis_tuser  (tx_eth_payload_tuser),

        // ---- IP interface (not used — tied off) ----
        .s_ip_hdr_valid            (1'b0),
        .s_ip_hdr_ready            (),
        .s_ip_dscp                 (6'd0),
        .s_ip_ecn                  (2'd0),
        .s_ip_length               (16'd0),
        .s_ip_ttl                  (8'd0),
        .s_ip_protocol             (8'd0),
        .s_ip_source_ip            (32'd0),
        .s_ip_dest_ip              (32'd0),
        .s_ip_payload_axis_tdata   (8'd0),
        .s_ip_payload_axis_tvalid  (1'b0),
        .s_ip_payload_axis_tready  (),
        .s_ip_payload_axis_tlast   (1'b0),
        .s_ip_payload_axis_tuser   (1'b0),
        .m_ip_hdr_valid            (),
        .m_ip_hdr_ready            (1'b1),
        .m_ip_eth_dest_mac         (),
        .m_ip_eth_src_mac          (),
        .m_ip_eth_type             (),
        .m_ip_version              (),
        .m_ip_ihl                  (),
        .m_ip_dscp                 (),
        .m_ip_ecn                  (),
        .m_ip_length               (),
        .m_ip_identification       (),
        .m_ip_flags                (),
        .m_ip_fragment_offset      (),
        .m_ip_ttl                  (),
        .m_ip_protocol             (),
        .m_ip_header_checksum      (),
        .m_ip_source_ip            (),
        .m_ip_dest_ip              (),
        .m_ip_payload_axis_tdata   (),
        .m_ip_payload_axis_tvalid  (),
        .m_ip_payload_axis_tready  (1'b1),
        .m_ip_payload_axis_tlast   (),
        .m_ip_payload_axis_tuser   (),

        // ---- IP configuration ----
        .local_mac          (FPGA_MAC),
        .local_ip           (FPGA_IP),
        .gateway_ip         (FPGA_GATEWAY),
        .subnet_mask        (SUBNET_MASK),
        .clear_arp_cache    (1'b0),

        // ---- UDP TX (our logic -> network) ----
        .s_udp_hdr_valid    (udp_tx_hdr_valid),
        .s_udp_hdr_ready    (udp_tx_hdr_ready),
        .s_udp_ip_dscp      (6'd0),
        .s_udp_ip_ecn       (2'd0),
        .s_udp_ip_ttl       (8'd64),
        .s_udp_ip_source_ip (FPGA_IP),
        .s_udp_ip_dest_ip   (reply_ip_dst_ip),
        .s_udp_source_port  (UDP_PORT),
        .s_udp_dest_port    (reply_dst_port),
        .s_udp_length       (reply_length),
        .s_udp_checksum     (16'd0),
        .s_udp_payload_axis_tdata  (udp_tx_payload_tdata),
        .s_udp_payload_axis_tvalid (udp_tx_payload_tvalid),
        .s_udp_payload_axis_tready (udp_tx_payload_tready),
        .s_udp_payload_axis_tlast  (udp_tx_payload_tlast),
        .s_udp_payload_axis_tuser  (1'b0),

        // ---- UDP RX (network -> our logic) ----
        .m_udp_hdr_valid    (udp_rx_hdr_valid),
        .m_udp_hdr_ready    (udp_rx_hdr_ready),
        .m_udp_ip_dscp      (),
        .m_udp_ip_ecn       (),
        .m_udp_ip_ttl       (),
        .m_udp_eth_src_mac  (udp_rx_eth_src_mac),
        .m_udp_eth_dest_mac (),
        .m_udp_eth_type     (),
        .m_udp_ip_source_ip (udp_rx_ip_src_ip),
        .m_udp_ip_dest_ip   (udp_rx_ip_dst_ip),
        .m_udp_source_port  (udp_rx_src_port),
        .m_udp_dest_port    (udp_rx_dst_port),
        .m_udp_length       (udp_rx_length),
        .m_udp_checksum     (),
        .m_udp_payload_axis_tdata  (udp_rx_payload_tdata),
        .m_udp_payload_axis_tvalid (udp_rx_payload_tvalid),
        .m_udp_payload_axis_tready (udp_rx_payload_tready),
        .m_udp_payload_axis_tlast  (udp_rx_payload_tlast),
        .m_udp_payload_axis_tuser  (),

        // ---- Status (ignored) ----
        .ip_rx_busy                             (),
        .ip_tx_busy                             (),
        .udp_rx_busy                            (),
        .udp_tx_busy                            (),
        .ip_rx_error_header_early_termination   (),
        .ip_rx_error_payload_early_termination  (),
        .ip_rx_error_invalid_header             (),
        .ip_rx_error_invalid_checksum           (),
        .ip_tx_error_payload_early_termination  (ip_tx_error_payload_early_termination),
        .ip_tx_error_arp_failed                 (ip_tx_error_arp_failed),
        .udp_rx_error_header_early_termination  (),
        .udp_rx_error_payload_early_termination (),
        .udp_tx_error_payload_early_termination ()
    );

    // Accept header when it arrives; filter by port
    assign udp_rx_hdr_ready = 1'b1;

    // =====================================================================
    // Port filtering: only process packets to our UDP_PORT
    // =====================================================================

    wire port_match = (udp_rx_dst_port == UDP_PORT);

    // Gate payload with port match (drop non-matching packets)
    wire [7:0]  filtered_payload_tdata;
    wire        filtered_payload_tvalid;
    wire        filtered_payload_tlast;
    wire        filtered_payload_tready;

    reg port_matched_r;

    always @(posedge clk) begin
        if (rst)
            port_matched_r <= 1'b0;
        else if (udp_rx_hdr_valid && udp_rx_hdr_ready)
            port_matched_r <= port_match;
        else if (udp_rx_payload_tvalid && udp_rx_payload_tlast && udp_rx_payload_tready)
            port_matched_r <= 1'b0;
    end

    assign filtered_payload_tdata  = udp_rx_payload_tdata;
    assign filtered_payload_tvalid = udp_rx_payload_tvalid && port_matched_r;
    assign filtered_payload_tlast  = udp_rx_payload_tlast;
    assign udp_rx_payload_tready   = port_matched_r ? filtered_payload_tready : 1'b1;

    // =====================================================================
    // UDP Payload <-> TSI Serial conversion
    //
    // This is the only custom logic in the design.
    // RX: collects payload bytes into SERIAL_WIDTH-bit words -> serial_out
    // TX: breaks serial words from TSI into bytes -> UDP TX payload
    // =====================================================================

    udp_payload_to_tsi_serial #(
        .SERIAL_WIDTH   (SERIAL_WIDTH),
        .ACK_PAYLOAD    (32'hAC01_0001)  // placeholder ACK encoding
    ) u_tsi_serial (
        .clk                (clk),
        .rst                (rst),

        // UDP RX payload -> TSI serial out
        .rx_payload_tdata   (filtered_payload_tdata),
        .rx_payload_tvalid  (filtered_payload_tvalid),
        .rx_payload_tlast   (filtered_payload_tlast),
        .rx_payload_tready  (filtered_payload_tready),

        // TSI serial interface
        .serial_out_bits    (serial_out_bits),
        .serial_out_valid   (serial_out_valid),
        .serial_out_ready   (serial_out_ready),
        .serial_in_bits     (serial_in_bits),
        .serial_in_valid    (serial_in_valid),
        .serial_in_ready    (serial_in_ready),

        // UDP TX payload (responses back to host)
        .tx_payload_tdata   (udp_tx_payload_tdata),
        .tx_payload_tvalid  (udp_tx_payload_tvalid),
        .tx_payload_tlast   (udp_tx_payload_tlast),
        .tx_payload_tready  (udp_tx_payload_tready),

        // UDP TX header control
        .tx_hdr_valid       (udp_tx_hdr_valid),
        .tx_hdr_ready       (udp_tx_hdr_ready),
        .tx_length          (reply_length)
    );

    // Always-on UART debug ILA (not guarded by ENABLE_DEBUG_ILA).
    // ila_5 uart_debug_ila (
    //     .clk    (clk),
    //     .probe0 (uart_rx),
    //     .probe1 (uart_rx_valid),
    //     .probe2 (uart_rx_data),
    //     .probe3 (uart_tx),
    //     .probe4 (uart_tx_valid_reg),
    //     .probe5 (uart_tx_ready),
    //     .probe6 (uart_tx_data_reg),
    //     .probe7 (uart_state_reg)
    // );

`ifdef ENABLE_DEBUG_ILA
    ila_2 udp_stack_filter_ila (
        .clk    (clk),
        .probe0 (udp_rx_hdr_valid),
        .probe1 (udp_rx_ip_dst_ip),
        .probe2 (udp_rx_dst_port)
    );

    ila_4 udp_stack_tx_debug_ila (
        .clk    (clk),
        .probe0 (udp_tx_hdr_valid),
        .probe1 (udp_tx_hdr_ready),
        .probe2 (udp_tx_payload_tvalid),
        .probe3 (udp_tx_payload_tlast),
        .probe4 (tx_eth_hdr_valid),
        .probe5 (mac_tx_axis_tvalid),
        .probe6 (ip_tx_error_payload_early_termination),
        .probe7 (udp_tx_payload_tready),
        .probe8 (ip_tx_error_arp_failed),
        .probe9 (reply_ip_dst_ip),
        .probe10(reply_dst_port),
        .probe11(reply_eth_dst_mac),
        .probe12(mac_tx_axis_tready),
        .probe13(mac_tx_fifo_overflow),
        .probe14(mac_tx_fifo_bad_frame),
        .probe15(mac_tx_error_underflow),
        .probe16(tx_eth_hdr_ready)
    );
`endif

endmodule
