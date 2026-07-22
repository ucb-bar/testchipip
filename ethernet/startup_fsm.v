// Hardware re-implementation of set_phy_rx_delay.sh + startup.sh (RATE=100).
//
// On reset, waits RESET_WAIT_CYCLES cycles, then drives, in the SAME order and
// with the SAME MDIO transactions and inter-step delays as the two shell
// scripts, the RTL8211E bring-up that used to be done by hand over the UART
// MDIO bridge. The transcript reproduced (default `./startup.sh`, RATE=100):
//
//   -- set_phy_rx_delay.sh: force RX-only 2ns RGMII delay --
//     W 0x1f <= 0x0007      select ExtPage access mode
//     W 0x1e <= 0x00a4      select ExtPage 0xA4
//     R 0x1c   -> OLD       read current RGMII delay config
//     W 0x1c <= OLD|0x3000  bit13 force delay ctrl + bit12 RX delay (RX-only 2ns)
//     sleep 2
//     W 0x1f <= 0x0007      re-arm ExtPage access before readback
//     W 0x1e <= 0x00a4      re-arm ExtPage 0xA4
//     R 0x1c   -> readback  (script verifies &0x3000==0x3000; HW just reads bus)
//     W 0x1f <= 0x0000      return to page 0
//     R 0x11   (mdio-link)  PHYSR read (link status)
//
//   -- startup.sh: program rate/duplex (RATE=100 -> no reg 0x09 gig-ctrl write) --
//     W 0x00 <= 0x8000      BMCR soft reset (reset bit only)
//     sleep 1
//     W 0x00 <= 0x2100      BMCR 100 Mbps, full duplex, autoneg off (PHY_BMCR_FORCE)
//     R 0x00                immediate BMCR readback
//     sleep 1
//     R 0x00                settled BMCR readback
//     R 0x11                settled PHYSR readback
//     sleep 2               margin before UDP traffic
//     ping                  -> hello_start / wait hello_done (replaces host `ping`)
//     select-chip 0         -> pulse select_use_switch (default chip-select mux)
//
// The mdio_cmd_* outputs are muxed into the same mdio_master command port used
// by the UART MDIO bridge in udp_tsi_top, with the UART path taking priority
// (this FSM only advances when `cmd_ready` is asserted by the arbiter, and
// waits for the transaction to retire before issuing the next command).
//
// NOTE: for RATE=1000 the scripts also advertise reg 0x09 <= 0x0200 and use
// BMCR 0x1340; that path is intentionally not reproduced here (default is 100).
module startup_fsm #(
    parameter [4:0]  PHY_MDIO_ADDR     = 5'd1,
    parameter [31:0] RESET_WAIT_CYCLES = 32'd5000000,
    // Logic-clock frequency in Hz; used to turn the scripts' `sleep N` into
    // cycle counts (sleep 1 -> CLK_FREQ_HZ, sleep 2 -> 2*CLK_FREQ_HZ).
    parameter [31:0] CLK_FREQ_HZ       = 32'd125000000,
    // Final BMCR mode value written after the soft reset. Default 0x2100 =
    // 100 Mbps, full duplex, autoneg off (startup.sh RATE=100).
    parameter [15:0] PHY_BMCR_FORCE    = 16'h2100,
    // OR mask applied to reg 0x1C: bit13 force delay control + bit12 RX delay.
    parameter [15:0] RGMII_DELAY_MASK  = 16'h3000
) (
    input  wire        clk,
    input  wire        rst,

    // ---- MDIO command port (arbitrated with UART path) ----
    output reg  [4:0]  mdio_cmd_phy_addr,
    output reg  [4:0]  mdio_cmd_reg_addr,
    output reg  [15:0] mdio_cmd_data,
    output reg  [1:0]  mdio_cmd_opcode,
    output reg         mdio_cmd_valid,
    input  wire        cmd_ready,          // accepted-by-arbiter / master-idle gate

    input  wire [15:0] mdio_data_out,
    input  wire        mdio_data_out_valid,

    // ---- "Hello!"/ping message over debug UART ----
    output reg         hello_start,
    input  wire        hello_done,

    // ---- Startup default-to-switch one-shot (== host `select-chip 0`) ----
    // Pulsed once at end of bring-up: forces the chip-select recency mux to
    // default to the BOARD SWITCH (io_select), NOT the host-written software
    // select register value. This seeds the boot default; a later host write
    // or switch change can still take over via the recency arbiter.
    output reg         select_use_switch,

    output reg         done
);

    localparam [1:0]
        OP_WRITE = 2'b01,
        OP_READ  = 2'b10;

    // BMCR soft-reset value (reg 0x00 bit15). Script step: W 0x00 <= 0x8000.
    localparam [15:0] BMCR_SOFT_RESET = 16'h8000;

    // Sleep durations in clock cycles (from the scripts' `sleep 1` / `sleep 2`).
    localparam [31:0] SLEEP_1S = CLK_FREQ_HZ;
    localparam [31:0] SLEEP_2S = CLK_FREQ_HZ + CLK_FREQ_HZ;

    // -----------------------------------------------------------------
    // State encoding. Each MDIO transaction is a matched ISSUE/WAIT pair:
    //   *_ISSUE asserts mdio_cmd_valid and, when cmd_ready accepts it,
    //           drops valid and moves to *_WAIT;
    //   *_WAIT  holds until the transaction retires (writes: cmd_ready
    //           re-asserts; reads: mdio_data_out_valid) then advances.
    // States are grouped 1:1 with the shell-script steps above.
    // -----------------------------------------------------------------
    localparam [5:0]
        S_RESET_WAIT   = 6'd0,   // wait RESET_WAIT_CYCLES after reset

        // ---- set_phy_rx_delay.sh ----
        S_EXTA_ISSUE   = 6'd1,   // W 0x1f <= 0x0007  (ExtPage access mode)
        S_EXTA_WAIT    = 6'd2,
        S_A4A_ISSUE    = 6'd3,   // W 0x1e <= 0x00a4  (select ExtPage 0xA4)
        S_A4A_WAIT     = 6'd4,
        S_R1C_ISSUE    = 6'd5,   // R 0x1c            (read current delay cfg -> OLD)
        S_R1C_WAIT     = 6'd6,
        S_W1C_ISSUE    = 6'd7,   // W 0x1c <= OLD | RGMII_DELAY_MASK
        S_W1C_WAIT     = 6'd8,
        S_SLEEP_1C     = 6'd9,   // sleep 2 (let delay write settle)
        S_EXTB_ISSUE   = 6'd10,  // W 0x1f <= 0x0007  (re-arm ExtPage access)
        S_EXTB_WAIT    = 6'd11,
        S_A4B_ISSUE    = 6'd12,  // W 0x1e <= 0x00a4  (re-arm ExtPage 0xA4)
        S_A4B_WAIT     = 6'd13,
        S_R1CB_ISSUE   = 6'd14,  // R 0x1c            (readback; SW verifies &0x3000)
        S_R1CB_WAIT    = 6'd15,
        S_PG0_ISSUE    = 6'd16,  // W 0x1f <= 0x0000  (return to page 0)
        S_PG0_WAIT     = 6'd17,
        S_PHYSRA_ISSUE = 6'd18,  // R 0x11  (mdio-link: PHYSR read)
        S_PHYSRA_WAIT  = 6'd19,

        // ---- startup.sh (RATE=100) ----
        S_RST_ISSUE    = 6'd20,  // W 0x00 <= 0x8000  (BMCR soft reset)
        S_RST_WAIT     = 6'd21,
        S_SLEEP_RST    = 6'd22,  // sleep 1 (allow PHY reset to complete)
        S_BMCR_ISSUE   = 6'd23,  // W 0x00 <= PHY_BMCR_FORCE  (final rate/duplex)
        S_BMCR_WAIT    = 6'd24,
        S_RBMCR1_ISSUE = 6'd25,  // R 0x00  (immediate BMCR readback)
        S_RBMCR1_WAIT  = 6'd26,
        S_SLEEP_SETTLE = 6'd27,  // sleep 1 (allow PHY/link state to settle)
        S_RBMCR2_ISSUE = 6'd28,  // R 0x00  (settled BMCR readback)
        S_RBMCR2_WAIT  = 6'd29,
        S_PHYSRB_ISSUE = 6'd30,  // R 0x11  (settled PHYSR readback)
        S_PHYSRB_WAIT  = 6'd31,
        S_SLEEP_MARGIN = 6'd32,  // sleep 2 (margin before UDP traffic)

        S_HELLO        = 6'd33,  // ping: pulse hello_start
        S_SELECT_SW    = 6'd34,  // select-chip 0: pulse select_use_switch on hello_done
        S_DONE         = 6'd35;

    reg [5:0]  state;
    reg [31:0] wait_ctr;
    reg [15:0] reg1c_value;   // OLD value of reg 0x1C (used to build the delay write)
    reg [15:0] last_read;     // most recent read data (readbacks; for debug/visibility)

    always @(posedge clk) begin
        if (rst) begin
            state             <= S_RESET_WAIT;
            wait_ctr          <= 32'd0;
            reg1c_value       <= 16'd0;
            last_read         <= 16'd0;
            mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
            mdio_cmd_reg_addr <= 5'd0;
            mdio_cmd_data     <= 16'd0;
            mdio_cmd_opcode   <= OP_WRITE;
            mdio_cmd_valid    <= 1'b0;
            hello_start       <= 1'b0;
            select_use_switch <= 1'b0;
            done              <= 1'b0;
        end else begin
            // One-shot outputs: default low every cycle, pulsed by their state.
            hello_start       <= 1'b0;
            select_use_switch <= 1'b0;

            case (state)
                // =========================================================
                // Post-reset settle: wait RESET_WAIT_CYCLES before MDIO.
                // =========================================================
                S_RESET_WAIT: begin
                    if (wait_ctr < RESET_WAIT_CYCLES - 1) begin
                        wait_ctr <= wait_ctr + 1'b1;
                    end else begin
                        wait_ctr <= 32'd0;
                        state    <= S_EXTA_ISSUE;
                    end
                end

                // =========================================================
                // set_phy_rx_delay.sh: force RX-only 2ns RGMII delay
                // =========================================================

                // [1] W 0x1f <= 0x0007 : select ExtPage access mode
                S_EXTA_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1f;
                    mdio_cmd_data     <= 16'h0007;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_EXTA_WAIT;
                    end
                end
                S_EXTA_WAIT: if (cmd_ready) state <= S_A4A_ISSUE;

                // [2] W 0x1e <= 0x00a4 : select ExtPage 0xA4
                S_A4A_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1e;
                    mdio_cmd_data     <= 16'h00a4;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_A4A_WAIT;
                    end
                end
                S_A4A_WAIT: if (cmd_ready) state <= S_R1C_ISSUE;

                // [3] R 0x1c : read current RGMII delay config into reg1c_value
                S_R1C_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1c;
                    mdio_cmd_data     <= 16'h0000;
                    mdio_cmd_opcode   <= OP_READ;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_R1C_WAIT;
                    end
                end
                S_R1C_WAIT: begin
                    if (mdio_data_out_valid) begin
                        reg1c_value <= mdio_data_out;
                        last_read   <= mdio_data_out;
                        state       <= S_W1C_ISSUE;
                    end
                end

                // [4] W 0x1c <= OLD | RGMII_DELAY_MASK : force RX-only 2ns delay
                S_W1C_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1c;
                    mdio_cmd_data     <= reg1c_value | RGMII_DELAY_MASK;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_W1C_WAIT;
                    end
                end
                S_W1C_WAIT: if (cmd_ready) state <= S_SLEEP_1C;

                // [5] sleep 2 : let the delay write settle
                S_SLEEP_1C: begin
                    if (wait_ctr < SLEEP_2S - 1) begin
                        wait_ctr <= wait_ctr + 1'b1;
                    end else begin
                        wait_ctr <= 32'd0;
                        state    <= S_EXTB_ISSUE;
                    end
                end

                // [6] W 0x1f <= 0x0007 : re-arm ExtPage access before readback
                S_EXTB_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1f;
                    mdio_cmd_data     <= 16'h0007;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_EXTB_WAIT;
                    end
                end
                S_EXTB_WAIT: if (cmd_ready) state <= S_A4B_ISSUE;

                // [7] W 0x1e <= 0x00a4 : re-arm ExtPage 0xA4
                S_A4B_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1e;
                    mdio_cmd_data     <= 16'h00a4;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_A4B_WAIT;
                    end
                end
                S_A4B_WAIT: if (cmd_ready) state <= S_R1CB_ISSUE;

                // [8] R 0x1c : readback (script verifies delay bits in SW; HW just
                //     reproduces the bus read and latches it for visibility)
                S_R1CB_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1c;
                    mdio_cmd_data     <= 16'h0000;
                    mdio_cmd_opcode   <= OP_READ;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_R1CB_WAIT;
                    end
                end
                S_R1CB_WAIT: begin
                    if (mdio_data_out_valid) begin
                        last_read <= mdio_data_out;
                        state     <= S_PG0_ISSUE;
                    end
                end

                // [9] W 0x1f <= 0x0000 : return to page 0
                S_PG0_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1f;
                    mdio_cmd_data     <= 16'h0000;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_PG0_WAIT;
                    end
                end
                S_PG0_WAIT: if (cmd_ready) state <= S_PHYSRA_ISSUE;

                // [10] R 0x11 : mdio-link PHYSR read (link status)
                S_PHYSRA_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h11;
                    mdio_cmd_data     <= 16'h0000;
                    mdio_cmd_opcode   <= OP_READ;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_PHYSRA_WAIT;
                    end
                end
                S_PHYSRA_WAIT: begin
                    if (mdio_data_out_valid) begin
                        last_read <= mdio_data_out;
                        state     <= S_RST_ISSUE;
                    end
                end

                // =========================================================
                // startup.sh (RATE=100): program rate/duplex
                // (RATE=100 -> gig-ctrl reg 0x09 write is skipped)
                // =========================================================

                // [11] W 0x00 <= 0x8000 : BMCR soft reset (reset bit only)
                S_RST_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h00;
                    mdio_cmd_data     <= BMCR_SOFT_RESET;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_RST_WAIT;
                    end
                end
                S_RST_WAIT: if (cmd_ready) state <= S_SLEEP_RST;

                // [12] sleep 1 : allow PHY reset to complete
                S_SLEEP_RST: begin
                    if (wait_ctr < SLEEP_1S - 1) begin
                        wait_ctr <= wait_ctr + 1'b1;
                    end else begin
                        wait_ctr <= 32'd0;
                        state    <= S_BMCR_ISSUE;
                    end
                end

                // [13] W 0x00 <= PHY_BMCR_FORCE : final rate/duplex after reset
                S_BMCR_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h00;
                    mdio_cmd_data     <= PHY_BMCR_FORCE;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_BMCR_WAIT;
                    end
                end
                S_BMCR_WAIT: if (cmd_ready) state <= S_RBMCR1_ISSUE;

                // [14] R 0x00 : immediate BMCR readback
                S_RBMCR1_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h00;
                    mdio_cmd_data     <= 16'h0000;
                    mdio_cmd_opcode   <= OP_READ;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_RBMCR1_WAIT;
                    end
                end
                S_RBMCR1_WAIT: begin
                    if (mdio_data_out_valid) begin
                        last_read <= mdio_data_out;
                        state     <= S_SLEEP_SETTLE;
                    end
                end

                // [15] sleep 1 : allow PHY/link state to settle
                S_SLEEP_SETTLE: begin
                    if (wait_ctr < SLEEP_1S - 1) begin
                        wait_ctr <= wait_ctr + 1'b1;
                    end else begin
                        wait_ctr <= 32'd0;
                        state    <= S_RBMCR2_ISSUE;
                    end
                end

                // [16] R 0x00 : settled BMCR readback
                S_RBMCR2_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h00;
                    mdio_cmd_data     <= 16'h0000;
                    mdio_cmd_opcode   <= OP_READ;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_RBMCR2_WAIT;
                    end
                end
                S_RBMCR2_WAIT: begin
                    if (mdio_data_out_valid) begin
                        last_read <= mdio_data_out;
                        state     <= S_PHYSRB_ISSUE;
                    end
                end

                // [17] R 0x11 : settled PHYSR readback
                S_PHYSRB_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h11;
                    mdio_cmd_data     <= 16'h0000;
                    mdio_cmd_opcode   <= OP_READ;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_PHYSRB_WAIT;
                    end
                end
                S_PHYSRB_WAIT: begin
                    if (mdio_data_out_valid) begin
                        last_read <= mdio_data_out;
                        state     <= S_SLEEP_MARGIN;
                    end
                end

                // [18] sleep 2 : margin before UDP traffic
                S_SLEEP_MARGIN: begin
                    if (wait_ctr < SLEEP_2S - 1) begin
                        wait_ctr <= wait_ctr + 1'b1;
                    end else begin
                        wait_ctr <= 32'd0;
                        state    <= S_HELLO;
                    end
                end

                // =========================================================
                // Tail: ping (Hello!) + select-chip 0 + done
                // =========================================================

                // [19] ping : pulse hello_start (host `ping` equivalent)
                S_HELLO: begin
                    hello_start <= 1'b1;
                    state       <= S_SELECT_SW;
                end

                // [20] select-chip 0 : once the Hello!/ping completes, pulse
                //      select_use_switch so the chip-select mux defaults to the
                //      BOARD SWITCH (io_select), not the host software select
                //      register value.
                S_SELECT_SW: begin
                    if (hello_done) begin
                        select_use_switch <= 1'b1;
                        state             <= S_DONE;
                    end
                end

                // Bring-up complete; hold done high.
                S_DONE: done <= 1'b1;

                default: state <= S_RESET_WAIT;
            endcase
        end
    end

endmodule
