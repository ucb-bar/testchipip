// Hardware re-implementation of startup.sh / set_phy_rx_delay.sh.
//
// On reset, waits RESET_WAIT_CYCLES cycles (default 32'd5000000, i.e. the
// original PHY_MDIO_WAIT_CYCLES BMCR-force delay), then drives the PHY
// MDIO bring-up sequence that used to be done by hand over the UART MDIO
// bridge:
//   1. mdio-write 0x1f <= 0x0007   (select ExtPage access mode)
//   2. mdio-write 0x1e <= 0x00a4   (select ExtPage 0xA4)
//   3. mdio-read  0x1c             (read current RGMII delay config)
//   4. mdio-write 0x1c <= old|0x3000 (force RX-only 2ns delay)
//   5. mdio-write 0x1f <= 0x0000   (return to page 0)
//   6. mdio-write 0x00 <= 0x0100   (PHY soft reset)
//   7. send "Hello!" out the debug UART (replaces host `ping`)
//   8. assert select_invert        (replaces host `set-select-invert 1`)
//
// The mdio_cmd_* outputs are intended to be muxed into the same
// mdio_master command port used by the existing UART MDIO bridge in
// udp_tsi_top, with the UART path taking priority (this FSM only
// advances when `cmd_ready` is asserted by the arbiter).
module startup_fsm #(
    parameter [4:0]  PHY_MDIO_ADDR     = 5'd1,
    parameter [31:0] RESET_WAIT_CYCLES = 32'd5000000
) (
    input  wire        clk,
    input  wire        rst,

    // ---- MDIO command port (arbitrated with UART path) ----
    output reg  [4:0]  mdio_cmd_phy_addr,
    output reg  [4:0]  mdio_cmd_reg_addr,
    output reg  [15:0] mdio_cmd_data,
    output reg  [1:0]  mdio_cmd_opcode,
    output reg         mdio_cmd_valid,
    input  wire        cmd_ready,          // accepted-by-arbiter pulse gate

    input  wire [15:0] mdio_data_out,
    input  wire        mdio_data_out_valid,

    // ---- "Hello!" message over debug UART ----
    output reg         hello_start,
    input  wire        hello_done,

    // ---- Chip-select invert latch ----
    output reg         select_invert,

    output reg         done
);

    localparam [1:0]
        OP_WRITE = 2'b01,
        OP_READ  = 2'b10;

    localparam [4:0]
        S_RESET_WAIT = 5'd0,
        S_W1_ISSUE   = 5'd1,  // 0x1f <= 0x0007
        S_W1_WAIT    = 5'd2,
        S_W2_ISSUE   = 5'd3,  // 0x1e <= 0x00a4
        S_W2_WAIT    = 5'd4,
        S_R3_ISSUE   = 5'd5,  // read 0x1c
        S_R3_WAIT    = 5'd6,
        S_W4_ISSUE   = 5'd7,  // 0x1c <= old | 0x3000
        S_W4_WAIT    = 5'd8,
        S_W5_ISSUE   = 5'd9,  // 0x1f <= 0x0000
        S_W5_WAIT    = 5'd10,
        S_W6_ISSUE   = 5'd11, // 0x00 <= 0x0100 (PHY soft reset)
        S_W6_WAIT    = 5'd12,
        S_HELLO      = 5'd13,
        S_SELECT_INV = 5'd14,
        S_DONE       = 5'd15;

    reg [5:0]  state;
    reg [31:0] wait_ctr;
    reg [15:0] reg1c_value;

    always @(posedge clk) begin
        if (rst) begin
            state             <= S_RESET_WAIT;
            wait_ctr          <= 32'd0;
            reg1c_value       <= 16'd0;
            mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
            mdio_cmd_reg_addr <= 5'd0;
            mdio_cmd_data     <= 16'd0;
            mdio_cmd_opcode   <= OP_WRITE;
            mdio_cmd_valid    <= 1'b0;
            hello_start       <= 1'b0;
            select_invert     <= 1'b0;
            done              <= 1'b0;
        end else begin
            hello_start <= 1'b0;

            case (state)
                // ---- wait RESET_WAIT_CYCLES cycles after reset ----
                S_RESET_WAIT: begin
                    if (wait_ctr < RESET_WAIT_CYCLES - 1) begin
                        wait_ctr <= wait_ctr + 1'b1;
                    end else begin
                        wait_ctr <= 32'd0;
                        state    <= S_W1_ISSUE;
                    end
                end

                // ---- 1. mdio-write 0x1f <= 0x0007 ----
                S_W1_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1f;
                    mdio_cmd_data     <= 16'h0007;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_W1_WAIT;
                    end
                end
                S_W1_WAIT: begin
                    if (cmd_ready) state <= S_W2_ISSUE;
                end

                // ---- 2. mdio-write 0x1e <= 0x00a4 ----
                S_W2_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1e;
                    mdio_cmd_data     <= 16'h00a4;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_W2_WAIT;
                    end
                end
                S_W2_WAIT: begin
                    if (cmd_ready) state <= S_R3_ISSUE;
                end

                // ---- 3. mdio-read 0x1c ----
                S_R3_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1c;
                    mdio_cmd_data     <= 16'h0000;
                    mdio_cmd_opcode   <= OP_READ;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_R3_WAIT;
                    end
                end
                S_R3_WAIT: begin
                    if (mdio_data_out_valid) begin
                        reg1c_value <= mdio_data_out;
                        state       <= S_W4_ISSUE;
                    end
                end

                // ---- 4. mdio-write 0x1c <= old | 0x3000 ----
                S_W4_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1c;
                    mdio_cmd_data     <= reg1c_value | 16'h3000;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_W4_WAIT;
                    end
                end
                S_W4_WAIT: begin
                    if (cmd_ready) state <= S_W5_ISSUE;
                end

                // ---- 5. mdio-write 0x1f <= 0x0000 (back to page 0) ----
                S_W5_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h1f;
                    mdio_cmd_data     <= 16'h0000;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_W5_WAIT;
                    end
                end
                S_W5_WAIT: begin
                    if (cmd_ready) state <= S_W6_ISSUE;
                end

                // ---- 6. mdio-write 0x00 <= 0x0100 (PHY soft reset) ----
                S_W6_ISSUE: begin
                    mdio_cmd_phy_addr <= PHY_MDIO_ADDR;
                    mdio_cmd_reg_addr <= 5'h00;
                    mdio_cmd_data     <= 16'h0100;
                    mdio_cmd_opcode   <= OP_WRITE;
                    mdio_cmd_valid    <= 1'b1;
                    if (cmd_ready) begin
                        mdio_cmd_valid <= 1'b0;
                        state          <= S_W6_WAIT;
                    end
                end
                S_W6_WAIT: begin
                    if (cmd_ready) state <= S_HELLO;
                end

                // ---- 7. send "Hello!" over debug UART (replaces `ping`) ----
                S_HELLO: begin
                    hello_start <= 1'b1;
                    state       <= S_SELECT_INV;
                end

                // ---- 8. assert select_invert (replaces `set-select-invert 1`) ----
                S_SELECT_INV: begin
                    if (hello_done) begin
                        select_invert <= 1'b1;
                        state         <= S_DONE;
                    end
                end

                S_DONE: begin
                    done <= 1'b1;
                end

                default: state <= S_RESET_WAIT;
            endcase
        end
    end

endmodule
