module SimSPIFlashModel #(
    parameter CAPACITY_BYTES,
    parameter ID
) (
    input sck,
    input cs_0,
    input reset,
    inout dq_0,
    inout dq_1,
    inout dq_2,
    inout dq_3
);

    // CAUTION! This model only supports a small subset of standard QSPI flash
    // features. It is useful for modeling a pre-loaded flash memory intended
    // to be used as a ROM. You should replace this with a verilog model of
    // your specific flash device for more rigorous verification. It also very
    // likely contains bugs. Use at your own risk!

    // Supported SPI instructions
    // 3-byte address reads
    localparam CMD_READ               = 8'h03; // No dummy
    localparam CMD_FAST_READ          = 8'h0B;
    localparam CMD_QUAD_O_FAST_READ   = 8'h6B;
    localparam CMD_QUAD_IO_FAST_READ  = 8'hEB;
    // 4-byte address reads
    localparam CMD_READ4              = 8'h13; // No dummy
    localparam CMD_FAST_READ4         = 8'h0C;
    localparam CMD_QUAD_O_FAST_READ4  = 8'h6C;
    localparam CMD_QUAD_IO_FAST_READ4 = 8'hEC;
    // No writes/erases are supported by this model yet
    // No register reads are supported by this model yet

    // SPI mode settings for clock polarity and phase
    localparam CPOL = 1'b0;
    localparam CPHA = 1'b0;
    // CS active low by default, but set this to 1 to invert
    localparam CSXOR = 1'b0;

    // SPI flash behavior settings
    // Often the number of dummy cycles depends on the instruction, but here
    // we'll make it all uniform.
    localparam DUMMY_CYCLES = 8;

    // Stores the file name for the binary memory contents of the flash
    string filename;

    // Flash memory contents
    reg [7:0] mem [0:CAPACITY_BYTES-1];

    initial begin
        // +spiflash0=/path/to/file for SPI flash 0, etc.
        if ($value$plusargs($sformatf("spiflash%0d=%%s", ID), filename)) begin
            $readmemh(filename, mem); // TODO do we want to use the blkdev stuff here?
        end else begin
            $fatal("No hex memory file provided for SPI flash %0d. Use +spiflash%0d=<file> to specify.", ID, ID);
        end
    end

    // State
    reg [2:0] state;
    logic [2:0] state_next;
    // States
    localparam STANDBY  = 3'd0;
    localparam GET_CMD  = 3'd1;
    localparam GET_ADDR = 3'd2;
    localparam DUMMY    = 3'd3;
    localparam PUT_DATA = 3'd4;

    // Incoming data
    reg [31:0] data_buf;
    logic [31:0] data_buf_next;
    // Incoming data bit count
    reg [6:0] data_count;
    logic [6:0] data_count_next;
    // Dummy cycle counter
    reg [7:0] dummy;
    logic [7:0] dummy_next;
    // Command
    reg [7:0] cmd;
    logic [7:0] cmd_next;
    // Address
    reg [31:0] raw_addr;
    logic [31:0] raw_addr_next;
    logic [$clog2(CAPACITY_BYTES)-1:0] addr_next;
    assign addr_next = raw_addr_next[$clog2(CAPACITY_BYTES)-1:0];

    // Internal clock to deal with clock polarity
    logic clock, cs;
    assign clock = CPOL ^ sck;
    assign cs = CSXOR ^ cs_0;

    // Quad data stuff
    logic quad_io, quad_i, quad_o;

    assign quad_i = (cmd_next == CMD_QUAD_IO_FAST_READ) || (cmd_next == CMD_QUAD_IO_FAST_READ4);
    assign quad_o = quad_i || (cmd_next == CMD_QUAD_O_FAST_READ) || (cmd_next == CMD_QUAD_O_FAST_READ4);
    assign quad_io = (quad_i && (state_next == GET_ADDR)) || (quad_o && (state_next == PUT_DATA));

    // State machine stuff
    logic addr_4byte, cmd_done, addr_done, dummy_done, cmd_has_dummy, cmd_valid, incr_addr;

    assign addr_4byte = (cmd == CMD_READ4) || (cmd == CMD_FAST_READ4) || (cmd == CMD_QUAD_O_FAST_READ4) || (cmd == CMD_QUAD_IO_FAST_READ4);
    assign cmd_valid =
        (cmd_next == CMD_READ)  || (cmd_next == CMD_FAST_READ)  || (cmd_next == CMD_QUAD_O_FAST_READ) || (cmd_next == CMD_QUAD_IO_FAST_READ) ||
        (cmd_next == CMD_READ4) || (cmd_next == CMD_FAST_READ4) || (cmd_next == CMD_QUAD_O_FAST_READ4) || (cmd_next == CMD_QUAD_IO_FAST_READ4);
    assign cmd_done  = (state == GET_CMD) && (data_count == 6'd8);
    assign addr_done = (state == GET_ADDR) && (data_count == (addr_4byte ? 6'd40 : 6'd32));
    assign dummy_done = dummy == (DUMMY_CYCLES - 1);
    assign cmd_has_dummy = (cmd != CMD_READ) && (cmd != CMD_READ4);
    assign incr_addr = (state == PUT_DATA) && (data_count[2:0] == 3'b000); // We let the counter free run...this just increments every byte

    assign data_buf_next = quad_io ? {data_buf[27:0], dq_3, dq_2, dq_1, dq_0} : {data_buf[30:0], dq_0};
    assign data_count_next = (state_next == DUMMY) ? data_count : data_count + (quad_io ? 6'd4 : 6'd1) ;
    assign dummy_next = (state == DUMMY) ? dummy + 8'd1 : 8'd0 ;

    always_comb begin
        if (state == DUMMY) raw_addr_next = raw_addr;
        else if (addr_done) raw_addr_next = addr_4byte ? data_buf : (data_buf & 32'h00ff_ffff);
        else if (incr_addr) raw_addr_next = raw_addr + 32'd1;
        else raw_addr_next = raw_addr;
    end

    assign cmd_next = cmd_done ? data_buf[7:0] : cmd ;

    // Output driving
    reg [7:0] data_out;
    reg drive_dq;
    logic [3:0] dq_out;

    assign dq_out[3] = data_out[7];
    assign dq_out[2] = data_out[6];
    assign dq_out[1] = quad_io ? data_out[5] : data_out[7] ;
    assign dq_out[0] = data_out[4];

    assign dq_3 = (drive_dq && quad_io) ? dq_out[3] : 1'bz;
    assign dq_2 = (drive_dq && quad_io) ? dq_out[2] : 1'bz;
    assign dq_1 =  drive_dq             ? dq_out[1] : 1'bz;
    assign dq_0 = (drive_dq && quad_io) ? dq_out[0] : 1'bz;

    always_ff @(posedge clock or negedge clock or posedge reset or posedge cs) begin
        if (reset | cs) begin
            data_count <= 5'd0;
            state <= STANDBY;
            dummy <= 8'd0;
            drive_dq <= 1'b0;
            cmd <= 8'd0;
        end else begin
            if (clock ^ CPHA) begin
                // Capture edge
                data_buf    <= data_buf_next;
                data_count  <= data_count_next;
                state       <= state_next;
                dummy       <= dummy_next;
                raw_addr    <= raw_addr_next;
                cmd         <= cmd_next;
            end else begin
                // Launch edge
                drive_dq <= (state_next == PUT_DATA);
                if (data_count[2:0] == 3'b000) begin
                    data_out <= mem[addr_next];
                end else begin
                    data_out <= quad_io ? {data_out[3:0], 4'd0} : {data_out[6:0], 1'd0} ;
                end
            end
        end
    end

    always_comb begin
        casez(state)
            STANDBY:  state_next = cs         ? STANDBY  : GET_CMD ;
            GET_CMD:  state_next = cmd_done   ? (cmd_valid ? GET_ADDR : STANDBY) : GET_CMD ;
            GET_ADDR: state_next = addr_done  ? (cmd_has_dummy ? DUMMY : PUT_DATA) : GET_ADDR ;
            DUMMY:    state_next = dummy_done ? PUT_DATA : DUMMY ;
            PUT_DATA: state_next = cs         ? STANDBY  : PUT_DATA ;
            default:  state_next = STANDBY;
        endcase
    end

endmodule
