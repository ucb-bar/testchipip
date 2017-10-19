import "DPI-C" function void fsim_manager_init();

import "DPI-C" function void fsim_manager_tick
(
	input   bit req_valid,
	output  bit req_ready,
	input     int req_bits,
	output  bit     resp_valid,
	input           bit resp_ready,
	output  int     resp_bits
);

`define RESP_BITS       32
`define REQ_BITS        32

module SimFSimManager (
	input           clock,
	input           reset,
	input           fsim_req_valid,
	output  fsim_req_ready,
	input           [`REQ_BITS-1:0] fsim_req_bits,

	output  fsim_resp_valid,
	input           fsim_resp_ready,
	output[`RESP_BITS-1:0]  fsim_resp_bits
);

bit __req_ready;
bit __resp_valid;
int     __resp_bits;

reg __req_ready_reg;
reg __resp_valid_reg;
reg [`RESP_BITS-1:0] __resp_bits_reg;

assign fsim_req_ready = __req_ready_reg;
assign fsim_resp_valid = __resp_valid_reg;
assign fsim_resp_bits = __resp_bits_reg;

initial begin
	fsim_manager_init();
end

always @(posedge clock) begin
	if(reset) begin
		__req_ready_reg <= 0;
		__resp_valid_reg <= 0;
		__resp_bits_reg <= 0;
	end else begin
		fsim_manager_tick(
			fsim_req_valid,
			__req_ready,
			fsim_req_bits,

			__resp_valid,
			fsim_resp_ready,
			__resp_bits);

		__req_ready_reg <= __req_ready;
		__resp_valid_reg <= __resp_valid;
		__resp_bits_reg <= __resp_bits;
	end
end
endmodule
