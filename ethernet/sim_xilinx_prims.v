`timescale 1ns / 1ps

module BUFG (
    input  wire I,
    output wire O
);
    assign O = I;
endmodule

module BUFIO (
    input  wire I,
    output wire O
);
    assign O = I;
endmodule

module BUFR #(
    parameter BUFR_DIVIDE = "BYPASS"
) (
    input  wire I,
    output wire O,
    input  wire CE,
    input  wire CLR
);
    assign O = CLR ? 1'b0 : (CE ? I : 1'b0);
endmodule

module ODDR #(
    parameter DDR_CLK_EDGE = "SAME_EDGE",
    parameter INIT = 1'b0,
    parameter SRTYPE = "ASYNC"
) (
    output reg  Q,
    input  wire C,
    input  wire CE,
    input  wire D1,
    input  wire D2,
    input  wire R,
    input  wire S
);
    initial Q = INIT;

    always @(posedge C or posedge R or posedge S) begin
        if (R) begin
            Q <= 1'b0;
        end else if (S) begin
            Q <= 1'b1;
        end else if (CE) begin
            Q <= D1;
        end
    end

    always @(negedge C or posedge R or posedge S) begin
        if (R) begin
            Q <= 1'b0;
        end else if (S) begin
            Q <= 1'b1;
        end else if (CE) begin
            Q <= D2;
        end
    end
endmodule

module IDDR #(
    parameter DDR_CLK_EDGE = "SAME_EDGE_PIPELINED",
    parameter INIT_Q1 = 1'b0,
    parameter INIT_Q2 = 1'b0,
    parameter SRTYPE = "ASYNC"
) (
    output reg  Q1,
    output reg  Q2,
    input  wire C,
    input  wire CE,
    input  wire D,
    input  wire R,
    input  wire S
);
    initial begin
        Q1 = INIT_Q1;
        Q2 = INIT_Q2;
    end

    always @(posedge C or posedge R or posedge S) begin
        if (R) begin
            Q1 <= 1'b0;
        end else if (S) begin
            Q1 <= 1'b1;
        end else if (CE) begin
            Q1 <= D;
        end
    end

    always @(negedge C or posedge R or posedge S) begin
        if (R) begin
            Q2 <= 1'b0;
        end else if (S) begin
            Q2 <= 1'b1;
        end else if (CE) begin
            Q2 <= D;
        end
    end
endmodule
