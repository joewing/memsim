
module split(clk, rst, addr, din, dout, re, we, ready,
             maddr0, mout0, min0, mre0, mwe0, mready0,
             maddr1, mout1, min1, mre1, mwe1, mready1);

   parameter ADDR_WIDTH = 64;
   parameter WORD_WIDTH = 64;
   parameter OFFSET     = 128;

   input wire clk;
   input wire rst;

   input  wire [ADDR_WIDTH-1:0] addr;
   input  wire [WORD_WIDTH-1:0] din;
   output wire [WORD_WIDTH-1:0] dout;
   input  wire re;
   input  wire we;
   output wire ready;

   output wire [ADDR_WIDTH-1:0] maddr0;
   output wire [WORD_WIDTH-1:0] mout0;
   input  wire [WORD_WIDTH-1:0] min0;
   output wire mre0;
   output wire mwe0;
   input  wire mready0;

   output wire [ADDR_WIDTH-1:0] maddr1;
   output wire [WORD_WIDTH-1:0] mout1;
   input  wire [WORD_WIDTH-1:0] min1;
   output wire mre1;
   output wire mwe1;
   input  wire mready1;

   wire above_split = addr >= OFFSET;

   assign maddr0  = addr;
   assign maddr1  = addr - OFFSET;
   assign mout0   = din;
   assign mout1   = din;
   assign dout    = above_split ? min1 : min0;
   assign mre0    = ~above_split & re;
   assign mre1    = above_split & re;
   assign mwe0    = ~above_split & we;
   assign mwe1    = above_split & we;
   assign ready   = above_split ? mready1 : mready0;

endmodule
