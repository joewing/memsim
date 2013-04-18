
module combine(clk, rst,
               addr0, din0, dout0, re0, we0, ready0,
               addr1, dout1, din1, re1, we1, ready1,
               maddr, mout, min, mre, mwe, mready);

   parameter ADDR_WIDTH = 64;
   parameter WORD_WIDTH = 64;
   parameter OFFSET     = 128;

   input wire clk;
   input wire rst;

   input  wire [ADDR_WIDTH-1:0] addr0;
   input  wire [WORD_WIDTH-1:0] din0;
   output wire [WORD_WIDTH-1:0] dout0;
   input  wire re0;
   input  wire we0;
   output wire ready0;

   input  wire [ADDR_WIDTH-1:0] addr1;
   input  wire [WORD_WIDTH-1:0] din1;
   output wire [WORD_WIDTH-1:0] dout1;
   input  wire re1;
   input  wire we1;
   output wire ready1;

   output wire [ADDR_WIDTH-1:0] maddr;
   output wire [WORD_WIDTH-1:0] mout;
   input  wire [WORD_WIDTH-1:0] min;
   output wire mre;
   output wire mwe;
   input  wire mready;

   wire above_split = addr >= OFFSET;

   assign maddr   = above_split ? (addr1 + OFFSET) : addr0;
   assign mout    = above_split ? dout1 : dout0;
   assign din0    = min;
   assign din1    = min;
   assign mre     = above_split ? re1 : re0;
   assign mwe     = above_split ? we1 : we1;
   assign ready0  = mready;
   assign ready1  = mready;

endmodule

