
module spm(clk, rst, addr, din, dout, re, we, ready,
           maddr, mout, min, mre, mwe, mready);

   parameter SIZE       = 128;   // Size in words.
   parameter ADDR_WIDTH = 64;    // Size of address in bits.
   parameter WORD_WIDTH = 64;    // Size of a word in bits.

   input wire clk;
   input wire rst;

   input  wire [ADDR_WIDTH-1:0] addr;
   input  wire [WORD_WIDTH-1:0] din;
   output reg  [WORD_WIDTH-1:0] dout;
   input  wire re;
   input  wire we;
   output wire ready;

   output reg  [ADDR_WIDTH-1:0] maddr;
   output reg  [WORD_WIDTH-1:0] mout;
   input  wire [WORD_WIDTH-1:0] min;
   output reg  mre;
   output reg  mwe;
   input  wire mready;

   reg [WORD_WIDTH-1:0] data [0:SIZE-1];

   wire is_hit    = addr < SIZE;
   wire is_miss   = !is_hit;

   always @(posedge clk) begin
      mre <= 0;
      mwe <= 0;
      if (!rst) begin
         if (!ready) begin
            dout  <= min;
         end else if (re) begin
            if (is_hit) begin
               dout <= data[addr];
            end else begin
               maddr <= addr;
               mre   <= 1;
            end
         end else if (we) begin
            if (is_hit) begin
               data[addr] <= din;
            end else begin
               maddr <= addr;
               mout  <= din;
               mwe   <= 1;
            end
         end
      end
   end

   assign ready = is_hit ? 1 : mready;

endmodule
