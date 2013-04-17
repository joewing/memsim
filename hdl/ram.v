
module ram(clk, rst, addr, din, dout, re, we, ready);

   parameter SIZE       = 65536; // Size in words.
   parameter ADDR_WIDTH = 64;    // Size of address in bits.
   parameter WORD_WIDTH = 64;    // Size of a word in bits.
   parameter LATENCY    = 100;   // Access latency in cycles.

   input wire clk;
   input wire rst;

   input  wire [ADDR_WIDTH-1:0] addr;
   input  wire [WORD_WIDTH-1:0] din;
   output reg  [WORD_WIDTH-1:0] dout;
   input  wire re;
   input  wire we;
   output wire ready;

   reg [WORD_WIDTH-1:0] data [0:SIZE-1];

   reg [31:0] counter;

   always @(posedge clk) begin
      if (rst) begin
         counter <= 0;
      end else begin
         if (re) begin
            counter <= LATENCY - 1;
            dout <= data[addr];
         end else if (we) begin
            counter <= LATENCY - 1;
            data[addr] <= din;
         end else if (!ready) begin
            counter <= counter - 1;
         end
      end
   end

   assign ready = counter == 0 && !re && !we;

endmodule
