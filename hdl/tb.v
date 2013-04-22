
`include "ram.v"
`include "spm.v"
`include "split.v"
`include "combine.v"
`include "cache.v"

`define CHECK( tst ) if (!(tst)) begin $display("error"); $stop; end
`define CYCLE        #10 clk <= 1; #10 clk <= 0;

module tb();

   reg clk;
   reg rst;

   wire [63:0] ram_addr;
   wire [63:0] ram_din;
   wire [63:0] ram_dout;
   wire ram_re;
   wire ram_we;
   wire ram_ready;

   wire [63:0] combine0_addr;
   wire [63:0] combine0_din;
   wire [63:0] combine0_dout;
   wire combine0_re;
   wire combine0_we;
   wire combine0_ready;
   wire [63:0] combine1_addr;
   wire [63:0] combine1_din;
   wire [63:0] combine1_dout;
   wire combine1_re;
   wire combine1_we;
   wire combine1_ready;

   wire [63:0] split_addr;
   wire [63:0] split_din;
   wire [63:0] split_dout;
   wire split_re;
   wire split_we;
   wire split_ready;

   reg [63:0] spm_addr;
   reg [63:0] spm_din;
   wire [63:0] spm_dout;
   reg spm_re;
   reg spm_we;
   wire spm_ready;

   integer i;

   ram r(clk, rst, ram_addr, ram_din, ram_dout, ram_re, ram_we, ram_ready);

/*
   combine c(clk, rst,
           combine0_addr, combine0_dout, combine0_din,
           combine0_re, combine0_we, combine0_ready,
           combine1_addr, combine1_dout, combine1_din,
           combine1_re, combine1_we, combine1_ready,
           ram_addr, ram_din, ram_dout, ram_re, ram_we, ram_ready);

   split sp(clk, rst,
         split_addr, split_din, split_dout, split_re, split_we, split_ready,
         combine0_addr, combine0_din, combine0_dout,
         combine0_re, combine0_we, combine0_ready,
         combine1_addr, combine1_din, combine1_dout,
         combine1_re, combine1_we, combine1_ready);

   spm s(clk, rst, spm_addr, spm_din, spm_dout, spm_re, spm_we, spm_ready,
         split_addr, split_din, split_dout, split_re, split_we, split_ready);
*/
   cache c(clk, rst, spm_addr, spm_din, spm_dout, spm_re, spm_we, spm_ready,
           ram_addr, ram_din, ram_dout, ram_re, ram_we, ram_ready);

   initial begin

      $dumpvars;

      clk <= 0;
      rst <= 1;
      spm_addr <= 0;
      spm_din <= 0;
      spm_re <= 0;
      spm_we <= 0;
      `CYCLE
      rst <= 0;
      `CYCLE

      `CHECK( spm_ready )
      `CHECK( ram_ready )

      // Write a value to the scratchpad.
      spm_addr <= 1;
      spm_din  <= 64'h0123456789abcdef;
      spm_we   <= 1;
      `CYCLE
      spm_we   <= 0;

      `CHECK( spm_ready )
      `CHECK( ram_ready )

      // Make sure the write took.
      spm_addr <= 1;
      spm_din  <= 0;
      spm_re   <= 1;
      `CYCLE
      spm_re   <= 0;

      `CHECK( spm_ready )
      `CHECK( ram_ready )
      `CHECK( spm_dout == 64'h0123456789abcdef )

      // Write a value to the RAM.
      spm_addr <= 257;
      spm_din  <= 123;
      spm_we   <= 1;
      `CYCLE
      spm_we   <= 0;

      for (i = 0; i < 100; i = i + 1) begin
         `CHECK( spm_ready == 0 )
         `CYCLE
      end
      `CHECK( spm_ready == 1 )

      $display("Test complete");

   end

endmodule
