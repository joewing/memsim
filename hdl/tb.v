
`include "ram.v"
`include "spm.v"
`include "split.v"
`include "combine.v"
`include "cache.v"

`define CHECK( tst, msg ) \
   if (!(tst)) begin \
      $display("assertion failure: %s", msg); \
      $finish(1); \
   end

`define CYCLE        #10 clk <= 1; #10 clk <= 0;

`define WAIT_READY while (!mem_ready) begin `CYCLE end

`define WRITE( addr, data ) \
   `WAIT_READY \
   mem_addr <= addr; mem_din <= data; mem_we <= 1; \
   `CYCLE \
   mem_we <= 0;

`define READ( addr ) \
   `WAIT_READY \
   mem_addr <= addr; mem_re <= 1; \
   `CYCLE \
   mem_re <= 0;

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

   reg [63:0] mem_addr;
   reg [63:0] mem_din;
   wire [63:0] mem_dout;
   reg mem_re;
   reg mem_we;
   wire mem_ready;

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

   spm s(clk, rst, mem_addr, mem_din, mem_dout, mem_re, mem_we, mem_ready,
         split_addr, split_din, split_dout, split_re, split_we, split_ready);
*/
   cache c(clk, rst, mem_addr, mem_din, mem_dout, mem_re, mem_we, mem_ready,
           ram_addr, ram_din, ram_dout, ram_re, ram_we, ram_ready);

   initial begin

      $dumpvars;

      clk <= 0;
      rst <= 1;
      mem_addr <= 0;
      mem_din <= 0;
      mem_re <= 0;
      mem_we <= 0;
      `CYCLE
      rst <= 0;
      `CYCLE


      // Write a value to the scratchpad.
      `CHECK( mem_ready, "not ready after reset" )
      `WRITE( 1, 64'h0123456789abcdef )
      `CHECK( !mem_ready, "ready too soon after first write [1]" )
      `CYCLE
      `CHECK( mem_ready, "not ready after first write [1]" )

      // Make sure the write took (read; hit).
      `READ( 1 )
      `CHECK( !mem_ready, "ready too soon after read [1]" )
      `CYCLE
      `CHECK( mem_ready, "not ready after read [1]" )
      `CHECK( mem_dout === 64'h0123456789abcdef, "invalid data from read [1]" )
      `CYCLE
      `CYCLE

      // Write a value to the RAM (conflict).
      `WRITE( 257, 123 )
      for (i = 0; i < 102; i = i + 1) begin
         `CHECK( !mem_ready, "ready too soon after write [257]" )
         `CYCLE
      end
      `CHECK( mem_ready, "not ready after write [257]" )

      // Read the value from RAM (hit).
      `READ( 257 )
      `CYCLE
      `CHECK( mem_ready, "not ready after read [257]")
      `CHECK( mem_dout === 123, "invalid data from read [257]" )

      // Make sure the first write is still there (read; miss).
      `WAIT_READY
      `READ( 1 )
      `WAIT_READY
      `CHECK( mem_dout === 64'h0123456789abcdef, "invalid data from read [1]" )

      // Write another value.
      `WRITE( 256, 321 )
      `WAIT_READY

      // Make sure everything is still ok.
      `READ( 257 )
      `WAIT_READY
      `CHECK( mem_dout === 123, "invalid data from read [257]" )
      `READ( 1 )
      `WAIT_READY
      `CHECK( mem_dout === 64'h0123456789abcdef, "invalid data from read [1]" )
      `READ( 256 )
      `WAIT_READY
      `CHECK( mem_dout === 321, "invalid data from read [256]" )

      // Overwrite the value at 1.
      `WRITE( 1, 5 )
      `WAIT_READY
      `CHECK( mem_dout === 123, "invalid data from read [257]" )
      `READ( 1 )
      `WAIT_READY
      `CHECK( mem_dout === 1, "invalid data from read [1]" )
      `READ( 256 )
      `WAIT_READY
      `CHECK( mem_dout === 321, "invalid data from read [256]" )

      $display("Test complete");

   end

endmodule
