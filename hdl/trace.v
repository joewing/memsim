
`include "ram.v"

module trace();

   parameter ADDR_WIDTH = 64;    // Bits in an address.
   parameter WORD_SHIFT = 3;     // n where 2^n is bytes in a word.

   localparam WORD_SIZE  = 1 << WORD_SHIFT;
   localparam WORD_WIDTH = 8 * WORD_SIZE;
   localparam WORD_MASK = (1 << WORD_SHIFT) - 1;

   integer i;
   integer fd;
   integer ch;
   integer state;
   integer action;
   integer cycle_count;
   reg [ADDR_WIDTH-1:0] address;
   integer size;
   integer value;
   integer count;

   localparam STATE_ACTION    = 0;
   localparam STATE_ADDRESS   = 1;
   localparam STATE_SIZE      = 2;

   reg clk;
   reg rst;
   reg [ADDR_WIDTH-1:0] mem_addr;
   wire [WORD_WIDTH-1:0] mem_din;
   wire [WORD_WIDTH-1:0] mem_dout;
   reg mem_re;
   reg mem_we;
   wire mem_ready;

   ram r(clk, rst, mem_addr, mem_din, mem_dout, mem_re, mem_we, mem_ready);

   function integer parse_number;
      input integer ch;
      begin
         if (ch >= 8'h30 && ch <= 8'h39) begin
            parse_number = ch - 8'h30;
         end else if (ch >= 8'h61 && ch <= 8'h66) begin
            parse_number = ch - 8'h61 + 10;
         end else begin
            parse_number = -1;
         end
      end
   endfunction

   function [63:0] get_count;
      input [63:0] addr;
      input [63:0] size;
      begin
         get_count = (size + (addr & WORD_MASK) + WORD_SIZE - 1) / WORD_SIZE;
      end
   endfunction

   task cycle;
      begin
         #10 clk <= 1;
         #10 clk <= 0;
         cycle_count <= cycle_count + 1;
      end
   endtask

   task wait_idle;
      while (!mem_ready) begin
         cycle();
      end
   endtask

   initial begin

      fd = $fopen("input.trace", "r");
      if (fd == 0) begin
         $display("could not open trace file");
         $stop;
      end

      cycle_count = 0;
      mem_re = 0;
      mem_we = 0;
      state = STATE_ACTION;
      action = 0;
      rst <= 1;
      #10 clk <= 1; #10 clk <= 0;
      rst <= 0;
      cycle();
      ch = $fgetc(fd);
      while (ch != -1) begin
         case (state)
            STATE_ACTION:
               begin
                  action   = ch;
                  address  = 0;
                  size     = 0;
                  case (ch)
                     8'h52:   // 'R'
                        state = STATE_ADDRESS;
                     8'h57:   // 'W'
                        state = STATE_ADDRESS;
                     8'h4d:   // 'M'
                        state = STATE_ADDRESS;
                     8'h49:   // 'I'
                        state = STATE_SIZE;
                     default:
                        state = STATE_ACTION;
                  endcase
                  ch = $fgetc(fd);
               end
            STATE_ADDRESS:
               begin
                  if (ch == 8'h3A) begin     // ':'
                     state = STATE_SIZE;
                     ch = $fgetc(fd);
                  end else begin
                     value = parse_number(ch);
                     if (value < 0) begin
                        state = STATE_ACTION;
                     end else begin
                        address = address * 16 + value;
                        ch = $fgetc(fd);
                     end
                  end
               end
            STATE_SIZE:
               begin
                  value = parse_number(ch);
                  if (value < 0) begin
                     case (action)
                        8'h52:   // 'R'
                           begin
                              count = get_count(address, size);
                              mem_addr = address & ~WORD_MASK;
                              for (i = 0; i < count; i = i + 1) begin
                                 mem_re <= 1;
                                 cycle();
                                 mem_re <= 0;
                                 wait_idle();
                                 mem_addr <= mem_addr + WORD_SIZE;
                              end
                           end
                        8'h57:   // 'W'
                           begin
                              count = get_count(address, size);
                              mem_addr = address & ~WORD_MASK;
                              for (i = 0; i < count; i = i + 1) begin
                                 mem_we <= 1;
                                 cycle();
                                 mem_we <= 0;
                                 wait_idle();
                                 mem_addr <= mem_addr + WORD_SIZE;
                              end
                           end
                        8'h4d:   // 'M'
                           begin
                              count = get_count(address, size);
                              mem_addr = address & ~WORD_MASK;
                              for (i = 0; i < count; i = i + 1) begin
                                 mem_re <= 1;
                                 cycle();
                                 mem_re <= 0;
                                 wait_idle();
                                 mem_we <= 1;
                                 cycle();
                                 mem_we <= 0;
                                 wait_idle();
                                 mem_addr <= mem_addr + WORD_SIZE;
                              end
                           end
                        8'h49:   // 'I'
                           for (i = 0; i < size; i = i + 1) begin
                              cycle();
                           end
                        default:
                           begin
                              $display("invalid action");
                              $stop;
                           end
                     endcase
                     state = STATE_ACTION;
                  end else begin
                     size = size * 16 + value;
                     ch = $fgetc(fd);
                  end
               end
            default:
               begin
                  $display("invalid state: %d", state);
                  $stop;
               end
         endcase
      end

      $fclose(fd);

      $display("cycles: %d", cycle_count);

   end

endmodule
