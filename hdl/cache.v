
module cache(clk, rst, addr, din, dout, re, we, ready,
             maddr, mout, min, mre, mwe, mready);

   parameter ADDR_WIDTH    = 64;    // Size of address in bits.
   parameter WORD_WIDTH    = 64;    // Size of a word in bits.
   parameter LINE_SIZE     = 1;     // Words per line.
   parameter LINE_COUNT    = 256;   // Number of cache lines.
   parameter ASSOCIATIVITY = 1;     // Associativity.
   parameter LATENCY       = 1;     // Access latency.

   input wire clk;
   input wire rst;

   input  wire [ADDR_WIDTH-1:0] addr;
   input  wire [WORD_WIDTH-1:0] din;
   output reg  [WORD_WIDTH-1:0] dout;
   input  wire re;
   input  wire we;
   output reg  ready;

   output reg  [ADDR_WIDTH-1:0] maddr;
   output reg  [WORD_WIDTH-1:0] mout;
   input  wire [WORD_WIDTH-1:0] min;
   output reg  mre;
   output reg  mwe;
   input  wire mready;

   localparam INDEX_BITS   = ADDR_WIDTH - (1 << LINE_SIZE);
   localparam TAG_BITS     = (ADDR_WIDTH / LINE_SIZE) + ASSOCIATIVITY;
   localparam AGE_BITS     = ASSOCIATIVITY - 1;
   localparam LINE_BITS    = LINE_SIZE * WORD_WIDTH + TAG_BITS + AGE_BITS + 1;
   localparam DATA_WIDTH   = (LINE_BITS * ASSOCIATIVITY) / LATENCY;
   localparam DATA_OFFSET  = 0;
   localparam TAG_OFFSET   = DATA_OFFSET + DATA_WIDTH;
   localparam DIRTY_OFFSET = TAG_OFFSET + TAG_BITS;
   localparam AGE_OFFSET   = DIRTY_OFFSET + 1;

   reg  [DATA_WIDTH-1:0] data [0:LINE_COUNT-1];
   wire [INDEX_BITS-1:0] current_index = addr[INDEX_BITS-1:0];
   wire [TAG_BITS-1:0] current_tag = addr[ADDR_WIDTH-1:ADDR_WIDTH-TAG_BITS];
   wire [DATA_WIDTH-1:0] row = data[current_index];
   wire [31:0] line_offset = addr & ((1 << LINE_SIZE) - 1);
   wire [31:0] line_shift  = line_offset * WORD_WIDTH;

   wire [LINE_SIZE*WORD_WIDTH-1:0]  line  [0:ASSOCIATIVITY-1];
   wire [TAG_BITS-1:0]              tag   [0:ASSOCIATIVITY-1];
   wire                             dirty [0:ASSOCIATIVITY-1];
   wire [AGE_BITS-1:0]              age   [0:ASSOCIATIVITY-1];
   genvar i;
   generate
      for (i = 0; i < ASSOCIATIVITY; i = i + 1) begin
         localparam offset       = i * DATA_WIDTH;
         localparam line_start   = offset + DATA_OFFSET;
         localparam tag_start    = offset + TAG_OFFSET;
         localparam dirty_start  = offset + DIRTY_OFFSET;
         localparam age_start    = offset + AGE_OFFSET;
         assign line[i]    = row[line_start+DATA_WIDTH-1:line_start];
         assign tag[i]     = row[tag_start+TAG_BITS-1:tag_start];
         assign dirty[i]   = row[dirty_start];
         if (AGE_BITS > 0) begin
            assign age[i]  = row[age_start+AGE_BITS-1:age_start];
         end
      end
   endgenerate

   reg [ADDR_WIDTH-1:0]             oldest_addr;
   reg [ASSOCIATIVITY-1:0]          oldest_way;
   reg [LINE_SIZE*WORD_WIDTH-1:0]   oldest_line;
   reg                              oldest_dirty;
   reg [AGE_BITS-1:0]               oldest_age;
   reg [ASSOCIATIVITY-1:0] way;
   reg is_hit;
   reg [LINE_SIZE*WORD_WIDTH-1:0] hit_line;
   reg [31:0] wayi;
   always @(*) begin
      oldest_age <= 0;
      way <= 0;
      is_hit <= 0;
      for (wayi = 0; wayi < ASSOCIATIVITY; wayi = wayi + 1) begin
         if (oldest_age < age[wayi]) begin
            oldest_age     <= age[wayi];
            oldest_way     <= 1 << wayi;
            oldest_dirty   <= dirty[wayi];
            oldest_line    <= line[wayi];
            oldest_addr    <= {current_index, tag[wayi]};
         end
         if (current_tag == tag[wayi]) begin
            way            <= 1 << wayi;
            is_hit         <= 1;
            hit_line       <= line[wayi];
         end
      end
   end

   localparam STATE_IDLE            = 0;
   localparam STATE_READ            = 1;
   localparam STATE_READ_WRITEBACK  = 2;
   localparam STATE_WRITE_WRITEBACK = 3;

   reg [1:0] state;
   reg [31:0] transfer_count;

   reg [LINE_SIZE*WORD_WIDTH-1:0] ram_row;
   reg [LINE_SIZE*WORD_WIDTH-1:0] shifted_input;
   reg [WORD_WIDTH-1:0]           shifted_output [0:LINE_SIZE-1];
   genvar shifti;
   generate
      for (shifti = 0; shifti < LINE_SIZE; shifti = shifti + 1) begin
         localparam offset = shifti * WORD_WIDTH;
         always @(*) begin
            if (line_offset == shifti) begin
               shifted_input[offset+WORD_WIDTH-1:offset] =
                  hit_line[offset+WORD_WIDTH-1:offset];
            end else begin
               shifted_input[offset+WORD_WIDTH-1:offset] = din;
            end
            shifted_output[shifti] = oldest_line[offset+WORD_WIDTH-1:offset];
         end
      end
   endgenerate

   always @(posedge clk) begin
      ready <= 0;
      mre   <= 0;
      mwe   <= 0;
      if (rst) begin
         state <= STATE_IDLE;
         transfer_count <= 0;
      end else begin
         case (state)
            STATE_READ:
               if (mready) begin
                  if (transfer_count < LINE_SIZE) begin
                     transfer_count <= transfer_count - 1;
                  end else begin
                     state <= STATE_IDLE;
                  end
               end
            STATE_WRITEBACK_READ, STATE_WRITEBACK_WRITE:
               if (mready) begin
                  if (transfer_count < LINE_SIZE) begin
                     transfer_count <= transfer_count - 1;
                  end else if (state == STATE_WRITEBACK_WRITE) begin
                     state <= STATE_IDLE;
                  end else begin
                     state <= STATE_READ;
                  end
               end
            default:    // Idle
               if (re) begin
                  if (is_hit) begin
                     ready <= 1;
                     dout  <= hit_line;
                  end else begin
                     if (oldest_dirty) begin
                        state <= STATE_WRITEBACK;
                        mwe   <= 1;
                        mre   <= 0;
                        transfer_count <= 0;
                     end else begin
                        state <= STATE_READ;
                     end
                  end
               end else if (we) begin
                  if (is_hit) begin
                     ready <= 1;
                  end else begin
                     if (oldest_dirty) begin
                        state <= STATE_WRITEBACK;
                     end else begin
                        state <= STATE_WRITE;
                     end
                  end
               end
         endcase
      end
   end

   reg [31:0] ti;
   always @(*) begin
      maddr <= oldest_addr + transfer_count * WORD_WIDTH / 8;
      mout  <= shifted_output[transfer_count];
   end

endmodule
