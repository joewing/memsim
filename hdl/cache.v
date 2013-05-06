
module cache(clk, rst, addr, din, dout, re, we, ready,
             maddr, mout, min, mre, mwe, mready);

   parameter ADDR_WIDTH       = 64;    // Size of address in bits.
   parameter WORD_WIDTH       = 64;    // Size of a word in bits.
   parameter LINE_SIZE_BITS   = 0;     // 2^n words per line.
   parameter LINE_COUNT_BITS  = 8;     // 2^n cache lines.
   parameter ASSOC_BITS       = 0;     // 2^n associativity.

   input wire clk;
   input wire rst;

   input  wire [ADDR_WIDTH-1:0] addr;
   input  wire [WORD_WIDTH-1:0] din;
   output wire [WORD_WIDTH-1:0] dout;
   input  wire re;
   input  wire we;
   output wire ready;

   output wire [ADDR_WIDTH-1:0] maddr;
   output wire [WORD_WIDTH-1:0] mout;
   input  wire [WORD_WIDTH-1:0] min;
   output reg  mre;
   output reg  mwe;
   input  wire mready;

   localparam ASSOCIATIVITY   = 1 << ASSOC_BITS;
   localparam LINE_SIZE       = 1 << LINE_SIZE_BITS;
   localparam LINE_COUNT      = 1 << LINE_COUNT_BITS;
   localparam INDEX_BITS      = LINE_COUNT_BITS;
   localparam TAG_BITS        = ADDR_WIDTH - INDEX_BITS - LINE_SIZE_BITS
                              + ASSOC_BITS;
   localparam AGE_BITS        = ASSOC_BITS;
   localparam LINE_BITS       = WORD_WIDTH << LINE_SIZE_BITS;
   localparam WAY_BITS        = LINE_BITS + TAG_BITS + AGE_BITS + 1 + 1;
   localparam ROW_BITS        = WAY_BITS << ASSOC_BITS;
   localparam LINE_OFFSET     = 0;
   localparam TAG_OFFSET      = LINE_OFFSET + LINE_BITS;
   localparam AGE_OFFSET      = TAG_OFFSET + TAG_BITS;
   localparam DIRTY_OFFSET    = AGE_OFFSET + AGE_BITS;
   localparam VALID_OFFSET    = DIRTY_OFFSET + 1;
   localparam MAX_AGE         = (1 << AGE_BITS) - 1;
   localparam MASK_BITS       = ADDR_WIDTH - LINE_SIZE_BITS;
   localparam ADDR_MASK       = {{MASK_BITS{1'b1}},{LINE_SIZE_BITS{1'b0}}};
   localparam TAG_SHIFT       = ADDR_WIDTH - TAG_BITS;

   reg [ROW_BITS-1:0] row;
   reg  [ROW_BITS-1:0] data [0:LINE_COUNT-1];
   wire [INDEX_BITS-1:0] current_index = addr >> LINE_SIZE_BITS;
   wire [TAG_BITS-1:0] current_tag = addr >> TAG_SHIFT;
   wire [LINE_SIZE_BITS:0] line_offset = addr & (LINE_SIZE - 1);

   // Break out fields of the current row.
   wire [LINE_BITS-1:0] line  [0:ASSOCIATIVITY-1];
   wire [TAG_BITS-1:0]  tag   [0:ASSOCIATIVITY-1];
   wire [AGE_BITS-1:0]  age   [0:ASSOCIATIVITY-1];
   wire                 dirty [0:ASSOCIATIVITY-1];
   wire                 valid [0:ASSOCIATIVITY-1];
   genvar i;
   generate
      for (i = 0; i < ASSOCIATIVITY; i = i + 1) begin
         localparam offset       = i * WAY_BITS;
         localparam line_start   = offset + LINE_OFFSET;
         localparam tag_start    = offset + TAG_OFFSET;
         localparam age_start    = offset + AGE_OFFSET;
         localparam dirty_start  = offset + DIRTY_OFFSET;
         localparam valid_start  = offset + VALID_OFFSET;
         assign line[i]          = row[line_start+LINE_BITS-1:line_start];
         assign tag[i]           = row[tag_start+TAG_BITS-1:tag_start];
         assign dirty[i]         = row[dirty_start];
         assign valid[i]         = row[valid_start];
         if (AGE_BITS > 0) begin
            assign age[i]  = row[age_start+AGE_BITS-1:age_start];
         end
      end
   endgenerate

   // Initialize the cache to zeros for simulation.
   integer init_i;
   initial begin
      for (init_i = 0; init_i < LINE_COUNT; init_i = init_i + 1) begin
         data[init_i] = 0;
      end
   end

   // Determine the line to evict if necessry and if we have a hit.
   reg [ADDR_WIDTH-1:0] oldest_addr;
   reg [ASSOC_BITS:0]   oldest_way;
   reg [LINE_BITS-1:0]  oldest_line;
   reg                  oldest_dirty;
   reg [AGE_BITS-1:0]   oldest_age;
   reg [ASSOC_BITS:0]   hit_way;
   reg [LINE_BITS-1:0]  hit_line;
   reg                  is_hit;
   reg [ASSOC_BITS:0]   wayi;
   always @(*) begin
      oldest_addr    <= {tag[0], current_index} << LINE_SIZE_BITS;
      oldest_way     <= 0;
      oldest_line    <= line[0];
      oldest_dirty   <= dirty[0];
      oldest_age     <= age[0];
      hit_way        <= 0;
      hit_line       <= line[0];
      is_hit         <= current_tag == tag[0] && valid[0];
      for (wayi = 1; wayi < ASSOCIATIVITY; wayi = wayi + 1) begin
         if (oldest_age < age[wayi]) begin
            oldest_age     <= age[wayi];
            oldest_way     <= wayi;
            oldest_dirty   <= dirty[wayi];
            oldest_line    <= line[wayi];
            oldest_addr    <= {tag[wayi], current_index} << LINE_SIZE_BITS;
         end
         if (current_tag == tag[wayi]) begin
            hit_way        <= wayi;
            is_hit         <= valid[wayi];
            hit_line       <= line[wayi];
         end
      end
   end

   localparam STATE_IDLE            = 0;  // Ready
   localparam STATE_READ            = 1;  // Start a read.
   localparam STATE_WRITE           = 2;  // Start a write.
   localparam STATE_READ_MISS       = 3;  // Read from memory
   localparam STATE_WRITE_FILL      = 4;  // Read from memory for a write
   localparam STATE_WRITEBACK_READ  = 5;  // Writeback for a read
   localparam STATE_WRITEBACK_WRITE = 6;  // Writeback for a write
   localparam STATE_BITS            = 3;

   reg [LINE_SIZE_BITS:0] transfer_count;
   reg [LINE_SIZE_BITS:0] next_transfer_count;
   wire transfer_done = mready && transfer_count == 0 && !mwe && !mre;

   // Determine the next state.
   reg [STATE_BITS-1:0] state;
   reg [STATE_BITS-1:0] next_state;
   wire is_idle = state == STATE_IDLE;
   always @(*) begin
      if (rst) begin
         next_state <= STATE_IDLE;
      end else begin
         next_state <= state;
         case (state)
            STATE_IDLE: // Idle (ready for a read/write).
               if (re) begin
                  next_state <= STATE_READ;
               end else if (we) begin
                  next_state <= STATE_WRITE;
               end
            STATE_READ: // Process a read.
                  if (is_hit) begin
                     next_state <= STATE_IDLE;
                  end else if (oldest_dirty) begin
                     next_state <= STATE_WRITEBACK_READ;
                  end else begin
                     next_state <= STATE_READ_MISS;
                  end
            STATE_WRITE: // Process a write.
                  if (is_hit) begin
                     next_state <= STATE_IDLE;
                  end else if (oldest_dirty) begin
                     next_state <= STATE_WRITEBACK_WRITE;
                  end else begin
                     next_state <= LINE_SIZE > 1 ? STATE_WRITE_FILL
                                                 : STATE_IDLE;
                  end
            STATE_READ_MISS: // Read miss; fill line.
               if (transfer_done) next_state <= STATE_IDLE;
            STATE_WRITE_FILL: // Write miss; fill line.
               if (transfer_done) next_state <= STATE_IDLE;
            STATE_WRITEBACK_READ: // Read miss; writeback dirty slot.
               if (transfer_done) next_state <= STATE_READ_MISS;
            STATE_WRITEBACK_WRITE: // Write miss; writeback dirty slot.
               if (transfer_done) next_state <= STATE_IDLE;
         endcase
      end
   end

   // Update the state.
   always @(posedge clk) begin
      if (rst)       state <= STATE_IDLE;
      else           state <= next_state;
   end

   // Update the transfer count.
   always @(*) begin
      if (transfer_done) begin
         next_transfer_count <= LINE_SIZE - 1;
      end else if (mready) begin
         next_transfer_count <= transfer_count - 1;
      end else begin
         next_transfer_count <= transfer_count;
      end
   end
   always @(posedge clk) begin
      if (rst) begin
         transfer_count <= 0;
      end else begin
         transfer_count <= next_transfer_count;
      end
   end

   // Build up a line used for cache accesses.
   wire load_mem = next_state == STATE_WRITEBACK_WRITE
                 | next_state == STATE_WRITEBACK_READ
                 | state == STATE_READ_MISS
                 | state == STATE_WRITE_FILL;
   wire write_line = state == STATE_WRITE
                   | state == STATE_WRITEBACK_WRITE
                   | state == STATE_WRITE_FILL;
   wire update_age = state == STATE_READ | state == STATE_WRITE;
   wire [ROW_BITS-1:0] updated_row;
   wire [ASSOC_BITS:0] write_way = is_hit ? hit_way : oldest_way;
   wire [LINE_SIZE_BITS:0] write_offset = load_mem
                                        ? transfer_count : line_offset;
   genvar row_i;
   genvar row_w;
   generate
      for (row_w = 0; row_w < ASSOCIATIVITY; row_w = row_w + 1) begin
         localparam offset       = row_w * WAY_BITS;
         localparam line_start   = offset + LINE_OFFSET;
         localparam tag_start    = offset + TAG_OFFSET;
         localparam age_start    = offset + AGE_OFFSET;
         localparam dirty_start  = offset + DIRTY_OFFSET;
         localparam valid_start  = offset + VALID_OFFSET;
         assign updated_row[tag_start+TAG_BITS-1:tag_start]
            = row_w == oldest_way ? current_tag : tag[row_w];
         assign updated_row[dirty_start]
            = row_w == oldest_way ? !load_mem : dirty[row_w];
         assign updated_row[valid_start] = 1;
         if (AGE_BITS > 0) begin
            assign updated_row[age_start+AGE_BITS-1:age_start]
               = update_age ? (row_w == oldest_way ? 0 : (age[row_w] < MAX_AGE
                                                       ? (age[row_w] + 1)
                                                       : MAX_AGE))
                            : age[row_w];
         end
         for (row_i = 0; row_i < LINE_SIZE; row_i = row_i + 1) begin
            localparam line_start = row_w * WAY_BITS + LINE_OFFSET;
            localparam word_start = line_start + row_i * WORD_WIDTH;
            assign updated_row[word_start+WORD_WIDTH-1:word_start]
               = (row_w == write_way && row_i == transfer_count && load_mem)
               ? min :
                  ((row_w == write_way && row_i == line_offset && write_line)
                  ? din : row[word_start+WORD_WIDTH-1:word_start]);
         end
      end
   endgenerate
   wire update_read  = state == STATE_READ_MISS && mready;
   wire update_write = (state == STATE_WRITE && !oldest_dirty)
                     | (state == STATE_WRITE_FILL && mready);
   wire update_row = update_read | update_write;
   always @(posedge clk) begin
      if (rst) begin
         row <= 0;
      end else if (state == STATE_IDLE && next_state != state) begin
         row <= data[current_index];
      end else if (update_row) begin
         row <= updated_row;
      end
   end

   // Update the cache.
   wire write_hit = state == STATE_WRITE && (is_hit || !oldest_dirty);
   wire write_ok  = (state == STATE_WRITEBACK_WRITE && transfer_done)
                  | (state == STATE_WRITE_FILL && mready);
   wire fill_ok   = state == STATE_READ_MISS && transfer_done;
   always @(posedge clk) begin
      if (!rst) begin
         if (write_hit | write_ok | fill_ok) begin
            data[current_index] <= updated_row;
         end
      end
   end

   // Drive main memory read/write.
   always @(posedge clk) begin
      if (rst) begin
         mre <= 0;
         mwe <= 0;
      end else begin
         mre <= 0;
         mwe <= 0;
         if (state != next_state || transfer_count > 0) begin
            case (next_state)
               STATE_WRITEBACK_READ:   mwe <= mready;
               STATE_WRITEBACK_WRITE:  mwe <= mready;
               STATE_READ_MISS:        mre <= mready;
               STATE_WRITE_FILL:
                  mre <= mready && next_transfer_count != line_offset;
            endcase
         end
      end
   end

   // Drive memory address.
   wire is_writeback = (next_state == STATE_WRITEBACK_READ)
                     | (next_state == STATE_WRITEBACK_WRITE);
   assign maddr = ((is_writeback ? oldest_addr : addr) & ADDR_MASK)
                | transfer_count;

   // Drive memory data.
   wire [WORD_WIDTH-1:0] oldest_words[0:LINE_SIZE-1];
   genvar memi;
   generate
      for (memi = 0; memi < LINE_SIZE; memi = memi + 1) begin
         localparam offset = memi * WORD_WIDTH;
         assign oldest_words[memi] = oldest_line[offset+WORD_WIDTH-1:offset];
      end
   endgenerate
   assign mout = oldest_words[transfer_count];

   // Drive the ready bit.
   assign ready = state == STATE_IDLE;

   // Drive dout.
   wire [WORD_WIDTH-1:0] words [0:LINE_SIZE-1];
   genvar wordi;
   generate
      for (wordi = 0; wordi < LINE_SIZE; wordi = wordi + 1) begin
         localparam offset = wordi * WORD_WIDTH;
         assign words[wordi] = hit_line[offset+WORD_WIDTH-1:offset];
      end
   endgenerate
   assign dout = words[line_offset];

endmodule
