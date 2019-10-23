
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cache is
   --Generic variables to be used later
   generic (
      ADDR_WIDTH        : in natural := 32;
      WORD_WIDTH        : in natural := 32;
      LINE_SIZE_BITS    : in natural := 0;
      LINE_COUNT_BITS   : in natural := 8;
      ASSOC_BITS        : in natural := 1;
      REPLACEMENT       : in natural := 0;
         -- 0: LRU
         -- 1: MRU
         -- 2: FIFO
         -- 3: PLRU
      WRITE_POLICY      : in natural := 0
         -- 0: write-back,    write-allocate
         -- 1: write-through, write-around
   );
   port (
      clk      : in  std_logic;
      rst      : in  std_logic;
      addr     : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);--31 downto 0 or 32 bits
      din      : in  std_logic_vector(WORD_WIDTH - 1 downto 0);--31 downto 0 or 32 bits
      dout     : out std_logic_vector(WORD_WIDTH - 1 downto 0);--31 downto 0 or 32 bits
      re       : in  std_logic;
      we       : in  std_logic;
      mask     : in  std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);--3 downto 0 or 4 bits
      ready    : out std_logic;
      maddr    : out std_logic_vector(ADDR_WIDTH - 1 downto 0);--31 downto 0 or 32 bits
      mout     : out std_logic_vector(WORD_WIDTH - 1 downto 0);--31 downto 0 or 32 bits
      min      : in  std_logic_vector(WORD_WIDTH - 1 downto 0);--31 downto 0 or 32 bits
      mre      : out std_logic;
      mwe      : out std_logic;
      mmask    : out std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
      mready   : in  std_logic
   );
end cache;

architecture cache_arch of cache is

   constant ASSOCIATIVITY  : natural := 2 ** ASSOC_BITS; --2^1(Assoc) = 2
   constant LINE_SIZE      : natural := 2 ** LINE_SIZE_BITS; --2^0(LineSizeBits) = 1 
   constant LINE_COUNT     : natural := 2 ** LINE_COUNT_BITS;--2^8(LineCountBits) = 256
   constant ROW_COUNT      : natural := LINE_COUNT;--256
   constant INDEX_BITS     : natural := LINE_COUNT_BITS;--8
   constant TAG_BITS       : natural := ADDR_WIDTH - INDEX_BITS-- = 32-8-1 = 23
                                        - LINE_SIZE_BITS;

   function get_dirty_bits return natural is
   begin
      case WRITE_POLICY is
         when 0      =>    -- write back or write allocate
            return ASSOCIATIVITY; --2^(Assoc_Bits or 1) = 2
         when others =>  -- if not then write through or write around 
            return 0;
      end case;
   end get_dirty_bits;

   function get_age_bits return natural is
   begin
      case REPLACEMENT is -- 0-Least recently used,1-Most recently used and 2-first in first out
         when 3 =>   -- PRLU requires 1 bit per way
            return 1;
         when others =>
            return ASSOC_BITS; -- 1
      end case;
   end get_age_bits;

   constant AGE_BITS       : natural := get_age_bits;
   constant LINE_BITS      : natural := WORD_WIDTH * LINE_SIZE; -- 32*1= 32

   function get_way_bits return natural is
   begin
      case WRITE_POLICY is
         when 0         => return LINE_BITS + TAG_BITS + AGE_BITS + 2;--write back or write allocate then  32 +23+1+2= 58
         when others    => return LINE_BITS + TAG_BITS + AGE_BITS + 1;-- write through or write around then  32 +23+1+1=57
      end case;
   end get_way_bits;

   constant WAY_BITS       : natural := get_way_bits;-- 58 or 57 dependant on Write_Policy
   constant ROW_BITS       : natural := WAY_BITS * ASSOCIATIVITY;--(above)*2^(Assoc_Bits or 1)
   constant DIRTY_BITS     : natural := get_dirty_bits;
   constant LINE_OFFSET    : natural := 0;
   constant TAG_OFFSET     : natural := LINE_OFFSET + LINE_BITS;
   constant AGE_OFFSET     : natural := TAG_OFFSET + TAG_BITS;
   constant VALID_OFFSET   : natural := AGE_OFFSET + AGE_BITS;
   constant DIRTY_OFFSET   : natural := VALID_OFFSET + 1;
   constant MASK_BITS      : natural := ADDR_WIDTH - LINE_SIZE_BITS;
   constant TAG_SHIFT      : natural := ADDR_WIDTH - TAG_BITS;
   constant OFFSET_BOTTOM  : natural := 0;
   constant OFFSET_TOP     : integer := OFFSET_BOTTOM + LINE_SIZE_BITS - 1;
   constant INDEX_BOTTOM   : natural := OFFSET_TOP + 1;
   constant INDEX_TOP      : integer := INDEX_BOTTOM + INDEX_BITS - 1;
   constant TAG_BOTTOM     : natural := INDEX_TOP + 1;
   constant TAG_TOP        : natural := ADDR_WIDTH - 1;

   type state_type is (
      STATE_IDLE,
      STATE_READ1,
      STATE_READ2,
      STATE_WRITE1,
      STATE_WRITE2,
      STATE_READ_MISS1,
      STATE_READ_MISS2,
      STATE_WRITE_FILL1,
      STATE_WRITE_FILL2,
      STATE_WRITEBACK_READ1,
      STATE_WRITEBACK_READ2,
      STATE_WRITEBACK_WRITE1,
      STATE_WRITEBACK_WRITE2
   );

   subtype line_type is std_logic_vector(LINE_BITS - 1 downto 0);
   subtype tag_type is std_logic_vector(TAG_BITS - 1 downto 0);
   subtype age_type is std_logic_vector(AGE_BITS downto 0);
   subtype row_type is std_logic_vector(ROW_BITS - 1 downto 0);
   subtype word_type is std_logic_vector(WORD_WIDTH - 1 downto 0);
   subtype way_type is std_logic_vector(ASSOC_BITS downto 0);
   subtype offset_type is std_logic_vector(LINE_SIZE_BITS - 1 downto 0);
   subtype transfer_type is std_logic_vector(LINE_SIZE_BITS downto 0);
   subtype mask_type is std_logic_vector(WORD_WIDTH / 8 - 1 downto 0);

   type line_array_type is array(0 to ASSOCIATIVITY - 1) of line_type;
   type tag_array_type is array(0 to ASSOCIATIVITY - 1) of tag_type;
   type age_array_type is array(0 to ASSOCIATIVITY - 1) of age_type;
   type row_array_type is array(0 to ROW_COUNT - 1) of row_type;
   type word_array_type is array(0 to LINE_SIZE - 1) of word_type;

   constant ZERO_OFFSET    : offset_type  := (others => '0');
   constant FULL_MASK      : mask_type    := (others => '1');

   signal data          : row_array_type := (others => (others => '0'));
   signal row           : row_type := (others => '0');
   signal updated_row   : row_type;
   signal updated_ages  : age_array_type;

   signal next_state : state_type;
   signal state      : state_type;

   signal next_transfer_count    : transfer_type;
   signal updated_transfer_count : transfer_type;
   signal transfer_count         : transfer_type;
   signal transfer_done          : std_logic;

   signal lines   : line_array_type;
   signal tags    : tag_array_type;
   signal words   : word_array_type;
   signal dirty   : std_logic_vector(DIRTY_BITS - 1 downto 0);
   signal valid   : std_logic_vector(ASSOCIATIVITY - 1 downto 0);
   signal ages    : age_array_type;

   signal current_offset   : std_logic_vector(LINE_SIZE_BITS downto 0);
   signal current_index    : std_logic_vector(INDEX_BITS - 1 downto 0);
   signal current_tag      : std_logic_vector(TAG_BITS - 1 downto 0);
   signal rindex           : natural;

   signal oldest_addr      : std_logic_vector(ADDR_WIDTH - 1 downto 0);
   signal oldest_way       : way_type;
   signal oldest_line      : line_type;
   signal oldest_dirty     : std_logic;
   signal oldest_age       : age_type;
   signal oldest_words     : word_array_type;
   signal hit_way          : way_type;
   signal hit_line         : line_type;
   signal hit_age          : age_type;
   signal is_hit           : std_logic;
   signal was_hit          : std_logic;

   signal rmin             : std_logic_vector(WORD_WIDTH - 1 downto 0);
   signal mre_temp         : std_logic;
   signal mwe_temp         : std_logic;

begin

   -- Break out fields in the current row.
   process(row)
      variable offset      : natural;
      variable line_start  : natural;
      variable tag_start   : natural;
      variable age_start   : natural;
   begin
      for i in 0 to ASSOCIATIVITY - 1 loop
         offset      := i * WAY_BITS;
         line_start  := offset + LINE_OFFSET;
         tag_start   := offset + TAG_OFFSET;
         age_start   := offset + AGE_OFFSET;
         lines(i) <= row(line_start + LINE_BITS - 1 downto line_start);
         tags(i)  <= row(tag_start + TAG_BITS - 1 downto tag_start);
         if AGE_BITS > 0 then
            ages(i) <= "0" & row(age_start + AGE_BITS - 1 downto age_start);
         end if;
         if DIRTY_BITS > 0 then
            dirty(i) <= row(offset + DIRTY_OFFSET);
         end if;
         valid(i) <= row(offset + VALID_OFFSET);
      end loop;
   end process;

   -- Determine the oldest line and if we have a hit.
   process(tags, dirty, lines, ages, valid, current_index, current_tag)
      variable temp_age    : age_type;
      variable is_oldest   : boolean;
   begin
      oldest_addr    <= tags(0) & current_index & ZERO_OFFSET;
      oldest_way     <= (others => '0');
      if DIRTY_BITS > 0 then
         oldest_dirty   <= dirty(0);
      end if;
      oldest_line    <= lines(0);
      oldest_age     <= ages(0);
      temp_age       := ages(0);
      hit_way        <= (others => '0');
      hit_line       <= lines(0);
      hit_age        <= ages(0);
      if TAG_BITS > 0 then
         if current_tag = tags(0) then
            is_hit      <= valid(0);
         else
            is_hit      <= '0';
         end if;
      else
         is_hit <= valid(0);
      end if;
      for i in 1 to ASSOCIATIVITY - 1 loop
         case REPLACEMENT is
            when 0      => -- LRU
               is_oldest := unsigned(temp_age) < unsigned(ages(i));
            when 1      => -- MRU
               is_oldest := unsigned(temp_age) > unsigned(ages(i));
            when 2      => -- FIFO
               is_oldest := unsigned(temp_age) < unsigned(ages(i));
            when 3      => -- PLRU
               is_oldest := ages(i) = "00";
            when others =>
               report "invalid replacement policy" severity failure;
         end case;
         if is_oldest or valid(i) /= '1' then
            oldest_addr    <= tags(i) & current_index & ZERO_OFFSET;
            oldest_way     <= std_logic_vector(to_unsigned(i, ASSOC_BITS + 1));
            if DIRTY_BITS > 0 then
               oldest_dirty   <= dirty(i);
            end if;
            oldest_line    <= lines(i);
            oldest_age     <= ages(i);
            temp_age       := ages(i);
         end if;
         if current_tag = tags(i) and valid(i) = '1' then
            hit_way  <= std_logic_vector(to_unsigned(i, ASSOC_BITS + 1));
            hit_line <= lines(i);
            hit_age  <= ages(i);
            is_hit   <= '1';
         end if;
      end loop;
   end process;

   -- Determine the next state.
   process(state, re, we, is_hit, oldest_dirty, transfer_done, mready)
   begin
      next_state <= state;
      case state is
         when STATE_IDLE =>
            if re = '1' then
               next_state <= STATE_READ1;
            elsif we = '1' then
               next_state <= STATE_WRITE1;
            end if;
         when STATE_READ1 =>
            next_state <= STATE_READ2;
         when STATE_READ2 =>
            if WRITE_POLICY = 0 then 
               if is_hit = '1' then
                  next_state <= STATE_IDLE;
               elsif DIRTY_BITS > 0 and oldest_dirty = '1' then
                  next_state <= STATE_WRITEBACK_READ1;
               else
                  next_state <= STATE_READ_MISS1;
               end if;
            elsif WRITE_POLICY = 1 then
               if is_hit = '1' then
                  next_state <= STATE_IDLE;
               else
                  next_state <= STATE_READ_MISS1;
               end if;
            else
               report "invalid write policy" severity failure;
            end if;
         when STATE_WRITE1 =>
            next_state <= STATE_WRITE2;
         when STATE_WRITE2 =>
            if WRITE_POLICY = 0 then
               if is_hit = '1' then
                  next_state <= STATE_IDLE;
               elsif oldest_dirty = '1' then
                  next_state           <= STATE_WRITEBACK_WRITE1;
               elsif LINE_SIZE > 1 or mask /= FULL_MASK then
                  next_state           <= STATE_WRITE_FILL1;
               else
                  next_state <= STATE_IDLE;
               end if;
            elsif WRITE_POLICY = 1 then
               if mready = '1' then
                  next_state <= STATE_IDLE;
               end if;
            else
               report "invalid write policy" severity failure;
            end if;
         when STATE_READ_MISS1 =>
            next_state <= STATE_READ_MISS2;
         when STATE_READ_MISS2 =>
            if transfer_done = '1' and mready = '1' then
               next_state <= STATE_IDLE;
            elsif mready = '1' then
               next_state <= STATE_READ_MISS1;
            end if;
         when STATE_WRITE_FILL1 =>
            next_state <= STATE_WRITE_FILL2;
         when STATE_WRITE_FILL2 =>
            if transfer_done = '1' and mready = '1' then
               next_state <= STATE_IDLE;
            elsif mready = '1' then
               next_state <= STATE_WRITE_FILL1;
            end if;
         when STATE_WRITEBACK_READ1 =>
            next_state <= STATE_WRITEBACK_READ2;
         when STATE_WRITEBACK_READ2 =>
            if transfer_done = '1' and mready = '1' then
               next_state           <= STATE_READ_MISS1;
            elsif mready = '1' then
               next_state           <= STATE_WRITEBACK_READ1;
            end if;
         when STATE_WRITEBACK_WRITE1 =>
            next_state <= STATE_WRITEBACK_WRITE2;
         when STATE_WRITEBACK_WRITE2 =>
            if mready = '1' then
               if transfer_done = '1' and mready = '1' then
                  if LINE_SIZE > 1 or mask /= FULL_MASK then
                     next_state <= STATE_WRITE_FILL1;
                  else
                     next_state <= STATE_IDLE;
                  end if;
               else
                  next_state <= STATE_WRITEBACK_WRITE1;
               end if;
            end if;
      end case;
   end process;

   -- Determine the next transfer count.
   process(state, transfer_count, updated_transfer_count,
           mready, transfer_done)
   begin
      next_transfer_count <= transfer_count;
      case state is
         when STATE_READ2 =>
            next_transfer_count <= (others => '0');
         when STATE_WRITE2 =>
            if WRITE_POLICY = 0 then
               next_transfer_count <= (others => '0');
            end if;
         when STATE_READ_MISS2 =>
            if mready = '1' then
               next_transfer_count <= updated_transfer_count;
            end if;
         when STATE_WRITE_FILL2 =>
            if mready = '1' then
               next_transfer_count <= updated_transfer_count;
            end if;
         when STATE_WRITEBACK_READ2 =>
            if mready = '1' then
               if transfer_done = '1' then
                  next_transfer_count <= (others => '0');
               else
                  next_transfer_count <= updated_transfer_count;
               end if;
            end if;
         when STATE_WRITEBACK_WRITE2 =>
            if mready = '1' then
               if transfer_done = '1' then
                  next_transfer_count <= (others => '0');
               else
                  next_transfer_count <= updated_transfer_count;
               end if;
            end if;
         when others =>
            null;
      end case;
   end process;

   -- Update the state and transfer count.
   process(clk)
   begin
      if clk'event and  clk = '1' then
         if rst = '1' then
            state          <= STATE_IDLE;
            transfer_count <= (others => '0');
         else
            state          <= next_state;
            transfer_count <= next_transfer_count;
         end if;
      end if;
   end process;

   -- Drive transfer_done and inc_transfer_count.
   process(transfer_count, state, current_offset, mask)
      variable upd      : transfer_type;
      variable inc1     : transfer_type;
      variable inc2     : transfer_type;
   begin
      inc1 := std_logic_vector(unsigned(transfer_count) + 1);
      inc2 := std_logic_vector(unsigned(transfer_count) + 2);
      case state is
         when STATE_WRITE_FILL1 | STATE_WRITE_FILL2 =>
            if inc1 = current_offset and mask = FULL_MASK then
               upd := inc2;
            else
               upd := inc1;
            end if;
         when others =>
            upd := inc1;
      end case;
      updated_transfer_count <= upd;
      if unsigned(upd) = LINE_SIZE then
         transfer_done <= '1';
      else
         transfer_done <= '0';
      end if;
   end process;

   -- Update the current row.
   process(hit_way, oldest_way, state, current_tag, valid,
           dirty, tags, updated_ages, ages, min, din, current_offset,
           transfer_count, row, is_hit, mask)
      variable offset      : natural;
      variable line_top    : natural;
      variable line_bottom : natural;
      variable tag_top     : natural;
      variable tag_bottom  : natural;
      variable age_top     : natural;
      variable age_bottom  : natural;
      variable dirty_start : natural;
      variable valid_start : natural;
      variable word_top    : natural;
      variable word_bottom : natural;
      variable write_way   : way_type;
      variable load_mem    : boolean;
      variable write_line  : boolean;
      variable byte_top    : natural;
      variable byte_bottom : natural;
   begin
      if is_hit = '1' then
         write_way := hit_way;
      else
         write_way := oldest_way;
      end if;
      load_mem := state = STATE_WRITEBACK_WRITE1
               or state = STATE_WRITEBACK_READ1
               or state = STATE_READ_MISS2
               or state = STATE_WRITE_FILL2;
      if WRITE_POLICY = 0 then
         write_line :=  state = STATE_WRITE2
                     or state = STATE_WRITEBACK_WRITE2
                     or state = STATE_WRITE_FILL2;
      elsif WRITE_POLICY = 1 then
         write_line := (state = STATE_WRITE2 and is_hit = '1')
                     or state = STATE_WRITEBACK_WRITE2
                     or state = STATE_WRITE_FILL2;
      else
         report "invalid write policy" severity failure;
      end if;
      updated_row <= row;
      for way in 0 to ASSOCIATIVITY - 1 loop
         offset      := way * WAY_BITS;
         line_bottom := offset + LINE_OFFSET;
         line_top    := line_bottom + LINE_BITS - 1;
         tag_bottom  := offset + TAG_OFFSET;
         tag_top     := tag_bottom + TAG_BITS - 1;
         age_bottom  := offset + AGE_OFFSET;
         age_top     := age_bottom + AGE_BITS - 1;
         dirty_start := offset + DIRTY_OFFSET;
         valid_start := offset + VALID_OFFSET;
         if way = unsigned(write_way) then
            if DIRTY_BITS > 0 then
               case state is
                  when STATE_WRITE2 |
                       STATE_WRITEBACK_WRITE2 |
                       STATE_WRITE_FILL2 =>
                     updated_row(dirty_start) <= '1';
                  when STATE_READ_MISS2 =>
                     updated_row(dirty_start) <= '0';
                  when others =>
                     null;
               end case;
            end if;
            if load_mem or write_line then
               updated_row(tag_top downto tag_bottom) <= current_tag;
            end if;
            if load_mem or write_line then
               updated_row(valid_start) <= '1';
            end if;
         end if;
         if AGE_BITS > 0 then
            if state = STATE_READ2 or state = STATE_WRITE2 then
               updated_row(age_top downto age_bottom)
                  <= updated_ages(way)(AGE_BITS - 1 downto 0);
            end if;
         end if;
         for i in 0 to LINE_SIZE - 1 loop
            word_bottom := line_bottom + i * WORD_WIDTH;
            word_top    := word_bottom + WORD_WIDTH - 1;
            if unsigned(write_way) = way then
               if unsigned(transfer_count) = i and load_mem then
                  updated_row(word_top downto word_bottom) <= min;
               end if;
               if unsigned(current_offset) = i and write_line then
                  for b in 0 to (WORD_WIDTH / 8) - 1 loop
                     byte_bottom := word_bottom + b * 8;
                     byte_top    := byte_bottom + 7;
                     if mask(b) = '1' then
                        updated_row(byte_top downto byte_bottom) <=
                           din(b * 8 + 7 downto b * 8);
                     end if;
                  end loop;
               end if;
            end if;
         end loop;
      end loop;
   end process;

   -- Update ages.
   process(oldest_age, ages, hit_way, was_hit, hit_age, oldest_way)
      variable reset_bits : boolean;
   begin
      if REPLACEMENT = 3 then
         reset_bits := true;
         for i in 0 to ASSOCIATIVITY - 1 loop
            if ages(i)(0) = '0' then
               if was_hit = '1' and unsigned(hit_way) /= i then
                  reset_bits := false;
               elsif was_hit = '0' and unsigned(oldest_way) /= i then
                  reset_bits := false;
               end if;
            end if;
         end loop;
      end if;
      for i in 0 to ASSOCIATIVITY - 1 loop
         updated_ages(i) <= ages(i);
         if REPLACEMENT = 0 then -- LRU
            if was_hit = '1' then
               if i = unsigned(hit_way) then
                  updated_ages(i) <= (others => '0');
               elsif unsigned(ages(i)) <= unsigned(hit_age) then
                  updated_ages(i) <= std_logic_vector(unsigned(ages(i)) + 1);
               end if;
            else
               if i = unsigned(oldest_way) then
                  updated_ages(i) <= (others => '0');
               elsif unsigned(ages(i)) <= unsigned(oldest_age) then
                  updated_ages(i) <= std_logic_vector(unsigned(ages(i)) + 1);
               end if;
            end if;
         elsif REPLACEMENT = 1 then -- MRU
            if was_hit = '1' then
               if i = unsigned(hit_way) then
                  updated_ages(i) <= (others => '0');
               elsif unsigned(ages(i)) <= unsigned(hit_age) then
                  updated_ages(i) <= std_logic_vector(unsigned(ages(i)) + 1);
               end if;
            end if;
         elsif REPLACEMENT = 2 then -- FIFO
            if was_hit = '0' then
               if unsigned(oldest_way) = i then
                  updated_ages(i) <= (others => '0');
               else
                  updated_ages(i) <= std_logic_vector(unsigned(ages(i)) + 1);
               end if;
            end if;
         elsif REPLACEMENT = 3 then -- PRLU
            if was_hit = '1' then
               if unsigned(hit_way) = i then
                  updated_ages(i) <= "11";
               elsif reset_bits then
                  updated_ages(i) <= "00";
               end if;
            else
               if unsigned(oldest_way) = i then
                  updated_ages(i) <= "11";
               elsif reset_bits then
                  updated_ages(i) <= "00";
               end if;
            end if;
         else
            report "unimplemented policy" severity failure;
         end if;
      end loop;
   end process;

   -- Update the cache.
   process(clk) is
      variable write_hit   : boolean;
      variable write_ok    : boolean;
      variable fill_ok     : boolean;
   begin
      if clk'event and clk = '1' then

         -- Note that rindex is registered in STATE_IDLE.
         case state is
            when STATE_READ1 | STATE_WRITE1 =>
               row <= data(rindex);
            when STATE_WRITE2 =>
               if is_hit = '1' then
                  row <= updated_row;
               end if;
            when STATE_READ2 =>
               if is_hit = '1' then
                  row <= updated_row;
               end if;
            when STATE_WRITEBACK_WRITE2 =>
               if transfer_done = '1' then
                  row <= updated_row;
               end if;
            when STATE_WRITE_FILL2 =>
               row <= updated_row;
            when STATE_READ_MISS2 =>
               row <= updated_row;
            when others =>
               null;
         end case;

         -- We cannot write updated_row to data in STATE_IDLE
         -- since rindex will be invalid.
         if state /= STATE_IDLE then
            data(rindex) <= updated_row;
         end if;

      end if;
   end process;

   -- Drive main memory read/write.
   process(next_state)
   begin
      mwe_temp <= '0';
      mre_temp <= '0';
      case next_state is
         when STATE_WRITE1             =>
            if WRITE_POLICY = 1 then
               mwe_temp <= '1';
            end if;
         when STATE_WRITEBACK_READ1    => mwe_temp <= '1';
         when STATE_WRITEBACK_WRITE1   => mwe_temp <= '1';
         when STATE_READ_MISS1         => mre_temp <= '1';
         when STATE_WRITE_FILL1        => mre_temp <= '1';
         when others                   => null;
      end case;
   end process;
   mre   <= mre_temp;
   mwe   <= mwe_temp;
   mmask <= (others => '1') when WRITE_POLICY = 0 else mask;

   -- Drive memory address.
   process(next_transfer_count, oldest_addr, addr, next_state)
      subtype high_bits_type is
         std_logic_vector(ADDR_WIDTH - 1 downto LINE_SIZE_BITS);
      variable low_bits       : offset_type;
      variable oldest_high    : high_bits_type;
      variable current_high   : high_bits_type;
   begin
      oldest_high    := oldest_addr(ADDR_WIDTH - 1 downto LINE_SIZE_BITS);
      current_high   := addr(ADDR_WIDTH - 1 downto  LINE_SIZE_BITS);
      if LINE_SIZE > 1 then
         low_bits := next_transfer_count(LINE_SIZE_BITS - 1 downto 0);
         if WRITE_POLICY = 0 then
            case next_state is
               when STATE_WRITEBACK_READ1    | STATE_WRITEBACK_READ2 |
                    STATE_WRITEBACK_WRITE1   | STATE_WRITEBACK_WRITE2 =>
                  maddr <= oldest_high & low_bits;
               when others =>
                  maddr <= current_high & low_bits;
            end case;
         else
            case next_state is
               when STATE_WRITE1             | STATE_WRITE2 =>
                  maddr <= addr;
               when others =>
                  maddr <= current_high & low_bits;
            end case;
         end if;
      else
         if WRITE_POLICY = 0 then
            case next_state is
               when STATE_WRITEBACK_READ1    | STATE_WRITEBACK_READ2 |
                    STATE_WRITEBACK_WRITE1   | STATE_WRITEBACK_WRITE2 =>
                  maddr <= oldest_high;
               when others =>
                  maddr <= current_high;
            end case;
         else
            maddr <= current_high;
         end if;
      end if;
   end process;

   -- Break out words of the oldest and hit lines.
   process(oldest_line, hit_line)
      variable start : natural;
      variable stop  : natural;
   begin
      for w in 0 to LINE_SIZE - 1 loop
         stop  := w * WORD_WIDTH;
         start := stop + WORD_WIDTH - 1;
         oldest_words(w)   <= oldest_line(start downto stop);
         words(w)          <= hit_line(start downto stop);
      end loop;
   end process;

   -- Drive memory data (mout).
   drive_mout1 : if WRITE_POLICY = 0 and LINE_SIZE = 1 generate
      mout <= oldest_words(0);
   end generate;
   drive_moutn : if WRITE_POLICY = 0 and LINE_SIZE > 1 generate
      mout <= oldest_words(to_integer(unsigned(
                           next_transfer_count(LINE_SIZE_BITS - 1 downto 0))));
   end generate;
   drive_mout_wt : if WRITE_POLICY = 1 generate
      mout <= din;
   end generate;

   -- Drive dout.
   process(state, rmin, words, current_offset)
   begin
      if LINE_SIZE = 1 then
         if state = STATE_READ_MISS2 then
            dout <= rmin;
         else
            dout <= words(0);
         end if;
      elsif state = STATE_READ_MISS2
         and unsigned(current_offset) = LINE_SIZE - 1 then
         dout <= rmin;
      else
         dout <= words(to_integer(unsigned(current_offset)));
      end if;
   end process;

   -- Drive the ready bit.
   ready <= '1' when state = STATE_IDLE else '0';

   process(clk)
   begin
      if clk'event and clk = '1' then
         if INDEX_BITS > 0 then
            current_index <= addr(INDEX_TOP downto INDEX_BOTTOM);
            rindex <= to_integer(unsigned(addr(INDEX_TOP downto INDEX_BOTTOM)));
         end if;
         if TAG_BITS > 0 then
            current_tag <= addr(TAG_TOP downto TAG_BOTTOM);
         end if;
         if OFFSET_TOP >= 0 then
            current_offset <= "0" & addr(OFFSET_TOP downto OFFSET_BOTTOM);
         else
            current_offset <= "0";
         end if;
         was_hit <= is_hit;
         rmin <= min;
      end if;
   end process;

end cache_arch;
