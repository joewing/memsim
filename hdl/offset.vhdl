
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity offset is
   generic (
      ADDR_WIDTH     : in natural := 32;
      WORD_WIDTH     : in natural := 32;
      VALUE          : in integer := 0
   );
   port (
      clk      : in  std_logic;
      rst      : in  std_logic;
      addr     : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      din      : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
      dout     : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      re       : in  std_logic;
      we       : in  std_logic;
      mask     : in  std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
      ready    : out std_logic;
      maddr    : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
      mout     : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      min      : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
      mre      : out std_logic;
      mwe      : out std_logic;
      mmask    : out std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
      mready   : in  std_logic
   );
end offset;

architecture offset_arch of offset is

   constant WORD_BYTES  : natural := WORD_WIDTH / 8;
   constant BOFFSET     : natural := VALUE mod WORD_BYTES;
   constant ZERO_MASK   : std_logic_vector(WORD_BYTES - 1 downto 0)
                           := (others => '0');

   signal shifted_mask  : std_logic_vector(2 * WORD_BYTES - 1 downto 0);
   signal maska         : std_logic_vector(WORD_BYTES - 1 downto 0);
   signal maskb         : std_logic_vector(WORD_BYTES - 1 downto 0);
   signal first_word    : std_logic;
   signal split_access  : std_logic;
   signal next_addr     : std_logic_vector(ADDR_WIDTH - 1 downto 0);
   signal rre           : std_logic;
   signal rwe           : std_logic;
   signal saved         : std_logic_vector(WORD_WIDTH - 1 downto 0);
   signal state         : natural;

begin

   -- Handle the simple case when the offset is a multiple of the word size.
   word_offset : if BOFFSET = 0 generate

      process(addr)
         variable woffset : signed(ADDR_WIDTH - 1 downto 0);
      begin
         woffset  := to_signed(VALUE / WORD_WIDTH, ADDR_WIDTH);
         maddr    <= std_logic_vector(signed(addr) + woffset);
      end process;

      dout     <= min;
      mout     <= din;
      mre      <= re;
      mwe      <= we;
      mmask    <= mask;
      ready    <= mready;

   end generate;

   -- Handle the case when the offset is not a multiple of the word size.
   -- This can cause a single access to turn into multiple accesses.
   byte_offset : if BOFFSET /= 0 generate

      -- Get the shifted mask used for byte offsets.
      process(mask)
      begin
         shifted_mask <= (others => '0');
         for b in 0 to WORD_BYTES - 1 loop
            shifted_mask(b + BOFFSET) <= mask(b);
         end loop;
      end process;
      maska <= shifted_mask(WORD_BYTES - 1 downto 0);
      maskb <= shifted_mask(2 * WORD_BYTES - 1 downto WORD_BYTES);

      -- State machine.
      process(clk)
      begin
         if clk'event and clk = '1' then
            if rst = '1' then
               state       <= 0;
            elsif state = 0 and (re = '1' or we = '1') then
               state <= 1;
            elsif state = 1 and mready = '1' then
               if split_access = '1' then
                  state <= 2;
               else
                  state <= 0;
               end if;
            elsif state = 2 and mready = '1' then
               state <= 0;
            end if;
         end if;
      end process;

      -- Determine if this is a split access.
      split_access <= '1' when maska /= ZERO_MASK and maskb /= ZERO_MASK
                      else '0';

      -- Determine if we should be accessing the first word.
      process(state, maska, mready)
      begin
         if state = 0 and maska /= ZERO_MASK then
            first_word <= '1';
         elsif state = 1 and maska /= ZERO_MASK and mready = '0' then
            first_word <= '1';
         else
            first_word <= '0';
         end if;
      end process;

      -- Drive maddr and mmask.
      process(addr, next_addr, maska, maskb, first_word)
         variable woffset     : signed(ADDR_WIDTH - 1 downto 0);
      begin
         woffset  := to_signed(VALUE / WORD_WIDTH, ADDR_WIDTH);
         if first_word = '1' then
            maddr <= std_logic_vector(signed(addr) + woffset);
            mmask <= maska;
         else
            maddr <= std_logic_vector(signed(next_addr) + woffset);
            mmask <= maskb;
         end if;
      end process;
      next_addr <= std_logic_vector(unsigned(addr) + 1);

      -- Drive mre and mwe.
      process(state, re, we, rre, rwe, mready, split_access)
      begin
         mre <= '0';
         mwe <= '0';
         if state = 0 then
            -- Start first access and save access type.
            mre <= re;
            mwe <= we;
            rre <= re;
            rwe <= we;
         elsif state = 1 and mready = '1' and split_access = '1' then
            -- Start second access.
            mre <= rre;
            mwe <= rwe;
         end if;
      end process;

      -- Drive ready.
      process(state, mready, maska, maskb)
      begin
         if state = 1 then
            if split_access = '0' then
               ready <= mready;
            else
               ready <= '0';
            end if;
         else
            ready <= mready;
         end if;
      end process;

      -- Drive mout.
      process(din, first_word)
         variable start       : integer;
      begin
         mout <= (others => 'Z');
         for b in 0 to WORD_BYTES - 1 loop
            if first_word = '1' then
               start := (b + BOFFSET) * 8;
               if start < WORD_WIDTH then
                  mout(start + 7 downto start) <= din(b * 8 + 7 downto b * 8);
               end if;
            else
               start := (b + BOFFSET) * 8 - WORD_WIDTH;
               if start >= 0 then
                  mout(start + 7 downto start) <= din(b * 8 + 7 downto b * 8);
               end if;
            end if;
         end loop;
      end process;

      -- Save the output of a read.
      -- This is used for driving dout.
      process(clk)
      begin
         if clk'event and clk = '1' and rst = '0' then
            if state = 1 and mready = '1' then
               saved <= min;
            end if;
         end if;
      end process;

      -- Drive dout.
      process(state, maska, min)
         variable start : integer;
      begin
         dout <= (others => 'Z');
         for b in 0 to WORD_BYTES - 1 loop
            if b >= WORD_BYTES - BOFFSET then
               start := (b + BOFFSET - WORD_BYTES) * 8;
               dout(b * 8 + 7 downto b * 8) <= min(start + 7 downto start);
            else
               start := (b + BOFFSET) * 8;
               dout(b * 8 + 7 downto b * 8) <= saved(start + 7 downto start);
            end if;
         end loop;
      end process;

   end generate;

end offset_arch;
