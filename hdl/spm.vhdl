
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity spm is
   generic (
      ADDR_WIDTH  : in natural := 64;
      WORD_WIDTH  : in natural := 64;
      SIZE_BITS   : in natural := 7
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
end spm;

architecture spm_arch of spm is

   constant WORD_BYTES  : natural := WORD_WIDTH / 8;
   constant SIZE        : natural := 2 ** SIZE_BITS;

   subtype byte_type is std_logic_vector(7 downto 0);
   type byte_array_type is array(0 to SIZE - 1) of byte_type;
   type word_array_type is array(0 to WORD_BYTES - 1) of byte_array_type;

   signal data       : word_array_type;
   signal addr_slice : std_logic_vector(ADDR_WIDTH - 1 downto SIZE_BITS);
   signal is_hit     : std_logic;
   signal value      : std_logic_vector(WORD_WIDTH - 1 downto 0);
   signal raddr      : natural;
   signal rin        : std_logic_vector(WORD_WIDTH - 1 downto 0);
   signal rre        : std_logic;
   signal rwe        : std_logic;
   signal rmask      : std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
   signal busy       : std_logic;
   signal was_hit    : std_logic;

begin

   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '0' then
            if rwe = '1' then
               for b in 0 to WORD_BYTES - 1 loop
                  if rmask(b) = '1' then
                     data(b)(raddr) <= rin(b * 8 + 7 downto b * 8);
                  end if;
               end loop;
            elsif rre = '1' then
               for b in 0 to WORD_BYTES - 1 loop
                  value(b * 8 + 7 downto b * 8) <= data(b)(raddr);
               end loop;
            end if;
         end if;
      end if;
   end process;

   addr_slice  <= addr(ADDR_WIDTH - 1 downto SIZE_BITS);
   is_hit      <= '1' when unsigned(addr_slice) = 0 else '0';

   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            rre      <= '0';
            rwe      <= '0';
            busy     <= '0';
            was_hit  <= '0';
         else
            raddr    <= to_integer(unsigned(addr));
            rin      <= din;
            rmask    <= mask;
            rre      <= is_hit and re;
            rwe      <= is_hit and we;
            busy     <= re or we;
            was_hit  <= is_hit;
         end if;
      end if;
   end process;

   maddr <= addr;
   mre   <= re and not is_hit;
   mwe   <= we and not is_hit;
   mmask <= mask;
   dout  <= value when was_hit = '1' else min;
   mout  <= din;
   ready <= mready and not busy;

end spm_arch;
