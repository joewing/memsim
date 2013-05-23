
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity spm is
   generic (
      ADDR_WIDTH  : in natural := 64;
      WORD_WIDTH  : in natural := 64;
      SIZE        : in natural := 128
   );
   port (
      clk      : in std_logic;
      rst      : in std_logic;
      addr     : in std_logic_vector(ADDR_WIDTH - 1 downto 0);
      din      : in std_logic_vector(WORD_WIDTH - 1 downto 0);
      dout     : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      re       : in std_logic;
      we       : in std_logic;
      ready    : out std_logic;
      maddr    : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
      mout     : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      min      : in std_logic_vector(WORD_WIDTH - 1 downto 0);
      mre      : out std_logic;
      mwe      : out std_logic;
      mready   : in std_logic
   );
end spm;

architecture spm_arch of spm is

   subtype word_type is std_logic_vector(WORD_WIDTH - 1 downto 0);
   type word_array_type is array(0 to  SIZE - 1) of word_type;

   signal data    : word_array_type;
   signal value   : word_type;

   signal is_hit : std_logic;

begin

   is_hit <= '1' when unsigned(addr) < SIZE else '0';

   process(clk)
   begin
      if clk'event and clk = '1' and rst = '0' then
         if is_hit = '1' and re = '1' then
            value <= data(to_integer(unsigned(addr)));
         elsif is_hit = '1' and we = '1' then
            data(to_integer(unsigned(addr))) <= din;
         end if;
      end if;
   end process;

   maddr <= addr;
   mre   <= '1' when is_hit = '0' and re = '1' else '0';
   mwe   <= '1' when is_hit = '0' and we = '1' else '0';
   dout  <= value when is_hit = '1' else min;
   mout  <= din;
   ready <= mready;

end spm_arch;
