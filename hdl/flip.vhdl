
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity flip is
   generic (
      ADDR_WIDTH  : natural := 32;
      WORD_WIDTH  : natural := 32;
      VALUE       : natural := 0
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
end flip;

architecture flip_arch of flip is
begin

   process(addr)
   begin
      for i in 0 to ADDR_WIDTH - 1 loop
         maddr(i) <= addr(ADDR_WIDTH - i - 1);
      end loop;
   end process;

   mout  <= din;
   dout  <= min;
   mre   <= re;
   mwe   <= we;
   mmask <= mask;
   ready <= mready;

end flip_arch;
