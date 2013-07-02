

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reg is
   generic (
      ADDR_WIDTH     : natural := 32;
      WORD_WIDTH     : natural := 32
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
end reg;

architecture reg_arch of reg is
begin

   process(clk)
   begin
      if clk'event and clk = '1' then
         dout  <= min;
         ready <= mready;
      end if;
   end process;

   maddr <= addr;
   mout  <= din;
   mmask <= mask;
   mre   <= re;
   mwe   <= we;

end reg_arch;
