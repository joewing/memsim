
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity shift is
   generic (
      ADDR_WIDTH     : in natural := 32;
      WORD_WIDTH     : in natural := 32;
      SHIFT          : in integer := 0
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
end shift;

architecture shift_arch of shift is
begin

   shift_positive : if SHIFT >= 0 generate
      maddr <= std_logic_vector(rotate_left(unsigned(addr), SHIFT));
   end generate;
   shift_negative : if SHIFT < 0 generate
      maddr <= std_logic_vector(rotate_right(unsigned(addr), -SHIFT));
   end generate;

   mout  <= din;
   dout  <= min;
   mre   <= re;
   mwe   <= we;
   mmask <= mask;
   ready <= mready;

end shift_arch;
