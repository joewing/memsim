
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity split is
   generic (
      ADDR_WIDTH  : in natural := 64;
      WORD_WIDTH  : in natural := 64;
      BOFFSET     : in natural := 7
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
      maddr0   : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
      min0     : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
      mout0    : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      mre0     : out std_logic;
      mwe0     : out std_logic;
      mmask0   : out std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
      mready0  : in  std_logic;
      maddr1   : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
      mout1    : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      min1     : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
      mre1     : out std_logic;
      mwe1     : out std_logic;
      mmask1   : out std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
      mready1  : in  std_logic
   );
end split;

architecture split_arch of split is

   constant OFFSET   : natural := 2 ** BOFFSET;

   signal addr_slice : std_logic_vector(ADDR_WIDTH - 1 downto BOFFSET);
   signal bank0      : std_logic;
   signal bank1      : std_logic;

begin

   addr_slice  <= addr(ADDR_WIDTH - 1 downto BOFFSET);
   bank0       <= '1' when unsigned(addr_slice) = 0 else '0';
   bank1       <= not bank0;

   maddr0   <= addr;
   maddr1   <= std_logic_vector(unsigned(addr) - OFFSET);
   mout0    <= din;
   mout1    <= din;
   dout     <= min0 when bank0 = '1' else min1;
   mre0     <= re and bank0;
   mre1     <= re and bank1;
   mwe0     <= we and bank0;
   mwe1     <= we and bank1;
   mmask0   <= mask;
   mmask1   <= mask;
   ready    <= mready0 and mready1;

end split_arch;
