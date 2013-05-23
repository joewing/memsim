

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity combine is
   generic (
      ADDR_WIDTH  : in natural := 64;
      WORD_WIDTH  : in natural := 64;
      OFFSET      : in natural := 128
   );
   port (
      clk      : in  std_logic;
      rst      : in  std_logic;
      addr0    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      din0     : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
      dout0    : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      re0      : in  std_logic;
      we0      : in  std_logic;
      ready0   : out std_logic;
      addr1    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      din1     : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
      dout1    : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      re1      : in  std_logic;
      we1      : in  std_logic;
      ready1   : out std_logic;
      maddr    : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
      mout     : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      min      : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
      mre      : out std_logic;
      mwe      : out std_logic;
      mready   : in  std_logic
   );
end combine;

architecture combine_arch of combine is

   signal bank0 : std_logic;

begin

   bank0    <= '1' when re0 = '1' or we0 = '1' else '0';
   maddr    <= addr0 when bank0 = '1'
               else std_logic_vector(unsigned(addr1) + OFFSET);
   mout     <= din0 when bank0 = '1' else din1;
   dout0    <= min;
   dout1    <= min;
   mre      <= '1' when re0 = '1' or re1 = '1' else '0';
   mwe      <= '1' when we0 = '1' or we1 = '1' else '0';
   ready0   <= mready;
   ready1   <= mready;

end combine_arch;

