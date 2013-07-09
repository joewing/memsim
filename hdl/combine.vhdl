

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity combine is
   generic (
      ADDR_WIDTH  : in natural := 32;
      WORD_WIDTH  : in natural := 64;
      BOFFSET     : in natural := 7
   );
   port (
      clk      : in  std_logic;
      rst      : in  std_logic;
      addr0    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      din0     : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
      dout0    : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      re0      : in  std_logic;
      we0      : in  std_logic;
      mask0    : in  std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
      ready0   : out std_logic;
      addr1    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      din1     : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
      dout1    : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      re1      : in  std_logic;
      we1      : in  std_logic;
      mask1    : in  std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
      ready1   : out std_logic;
      maddr    : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
      mout     : out std_logic_vector(WORD_WIDTH - 1 downto 0);
      min      : in  std_logic_vector(WORD_WIDTH - 1 downto 0);
      mre      : out std_logic;
      mwe      : out std_logic;
      mmask    : out std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);
      mready   : in  std_logic
   );
end combine;

architecture combine_arch of combine is

   signal rbank0  : std_logic;
   signal bank0   : std_logic;
   signal b1_addr : std_logic_vector(ADDR_WIDTH - 1 downto 0);

begin

   process(clk)
   begin
      if clk'event and clk = '1' then
         if (re0 or we0) = '1' then
            rbank0 <= '1';
         elsif (re1 or we1) = '1' then
            rbank0 <= '0';
         end if;
      end if;
   end process;

   b1_addr(ADDR_WIDTH - 1 downto BOFFSET) <=
      std_logic_vector(unsigned(addr1(ADDR_WIDTH - 1 downto BOFFSET)) + 1);
   b1_addr(BOFFSET - 1 downto 0) <= addr1(BOFFSET - 1 downto 0);

   bank0    <= re0 or we0 or bank0;
   maddr    <= addr0 when bank0 = '1' else b1_addr;
   mout     <= din0 when bank0 = '1' else din1;
   dout0    <= min;
   dout1    <= min;
   mre      <= re0 or re1;
   mwe      <= we0 or we1;
   mmask    <= mask0 when bank0 = '1' else mask1;
   ready0   <= mready;
   ready1   <= mready;

end combine_arch;

