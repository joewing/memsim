
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity eor is
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
end eor;

architecture arch of eor is

   constant WORD_BITS   : integer := VALUE / (WORD_WIDTH / 8);
   constant MASK_BITS   : integer := VALUE mod (WORD_WIDTH / 8);
   constant WORD_BYTES  : integer := WORD_WIDTH / 8;

begin

   dout     <= min;
   mout     <= din;
   mre      <= re;
   mwe      <= we;
   maddr    <= addr xor std_logic_vector(to_signed(WORD_BITS, ADDR_WIDTH));
   ready    <= mready;

   -- Determine the updated mask.
   process(mask)
      variable xor_mask : unsigned(WORD_BYTES - 1 downto 0);
      variable nb       : unsigned(WORD_BYTES - 1 downto 0);
   begin
      xor_mask := to_unsigned(MASK_BITS, WORD_BYTES);
      for b in 0 to WORD_BYTES - 1 loop
         nb := to_unsigned(b, WORD_BYTES) xor xor_mask;
         mmask(to_integer(nb)) <= mask(b);
      end loop;
   end process;

end arch;
