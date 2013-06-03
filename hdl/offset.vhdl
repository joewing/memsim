
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity offset is
   generic (
      ADDR_WIDTH     : in natural := 32;
      WORD_WIDTH     : in natural := 32;
      OFFSET         : in integer := 0
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
begin

   -- Handle the simple case when the offset is a multiple of the word size.
   word_offset : if (OFFSET mod (WORD_WIDTH / 8)) = 0 generate

      process(addr)
         variable woffset : signed(ADDR_WIDTH - 1 downto 0);
      begin
         woffset  := to_signed(OFFSET / WORD_WIDTH, ADDR_WIDTH);
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
   byte_offset : if (OFFSET mod (WORD_WIDTH / 8)) /= 0 generate
      -- TODO
   end generate;

end offset_arch;
