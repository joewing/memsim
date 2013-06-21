
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

   constant WORD_BYTES : natural := WORD_WIDTH / 8;

   subtype byte_type is std_logic_vector(7 downto 0);
   type byte_array_type is array(0 to SIZE - 1) of byte_type;
   type word_array_type is array(0 to WORD_BYTES - 1) of byte_array_type;

   signal data    : word_array_type;
   signal is_hit  : std_logic;
   signal value   : std_logic_vector(WORD_WIDTH - 1 downto 0);
   signal raddr   : std_logic_vector(ADDR_WIDTH - 1 downto 0);
   signal rin     : std_logic_vector(WORD_WIDTH - 1 downto 0);
   signal rre     : std_logic;
   signal rwe     : std_logic;
   signal rmask   : std_logic_vector((WORD_WIDTH / 8) - 1 downto 0);

begin

   process(clk)
      variable nat_addr : natural;
   begin
      if clk'event and clk = '1' then
         if rst = '0' and is_hit = '1' then
            nat_addr := to_integer(unsigned(raddr));
            if rre = '1' then
               for b in 0 to WORD_BYTES - 1 loop
                  value(b * 8 + 7 downto b * 8) <= data(b)(nat_addr);
               end loop;
            elsif rwe = '1' then
               for b in 0 to WORD_BYTES - 1 loop
                  if rmask(b) = '1' then
                     data(b)(nat_addr) <= rin(b * 8 + 7 downto b * 8);
                  end if;
               end loop;
            end if;
         end if;
      end if;
   end process;

   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            rre      <= '0';
            rwe      <= '0';
            is_hit   <= '0';
         else
            if unsigned(addr) < SIZE then
               is_hit <= '1';
            else
               is_hit <= '0';
            end if;
            raddr <= addr;
            rre   <= re;
            rwe   <= we;
            rin   <= din;
            rmask <= mask;
         end if;
      end if;
   end process;

   maddr <= raddr;
   mre   <= rre when is_hit = '0' else '0';
   mwe   <= rwe when is_hit = '0' else '0';
   mmask <= mask;
   dout  <= value when is_hit = '1' else min;
   mout  <= din;
   ready <= mready when rre = '0' and rwe = '0' else '0';

end spm_arch;
