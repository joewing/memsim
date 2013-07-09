
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram is
   generic (
      ADDR_WIDTH  : natural := 64;
      WORD_WIDTH  : natural := 64;
      SIZE        : natural := 65536;
      LATENCY     : natural := 10;
      BURST       : natural := 0
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
      ready    : out std_logic
   );
end ram;

architecture ram_arch of ram is

   subtype word_type is std_logic_vector(WORD_WIDTH - 1 downto 0);
   type word_array_type is array (0 to SIZE - 1) of word_type;

   signal data       : word_array_type := (others => (others => '0'));
   signal value      : word_type;
   signal counter    : unsigned(LATENCY - 1 downto 0);
   signal nat_addr   : natural;
   signal do_read    : std_logic;
   signal do_write   : std_logic;

begin

   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            counter <= to_unsigned(1, LATENCY);
            do_read <= '0';
            do_write <= '0';
         elsif re = '1' then
            counter <= "1" & to_unsigned(0, LATENCY - 1);
            do_read <= '1';
            do_write <= '0';
         elsif we = '1' then
            counter <= "1" & to_unsigned(0, LATENCY - 1);
            do_write <= '1';
            do_read <= '0';
            value <= din;
         elsif counter(0) = '0'  then
            counter <= shift_right(counter, 1);
            do_read <= '0';
            do_write <= '0';
         end if;
         if SIZE > 0 then
            if do_read = '1' then
               value <= data(nat_addr);
            elsif do_write = '1' then
               for b in 0 to (WORD_WIDTH / 8) - 1 loop
                  if mask(b) = '1' then
                     data(nat_addr)(b * 8 + 7 downto b * 8) <=
                        din(b * 8 + 7 downto b * 8);
                  end if;
               end loop;
            end if;
         else
            value <= (others => '0');
         end if;
      end if;
   end process;

   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            nat_addr <= 0;
         elsif re = '1' or we = '1' then
            if ADDR_WIDTH > 31 then
               nat_addr <= to_integer(unsigned(addr(30 downto 0)));
            else
               nat_addr <= to_integer(unsigned(addr));
            end if;
         end if;
      end if;
   end process;

   ready <= counter(0);

   doutn : if SIZE > 0 generate
      dout  <= value when counter(0) = '1' else (others => 'Z');
   end generate;
   dout0 : if SIZE = 0 generate
      dout <= (others => 'Z');
   end generate;

end ram_arch;
