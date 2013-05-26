
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity trace is
end trace;

architecture trace_arch of trace is

   constant ADDR_BITS : natural := 64;
   constant WORD_BITS : natural := 64;

   function log2(n : natural) return natural is
      variable i        : natural := n;
      variable result   : natural := 0;
   begin
      while i > 1 loop
         i := i / 2;
         result := result + 1;
      end loop;
      return result;
   end log2;

   constant WORD_BYTES : natural := WORD_BITS / 8;
   constant WORD_SHIFT : natural := log2(WORD_BYTES);

   type state_type is (
      STATE_ACTION,
      STATE_ADDRESS,
      STATE_SIZE
   );

   function parse_number(ch : character) return integer is
   begin
      case ch is
         when '0'       => return 0;
         when '1'       => return 1;
         when '2'       => return 2;
         when '3'       => return 3;
         when '4'       => return 4;
         when '5'       => return 5;
         when '6'       => return 6;
         when '7'       => return 7;
         when '8'       => return 8;
         when '9'       => return 9;
         when 'a' | 'A' => return 10;
         when 'b' | 'B' => return 11;
         when 'c' | 'C' => return 12;
         when 'd' | 'D' => return 13;
         when 'e' | 'E' => return 14;
         when 'f' | 'F' => return 15;
         when others    => return -1;
      end case;
   end parse_number;

   procedure cycle(signal clk : out std_logic) is
   begin
      clk <= '1';
      wait for 10 ns;
      clk <= '0';
      wait for 10 ns;
   end cycle;

   procedure wait_ready(signal clk : out std_logic;
                        signal rdy : in std_logic) is
   begin
      while rdy = '0' loop
         cycle(clk);
      end loop;
   end wait_ready;

   procedure update(signal clk : out std_logic;
                    signal ena : out std_logic;
                    signal rdy : in std_logic) is
   begin
      wait_ready(clk, rdy);
      ena <= '1';
      cycle(clk);
      ena <= '0';
      cycle(clk);
      wait_ready(clk, rdy);
   end update;

   signal clk           : std_logic;
   signal rst           : std_logic;
   signal mem_addr      : std_logic_vector(ADDR_BITS - 1 downto 0);
   signal mem_din       : std_logic_vector(WORD_BITS - 1 downto 0);
   signal mem_dout      : std_logic_vector(WORD_BITS - 1 downto 0);
   signal mem_re        : std_logic;
   signal mem_we        : std_logic;
   signal mem_ready     : std_logic;
   signal cycle_count   : natural := 0;

begin

   process

      file infile       : text is "input.trace";
      variable temp     : line;
      variable ch       : character;
      variable good     : boolean;
      variable state    : state_type := STATE_ACTION;
      variable action   : character;
      variable address  : unsigned(ADDR_BITS - 1 downto 0);
      variable size     : integer;
      variable value    : integer;
      variable offset   : integer;
      variable count    : integer;
variable start : integer;

   begin

      rst      <= '1';
      mem_re   <= '0';
      mem_we   <= '0';
      cycle(clk);
      rst      <= '0';

      while not endfile(infile) loop
         readline(infile, temp);
         read(temp, ch, good);
         while good loop
            case state is
               when STATE_ACTION =>
                  action   := ch;
                  address  := to_unsigned(0, 64);
                  size     := 0;
                  case ch is
                     when 'R' | 'W' | 'M' => state := STATE_ADDRESS;
                     when 'I'             => state := STATE_SIZE;
                     when others          => state := STATE_ACTION;
                  end case;
                  read(temp, ch, good);
               when STATE_ADDRESS =>
                  if ch = ':' then
                     state := STATE_SIZE;
                     read(temp, ch, good);
                  else
                     value := parse_number(ch);
                     if value < 0 then
                        state := STATE_ACTION;
                     else
                        address := address(ADDR_BITS - 5 downto 0) & "0000";
                        address := address + to_unsigned(value, ADDR_BITS);
                        read(temp, ch, good);
                     end if;
                  end if;
               when STATE_SIZE =>
                  value := parse_number(ch);
                  if value < 0 then
                     offset  := to_integer(unsigned(address)) mod WORD_BYTES;
                     count   := (size + offset + WORD_BYTES - 1) / WORD_BYTES;
                     address := shift_right(address, WORD_SHIFT);
                     case action is
                        when 'R' =>
                           for i in 1 to count loop
                              mem_addr <= std_logic_vector(address);
                              update(clk, mem_re, mem_ready);
                              address := address + to_unsigned(1, ADDR_BITS);
                           end loop;
                        when 'W' =>
                           if offset /= 0 then
                              mem_addr <= std_logic_vector(address);
                              update(clk, mem_re, mem_ready);
                           elsif offset + size < WORD_BYTES then
                              mem_addr <= std_logic_vector(address);
                              update(clk, mem_re, mem_ready);
                           end if;
                           for i in 1 to count loop
                              mem_addr <= std_logic_vector(address);
                              update(clk, mem_we, mem_ready);
                              address := address + to_unsigned(WORD_BYTES, 1);
                           end loop;
                        when 'M' =>
                           for i in 1 to count loop
                              mem_addr <= std_logic_vector(address);
                              update(clk, mem_re, mem_ready);
                              update(clk, mem_we, mem_ready);
                              address := address + to_unsigned(WORD_BYTES, 1);
                           end loop;
                        when 'I' =>
                           for i in 1 to size loop
                              cycle(clk);
                           end loop;
                        when others =>
                           report "invalid action" severity failure;
                     end case;
                     state := STATE_ACTION;
                     action := '?';
                  else
                     size := size * 16 + value;
                     read(temp, ch, good);
                  end if;
            end case;
         end loop;
      end loop;

      offset  := to_integer(unsigned(address)) mod WORD_BYTES;
      count   := (size + offset + WORD_BYTES - 1) / WORD_BYTES;
      address := shift_right(address, WORD_SHIFT);
      case action is
         when 'R' =>
            for i in 1 to count loop
               mem_addr <= std_logic_vector(address);
               update(clk, mem_re, mem_ready);
               address := address + to_unsigned(1, ADDR_BITS);
            end loop;
         when 'W' =>
            if offset /= 0 then
               mem_addr <= std_logic_vector(address);
               update(clk, mem_re, mem_ready);
            elsif offset + size < WORD_BYTES then
               mem_addr <= std_logic_vector(address);
               update(clk, mem_re, mem_ready);
            end if;
            for i in 1 to count loop
               mem_addr <= std_logic_vector(address);
               update(clk, mem_we, mem_ready);
               address := address + to_unsigned(WORD_BYTES, 1);
            end loop;
         when 'M' =>
            for i in 1 to count loop
               mem_addr <= std_logic_vector(address);
               update(clk, mem_re, mem_ready);
               update(clk, mem_we, mem_ready);
               address := address + to_unsigned(WORD_BYTES, 1);
            end loop;
         when 'I' =>
            for i in 1 to size loop
               cycle(clk);
            end loop;
         when others => null;
      end case;

      report "cycles: " & natural'image(cycle_count);
      wait;

   end process;

   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            cycle_count <= 0;
         else
            cycle_count <= cycle_count + 1;
         end if;
      end if;
   end process;

   mem_din  <= (others => '0');

   mem1 : entity work.memory
      generic map (
         ADDR_WIDTH => ADDR_BITS,
         WORD_WIDTH => WORD_BITS
      )
      port map (
         clk     => clk,
         rst     => rst,
         addr    => mem_addr,
         din     => mem_din,
         dout    => mem_dout,
         re      => mem_re,
         we      => mem_we,
         ready   => mem_ready
      );

end trace_arch;
