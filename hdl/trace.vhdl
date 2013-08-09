
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

   procedure update(signal clk : out std_logic;
                    signal ena : out std_logic;
                    signal rdy : in std_logic) is
   begin
      if rdy = '0' then
         ena <= '1';
         while rdy = '0' loop
            clk <= '1';
            wait for 10 ns;
            clk <= '0';
            wait for 10 ns;
         end loop;
         clk <= '1';
         wait for 10 ns;
         clk <= '0';
         wait for 10 ns;
         ena <= '0';
      else
         ena <= '1';
         clk <= '1';
         wait for 10 ns;
         clk <= '0';
         wait for 10 ns;
         ena <= '0';
         clk <= '1';
         wait for 10 ns;
         clk <= '0';
         wait for 10 ns;
      end if;
   end update;

   procedure run_action(signal clk     : out std_logic;
                        variable act   : in  character;
                        variable addr  : in  unsigned(ADDR_BITS - 1 downto 0);
                        variable size  : in  integer;
                        signal maddr   : out std_logic_vector(ADDR_BITS - 1
                                                              downto 0);
                        signal re      : out std_logic;
                        signal we      : out std_logic;
                        signal mask    : out std_logic_vector(ADDR_BITS / 8 - 1
                                                              downto 0);
                        signal rdy     : in  std_logic) is
      variable r_act       : character := 'R';
      variable w_act       : character := 'W';
      variable offset_a    : integer;
      variable offset_b    : integer;
      variable word_addr   : unsigned(ADDR_BITS - 1 downto 0);
      variable end_addr    : unsigned(ADDR_BITS - 1 downto 0);
   begin
      offset_a    := to_integer(addr) mod WORD_BYTES;
      offset_b    := (to_integer(addr) + size - 1) mod WORD_BYTES;
      word_addr   := shift_right(addr, WORD_SHIFT);
      end_addr    := shift_right(addr + to_unsigned(size - 1, ADDR_BITS),
                                 WORD_SHIFT);
      case act is
         when 'R' =>
            mask  <= (others => '0');
            maddr <= std_logic_vector(word_addr);
            if word_addr = end_addr then
               for b in offset_a to offset_b loop
                  mask(b) <= '1';
               end loop;
               update(clk, re, rdy);
            else
               for b in offset_a to WORD_BYTES - 1 loop
                  mask(b) <= '1';
               end loop;
               update(clk, re, rdy);
               word_addr := word_addr + to_unsigned(1, ADDR_BITS);
               while word_addr /= end_addr loop
                  mask <= (others => '1');
                  maddr <= std_logic_vector(word_addr);
                  update(clk, re, rdy);
                  word_addr := word_addr + to_unsigned(1, ADDR_BITS);
               end loop;
               mask <= (others => '0');
               for b in 0 to offset_b - 1 loop
                  mask(b) <= '1';
               end loop;
               maddr <= std_logic_vector(word_addr);
               update(clk, re, rdy);
            end if;
         when 'W' =>
            mask  <= (others => '0');
            maddr <= std_logic_vector(word_addr);
            if word_addr = end_addr then
               for b in offset_a to offset_b loop
                  mask(b) <= '1';
               end loop;
               update(clk, we, rdy);
            else
               for b in offset_a to WORD_BYTES - 1 loop
                  mask(b) <= '1';
               end loop;
               update(clk, we, rdy);
               word_addr := word_addr + to_unsigned(1, ADDR_BITS);
               while word_addr /= end_addr loop
                  mask <= (others => '1');
                  maddr <= std_logic_vector(word_addr);
                  update(clk, we, rdy);
                  word_addr := word_addr + to_unsigned(1, ADDR_BITS);
               end loop;
               mask <= (others => '0');
               for b in 0 to offset_b - 1 loop
                  mask(b) <= '1';
               end loop;
               maddr <= std_logic_vector(word_addr);
               update(clk, we, rdy);
            end if;
         when 'M' =>
            run_action(clk, r_act, addr, size, maddr, re, we, mask, rdy);
            run_action(clk, w_act, addr, size, maddr, re, we, mask, rdy);
         when 'I' =>
            while rdy = '0' loop
               clk <= '1';
               wait for 10 ns;
               clk <= '0';
               wait for 10 ns;
            end loop;
            for i in 1 to size loop
               clk <= '1';
               wait for 10 ns;
               clk <= '0';
               wait for 10 ns;
            end loop;
         when '?' => null;
         when others =>
            report "invalid action" severity failure;
      end case;
   end run_action;

   signal clk           : std_logic;
   signal rst           : std_logic;
   signal mem_addr      : std_logic_vector(ADDR_BITS - 1 downto 0);
   signal mem_din       : std_logic_vector(WORD_BITS - 1 downto 0);
   signal mem_dout      : std_logic_vector(WORD_BITS - 1 downto 0);
   signal mem_re        : std_logic;
   signal mem_we        : std_logic;
   signal mem_mask      : std_logic_vector((WORD_BITS / 8) - 1 downto 0);
   signal mem_ready     : std_logic;
   signal ram_addr      : std_logic_vector(ADDR_BITS - 1 downto 0);
   signal ram_din       : std_logic_vector(WORD_BITS - 1 downto 0);
   signal ram_dout      : std_logic_vector(WORD_BITS - 1 downto 0);
   signal ram_re        : std_logic;
   signal ram_we        : std_logic;
   signal ram_mask      : std_logic_vector((WORD_BITS / 8) - 1 downto 0);
   signal ram_ready     : std_logic;
   signal cycle_count   : natural := 0;
   signal do_read       : std_logic;
   signal do_write      : std_logic;
   signal next_addr     : std_logic_vector(ADDR_BITS - 1 downto 0);

begin

   process

      file infile          : text is "input.trace";
      variable temp        : line;
      variable ch          : character;
      variable good        : boolean;
      variable state       : state_type := STATE_ACTION;
      variable action      : character;
      variable address     : unsigned(ADDR_BITS - 1 downto 0);
      variable temp_addr   : unsigned(ADDR_BITS - 1 downto 0);
      variable size        : integer;
      variable value       : integer;

   begin

      rst      <= '1';
      do_read  <= '0';
      do_write <= '0';
      clk      <= '1';
      wait for 10 ns;
      clk      <= '0';
      wait for 10 ns;
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
                     run_action(clk, action, address, size, next_addr,
                                do_read, do_write, mem_mask, mem_ready);
                     state := STATE_ACTION;
                     action := '?';
                  else
                     size := size * 16 + value;
                     read(temp, ch, good);
                  end if;
            end case;
         end loop;
      end loop;

      run_action(clk, action, address, size, next_addr,
                 do_read, do_write, mem_mask, mem_ready);

      while mem_ready = '0' loop
         clk <= '1';
         wait for 10 ns;
         clk <= '0';
         wait for 10 ns;
      end loop;

      report "cycles: " & natural'image(cycle_count - 1);
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
   mem_re   <= mem_ready when do_read = '1' else '0';
   mem_we   <= mem_ready when do_write = '1' else '0';

   process(clk)
   begin
      if clk'event and clk = '1' then
         if mem_ready = '1' then
            mem_addr <= next_addr;
         end if;
      end if;
   end process;

   ram1 : entity work.ram
      generic map (
         ADDR_WIDTH  => ADDR_BITS,
         WORD_WIDTH  => WORD_BITS,
         SIZE        => 65536,
         LATENCY     => 10,
         BURST       => 0
      )
      port map (
         clk      => clk,
         rst      => rst,
         addr     => ram_addr,
         din      => ram_din,
         dout     => ram_dout,
         re       => ram_re,
         we       => ram_we,
         mask     => ram_mask,
         ready    => ram_ready
      );

   mem1 : entity work.mem
      generic map (
         ADDR_WIDTH => ADDR_BITS,
         WORD_WIDTH => WORD_BITS
      )
      port map (
         clk         => clk,
         rst         => rst,
         port0_addr  => ram_addr,
         port0_din   => ram_dout,
         port0_dout  => ram_din,
         port0_re    => ram_re,
         port0_we    => ram_we,
         port0_mask  => ram_mask,
         port0_ready => ram_ready,
         addr        => mem_addr,
         din         => mem_din,
         dout        => mem_dout,
         re          => mem_re,
         we          => mem_we,
         mask        => mem_mask,
         ready       => mem_ready
      );

end trace_arch;
