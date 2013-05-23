
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb is
end entity;

architecture tb_arch of tb is

   signal clk     : std_logic;
   signal rst     : std_logic;
   signal cycles  : integer;

   constant ADDR_WIDTH : integer := 64;
   constant WORD_WIDTH : integer := 32;

   signal mem_addr   : std_logic_vector(ADDR_WIDTH - 1 downto 0);
   signal mem_din    : std_logic_vector(WORD_WIDTH - 1 downto 0);
   signal mem_dout   : std_logic_vector(WORD_WIDTH - 1 downto 0);
   signal mem_re     : std_logic;
   signal mem_we     : std_logic;
   signal mem_ready  : std_logic;

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

begin

   mem1 : entity work.memory
      generic map (
         ADDR_WIDTH  => ADDR_WIDTH,
         WORD_WIDTH  => WORD_WIDTH
      )
      port map (
         clk      => clk,
         rst      => rst,
         addr     => mem_addr,
         din      => mem_din,
         dout     => mem_dout,
         re       => mem_re,
         we       => mem_we,
         ready    => mem_ready
      );

   process
   begin

      -- Reset
      mem_addr <= (others => '0');
      mem_we   <= '0';
      mem_re   <= '0';
      mem_din  <= (others => '0');
      rst <= '1';
      cycle(clk);
      rst <= '0';

      assert mem_ready = '1' report "not ready" severity failure;

      -- Write a value.
      mem_addr <= x"00000000_00000001";
      mem_din <= x"01234567";
      update(clk, mem_we, mem_ready);

      -- Make sure the write took (hit).
      mem_addr <= x"00000000_00000001";
      update(clk, mem_re, mem_ready);
      assert mem_dout = x"01234567" report "read failed" severity failure;

      -- Read another value (cold miss).
      mem_addr <= x"00000000_00000000";
      update(clk, mem_re, mem_ready);
      assert mem_dout = x"FFFFFFFF" report "read failed" severity failure;

      -- Write another value (conflict).
      mem_addr <= x"00000000_00000100";
      mem_din  <= x"00000321";
      update(clk, mem_we, mem_ready);

      -- Read the value (hit).
      mem_addr <= x"00000000_00000100";
      update(clk, mem_re, mem_ready);
      assert mem_dout = x"00000321" report "read failed" severity failure;

      -- Read the previous value.
      mem_addr <= x"00000000_00000001";
      update(clk, mem_re, mem_ready);
      assert mem_dout = x"01234567" report "read failed" severity failure;

      -- Another write.
      mem_addr <= x"00000000_00000101";
      mem_din  <= x"00000abc";
      update(clk, mem_we, mem_ready);

      -- Some reads.
      mem_addr <= x"00000000_00000101";
      update(clk, mem_re, mem_ready);
      assert mem_dout = x"00000abc" report "read failed" severity failure;
      mem_addr <= x"00000000_00000001";
      update(clk, mem_re, mem_ready);
      assert mem_dout = x"01234567" report "read failed" severity failure;
      mem_addr <= x"00000000_00000100";
      update(clk, mem_re, mem_ready);
      assert mem_dout = x"00000321" report "read failed" severity failure;

      -- Overwite.
      mem_addr <= x"00000000_00000001";
      mem_din  <= x"55555555";
      update(clk, mem_we, mem_ready);

      -- More reads.
      mem_addr <= x"00000000_00000001";
      update(clk, mem_re, mem_ready);
      assert mem_dout = x"55555555" report "read failed" severity failure;
      mem_addr <= x"00000000_00000101";
      update(clk, mem_re, mem_ready);
      assert mem_dout = x"00000abc" report "read failed" severity failure;
      mem_addr <= x"00000000_00000100";
      update(clk, mem_re, mem_ready);
      assert mem_dout = x"00000321" report "read failed" severity failure;

      -- Test some reads/writes.
      for j in 2 to 100 loop
         for i in 1 to j loop
            mem_addr <= std_logic_vector(to_unsigned(i, ADDR_WIDTH));
            mem_din  <= std_logic_vector(to_unsigned(i, WORD_WIDTH));
            update(clk, mem_we, mem_ready);
         end loop;
         for i in 1 to j loop
            mem_addr <= std_logic_vector(to_unsigned(i, ADDR_WIDTH));
            update(clk, mem_re, mem_ready);
            assert unsigned(mem_dout) = i
               report "failed at j=" & natural'image(j);
         end loop;
      end loop;

      wait_ready(clk, mem_ready);
      report "cycles: " & integer'image(cycles);
      wait;

   end process;

   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            cycles <= 0;
         else
            cycles <= cycles + 1;
         end if;
      end if;
   end process;

end tb_arch;
