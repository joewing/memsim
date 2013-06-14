
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity prefetch is
   generic (
      ADDR_WIDTH  : natural := 32;
      WORD_WIDTH  : natural := 32;
      STRIDE      : natural := 1
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
end prefetch;

architecture arch of prefetch is

   type state_type is (
      STATE_IDLE,
      STATE_INIT,
      STATE_WAIT
   );

   signal state            : state_type;
   signal next_state       : state_type;
   signal prefetch_addr    : std_logic_vector(ADDR_WIDTH - 1 downto 0);
   signal pre              : std_logic;
   signal pwe              : std_logic;

begin

   -- Determine the next state.
   process(state, re, we, pre, pwe, mready)
   begin
      next_state <= state;
      case state is
         when STATE_IDLE =>
            -- No prefetch active.
            if re = '1' or we = '1' then
               next_state <= STATE_INIT;
            end if;
         when STATE_INIT =>
            -- Prefetch to be started as soon as mready.
            if mready = '1' then
               next_state <= STATE_WAIT;
            end if;
         when STATE_WAIT =>
            -- Prefetch in progress.
            if mready = '1' then
               if pre = '1' or pwe = '1'  then
                  next_state <= STATE_INIT;
               else
                  next_state <= STATE_IDLE;
               end if;
            end if;
      end case;
   end process;

   -- State machine.
   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            state <= STATE_IDLE;
         else
            state <= next_state;
         end if;
      end if;
   end process;

   -- Register the address to be prefetched.
   process(clk)
   begin
      if clk'event and clk = '1' then
         if state = STATE_IDLE and (re = '1' or we = '1')  then
            prefetch_addr  <= std_logic_vector(unsigned(addr) + STRIDE);
         end if;
      end if;
   end process;

   -- Drive memory.
   process(state, re, we, pre, pwe, prefetch_addr, addr, mask, mready)
   begin
      case state is
         when STATE_IDLE =>
            -- No prefetch active.  Forward the memory access immediately.
            mre   <= re or pre;
            mwe   <= we or pwe;
            maddr <= addr;
            mmask <= mask;
            ready <= mready and not (pre or pwe);
         when STATE_INIT =>
            -- Memory access active.
            -- Start a prefetch as soon as the memory is ready.
            mwe <= '0';
            if mready = '1' then
               mre   <= '1';
               maddr <= prefetch_addr;
               mmask <= (others => '1');
            else
               mre   <= '0';
               maddr <= addr;
               mmask <= mask;
            end if;
            ready <= mready;
         when STATE_WAIT =>
            -- Prefetch active.
            if mready = '1' then
               mre   <= pre;
               mwe   <= pwe;
               maddr <= addr;
               mmask <= mask;
            else
               maddr <= prefetch_addr;
               mmask <= (others => '1');
               mre   <= '0';
               mwe   <= '0';
            end if;
            ready <= not (pre or pwe);
      end case;
   end process;

   -- Enqueue reads and writes that happen during prefetching.
   process(clk)
   begin
      if clk'event and clk = '1' then
         case state is
            when STATE_IDLE =>
               -- Reads and writes can happen immediately.
               pre      <= '0';
               pwe      <= '0';
            when others =>
               -- Access must wait until after the prefetch.
               if mready = '1' or re = '1' or we = '1' then
                  pre      <= re;
                  pwe      <= we;
               end if;
         end case;
      end if;
   end process;

   mout  <= din;
   dout  <= min;

end arch;
