-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2022 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Elena Marochkina <xmaroc00@stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;
-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

-- CNT 
  -- in
  signal cnt_inc: STD_LOGIC;  -- counter increment
  signal cnt_dec: STD_LOGIC;  -- counter decrement
  signal cnt_clear: STD_LOGIC;  -- counter clear
  -- out
  signal cnt_reg: STD_LOGIC_VECTOR(12 downto 0);     -- counter itself
--  PTR 
  -- in
  signal ptr_inc: STD_LOGIC;  -- pointer increment
  signal ptr_dec: STD_LOGIC;  -- pointer decrement
  signal ptr_clear: STD_LOGIC; -- pointer clear
  -- out
  signal ptr_reg: STD_LOGIC_VECTOR(12 downto 0); -- pointer register
-- PC  
  -- in
  signal prg_inc: STD_LOGIC;   -- program counter increment
  signal prg_dec: STD_LOGIC;   -- program counter decrement
  signal prg_clear: STD_LOGIC;   -- program counter clear
  -- out
  signal prg_reg: STD_LOGIC_VECTOR(12 downto 0); -- program counter register 
-- mux1 
  -- in
  signal mux1_sel:  STD_LOGIC;   -- mux1 select
  -- out
  signal mux1_out: STD_LOGIC_VECTOR(12 downto 0); 
--  mux2 
  -- in
  signal mux2_sel: std_logic_vector(1 downto 0);   -- mux1 select 
  -- out
  signal mux2_out: STD_LOGIC_VECTOR(7 downto 0); 
  type fsm_state is (
    s_start, -- start state
    s_instr_fetch, -- fetch instruction
    s_instr_decode, -- instruction decoding
    s_inc_ptr, -- incrementing the pointer value
    s_dec_ptr, -- decrementing the pointer value
    s_inc_0, s_inc_1, s_inc_2, -- incrementing the value 
    s_dec_0, s_dec_1, s_dec_2, -- decrementing the value
    s_while_0, s_while_01, s_while_1, s_while_2, s_end_while_0, s_end_while_01, s_end_while_1, s_end_while_2,
    s_do_0,
    s_dowhile_0, s_dowhile_01, s_dowhile_1, s_dowhile_2,
    s_putchar_0, s_putchar_1, -- print value
    s_getchar_0, s_getchar_1, --  read value
    s_return, -- null
    s_others, wait_and_fetch 
  );
  signal fsm_present_state : fsm_state := s_start; -- actual state
  signal fsm_next_state : fsm_state; -- next state
begin
-- Program counter 
  prg_counter: process (CLK, RESET, prg_inc, prg_dec)
  begin
    if RESET = '1' then
      prg_reg <= (others => '0');
    elsif CLK'event and CLK = '1' then
      if prg_inc = '1' then
        prg_reg <= prg_reg + 1;
      elsif prg_dec = '1' then
        prg_reg <= prg_reg - 1;
      elsif prg_clear = '1' then
        prg_reg <= (others => '0');
      end if;
    end if;
  end process;

  ptr_counter: process (CLK, RESET, ptr_inc, ptr_dec)
  begin
    if RESET = '1' then
      ptr_reg <= (12 => '1', others => '0'); -- 0x1000;
    elsif CLK'event and CLK = '1' then
      if ptr_inc = '1' then
        ptr_reg <= ptr_reg + 1;
      elsif ptr_dec = '1' then
        ptr_reg <= ptr_reg - 1;
      elsif ptr_clear = '1' then
        ptr_reg <= (others => '0');
      end if;
    end if;
  end process;

  cnt_counter: process (CLK, RESET, cnt_inc, cnt_dec)
  begin
    if RESET = '1' then
      cnt_reg <= (others => '0');
    elsif CLK'event and CLK = '1' then
      if cnt_inc = '1' then
        cnt_reg <= cnt_reg + 1;
      elsif cnt_dec = '1' then
        cnt_reg <= cnt_reg - 1;
      elsif cnt_clear = '1' then
        cnt_reg <= (others => '0');
      end if;
    end if;
  end process;

  fsm_present_state_proc: process (CLK, RESET, EN)
  begin
    if RESET = '1' then
      fsm_present_state <= s_start;
    elsif CLK'event and CLK = '1' then
      if EN = '1' then
        fsm_present_state <= fsm_next_state;
      end if;
    end if;
  end process;

  mux1: process (CLK, RESET, mux1_sel, ptr_reg, prg_reg)
  begin
    if RESET = '1' then
      -- Ukazatel ptr ukazuje po resetu do pameti na adresu 0x1000
      -- Data is stored from 0x1000.
      -- PTR points to dat
      mux1_out <= (12 => '1', others => '0'); -- 0x1000
    elsif CLK'event and CLK = '1' then
      case mux1_sel is
        when '0' =>
          -- Points to pragramm data
          mux1_out <= prg_reg;
        when '1' =>
          -- Points to data
          mux1_out <= ptr_reg;
        when others => 
          mux1_out <= (others => '0');
      end case;
    end if;
  end process;

  mux2: process (CLK, RESET, mux2_sel, DATA_RDATA, IN_DATA)
  begin
    if RESET = '1' then
      mux2_out <= (others => '0');
    elsif CLK'event and CLK = '1' then
      case mux2_sel is
        when "00" =>
          -- Writes input data 
          mux2_out <= IN_DATA;
        when "01" =>
         -- Writes just readed data decreased by 1 
          mux2_out <= DATA_RDATA - 1;
        when "10" =>
         -- Writes just readed data increased by 1
          mux2_out <= DATA_RDATA + 1;
        when others =>
          mux2_out <= (others => '0');
      end case;
    end if;
  end process;

  DATA_ADDR <= mux1_out;
  DATA_WDATA <= mux2_out;
  OUT_DATA <= DATA_RDATA;

  fsm_next_state_proc: process (fsm_present_state, OUT_BUSY, IN_VLD, DATA_RDATA, cnt_reg, DATA_RDATA)
  begin

    -- INIT
    -- Process
    prg_inc <= '0';
    prg_dec <= '0';
    prg_clear <= '0';
    -- PTR
    ptr_inc <= '0';
    ptr_dec <= '0';
    ptr_clear <= '0';
    -- CNT
    cnt_inc <= '0';
    cnt_dec <= '0';
    cnt_clear <= '0';
    -- TODO:
    IN_REQ <= '0';
    OUT_WE <= '0';
    -- mux1
    mux1_sel <= '0';
    -- mux2 
    mux2_sel <= "11";
    DATA_RDWR <= '0';
    DATA_EN <= '0';

    case fsm_present_state is
      when s_start =>
        mux1_sel <= '0';
        DATA_RDWR <= '0';
        DATA_EN <= '1';
        fsm_next_state <= s_instr_fetch;

      when s_instr_fetch =>
        mux1_sel <= '1'; -- PTR
        DATA_RDWR <= '0';
        DATA_EN <= '1';
        fsm_next_state <= s_instr_decode;

      when s_instr_decode =>
        case DATA_RDATA is
          when X"3E" =>
            fsm_next_state <= s_inc_ptr; -- incrementing the pointer value (>)
          when X"3C" =>
            fsm_next_state <= s_dec_ptr; -- decrementing the pointer value (<)
          when X"2B" =>
            fsm_next_state <= s_inc_0; -- increment the value of the current cell (+)
          when X"2D" =>
            fsm_next_state <= s_dec_0; -- decrementing the value of the current cell (-)
          when X"5B" =>
            fsm_next_state <= s_while_0; -- the beginning of the while loop ([)
          when X"5D" =>
            fsm_next_state <= s_end_while_0; -- end of while loop (])
          when X"28" =>
            fsm_next_state <= s_do_0; -- the beginning of the do-while cycle (()
          when X"29" =>
            fsm_next_state <= s_dowhile_0; -- end of the do-while cycle ())
          when X"2E" =>
            fsm_next_state <= s_putchar_0; -- print the value of the current cell (.)
          when X"2C" =>
            fsm_next_state <= s_getchar_0; -- loading the value into the current cell (,)
          when X"00" =>
            fsm_next_state <= s_return; -- null
          when others =>
            fsm_next_state <= s_others;
        end case;
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        mux1_sel <= '0';
        IN_REQ <= '1';

      when s_inc_ptr =>
        ptr_inc <= '1'; -- PTR += 1
        prg_inc <= '1'; -- PC += 1
        mux1_sel <= '1';
        fsm_next_state <= wait_and_fetch;

      when s_dec_ptr =>
        ptr_dec <= '1'; -- PTR -= 1
        prg_inc <= '1'; -- PC += 1
        mux1_sel <= '1';
        fsm_next_state <= wait_and_fetch;

      when s_inc_0 =>
        mux1_sel <= '1';
        mux2_sel <= "10"; -- DATA_WDATA += 1
        fsm_next_state <= s_inc_1;
        prg_inc <= '1'; -- PC += 1

      when s_inc_1 =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        mux1_sel <= '0';
        fsm_next_state <= s_instr_fetch;
      
      when s_inc_2 =>
        mux1_sel <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        fsm_next_state <= s_instr_fetch;


      when s_dec_0 =>
        mux1_sel <= '1';
        mux2_sel <= "01"; -- DATA_WDATA -= 1
        fsm_next_state <= s_dec_1;
        prg_inc <= '1'; -- PC += 1

      when s_dec_1 =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        mux1_sel <= '0';
        fsm_next_state <= s_instr_fetch;
    
      when s_dec_2 =>
        mux1_sel <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        fsm_next_state <= s_instr_fetch;


      -- start of while loop
      when s_while_0 =>
        prg_inc <= '1';
        if DATA_RDATA = (DATA_RDATA'range => '0') then
          cnt_inc <= '0';
          fsm_next_state <= s_while_1;
        else
          fsm_next_state <= wait_and_fetch;
        end if;
      
      when s_while_01 => 
        DATA_EN <= '1';
        fsm_next_state <= s_while_1;
        
      when s_while_1 =>
        prg_inc <= '1';
        if cnt_reg = (cnt_reg'range => '0') then
          fsm_next_state <= wait_and_fetch;
        else
          DATA_EN <= '1';
          fsm_next_state <= s_while_2;
        end if;
      
      when s_while_2 =>
        if cnt_reg = (cnt_reg'range => '0') then
          prg_inc <= '1';
          fsm_next_state <=  wait_and_fetch;
        else
          if DATA_RDATA = X"5B" then
            cnt_inc <= '1';
          end if;
          if DATA_RDATA = X"5D" then
            cnt_dec <= '1';
          end if;
          fsm_next_state <= s_while_1;  
        end if;
      
      -- end of while
      when s_end_while_0 =>
        DATA_RDWR <= '0';
        if DATA_RDATA = (DATA_RDATA'range => '0') then
          prg_inc <= '1';
          fsm_next_state <= wait_and_fetch;
        else
          prg_dec <= '1';
          cnt_inc <= '1';
          fsm_next_state <= s_end_while_01;
        end if;

      when s_end_while_01 => 
        fsm_next_state <= s_end_while_1;
        
      when s_end_while_1 =>
        if cnt_reg = (cnt_reg'range => '0') then
          prg_inc <= '1';
          fsm_next_state <= wait_and_fetch;
        else
          prg_dec <= '1';
          DATA_EN <= '1';
          fsm_next_state <= s_end_while_2;
        end if;

      when s_end_while_2 =>
        if cnt_reg = (cnt_reg'range => '0') then
          prg_inc <= '1';
          fsm_next_state <=  wait_and_fetch;
        else
          if DATA_RDATA = X"5B" then
            cnt_dec <= '1';
          end if;
          if DATA_RDATA = X"5D" then
            cnt_inc <= '1';
          end if;
          fsm_next_state <= s_end_while_1;  
        end if;

      -- start of do-while  
      when s_do_0 =>
        prg_inc <= '1';
        fsm_next_state <= wait_and_fetch; 

      when s_dowhile_0 =>
        DATA_RDWR <= '0';
        if DATA_RDATA = (DATA_RDATA'range => '0') then
          prg_inc <= '1';
          fsm_next_state <= wait_and_fetch;
        else
          cnt_inc <= '1';
          prg_dec <= '1';
          fsm_next_state <= s_dowhile_01;
        end if;

      when s_dowhile_01 => 
        fsm_next_state <= s_dowhile_1;
        
      when s_dowhile_1 =>
        if cnt_reg = (cnt_reg'range => '0') then
          prg_inc <= '1';
          fsm_next_state <= wait_and_fetch;
        else
          prg_dec <= '1';
          DATA_EN <= '1';
          fsm_next_state <= s_dowhile_2;
        end if;

      when s_dowhile_2 =>
        if cnt_reg = (cnt_reg'range => '0') then
          prg_inc <= '1';
          fsm_next_state <=  wait_and_fetch;
        else
          if DATA_RDATA = X"28" then
            cnt_dec <= '1';
          end if;
          if DATA_RDATA = X"29" then
            cnt_inc <= '1';
          end if;
          fsm_next_state <= s_dowhile_1;  
        end if;

      when s_putchar_0 =>
          if OUT_BUSY = '0' then 
          OUT_WE <= '1'; 
          prg_inc <= '1'; 
          -- next state
          fsm_next_state <= wait_and_fetch;
          elsif OUT_BUSY = '1' then 
          mux1_sel <= '1'; 
          -- next state
          fsm_next_state <= s_putchar_0; 
          end if;
      
      when s_getchar_0 =>
        mux1_sel <= '1';
        if IN_VLD /= '1' then
          IN_REQ <= '1';
          fsm_next_state <= s_getchar_0;
        else
          mux2_sel <= "00"; -- DATA_WDATA = IN_DATA
          fsm_next_state <= s_getchar_1;
        end if;
  
      when s_getchar_1 =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        mux1_sel <= '1';  
        prg_inc <= '1';
        fsm_next_state <= wait_and_fetch;

      when wait_and_fetch =>
        fsm_next_state <= s_instr_fetch;
      when others =>
        prg_inc <= '1';
        fsm_next_state <= wait_and_fetch;
    end case;
  end process;
end behavioral;
-- v 1.1