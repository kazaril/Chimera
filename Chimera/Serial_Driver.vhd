----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 13.04.2015 18:54:13
-- Design Name: 
-- Module Name: serial_driver - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity serial_driver is
     Port (
          clk_250:in STD_LOGIC;
          left : in STD_LOGIC_VECTOR (31 downto 0);
          right : in STD_LOGIC_VECTOR (31 downto 0);
          bclk: out STD_LOGIC;
          lrclk:out STD_LOGIC;
          sdata:out STD_LOGIC;
          mclk:out STD_LOGIC;
          initialized:in Std_logic
     );
end serial_driver;

architecture Behavioral of serial_driver is
    --- CLOCK------
    signal bclk_sig:Std_logic;
    signal mclk_sig:std_logic;
    signal lrclk_sig:std_logic;

    ---CLOCK COUNTER

    signal mclk_cnt: std_logic_vector(3 downto 0);
    signal lrclk_cnt: std_logic_vector(5 downto 0);
    signal bclk_cnt:std_logic_vector(4 downto 0);    
    ---- CODEC STUFF
    signal sdata_buff: std_logic_vector(63 downto 0);

begin

--- SIGNAL TO NET


bclk<=bclk_sig;
mclk<=mclk_sig;
lrclk<=lrclk_sig;





---- PROCESS ----

ClockDivider:process(Clk_250)
begin
if rising_edge(Clk_250) then
    if mclk_cnt<3 then -- clk 30 mhz, good enough for CODEC
       mclk_cnt<=mclk_cnt+1;
    else
        mclk_cnt<= (others=>'0');
        mclk_sig<=not mclk_sig;
    end if;
end if;
end process;

bclk_driver:process(mclk_sig) -- CODEC CLOCK
begin
    if falling_edge(mclk_sig) then
        if bclk_cnt<7 then -- 16 time slower than master clock, 64 time faster than lrclock
            bclk_cnt<=bclk_cnt+1;
        else
            bclk_cnt<=(others=>'0');
            bclk_sig<=not bclk_sig;
        end if;
    end if;
end process;

lrclk_driver:process(bclk_sig) --CODEC CLOCK and data line driver
begin
    if falling_edge(bclk_sig) then
        if lrclk_cnt<31 then
           lrclk_cnt<=lrclk_cnt+1;
           sdata<=sdata_buff(63); -- shift data out @ falling edge, trigger at rising edge , insert audio ready here
           sdata_buff<=sdata_buff(62 downto 0) &'0';
        else
           lrclk_sig<=not lrclk_sig;
           lrclk_cnt<=(others=>'0');
           if lrclk_sig='0' then  -- must be rising edge
             if (initialized='1') then
                sdata_buff<=left&right;
              else
                 sdata_buff<=(others=>'0');
            end if;
           end if;
        end if;
    end if;
end process;


end Behavioral;
