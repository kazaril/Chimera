----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 07.04.2015 23:08:27
-- Design Name: 
-- Module Name: spi_interface - Behavioral
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

entity spi_interface is
    Port ( 
           clk_250:in std_logic;
           cclk : out STD_LOGIC; -- MAX 10Mhz
           start:in STD_LOGIC; -- enable transaction
           clatch : out STD_LOGIC; -- slave select
           data_in : in STD_LOGIC_VECTOR (23 downto 0); -- first 8 bit downto 0
           cdata : out STD_LOGIC -- mosi , miso is cout, ignored
           ); -- transaction in progress
end spi_interface;

architecture Behavioral of spi_interface is
    signal transaction_in_progress,transaction_in_progress_buff:std_logic:='0';
    signal bit_cnt:std_logic_vector(5 downto 0); -- count 40bit
    signal data_buffer:std_logic_vector(23 downto 0); -- buffer data in case change halfway
    signal cclk_sig:std_logic;
    signal cclk_cnt:std_logic_vector(5 downto 0);
begin

clatch<=not transaction_in_progress_buff; -- to pull clatch low easier
cclk<=cclk_sig;

ClockDivider:process(Clk_250)
begin
if rising_edge(Clk_250) then
     if cclk_cnt<7 then -- clk 9.6Mhz, good enough for SPI
         cclk_cnt<=cclk_cnt+1;
     else
         cclk_cnt<= (others=>'0');
         cclk_sig<=not cclk_sig;
     end if;
end if;
end process;


transaction:process(cclk_sig)
begin
    if falling_edge(cclk_sig) then 
       transaction_in_progress_buff<=transaction_in_progress; 
       if (transaction_in_progress='0') then 
        if (start ='1') then -- ready to transmit data and request to process data
            bit_cnt<=(others=>'0'); -- reset bit counter
            transaction_in_progress<='1'; -- transaction in progress
            data_buffer <=data_in; -- this will create 1 free 0 bit
        end if;
        cdata<='0';
       else
         if (bit_cnt<31) then -- 0->31, 4 byte , last byte will be ignore d cause clatch go high, position 32 will just be ignored because clatch go high
             bit_cnt<= bit_cnt+1; -- increasing bit cnt
         else
             transaction_in_progress<='0'; -- indicate not busy and set clatch high
         end if; 
      --if transaction is progress
         if (bit_cnt<8) then -- 0-7 can all be 0, 1 free 0 bit at the start , chip address and not write bit
           cdata<='0';        
         else
            cdata<=data_buffer(23); -- else start flushing data out and bit shifting,
            data_buffer<=data_buffer(22 downto 0)&'0'; -- flush data the buffer out  , should only do this wwhile ou are in transaction 
         end if;        
      end if; 
    end if;
end process;
end Behavioral;
