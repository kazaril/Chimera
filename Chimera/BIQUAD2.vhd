----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 17.08.2015 17:14:49
-- Design Name: 
-- Module Name: BIQUAD - Behavioral
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

-------y[n] = a0*x[n] + a1*x[n-1] + a2*x[n-2] - b1*y[n-1] - b2*y[n-2] BIQUAD FILTER

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity BIQUAD2 is
    Port ( clk : in STD_LOGIC;
           a0 : in STD_LOGIC_VECTOR (15 downto 0);  --coefficients are 1-65000 full scale
           a1 : in STD_LOGIC_VECTOR (15 downto 0);
           a2 : in STD_LOGIC_VECTOR (15 downto 0);
           b1 : in STD_LOGIC_VECTOR (15 downto 0);
           b2 : in STD_LOGIC_VECTOR (15 downto 0);
           S : in STD_LOGIC_VECTOR (15 downto 0); --audio in
           S_out : out STD_LOGIC_VECTOR (15 downto 0)); --audio out
end BIQUAD2;

architecture Behavioral of BIQUAD2 is



signal x0 : std_logic_vector(31 downto 0); ---post multiply
signal x1 : std_logic_vector(31 downto 0);
signal x2 : std_logic_vector(31 downto 0);
signal y1 : std_logic_vector(31 downto 0);
signal y2 : std_logic_vector(31 downto 0);


signal div : std_logic_vector(15 downto 0):=x"2000";
signal S_out_buff : STD_LOGIC_VECTOR(31 downto 0);

begin


process(clk) begin
if(clk'event and clk='1') then

        S_out_buff <= (x0(31 downto 16)*a0 + x1(31 downto 16)*a1 + x2(31 downto 16)*a2 - y1(31 downto 16)*b1 - y2(31 downto 16)*b2); 
        y1 <= x0(31 downto 16)*a0 + x1(31 downto 16)*a1 + x2(31 downto 16)*a2 - y1(31 downto 16)*b1 - y2(31 downto 16)*b2;  
        y2 <= y1;
        
        x1 <= x"0000" & S;
        x2 <= x1;
    
    S_out <= S_out_buff(31 downto 16);
end if;
end process;

end Behavioral;
