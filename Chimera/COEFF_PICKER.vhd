----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02.10.2015 20:26:18
-- Design Name: 
-- Module Name: COEFF_PICKER - Behavioral
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity COEFF_PICKER is
    Port ( clk : in STD_LOGIC;
           clkS : in STD_LOGIC;
           a0 : in STD_LOGIC_VECTOR (31 downto 0);
           a1 : in STD_LOGIC_VECTOR (31 downto 0);
           a2 : in STD_LOGIC_VECTOR (31 downto 0);
           b1 : in STD_LOGIC_VECTOR (31 downto 0);
           b2 : in STD_LOGIC_VECTOR (31 downto 0);
           f : in STD_LOGIC_VECTOR (11 downto 0);
           Q : in STD_LOGIC_VECTOR (7 downto 0));
end COEFF_PICKER;

architecture Behavioral of COEFF_PICKER is

COMPONENT Tan_simulator
  PORT (
    aclk : IN STD_LOGIC;
    s_axis_a_tvalid : IN STD_LOGIC;
    s_axis_a_tready : OUT STD_LOGIC;
    s_axis_a_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    s_axis_b_tvalid : IN STD_LOGIC;
    s_axis_b_tready : OUT STD_LOGIC;
    s_axis_b_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    m_axis_result_tvalid : OUT STD_LOGIC;
    m_axis_result_tdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
  );
END COMPONENT;

begin




end Behavioral;
