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
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity BIQUAD is
    Port ( clk : in STD_LOGIC;
         clkS : in std_logic;
           a0 : in STD_LOGIC_VECTOR (15 downto 0);  --coefficients are 1-65000 full scale
           a1 : in STD_LOGIC_VECTOR (15 downto 0);
           a2 : in STD_LOGIC_VECTOR (15 downto 0);
           b1 : in STD_LOGIC_VECTOR (15 downto 0);
           b2 : in STD_LOGIC_VECTOR (15 downto 0);
           SWS : in std_logic_vector(3 downto 0);
           S : in STD_LOGIC_VECTOR (15 downto 0); --audio in
           S_out : out STD_LOGIC_VECTOR (15 downto 0)); --audio out
end BIQUAD;

architecture Behavioral of BIQUAD is

COMPONENT div_gen_0
  PORT (
    aclk : IN STD_LOGIC;
    s_axis_divisor_tvalid : IN STD_LOGIC;
    s_axis_divisor_tdata : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    s_axis_dividend_tvalid : IN STD_LOGIC;
    s_axis_dividend_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    m_axis_dout_tvalid : OUT STD_LOGIC;
    m_axis_dout_tdata : OUT STD_LOGIC_VECTOR(47 DOWNTO 0)
  );
END COMPONENT;






COMPONENT mult_BIQUAD
  PORT (clk : in std_logic;
    A : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    B : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    P : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
  );
END COMPONENT;

signal x0 : std_logic_vector(31 downto 0); ---post multiply
signal x1 : std_logic_vector(31 downto 0);
signal x2 : std_logic_vector(31 downto 0);
signal y1 : std_logic_vector(31 downto 0);
signal y2 : std_logic_vector(31 downto 0);
signal x0_d : std_logic_vector(15 downto 0);  --after division
signal x1_d : std_logic_vector(15 downto 0);
signal x2_d : std_logic_vector(15 downto 0);
signal y1_d : std_logic_vector(15 downto 0);
signal y2_d : std_logic_vector(15 downto 0);

signal sY1 : std_logic_vector(15 downto 0); ---Buffers
signal sY2 : std_logic_vector(15 downto 0); 
signal sX1 : std_logic_vector(15 downto 0); 
signal sX2 : std_logic_vector(15 downto 0); 
signal SOUT_pre : std_logic_vector(31 downto 0);
signal Sout_d  : std_logic_vector(31 downto 0);
signal cnt : integer range 0 to 3;

signal dumb1 : std_logic_vector(15 downto 0);
signal dumb2 : std_logic_vector(15 downto 0);

begin

x0_a0: mult_BIQUAD
PORT MAP(A => S,
        B => a0,
        P => x0,
        clk => clk
        ); 

x1_a1: mult_BIQUAD
PORT MAP(A => sX1,
        B => a1,
        P => x1,
        clk => clk
        ); 

x2_a2: mult_BIQUAD
PORT MAP(A => sX2,
        B => a2,
        P => x2,
        clk => clk
        ); 

y1_b1: mult_BIQUAD
PORT MAP(A => sY1,
        B => b1,
        P => y1,
        clk => clk
        ); 

y2_b2: mult_BIQUAD
PORT MAP(A => sY2,
        B => b2,
        P => y2,
        clk => clk
        ); 

S_div: div_gen_0
  PORT MAP (
    aclk => clk,
    s_axis_divisor_tvalid => '1',
    s_axis_divisor_tdata => "000000000000" & SWS,
    s_axis_dividend_tvalid => '1', 
    s_axis_dividend_tdata => Sout_pre,
    m_axis_dout_tdata(47 downto 32) => dumb1,
    m_axis_dout_tdata(31 downto 0) => Sout_d

  );



--U1: ila_0
--  PORT MAP(
--    clk => clk,
--    probe0 => Sout_d(15 downto 16),
--    probe1 => Sout_pre
--    );



process(clkS) begin
if(clkS'event and clkS='1') then

        Sout_pre <= x0 + x1 + x2 - y1 - y2; 
        sY1 <= Sout_d(31 downto 16);
        S_out <= Sout_d(31 downto 16);
        sY2 <= sY1;
        
        sX1 <= S;
        sX2 <= sX1;


end if;
end process;

end Behavioral;
