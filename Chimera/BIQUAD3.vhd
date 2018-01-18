----------------------------------------------------------------------------------
--    SK-Synth - FPGA-Synthesizer
--    Copyright (C) 2009  Stefan Kristiansson
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.SK_Synth_pack.ALL;
-- Biquad IIR filter using the formula below (direct form 1 with a0 and a2 substituted into q)
-- y0 = q*(b0*x0 + b1*x1 + b2*x2 + a1*y1 - 2*y2) + y2
-- where q = qmin + qin*qmul 
entity IIR_filter is
	Port ( 
			reset : in  STD_LOGIC;
			clk : in  STD_LOGIC;
			qin : in unsigned(7 downto 0);
			qmin : in unsigned(21 downto 0);
			qmul : in unsigned(21 downto 0);
			a1 : in unsigned(21 downto 0);
			b0 : in unsigned(21 downto 0);
			b1 : in unsigned(21 downto 0);
			b2 : in unsigned(21 downto 0);
			voicenr : in integer range 0 to NUM_VOICES - 1;
			input_ready : in STD_LOGIC;
			input : in signed(15 downto 0);
			output_ready : out STD_LOGIC;
			output : out signed((FILTER_WIDTH-1)-8 downto 0)
	);
end IIR_filter;

architecture rtl of IIR_filter is
	-- Operands are fixed point (FILTER_WIDTH-8):8
	type xyram is array(0 to NUM_VOICES*4 -1) of signed(FILTER_WIDTH-1 downto 0);
	signal xy : xyram;
	signal xy_we : STD_LOGIC;
	signal xy_di : signed(FILTER_WIDTH-1 downto 0);
	signal xy_do : signed(FILTER_WIDTH-1 downto 0);
	signal xy_addr : integer range 0 to NUM_VOICES*4 -1;

	signal y0_i,inv_y0_i 	: signed(FILTER_WIDTH*2-1 downto 0);
	signal q		: signed(FILTER_WIDTH-1 downto 0);
	signal mul_op1 		: signed(FILTER_WIDTH-1 downto 0);
	signal mul_op2 		: signed(FILTER_WIDTH-1 downto 0);
	signal mul_res 		: signed(FILTER_WIDTH*2-1 downto 0);
	signal neg : STD_LOGIC;
	type states is (s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13);
	signal IIR_state : states;
begin
	-- generate BRAM
	process(clk)
	begin
		if clk'event and clk='1' then
			if xy_we = '1' then
				xy(xy_addr) <= xy_di;
			end if;
			xy_do <= xy(xy_addr);
		end if;
	end process;
	
	process(clk)
	begin
		if clk'event and clk = '1' then
			output_ready <= '0';
			xy_we <= '0';
			case (IIR_state) is
				when s1 =>
					if input_ready = '1' then		
						xy_di <= resize(input & X"00", FILTER_WIDTH);
						xy_addr <= voicenr+NUM_VOICES*0; -- x1;
						xy_we <= '1';
						q <= signed(resize('0' & qmin, FILTER_WIDTH));
						mul_op1 <= signed(resize('0' & qin, FILTER_WIDTH));
						mul_op2 <= signed(resize('0' & qmul, FILTER_WIDTH));
						IIR_state <= s2;
					end if;
				when s2 =>
					xy_addr <= voicenr+NUM_VOICES*1; -- x2;
					mul_op1 <= xy_di; --x0;
					mul_op2 <= signed(resize('0' & b0,FILTER_WIDTH));
					IIR_state <= s3;					
				when s3 =>
					xy_di <= xy_do; -- x2 <= x1
					xy_we <= '1';
					q <= q + mul_res(FILTER_WIDTH-1 downto 0); -- q = qmin + qin*qmul
					mul_op1 <= xy_do; -- x1;
					mul_op2 <= signed(resize('0' & b1,FILTER_WIDTH));
					IIR_state <= s4;
				when s4 =>
					xy_addr <= voicenr+NUM_VOICES*2; -- y1;
					y0_i <= mul_res; -- y0 = b0*x0
					mul_op1 <= xy_do; -- x2;
					mul_op2 <= signed(resize('0' & b2,FILTER_WIDTH));
					IIR_state <= s5;
				when s5 =>
					xy_addr <= voicenr+NUM_VOICES*3; -- y2;
					y0_i <= y0_i + mul_res; -- y0 = b0*x0 + b1*x1
					IIR_state <= s6;
				when s6 =>	
					xy_di <= xy_do; -- y2 <= y1
					mul_op1 <= xy_do; -- y1
					mul_op2 <= signed(resize('0' & a1 & "000",FILTER_WIDTH)); -- shift a1 to get a total multiplier of 2^23
					IIR_state <= s7;
				when s7 =>
					y0_i <= y0_i + mul_res; -- y0 = b0*x0 + b1*x1 + b2*x2
					mul_op1 <= resize(xy_do,FILTER_WIDTH); -- y2
					mul_op2 <= to_signed(2,FILTER_WIDTH);
					IIR_state <= s8;
				when s8 =>
					y0_i <= y0_i + mul_res; -- y0 = b0*x0 + b1*x1 + b2*x2 + a1*y1
					inv_y0_i <= (not (y0_i + mul_res))+1;
					IIR_state <= s9;
				when s9 =>
					-- here we divide y0 with 2^23
					if y0_i(FILTER_WIDTH*2-1) = '0' then --positive
						mul_op1 <= resize(y0_i(FILTER_WIDTH*2-1 downto 23) - mul_res,FILTER_WIDTH); -- y0 = b0*x0 + b1*x1 + b2*x2 + a1*y1 - 2*y2 (y0/2^23 - 2*y2)
					else -- negative
						mul_op1 <= resize((not (inv_y0_i(FILTER_WIDTH*2-1 downto 23))+1) - mul_res,FILTER_WIDTH); -- y0 = b0*x0 + b1*x1 + b2*x2 + a1*y1 - 2*y2 (y0/2^23 - 2*y2)
					end if;
					mul_op2 <= q;
					IIR_state <= s10;
				when s10 => 
					IIR_state <= s11;
				when s11 =>
					-- xy_do contains y2 here
					if mul_res(FILTER_WIDTH*2-1) = '0' then --positive
						-- divide with q's multiplier
						y0_i <= resize((mul_res(FILTER_WIDTH*2-1 downto 22)), FILTER_WIDTH*2) + xy_do; -- y0 = q*(b0*x0 + b1*x1 + b2*x2 + a1*y1 - 2*y2) + y2;
						neg <= '0';
					else
						inv_y0_i <= (not mul_res) + 1;
						neg <= '1';
					end if;
					IIR_state <= s12;
				when s12 =>
					if neg = '1' then
						y0_i <= resize(((not (inv_y0_i(FILTER_WIDTH*2-1 downto 22)))+1),FILTER_WIDTH*2) + xy_do; -- y0 = q*(b0*x0 + b1*x1 + b2*x2 + a1*y1 - 2*y2) + y2;
					end if;
					-- xy_di is loaded with y1 in s6
					-- xy_a is loaded with y2 adress in s5
					xy_we <= '1';
					IIR_state <= s13;					
				when s13 =>
					xy_addr <= voicenr+NUM_VOICES*2; --y1
					xy_we <= '1';
					xy_di <= y0_i(FILTER_WIDTH-1 downto 0);
					-- Check for overflows
					if y0_i < -(2**(FILTER_WIDTH-1)-1) then
						xy_di <= to_signed((2**(FILTER_WIDTH-1)), FILTER_WIDTH);
						y0_i <= to_signed((2**(FILTER_WIDTH-1)), FILTER_WIDTH*2);
					elsif y0_i > (2**(FILTER_WIDTH-1)-1) then
						xy_di <= to_signed((2**(FILTER_WIDTH-1)-1), FILTER_WIDTH);
						y0_i <= to_signed((2**(FILTER_WIDTH-1)-1), FILTER_WIDTH*2);
					end if;
					output_ready <= '1';
					IIR_state <= s1;
				when others => null;
			end case;
			mul_res <= mul_op1*mul_op2;
		end if;
	end process;
	output <= y0_i(FILTER_WIDTH-1 downto 8);
end rtl;

