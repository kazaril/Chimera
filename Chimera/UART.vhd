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
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity uart is
   generic (
			CLK_FREQ : integer;	-- master clock frequency 
			BAUDRATE : integer
	);
	Port ( 
			reset : in  STD_LOGIC;
			clk : in  STD_LOGIC;
			RXD : in STD_LOGIC;
			TXD : out STD_LOGIC;
			RXD_ready : out STD_LOGIC;
			RXD_act : out STD_LOGIC; -- '1' when receiving data '0' otherwise
			TXD_ready : out STD_LOGIC;
			TXD_start : in STD_LOGIC;
			RXD_data : out STD_LOGIC_VECTOR (7 downto 0);
			TXD_data : in STD_LOGIC_VECTOR (7 downto 0)
	);
end uart;
architecture rtl of uart is
	constant BAUD16_ACC_WIDTH : integer := 16;
	constant BAUD16_ACC_PRESCALER : integer := integer(real(2**BAUD16_ACC_WIDTH)/(real(CLK_FREQ)/real(BAUDRATE*16)));
	signal baud16_acc : STD_LOGIC_VECTOR (BAUD16_ACC_WIDTH - 1 downto 0);
	signal baud_tick : STD_LOGIC;
	signal baud16_clk : STD_LOGIC;
	signal baud16_clk_q : STD_LOGIC;
	signal baud16_tick : STD_LOGIC;
	signal TXD_data_i : STD_LOGIC_VECTOR (7 downto 0);
	signal TXD_start_i : STD_LOGIC;
	signal RXD_data_i : STD_LOGIC_VECTOR (7 downto 0);
	signal RXD_filtered : STD_LOGIC;
	type states is (start,send);
	signal tx_state : states;

begin
	baud_gen : process(reset,clk)
	variable baud_cnt : integer range 0 to 15;
	begin
		if reset = '1' then
			baud16_clk <= '0';
			baud16_clk_q <= '0';
			baud16_acc <= (others => '0');
			baud_tick <= '0';
			baud_cnt := 0;
		elsif clk'event and clk = '1' then
			baud16_acc <= baud16_acc + conv_std_logic_vector(BAUD16_ACC_PRESCALER,BAUD16_ACC_WIDTH);
			baud16_clk_q <= baud16_clk;
			baud16_clk <= baud16_acc(BAUD16_ACC_WIDTH-1);
			baud_tick <= '0';
			if baud16_tick = '1' then
				if baud_cnt = 15 then
					baud_tick <= '1';
					baud_cnt := 0;
				else
					baud_cnt := baud_cnt + 1;
				end if;
			end if;
		end if;
	end process baud_gen;
	
	baud16_tick <= '1' when (baud16_clk = '1' and baud16_clk_q = '0') else '0';
	
	transmit : process(reset,clk)
	variable TXD_bit_cnt : integer range 0 to 8;
	begin
		if reset = '1' then
			TXD <= '1';
			TXD_bit_cnt := 0;
			TXD_ready <= '1';
		elsif clk'event and clk = '1' then
			if TXD_start = '1' then
				TXD_start_i <= '1';
			end if;
			if baud_tick = '1' then
				case (tx_state) is
					when start =>
						if TXD_start_i = '1' then
							TXD_start_i <= '0';
							tx_state <= send;
							TXD <= '0';
							TXD_ready <= '0';
							TXD_data_i <= TXD_data;
						end if;
					when send =>
						if TXD_bit_cnt = 8 then
							TXD_ready <= '1';
							TXD <= '1';
							TXD_bit_cnt := 0;
							tx_state <= start;
						else
							TXD <= TXD_data_i(0);
							TXD_data_i <= '0' & TXD_data_i(7 downto 1);
							TXD_bit_cnt := TXD_bit_cnt + 1;
						end if;
					when others =>
						NULL;
				end case;
			end if;
		end if;
	end process transmit;
	
	receive_filter : process(reset,clk)
	variable RXD_samples : STD_LOGIC_VECTOR(1 downto 0);
	begin
		if reset = '1' then
			RXD_samples := "00";
			RXD_filtered <= '1';
		elsif clk'event and clk = '1' then
			if baud16_tick = '1' then
				RXD_samples(1) := RXD_samples(0);
				RXD_samples(0) := RXD;
			end if;
			if RXD_samples = "00" then
				RXD_filtered <= '0';
			end if;
			if RXD_samples = "11" then
				RXD_filtered <= '1';
			end if;
		end if;
	end process receive_filter;
	
	receive : process(reset,clk)
	variable RXD_bit_cnt : integer range 0 to 10;
	variable baud16_tick_cnt: integer range 0 to 15;
	begin
		if reset = '1' then
			RXD_ready <= '0';
			RXD_bit_cnt := 0;
			baud16_tick_cnt := 0;
		elsif clk'event and clk = '1' then
			RXD_ready <= '0';
			if baud16_tick = '1' then
				-- sync to start bit and sample on 13th baud_tick16
				if (RXD_bit_cnt = 0) then
					RXD_act <= '0';
					if (RXD_filtered = '1') then 
						baud16_tick_cnt := 0;
					else
						if (baud16_tick_cnt = 12) then
							baud16_tick_cnt := 0;
							RXD_bit_cnt := RXD_bit_cnt + 1;
						else
							baud16_tick_cnt := baud16_tick_cnt + 1;
						end if;
					end if;
				else
					RXD_act <= '1';
					if (baud16_tick_cnt = 15) then
						baud16_tick_cnt := 0;
						if (RXD_bit_cnt = 9) then -- stop bit
							RXD_bit_cnt := 0;
							RXD_ready <= '1';
							RXD_data <= RXD_data_i;
						else
							RXD_bit_cnt := RXD_bit_cnt + 1;
							RXD_data_i <= RXD_filtered & RXD_data_i(7 downto 1);
						end if;
					else
						baud16_tick_cnt := baud16_tick_cnt + 1;
					end if;
				end if;
			end if;
		end if;
	end process receive;

end rtl;