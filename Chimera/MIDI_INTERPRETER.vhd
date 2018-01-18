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

entity MIDI_INTERPRETER is
		generic (
			CLK_FREQ : integer
		);
		Port ( 		
			reset	: in STD_LOGIC;
			clk : in STD_LOGIC;
			RXD : in STD_LOGIC;
			TXD : out STD_LOGIC;
			LED : out STD_LOGIC;
			pitchwheel_change : out STD_LOGIC;
			pitchwheel : out STD_LOGIC_VECTOR(13 downto 0);
			note_on : out STD_LOGIC;
			note_off : out STD_LOGIC;
			note : out STD_LOGIC_VECTOR(7 downto 0);
			velocity : out STD_LOGIC_VECTOR(7 downto 0);
			control_change : out STD_LOGIC;
			control_change_num : out STD_LOGIC_VECTOR(7 downto 0);
			control_change_value : out STD_LOGIC_VECTOR(7 downto 0);
			chan : out STD_LOGIC_VECTOR(3 downto 0)
	);
end MIDI_INTERPRETER;

architecture rtl of MIDI_INTERPRETER is
	component uart
		generic (
			CLK_FREQ : integer;
			BAUDRATE : integer
	);
	Port ( 
			reset : in  STD_LOGIC;
			clk : in  STD_LOGIC;
			RXD : in STD_LOGIC;
			TXD : out STD_LOGIC;
			RXD_ready : out STD_LOGIC;
			RXD_act : out STD_LOGIC;
			TXD_ready : out STD_LOGIC;
			TXD_start : in STD_LOGIC;
			RXD_data : out STD_LOGIC_VECTOR (7 downto 0);
			TXD_data : in STD_LOGIC_VECTOR (7 downto 0)
		);
	end component;
	for uart_0 : uart use entity work.uart(rtl);
	signal uart_0_RXD_data : STD_LOGIC_VECTOR (7 downto 0);
	signal uart_0_TXD_data : STD_LOGIC_VECTOR (7 downto 0);
	signal uart_0_RXD_ready : STD_LOGIC;
	signal uart_0_TXD_ready : STD_LOGIC;
	signal uart_0_TXD_start : STD_LOGIC;
	
	signal status_byte : STD_LOGIC_VECTOR (7 downto 0);
	signal realtime_status_byte : STD_LOGIC_VECTOR (7 downto 0);
	type data_byte_t is array(0 to 1) of STD_LOGIC_VECTOR (7 downto 0);
	signal data_byte : data_byte_t;
	signal message_ready : STD_LOGIC;
	signal realtime_message_ready : STD_LOGIC;
begin
	uart_0 : uart 	generic map (
		CLK_FREQ => CLK_FREQ,
		BAUDRATE => 31250
	)
	port map (
			reset => reset,
			clk => clk,
			RXD => RXD,
			TXD => TXD,
			RXD_ready => uart_0_RXD_ready,
			RXD_act => LED,
			TXD_ready => uart_0_TXD_ready,
			TXD_start => uart_0_TXD_start,
			RXD_data => uart_0_RXD_data,
			TXD_data => uart_0_TXD_data
	);
	
	receive_data : process(reset,clk)
	variable data_cnt: integer range 0 to 1;
	begin
		if reset = '1' then
			message_ready <= '0';
			realtime_message_ready <= '0';
			data_cnt := 0;
		elsif clk'event and clk = '1' then
			message_ready <= '0';
			realtime_message_ready <= '0';
			if uart_0_RXD_ready = '1' then
				if uart_0_RXD_data(7) = '1' then -- status byte
					if uart_0_RXD_data(7 downto 3) = "11111" then -- take care of realtime messages seperately
						realtime_status_byte <= uart_0_RXD_data;
						realtime_message_ready <= '1';
					else
						status_byte <= uart_0_RXD_data;
						data_cnt := 0;
						if uart_0_RXD_data > X"F3" then	-- no data bytes
							message_ready <= '1';
						end if;
					end if;
				else	-- data byte
					data_byte(data_cnt) <= uart_0_RXD_data;
					if (status_byte > X"BF" and status_byte < X"E0") or (status_byte = X"F3") then -- only 1 data byte
						message_ready <= '1';
					end if;
					if data_cnt = 1 then
						data_cnt := 0;						
						message_ready <= '1';
					else
						data_cnt := data_cnt + 1;
					end if;
				end if;
			end if;
		end if;
	end process receive_data;

	process_message : process(reset,clk)
	begin
		if reset = '1' then
			chan <= (others => '0');
			note_on <= '0';
			velocity <= (others => '0');
			pitchwheel <= "10000000000000";	-- 0x2000
		elsif clk'event and clk = '1' then
			note_on <= '0';
			note_off <= '0';
			control_change <= '0';
			pitchwheel_change <= '0';
			if message_ready = '1' then
				case (status_byte(7 downto 4)) is
					when "1000" =>	-- Note Off event
						note_off <= '1';
						chan <= status_byte(3 downto 0);
						note <= data_byte(0);
						velocity <= data_byte(1);
					when "1001" => -- Note On event
						if data_byte(1) = X"00" then -- a note on with 0 velocity should be considered as a note off
							note_off <= '1';
							velocity <= X"40";
						else
							note_on <= '1';
							velocity <= data_byte(1);
						end if;
						chan <= status_byte(3 downto 0);
						note <= data_byte(0);
					when "1010" => -- Polyphonic Key Pressure (Aftertouch)
					when "1011" => -- Control Change/Channel Mode Messages
						chan <= status_byte(3 downto 0);
						control_change <= '1';
						control_change_num <= data_byte(0);
						control_change_value <= data_byte(1);
					when "1100" => -- Program Change
					when "1101" => -- Channel Pressure (After-touch)
					when "1110" => -- Pitch Wheel Change
						pitchwheel_change <= '1';
						pitchwheel <= data_byte(1)(6 downto 0) & 	data_byte(0)(6 downto 0);
						chan <= status_byte(3 downto 0);
					when "1111" => -- System Common messages (system realtime messages is taken care of seperately)					
					when others =>
						NULL;
				end case;
			end if;
		end if;
	end process process_message;
	
	process_realtime_message : process(reset,clk)
	begin
		if reset = '1' then
		elsif clk'event and clk = '1' then
			if realtime_message_ready = '1' then
				case (realtime_status_byte(2 downto 0)) is
					when "000" => -- Timing clock
					when "001" => -- Undefined (Reserved)
					when "010" => -- Start
					when "011" => -- Continue
					when "100" => -- Stop
					when "101" => -- Undefined (Reserved)
					when "110" => -- Active Sensing
					when "111" => -- System Reset
					when others =>
						NULL;
				end case;
			end if;
		end if;
	end process process_realtime_message;
	
	-- MIDI thru
	process(clk)
	begin
		if clk'event and clk = '1' then
			uart_0_TXD_start <= '0';
			if uart_0_RXD_ready = '1' then
				uart_0_TXD_data <= uart_0_RXD_data;
				uart_0_TXD_start <= '1';
			end if;
		end if;
	end process;
end rtl;


