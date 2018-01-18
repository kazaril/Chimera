----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 13.10.2015 01:28:11
-- Design Name: 
-- Module Name: ENVELOPE_GEN - Behavioral
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


--     /\d
--    /  \_____
--  a/      s  \ r
--  /           \

entity ENVELOPE_GEN is
    Port ( clk : in STD_LOGIC;
           clkS : in std_logic;
           A : in STD_LOGIC_VECTOR (6 downto 0);
           D : in STD_LOGIC_VECTOR (6 downto 0);
           S : in STD_LOGIC_VECTOR (6 downto 0);
           R : in STD_LOGIC_VECTOR (6 downto 0);
           Trigger : in STD_LOGIC;
           note_off : in std_logic;
           MIDI_VELOCITY : in STD_LOGIC_VECTOR (6 downto 0);
           VAL_OUT : out integer range 0 to 127);
end ENVELOPE_GEN;

architecture Behavioral of ENVELOPE_GEN is

signal state : std_logic_vector(2 downto 0);  --keeps track of where we are in the envelope
signal full_scale : integer range 0 to 65535;--std_logic_vector(15 downto 0);
signal A_count : std_logic_vector(6 downto 0);
signal triggered : std_logic;
signal curr_val : std_logic_vector(15 downto 0);
signal A_rate : integer range 0 to 65535;
signal timerizer : integer range 0 to 375;  ----Mkes 127 = 1 second. 
signal trig_flag : std_logic;
signal trig_flag_o : std_logic;
signal triggered_o : std_logic;
signal offed : std_logic;

begin

process(clk) begin       ----------pass trigger val to slower clock.
if (clk'event and clk='1')then
    if(trigger='1')then
        triggered <= '1';
    elsif(trig_flag = '1')then
        triggered <= '0';
    end if;
 
     if(note_off='1')then
        triggered_o <= '1';
    elsif(trig_flag_o = '1')then
        triggered_o <= '0';
    end if;   
    
end if;
end process;

process(clkS) begin
if (clkS'event and clkS='1')then
    
    if(triggered_o = '1')then
    
        if(offed='1')then
            trig_flag_o<='0';
            offed<='0';
        else
            trig_flag_o <= '1';
            offed<='1';            
        end if;
        
        curr_val <=x"0000";
        
    elsif(state = "000" and triggered = '1')then
        full_scale <= to_integer(unsigned(MIDI_VELOCITY & "111111111")) ;
        A_rate <= full_scale/to_integer(unsigned(A));
        trig_flag<='1';
        A_count<=(others => '0');
        curr_val <= x"0000";
        state <= "001";
    elsif (state = "001")then
         trig_flag <='0';
         if(timerizer < 374)then
            timerizer<=timerizer+1;
         else
            timerizer<=0;   
            
            if(A_count < A)then
                A_count<=A_count+1;
                if((curr_val+A_rate) < full_scale)then 
                    curr_val<=curr_val+A_rate;
                else
                    curr_val <= MIDI_VELOCITY & "111111111";
                end if;
             else
                state<= "000";
             end if;
          end if;
      elsif(state = "010")then
        
      
    end if;    
end if;

VAL_OUT <= (to_integer(unsigned(curr_val(15 downto 9))));
end process;



end Behavioral;
