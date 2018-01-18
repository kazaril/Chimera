---------------Contains a case statement to select the DDS_RATE


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.numeric_std.all;

entity DDS_DRIVER is
    Port ( clk : in STD_LOGIC;
           MIDI_NOTE : in STD_LOGIC_VECTOR (7 downto 0);
           DDS_RATE : out STD_LOGIC_VECTOR (19 downto 0));
end DDS_DRIVER;

architecture Behavioral of DDS_DRIVER is




begin
process(clk) begin
if(clk'event and clk='1') then

    
    case MIDI_NOTE is

                WHEN x"15" => DDS_RATE <= x"00323";
                
                WHEN x"16" => DDS_RATE <= x"00352";
                
                WHEN x"17" => DDS_RATE <= x"00385";
                
                WHEN x"18" => DDS_RATE <= x"003bb";
                
                WHEN x"19" => DDS_RATE <= x"003f3";
                
                WHEN x"1a" => DDS_RATE <= x"0042f";
                
                WHEN x"1b" => DDS_RATE <= x"0046f";
                
                WHEN x"1c" => DDS_RATE <= x"004b3";
                
                WHEN x"1d" => DDS_RATE <= x"004fa";
                
                WHEN x"1e" => DDS_RATE <= x"00546";
                
                WHEN x"1f" => DDS_RATE <= x"00596";
                
                WHEN x"20" => DDS_RATE <= x"005eb";
                
                WHEN x"21" => DDS_RATE <= x"00645";
                
                WHEN x"22" => DDS_RATE <= x"006a5";
                
                WHEN x"23" => DDS_RATE <= x"0070a";
                
                WHEN x"24" => DDS_RATE <= x"00775";
                
                WHEN x"25" => DDS_RATE <= x"007e7";
                
                WHEN x"26" => DDS_RATE <= x"0085f";
                
                WHEN x"27" => DDS_RATE <= x"008de";
                
                WHEN x"28" => DDS_RATE <= x"00965";
                
                WHEN x"29" => DDS_RATE <= x"009f4";
                
                WHEN x"2a" => DDS_RATE <= x"00a8c";
                
                WHEN x"2b" => DDS_RATE <= x"00b2c";
                
                WHEN x"2c" => DDS_RATE <= x"00bd7";
                
                WHEN x"2d" => DDS_RATE <= x"00c8b";
                
                WHEN x"2e" => DDS_RATE <= x"00d4a";
                
                WHEN x"2f" => DDS_RATE <= x"00e14";
                
                WHEN x"30" => DDS_RATE <= x"00eea";
                
                WHEN x"31" => DDS_RATE <= x"00fcd";
                
                WHEN x"32" => DDS_RATE <= x"010be";
                
                WHEN x"33" => DDS_RATE <= x"011bd";
                
                WHEN x"34" => DDS_RATE <= x"012cb";
                
                WHEN x"35" => DDS_RATE <= x"013e9";
                
                WHEN x"36" => DDS_RATE <= x"01518";
                
                WHEN x"37" => DDS_RATE <= x"01659";
                
                WHEN x"38" => DDS_RATE <= x"017ad";
                
                WHEN x"39" => DDS_RATE <= x"01915";
                
                WHEN x"3a" => DDS_RATE <= x"01a93";
                
                WHEN x"3b" => DDS_RATE <= x"01c28";
                
                WHEN x"3c" => DDS_RATE <= x"01dd4";
                
                WHEN x"3d" => DDS_RATE <= x"01f9a";
                
                WHEN x"3e" => DDS_RATE <= x"0217c";
                
                WHEN x"3f" => DDS_RATE <= x"02379";
                
                WHEN x"40" => DDS_RATE <= x"02595";
                
                WHEN x"41" => DDS_RATE <= x"027d1";
                
                WHEN x"42" => DDS_RATE <= x"02a2f";
                
                WHEN x"43" => DDS_RATE <= x"02cb2";
                
                WHEN x"44" => DDS_RATE <= x"02f5a";
                
                WHEN x"45" => DDS_RATE <= x"0322b";
                
                WHEN x"46" => DDS_RATE <= x"03527";
                
                WHEN x"47" => DDS_RATE <= x"03850";
                
                WHEN x"48" => DDS_RATE <= x"03ba9";
                
                WHEN x"49" => DDS_RATE <= x"03f35";
                
                WHEN x"4a" => DDS_RATE <= x"042f7";
                
                WHEN x"4b" => DDS_RATE <= x"046f3";
                
                WHEN x"4c" => DDS_RATE <= x"04b2b";
                
                WHEN x"4d" => DDS_RATE <= x"04fa3";
                
                WHEN x"4e" => DDS_RATE <= x"0545f";
                
                WHEN x"4f" => DDS_RATE <= x"05963";
                
                WHEN x"50" => DDS_RATE <= x"05eb4";
                
                WHEN x"51" => DDS_RATE <= x"06456";
                
                WHEN x"52" => DDS_RATE <= x"06a4d";
                
                WHEN x"53" => DDS_RATE <= x"0709f";
                
                WHEN x"54" => DDS_RATE <= x"07752";
                
                WHEN x"55" => DDS_RATE <= x"07e6a";
                
                WHEN x"56" => DDS_RATE <= x"085ee";
                
                WHEN x"57" => DDS_RATE <= x"08de5";
                
                WHEN x"58" => DDS_RATE <= x"09655";
                
                WHEN x"59" => DDS_RATE <= x"09f45";
                
                WHEN x"5a" => DDS_RATE <= x"0a8be";
                
                WHEN x"5b" => DDS_RATE <= x"0b2c7";
                
                WHEN x"5c" => DDS_RATE <= x"0bd68";
                
                WHEN x"5d" => DDS_RATE <= x"0c8ab";
                
                WHEN x"5e" => DDS_RATE <= x"0d49a";
                
                WHEN x"5f" => DDS_RATE <= x"0e13e";
                
                WHEN x"60" => DDS_RATE <= x"0eea3";
                
                WHEN x"61" => DDS_RATE <= x"0fcd4";
                
                WHEN x"62" => DDS_RATE <= x"10bdd";
                
                WHEN x"63" => DDS_RATE <= x"11bca";
                
                WHEN x"64" => DDS_RATE <= x"12caa";
                
                WHEN x"65" => DDS_RATE <= x"13e8b";
                
                WHEN x"66" => DDS_RATE <= x"1517c";
                
                WHEN x"67" => DDS_RATE <= x"1658d";
                
                WHEN x"68" => DDS_RATE <= x"17ad0";
                
                WHEN x"69" => DDS_RATE <= x"19157";
                
                WHEN x"6a" => DDS_RATE <= x"1a934";
                
                WHEN x"6b" => DDS_RATE <= x"1c27d";
                
                WHEN x"6c" => DDS_RATE <= x"1dd46";
                
                WHEN x"6d" => DDS_RATE <= x"1f9a8";
                
                WHEN x"6e" => DDS_RATE <= x"217b9";
                
                WHEN x"6f" => DDS_RATE <= x"23794";
                
                WHEN x"70" => DDS_RATE <= x"25954";
                
                WHEN x"71" => DDS_RATE <= x"27d16";
                
                WHEN x"72" => DDS_RATE <= x"2a2f8";
                
                WHEN x"73" => DDS_RATE <= x"2cb1b";
                
                WHEN x"74" => DDS_RATE <= x"2f5a0";
                
                WHEN x"75" => DDS_RATE <= x"322ad";
                
                WHEN x"76" => DDS_RATE <= x"35268";
                
                WHEN x"77" => DDS_RATE <= x"384fa";
                
                WHEN x"78" => DDS_RATE <= x"3ba8d";
                
                WHEN x"79" => DDS_RATE <= x"3f34f";
                
                WHEN x"7a" => DDS_RATE <= x"42f72";
                
                WHEN x"7b" => DDS_RATE <= x"46f28";
                
                WHEN x"7c" => DDS_RATE <= x"4b2a8";
                
                WHEN x"7d" => DDS_RATE <= x"4fa2c";
                
                WHEN x"7e" => DDS_RATE <= x"545f0";
                
                WHEN x"7f" => DDS_RATE <= x"59635";
    
        WHEN others => DDS_RATE <=       x"10000";
    end case;
end if;
end process;
end Behavioral;
