--Writes to up to 4 BRAMS for waveform storage. HAS DDS for each, sums them.


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;




entity STORE_PRIMARY is
    Port ( clk : in STD_LOGIC;
           MIDI_NOTE : in STD_LOGIC_VECTOR (7 downto 0);
           save : in std_logic;
           FEED_IN : in STD_LOGIC_VECTOR (15 downto 0);
           Address : in STD_LOGIC_VECTOR (8 downto 0);
           S : out STD_LOGIC_VECTOR (15 downto 0));
end STORE_PRIMARY;

architecture Behavioral of STORE_PRIMARY is

component DDS_DRIVER is
    Port ( clk : in STD_LOGIC;
           MIDI_NOTE : in STD_LOGIC_VECTOR (7 downto 0);
           DDS_RATE : out STD_LOGIC_VECTOR (19 downto 0));
end component;

COMPONENT blk_mem_for_DDS
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    clkb : IN STD_LOGIC;
    enb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
  );
END COMPONENT;

signal delay0 : integer range 0 to 2048; ----To give the BRAM time to populate
signal OUT_EN : std_logic_vector(3 downto 0);  ----enable BRAM output
signal SEL_RAM : std_logic_vector(3 downto 0);   -------choose which ram to write to
signal DDS_acc0 : std_logic_vector(31 downto 0);
signal DDS_acc1 : std_logic_vector(31 downto 0);
signal DDS_acc2 : std_logic_vector(31 downto 0);
signal DDS_acc3 : std_logic_vector(31 downto 0);

signal DDS_out0 : std_logic_vector(15 downto 0);
signal DDS_out1 : std_logic_vector(15 downto 0);
signal DDS_out2 : std_logic_vector(15 downto 0);
signal DDS_out3 : std_logic_vector(15 downto 0);

signal DDS_rate0 : std_logic_vector(19 downto 0);
signal DDS_rate1 : std_logic_vector(19 downto 0);
signal DDS_rate2 : std_logic_vector(19 downto 0);
signal DDS_rate3 : std_logic_vector(19 downto 0);

signal done_flag : std_logic; ---goeas high when comencing write, and doesn't go down until it sees a save=0

begin

WAVE0: blk_mem_for_DDS
PORT MAP(clka => clk,
    wea => CONV_STD_LOGIC_VECTOR(SEL_RAM(0),1),
    addra => Address(8 downto 0),
    dina => FEED_IN,
    clkb => clk,
    enb => OUT_EN(0),
    addrb => DDS_acc0(31 downto 23),
    doutb => DDS_out0
    );

WAVE1: blk_mem_for_DDS
PORT MAP(clka => clk,
    wea => CONV_STD_LOGIC_VECTOR(SEL_RAM(1),1),
    addra => Address(8 downto 0),
    dina => FEED_IN,
    clkb => clk,
    enb => OUT_EN(1),
    addrb => DDS_acc1(31 downto 23),
    doutb => DDS_out1
    );
    
WAVE2: blk_mem_for_DDS
    PORT MAP(clka => clk,
        wea => CONV_STD_LOGIC_VECTOR(SEL_RAM(2),1),
        addra => Address(8 downto 0),
        dina => FEED_IN,
        clkb => clk,
        enb => OUT_EN(2),
        addrb => DDS_acc2(31 downto 23),
        doutb => DDS_out2
        );
        
WAVE3: blk_mem_for_DDS
        PORT MAP(clka => clk,
            wea => CONV_STD_LOGIC_VECTOR(SEL_RAM(3),1),
            addra => Address(8 downto 0),
            dina => FEED_IN,
            clkb => clk,
            enb => OUT_EN(3),
            addrb => DDS_acc3(31 downto 23),
            doutb => DDS_out3
            );

RATE_PICKER0: DDS_DRIVER
PORT MAP(clk => clk,
        DDS_RATE => DDS_RATE0,
        MIDI_NOTE => MIDI_NOTE
        );
        
RATE_PICKER1: DDS_DRIVER
        PORT MAP(clk => clk,
                DDS_RATE => DDS_RATE1,
                MIDI_NOTE => MIDI_NOTE
                );

RATE_PICKER2: DDS_DRIVER
PORT MAP(clk => clk,
        DDS_RATE => DDS_RATE2,
        MIDI_NOTE => MIDI_NOTE
        );

RATE_PICKER3: DDS_DRIVER
PORT MAP(clk => clk,
        DDS_RATE => DDS_RATE3,
        MIDI_NOTE => MIDI_NOTE
        );                

process(clk) begin

if(clk'event and clk='1')then

DDS_acc0<=DDS_acc0+DDS_rate0;
DDS_acc1<=DDS_acc1+DDS_rate1;
DDS_acc2<=DDS_acc2+DDS_rate2;
DDS_acc3<=DDS_acc3+DDS_rate3;

S <= DDS_out0 + DDS_out1 + DDS_out2 + DDS_out3; 

    if(save='1')then
        if(delay0 < 1)then
            if(done_flag='0')then
                case SEL_RAM is
                    when "0000" =>
                        done_flag<='1';
                        SEL_RAM <= "0001";
                        OUT_EN(0) <='1';
                        delay0<=2048;
                    when "0001" =>
                        done_flag<='1';
                        SEL_RAM <= "0010";
                        OUT_EN(1) <='1';
                        delay0<=2048;
                    when "0010" =>
                        done_flag<='1';
                         SEL_RAM <= "0100";
                         OUT_EN(2) <='1';
                            delay0<=2048;       
                    when "0100" =>
                          done_flag<='1';
                          SEL_RAM <= "1000";
                          OUT_EN(3) <='1';
                          delay0<=2048;
                    when others =>
                end case;
            end if;
        else
            delay0<=delay0-1;
        end if;
    else
        done_flag<='0';
    end if;
    
end if;
end process;
end Behavioral;
