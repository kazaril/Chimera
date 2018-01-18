-------------TODO LIST
--imperative!!!:
--Fix velocity
--ADSR curve
--LFO
--Filter coeffs
--home view
--fix FM

--SECONDARY
--performer
--fix distortion
--effects
--clean graphics/text
---------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity CHIMERA_TOP_LEVEL is
    Port ( gclk : in STD_LOGIC;  --100MHz INPUT CLOCk
    
          --------------SCREEN OUTPUTS
           R : out STD_LOGIC_VECTOR (7 downto 0); ---RGB OUT
           G : out STD_LOGIC_VECTOR(7 downto 0);
           B : out std_logic_vector(7 downto 0);
           Pixel_clk : out std_logic;   --32MHz OUTPUT CLK TO SCREEN
           HSYNC : out std_logic; --VGA SYNC SIGNALS
           VSYNC : out std_logic;           
           DISP : out std_logic;  -- NOT SURE IF USED? DISPLAY NEABLE
           DE : out std_logic; -- DISPLAY ENABLE FOR VGA TIMING
           
           ------------Touch Control
           CTP_RST : out std_logic; --ACTIVE HIGH TS RESET (JUST KEEP HIGH?)           
           SDA : inout std_logic; -- TS I2C
           SCL : inout std_logic;          
           
           -----------Misc I/O (inc. MIDI)
           leds : out std_logic_vector(7 downto 0); --FOR DEBUG, ON ZEDBOARD
           O1 : out std_logic;  --TESTPOINT ON BREAKOUT
           O2 : out std_logic;
           JB1 : out std_logic;  --TESTPOINT ON PMOD B1
           MIDI_IN : in std_logic;  --MIDI_IN
            btn : in std_logic_vector(1 downto 0);
           
           SW0 : in std_logic; --ZEDBOARD SWITCH 0
           SW1 : in std_logic; --ZEDBOARD SWITCH 1
            SW2 : in std_logic; --ZEDBOARD SWITCH 2
            SW3 : in std_logic;
            SW4 : in std_logic;
            SW5 : in std_logic;
            SW6 : in std_logic;
            SW7 : in std_logic; 
            
           --------DAC control
--           DAC_SDA : inout std_logic;
--           DAC_SCL : inout std_logic;
             CDATA : out std_logic;
             CLATCH : out std_logic;
             CCLK : out std_logic;         
            
            ----ROTARY SIGNALS
            A1 : in std_logic;
            B1 : in std_logic;
            
            
           ---------HS SERIAL OUTPUT
           bclk    : out std_logic;
           lrclk:out STD_LOGIC;
           sdata:out STD_LOGIC; --datastream, left justified
           mclk:out STD_LOGIC 
           ------------------------------
           
           );
end CHIMERA_TOP_LEVEL;

architecture Behavioral of CHIMERA_TOP_LEVEL is

component SCREEN_CONTROL is
    Port (clk : in STD_LOGIC;  --100MHz INPUT CLOCK
           locked : in std_logic;
           RGB : out STD_LOGIC_VECTOR (23 downto 0); ---RGB OUT
           Pixel_clk : out std_logic;   --32MHz OUTPUT CLK TO SCREEN
           HSYNC : out std_logic; --VGA SYNC SIGNALS
           VSYNC : out std_logic;
           DISP : out std_logic;  -- NOT SURE IF USED? DISPLAY NEABLE
           SDA : inout std_logic; -- TS I2C
           SCL : inout std_logic;
           mode : in std_logic_VECTOR(2 DOWNTO 0);
           S : out std_logic_vector(15 downto 0); --audio_data
           S_addr : out std_logic_vector(11 downto 0); ---address of audio
           CTP_RST : out std_logic:='1'; --ACTIVE HIGH TS RESET (JUST KEEP HIGH?)
           add_note : in std_logic_vector(4 downto 0);
           attack : in std_logic_vector(6 downto 0);
           DE : out std_logic -- DISPLAY ENABLE FOR VGA TIMING
           );
end component;

component STORE_PRIMARY is  ----------Store and recall up to 4 waveforms for primary synthasis
    Port ( clk : in STD_LOGIC;
           MIDI_NOTE : in STD_LOGIC_VECTOR (7 downto 0);
           FEED_IN : in STD_LOGIC_VECTOR (15 downto 0);
           save : in std_logic;
           Address : in STD_LOGIC_VECTOR (8 downto 0);
           S : out STD_LOGIC_VECTOR (15 downto 0));
end component;

component rotaryencoder is port
(
    clk_i : in  std_logic;              -- Input clock.
    a_i   : in  std_logic;              -- A phase of rotary encoder.
    b_i   : in  std_logic;              -- B phase of rotary encoder.
    en_o  : out std_logic;              -- True when rotation increment occurs.
    cw_o  : out std_logic  -- True if rotation is clockwise, false if counter-clockwise.
    );
end component;



COMPONENT VEL_MULT ----multiplies audio by MIDI velocity and adsr curve
  PORT (
    CLK : IN STD_LOGIC;
    A : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    B : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    P : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
  );
END COMPONENT;


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


component DDS_DRIVER is
    Port ( clk : in STD_LOGIC;
           MIDI_NOTE : in STD_LOGIC_VECTOR (7 downto 0);
           DDS_RATE : out STD_LOGIC_VECTOR (19 downto 0));
end component;


component AUDIO_OUT is
    Port ( clk : in STD_LOGIC;
           AUDIO_left : in STD_LOGIC_VECTOR (15 downto 0);
           AUDIO_right : in STD_LOGIC_VECTOR (15 downto 0);
           bclk: out STD_LOGIC;
           lrclk:out STD_LOGIC;
           sdata:out STD_LOGIC;
           mclk:out STD_LOGIC;
           cdata : out STD_LOGIC; -- mosi , miso is cout, ignored
           clatch : out STD_LOGIC; -- slave select
           cclk : out STD_LOGIC; -- MAX 10Mhz
           leds : out std_logic_vector(7 downto 0);
           locked:in STD_LOGIC  
);
end component;

component ENVELOPE_GEN is
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
end component;

component debounce IS
  GENERIC(
    counter_size  :  INTEGER := 20); --counter size (19 bits gives 10.5ms with 50MHz clock)
  PORT(
    clk     : IN  STD_LOGIC;  --input clock
    button  : IN  STD_LOGIC;  --input signal to be debounced
    result  : OUT STD_LOGIC); --debounced signal
END component;

component FILTER_BLOCK is
    Port ( clk : in STD_LOGIC;
            clkS : in std_logic;
           S : in STD_LOGIC_VECTOR (15 downto 0);
           SW : in std_logic;
           SWS : in std_logic_vector(3 downto 0);
           S_out : out STD_LOGIC_VECTOR (15 downto 0));
end component;


component MIDI_INTERPRETER is
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
end component;

-----------------clk considerations::
----------- must be integer multiple of 48000 (3072)
------------ Must have P_clk within range that is clk/3


component clk_wiz_147_147    --Takes in 100Mhz clock and creates 147_451Mhz clk
port
 (-- Clock in ports
  clk_in1           : in     std_logic;
  -- Clock out ports
  clk_out1          : out    std_logic;
  -- Status and control signals
  locked            : out    std_logic
 );
end component;




---AUDIO ROUTING
signal S : std_logic_vector(15 downto 0); --audio_data from input
signal S_addr : std_logic_vector(15 downto 0); ---address of audio input
signal clkS : std_logic; ----48000 clk for audio
signal clkS_cnt : std_logic_vector(10 downto 0); ---counter to create clkS
signal clk : std_logic; ----master clock signal 147.451 MHz
signal locked : std_logic:='0'; --Clock locked signal
signal AUDIO_LEFT : std_logic_vector(15 downto 0); --main audio outputs
signal AUDIO_RIGHT : std_logic_vector(15 downto 0); --main audio outputs
signal VEL_MULT_OUT : std_logic_vector(31 downto 0);  ----after VELOCITY MULTIPLYER. leads to main audio out

----------MIDI SIGNALS FROM THE INTERPRETER
signal NOTE_ON : std_logic;
signal NOTE_OFF : std_logic;
signal MIDI_VELOCITY : std_logic_vector(7 downto 0);
signal MIDI_NOTE : std_logic_vector(7 downto 0);
signal NOTE_FLAG : std_logic;

signal TEMP_THINGY : std_logic_vector(15 downto 0);

-----------------DDS STUFF
signal DDS_rate : std_logic_vector(19 downto 0);
signal DDS_acc : std_logic_vector(31 downto 0);
signal DDS_out : std_logic_vector(15 downto 0);
signal add_note : std_logic_vector(4 downto 0);


--------------High speed serial output buffers (delete?)
signal mclk_buff : std_logic;
signal bclk_buff : std_logic;
signal SDATA_buff : std_logic;


------------------FOR MODE SELECTS
signal phase_mod_on : std_logic;
signal primary_on : std_logic;
signal mode : std_logic_VECTOR(2 DOWNTO 0);


-----------------PM signals
signal DDS_addr_PM : std_logic_vector(8 downto 0);
signal PM_out : std_logic_vector(15 downto 0);
signal PM_acc : std_logic_vector(31 downto 0);
signal PM_rate : std_logic_vector(19 downto 0);

--------------For primary saving
signal save_trig : std_logic; --------OUtput from save button debounce
signal save_state : std_logic; -----------changes state through the save procedure
signal DDS_addr_B : std_logic_vector(8 downto 0); -----is phase_acc when normal mode and save_addr_cnt when saving
signal save_addr_cnt : std_logic_vector(8 downto 0); ----current position in the save write
signal primary_out : std_logic_vector(15 downto 0);


-------------Filter Block signals
signal To_filt : std_logic_vector(15 downto 0);
signal from_filt : std_logic_vector(15 downto 0);

signal RGB : std_logic_vector(23 downto 0);


------------Envelope signals
signal attack : std_logic_vector(6 downto 0);
signal ENV_OUT : integer range 0 to 127;

------------Rotary outputs
signal en_o1 : std_logic;
signal cw_o1 : std_logic;

--type VEL_array is array(0 to 127) of std_logic_vector(8 downto 0);
--signal Velocity_rom : VEL_array := ("000000100", "000000100",	"000000100",	"000000100",	"000000101",	"000000101",	"000000101",	"000000101",	"000000101",	"000000110",	"000000110",	"000000110",	"000000110",	"000000110",	"000000111",	"000000111",	"000000111",	"000000111",	"000001000",	"000001000",	"000001000",	"000001000",	"000001001",	"000001001",	"000001001",	"000001010",	"000001010",	"000001011",	"000001011",	"000001011",	"000001100",	"000001100",	"000001101",	"000001101",	"000001110",	"000001110",	"000001111",	"000001111",	"000010000",	"000010000",	"000010001",	"000010001",	"000010010",	"000010011",	"000010011",	"000010100",	"000010101",	"000010110",	"000010110",	"000010111",	"000011000",	"000011001",	"000011010",	"000011011",	"000011100",	"000011101",	"000011110",	"000011111",	"000100000",	"000100001",	"000100010",	"000100100",	"000100101",	"000100110",	"000101000",	"000101001",	"000101011",	"000101100",	"000101110",	"000110000",	"000110001",	"000110011",	"000110101",	"000110111",	"000111001",	"000111011",	"000111101",	"001000000",	"001000010",	"001000100",	"001000111",	"001001001",	"001001100",	"001001111",	"001010010",	"001010101",	"001011000",	"001011011",	"001011110",	"001100010",	"001100101",	"001101001",	"001101101",	"001110001",	"001110101",	"001111001",	"001111110",	"010000011",	"010000111",	"010001100",	"010010001",	"010010111",	"010011100",	"010100010",	"010101000",	"010101110",	"010110100",	"010111011",	"011000010",	"011001001",	"011010000",	"011011000",	"011100000",	"011101000",	"011110001",	"011111001",	"100000011",	"100001100",	"100010110",	"100100000",	"100101011",	"100110101",	"101000001",	"101001101",	"101011001",	"101100101",	"101110010",	"110000000");

--type VEL_array is array(0 to 127) of std_logic_vector(11 downto 0);
--signal Velocity_rom : VEL_array := (x"258",	x"23B",	x"21F",	x"204",	x"1EB",	x"1D3",	x"1BC",	x"1A7",	x"192",	x"17F",	x"16C",	x"15A",	x"149",	x"139",	x"12A",	x"11B",	x"10E",	x"100",	x"0F4",	x"0E8",	x"0DD",	x"0D2",	x"0C8",	x"0BE",	x"0B5",	x"0AC",	x"0A4",	x"09C",	x"094",	x"08D",	x"086",	x"07F",	x"079",	x"073",	x"06E",	x"068",	x"063",	x"05E",	x"05A",	x"055",	x"051",	x"04D",	x"04A",	x"046",	x"043",	x"03F",	x"03C",	x"039",	x"036",	x"034",	x"031",	x"02F",	x"02D",	x"02A",	x"028",	x"026",	x"025",	x"023",	x"021",	x"01F",	x"01E",	x"01C",	x"01B",	x"01A",	x"018",	x"017",	x"016",	x"015",	x"014",	x"013",	x"012",	x"011",	x"010",	x"010",	x"00F",	x"00E",	x"00D",	x"00D",	x"00C",	x"00C",	x"00B",	x"00A",	x"00A",	x"009",	x"009",	x"009",	x"008",	x"008",	x"007",	x"007",	x"007",	x"006",	x"006",	x"006",	x"005",	x"005",	x"005",	x"005",	x"004",	x"004",	x"004",	x"004",	x"004",	x"003",	x"003",	x"003",	x"003",	x"003",	x"003",	x"003",	x"002",	x"002",	x"002",	x"002",	x"002",	x"002",	x"002",	x"002",	x"002",	x"002",	x"001",	x"001",	x"001",	x"001",	x"001",	x"001",	x"001",	x"001");
--signal Velocity_rom : VEL_array := (x"001",	x"01E",	x"03A",	x"055",	x"06E",	x"086",	x"09D",	x"0B2",	x"0C7",	x"0DA",	x"0ED",	x"0FF",	x"110",	x"120",	x"12F",	x"13E",	x"14B",	x"159",	x"165",	x"171",	x"17C",	x"187",	x"191",	x"19B",	x"1A4",	x"1AD",	x"1B5",	x"1BD",	x"1C5",	x"1CC",	x"1D3",	x"1DA",	x"1E0",	x"1E6",	x"1EB",	x"1F1",	x"1F6",	x"1FB",	x"1FF",	x"204",	x"208",	x"20C",	x"20F",	x"213",	x"216",	x"21A",	x"21D",	x"220",	x"223",	x"225",	x"228",	x"22A",	x"22C",	x"22F",	x"231",	x"233",	x"234",	x"236",	x"238",	x"23A",	x"23B",	x"23D",	x"23E",	x"23F",	x"241",	x"242",	x"243",	x"244",	x"245",	x"246",	x"247",	x"248",	x"249",	x"249",	x"24A",	x"24B",	x"24C",	x"24C",	x"24D",	x"24D",	x"24E",	x"24F",	x"24F",	x"250",	x"250",	x"250",	x"251",	x"251",	x"252",	x"252",	x"252",	x"253",	x"253",	x"253",	x"254",	x"254",	x"254",	x"254",	x"255",	x"255",	x"255",	x"255",	x"255",	x"256",	x"256",	x"256",	x"256",	x"256",	x"256",	x"256",	x"257",	x"257",	x"257",	x"257",	x"257",	x"257",	x"257",	x"257",	x"257",	x"257",	x"258",	x"258",	x"258",	x"258",	x"258",	x"258",	x"258",	x"258");
type VEL_array is array(0 to 127) of std_logic_vector(15 downto 0);
signal Velocity_rom : VEL_array := (
x"0214",
x"0228",
x"023E",
x"0254",
x"026B",
x"0283",
x"029C",
x"02B5",
x"02D0",
x"02EC",
x"0309",
x"0327",
x"0346",
x"0366",
x"0388",
x"03AB",
x"03CF",
x"03F5",
x"041C",
x"0445",
x"046F",
x"049B",
x"04C8",
x"04F7",
x"0529",
x"055C",
x"0591",
x"05C8",
x"0601",
x"063C",
x"067A",
x"06BA",
x"06FC",
x"0741",
x"0789",
x"07D4",
x"0821",
x"0871",
x"08C5",
x"091C",
x"0976",
x"09D3",
x"0A34",
x"0A99",
x"0B02",
x"0B6F",
x"0BE0",
x"0C56",
x"0CD0",
x"0D4E",
x"0DD2",
x"0E5A",
x"0EE8",
x"0F7C",
x"1015",
x"10B4",
x"1159",
x"1205",
x"12B7",
x"1370",
x"1430",
x"14F8",
x"15C7",
x"169E",
x"177E",
x"1866",
x"1958",
x"1A52",
x"1B57",
x"1C65",
x"1D7E",
x"1EA1",
x"1FD0",
x"210B",
x"2252",
x"23A5",
x"2505",
x"2674",
x"27F0",
x"297B",
x"2B15",
x"2CBF",
x"2E79",
x"3045",
x"3222",
x"3412",
x"3615",
x"382C",
x"3A57",
x"3C98",
x"3EEF",
x"415E",
x"43E4",
x"4683",
x"493D",
x"4C11",
x"4F01",
x"520E",
x"553A",
x"5885",
x"5BF0",
x"5F7D",
x"632D",
x"6702",
x"6AFD",
x"6F1F",
x"7369",
x"77DF",
x"7C80",
x"814F",
x"864E",
x"8B7E",
x"90E1",
x"967A",
x"9C4A",
x"A254",
x"A899",
x"AF1C",
x"B5E0",
x"BCE6",
x"C432",
x"CBC6",
x"D3A5",
x"DBD2",
x"E450",
x"ED22",
x"F64B",
x"FFCE");

begin
------147456


--MAIN_OUT: AUDIO_OUT
--PORT MAP(clk=> clk,
--    DAC_SDA => DAC_SDA,
--    DAC_SCL => DAC_SCL,
--    locked => locked,
--    bclk => bclk_buff,
--    lrclk => lrclk,
--    sdata => sdata_buff,
--    mclk => mclk_buff,
--    AUDIO_left => AUDIO_LEFT,
--    AUDIO_right => AUDIO_RIGHT,
    
--    DEBUG => leds(7 downto 3)
--    );


ADSR: ENVELOPE_GEN
    PORT MAP( clk=> clk,
            clkS => clkS,
            A => attack,
            D => "1100000",
            S => "1100000",
            R => "1100000",
            trigger => NOTE_ON,
            note_off => NOTE_OFF,
            MIDI_VELOCITY => MIDI_VELOCITY(6 downto 0),
            VAL_OUT => env_out
            );
            
STORER: STORE_PRIMARY   
    Port MAP ( clk => clk,
           MIDI_NOTE => MIDI_NOTE,
           FEED_IN => DDS_out,
           save => save_state,
           Address => Save_addr_cnt,
           S => primary_out
           );
 

BQ_FILT: filter_block
PORT MAP(clk => clk,
        clkS => clkS,
        S => To_filt,
        SW => SW3,
        SWS => SW4 & SW5 & SW6 & SW7,
        S_out => From_filt
        ); 

ROT1: rotaryencoder
PORT MAP(CLK_i=> clk,
        A_i => A1,
        B_i => B1,
        en_o => en_o1,
        cw_o => cw_o1
        );

ACCEPT_BTN: debounce 
      PORT MAP(clk=> clk,
              button => btn(0),
              result => save_trig
               );
    
--LOUDENER: VEL_MULT
--      PORT MAP (
--            CLK => CLK,
--            A => Audio_left,  ----audio signal
--            B => Velocity_rom(to_integer(unsigned(MIDI_VELOCITY))),
--            P => VEL_MULT_OUT
--            );


MAIN_OUT: AUDIO_OUT
PORT MAP(clk=> clk,
    CLATCH => CLATCH,
    CCLK => CCLK,
    CDATA=> CDATA,
    locked => locked,
    bclk => bclk,
    lrclk => lrclk,
    sdata => sdata,
    mclk => mclk,
    leds => leds,
    AUDIO_left => VEL_MULT_OUT(31 downto 16),
    AUDIO_right => VEL_MULT_OUT(31 downto 16)
    
    
    );

Master_gen: clk_wiz_147_147
PORT MAP(clk_in1 => gclk,
        clk_out1 => clk,
        locked => locked
        );


PLAY_SOUNDS: blk_mem_for_DDS ---Working memory whilst drawing
  PORT MAP (
    clka => clk,
    wea(0) => Primary_on,
    addra => S_addr(8 downto 0),
    dina => S,
    enb => '1',
    clkb => clk,
    addrb => DDS_addr_B,
    doutb => DDS_out
    );

PHASE_MOD: blk_mem_for_DDS ---For Phase modulation
  PORT MAP (
    clka => clk,
    wea(0) =>  Phase_mod_on,
    addra => S_addr(8 downto 0),
    dina => STD_LOGIC_VECTOR(RESIZE(signed(S(15 downto 11)),16)),
    enb => Phase_mod_on,
    clkb => clk,
    addrb => DDS_addr_PM,
    doutb => PM_out
    );


SCREEN_THING: SCREEN_CONTROL
PORT MAP(clk => clk,
        locked => locked,
        RGB => RGB,              
        Pixel_clk => Pixel_clk,
        HSYNC => HSYNC,
        VSYNC => VSYNC,
        DISP => DISP,
        SDA => SDA,
        SCL => SCL,
        S => S,
        mode => mode,
        add_note => add_note,
        attack => attack,
        S_addr => S_addr(11 downto 0),
        CTP_RST => CTP_RST,
        DE => DE
        );


PM_PICKER: DDS_DRIVER
PORT MAP(clk => clk,
        DDS_RATE => PM_RATE,
        MIDI_NOTE => MIDI_NOTE
        );


RATE_PICKER: DDS_DRIVER
PORT MAP(clk => clk,
        DDS_RATE => DDS_RATE,
        MIDI_NOTE => MIDI_NOTE + ADD_NOTE
        );


MIDI: MIDI_INTERPRETER 
generic MAP (
			CLK_FREQ => 147_147_000
		)
PORT MAP ( 		
			reset=> '0',
			clk => clk,
			RXD => MIDI_IN,
			note_on => NOTE_ON,
			note_off => NOTE_OFF,
			note => MIDI_NOTE,
			velocity => MIDI_VELOCITY
			);


process(clk) begin --HANDLES NOTE ON/OFF and DDS accumulator


if(clk'event and clk='1') then
    
    R<=RGB(23 downto 16);
    G<=RGB(15 downto 8);
    B<=RGB(7 downto 0);
    
    
    
    if(phase_mod_on='1')then
     DDS_acc<=DDS_acc+DDS_rate + PM_out;
    else
     DDS_acc<=DDS_acc+DDS_rate;   -------------ACC for working memory DDS
    end if;
    
    
      leds <= "1" & std_logic_vector(to_unsigned(ENV_OUT,7));
--    leds(1)<= VEL_MULT_OUT(31);
--    leds(2)<= VEL_MULT_OUT(30);
--    leds(3)<= VEL_MULT_OUT(29);
--    leds(4)<= VEL_MULT_OUT(28);
--    leds(5)<= VEL_MULT_OUT(27);
--    leds(6)<= VEL_MULT_OUT(26);
--    leds(7)<= VEL_MULT_OUT(25);
    
    
    PM_acc<=PM_acc+PM_rate;
    DDS_ADDR_PM<=PM_acc(31 downto 23);
    
     if(NOTE_OFF='1') then   
        NOTE_FLAG<='0';
        
     elsif(NOTE_ON='1')then
        NOTE_FLAG<='1';
        
    end if;
      
      
     
     
        
        
      if(NOTE_FLAG='1')then  
        if(SW2 ='1')then  
            Audio_left<=DDS_out + primary_out;
            
            Audio_right<=DDS_out + primary_out;
         else
                 To_filt<=DDS_out + primary_out;
                 Audio_left<=from_filt;
                Audio_right<=from_filt;
        end if;
      else 
              Audio_left<=(others => '0');
              Audio_right<=(others => '0');
      end if;
      
end if;
end process;

process(clkS) begin --create 48k clkS

if(clkS'event and clkS='1') then
    VEL_MULT_OUT <= AUDIO_LEFT*Velocity_rom(ENV_OUT);--Velocity_rom(to_integer(unsigned(MIDI_VELOCITY)));
END IF;
END PROCESS;


process(clk) begin --create 48k clkS

if(clk'event and clk='1') then
    if(clkS_cnt < 1436)then
        clkS_cnt <= clkS_cnt+1;
    else
        clkS_cnt<= (others=> '0');
        clkS <= not(clkS);
    end if;

end if;
end process;


process(clkS) begin --create 48k clkS

if(clkS'event and clkS='1') then

           
       

end if;
end process;


process(clk) begin --HANDLES save procedure for primary DDS

if(clk'event and clk='1') then
    
    
    case save_state is
        when '0' => -----waiting for save command
            if(save_trig='1')then
                save_state<='1';
                
            end if;
            DDS_addr_B <= DDS_acc(31 downto 23);
       when '1' => ---------DDS does a write to STORE_PRIMARY
                if(save_addr_cnt<511)then
                    DDS_addr_B <= save_addr_cnt; 
                    save_addr_cnt<= save_addr_cnt+1;
                else
                    save_addr_cnt<=(others => '0');
                    DDS_addr_B <= DDS_acc(31 downto 23);
                    save_state<='0';
                end if;
        when others =>
      end case;
    
    
        mode<=SW2 & SW1 & SW0;
        if(mode="001")then
            
            Phase_mod_on<='1';
            Primary_on <='0';
             else
                Phase_mod_on<='0';
                Primary_on <='1';          
        end if;
end if;
end process;



process(clk) begin --ROTARY STUFF

if(clk'event and clk='1') then


    case mode is
        when "000" =>
            if(en_o1 ='1' and cw_o1 = '1')then --change note pitch
               
  --               if(add_note < 12)then
                   add_note <= add_note+1;
--                 else
--                    add_note <= std_logic_vector(to_signed(12,5));
            --    end if;
             elsif(en_o1 ='1' and cw_o1 = '0')then
 --              if(add_note > -13)then
                 add_note <= add_note-1;
 --                     else
 --                add_note <= std_logic_vector(to_signed(-12,5));
 --               end if;
            end if;
         when "011" =>
          if(en_o1 ='1' and cw_o1 = '1')then  ---change attack
            
              if(attack < 127)then
                attack <= attack+1;
              else
                 attack <= "1111111";
             end if;
          elsif(en_o1 ='1' and cw_o1 = '0')then
            if(attack > 1)then
              attack <= attack-1;
                   else
              attack <= "0000001";
             end if;
         end if;        
            
         when others => --
    end case;
end if;
end process;


end Behavioral;