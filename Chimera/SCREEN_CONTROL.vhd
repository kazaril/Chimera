----HANDLES SCREEN OUT, BUFFERING AND TOUCH SCREEN CONTROL


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;


entity SCREEN_CONTROL is
    Port ( clk : in STD_LOGIC;  --100MHz INPUT CLOCK
           locked : in std_logic;
           RGB : out STD_LOGIC_VECTOR (23 downto 0); ---RGB OUT
           Pixel_clk : out std_logic;   --32MHz OUTPUT CLK TO SCREE
           HSYNC : out std_logic; --VGA SYNC SIGNALS
           VSYNC : out std_logic;
           DISP : out std_logic;  -- NOT SURE IF USED? DISPLAY NEABLE
           SDA : inout std_logic; -- TS I2C
           SCL : inout std_logic;
           mode : in std_logic_vector(2 downto 0);
           CTP_RST : out std_logic:='1'; --ACTIVE HIGH TS RESET (JUST KEEP HIGH?)
           S : out std_logic_vector(15 downto 0); --audio_data
           S_addr : out std_logic_vector(11 downto 0); ---address of audio
           add_note : in std_logic_vector(4 downto 0);
           attack : in std_logic_vector(6 downto 0);
           DE : out std_logic -- DISPLAY ENABLE FOR VGA TIMING
           );
end SCREEN_CONTROL;

architecture Behavioral of SCREEN_CONTROL is


component Sync_gen is  

    Port (pixel_clk :  IN   STD_LOGIC;  --pixel clock at frequency of VGA mode being used
    
    h_sync    :  OUT  STD_LOGIC;  --horiztonal sync pulse
    v_sync    :  OUT  STD_LOGIC;  --vertical sync pulse
    disp_ena  :  OUT  STD_LOGIC;  --display enable ('1' = display time, '0' = blanking time)
    column    :  OUT  std_logic_vector(10 downto 0);    --horizontal pixel coordinate
    row       :  OUT  std_logic_vector(15 downto 0));    --vertical pixel coordinate);
end component;


component font_rom is ----------------ascii ROM
   port(
      clk: in std_logic;
      addr: in std_logic_vector(10 downto 0);
      data: out std_logic_vector(7 downto 0)
   );
end component;


COMPONENT SCREEN_WORD_BUFFER ----------buffer to sit along bottom of screen
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END COMPONENT;

--COMPONENT fir_Interpolate
--  PORT (
--    aclk : IN STD_LOGIC;
--    s_axis_data_tvalid : IN STD_LOGIC;
--    s_axis_data_tready : OUT STD_LOGIC;
--    s_axis_data_tdata : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
--    m_axis_data_tvalid : OUT STD_LOGIC;
--    m_axis_data_tdata : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
--  );
--END COMPONENT;



-------------------------------------ROMS FOR WRITING WAVEFORMS
COMPONENT SQUARE_ROM
  PORT (
    clka : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
  );
END COMPONENT;

COMPONENT SINE_ROM
  PORT (
    clka : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
  );
END COMPONENT;

COMPONENT NOISE_ROM
  PORT (
    clka : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
  );
END COMPONENT;

COMPONENT TRIANGLE_ROM
  PORT (
    clka : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
  );
END COMPONENT;

COMPONENT SAW_ROM
  PORT (
    clka : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
  );
END COMPONENT;




COMPONENT blk_mem_gen_0
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
  );
END COMPONENT;

component i2c_master IS --for touch reading and DAC configure
  GENERIC(
    input_clk : INTEGER := 147_451_000; --input clock speed from user logic in Hz
    bus_clk   : INTEGER := 400_000);   --speed the i2c bus (scl) will run at in Hz
  PORT(
    clk       : IN     STD_LOGIC;                    --system clock
    reset_n   : IN     STD_LOGIC;                    --active low reset
    ena       : IN     STD_LOGIC;                    --latch in command
    addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
    rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
    data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data to write to slave
    busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
    data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
    ack_error : BUFFER STD_LOGIC;                    --flag if improper acknowledge from slave
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC);                   --serial clock output of i2c bus
END component;


COMPONENT fir_from_screen   -------Filter from second BRAM to remove distortion artifacts. Feed at 1/100th of clock
  PORT (
    aclk : IN STD_LOGIC;
    s_axis_data_tvalid : IN STD_LOGIC;
    s_axis_data_tready : OUT STD_LOGIC;
    s_axis_data_tdata : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    m_axis_data_tvalid : OUT STD_LOGIC;
    m_axis_data_tdata : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
  );
END COMPONENT;


signal P_count : integer range 0 to 6; --for pixel clock divider
signal P_clk : std_logic; --pixel clock buffer
signal DE_sig : std_logic; -- Display enable buffer


-----------I2C signals for Touch interface
signal i2c_ena : std_logic;
signal i2c_addr : std_logic_vector(6 downto 0);
signal i2c_rw : std_logic;
signal i2c_data_wr : std_logic_vector(7 downto 0);
signal i2c_busy : std_logic;
signal busy_prev : std_logic;
signal i2c_data_rd : std_logic_vector(7 downto 0);
signal TS_ack_err : std_logic;
signal busy_cnt : integer range 0 to 8;
signal data : std_logic_vector(15 downto 0);
signal XTOUCH : std_logic_vector(15 downto 0);
signal YTOUCH : std_logic_vector(15 downto 0);
signal y_toggle : std_logic;
signal init_done : std_logic; --Have the initilisation registers been set?
signal draw_in : std_logic_vector(11 downto 0); ----------MUX signal for drawing either from touch or wave_draw


---------------For DRAWING FROM ROMS
signal sine_write : std_logic_vector(11 downto 0);
signal saw_write : std_logic_vector(11 downto 0);
signal square_write : std_logic_vector(11 downto 0);
signal triangle_write : std_logic_vector(11 downto 0);
signal noise_write : std_logic_vector(11 downto 0);
signal draw_pos : std_logic_vector(11 downto 0):=x"2AB";
signal draw_pos_in : std_logic_vector(11 downto 0);
signal rom_ena : std_logic;
signal draw_from_rom : std_logic_vector(2 downto 0);

----------Interface BRAM with VGA
signal row : std_logic_vector(15 downto 0);
signal column : std_logic_vector(10 downto 0);
signal column_buff : std_logic_vector(11 downto 0);
signal Y1 : std_logic_vector(15 downto 0);
signal wave_en : std_logic_vector(0 downto 0); ----High when within drawzone
signal circle_height : std_logic_vector(2 downto 0);
signal S_buff : std_logic_vector(15 downto 0);

--------------SCREEN Interpolator signals
signal int_count : std_logic_vector(3 downto 0); ----To scan across size 
signal XPOS : std_logic_vector(11 downto 0); ----cureent spot on screen for making larger pixels
signal YPOS : std_logic_vector(11 downto 0);
signal XPOS_total : std_logic_vector(11 downto 0); ----For checking if we're in the zone

--------------AUDIO INTERPOLATOR
signal TO_INT : std_logic_vector(11 downto 0); --comes from BRAM into INT filter
signal INT_addr : std_logic_vector(11 downto 0); ---Increment every 128 samples for the INT filter
signal count128 : std_logic_vector(6 downto 0); --to increment INT_addr
signal new_addr : std_logic; -----High when count128 resets
signal down3_4 : std_logic_vector(1 downto 0); --count from 0 to 3 and skip 1 sample when writing to AUDIO WAVE RAM
signal skip_count : std_logic_vector(11 downto 0);  --how many samples have been dropped for column  de-incriment
signal INT_wants : std_logic; ----connects to S_AXIS_DATA_TVALID of interpolator
signal INT_OUT : std_logic_vector(15 downto 0);     --AUDIO out
signal INT_sample_out : std_logic; --new AUDIO sample availible
signal AUDIO_ADDRESS : std_logic_vector(15 downto 0);  ---address for AUDIO BRAM on top level



---------------FILTER FROM WAVE BRAM1
signal new_data : std_logic;
signal OUT_512 : std_logic_vector(11 downto 0);
signal INT_100 : std_logic_vector(7 downto 0);

--------------DEFINITIONS
CONSTANT TS_ADDR : std_logic_vector(6 downto 0):="0111000";
CONSTANT TS_TOUCH1_XH : std_logic_vector(7 downto 0):=x"03";
CONSTANT TS_TOUCH1_XL : std_logic_vector(7 downto 0):=x"04";
CONSTANT TS_TOUCH1_YH : std_logic_vector(7 downto 0):=x"05";
CONSTANT TS_TOUCH1_YL : std_logic_vector(7 downto 0):=x"06";
CONSTANT TS_ID_G_THDIFF : std_logic_vector(7 downto 0):=x"85"; --COORDINATE THRESHOLD


---------------FOR WORD RAM
CONSTANT A : std_logic_vector(7 downto 0):=x"41";
CONSTANT B : std_logic_vector(7 downto 0):=x"42";
CONSTANT C : std_logic_vector(7 downto 0):=x"43";
CONSTANT D : std_logic_vector(7 downto 0):=x"44";
CONSTANT Ee : std_logic_vector(7 downto 0):=x"45";
CONSTANT F : std_logic_vector(7 downto 0):=x"46";
CONSTANT G : std_logic_vector(7 downto 0):= x"47";
CONSTANT H : std_logic_vector(7 downto 0):= x"48";
CONSTANT I : std_logic_vector(7 downto 0):= x"49";
CONSTANT J : std_logic_vector(7 downto 0):= x"4A";
CONSTANT K : std_logic_vector(7 downto 0):= x"4B";
CONSTANT L : std_logic_vector(7 downto 0):= x"4C";
CONSTANT M : std_logic_vector(7 downto 0):= x"4D";
CONSTANT N : std_logic_vector(7 downto 0):=x"4E";
CONSTANT O : std_logic_vector(7 downto 0):=x"4F";
CONSTANT P : std_logic_vector(7 downto 0):= x"50";
CONSTANT Q : std_logic_vector(7 downto 0):= x"51";
CONSTANT R : std_logic_vector(7 downto 0):= x"52";
CONSTANT Se : std_logic_vector(7 downto 0):=x"53";
CONSTANT T : std_logic_vector(7 downto 0):=x"54";
CONSTANT U : std_logic_vector(7 downto 0):= x"55";
CONSTANT V : std_logic_vector(7 downto 0):= x"56";
CONSTANT W : std_logic_vector(7 downto 0):= x"57";
CONSTANT X : std_logic_vector(7 downto 0):=x"58";
CONSTANT Y : std_logic_vector(7 downto 0):=x"59";
CONSTANT Z : std_logic_vector(7 downto 0):=x"5A";


signal charR1 : std_logic_vector(7 downto 0);
signal charR2 : std_logic_vector(7 downto 0);
signal chaRps : std_logic_vector(7 downto 0);

type word_array is array(0 to 65) of std_logic_vector(7 downto 0);
signal words : word_array;--:=(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A);

type word_arrayb1 is array(0 to 44) of std_logic_vector(7 downto 0);
signal words2 : word_arrayb1;



---------------COLOUR signals for different modes
signal BACKGROUND : std_logic_vector(23 downto 0);
signal BORDER : std_logic_vector(23 downto 0);
signal DRAW_COLOUR : std_logic_vector(23 downto 0);


--------------Signals for word buff
signal word_write_addr : std_logic_vector(8 downto 0);
signal word_read_addr : std_logic_vector(8 downto 0);
signal word_write : std_logic_vector(7 downto 0);
signal word : std_logic_vector(7 downto 0); --------comes out of the word buffer
signal letter : std_logic_vector(7 downto 0);  -----current lettter from ROM
signal letter_Hcount : std_logic_vector(3 downto 0); ----Based off row, for the buffered area
signal letter_pos_count : std_logic_vector(3 downto 0); ---sets up a 16 unit space in which the letter sits in the middle
signal letter_in_addr : integer range 0 to 7; ---for addressing the bit of the LETTER
signal word_en : std_logic_vector(0 downto 0);
signal resize_buff : std_logic_vector(15 downto 0);   ------a buffer to rezise to letter_Hcount
signal letter_on : std_logic; ----'1' when drawing a letter, 0 when the gap between leters

signal word_write_addr2 : std_logic_vector(8 downto 0);
signal word_read_addr2 : std_logic_vector(8 downto 0);
signal word_write2 : std_logic_vector(7 downto 0);
signal word2 : std_logic_vector(7 downto 0); --------comes out of the word buffer
signal letter2 : std_logic_vector(7 downto 0);  -----current lettter from ROM
signal letter_Hcount2 : std_logic_vector(3 downto 0); ----Based off row, for the buffered area
signal letter_pos_count2 : std_logic_vector(3 downto 0); ---sets up a 16 unit space in which the letter sits in the middle
signal letter_in_addr2 : integer range 0 to 7; ---for addressing the bit of the LETTER
signal word_en2 : std_logic_vector(0 downto 0);
signal resize_buff2 : std_logic_vector(15 downto 0);   ------a buffer to rezise to letter_Hcount
signal letter_on2 : std_logic; ----'1' when drawing a letter, 0 when the gap between leters
signal word_addr_add : std_logic_vector(8 downto 0);


begin

SWB: SCREEN_WORD_BUFFER
  PORT MAP (
    clka => clk,
    wea => word_en,
    addra => word_write_addr,
    dina => word_write,
    clkb => p_clk,
    addrb => word_read_addr,
    doutb => word
    );

SWB2: SCREEN_WORD_BUFFER
  PORT MAP (
    clka => clk,
    wea => word_en2,
    addra => word_write_addr2,
    dina => word_write2,
    clkb => p_clk,
    addrb => word_read_addr2 + word_addr_add,
    doutb => word2
    );

FONTER: font_rom  ----------------ascii ROM
   port map(
      clk => p_clk,
      addr => word(6 downto 0) & letter_Hcount,
      data => letter
      );

FONTER2: font_rom  ----------------ascii ROM
   port map(
      clk => p_clk,
      addr => word2(6 downto 0) & letter_Hcount2,
      data => letter2
      );

TOUCH_CONTROLLER: i2c_master
PORT MAP(clk => clk,
        reset_n=>'1',
        ena => i2c_ena,
        addr => i2c_addr,
        rw => i2c_rw,
        data_wr => i2c_data_wr,
        busy => i2c_busy,
        data_rd => i2c_data_rd,
        ack_error => TS_ack_err,
        sda => SDA,
        scl => SCL);
         
      

Sreen_sync: Sync_gen
PORT MAP(Pixel_clk => p_clk, 
        h_sync => HSYNC,
        v_sync => VSYNC,
        row => row,
        column => column,
        disp_ena => DE_sig);



WAVE_RAM: blk_mem_gen_0
  PORT MAP(
    clka => clk,
    wea => "1",
    addra => draw_pos_in,
    dina => draw_in,

    clkb => p_clk,
    addrb => column_buff,
    doutb => Y1(11 downto 0));


WAVE_RAM_FOR_INT: blk_mem_gen_0
  PORT MAP(
    clka => p_clk,
    wea => "1",
    addra => column_buff - skip_count, ---address goes back 1 for every 4 samples, thus turning 683 samples in to 512
    dina => Y1(11 DOWNTO 0),

    clkb => clk,
    addrb => INT_addr,
    doutb => S_buff(11 downto 0));


----------------ROM INSTANCES
ROM1: SINE_ROM
  PORT MAP (
    clka => clk,
    ena => '1',
    addra => draw_pos(9 downto 0),
    douta => sine_write
  );

ROM2: SQUARE_ROM
  PORT MAP (
    clka => clk,
    ena => '1',
    addra => draw_pos(9 downto 0),
    douta => square_write
  );
  
ROM3: SAW_ROM
    PORT MAP (
      clka => clk,
      ena => '1',
      addra => draw_pos(9 downto 0),
      douta => saw_write
    );   
    
ROM4: TRIANGLE_ROM
      PORT MAP (
        clka => clk,
        ena => '1',
        addra => draw_pos(9 downto 0),
        douta => triangle_write
      );
      
rom5: NOISE_ROM
        PORT MAP (
          clka => clk,
          ena => '1',
          addra => draw_pos(9 downto 0),
          douta => noise_write
        );


Pixel_clk <= P_clk;
DE <= DE_sig;

process(clk) begin  ------Increment INT_addr at correct rate
if(clk'event and clk='1') then
    S <= S_buff - 222;

end if;
end process;

process(clk) begin  ------Increment INT_addr at correct rate
if(clk'event and clk='1') then

--    if(INT_WANTS='0') then
--        new_addr <='0';
--    else        
--        new_addr <='1';
--    if(INT_100=99)then
--        new_data<='1';
--        INT_100<=(others => '0');
        if(INT_addr<511)then
            INT_addr<=INT_addr+1;
       else
            INT_addr<=(others => '0');
        end if;
--    end if;
--    else
--        INT_100<=INT_100+1;
--        new_data<='0';
--    end if;
    S_addr <= INT_addr;
    
    

end if;
end process;

process(clk) begin  ------Write to word buffer
if(clk'event and clk='1') then

    if(word_write_addr <65)then
        word_write_addr <= word_write_addr+1;
        word_en <="1";
        word_write<= words(to_integer(unsigned(word_write_addr)));
    else
        word_write_addr<=(others=>'0');
    end if;
   
   IF (mode = "000") then 
     words <= (A,M,P,L,I,T,U,D,Ee,x"20",x"3d",x"20", x"41",x"20",P,I,T,C,H,x"20",X"3D",x"20",charps,charR1, charR2,x"20",x"20",x"20",F,I,N,Ee,x"20",X"3D",x"20", X"41",X"41",x"20",x"20",x"20",x"20",V,I,Ee,W,x"20",x"20",x"20",x"20",x"20",x"20",x"20",A,C,C,EE,P,T,x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20");
     words2 <= (x"20",x"20",Se,I,N,Ee,x"20", x"20",x"20",x"20",Se,Q,U,A,R,Ee,x"20",x"20",x"20",x"20",x"20",Se,A,W,x"20",x"20",x"20",T,R,I,A,N,G,L,Ee,x"20",x"20",N,O,I,Se,Ee,x"20",x"20",x"20"); 
   elsif (mode="001")then
    words <= (A,M,P,L,I,T,U,D,Ee,x"20",x"3d",x"20", x"41",x"20",I,N,D,EE,X,x"20",X"3D",x"20",X"41",X"41",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20", x"20",x"20",x"20",x"20",x"20",x"20",V,I,Ee,W,x"20",x"20",x"20",x"20",x"20",x"20",x"20",A,C,C,EE,P,T,x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20");
    words2 <= (x"20",x"20",Se,I,N,Ee,x"20", x"20",x"20",x"20",Se,Q,U,A,R,Ee,x"20",x"20",x"20",x"20",x"20",Se,A,W,x"20",x"20",x"20",T,R,I,A,N,G,L,Ee,x"20",x"20",N,O,I,Se,Ee,x"20",x"20",x"20"); 
   elsif (mode="010")then
    words <= (C,U,T,x"20",O,F,F,x"20",X"3D",x"20",x"41",x"41", x"41",x"20",x"20",x"20",x"20",Q,x"20",x"3d",X"20",x"41",X"41",X"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20", x"20",x"20",x"20",x"20",x"20",x"20",V,I,Ee,W,x"20",x"20",x"20",x"20",x"20",x"20",x"20",A,C,C,EE,P,T,x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20");
    words2 <= (x"20",x"20",H,P,F,x"20",x"20",x"20",x"20",x"20",x"20",x"20",L,P,F,x"20",x"20",x"20",x"20",x"20",x"20",B,P,F,x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20"); 
    elsif (mode="011")then
       words <= (A,T,T,A,C,K,x"20",x"20",X"3D",x"20",charR1,charR2, x"41",x"20",x"20",x"20",x"20",Q,x"20",x"3d",X"20",x"41",X"41",X"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20", x"20",x"20",x"20",x"20",x"20",x"20",V,I,Ee,W,x"20",x"20",x"20",x"20",x"20",x"20",x"20",A,C,C,EE,P,T,x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20");
       words2 <= (x"20",x"20",H,P,F,x"20",x"20",x"20",x"20",x"20",x"20",x"20",L,P,F,x"20",x"20",x"20",x"20",x"20",x"20",B,P,F,x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20",x"20"); 
   end if;
  
   if(word_write_addr2 <44)then
       word_write_addr2 <= word_write_addr2+1;
       word_en2 <="1";
       word_write2<= words2(to_integer(unsigned(word_write_addr2)));
   else
       word_write_addr2<=(others=>'0');
       
   end if;
   
END IF;                                
end process;


process(p_clk) begin  ------Increment INT_addr at correct rate
if(p_clk'event and p_clk='1') then
    if(down3_4<3)then
        down3_4<=down3_4+1;
    else
        down3_4<=(others=> '0');
        if(column<683) then
    
            skip_count<=skip_count+1;
        else
            skip_count<=(others=> '0');
        end if;
    end if;
    
if(column<683)then
    column_buff<="0" & column;
end if;

end if;
end process;



process(p_clk) begin
if(p_clk'event and p_clk='1') then


            if(mode="000")then  ---------------PRIMARY SYNTHASIS --RED THEME, WAVEDRAW
                DRAW_COLOUR <= x"EE0000"; 
                BACKGROUND  <= x"990909";
                BORDER      <= x"FF1111";
             
            
            elsif(mode="001")then  ----------FM(PM). Blue theme, wave draw
                DRAW_COLOUR <= x"3FA0FF"; 
                BACKGROUND  <= x"0000AA";
                BORDER      <= x"00CFFF";   
                         
           elsif(mode="010")then  ----------FILTER BLOCK. green theme, type select
                DRAW_COLOUR <= x"00CC00"; 
                BACKGROUND  <= x"004C00";
                BORDER      <= x"00FF00";              
            elsif(mode="011")then  ----------ENVELOPE BLOCK. orange, type select
                     DRAW_COLOUR <= x"FF9900"; 
                     BACKGROUND  <= x"663D00";
                     BORDER      <= x"FFAD33";   
             elsif(mode="111")then  ----------ENVELOPE BLOCK. orange, type select
                      DRAW_COLOUR <= x"FF9900"; 
                      BACKGROUND  <= x"000000";
                      BORDER      <= x"FF00FF"; 
            end if;
            
            
               if(row < 440)then  ------------IN PRIMARY DDS MODE
                    if(column < 683)then 
                       if((column=1 or column=682) and (row > 210 and row < 233)) then   --------Draw endzone balls               
                                RGB<=BORDER;
                       elsif((column=2 or column=3 or column= 4 or column=681 or column=680 or column=679) and (row > 211 and row < 232)) then
                                RGB<=BORDER;
                       elsif((column=5 or column=678) and (row > 212 and row < 231)) then                          
                                RGB<=BORDER;
                       elsif((column=6 or column=7 or column=677 or column = 676) and (row > 213 and row < 230)) then                          
                                RGB<=BORDER;
                       elsif((column=8 or column=675) and (row > 214 and row < 229)) then                          
                                RGB<=BORDER;
                       elsif((column=9 or column=674) and (row > 215 and row < 228)) then                          
                                RGB<=BORDER;
                       elsif((column=10 or column=673) and (row > 217 and row < 226)) then                          
                                RGB<=BORDER;
                       elsif((column=11 or column=672) and (row > 219 and row < 224)) then                          
                                RGB<=BORDER;
                       else    
                       
                            if(row < (Y1+circle_height) and row > (Y1-circle_height)) then
                                RGB<=DRAW_COLOUR;
                            else
                                RGB<=BACKGROUND;  ----------------background
                            end if;
                       end if;
                   elsif(column < 688)then
                           RGB<=BORDER;  --------------Border Line

                   elsif((row > 85 and row < 91) or (row > 173 and row < 179) or (row > 261 and row < 267) or (row > 349 and row < 355))then
                           RGB<=BORDER;
                  elsif((row > 35 and row < 51) or (row > 123 and row < 139) or (row > 211 and row < 227) or (row > 299 and row < 315) or (row > 387 and row < 403)) then
                                 if (letter2(letter_in_addr2)='1' and letter_on2 ='1') then
                                    RGB<=DRAW_COLOUR;
                                  else
                                     RGB<=BACKGROUND;  
                                end if;   
                   else
                           RGB<=BACKGROUND;
                      
                   end if;
                   
               elsif(row <445)then
                           RGB<=BORDER;  --------------Border Line 
               elsif((row > 454) and (row < 472))then
                    if (letter(letter_in_addr)='1' and letter_on ='1') then
                        RGB<=DRAW_COLOUR;
                       else
                         RGB<=BACKGROUND;  
                    end if; 
                   
               else
                       RGB<=BACKGROUND;
                end if;    
           
               if(column = 0)then
                    letter_pos_count <= (others => '0');
                    word_read_addr <= (others => '0');
                else
                    if(letter_pos_count <11)then
                         letter_pos_count <= letter_pos_count+1;
                    else
                        letter_pos_count <= (others => '0');
                        word_read_addr <= word_read_addr+1;
                    end if;
                end if;
                
                 if(word_read_addr2 = 9) then
                    word_read_addr2 <= (others => '0');
                    letter_pos_count2 <= (others => '0'); 
                 end if;
                 
                 if(column = 688)then
                     letter_pos_count2 <= (others => '0');
                     word_read_addr2 <= (others => '0');
                 else
                     if(letter_pos_count2 <11)then
                          letter_pos_count2 <= letter_pos_count2+1;
                     else
                         letter_pos_count2 <= (others => '0');
                         word_read_addr2 <= word_read_addr2+1;
                     end if;
                 end if;
           
            case letter_pos_count is          
                when x"3" => 
                    letter_in_addr <= 7;
                    letter_on<='1';
                when x"4" => 
                    letter_in_addr <= 6;
                when x"5" => 
                    letter_in_addr <= 5;
                when x"6" => 
                    letter_in_addr <= 4;
                when x"7" => 
                    letter_in_addr <= 3;
                when x"8" => 
                    letter_in_addr <= 2;
                when x"9" => 
                    letter_in_addr <= 1;
                when x"A" => 
                    letter_in_addr <= 0;
                when others =>  
                    letter_in_addr <= 0;
                    letter_on <= '0';
           end case;

            case letter_pos_count2 is          
                when x"3" => 
                    letter_in_addr2 <= 7;
                    letter_on2<='1';
                when x"4" => 
                    letter_in_addr2 <= 6;
                when x"5" => 
                    letter_in_addr2 <= 5;
                when x"6" => 
                    letter_in_addr2 <= 4;
                when x"7" => 
                    letter_in_addr2 <= 3;
                when x"8" => 
                    letter_in_addr2 <= 2;
                when x"9" => 
                    letter_in_addr2 <= 1;
                when x"A" => 
                    letter_in_addr2 <= 0;
                when others =>  
                    letter_in_addr2 <= 0;
                    letter_on2 <= '0';
           end case;

END IF;

end process;


process(clk) begin   ---"interpolator from touch to screen BRAM
if(clk'event and clk='1')then
--    if(XTOUCH < x"2AB") then     ----------683 chosen because 683 * 3/4 ~=512, so sample drop is easy
--        wave_en <= "1";
--    else
--        wave_en <= "0";
--    end if;
      
        if(int_count<10)then
            int_count<=int_count+1;
        else
            int_count<=(others => '0');
        end if;    
        
        
       XPOS_total<= XTOUCH(11 downto 0) + int_count -5;
        
       if(XPOS_total < 683)then  ---------To test if in draw range
            XPOS<=XPOS_total;
       end if; 
       
        
        case int_count is -----To create circular points rather than square
            when x"1"|x"A" =>
                circle_height <= "010";
            when x"2"|x"9" =>
                circle_height <= "011";    
            when x"3"|x"8" =>
                circle_height <= "100";
            when others =>
                circle_height <= "101";
       end case;
        
end if;
end process;



process(clk) begin   --PIXEL CLOCK DIVIDER. 147.145/4=36.786250MHz
if(clk'event and clk='1')then
    if(P_count<2)then
        p_clk<='0';
        P_count<=P_count+1;
    elsif(P_count<3)then
        p_clk<='1';
        P_count<=P_count+1;
    else
        P_count<=0;
    end if;
end if;



end process;



process(p_clk) begin  --for word buffering stuff
if(p_clk'event and p_clk='1') then
    if((row > 454) and (row < 472)) then  --gives horizontal letter position
        resize_buff<=row-455;
        letter_Hcount <= resize_buff(3 downto 0);
    end if;
    
    
    if((row > 35) and (row < 51)) then  --for 1st block
        resize_buff2<=row-36;
        word_addr_add<=(others => '0');
        letter_Hcount2 <= resize_buff2(3 downto 0);
    end if;        
    
    if((row > 123) and (row < 139)) then  --gives horizontal letter position
        resize_buff2<=row-124;
        word_addr_add<= '0' & x"09";
        letter_Hcount2 <= resize_buff2(3 downto 0);
    end if;
    
    if((row > 211) and (row < 227)) then  --gives horizontal letter position
        resize_buff2<=row-212;
        word_addr_add<='0' & x"12";
        letter_Hcount2 <= resize_buff2(3 downto 0);
    end if;   
 
     if((row > 299) and (row < 315)) then  --gives horizontal letter position
        resize_buff2<=row-300;
        word_addr_add<='0' & x"1B";
        letter_Hcount2 <= resize_buff2(3 downto 0);
    end if;
    
    if((row > 387) and (row < 403)) then  --gives horizontal letter position
        resize_buff2<=row-388;
        word_addr_add<='0' & x"24";
        letter_Hcount2 <= resize_buff2(3 downto 0);
    end if;   
       
end if;
end process;

process(clk) begin  --Handle rotary inputs
if(clk'event and clk='1') then
    case mode is
        when "000" =>
                   if(add_note > 9 or add_note < -9) then
                       charR1 <= x"31"; 
                   else
                       charR1 <= x"20";
                   end if;
                   
                   charR2 <= add_note + x"30";            
            
            
            if(add_note(4) = '1')then
                charps <= x"2D";
            else
                charps <= x"20";
            end if; 
            
      when "011" =>
            CHARR1 <= "0" & attack + x"21";
            CHARR2 <= "0" & attack + x"21";
            CHARps <= x"20";      
      when others => --  
      end case;  
end if;
end process;

process(clk) begin  --checks side button and acts on them
if(clk'event and clk='1') then
    if(draw_pos > 682)then
        if(XTOUCH(15 downto 14)="00" and XTOUCH(11 downto 0) > 700)then ---checks if it's the beginning of a touch and if it's in the side_button area
        
            if(YTOUCH(11 downto 0) > 5 and YTOUCH(11 downto 0) < 83)then  ---cchecks which button
                draw_pos<=(others=>'0');
                draw_from_rom <= "001";
                ROM_ena <='1';
            elsif(YTOUCH(11 downto 0) > 93 and YTOUCH(11 downto 0) < 171)then
                draw_pos<=(others=>'0');
                draw_from_rom <= "010"; 
                ROM_ena <='1';  
            elsif(YTOUCH(11 downto 0) > 181 and YTOUCH(11 downto 0) < 259)then       
                draw_pos<=(others=>'0');
                draw_from_rom <= "011";
                ROM_ena <='1';    
            elsif(YTOUCH(11 downto 0) > 269 and YTOUCH(11 downto 0) < 347)then      
                 draw_pos<=(others=>'0');
                 draw_from_rom <= "100";
                ROM_ena <='1';  
            elsif(YTOUCH(11 downto 0) > 357 and YTOUCH(11 downto 0) < 443)then      
                draw_pos<=(others=>'0');
                draw_from_rom <= "101";
                ROM_ena <='1';
            end if;
        else
            draw_in <= YTOUCH(11 downto 0);
            draw_pos_in <= XPOS;
            ROM_ena <='0';
        end if;
    else
        case draw_from_rom is
            when "001" =>
                draw_in<=Sine_write;
            when "010" =>
                draw_in<=square_write; 
            when "011" =>
                draw_in<=saw_write;
            when "100" =>
                draw_in<=triangle_write;
            when "101" =>   
                draw_in<=noise_write;  
            when others =>   --
       end case;    
                draw_pos<=draw_pos+1;
                draw_pos_in <= draw_pos;
    end if;

end if;
end process;


process(clk) begin  --I2C Continuous read for TS XPOS and YPOS
if(clk'event and clk='1') then
                              --state for conducting this transaction
  busy_prev <= i2c_busy;                       --capture the value of the previous i2c busy signal
  IF(busy_prev = '0' AND i2c_busy = '1') THEN  --i2c busy just went high
    busy_cnt <= busy_cnt + 1;                  --counts the times busy has gone from low to high during transaction
  END IF;
  
      if(y_toggle='0') then
          CASE busy_cnt IS                             --busy_cnt keeps track of which command we are on
            WHEN 0 =>                                  --no command latched in yet
              i2c_ena <= '1';                            --initiate the transaction
              i2c_addr <= TS_ADDR;                    --set the address of the slave
              i2c_rw <= '0';                             --command 1 is a write
              i2c_data_wr <= TS_TOUCH1_XH;              --data to be written
            WHEN 1 =>                                  --1st busy high: command 1 latched, okay to issue command 2
              i2c_rw <= '1';                             --command 2 is a read (addr stays the same)
            WHEN 2 =>                                  --2nd busy high: command 2 latched, okay to issue command 3
              i2c_rw <= '0';                             --command 3 is a write
              i2c_data_wr <= TS_TOUCH1_XL;                --data to be written
              IF(i2c_busy = '0') THEN                    --indicates data read in command 2 is ready
                XTOUCH(15 DOWNTO 8) <= i2c_data_rd;       --retrieve data from command 2
              END IF;
            WHEN 3 =>                                  --3rd busy high: command 3 latched, okay to issue command 4
              i2c_rw <= '1';                             --command 4 is read (addr stays the same)
            WHEN 4 =>                                  --4th busy high: command 4 latched, ready to stop
              i2c_ena <= '0';                            --deassert enable to stop transaction after command 4
              IF(i2c_busy = '0') THEN                    --indicates data read in command 4 is ready
                XTOUCH(7 DOWNTO 0) <= i2c_data_rd;         --retrieve data from command 4
                busy_cnt <= 0;                           --reset busy_cnt for next transaction
                y_toggle<='1';                                         --transaction complete, go to next state in design
              END IF;
            WHEN OTHERS => busy_cnt <= 0; 
            end case;
        else   
           CASE busy_cnt IS 
                                         --busy_cnt keeps track of which command we are on
               WHEN 0 =>                                  --no command latched in yet
                 i2c_ena <= '1';                            --initiate the transaction
                 i2c_addr <= TS_ADDR;                    --set the address of the slave
                 i2c_rw <= '0';                             --command 1 is a write
                 i2c_data_wr <= TS_TOUCH1_YH;              --data to be written
               WHEN 1 =>                                  --1st busy high: command 1 latched, okay to issue command 2
                 i2c_rw <= '1';                             --command 2 is a read (addr stays the same)
               WHEN 2 =>                                  --2nd busy high: command 2 latched, okay to issue command 3
                 i2c_rw <= '0';                             --command 3 is a write
                 i2c_data_wr <= TS_TOUCH1_YL;          --data to be written
                 IF(i2c_busy = '0') THEN                    --indicates data read in command 2 is ready
                   YTOUCH(15 DOWNTO 8) <= i2c_data_rd;       --retrieve data from command 2
                 END IF;
               WHEN 3 =>                                  --3rd busy high: command 3 latched, okay to issue command 4
                 i2c_rw <= '1';                             --command 4 is read (addr stays the same)
               WHEN 4 =>                                  --4th busy high: command 4 latched, ready to stop
                 i2c_ena <= '0';                            --deassert enable to stop transaction after command 4
                 IF(i2c_busy = '0') THEN                    --indicates data read in command 4 is ready
                   YTOUCH(7 DOWNTO 0) <= i2c_data_rd;         --retrieve data from command 4
                   busy_cnt <= 0;                           --reset busy_cnt for next transaction
                   y_toggle<='0';                                         --transaction complete, go to next state in design
                 END IF;
                WHEN OTHERS => busy_cnt <= 0; 
               end case;
         end if;
  
end if;
end process;








end Behavioral;
