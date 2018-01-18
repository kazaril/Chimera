----SYNTH TOP_LEVEL


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;


entity SCREEN_CONTROL is
    Port ( gclk : in STD_LOGIC;  --100MHz INPUT CLOCK
           R : out STD_LOGIC_VECTOR (7 downto 0); ---RGB OUT
           G : out STD_LOGIC_VECTOR(7 downto 0);
           B : out std_logic_vector(7 downto 0);
           Pixel_clk : out std_logic;   --32MHz OUTPUT CLK TO SCREEN
           leds : out std_logic_vector(7 downto 0); --FOR DEBUG, ON ZEDBOARD
           O1 : out std_logic;  --TESTPOINT ON BREAKOUT
           O2 : out std_logic;
           JB1 : out std_logic;  --TESTPOINT ON PMOD B1
           MIDI_IN : in std_logic;  --MIDI_IN
           HSYNC : out std_logic; --VGA SYNC SIGNALS
           VSYNC : out std_logic;
           DISP : out std_logic;  -- NOT SURE IF USED? DISPLAY NEABLE
           SW : in std_logic; --ZEDBOARD SWITCH 0
           SDA : inout std_logic; -- TS I2C
           SCL : inout std_logic;
           CTP_RST : out std_logic; --ACTIVE HIGH TS RESET (JUST KEEP HIGH?)
           DE : out std_logic -- DISPLAY ENABLE FOR VGA TIMING
           );
end SCREEN CONTROL;

architecture Behavioral of TOP_LEVEL is

component Sync_gen is
    

    Port (pixel_clk :  IN   STD_LOGIC;  --pixel clock at frequency of VGA mode being used
    
    h_sync    :  OUT  STD_LOGIC;  --horiztonal sync pulse
    v_sync    :  OUT  STD_LOGIC;  --vertical sync pulse
    disp_ena  :  OUT  STD_LOGIC;  --display enable ('1' = display time, '0' = blanking time)
    column    :  OUT  std_logic_vector(10 downto 0);    --horizontal pixel coordinate
    row       :  OUT  std_logic_vector(15 downto 0));    --vertical pixel coordinate);
end component;

component Master_clk    --Takes in 100Mhz clock and creates 132MHz master clock
port
 (-- Clock in ports
  clk_in1           : in     std_logic;
  -- Clock out ports
  clk_out1          : out    std_logic
 );
end component;

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
    input_clk : INTEGER := 132_000_000; --input clock speed from user logic in Hz
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

type SCREEN_ARRAY is array (0 to 480) of std_logic_vector(9 downto 0);

signal SCREEN_X : SCREEN_ARRAY;

signal P_count : integer range 0 to 6; --for pixel clock divider
signal P_clk : std_logic; --pixel clock buffer
signal clk : std_logic; --master clock
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

-----------I2C signals for DAC
signal dac_ena : std_logic;
signal dac_addr : std_logic_vector(6 downto 0);
signal dac_rw : std_logic;
signal dac_data_wr : std_logic_vector(7 downto 0);
signal dac_busy : std_logic;
signal dac_busy_prev : std_logic;
signal dac_data_rd : std_logic_vector(7 downto 0);
signal dac_ack_err : std_logic;
signal dac_busy_cnt : integer range 0 to 20;


----------Interface BRAM with VGA
signal row : std_logic_vector(15 downto 0);
signal column : std_logic_vector(10 downto 0);
signal Y1 : std_logic_vector(15 downto 0);

--------------DEFINITIONS
CONSTANT TS_ADDR : std_logic_vector(6 downto 0):="0111000";
CONSTANT TS_TOUCH1_XH : std_logic_vector(7 downto 0):=x"03";
CONSTANT TS_TOUCH1_XL : std_logic_vector(7 downto 0):=x"04";
CONSTANT TS_TOUCH1_YH : std_logic_vector(7 downto 0):=x"05";
CONSTANT TS_TOUCH1_YL : std_logic_vector(7 downto 0):=x"06";

CONSTANT DAC_ADDR : std_logic_vector(6 downto 0):="0111000";

begin


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
         
DAC_CONTROLLER: i2c_master
        PORT MAP(clk => clk,
                reset_n=>'1',
                ena => iDAC_ena,
                addr => DAC_addr,
                rw => DAC_rw,
                data_wr => DAC_data_wr,
                busy => DAC_busy,
                data_rd => DAC_data_rd,
                ack_error => DAC_ack_err,
                sda => DAC_SDA,
                scl => DAC_SCL);        

Sreen_sync: Sync_gen
PORT MAP(Pixel_clk => p_clk, 
        h_sync => HSYNC,
        v_sync => VSYNC,
        row => row,
        column => column,
        disp_ena => DE_sig);

Masetr_gen: Master_clk
PORT MAP(clk_in1 => gclk,
        clk_out1 => clk);

WAVE_RAM: blk_mem_gen_0
  PORT MAP(
    clka => clk,
    wea => "1",
    addra => XTOUCH(11 downto 0),
    dina => YTOUCH(11 DOWNTO 0),
    clkb => p_clk,
    addrb => '0' & column,
    doutb => Y1(11 downto 0));


Pixel_clk <= P_clk;
CTP_RST <= SW;
DE <= DE_sig;
JB1 <= p_clk;


process(p_clk) begin
if(p_clk'event and p_clk='1') then
    if(Y1=row) then
        R<=x"FF";
        G<=x"00";
        B<=x"00";
    else
        R<=x"00";
        G<=x"FF";
        B<=x"00";
    end if;
END IF;

end process;

process(clk) begin
if(clk'event and clk='1') then
    leds<=i2c_data_rd;
end if;
end process;

process(clk) begin   --PIXEL CLOCK DIVIDER. 132/4=33MHz
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


process(clk) begin   --
if(clk'event and clk='1')then


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
          i2c_data_wr <= TS_TOUCH1_XL;          --data to be written
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




process(clk) begin  --I2C Set up for DAC
if(clk'event and clk='1') then
                              --state for conducting this transaction
  dac_busy_prev <= dac_busy;                       --capture the value of the previous i2c busy signal
  IF(dac_busy_prev = '0' AND dac_busy = '1') THEN  --i2c busy just went high
    dac_busy_cnt <= dac_busy_cnt + 1;                  --counts the times busy has gone from low to high during transaction
  END IF;

      CASE dac_busy_cnt IS                             --busy_cnt keeps track of which command we are on
        WHEN 0 =>                                  --no command latched in yet
          dac_ena <= '1';                            --initiate the transaction
          dac_addr <= TS_ADDR;                    --set the address of the slave
          i2c_rw <= '0';                             --command 1 is a write
          i2c_data_wr <= TS_TOUCH1_XH;              --data to be written
        WHEN 1 =>                                  --1st busy high: command 1 latched, okay to issue command 2
          i2c_rw <= '1';                             --command 2 is a read (addr stays the same)
        WHEN 2 =>                                  --2nd busy high: command 2 latched, okay to issue command 3
          i2c_rw <= '0';                             --command 3 is a write
          i2c_data_wr <= TS_TOUCH1_XL;          --data to be written
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
  

    
end if;
end process;

end Behavioral;
