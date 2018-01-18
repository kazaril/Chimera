-------handles i2c for DAC control, also converts serial audio into high speed serial for output via DAC


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;



entity AUDIO_OUT is
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
           leds : out std_logic_vector(7 downto 0); ---for debug
           locked:in STD_LOGIC  
);
end AUDIO_OUT;

architecture Behavioral of AUDIO_OUT is
    --- CLOCK------

    ---CLOCK COUNTER

    ---TIME 0 COUNTER --
    signal cnt_250ms:std_logic_vector(25 downto 0);
    --SPI STUFF
    signal SPI_delay:std_logic_vector(4 downto 0);
    signal SPI_on:std_logic;
    signal state_machine:std_logic_vector(7 downto 0);
    signal start_SPI_transmission: std_logic;
    signal clatch_sig:std_logic; -- need this, don't delete , feed back
    signal spi_data_in:std_logic_vector(23 downto 0);
    ---- CODEC STUFF
    signal sdata_buff: std_logic_vector(63 downto 0);
    signal lrclk_buff: std_logic;
    signal sdata_buff2: std_logic;
    signal initialized_sig:Std_logic:='0';
    
-- COMPONENT DECLARATION    
component spi_interface is
    Port ( 
       clk_250:in std_logic;
       cclk : out STD_LOGIC; -- MAX 10Mhz
       start:in STD_LOGIC; -- enable transaction
       clatch : out STD_LOGIC; -- slave select
       data_in : in STD_LOGIC_VECTOR (23 downto 0); -- first 8 bit downto 0
       cdata : out STD_LOGIC -- mosi , miso is cout, ignored
       ); -- transaction in progress
end component;

--component serial_driver is
--Port (
--     clk_250:in STD_LOGIC;
--     left : in STD_LOGIC_VECTOR (31 downto 0);
--     right : in STD_LOGIC_VECTOR (31 downto 0);
--     bclk: out STD_LOGIC;
--     lrclk:out STD_LOGIC;
--     sdata:out STD_LOGIC;
--     mclk:out STD_LOGIC;
--     initialized:in Std_logic
--);
--end component;

component HIGH_SPEED_SERIAL is
     Port (
          clk:in STD_LOGIC;
          left : in STD_LOGIC_VECTOR (31 downto 0);
          right : in STD_LOGIC_VECTOR (31 downto 0);
          bclk: out STD_LOGIC;
          lrclk:out STD_LOGIC;
          sdata:out STD_LOGIC;
          mclk:out STD_LOGIC
     );
end component;

begin

--- COMPONENT INSTANTIATE ---
SPIder:spi_interface
PORT Map(
    clk_250=>clk,
    cclk=>cclk,
    start=>start_spi_transmission,
    clatch=>clatch_sig,
    cdata=>cdata,
    data_in=>spi_data_in
);

--milk_and:serial_driver
--     Port map(
--          clk_250=>clk,
--          left=>AUDIO_left,
--          right=>AUDIO_right,
--          bclk=>bclk,
--          lrclk=>lrclk,
--          sdata=>sdata,
--          mclk=>mclk,
--          initialized=>initialized_sig
--     );

AUDIO_SERIALIZER: HIGH_SPEED_SERIAL                
PORT MAP(clk => clk,
          left => audio_left(15) &  audio_left(14 downto 0) & x"0000",
          right => audio_right(15) &  audio_right(14 downto 0) & x"0000",
          --left => audio_left & x"0000",
         -- right => audio_right & x"0000",
          bclk => bclk,
          lrclk => lrclk_buff,
          sdata => sdata_buff2,
          mclk => mclk         
          );

--- SIGNAL TO NET-----




sdata<=sdata_buff2;
lrclk<=lrclk_buff;
clatch<=clatch_sig; -- need this intermediate signal to provide feedback for state machine
--- PROCESS DRIVER-------



time0Generator:process(Clk)
begin
if rising_edge(Clk) then
    if locked='1' then
       if cnt_250ms<62499999 then
          cnt_250ms<=cnt_250ms+1;
          SPI_on<='0';
       else
          SPI_on<='1';
       end if;
    else
        SPI_on<='0';
    end if;
end if;    
end process;



start_sequence_withLed:process(Clk) --SPI stuff
begin
if rising_edge(Clk) then
 if SPI_on ='1' then
        if clatch_sig='1' then -- finish writing
              if SPI_delay<24 then
                  SPI_delay<=SPI_delay+1;
              else
                  if initialized_sig='0' then 
                    state_machine<=state_machine+1; -- infinite loop
                    
                    start_SPI_transmission<='1';
                  else
                    start_SPI_transmission<='0';
                  end if;
                  SPI_delay<=(others=>'0');
              end if;
        end if;
        
   else -- SPI ON =0 ;
        state_machine<=(others=>'0');
        start_SPI_transmission<='0';
        
   end if;
   
end if;
end process;

state_machine_LUT:process(Clk)
begin
if rising_edge(Clk) then
    case state_machine is
        when x"00"=> spi_data_in<=x"000000";initialized_sig<='0'; -- first dummy write R0, on with the clock
        when x"01"=> spi_data_in<=x"000000";initialized_sig<='0'; -- second dummy write
        when x"02"=> spi_data_in<=x"000000";initialized_sig<='0'; --third dummy write
        when x"03"=> spi_data_in<=x"000000";initialized_sig<='0'; -- just don't write      
        when x"04"=> spi_data_in<=x"4000"& x"01";initialized_sig<='0'; -- R0 mclk on,fs =master on div 1024 , enable, 
        when x"05"=> spi_data_in<=x"4015"& x"18";initialized_sig<='0'; --R1T to set the polarity of the bclk trigger to rising and left channel to rising
        when x"06"=> spi_data_in<=x"4016"& x"01";initialized_sig<='0';-- R16 64 bit, lrdel = 0 (0 delay for b clock)
        when x"07"=> spi_data_in<=x"4019"& x"03";initialized_sig<='0'; -- R22 dummy write
        when x"08"=> spi_data_in<=x"401C"& x"2B";initialized_sig<='0'; --R24 first dummy write  00100001
        when x"09"=> spi_data_in<=x"401E"& x"4B";initialized_sig<='0';-- R29 ENABLE, VOLCONTROL MAX VOLUME, ENABle,UNMUTE   01000001
        when x"0A"=> spi_data_in<=x"4020"& x"05";initialized_sig<='0'; -- R30 ENABLE HEADPHONE INSTEAD OF LINE
        when x"0B"=> spi_data_in<=x"4021"& x"11";initialized_sig<='0'; -- R35 playback all normal (default)
        when x"0C"=> spi_data_in<=x"4023"& x"BB";initialized_sig<='0';-- R36 2 DACs both on   11100011   11011011
        when x"0D"=> spi_data_in<=x"4024"& x"BB";initialized_sig<='0'; -- R58 0 padding bit from LRCLOCK 
        when x"0E"=> spi_data_in<=x"4025"& x"BB";initialized_sig<='0'; -- R65 enable all clock, 
        when x"0F"=> spi_data_in<=x"4029" &x"03";initialized_sig<='0'; -- R66 enable clock 0   
        when x"10"=> spi_data_in<=x"402A" &x"03";initialized_sig<='0'; -- R66 enable clock 0 
        when x"11"=> spi_data_in<=x"40F2" &x"01";initialized_sig<='0'; -- R66 enable clock 0
        when x"12"=> spi_data_in<=x"40F9" &x"7F";initialized_sig<='0'; -- R66 enable clock 0
        when x"13"=> spi_data_in<=x"402C"& x"08";initialized_sig<='0'; -- R65 enable all clock, 
        when x"14"=> spi_data_in<=x"402B" &x"08";initialized_sig<='0'; -- R66 enable clock 0
        when x"15"=> spi_data_in<=x"40FA" &x"01";initialized_sig<='1';  -- R66 enable clock 0
        when others => --          
    end case;
end if;

--if rising_edge(Clk) then
--    case state_machine is
--        when x"00"=> spi_data_in<=x"000000";initialized_sig<='0'; -- first dummy write R0, on with the clock
--        when x"01"=> spi_data_in<=x"000000";initialized_sig<='0'; -- second dummy write
--        when x"02"=> spi_data_in<=x"000000";initialized_sig<='0'; --third dummy write
--        when x"03"=> spi_data_in<=x"000000";initialized_sig<='0'; -- just don't write
--        when x"04"=> spi_data_in<=x"4000"& x"01";initialized_sig<='0'; -- R0 mclk on,fs =master on div 1024 , enable,
--        when x"05"=> spi_data_in<=x"4015"& x"18";initialized_sig<='0'; --R1T to set the polarity of the bclk trigger to rising and left channel to rising
--        when x"06"=> spi_data_in<=x"4016"& x"01";initialized_sig<='0';-- R16 64 bit, lrdel = 0 (0 delay for b clock)
--        when x"07"=> spi_data_in<=x"4019"& x"03";initialized_sig<='0'; -- R22 dummy write
--        when x"08"=> spi_data_in<=x"401C"& x"21";initialized_sig<='0'; --R24 first dummy write
--        when x"09"=> spi_data_in<=x"401E"& x"41";initialized_sig<='0';-- R29 ENABLE, VOLCONTROL MAX VOLUME, ENABle,UNMUTE
--        when x"0A"=> spi_data_in<=x"4020"& x"05";initialized_sig<='0'; -- R30 ENABLE HEADPHONE INSTEAD OF LINE
--        when x"0B"=> spi_data_in<=x"4021"& x"11";initialized_sig<='0'; -- R35 playback all normal (default)
--        when x"0C"=> spi_data_in<=x"4023"& x"FF";initialized_sig<='0';-- R36 2 DACs both on
--        when x"0D"=> spi_data_in<=x"4024"& x"FF";initialized_sig<='0'; -- R58 0 padding bit from LRCLOCK
--        when x"0E"=> spi_data_in<=x"4025"& x"FE";initialized_sig<='0'; -- R65 enable all clock, 
--        when x"0F"=> spi_data_in<=x"4029" &x"03";initialized_sig<='0'; -- R66 enable clock 0   
--        when x"10"=> spi_data_in<=x"402A" &x"03";initialized_sig<='0'; -- R66 enable clock 0 
--        when x"11"=> spi_data_in<=x"40F2" &x"01";initialized_sig<='0'; -- R66 enable clock 0
--        when x"12"=> spi_data_in<=x"40F9" &x"7F";initialized_sig<='0'; -- R66 enable clock 0
--        when x"13"=> spi_data_in<=x"40FA" &x"01";initialized_sig<='1';  -- R66 enable clock 0
--        when others => --          
--    end case;
--end if;

end process;

end Behavioral;





----------------I2C VARIANT. DOESN"T YET WORK
--entity AUDIO_OUT is
--    Port ( clk : in STD_LOGIC;
--        DAC_SDA : inout std_logic;
--        DAC_SCL : inout std_logic;
--        locked  : in std_logic;
--        bclk    : out std_logic;
--        lrclk:out STD_LOGIC;
--        sdata:out STD_LOGIC;
--        mclk:out STD_LOGIC;        
--        AUDIO_left : in std_logic_vector(15 downto 0);
--        AUDIO_right : in std_logic_vector(15 downto 0);
        
        
--        DEBUG : out std_logic_vector(4 downto 0)
--        );
--end AUDIO_OUT;








--architecture Behavioral of AUDIO_OUT is

--component HIGH_SPEED_SERIAL is
--     Port (
--          clk:in STD_LOGIC;
--          left : in STD_LOGIC_VECTOR (31 downto 0);
--          right : in STD_LOGIC_VECTOR (31 downto 0);
--          bclk: out STD_LOGIC;
--          lrclk:out STD_LOGIC;
--          sdata:out STD_LOGIC;
--          mclk:out STD_LOGIC
--     );
--end component;

--component i2c_DAC IS --for touch reading and DAC configure
--  GENERIC(
--    input_clk : INTEGER := 147_451_000; --input clock speed from user logic in Hz
--    bus_clk   : INTEGER := 400_000);   --speed the i2c bus (scl) will run at in Hz
--  PORT(
--    clk       : IN     STD_LOGIC;                    --system clock
--    reset_n   : IN     STD_LOGIC;                    --active low reset
--    ena       : IN     STD_LOGIC;                    --latch in command
--    addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
--    sub_addr  : IN     STD_LOGIC_VECTOR(15 downto 0);
--    rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
--    data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data to write to slave
--    busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
--    data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
--    ack_error : BUFFER STD_LOGIC;                    --flag if improper acknowledge from slave
--    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
--    scl       : INOUT  STD_LOGIC);                   --serial clock output of i2c bus
--END component;


-----------DEFINITIONS
--CONSTANT DAC_ADDR_I2C : std_logic_vector(6 downto 0):="0111000";
--CONSTANT DAC_R0 : std_logic_vector(15 downto 0):= x"4000";
--CONSTANT DAC_R15 : std_logic_vector(15 downto 0):= x"4015";
--CONSTANT DAC_R16 : std_logic_vector(15 downto 0):= x"4016";
--CONSTANT DAC_R19 : std_logic_vector(15 downto 0):= x"4019";
--CONSTANT DAC_R22 : std_logic_vector(15 downto 0):= x"401C";
--CONSTANT DAC_R24 : std_logic_vector(15 downto 0):= x"401E";
--CONSTANT DAC_R26 : std_logic_vector(15 downto 0):= x"4020";
--CONSTANT DAC_R27 : std_logic_vector(15 downto 0):= x"4021";
--CONSTANT DAC_R29 : std_logic_vector(15 downto 0):= x"4023";
--CONSTANT DAC_R30 : std_logic_vector(15 downto 0):= x"4024";
--CONSTANT DAC_R31 : std_logic_vector(15 downto 0):= x"4025";
--CONSTANT DAC_R32 : std_logic_vector(15 downto 0):= x"4026";
--CONSTANT DAC_R35 : std_logic_vector(15 downto 0):= x"4029";
--CONSTANT DAC_R36 : std_logic_vector(15 downto 0):= x"402A";
--CONSTANT DAC_R58 : std_logic_vector(15 downto 0):= x"40F2";
--CONSTANT DAC_R65 : std_logic_vector(15 downto 0):= x"40F9";
--CONSTANT DAC_R66 : std_logic_vector(15 downto 0):= x"40FA";

-------------I2C signals for DAC
--signal dac_ena : std_logic;
--signal dac_addr : std_logic_vector(6 downto 0);
--signal dac_rw : std_logic;
--signal dac_data_wr : std_logic_vector(7 downto 0);
--signal DAC_BUSY : std_logic;
--signal dac_busy_prev : std_logic;
--signal dac_data_rd : std_logic_vector(7 downto 0);
--signal dac_ack_err : std_logic;
--signal dac_busy_cnt : integer range 0 to 20;
--signal DAC_sub_addr : std_logic_vector(15 downto 0);
--signal I2C_ON : std_logic; --Wait before starting I2C instructions
--signal cnt_250ms : integer range 0 to 37000000;

--begin



--DAC_CONTROLLER: i2c_DAC
--        PORT MAP(clk => clk,
--                reset_n=>'1',
--                ena => DAC_ena,
--                addr => DAC_addr,
--                sub_addr => DAC_sub_addr,
--                rw => DAC_rw,
--                data_wr => DAC_data_wr,
--                busy => DAC_busy,
--                data_rd => DAC_data_rd,
--                ack_error => DAC_ack_err,
--                sda => DAC_SDA,
--                scl => DAC_SCL);  

--AUDIO_SERIALIZER: HIGH_SPEED_SERIAL                
--PORT MAP(clk => clk,
--          left => audio_left & x"0000",
--          right => audio_right & x"0000",
--          bclk => bclk,
--          lrclk => lrclk,
--          sdata => sdata,
--          mclk => mclk         
--          );
--process(Clk) --check locked is on and then wait 1/4 second for DAC initialise
--begin
--if rising_edge(Clk) then
--    if locked='1' then
--       if cnt_250ms<36999999 then
--          cnt_250ms<=cnt_250ms+1;
--          I2C_on<='0';
--       else
--          I2C_on<='1';
--       end if;
--    else
--        I2C_on<='0';
--    end if;
--end if;    
--end process;

--process(clk) begin  --I2C Set up for DAC
--if(clk'event and clk='1') then
 
--    if(I2C_ON='1') then                             --state for conducting this transaction
--    DEBUG(2)<='1';
--      dac_busy_prev <= dac_busy;                       --capture the value of the previous i2c busy signal
--      IF(dac_busy_prev = '0' AND dac_busy = '1') THEN  --i2c busy just went high
--        dac_busy_cnt <= dac_busy_cnt + 1;                  --counts the times busy has gone from low to high during transaction
--      END IF;

--      CASE dac_busy_cnt IS                             --busy_cnt keeps track of which command we are on
--        WHEN 0 =>                                  --no command latched in yet
--          dac_ena <= '1';                            --initiate the transaction
--          dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--          DAC_sub_addr <= DAC_R0;
--          dac_rw <= '0';                             --command 1 is a write
--          dac_data_wr <= x"01";              --data to be written
--        WHEN 1 =>                                  --1st busy high: command 1 latched, okay to issue command 2
--            dac_ena <= '1';                            --initiate the transaction
--            dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--            DAC_sub_addr <= DAC_R15;
--            dac_rw <= '0';                             --command 1 is a write
--            dac_data_wr <= x"00";              --data to be written                            
--        WHEN 2 =>                                  --2nd busy high: command 2 latched, okay to issue command 3
--            dac_ena <= '1';                            --initiate the transaction
--            dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--            DAC_sub_addr <= DAC_R16;
--            dac_rw <= '0';                             --command 1 is a write
--            dac_data_wr <= x"01";              --data to be written    
--        WHEN 3 =>                                  --no command latched in yet
--              dac_ena <= '1';                            --initiate the transaction
--              dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--              DAC_sub_addr <= DAC_R19;
--              dac_rw <= '0';                             --command 1 is a write
--              dac_data_wr <= x"03";              --data to be written
--        WHEN 4 =>                                  --1st busy high: command 1 latched, okay to issue command 2
--                dac_ena <= '1';                            --initiate the transaction
--                dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                DAC_sub_addr <= DAC_R22;
--                dac_rw <= '0';                             --command 1 is a write
--                dac_data_wr <= x"21";              --data to be written                            
--       WHEN 5 =>                                  --2nd busy high: command 2 latched, okay to issue command 3
--                dac_ena <= '1';                            --initiate the transaction
--                dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                DAC_sub_addr <= DAC_R24;
--                dac_rw <= '0';                             --command 1 is a write
--                dac_data_wr <= x"41";     
                          
--                WHEN 6 =>                                  --no command latched in yet
--                  dac_ena <= '1';                            --initiate the transaction
--                  dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                  DAC_sub_addr <= DAC_R26;
--                  dac_rw <= '0';                             --command 1 is a write
--                  dac_data_wr <= x"05";              --data to be written
--                WHEN 7 =>                                  --1st busy high: command 1 latched, okay to issue command 2
--                    dac_ena <= '1';                            --initiate the transaction
--                    dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                    DAC_sub_addr <= DAC_R27;
--                    dac_rw <= '0';                             --command 1 is a write
--                    dac_data_wr <= x"11";              --data to be written                            
--                WHEN 8 =>                                  --2nd busy high: command 2 latched, okay to issue command 3
--                    dac_ena <= '1';                            --initiate the transaction
--                    dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                    DAC_sub_addr <= DAC_R29;
--                    dac_rw <= '0';                             --command 1 is a write
--                    dac_data_wr <= x"FF";              --data to be written    
--                WHEN 9 =>                                  --no command latched in yet
--                      dac_ena <= '1';                            --initiate the transaction
--                      dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                      DAC_sub_addr <= DAC_R30;
--                      dac_rw <= '0';                             --command 1 is a write
--                      dac_data_wr <= x"FF";              --data to be written
--                WHEN 10 =>                                  --1st busy high: command 1 latched, okay to issue command 2
--                        dac_ena <= '1';                            --initiate the transaction
--                        dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                        DAC_sub_addr <= DAC_R31;
--                        dac_rw <= '0';                             --command 1 is a write
--                        dac_data_wr <= x"FE";              --data to be written                            
--               WHEN 11 =>                                  --2nd busy high: command 2 latched, okay to issue command 3
--                        dac_ena <= '1';                            --initiate the transaction
--                        dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                        DAC_sub_addr <= DAC_R32;
--                        dac_rw <= '0';                             --command 1 is a write
--                        dac_data_wr <= x"FE";  
--                                    --data to be written    
--                   WHEN 12 =>                                  --no command latched in yet
--                          dac_ena <= '1';                            --initiate the transaction
--                          dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                          DAC_sub_addr <= DAC_R35;
--                          dac_rw <= '0';                             --command 1 is a write
--                          dac_data_wr <= x"03";              --data to be written
--                        WHEN 13 =>                                  --1st busy high: command 1 latched, okay to issue command 2
--                            dac_ena <= '1';                            --initiate the transaction
--                            dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                            DAC_sub_addr <= DAC_R36;
--                            dac_rw <= '0';                             --command 1 is a write
--                            dac_data_wr <= x"03";              --data to be written                            
--                        WHEN 14 =>                                  --2nd busy high: command 2 latched, okay to issue command 3
--                            dac_ena <= '1';                            --initiate the transaction
--                            dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                            DAC_sub_addr <= DAC_R58;
--                            dac_rw <= '0';                             --command 1 is a write
--                            dac_data_wr <= x"01";              --data to be written    
--                        WHEN 15 =>                                  --no command latched in yet
--                              dac_ena <= '1';                            --initiate the transaction
--                              dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                              DAC_sub_addr <= DAC_R65;
--                              dac_rw <= '0';                             --command 1 is a write
--                              dac_data_wr <= x"7F";              --data to be written
--                        WHEN 16 =>                                  --1st busy high: command 1 latched, okay to issue command 2
--                                dac_ena <= '1';                            --initiate the transaction
--                                dac_addr <= DAC_ADDR_I2C;                    --set the address of the slave
--                                DAC_sub_addr <= DAC_R66;
--                                dac_rw <= '0';                             --command 1 is a write
--                                dac_data_wr <= x"01";              --data to be written   
--                                DEBUG(1)<='1';                                                   
--        WHEN OTHERS => dac_ena <= '0'; 
--        end case;
--  end if;



-------------------------DEBUG SIGNALS
--if(DAC_ACK_err ='1')then
--    DEBUG(0)<='1';
--end if;
    
--end if;
--end process;

--end Behavioral;
