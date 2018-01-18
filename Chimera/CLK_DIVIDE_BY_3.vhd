LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE  IEEE.NUMERIC_STD.ALL;

ENTITY clk_divide_by_3 IS
PORT (
 clk      : IN  STD_LOGIC                              ;
 reset_n  : IN  STD_LOGIC                              ;
 mclk: OUT STD_LOGIC                      
 );
END clk_divide_by_3;

ARCHITECTURE Arch OF clk_divide_by_3 IS
SIGNAL COUNTER : UNSIGNED(1 DOWNTO 0);
SIGNAL div_1   : STD_LOGIC;
SIGNAL div_2   : STD_LOGIC;
SIGNAL clk_low_cnt   : STD_LOGIC;
SIGNAL clk_high_cnt   : STD_LOGIC;

BEGIN

-- Counter generation
PROCESS(clk,reset_n)
  BEGIN
    IF (reset_n = '0') THEN
      COUNTER <= "11";
    ELSIF RISING_EDGE(clk) THEN
      IF COUNTER = "10" THEN
        COUNTER <= "00";
      ELSE
        COUNTER <=  COUNTER + 1;
      END IF;
    END IF;
END PROCESS;

-- clk_r generation
PROCESS(clk,reset_n)
  BEGIN
    IF (reset_n = '0') THEN
      clk_low_cnt  <= '0';
      clk_high_cnt <= '0';
    ELSIF RISING_EDGE(clk) THEN
      IF COUNTER = "00" THEN
        clk_low_cnt <= '1';
      ELSE
        clk_low_cnt <= '0';
      END IF;
      IF COUNTER = "10" THEN
        clk_high_cnt <= '1';
      ELSE
        clk_high_cnt <= '0';
      END IF;
    END IF;
END PROCESS;

-- div_1 generation
PROCESS(clk,reset_n)
  BEGIN
    IF (reset_n = '0') THEN
      div_1 <= '0';
    ELSIF RISING_EDGE(clk) THEN
      IF clk_low_cnt = '1' THEN
        div_1 <= NOT div_1;
      END IF;
    END IF;
  END PROCESS;

-- clk_f generation
PROCESS(clk,reset_n)
  BEGIN
    IF (reset_n = '0') THEN
      div_2 <= '0';
    ELSIF FALLING_EDGE(clk) THEN
      IF clk_high_cnt = '1' THEN
        div_2 <= NOT div_2;
      END IF;
    END IF;
  END PROCESS;

mclk <= div_1 XOR div_2;
   

END Arch;



