----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 17.08.2015 18:27:40
-- Design Name: 
-- Module Name: FILTER_BLOCK - Behavioral
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
use IEEE.STD_LOGIC_ARITH.ALL;
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

entity FILTER_BLOCK is
    Port ( clk : in STD_LOGIC;
           clkS : in std_logic;
           S : in STD_LOGIC_VECTOR (15 downto 0);
           SWS : in std_logic_vector(3 downto 0);
           SW : in STD_LOGIC;
           S_out : out STD_LOGIC_VECTOR (15 downto 0));
end FILTER_BLOCK;

architecture Behavioral of FILTER_BLOCK is

signal newtick : std_logic;
signal store_last : std_logic;
signal Sout_buff : std_logic_vector(17 downto 0);
--signal S_BUFF1: std_logic_vector(15 downto 0);
--signal S_BUFF2: std_logic_vector(15 downto 0);

--component BIQUAD IS
--  PORT( clk : in STD_LOGIC;
--        clkS : in std_logic;
--           a0 : in STD_LOGIC_VECTOR (15 downto 0);  --coefficients are 1-65000 full scale
--           a1 : in STD_LOGIC_VECTOR (15 downto 0);
--           a2 : in STD_LOGIC_VECTOR (15 downto 0);
--           b1 : in STD_LOGIC_VECTOR (15 downto 0);
--           b2 : in STD_LOGIC_VECTOR (15 downto 0);
--           SWS : in std_logic_vector(3 downto 0);
--           S : in STD_LOGIC_VECTOR (15 downto 0); --audio in
--           S_out : out STD_LOGIC_VECTOR (15 downto 0)); --audio out
        
--END component;
component COEFF_PICKER
    Port ( clk : in STD_LOGIC;
           clkS : in STD_LOGIC;
           a0 : in STD_LOGIC_VECTOR (31 downto 0);
           a1 : in STD_LOGIC_VECTOR (31 downto 0);
           a2 : in STD_LOGIC_VECTOR (31 downto 0);
           b1 : in STD_LOGIC_VECTOR (31 downto 0);
           b2 : in STD_LOGIC_VECTOR (31 downto 0);
           f : in STD_LOGIC_VECTOR (11 downto 0);
           Q : in STD_LOGIC_VECTOR (7 downto 0));
end Component;

component IIR_Biquad is
		Port ( 
				clk : in  STD_LOGIC;
				n_reset : in  STD_LOGIC;
				sample_trig : in  STD_LOGIC;
				X_in : in  STD_LOGIC_VECTOR (17 downto 0);
				filter_done : out STD_LOGIC;
				Y_out : out  STD_LOGIC_VECTOR (17 downto 0)
				);
end component;

--component BIQUAD is
--    Port ( clk : in STD_LOGIC;
--           a0 : in STD_LOGIC_VECTOR (15 downto 0);  --coefficients are 1-65000 full scale
--           a1 : in STD_LOGIC_VECTOR (15 downto 0);
--           a2 : in STD_LOGIC_VECTOR (15 downto 0);
--           b1 : in STD_LOGIC_VECTOR (15 downto 0);
--           b2 : in STD_LOGIC_VECTOR (15 downto 0);
--           S : in STD_LOGIC_VECTOR (15 downto 0); --audio in
--           S_out : out STD_LOGIC_VECTOR (15 downto 0)); --audio out
--end component;

--component bqmain is
--    port ( clk_i : in std_logic;
--            rst_i : in std_logic;
--            we_i : in std_logic;
--            stb_i : in std_logic;
--            ack_o : out std_logic;
--            dat_i : in std_logic_vector(15 downto 0);
--            dat_o : out std_logic_vector(15 downto 0);
--            adr_i : in std_logic_vector(2 downto 0);
--            dspclk : in std_logic;
--            nreset : in std_logic;
--            x : in std_logic_vector(15 downto 0);
--            valid : out std_logic;
--            y : out std_logic_vector(15 downto 0)
--            );
            
--end component;


--signal curr_addr : std_logic_vector(2 downto 0);
--signal coeff_write : std_logic_vector(15 downto 0);
--signal S_out_buff : std_logic_vector(15 downto 0);


begin

--BQ: BIQUAD 
--  PORT MAP( clk => clk,
--            clkS => clkS,

--        S => S,
--        a0 => CONV_STD_LOGIC_VECTOR(12823,16),
--        a1 => CONV_STD_LOGIC_VECTOR(-25647,16),
--        a2 => CONV_STD_LOGIC_VECTOR(12823,16),
--        b1 => CONV_STD_LOGIC_VECTOR(-24397,16),
--        b2 => CONV_STD_LOGIC_VECTOR(10512,16),
--        SWS => SWS,
--        S_out => S_out
--        );

BQ: IIR_Biquad 
		Port MAP ( 
				clk => clk,
				n_reset => '1',
				sample_trig => newtick,
				X_in => SXT(S,18),

				Y_out => Sout_buff
				);


process(clk) begin
if(clk'event and clk='1')then
    if(store_last ='0' and ClkS ='1')then
        newtick<='1';
    else
        newtick<='0';
    end if;
    
    store_last<=ClkS;
    
S_out <= Sout_buff(17 downto 2);  

end if;
end process;

     
--BIQUAD: bqmain
--PORT MAP(clk_i => clk,
--        rst_i => '0',
--        we_i => '1',
--        stb_i => '1',
--        dat_i => coeff_write,
--        adr_i => curr_addr,
--        dspclk => clk,
--        nreset => '1',
--        x => S,
--        y => S_out_buff
--        );
        
--FILT: BIQUAD
--PORT MAP(clk => clk,
--        a0 => CONV_STD_LOGIC_VECTOR(66,16),
--        a1 =>  CONV_STD_LOGIC_VECTOR(32,16),
--        a2 =>  CONV_STD_LOGIC_VECTOR(66,16),
--        b1 =>  CONV_STD_LOGIC_VECTOR(-30497,16),
--        b2 =>  CONV_STD_LOGIC_VECTOR(14376,16),
--        S => S,
--        S_out => S_out
--        );



--process(clk) begin
--if(clk'event and clk='1')then
--    if(curr_addr < "100")then
--        curr_addr<=(others => '0');
--    else
--        curr_addr <= curr_addr+1;
--    end if;
    
--    case curr_addr is
--        when "000" =>
--            coeff_write <= "1000010010111000";
--            when "001" =>
--                coeff_write <= "0011101101110011";
--                when "010" =>
--                    coeff_write <= "0000000000001010";
--                    when "011" =>
--                        coeff_write <=  "0000000000010101";
--                         when "100" =>
--                            coeff_write <=  "0000000000001010";
--      when others => --
--      end case;                                         
--     S_out <= S_out_buff;
--end if;
--end process;


end Behavioral;
