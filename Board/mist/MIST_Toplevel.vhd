library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;
use work.mist.ALL;
 
entity MIST_Toplevel is
	port
	(
		CLOCK_27	: in std_logic_vector(1 downto 0);
		
		LED			: out std_logic;

		UART_TX		: out STD_LOGIC;
		UART_RX		: in STD_LOGIC;

		SDRAM_DQ	: inout std_logic_vector(15 downto 0);
		SDRAM_A	    : out std_logic_vector(12 downto 0);
		SDRAM_DQMH	: out STD_LOGIC;
		SDRAM_DQML	: out STD_LOGIC;
		SDRAM_nWE	: out STD_LOGIC;
		SDRAM_nCAS	: out STD_LOGIC;
		SDRAM_nRAS	: out STD_LOGIC;
		SDRAM_nCS	: out STD_LOGIC;
		SDRAM_BA	: out std_logic_vector(1 downto 0);
		SDRAM_CLK	: out STD_LOGIC;
		SDRAM_CKE	: out STD_LOGIC;

		SPI_DO	    : inout std_logic;
		SPI_DI	    : in std_logic;
		SPI_SCK		: in STD_LOGIC;
		SPI_SS2		: in STD_LOGIC; -- FPGA
		SPI_SS3		: in STD_LOGIC; -- OSD
		SPI_SS4		: in STD_LOGIC; -- "sniff" mode
		CONF_DATA0  : in std_logic; -- SPI_SS for user_io

		VGA_HS		: out STD_LOGIC;
		VGA_VS		: out STD_LOGIC;
		VGA_R		: out std_logic_vector(5 downto 0);
		VGA_G		: out std_logic_vector(5 downto 0);
		VGA_B		: out std_logic_vector(5 downto 0);

		AUDIO_L : out std_logic;
		AUDIO_R : out std_logic
	);
END entity;

architecture rtl of MIST_Toplevel is

signal reset        : std_logic;
signal reset_d      : std_logic;
signal pll_locked   : std_logic;
signal clk42m       : std_logic;
signal memclk       : std_logic;

signal audiol : std_logic_vector(23 downto 0);
signal audior : std_logic_vector(23 downto 0);
--
signal pce_red : std_logic_vector(2 downto 0);
signal pce_green : std_logic_vector(2 downto 0);
signal pce_blue : std_logic_vector(2 downto 0);
signal pce_vs : std_logic;
signal pce_hs : std_logic;

-- user_io
signal buttons: std_logic_vector(1 downto 0);
signal status:  std_logic_vector(31 downto 0);
signal joy_0: std_logic_vector(31 downto 0);
signal joy_1: std_logic_vector(31 downto 0);
signal joy_2: std_logic_vector(31 downto 0);
signal joy_3: std_logic_vector(31 downto 0);
signal joy_4: std_logic_vector(31 downto 0);
signal joyn_0: std_logic_vector(11 downto 0);
signal joyn_a: std_logic_vector(11 downto 0);
signal joyn_1: std_logic_vector(11 downto 0);
signal joyn_b: std_logic_vector(11 downto 0);
signal joyn_2: std_logic_vector(11 downto 0);
signal joyn_3: std_logic_vector(11 downto 0);
signal joyn_4: std_logic_vector(11 downto 0);
signal joy_ana_0: std_logic_vector(15 downto 0);
signal joy_ana_1: std_logic_vector(15 downto 0);

signal ypbpr : std_logic;
signal scandoubler_disable : std_logic;

-- data_io
signal downloading      : std_logic;
signal data_io_wr       : std_logic;
signal data_io_clkref   : std_logic;
signal data_io_d        : std_logic_vector(7 downto 0);
signal data_io_index	: std_logic_vector(7 downto 0);
signal downloadingD     : std_logic;
signal d_state          : std_logic_vector(1 downto 0);

-- external controller signals
signal ext_reset_n      : std_logic_vector(2 downto 0) := "111";
signal ext_bootdone     : std_logic_vector(2 downto 0) := "000";
signal ext_data         : std_logic_vector(15 downto 0);
signal ext_data_req     : std_logic;
signal ext_data_ack     : std_logic := '0';
signal ext_sw           : std_logic_vector( 15 downto 0); --DIP switches

constant CONF_STR : string :=
    "TGFX16;BINPCE;"&
    "F,SGX,Load;"&
    "OBC,Scanlines,Off,25%,50%,75%;"&
    "O6,Joystick swap,Off,On;"&
    "O3,6 Buttons,Disable,Enable;"&
    "O4,Multitap,Off,On;"&
    "O2,ROM data swap,Off,On;"&
    "T0,Reset;";

function to_slv(s: string) return std_logic_vector is
    constant ss: string(1 to s'length) := s;
    variable rval: std_logic_vector(1 to 8 * s'length);
    variable p: integer;
    variable c: integer;
  
  begin  
    for i in ss'range loop
      p := 8 * i;
      c := character'pos(ss(i));
      rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8));
    end loop;
    return rval;

end function;

-- Sigma Delta audio
COMPONENT hybrid_pwm_sd
	PORT
	(
		clk		:	 IN STD_LOGIC;
		n_reset		:	 IN STD_LOGIC;
		din		:	 IN STD_LOGIC_VECTOR(15 DOWNTO 0);
		dout		:	 OUT STD_LOGIC
	);
END COMPONENT;

component data_io
    port (  clk         : in std_logic;
            clkref      : in std_logic;
            wr          : out std_logic;
            a           : out std_logic_vector(24 downto 0);
            d           : out std_logic_vector(7 downto 0);
            downloading : out std_logic;
            index       : out std_logic_vector(7 downto 0);

            sck         : in std_logic;
            ss          : in std_logic;
            sdi         : in std_logic
        );
    end component data_io;

begin

  U00 : entity work.pll
    port map(
        inclk0 => CLOCK_27(0),    -- 27 MHz external
        c0     => SDRAM_CLK,      -- 126Mhz external
        c1     => memclk,         -- 126Mhz internal
        c2     => clk42m,         -- 42.8 internal
        locked => pll_locked
    );

ext_sw(0) <= '1'; -- 15kHz output
ext_sw(2) <= status(2); -- rom data swap
ext_sw(3) <= status(3); -- 6 buttons
ext_sw(4) <= status(4); -- multitap
ext_sw(5) <= data_io_index(1); -- SGX mode

-- reset from IO controller
-- status bit 0 is always triggered by the i ocontroller on its own reset
-- button 1 is the core specfic button in the mists front
process(clk42m)
begin
    if rising_edge(clk42m) then
        reset_d <= not (status(0) or buttons(1)) and pll_locked;
        reset <= reset_d;
    end if;
end process;

virtualtoplevel : entity work.Virtual_Toplevel
	generic map
	(
		colAddrBits => 9,
		rowAddrBits => 13
	)
	port map(
        reset => reset,
        CLK => clk42m,
        SDR_CLK => memclk,

        -- SDRAM ports
        DRAM_CKE => SDRAM_CKE,
        DRAM_CS_N => SDRAM_nCS,
        DRAM_RAS_N => SDRAM_nRAS,
        DRAM_CAS_N => SDRAM_nCAS,
        DRAM_WE_N => SDRAM_nWE,
        DRAM_UDQM => SDRAM_DQMH,
        DRAM_LDQM => SDRAM_DQML,
        DRAM_BA_1 => SDRAM_BA(1),
        DRAM_BA_0 => SDRAM_BA(0),
        DRAM_ADDR => SDRAM_A,
        DRAM_DQ => SDRAM_DQ,

        -- Joystick ports (Port_A, Port_B)
        joya => joyn_0,
        joyb => joyn_1,
        joyc => joyn_2,
        joyd => joyn_3,
        joye => joyn_4,

        -- Video, Audio/CMT ports
        R => pce_red,
        G => pce_green,
        B => pce_blue,

        HS => pce_hs,
        VS => pce_vs,

        DAC_LDATA => audiol,
        DAC_RDATA => audior,

        ext_reset_n  => ext_reset_n(2) and ext_reset_n(1) and ext_reset_n(0),
        ext_bootdone => ext_bootdone(2) or ext_bootdone(1) or ext_bootdone(0),
        ext_data     => ext_data,
        ext_data_req => ext_data_req,
        ext_data_ack => ext_data_ack,
        ext_sw => ext_sw
);

mist_video : work.mist.mist_video
    generic map (
        OSD_COLOR   => "001", -- blue
        COLOR_DEPTH => 3,
        SD_HCNT_WIDTH => 10
    )
    port map (
        clk_sys     => clk42m,
        scanlines   => status(12 downto 11),
        scandoubler_disable => scandoubler_disable,
        ypbpr       => ypbpr,
        rotate      => "00",

        SPI_SCK     => SPI_SCK,
        SPI_SS3     => SPI_SS3,
        SPI_DI      => SPI_DI,

        HSync       => pce_hs,
        VSync       => pce_vs,
        R           => pce_red,
        G           => pce_green,
        B           => pce_blue,

        VGA_HS      => VGA_HS,
        VGA_VS      => VGA_VS,
        VGA_R       => VGA_R,
        VGA_G       => VGA_G,
        VGA_B       => VGA_B
    );

LED <= not downloading;

user_io_d : user_io
    generic map (STRLEN => CONF_STR'length)
    port map (
        clk_sys => clk42m,
        clk_sd  => clk42m,
        SPI_CLK => SPI_SCK,
        SPI_SS_IO => CONF_DATA0,
        SPI_MISO => SPI_DO,
        SPI_MOSI => SPI_DI,
        conf_str => to_slv(CONF_STR),
        status => status,
        ypbpr => ypbpr,
        scandoubler_disable => scandoubler_disable,

        joystick_0 => joy_0,
        joystick_1 => joy_1,
        joystick_2 => joy_2,
        joystick_3 => joy_3,
        joystick_4 => joy_4,
        joystick_analog_0 => joy_ana_0,
        joystick_analog_1 => joy_ana_1,
--      switches => switches,
        BUTTONS => buttons
 );

data_io_inst: data_io
    port map (
        clk     => memclk,
        clkref  => data_io_clkref,
        wr      => data_io_wr,
        a       => open,
        d       => data_io_d,
        downloading => downloading,
        index   => data_io_index,

        sck     => SPI_SCK,
        ss      => SPI_SS2,
        sdi     => SPI_DI
    );

process(memclk)
begin
    if rising_edge( memclk ) then
        downloadingD <= downloading;
        ext_reset_n <= ext_reset_n(1 downto 0)&'1'; --stretch reset
        ext_bootdone <= ext_bootdone(1 downto 0)&'0';
        ext_data_ack <= '0';
        if (downloadingD = '0' and downloading = '1') then
            -- ROM downloading start
            ext_reset_n(0) <= '0';
            d_state <= "00";
            data_io_clkref <= '1';
        elsif (downloading = '0') then
            -- ROM downloading finished
            ext_bootdone(0) <= '1';
            data_io_clkref <= '0';
        elsif (downloading = '1') then
            -- ROM downloading in progress
            case d_state is
            when "00" =>
                if data_io_wr = '1' then
                    ext_data(7 downto 0) <= data_io_d;
                    data_io_clkref <= '1';
                    d_state <= "01";
                end if;
            when "01" =>
                if data_io_wr = '1' then
                    ext_data(15 downto 8) <= data_io_d;
                    data_io_clkref <= '0';
                    d_state <= "10";
                end if;
            when "10" =>
                if ext_data_req = '1' then
                    ext_data_ack <= '1';
                    d_state <= "11";
                end if;
            when "11" =>
                data_io_clkref <= '1';
                d_state <= "00";
            end case;
        end if;
    end if;
end process;

-- swap, invert and remap joystick bits
 joyn_a <= not joy_0(11 downto 4) & not joy_0(1) & not joy_0(2) & not joy_0(0) & not joy_0(3);
 joyn_b <= not joy_1(11 downto 4) & not joy_1(1) & not joy_1(2) & not joy_1(0) & not joy_1(3);
 joyn_0 <= joyn_a when status(6) = '0' else joyn_b;
 joyn_1 <= joyn_b when status(6) = '0' else joyn_a;
 joyn_2 <= not joy_2(11 downto 4) & not joy_2(1) & not joy_2(2) & not joy_2(0) & not joy_2(3);
 joyn_3 <= not joy_3(11 downto 4) & not joy_3(1) & not joy_3(2) & not joy_3(0) & not joy_3(3);
 joyn_4 <= not joy_4(11 downto 4) & not joy_4(1) & not joy_4(2) & not joy_4(0) & not joy_4(3);

-- Do we have audio?  If so, instantiate a two DAC channels.
leftsd: component hybrid_pwm_sd
	port map
	(
		clk => memclk,
		n_reset => reset,
		din => not audiol(23) & std_logic_vector(audiol(22 downto 8)),
		dout => AUDIO_L
	);

rightsd: component hybrid_pwm_sd
	port map
	(
		clk => memclk,
		n_reset => reset,
		din => not audior(23) & std_logic_vector(audior(22 downto 8)),
		dout => AUDIO_R
	);

end architecture;
