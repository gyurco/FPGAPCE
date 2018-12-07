library STD;
use STD.TEXTIO.ALL;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
-- use IEEE.STD_LOGIC_ARITH.ALL;
-- use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_TEXTIO.all;
use IEEE.NUMERIC_STD.ALL;


entity Virtual_Toplevel is
	generic
	(
		colAddrBits : integer := 8;
		rowAddrBits : integer := 12
	);
	port(
		reset : in std_logic;
		CLK : in std_logic;
		SDR_CLK : in std_logic;

		DRAM_ADDR	: out std_logic_vector(rowAddrBits-1 downto 0);
		DRAM_BA_0	: out std_logic;
		DRAM_BA_1	: out std_logic;
		DRAM_CAS_N	: out std_logic;
		DRAM_CKE	: out std_logic;
		DRAM_CS_N	: out std_logic;
		DRAM_DQ		: inout std_logic_vector(15 downto 0);
		DRAM_LDQM	: out std_logic;
		DRAM_RAS_N	: out std_logic;
		DRAM_UDQM	: out std_logic;
		DRAM_WE_N	: out std_logic;
		
		DAC_LDATA : out std_logic_vector(15 downto 0);
		DAC_RDATA : out std_logic_vector(15 downto 0);
		
		R		: out std_logic_vector(2 downto 0);
		G		: out std_logic_vector(2 downto 0);
		B		: out std_logic_vector(2 downto 0);
		VS		: buffer std_logic;
		HS		: buffer std_logic;

		joya : in std_logic_vector(7 downto 0) := (others =>'1');
		joyb : in std_logic_vector(7 downto 0) := (others =>'1');
		joyc : in std_logic_vector(7 downto 0) := (others =>'1');
		joyd : in std_logic_vector(7 downto 0) := (others =>'1');
		joye : in std_logic_vector(7 downto 0) := (others =>'1');

        -- ROM Loader / Host boot data
        ext_reset_n    : in std_logic := '1';
        ext_bootdone   : in std_logic := '0';
        ext_data       : in std_logic_vector(15 downto 0) := (others => '0');
        ext_data_req   : out std_logic;
        ext_data_ack   : in std_logic := '0';

        -- DIP switches
        ext_sw         : in std_logic_vector(15 downto 0)
	);
end entity;

architecture rtl of Virtual_Toplevel is

constant addrwidth : integer := rowAddrBits+colAddrBits+2;

-- signal GPIO_CLKCNT	: std_logic_vector(15 downto 0);
signal GPIO_CLKCNT	: unsigned(15 downto 0);

signal GPIO_SEL		: std_logic;

signal PRE_RESET_N	: std_logic;
signal ROM_RESET_N	: std_logic := '0';
signal RESET_N		: std_logic := '0';

-- CPU signals
signal CPU_NMI_N	: std_logic;
signal CPU_IRQ1_N	: std_logic;
signal CPU_IRQ2_N	: std_logic;
signal CPU_RD_N		: std_logic;
signal CPU_WR_N		: std_logic;
signal CPU_DI		: std_logic_vector(7 downto 0);
signal CPU_DO		: std_logic_vector(7 downto 0);
signal CPU_A		: std_logic_vector(20 downto 0);
signal CPU_HSM		: std_logic;

signal CPU_CLKOUT	: std_logic;
signal CPU_CLKEN	: std_logic;
signal CPU_RDY		: std_logic;

signal CPU_VCE_SEL_N	: std_logic;
signal CPU_VDC_SEL_N	: std_logic;
signal CPU_RAM_SEL_N	: std_logic;

signal CPU_IO_DI		: std_logic_vector(7 downto 0);  -- bit 6: country, bits 3-0: joypad data
signal CPU_IO_DO		: std_logic_vector(7 downto 0);  -- bit 1: clr, bit 0: sel

-- RAM signals
signal RAM_A		: std_logic_vector(12 downto 0);
signal RAM_DI		: std_logic_vector(7 downto 0);
signal RAM_WE		: std_logic;
signal RAM_DO		: std_logic_vector(7 downto 0);

-- ROM signals
signal HEADER		: std_logic;
signal BITFLIP		: std_logic;

signal FL_RST_N_FF	: std_logic := '1';

-- VCE signals
signal VCE_DO		: std_logic_vector(7 downto 0);
signal HSIZE		: std_logic_vector(9 downto 0);
signal HSTART		: std_logic_vector(9 downto 0);

-- VDC signals
signal VDC_DO		: std_logic_vector(7 downto 0);
signal VDC_BUSY_N	: std_logic;
signal VDC_IRQ_N	: std_logic;

-- VDC signals
signal VDC_COLNO	: std_logic_vector(8 downto 0);
signal VDC_CLKEN	: std_logic;

signal VRAM_REQ	: std_logic;
signal VRAM_A	: std_logic_vector(15 downto 0);
signal VRAM_DO	: std_logic_vector(15 downto 0); -- Output from RAM
signal VRAM_DI	: std_logic_vector(15 downto 0);
signal VRAM_WE	: std_logic;
signal VRAM_ACK	: std_logic;

signal SDR_INIT_DONE	: std_logic;

type bootStates is (BOOT_READ_1, BOOT_WRITE_1, BOOT_WRITE_2, BOOT_DONE);
signal bootState : bootStates := BOOT_DONE;

signal romwr_req : std_logic := '0';
signal romwr_ack : std_logic;
signal romwr_a : unsigned(addrwidth downto 1);
signal romwr_d : std_logic_vector(15 downto 0);

signal romrd_req : std_logic := '0';
signal romrd_ack : std_logic;
signal romrd_a : std_logic_vector(addrwidth downto 3);
signal romrd_q : std_logic_vector(63 downto 0);
signal romrd_a_cached : std_logic_vector(addrwidth downto 3);
signal romrd_q_cached : std_logic_vector(63 downto 0);
signal rommap : std_logic_vector(1 downto 0);
signal romhdr : std_logic;
signal romrd_a_adj  : std_logic_vector(addrwidth downto 3);

signal FL_DQ : std_logic_vector(15 downto 0);

type romStates is (ROM_IDLE, ROM_READ);
signal romState : romStates := ROM_IDLE;

signal CPU_A_PREV : std_logic_vector(20 downto 0);
signal ROM_RDY	: std_logic;
signal ROM_DO	: std_logic_vector(7 downto 0);

signal gamepad_port : unsigned(2 downto 0);
signal multitap : std_logic;
signal prev_sel : std_logic;

begin

-- Bit flipping switch
BITFLIP <= ext_sw(2);
multitap <= ext_sw(4);

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- CPU
CPU : entity work.huc6280 port map(
	CLK 	=> CLK,
	RESET_N	=> RESET_N,
	
	NMI_N	=> CPU_NMI_N,
	IRQ1_N	=> CPU_IRQ1_N,
	IRQ2_N	=> CPU_IRQ2_N,

	DI		=> CPU_DI,
	DO 		=> CPU_DO,
	
	HSM		=> CPU_HSM,
	
	A 		=> CPU_A,
	WR_N 	=> CPU_WR_N,
	RD_N	=> CPU_RD_N,
	
	CLKEN	=> CPU_CLKOUT,
	RDY		=> CPU_RDY,
	
	CEK_N	=> CPU_VCE_SEL_N,
	CE7_N	=> CPU_VDC_SEL_N,
	CER_N	=> CPU_RAM_SEL_N,
	
	K		=> CPU_IO_DI,
	O		=> CPU_IO_DO,
	
	AUD_LDATA => DAC_LDATA,
	AUD_RDATA => DAC_RDATA
);

-- RAM
RAM : entity work.ram port map(
	address	=> RAM_A,
	clock	=> CLK,
	data	=> RAM_DI,
	wren	=> RAM_WE,
	q		=> RAM_DO
);

VCE : entity work.huc6260 port map(
	CLK 		=> CLK,
	RESET_N		=> RESET_N,  -- Allow 6260 to emit sync signals even during reset.

	-- CPU Interface
	A			=> CPU_A(2 downto 0),
	CE_N		=> CPU_VCE_SEL_N,
	WR_N		=> CPU_WR_N,
	RD_N		=> CPU_RD_N,
	DI			=> CPU_DO,
	DO 			=> VCE_DO,

	RVBL		=> '0',
	HSIZE		=> HSIZE,
	HSTART		=> HSTART,

	-- VDC Interface
	COLNO		=> VDC_COLNO,
	CLKEN		=> VDC_CLKEN,

	-- NTSC/RGB Video Output
	R			=> R,
	G			=> G,
	B			=> B,
	VS_N		=> VS,
	HS_N		=> HS
);


VDC : entity work.huc6270 port map(
	CLK 		=> CLK,
	RESET_N		=> RESET_N,
	HSIZE		=> HSIZE,
	HSTART		=> HSTART,
	SP64		=> '0',

	-- CPU Interface
	A			=> CPU_A(1 downto 0),
	CE_N		=> CPU_VDC_SEL_N,
	WR_N		=> CPU_WR_N,
	RD_N		=> CPU_RD_N,
	DI			=> CPU_DO,
	DO 			=> VDC_DO,
	BUSY_N		=> VDC_BUSY_N,
	IRQ_N		=> VDC_IRQ_N,

	vram_req	=> VRAM_REQ,
	vram_a		=> VRAM_A,
	vram_q		=> VRAM_DO,
	vram_d		=> VRAM_DI,
	vram_we		=> VRAM_WE,
	vram_ack	=> VRAM_ACK,

	-- VCE Interface
	COLNO		=> VDC_COLNO,
	CLKEN		=> VDC_CLKEN,
	HS_N		=> HS,
	VS_N		=> VS

);

romrd_a_adj <= romrd_a when romhdr = '0' else std_logic_vector(unsigned(romrd_a) + 64);

	sdr : entity work.chameleon_sdram
		generic map (
			casLatency => 2,
--			casLatency => 3,
			colAddrBits => colAddrBits,
			rowAddrBits => rowAddrBits,
--			t_ck_ns => 10.0
--			t_ck_ns => 6.7
			t_ck_ns => 11.7
--			t_ck_ns => 23.5
--			t_ck_ns => 8.3	
		)
		port map (
			clk => SDR_CLK,

			reserve => '0',

			sd_data => DRAM_DQ,
			sd_addr => DRAM_ADDR,
			sd_we_n => DRAM_WE_N,
			sd_ras_n => DRAM_RAS_N,
			sd_cas_n => DRAM_CAS_N,
			sd_ba_0 => DRAM_BA_0,
			sd_ba_1 => DRAM_BA_1,
			sd_ldqm => DRAM_LDQM,
			sd_udqm => DRAM_UDQM,

			vram_req => VRAM_REQ,
			vram_ack => VRAM_ACK,
			vram_we => VRAM_WE,
			vram_a => "100000" & VRAM_A,
			vram_d => VRAM_DI,
			vram_q => VRAM_DO,

			romwr_req => romwr_req,
			romwr_ack => romwr_ack,
			romwr_we => '1',
			romwr_a => std_logic_vector(romwr_a),
			romwr_d => romwr_d,

			romrd_req => romrd_req,
			romrd_ack => romrd_ack,
			romrd_a => romrd_a_adj,
			romrd_q => romrd_q,

			initDone => SDR_INIT_DONE,

			debugIdle => open,
			debugRefresh => open
		);

DRAM_CKE <= '1';
DRAM_CS_N <= '0';

-- Interrupt signals
CPU_NMI_N <= '1';
CPU_IRQ1_N <= VDC_IRQ_N;
CPU_IRQ2_N <= '1';
CPU_RDY <= VDC_BUSY_N and ROM_RDY;

-- CPU data bus
CPU_DI <= RAM_DO when CPU_RD_N = '0' and CPU_RAM_SEL_N = '0' 
	else ROM_DO when CPU_RD_N = '0' and CPU_A(20) = '0'
	else VCE_DO when CPU_RD_N = '0' and CPU_VCE_SEL_N = '0'
	else VDC_DO when CPU_RD_N = '0' and CPU_VDC_SEL_N = '0'
	else x"FF";

process( CLK )
begin
	if rising_edge( CLK ) then
		if ROM_RESET_N = '0' then
			RESET_N <= '0';
			romrd_req <= '0';
			romrd_a_cached <= (others => '1');
			romrd_q_cached <= (others => '0');
			ROM_RDY <= '1';
			CPU_A_PREV <= (others => '1');
			romState <= ROM_IDLE;
		else
			RESET_N <= '1';
			case romState is
			when ROM_IDLE =>
				--if CPU_CLKOUT = '1' then
					if CPU_RD_N = '0' or CPU_WR_N = '0' then
						CPU_A_PREV <= CPU_A;
					else 
						CPU_A_PREV <= (others => '1');
					end if;
					if CPU_A(20) = '0' and CPU_RD_N = '0' and CPU_A /= CPU_A_PREV then
						if CPU_A(19 downto 3) = romrd_a_cached(19 downto 3) then
							case CPU_A(2 downto 0) is
								when "000" =>
									ROM_DO <= romrd_q_cached(7 downto 0);
								when "001" =>
									ROM_DO <= romrd_q_cached(15 downto 8);
								when "010" =>
									ROM_DO <= romrd_q_cached(23 downto 16);
								when "011" =>
									ROM_DO <= romrd_q_cached(31 downto 24);
								when "100" =>
									ROM_DO <= romrd_q_cached(39 downto 32);
								when "101" =>
									ROM_DO <= romrd_q_cached(47 downto 40);
								when "110" =>
									ROM_DO <= romrd_q_cached(55 downto 48);
								when "111" =>
									ROM_DO <= romrd_q_cached(63 downto 56);
								when others => null;
							end case;						
						else
							romrd_req <= not romrd_req;
							romrd_a<=(others=>'0');
							romrd_a(19 downto 3) <= CPU_A(19 downto 3);
							-- Perform address mangling to mimic HuCard chip mapping.
							-- rommap => 00  -  Straight mapping
							-- rommap => 01  -  384K ROM, split in 3, mapped ABABCCCC
							-- rommap => 10  -  768K ROM, straight mapping for now
							-- rommap => 11  -  not yet defined.
							
							if rommap="01" then                       -- bits 19 downto 16
								-- 00000 -> 20000  => 00000 -> 20000		0000 -> 0000
								-- 20000 -> 40000  => 20000 -> 40000		0010 -> 0010
								-- 40000 -> 60000  => 00000 -> 20000		0100 -> 0000
								-- 60000 -> 80000  => 20000 -> 40000		0110 -> 0010
								-- 80000 -> A0000  => 40000 -> 60000		1000 -> 0100
								-- A0000 -> C0000  => 40000 -> 60000		1010 -> 0100
								-- C0000 -> E0000  => 40000 -> 60000		1100 -> 0100
								-- E0000 ->100000  => 40000 -> 60000		1110 -> 0100
								romrd_a(19)<='0';
								romrd_a(18)<=CPU_A(19);
								romrd_a(17)<=CPU_A(17) and not CPU_A(19);
							end if;
								

							romrd_a_cached<=(others=>'0');
							romrd_a_cached(19 downto 3) <= CPU_A(19 downto 3);
							ROM_RDY <= '0';
							romState <= ROM_READ;
						end if;
					end if;
				--end if;
			when ROM_READ =>
				if romrd_req = romrd_ack then
					ROM_RDY <= '1';
					romrd_q_cached <= romrd_q;
					case CPU_A(2 downto 0) is
						when "000" =>
							ROM_DO <= romrd_q(7 downto 0);
						when "001" =>
							ROM_DO <= romrd_q(15 downto 8);
						when "010" =>
							ROM_DO <= romrd_q(23 downto 16);
						when "011" =>
							ROM_DO <= romrd_q(31 downto 24);
						when "100" =>
							ROM_DO <= romrd_q(39 downto 32);
						when "101" =>
							ROM_DO <= romrd_q(47 downto 40);
						when "110" =>
							ROM_DO <= romrd_q(55 downto 48);
						when "111" =>
							ROM_DO <= romrd_q(63 downto 56);
						when others => null;
					end case;
					romState <= ROM_IDLE;
				end if;
			when others => null;
			end case;
		end if;
	end if;
end process;


-- Boot process

FL_DQ<=ext_data;

-- Reset
PRE_RESET_N <= ext_reset_n;
ROM_RESET_N <= ext_bootdone and ext_reset_n and reset and SDR_INIT_DONE;

process( SDR_CLK )
begin
	if rising_edge( SDR_CLK ) then
		if PRE_RESET_N = '0' then
				
			ext_data_req <='0';
			rommap <= "00";
			romhdr <= '0';
			
			romwr_req <= '0';
			romwr_a <= to_unsigned(0, addrwidth);
			bootState<=BOOT_READ_1;
			
		else
			case bootState is 
				when BOOT_READ_1 =>
					ext_data_req<='1';
					if ext_data_ack='1' then
						ext_data_req<='0';
						bootState <= BOOT_WRITE_1;
					end if;
					if ext_bootdone='1' then
						case romwr_a(19 downto 16) is
						when x"6" =>
							rommap <= "01"; -- 384K ROM
						when x"c" =>
							rommap <= "10"; -- 768K ROM
						when others =>
							rommap <= "00";
						end case;
						if romwr_a(9) = '1' then
							romhdr <= '1';
						end if;
						ext_data_req<='0';
						bootState <= BOOT_DONE;
					end if;
				when BOOT_WRITE_1 =>
					if BITFLIP = '1' then
						romwr_d <=
							FL_DQ(8)
							& FL_DQ(9)
							& FL_DQ(10)
							& FL_DQ(11)
							& FL_DQ(12)
							& FL_DQ(13)
							& FL_DQ(14)
							& FL_DQ(15)
							& FL_DQ(0)
							& FL_DQ(1)
							& FL_DQ(2)
							& FL_DQ(3)
							& FL_DQ(4)
							& FL_DQ(5)
							& FL_DQ(6)
							& FL_DQ(7);
					else
						romwr_d <= FL_DQ;
					end if;
					
					romwr_req <= not romwr_req;
					bootState <= BOOT_WRITE_2;
				when BOOT_WRITE_2 =>
					if romwr_req = romwr_ack then
						romwr_a <= romwr_a + 1;
						bootState <= BOOT_READ_1;
					end if;
				when others => null;
			end case;	
		end if;
	end if;
end process;



-- Block RAM
RAM_A <= CPU_A(12 downto 0);
RAM_DI <= CPU_DO;
process( CLK )
begin
	if rising_edge( CLK ) then
		RAM_WE <= '0';
		if CPU_CLKOUT = '1' and CPU_RAM_SEL_N = '0' and CPU_WR_N = '0' then
			RAM_WE <= '1';
		end if;
	end if;
end process;


-- I/O Port
CPU_IO_DI(7 downto 4) <= "1011"; -- No CD-Rom unit, TGFX-16
CPU_IO_DI(3 downto 0) <=
	joya(7) & joya(6) & joya(4) & joya(5)
		when CPU_IO_DO(1 downto 0) = "00" and gamepad_port = "000"
	else joya(2) & joya(1) & joya(3) & joya(0)
		when CPU_IO_DO(1 downto 0) = "01" and gamepad_port = "000"
		
	else joyb(7) & joyb(6) & joyb(4) & joyb(5)
		when CPU_IO_DO(1 downto 0) = "00" and gamepad_port = "001"
	else joyb(2) & joyb(1) & joyb(3) & joyb(0)
		when CPU_IO_DO(1 downto 0) = "01" and gamepad_port = "001"
		
	else joyc(7) & joyc(6) & joyc(4) & joyc(5)
		when CPU_IO_DO(1 downto 0) = "00" and gamepad_port = "010"
	else joyc(2) & joyc(1) & joyc(3) & joyc(0)
		when CPU_IO_DO(1 downto 0) = "01" and gamepad_port = "010"
		
	else joyd(7) & joyd(6) & joyd(4) & joyd(5)
		when CPU_IO_DO(1 downto 0) = "00" and gamepad_port = "011"
	else joyd(2) & joyd(1) & joyd(3) & joyd(0)
		when CPU_IO_DO(1 downto 0) = "01" and gamepad_port = "011"

	else joye(7) & joye(6) & joye(4) & joye(5)
		when CPU_IO_DO(1 downto 0) = "00" and gamepad_port = "100"
	else joye(2) & joye(1) & joye(3) & joye(0)
		when CPU_IO_DO(1 downto 0) = "01" and gamepad_port = "100"
		
	else "1111";

process(clk)
begin
	if rising_edge(clk) then
		if CPU_IO_DO(1)='1' then -- reset pad
			gamepad_port<=(others => '0');
		elsif prev_sel='0' and CPU_IO_DO(0)='1' and multitap='1' then -- Rising edge of select bit
			gamepad_port<=gamepad_port+1;
		end if;
		prev_sel<=CPU_IO_DO(0);
	end if;

end process;

end rtl;
