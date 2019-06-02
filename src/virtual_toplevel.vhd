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
		casLatency : integer := 3;
		rasCasTiming : integer := 3;
		prechargeTiming : integer := 3;
		colAddrBits : integer := 8;
		rowAddrBits : integer := 12;
--		t_ck_ns : real := 10.0  -- 100 MHz
--		t_ck_ns : real := 6.7   -- 150 MHz
--		t_ck_ns : real := 11.7  --  85 MHz
--		t_ck_ns : real := 23.5
		t_ck_ns : real := 7.9   -- 126 MHz
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
		
		DAC_LDATA : out std_logic_vector(23 downto 0);
		DAC_RDATA : out std_logic_vector(23 downto 0);
		
		R		: out std_logic_vector(2 downto 0);
		G		: out std_logic_vector(2 downto 0);
		B		: out std_logic_vector(2 downto 0);
		VS		: buffer std_logic;
		HS		: buffer std_logic;

		joya : in std_logic_vector(11 downto 0) := (others =>'1');
		joyb : in std_logic_vector(11 downto 0) := (others =>'1');
		joyc : in std_logic_vector(11 downto 0) := (others =>'1');
		joyd : in std_logic_vector(11 downto 0) := (others =>'1');
		joye : in std_logic_vector(11 downto 0) := (others =>'1');

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
signal CPU_VPC_SEL_N	: std_logic;
signal CPU_VDC_SEL_N	: std_logic;
signal CPU_VDC0_SEL_N	: std_logic;
signal CPU_VDC1_SEL_N	: std_logic;
signal CPU_RAM_SEL_N	: std_logic;

signal CPU_IO_DI		: std_logic_vector(7 downto 0);  -- bit 6: country, bits 3-0: joypad data
signal CPU_IO_DO		: std_logic_vector(7 downto 0);  -- bit 1: clr, bit 0: sel

-- RAM signals
signal RAM_A		: std_logic_vector(14 downto 0);
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
signal VDC_BUSY_N	: std_logic;
signal VDC_IRQ_N	: std_logic;
signal VDC_COLNO	: std_logic_vector(8 downto 0);
signal VDC0_DO		: std_logic_vector(7 downto 0);
signal VDC0_BUSY_N	: std_logic;
signal VDC0_IRQ_N	: std_logic;
signal VDC0_COLNO	: std_logic_vector(8 downto 0);
signal VDC1_DO		: std_logic_vector(7 downto 0);
signal VDC1_BUSY_N	: std_logic;
signal VDC1_IRQ_N	: std_logic;
signal VDC1_COLNO	: std_logic_vector(8 downto 0);
signal VDCNUM		: std_logic;
signal VDC_CLKEN	: std_logic;

signal VPC_DO		: std_logic_vector(7 downto 0);

signal VRAM0_REQ	: std_logic;
signal VRAM0_A	: std_logic_vector(16 downto 1);
signal VRAM0_DO	: std_logic_vector(15 downto 0); -- Output from RAM
signal VRAM0_DI	: std_logic_vector(15 downto 0);
signal VRAM0_WE	: std_logic;
signal VRAM0_ACK	: std_logic;
signal VRAM1_REQ	: std_logic;
signal VRAM1_A	: std_logic_vector(16 downto 1);
signal VRAM1_DO	: std_logic_vector(15 downto 0); -- Output from RAM
signal VRAM1_DI	: std_logic_vector(15 downto 0);
signal VRAM1_WE	: std_logic;
signal VRAM1_ACK	: std_logic;
signal VRAM0_A_PAD : std_logic_vector(addrwidth downto 17);
signal VRAM1_A_PAD : std_logic_vector(addrwidth downto 17);

signal SDR_INIT_DONE	: std_logic;

type bootStates is (BOOT_READ_1, BOOT_WRITE_1, BOOT_WRITE_2, BOOT_DONE);
signal bootState : bootStates := BOOT_DONE;

signal romwr_req : std_logic := '0';
signal romwr_ack : std_logic;
signal romwr_a : unsigned(21 downto 1);
signal romwr_d : std_logic_vector(15 downto 0);

signal rombank : std_logic_vector(1 downto 0); -- SF2+ bank
signal romrd_req : std_logic := '0';
signal romrd_ack : std_logic;
signal romrd_a : std_logic_vector(21 downto 3);
signal romrd_q : std_logic_vector(63 downto 0);
signal romrd_a_cached : std_logic_vector(21 downto 3);
signal romrd_q_cached : std_logic_vector(63 downto 0);
signal rommap : std_logic_vector(1 downto 0);
signal romhdr : std_logic;
signal romrd_a_adj  : std_logic_vector(21 downto 3);
signal roma_pad : std_logic_vector(addrwidth downto 22);

signal FL_DQ : std_logic_vector(15 downto 0);

type romStates is (ROM_IDLE, ROM_READ);
signal romState : romStates := ROM_IDLE;

signal CPU_A_PREV : std_logic_vector(20 downto 0);
signal ROM_RDY	: std_logic;
signal ROM_DO	: std_logic_vector(7 downto 0);

signal gamepad_port : unsigned(2 downto 0);
signal multitap : std_logic;
signal sixbutton_en : std_logic;
signal sixbutton_sel : std_logic;
signal prev_sel : std_logic_vector(1 downto 0);
signal SGX : std_logic;

begin

-- Bit flipping switch
BITFLIP <= ext_sw(2);
sixbutton_en <= ext_sw(3);
multitap <= ext_sw(4);
sgx <= ext_sw(5);

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

	VDCNUM  => VDCNUM,

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
RAM : entity work.DualPortRam generic map (15,8)
port map(
	clock		=> CLK,
	address_a	=> RAM_A,
	data_a		=> RAM_DI,
	wren_a		=> RAM_WE,
	q_a			=> RAM_DO,

	address_b	=> (others => '0'),
	data_b		=> (others => '0'),
	wren_b		=> '0'
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


VDC0 : entity work.huc6270 port map(
	CLK 		=> CLK,
	RESET_N		=> RESET_N,
	HSIZE		=> HSIZE,
	HSTART		=> HSTART,
	SP64		=> '0',

	-- CPU Interface
	A			=> CPU_A(1 downto 0),
	CE_N		=> CPU_VDC0_SEL_N,
	WR_N		=> CPU_WR_N,
	RD_N		=> CPU_RD_N,
	DI			=> CPU_DO,
	DO 			=> VDC0_DO,
	BUSY_N		=> VDC0_BUSY_N,
	IRQ_N		=> VDC0_IRQ_N,

	vram_req	=> VRAM0_REQ,
	vram_a		=> VRAM0_A,
	vram_q		=> VRAM0_DO,
	vram_d		=> VRAM0_DI,
	vram_we		=> VRAM0_WE,
	vram_ack	=> VRAM0_ACK,

	-- VCE Interface
	COLNO		=> VDC0_COLNO,
	CLKEN		=> VDC_CLKEN,
	HS_N		=> HS,
	VS_N		=> VS

);

VDC1 : entity work.huc6270 port map(
	CLK 		=> CLK,
	RESET_N		=> RESET_N,
	HSIZE		=> HSIZE,
	HSTART		=> HSTART,
	SP64		=> '0',

	-- CPU Interface
	A			=> CPU_A(1 downto 0),
	CE_N		=> CPU_VDC1_SEL_N,
	WR_N		=> CPU_WR_N,
	RD_N		=> CPU_RD_N,
	DI			=> CPU_DO,
	DO 			=> VDC1_DO,
	BUSY_N		=> VDC1_BUSY_N,
	IRQ_N		=> VDC1_IRQ_N,

	vram_req	=> VRAM1_REQ,
	vram_a		=> VRAM1_A,
	vram_q		=> VRAM1_DO,
	vram_d		=> VRAM1_DI,
	vram_we		=> VRAM1_WE,
	vram_ack	=> VRAM1_ACK,

	-- VCE Interface
	COLNO		=> VDC1_COLNO,
	CLKEN		=> VDC_CLKEN,
	HS_N		=> HS,
	VS_N		=> VS

);

VPC : entity work.huc6202
port map(
	CLK 		=> CLK,
	CLKEN		=> VDC_CLKEN,
	RESET_N	=> RESET_N,

	-- CPU Interface
	A			=> CPU_A(2 downto 0),
	CE_N		=> CPU_VPC_SEL_N,
	WR_N		=> CPU_WR_N,
	RD_N		=> CPU_RD_N,
	DI			=> CPU_DO,
	DO 		=> VPC_DO,

	HS_N		=> HS,
	VDC0_IN  => VDC0_COLNO,
	VDC1_IN  => VDC1_COLNO,
	VDC_OUT  => VDC_COLNO,

	VDCNUM   => VDCNUM
);

CPU_VDC0_SEL_N <= CPU_VDC_SEL_N or     CPU_A(3) or     CPU_A(4) when SGX = '1' else CPU_VDC_SEL_N;
CPU_VDC1_SEL_N <= CPU_VDC_SEL_N or     CPU_A(3) or not CPU_A(4) when SGX = '1' else '1';
CPU_VPC_SEL_N  <= CPU_VDC_SEL_N or not CPU_A(3) or     CPU_A(4) when SGX = '1' else '1';

romrd_a_adj <= romrd_a when romhdr = '0' else std_logic_vector(unsigned(romrd_a) + 64);
roma_pad <= (others => '0');
VRAM0_A_PAD <= (addrwidth => '1', others => '0');
VRAM1_A_PAD <= (addrwidth => '1', addrwidth-1 => '1', others => '0');

	sdr : entity work.chameleon_sdram
		generic map (
			casLatency => casLatency,
			rasCasTiming => rasCasTiming,
			prechargeTiming => prechargeTiming,
			colAddrBits => colAddrBits,
			rowAddrBits => rowAddrBits,
			t_ck_ns => t_ck_ns
		)
		port map (
			clk => SDR_CLK,
			reset_n => RESET_N,

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

			vram0_req => VRAM0_REQ,
			vram0_ack => VRAM0_ACK,
			vram0_we => VRAM0_WE,
			vram0_a => VRAM0_A_PAD & VRAM0_A,
			vram0_d => VRAM0_DI,
			vram0_q => VRAM0_DO,

			vram1_req => VRAM1_REQ,
			vram1_ack => VRAM1_ACK,
			vram1_we => VRAM1_WE,
			vram1_a => VRAM1_A_PAD & VRAM1_A,
			vram1_d => VRAM1_DI,
			vram1_q => VRAM1_DO,

			romwr_req => romwr_req,
			romwr_ack => romwr_ack,
			romwr_we => '1',
			romwr_a => roma_pad & std_logic_vector(romwr_a),
			romwr_d => romwr_d,

			romrd_req => romrd_req,
			romrd_ack => romrd_ack,
			romrd_a => roma_pad & romrd_a_adj,
			romrd_q => romrd_q,

			initDone => SDR_INIT_DONE,

			debugIdle => open,
			debugRefresh => open
		);

DRAM_CKE <= '1';
DRAM_CS_N <= '0';

-- Interrupt signals
CPU_NMI_N <= '1';
CPU_IRQ1_N <= VDC0_IRQ_N and VDC1_IRQ_N;
CPU_IRQ2_N <= '1';
CPU_RDY <= VDC0_BUSY_N and VDC1_BUSY_N and ROM_RDY;

-- CPU data bus
CPU_DI <= RAM_DO when CPU_RD_N = '0' and CPU_RAM_SEL_N = '0' 
	else ROM_DO when CPU_RD_N = '0' and CPU_A(20) = '0'
	else VCE_DO when CPU_RD_N = '0' and CPU_VCE_SEL_N = '0'
	else VDC0_DO when CPU_RD_N = '0' and CPU_VDC0_SEL_N = '0'
	else VDC1_DO when CPU_RD_N = '0' and CPU_VDC1_SEL_N = '0'
	else VPC_DO  when CPU_RD_N = '0' and CPU_VPC_SEL_N  = '0'
	else x"FF";

romrd_a <=    "00"&CPU_A(19 downto 3)                                         when rommap = "00" -- straight mapping
        else "000"&CPU_A(19)&(CPU_A(17) and not CPU_A(19))&CPU_A(16 downto 3) when rommap = "01" -- 384K
        else "00" &CPU_A(19)&(CPU_A(18) and not CPU_A(19))&CPU_A(17 downto 3) when rommap = "10" -- 768K
        else (CPU_A(19) and (rombank(0) and rombank(1)))
            &(CPU_A(19) and (rombank(0) xor rombank(1)))
            &(CPU_A(19) and not rombank(0))&CPU_A(18 downto 3);                                  -- SF2

-- SF2+ mapper
process( CLK, RESET_N ) begin
	if RESET_N = '0' then
		rombank <= "00";
	elsif rising_edge(CLK) then
		-- CPU_A(12 downto 2) = X"7FC" means CPU_A & 0x1FFC = 0x1FF0
		if CPU_A(20) = '0' and ('0' & CPU_A(12 downto 2)) = X"7FC" and CPU_WR_N = '0' then
			rombank <= CPU_A(1 downto 0);
		end if;
	end if;
end process;

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
						if romrd_a = romrd_a_cached then
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
							romrd_a_cached <= romrd_a;
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
			romwr_a <= (others => '0');
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
						case romwr_a(21 downto 16) is
						when "00"&x"6" =>
							rommap <= "01"; -- 384K ROM
						when "00"&x"c" =>
							rommap <= "10"; -- 768K ROM
						when "10"&x"8" =>
							rommap <= "11"; -- SF2+
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
RAM_A(12 downto 0)  <= CPU_A(12 downto 0);
RAM_A(14 downto 13) <= CPU_A(14 downto 13) when SGX = '1' else "00";
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
	"0000"            when CPU_IO_DO(1) = '1' or (CPU_IO_DO(0) = '1' and sixbutton_sel = '1') else
	joya( 7 downto 4) when CPU_IO_DO(1 downto 0) = "00" and sixbutton_sel = '0' and gamepad_port = "000" else
	joya( 3 downto 0) when CPU_IO_DO(1 downto 0) = "01" and sixbutton_sel = '0' and gamepad_port = "000" else
	joya(11 downto 8) when CPU_IO_DO(1 downto 0) = "00" and sixbutton_sel = '1' and gamepad_port = "000" else
	joyb( 7 downto 4) when CPU_IO_DO(1 downto 0) = "00" and sixbutton_sel = '0' and gamepad_port = "001" else
	joyb( 3 downto 0) when CPU_IO_DO(1 downto 0) = "01" and sixbutton_sel = '0' and gamepad_port = "001" else
	joyb(11 downto 8) when CPU_IO_DO(1 downto 0) = "00" and sixbutton_sel = '1' and gamepad_port = "001" else
	joyc( 7 downto 4) when CPU_IO_DO(1 downto 0) = "00" and sixbutton_sel = '0' and gamepad_port = "010" else
	joyc( 3 downto 0) when CPU_IO_DO(1 downto 0) = "01" and sixbutton_sel = '0' and gamepad_port = "010" else
	joyc(11 downto 8) when CPU_IO_DO(1 downto 0) = "00" and sixbutton_sel = '1' and gamepad_port = "010" else
	joyd( 7 downto 4) when CPU_IO_DO(1 downto 0) = "00" and sixbutton_sel = '0' and gamepad_port = "011" else
	joyd( 3 downto 0) when CPU_IO_DO(1 downto 0) = "01" and sixbutton_sel = '0' and gamepad_port = "011" else
	joyd(11 downto 8) when CPU_IO_DO(1 downto 0) = "00" and sixbutton_sel = '1' and gamepad_port = "011" else
	joye( 7 downto 4) when CPU_IO_DO(1 downto 0) = "00" and sixbutton_sel = '0' and gamepad_port = "100" else
	joye( 3 downto 0) when CPU_IO_DO(1 downto 0) = "01" and sixbutton_sel = '0' and gamepad_port = "100" else
	joye(11 downto 8) when CPU_IO_DO(1 downto 0) = "00" and sixbutton_sel = '1' and gamepad_port = "100" else
	"1111";

process(clk)
begin
	if rising_edge(clk) then
		if CPU_IO_DO(1)='1' then -- reset pad
			gamepad_port<=(others => '0');
			if prev_sel(1)='0' then
				sixbutton_sel <= not sixbutton_sel and sixbutton_en;
			end if;
		elsif prev_sel(0)='0' and CPU_IO_DO(0)='1' and multitap='1' then -- Rising edge of select bit
			gamepad_port<=gamepad_port+1;
		end if;
		prev_sel<=CPU_IO_DO(1 downto 0);
	end if;

end process;

end rtl;
