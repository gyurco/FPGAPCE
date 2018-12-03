-- -----------------------------------------------------------------------
--
-- Turbo Chameleon
--
-- Multi purpose FPGA expansion for the Commodore 64 computer
--
-- -----------------------------------------------------------------------
-- Copyright 2005-2011 by Peter Wendrich (pwsoft@syntiac.com)
-- All Rights Reserved.
--
-- Your allowed to re-use this file for non-commercial applications
-- developed for the Turbo Chameleon 64 cartridge. Either open or closed
-- source whatever might be required by other licenses.
--
-- Used with persmission in the FPGAPCE project.
--
-- http://www.syntiac.com/chameleon.html
-- -----------------------------------------------------------------------
--
-- SDRAM controller
--
-- -----------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

-- -----------------------------------------------------------------------

entity chameleon_sdram is
	generic (
		-- SDRAM cols/rows  8/12 = 8 Mbyte, 9/12 = 16 Mbyte, 9/13 = 32 Mbyte
		colAddrBits : integer := 9;
		rowAddrBits : integer := 12;

		-- Controller settings
		initTimeout : integer := 10000;
	-- SDRAM timing
		casLatency : integer := 3;
		t_refresh_ms  : real := 64.0;
		t_ck_ns  : real := 10.0 -- Clock cycle time
	);
	port (
-- System
		clk : in std_logic;

		reserve : in std_logic := '0';
		delay_refresh : in std_logic := '0';

-- SDRAM interface
		sd_data : inout std_logic_vector(15 downto 0);
		sd_addr : out std_logic_vector((rowAddrBits-1) downto 0);
		sd_we_n : out std_logic;
		sd_ras_n : out std_logic;
		sd_cas_n : out std_logic;
		sd_ba_0 : out std_logic;
		sd_ba_1 : out std_logic;
		sd_ldqm : out std_logic;
		sd_udqm : out std_logic;

--GE PCE VDC port (16 bits port)
		vram_req : in std_logic;
		vram_ack : out std_logic;
		vram_we : in std_logic;
		vram_a : in std_logic_vector((colAddrBits+rowAddrBits+2) downto 1);
		vram_d : in std_logic_vector(15 downto 0);
		vram_q : out std_logic_vector(15 downto 0);

		romwr_req : in std_logic;
		romwr_ack : out std_logic;
		romwr_we : in std_logic;
		romwr_a : in std_logic_vector((colAddrBits+rowAddrBits+2) downto 1);
		romwr_d : in std_logic_vector(15 downto 0);
		romwr_q : out std_logic_vector(15 downto 0);
		
		romrd_req : in std_logic;
		romrd_ack : out std_logic;
		romrd_a : in std_logic_vector((colAddrBits+rowAddrBits+2) downto 3);
		romrd_q : out std_logic_vector(63 downto 0);
		
--GE Temporary
		initDone : out std_logic;
		
-- Debug ports
		debugIdle : out std_logic;  -- '1' memory is idle
		debugRefresh : out std_logic -- '1' memory is being refreshed
	);
end entity;

-- -----------------------------------------------------------------------

architecture rtl of chameleon_sdram is
	constant refresh_interval : integer := integer((t_refresh_ms*1000000.0) / (t_ck_ns * 2.0**rowAddrBits));
-- ram state machine
	type ramStates is (
		RAM_INIT,
		RAM_INIT_PRECHARGE,
		RAM_INITAUTO1,
		RAM_INITAUTO2,
		RAM_SETMODE,
		RAM_IDLE,
		
		RAM_ACTIVE,
		RAM_READ_1,
		RAM_READ_2,
		RAM_READ_3,
		RAM_READ_4,
		RAM_READ_5,
		RAM_WRITE_1,
		
		RAM_PRECHARGE,
		RAM_PRECHARGE_ALL,
		RAM_AUTOREFRESH
	);
	
	type ramPorts is (
		PORT_NONE,
		PORT_VRAM,
		PORT_ROMRD,
		PORT_ROMWR
	);
	subtype row_t is std_logic_vector((rowAddrBits-1) downto 0);
	subtype col_t is std_logic_vector((colAddrBits-1) downto 0);
	
	signal ramTimer : integer range 0 to 32767;
	signal ramState : ramStates := RAM_INIT;
	signal ramDone : std_logic;
	
	signal ram_data_reg : std_logic_vector(sd_data'range);

-- Registered sdram signals
	signal sd_data_reg : std_logic_vector(15 downto 0);
	signal sd_data_ena : std_logic := '0';
	signal sd_addr_reg : std_logic_vector((rowAddrBits-1) downto 0);
	signal sd_we_n_reg : std_logic;
	signal sd_ras_n_reg : std_logic;
	signal sd_cas_n_reg : std_logic;
	signal sd_ba_0_reg : std_logic;
	signal sd_ba_1_reg : std_logic;
	signal sd_ldqm_reg : std_logic;
	signal sd_udqm_reg : std_logic;
--GE
	signal vram_ackReg : std_logic := '0';
	signal romwr_ackReg : std_logic := '0';
	signal romrd_ackReg : std_logic := '0';
--GE
	signal vram_qReg : std_logic_vector(15 downto 0);
	signal romwr_qReg : std_logic_vector(15 downto 0);
	signal romrd_qReg : std_logic_vector(63 downto 0);
	signal initDoneReg : std_logic := '0';
	
-- Active rows in SDRAM
	type bankRowDef is array(0 to 3) of row_t;
	signal bankActive : std_logic_vector(0 to 3) := (others => '0');
	signal bankRow : bankRowDef;

-- Memory auto refresh
	constant refreshClocks : integer := 9;
	signal refreshTimer : integer range 0 to 2047 := 0;
	signal refreshActive : std_logic := '0';
	signal refreshSubtract : std_logic := '0';

	signal currentState : ramStates;
	signal currentPort : ramPorts;
	signal currentBank : std_logic_vector(1 downto 0);
	signal currentRow : row_t;
	signal currentCol : col_t;
	signal currentRdData : std_logic_vector(63 downto 0);
	signal currentWrData : std_logic_vector(15 downto 0);
	signal currentLdqm : std_logic;
	signal currentUdqm : std_logic;

	signal nextRamBank : std_logic_vector(1 downto 0);
	signal nextRamRow : row_t;
	signal nextRamCol : col_t;
	signal nextRamPort : ramPorts;
	signal nextRamState : ramStates;
	signal nextLdqm : std_logic;
	signal nextUdqm : std_logic;

begin

-- -----------------------------------------------------------------------

	ram_data_reg <= sd_data;

-- -----------------------------------------------------------------------
-- Refresh timer
	process(clk)
	begin
		if rising_edge(clk) then
			if refreshSubtract = '1' then
				refreshTimer <= refreshTimer - refresh_interval;
			else
-- synthesis translate_off
				if refreshTimer < 2047 then --GE
-- synthesis translate_on
					refreshTimer <= refreshTimer + 1;
-- synthesis translate_off
				end if;
-- synthesis translate_on
			end if;
		end if;
	end process;

-- -----------------------------------------------------------------------
-- State machine
	process(clk)
	begin
		--if rising_edge(clk) then
			nextRamState <= RAM_IDLE;
			nextRamPort <= PORT_NONE;
			nextRamBank <= "00";
			nextRamRow <= ( others => '0');
			nextRamCol <= ( others => '0');
			nextLdqm <= '0';
			nextUdqm <= '0';

			if (vram_req /= vram_ackReg) and (currentPort /= PORT_VRAM) then --GE
				nextRamState <= RAM_READ_1;
				if vram_we = '1' then
					nextRamState <= RAM_WRITE_1;
				end if;
				nextRamPort <= PORT_VRAM;
				nextRamBank <= vram_a((colAddrBits+rowAddrBits+2) downto (colAddrBits+rowAddrBits+1));
				nextRamRow <= vram_a((colAddrBits+rowAddrBits) downto (colAddrBits+1));
				nextRamCol <= vram_a(colAddrBits downto 1);
			elsif (romwr_req /= romwr_ackReg) and (currentPort /= PORT_ROMWR) then
				nextRamState <= RAM_READ_1;
				if romwr_we = '1' then
					nextRamState <= RAM_WRITE_1;
				end if;
				nextRamPort <= PORT_ROMWR;
				nextRamBank <= romwr_a((colAddrBits+rowAddrBits+2) downto (colAddrBits+rowAddrBits+1));
				nextRamRow <= romwr_a((colAddrBits+rowAddrBits) downto (colAddrBits+1));
				nextRamCol <= romwr_a(colAddrBits downto 1);

			elsif (romrd_req /= romrd_ackReg) and (currentPort /= PORT_ROMRD) then
				nextRamState <= RAM_READ_1;
				nextRamPort <= PORT_ROMRD;
				nextRamBank <= romrd_a((colAddrBits+rowAddrBits+2) downto (colAddrBits+rowAddrBits+1));
				nextRamRow <= romrd_a((colAddrBits+rowAddrBits) downto (colAddrBits+1));
				nextRamCol <= romrd_a(colAddrBits downto 3) & "00";
				
			end if;
		--end if;		
	end process;

	process(clk)
	begin
		if rising_edge(clk) then
			sd_data <= (others => 'Z');
			if sd_data_ena = '1' then
				sd_data <= sd_data_reg;
			end if;
			sd_addr <= sd_addr_reg;
			sd_ras_n <= sd_ras_n_reg;
			sd_cas_n <= sd_cas_n_reg;
			sd_we_n <= sd_we_n_reg;
			sd_ba_0 <= sd_ba_0_reg;
			sd_ba_1 <= sd_ba_1_reg;
			sd_ldqm <= sd_ldqm_reg;
			sd_udqm <= sd_udqm_reg;
		end if;
	end process;

	process(clk)
	begin
		if rising_edge(clk) then
			refreshSubtract <= '0';
			ramDone <= '0';
			sd_data_ena <= '0';
			sd_addr_reg <= (others => '0');
			sd_ras_n_reg <= '1';
			sd_cas_n_reg <= '1';
			sd_we_n_reg <= '1';
			
			sd_ba_0_reg <= '0';
			sd_ba_1_reg <= '0';

			sd_ldqm_reg <= '0';
			sd_udqm_reg <= '0';
			
			if ramTimer /= 0 then
				ramTimer <= ramTimer - 1;
			else
				case ramState is
				when RAM_INIT =>
					-- Wait for clock to stabilise and PLL locks
					-- Then follow init steps in datasheet:
					--   precharge all banks
					--   perform a few autorefresh cycles (we do 2 of them)
					--   setmode (burst and CAS latency)
					--   after a few clocks ram is ready for use (we wait 10 just to be sure).
					ramTimer <= 20000;
					ramState <= RAM_INIT_PRECHARGE;
				when RAM_INIT_PRECHARGE =>
					-- Precharge all banks, part of initialisation sequence.
					ramTimer <= 100;
					ramState <= RAM_INITAUTO1;
					sd_ras_n_reg <= '0';
					sd_we_n_reg <= '0';
					sd_addr_reg(10) <= '1'; -- precharge all banks
				when RAM_INITAUTO1 =>
					-- refresh cycle to init ram (1st)
					ramTimer <= 10;
					ramState <= RAM_INITAUTO2;
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';
					sd_cas_n_reg <= '0';				
				when RAM_INITAUTO2 =>
					-- refresh cycle to init ram (2nd)
					ramTimer <= 10;
					ramState <= RAM_SETMODE;
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';
					sd_cas_n_reg <= '0';
				when RAM_SETMODE =>
					-- Set mode bits of RAM.
					ramTimer <= 10;
					ramState <= RAM_IDLE; -- ram is ready for commands after set-mode
					sd_addr_reg <= std_logic_vector(resize("001000100010", sd_addr'length)); -- CAS2, Burstlength 4 (8 bytes, 64 bits), no burst on writes
					if casLatency = 3 then
						sd_addr_reg(6 downto 4) <= "011";
					end if;
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';
					sd_cas_n_reg <= '0';
				when RAM_IDLE =>
					initDoneReg <= '1'; --GE
					refreshActive <= '0';
					currentPort <= PORT_NONE;
					if nextRamState /= RAM_IDLE then
						currentState <= nextRamState;
						currentPort <= nextRamPort;
						currentBank <= nextRamBank;
						currentRow <= nextRamRow;
						currentCol <= nextRamCol;
						currentLdqm <= nextLdqm;
						currentUdqm <= nextUdqm;
						
						case nextRamPort is
						when PORT_VRAM => --GE
							currentWrData <= vram_d;
						when PORT_ROMWR =>
							currentWrData <= romwr_d;
						when others =>
							null;
						end case;

						ramState <= nextRamState;
						if bankActive(to_integer(unsigned(nextRamBank))) = '0' then
							-- Current bank not active. Activate a row first
							ramState <= RAM_ACTIVE;
						elsif bankRow(to_integer(unsigned(nextRamBank))) /= nextRamRow then
							-- Wrong row active in bank, do precharge then activate a row.
							ramState <= RAM_PRECHARGE;
						end if;
					elsif (delay_refresh = '0') and (reserve = '0') and (refreshTimer > refresh_interval) then
						-- Refresh timeout, perform auto-refresh cycle
						refreshActive <= '1';
						refreshSubtract <= '1';
						if bankActive /= "0000" then
							-- There are still rows active, so we precharge them first							
							ramState <= RAM_PRECHARGE_ALL;
						else
							ramState <= RAM_AUTOREFRESH;
						end if;
					end if;
				when RAM_ACTIVE =>
					ramTimer <= 2;
					ramState <= currentState;
					sd_addr_reg <= currentRow;
					sd_ras_n_reg <= '0';
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);
					bankRow(to_integer(unsigned(currentBank))) <= currentRow;
					bankActive(to_integer(unsigned(currentBank))) <= '1';
				when RAM_READ_1 =>
					ramTimer <= casLatency + 1;
					ramState <= RAM_READ_2;
					sd_addr_reg <= std_logic_vector(resize(unsigned(currentCol), sd_addr'length));
					--GE sd_addr_reg <= resize(currentCol, sd_addr'length) or resize("10000000000", sd_addr'length); --GE Auto precharge
					sd_cas_n_reg <= '0';
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);
				when RAM_READ_2 =>
					ramState <= RAM_READ_3;
					currentRdData(15 downto 0) <= ram_data_reg;
					case currentPort is
					when PORT_VRAM => --GE
						vram_qReg <= ram_data_reg;
						ramDone <= '1';
						sd_we_n_reg <= '0'; -- abort burst
						sd_cas_n_reg <= '1';
						sd_ras_n_reg <= '1';
						ramState <= RAM_IDLE;
					when PORT_ROMWR => --GE
						romwr_qReg <= ram_data_reg;
						ramDone <= '1';
					when others =>
						null;
					end case;
				when RAM_READ_3 =>
					ramState <= RAM_READ_4;
					currentRdData(31 downto 16) <= ram_data_reg;
				when RAM_READ_4 =>
					ramState <= RAM_READ_5;
					currentRdData(47 downto 32) <= ram_data_reg;
				when RAM_READ_5 =>
					currentRdData(63 downto 48) <= ram_data_reg;
					ramState <= RAM_IDLE;
-- /!\
					case currentPort is
					when PORT_VRAM | PORT_ROMWR => --GE
						null;
					when others =>
						ramDone <= '1';
					end case;
-- /!\
				when RAM_WRITE_1 =>
					ramState <= RAM_IDLE;
					sd_data_ena <= '1';
					sd_we_n_reg <= '0';
					sd_cas_n_reg <= '0';
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);

					sd_addr_reg <= std_logic_vector(resize(unsigned(currentCol), sd_addr'length));
					--GE sd_addr_reg <= resize(currentCol, sd_addr'length) or resize("10000000000", sd_addr'length); --GE Auto precharge

					sd_data_reg <= currentWrData;
					sd_ldqm_reg <= currentLdqm;
					sd_udqm_reg <= currentUdqm;
					ramDone <= '1';

				when RAM_PRECHARGE =>
					ramTimer <= 2;
					ramState <= RAM_ACTIVE;
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';				
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);
					bankActive(to_integer(unsigned(currentBank))) <= '0';
				when RAM_PRECHARGE_ALL =>
					ramTimer <= 2;
					ramState <= RAM_IDLE;
					if refreshActive = '1' then
						ramTimer <= 1;
						ramState <= RAM_AUTOREFRESH;
					end if;
					sd_addr_reg(10) <= '1'; -- All banks
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';				
					bankActive <= "0000";
				when RAM_AUTOREFRESH =>
					ramTimer <= refreshClocks;
					ramState <= RAM_IDLE;
					sd_we_n_reg <= '1';
					sd_ras_n_reg <= '0';
					sd_cas_n_reg <= '0';
				end case;
			end if;
		end if;
	end process;

-- -----------------------------------------------------------------------
-- Debug and measurement signals
	debugIdle <= '1' when ((refreshActive = '0') and (ramState = RAM_IDLE)) else '0';
	debugRefresh <= refreshActive;

--GE -----------------------------------------------------------------------
--GE PCE VDC port
	process(clk)
	begin
		if rising_edge(clk) then
			if currentPort = PORT_VRAM
			and ramDone = '1' then
				vram_ackReg <= not vram_ackReg;
			end if;
		end if;
	end process;
	vram_ack <= vram_ackReg;
	vram_q <= vram_qReg; --GE

	process(clk)
	begin
		if rising_edge(clk) then
			if currentPort = PORT_ROMWR
			and ramDone = '1' then
				romwr_ackReg <= not romwr_ackReg;
			end if;
		end if;
	end process;
	romwr_ack <= romwr_ackReg;
	romwr_q <= romwr_qReg; --GE

	process(clk)
	begin
		if rising_edge(clk) then
			if currentPort = PORT_ROMRD
			and ramDone = '1' then
				romrd_ackReg <= not romrd_ackReg;
				romrd_qReg <= currentRdData;
			end if;
		end if;
	end process;
	romrd_ack <= romrd_ackReg;
	romrd_q <= romrd_qReg; --GE
	
	initDone <= initDoneReg; --Ge
	
end architecture;
