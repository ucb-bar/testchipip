# UDP-TSI Bridge Integration Guide
## Using verilog-ethernet + Chipyard on Nexys Video

### Overview

This replaces the UART-TSI path in Chipyard with a pure-fabric UDP interface.
The host sends TSI commands over UDP, and the FPGA responds with ACKs and
TSI read data — no MicroBlaze, no soft CPU.

The Ethernet stack is Alex Forencich's open-source verilog-ethernet library
(MIT license). The only custom RTL is `udp_payload_to_tsi_serial.v`, which
converts UDP payload bytes to/from TSI serial words.

### Architecture

```
Host PC (udp_tsi_host.py)
    |
    | UDP port 7000
    v
RGMII PHY (Nexys Video RTL8211E)
    |
    v
eth_mac_1g_rgmii_fifo   <-- from verilog-ethernet
    |                        handles: RGMII DDR, CRC, preamble,
    |                        padding, clock domain crossing
    v
udp_complete             <-- from verilog-ethernet
    |                        handles: Ethernet framing, ARP,
    |                        IPv4, UDP headers
    v
udp_payload_to_tsi_serial  <-- YOUR CUSTOM RTL (only ~200 lines)
    |                          handles: byte packing, ACK generation
    v
TSIToTileLink            <-- existing Chipyard
    |
    v
TileLink Front Bus -> RISC-V SoC
```

### Step 1: Clone verilog-ethernet

```bash
cd fpga/
git submodule add https://github.com/alexforencich/verilog-ethernet.git
```

This keeps it under `fpga/` alongside your other FPGA-specific resources
(constraints, board files, FPGA shell code) — it's infrastructure, not a
Chipyard generator.

### Step 2: Add Verilog sources to your build

Add these files from `fpga/verilog-ethernet/rtl/` to your Vivado project
(via TCL `add_files` or the Vivado GUI). Also add `udp_tsi_top.v` and
`udp_payload_to_tsi_serial.v` from this directory.

**Directory layout:**
```
chipyard/
├── fpga/
│   ├── verilog-ethernet/           ← git submodule (MIT license)
│   │   └── rtl/                    ← MAC, IP, ARP, UDP modules
│   ├── udp_tsi_top.v               ← our top-level wrapper
│   ├── udp_payload_to_tsi_serial.v ← our custom glue (only custom RTL)
│   └── ...                         ← XDC constraints, FPGA shell, etc.
├── generators/
│   └── testchipip/src/main/scala/testchipip/tsi/
│       └── UDPToTSI.scala          ← Chisel BlackBox + Chipyard trait
└── ...
```

**MAC layer:**
- `eth_mac_1g_rgmii_fifo.v`
- `eth_mac_1g_rgmii.v`
- `eth_mac_1g.v`
- `rgmii_phy_if.v`
- `axis_gmii_rx.v`
- `axis_gmii_tx.v`
- `ssio_ddr_in.v`
- `ssio_ddr_out.v`
- `iddr.v`, `oddr.v` (for generic/sim; Xilinx uses primitives)

**Ethernet framing:**
- `eth_axis_rx.v`
- `eth_axis_tx.v`
- `eth_arb_mux.v`

**ARP:**
- `arp.v`
- `arp_cache.v`
- `arp_eth_rx.v`
- `arp_eth_tx.v`

**IP:**
- `ip.v`
- `ip_complete.v`
- `ip_eth_rx.v`
- `ip_eth_tx.v`
- `ip_arb_mux.v`
- `ip_demux.v`
- `ip_mux.v`

**UDP:**
- `udp.v`
- `udp_complete.v`
- `udp_ip_rx.v`
- `udp_ip_tx.v`
- `udp_checksum_gen.v`
- `udp_demux.v`
- `udp_mux.v`

**Utilities:**
- `lfsr.v`
- `axis_fifo.v`
- `axis_async_fifo.v`
- `axis_async_fifo_adapter.v`

**Custom (from this project):**
- `udp_tsi_top.v`
- `udp_payload_to_tsi_serial.v`

### Step 3: Clock generation (Vivado)

The MAC needs three clock signals:

| Clock      | Frequency | Purpose                          |
|------------|-----------|----------------------------------|
| clk        | Any       | Your logic clock                 |
| gtx_clk    | 125 MHz   | Gigabit TX reference             |
| gtx_clk90  | 125 MHz   | 90° phase shifted for RGMII TX   |

Generate these from a Clocking Wizard (MMCM/PLL):
- Input: 100 MHz board oscillator
- Output 1: your logic frequency (e.g., 125 MHz)
- Output 2: 125 MHz (gtx_clk)
- Output 3: 125 MHz, 90° phase shift (gtx_clk90)
- Output 4: 200 MHz (for DDR3 MIG reference, if needed)

If your logic clock IS 125 MHz, outputs 1 and 2 can be the same signal.

### Step 4: Chipyard integration

In `DigitalTop.scala`:
```scala
// Remove:  with testchipip.tsi.CanHavePeripheryUARTTSI
// Add:
with testchipip.tsi.CanHavePeripheryUDPTSI
```

In your config:
```scala
class NexysVideoUDPConfig extends Config(
  new testchipip.tsi.WithUDPTSI(
    fpgaIp  = (192L << 24) | (168L << 16) | (1L << 8) | 10L,
    udpPort = 7000
  ) ++
  new testchipip.tsi.WithNoUARTTSI() ++
  new chipyard.config.AbstractConfig
)
```

### Step 5: Pin constraints (Nexys Video XDC)

```tcl
# RGMII PHY (RTL8211E on Nexys Video)
set_property -dict {PACKAGE_PIN V13  IOSTANDARD LVCMOS25} [get_ports {rgmii_txd[0]}]
set_property -dict {PACKAGE_PIN V14  IOSTANDARD LVCMOS25} [get_ports {rgmii_txd[1]}]
set_property -dict {PACKAGE_PIN V17  IOSTANDARD LVCMOS25} [get_ports {rgmii_txd[2]}]
set_property -dict {PACKAGE_PIN V18  IOSTANDARD LVCMOS25} [get_ports {rgmii_txd[3]}]
set_property -dict {PACKAGE_PIN V10  IOSTANDARD LVCMOS25} [get_ports rgmii_tx_ctl]
set_property -dict {PACKAGE_PIN AA14 IOSTANDARD LVCMOS25} [get_ports rgmii_txc]

set_property -dict {PACKAGE_PIN AB16 IOSTANDARD LVCMOS25} [get_ports {rgmii_rxd[0]}]
set_property -dict {PACKAGE_PIN AA15 IOSTANDARD LVCMOS25} [get_ports {rgmii_rxd[1]}]
set_property -dict {PACKAGE_PIN AB15 IOSTANDARD LVCMOS25} [get_ports {rgmii_rxd[2]}]
set_property -dict {PACKAGE_PIN AB11 IOSTANDARD LVCMOS25} [get_ports {rgmii_rxd[3]}]
set_property -dict {PACKAGE_PIN W10  IOSTANDARD LVCMOS25} [get_ports rgmii_rx_ctl]
set_property -dict {PACKAGE_PIN V12  IOSTANDARD LVCMOS25} [get_ports rgmii_rxc]

set_property -dict {PACKAGE_PIN U7   IOSTANDARD LVCMOS33} [get_ports phy_reset_n]
```

*NOTE: Verify these pin assignments against the Nexys Video schematic
for your board revision. The verilog-ethernet Nexys Video example
(`example/NexysVideo/`) has a tested XDC you can reference.*

### Step 6: Host-side usage

```bash
# Load a binary
python udp_tsi_host.py --load program.bin --base 0x80000000

# Write a word
python udp_tsi_host.py --write 0x80000000 0xDEADBEEF

# Read memory
python udp_tsi_host.py --read 0x80000000 64
```

### Key Parameters

| Parameter     | Default          | Description                    |
|---------------|------------------|--------------------------------|
| FPGA_MAC      | 00:0A:35:00:01:02| FPGA's Ethernet MAC address    |
| FPGA_IP       | 192.168.1.10     | FPGA's IP address              |
| FPGA_GATEWAY  | 192.168.1.1      | Gateway IP                     |
| SUBNET_MASK   | 255.255.255.0    | Subnet mask                    |
| UDP_PORT      | 7000             | Listening port                 |
| SERIAL_WIDTH  | 32               | TSI serial word width (bits)   |
| ACK_PAYLOAD   | 0xAC010001       | Placeholder ACK encoding       |

### Resource Usage (approximate, Artix-7)

| Component                    | LUTs  | FFs   |
|------------------------------|-------|-------|
| eth_mac_1g_rgmii_fifo        | ~1200 | ~1400 |
| udp_complete (IP+ARP+UDP)    | ~2500 | ~2800 |
| udp_payload_to_tsi_serial    | ~150  | ~200  |
| **Total (excl. TSIToTileLink)** | **~3850** | **~4400** |

The XC7A200T on the Nexys Video has 134,600 LUTs — this uses about 3%.
