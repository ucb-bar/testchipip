#!/usr/bin/env bash
set -euo pipefail

RATE="${1:-10}"
UART="${UART:-/dev/ttyUSB1}"

case "${RATE}" in
  10)
    BMCR="0x8100"  # BMCR: reset=1, speed=10 Mbps, duplex=full, autoneg=off
    ;;
  100)
    BMCR="0xA100"  # BMCR: reset=1, speed=100 Mbps, duplex=full, autoneg=off
    ;;
  1000)
    BMCR="0x8140"  # BMCR: reset=1, speed=1000 Mbps, duplex=full
    ;;
  *)
    echo "Usage: $0 [10|100|1000]" >&2
    exit 1
    ;;
esac

python udp_tsi_host.py --uart "${UART}" mdio-link || true           # Read PHYSR/reg 0x11 and print current link speed/duplex
./set_phy_rx_delay.sh "${UART}" 9600                               # ExtPage 0xA4 reg 0x1C: enable RX-only RGMII delay bits
python udp_tsi_host.py --uart "${UART}" mdio-write 0x0 "${BMCR}"   # BMCR/reg 0x00: force requested rate/duplex and assert PHY soft reset
python udp_tsi_host.py --uart "${UART}" ping                        # Send UDP payload to FPGA and wait for bridge ACK
python udp_tsi_host.py --uart "${UART}" select-chip 0              # Ctrl port: select chip 0 for subsequent UDP-TSI traffic
