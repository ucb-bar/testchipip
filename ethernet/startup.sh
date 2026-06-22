#!/usr/bin/env bash
set -euo pipefail

RATE="${1:-10}"
UART="${UART:-/dev/ttyUSB1}"

case "${RATE}" in
  10)
    BMCR="0x8100"  # BMCR: reset=1, speed=10 Mbps, duplex=full, autoneg=off
    GIG_CTRL=""
    ;;
  100)
    BMCR="0xA100"  # BMCR: reset=1, speed=100 Mbps, duplex=full, autoneg=off
    GIG_CTRL=""
    ;;
  1000)
    BMCR="0x9340"  # BMCR: reset=1, autoneg=on, restart autoneg=1, speed=1000 Mbps, duplex=full
    GIG_CTRL="0x0200"  # 1000BASE-T Control/reg 0x09: advertise 1000BASE-T full duplex
    ;;
  *)
    echo "Usage: $0 [10|100|1000]" >&2
    exit 1
    ;;
esac

python udp_tsi_host.py --uart "${UART}" mdio-link || true           # Read PHYSR/reg 0x11 and print current link speed/duplex
./set_phy_rx_delay.sh "${UART}" 9600                               # ExtPage 0xA4 reg 0x1C: enable RX-only RGMII delay bits
if [[ -n "${GIG_CTRL}" ]]; then
  python udp_tsi_host.py --uart "${UART}" mdio-write 0x09 "${GIG_CTRL}"  # 1000BASE-T Control/reg 0x09: advertise gigabit full duplex
fi
python udp_tsi_host.py --uart "${UART}" mdio-write 0x0 "${BMCR}"   # BMCR/reg 0x00: force requested rate/duplex and assert PHY soft reset
sleep 3                                                            # Allow PHY reset/link retrain to complete before UDP traffic
python udp_tsi_host.py --uart "${UART}" ping                        # Send UDP payload to FPGA and wait for bridge ACK
python udp_tsi_host.py --uart "${UART}" select-chip 0              # Ctrl port: select chip 0 for subsequent UDP-TSI traffic
