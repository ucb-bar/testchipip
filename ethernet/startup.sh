#!/usr/bin/env bash
set -euo pipefail

RATE="${1:-10}"
UART="${UART:-/dev/ttyUSB1}"

case "${RATE}" in
  10)
    BMCR="0x0100"  # BMCR: reset=0, speed=10 Mbps, duplex=full, autoneg=off
    GIG_CTRL=""
    ;;
  100)
    BMCR="0x2100"  # BMCR: reset=0, speed=100 Mbps, duplex=full, autoneg=off
    GIG_CTRL=""
    ;;
  1000)
    BMCR="0x1340"  # BMCR: reset=0, autoneg=on, restart autoneg=1, speed=1000 Mbps, duplex=full
    GIG_CTRL="0x0200"  # 1000BASE-T Control/reg 0x09: advertise 1000BASE-T full duplex
    ;;
  *)
    echo "Usage: $0 [10|100|1000]" >&2
    exit 1
    ;;
esac

./set_phy_rx_delay.sh "${UART}" 9600                               # ExtPage 0xA4 reg 0x1C: enable RX-only RGMII delay bits
if [[ -n "${GIG_CTRL}" ]]; then
  python udp_tsi_host.py --uart "${UART}" mdio-write 0x09 "${GIG_CTRL}"  # 1000BASE-T Control/reg 0x09: advertise gigabit full duplex
fi
python udp_tsi_host.py --uart "${UART}" mdio-write 0x0 0x8000      # BMCR/reg 0x00: assert PHY soft reset only
sleep 1                                                            # Allow PHY reset to complete before programming final mode
python udp_tsi_host.py --uart "${UART}" mdio-write 0x0 "${BMCR}"   # BMCR/reg 0x00: program requested rate/duplex after reset
python udp_tsi_host.py --uart "${UART}" mdio-read 0x0              # Immediate BMCR readback after final mode write
sleep 1                                                            # Allow PHY/link state to settle after final mode write
python udp_tsi_host.py --uart "${UART}" mdio-read 0x0              # Settled BMCR readback
python udp_tsi_host.py --uart "${UART}" mdio-read 0x11             # Settled PHYSR readback
sleep 2                                                            # Leave a small extra margin before UDP traffic
python udp_tsi_host.py --uart "${UART}" ping                        # Send UDP payload to FPGA and wait for bridge ACK
python udp_tsi_host.py --uart "${UART}" select-chip 0              # Ctrl port: select chip 0 for subsequent UDP-TSI traffic
