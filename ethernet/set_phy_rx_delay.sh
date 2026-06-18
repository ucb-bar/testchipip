#!/usr/bin/env bash
set -euo pipefail

# Enable PHY RX-only 2ns RGMII delay through RTL8211E ExtPage 0xA4 reg 0x1C.
# Uses udp_tsi_host.py MDIO UART bridge.
#
# Defaults match your setup:
#   UART: /dev/ttyUSB2
#   BAUD: 9600
#
# Usage:
#   ./set_phy_rx_delay.sh
#   ./set_phy_rx_delay.sh /dev/ttyUSB2 9600

UART_DEV="${1:-/dev/ttyUSB1}"
BAUD="${2:-9600}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HOST_PY="${SCRIPT_DIR}/udp_tsi_host.py"
CMD=(python "$HOST_PY" --uart "$UART_DEV" --baud "$BAUD")

echo "Using UART=${UART_DEV} BAUD=${BAUD}"

cleanup_page0() {
  "${CMD[@]}" mdio-write 0x1f 0x0000 >/dev/null 2>&1 || true
}
trap cleanup_page0 EXIT

echo "[1/7] Select ExtPage access mode"
"${CMD[@]}" mdio-write 0x1f 0x0007

echo "[2/7] Select ExtPage 0xA4"
"${CMD[@]}" mdio-write 0x1e 0x00a4

echo "[3/7] Read current reg 0x1C"
READ_OUT="$("${CMD[@]}" mdio-read 0x1c)"
echo "$READ_OUT"

OLD_HEX="$(echo "$READ_OUT" | sed -n 's/.*= \(0x[0-9A-Fa-f]\{1,4\}\).*/\1/p' | tail -n1)"
if [[ -z "${OLD_HEX}" ]]; then
  echo "ERROR: failed to parse mdio-read output"
  exit 1
fi

OLD=$((OLD_HEX))
NEW=$((OLD | 0x3000)) # bit13 force delay control + bit12 RX delay (RX-only 2ns)
printf -v NEW_HEX "0x%04X" "$NEW"

echo "[4/7] Write reg 0x1C <= ${NEW_HEX} (OLD=${OLD_HEX}, NEW=OLD|0x3000)"
"${CMD[@]}" mdio-write 0x1c "$NEW_HEX"

echo "[5/7] Readback reg 0x1C"
READBACK_OUT="$("${CMD[@]}" mdio-read 0x1c)"
echo "$READBACK_OUT"
RB_HEX="$(echo "$READBACK_OUT" | sed -n 's/.*= \(0x[0-9A-Fa-f]\{1,4\}\).*/\1/p' | tail -n1)"
if [[ -z "${RB_HEX}" ]]; then
  echo "ERROR: failed to parse readback output"
  exit 1
fi
RB=$((RB_HEX))

if (( (RB & 0x3000) != 0x3000 )); then
  printf "ERROR: readback %s does not have RX-only delay bits set as expected\n" "$RB_HEX"
  exit 1
fi
if (( (RB & 0x0800) != 0 )); then
  printf "WARNING: TX delay bit is set in readback (%s). This is not RX-only.\n" "$RB_HEX"
fi

echo "[6/7] Return to page 0"
"${CMD[@]}" mdio-write 0x1f 0x0000

echo "[7/7] Link status"
"${CMD[@]}" mdio-link || true

echo "Done."

