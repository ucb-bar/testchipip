#!/usr/bin/env bash
# Open the Arduino serial console on ttyUSB0, send "all", stream the reply to stdout.
set -euo pipefail

PORT="${PORT:-/dev/ttyUSB0}"
BAUD="${BAUD:-9600}"
SETTLE="${SETTLE:-2}"          # seconds to wait after open (Arduino auto-resets on DTR)
READ_SECONDS="${READ_SECONDS:-3}"   # how long to collect the response
MSG="${1:-all}"

[[ -e "$PORT" ]] || { echo "error: $PORT not found" >&2; exit 1; }

# Raw 8N1, no echo, no flow control, at BAUD.
stty -F "$PORT" "$BAUD" cs8 -cstopb -parenb -icanon -echo -ixon -ixoff -crtscts raw

# Open the port once for both directions on fd 3, so the DTR-reset happens a single time.
exec 3<>"$PORT"

# Let the board finish resetting/booting before we talk to it.
sleep "$SETTLE"

# Send the command (LF line ending).
printf '%s\n' "$MSG" >&3

# Stream whatever comes back for READ_SECONDS, then stop.
timeout "$READ_SECONDS" cat <&3 || true

exec 3<&-
exec 3>&-
