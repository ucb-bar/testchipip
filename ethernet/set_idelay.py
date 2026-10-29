#!/usr/bin/env python3
"""Set IDELAYE2 tap value via UART. Protocol: 0x01 (address) then tap_value."""

import sys
import serial
import argparse

PORT     = "/dev/ttyUSB2"
BAUDRATE = 9600

def main():
    parser = argparse.ArgumentParser(description="Set RGMII RX IDELAY tap value")
    parser.add_argument("tap", type=lambda x: int(x, 0), help="Tap value (0-31)")
    parser.add_argument("--port", default=PORT)
    parser.add_argument("--baud", type=int, default=BAUDRATE)
    args = parser.parse_args()

    if not 0 <= args.tap <= 31:
        print("Error: tap value must be 0-31")
        sys.exit(1)

    with serial.Serial(args.port, args.baud, timeout=1) as ser:
        ser.write(bytes([0x01, args.tap]))
        ser.flush()
        resp = ser.read(2)
        if len(resp) == 2 and resp[1] == 0x81:
            print(f"IDELAY tap set to {resp[0]} (0x{resp[0]:02X})")
        else:
            print(f"No/unexpected ACK: {resp.hex()}")

if __name__ == "__main__":
    main()
