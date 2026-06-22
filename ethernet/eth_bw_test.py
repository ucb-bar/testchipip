#!/usr/bin/env python3
"""
eth_bw_test.py

Simple host-side UDP bandwidth probe for the FPGA Ethernet link.

By default this targets the FPGA control UDP port (7001) so the test stays on
the Ethernet/UDP path and does not depend on the TSI/chip backend. Each packet
is counted only after the FPGA returns the standard 8-byte ACK payload.
"""

import argparse
import socket
import struct
import sys
import time

ACK_MAGIC = 0xAC010001
DEFAULT_FPGA_IP = "192.168.1.10"
DEFAULT_FPGA_CTRL_PORT = 7001
DEFAULT_BIND_IP = "192.168.1.1"


def parse_ack(data):
    if not data or len(data) < 8:
        return None
    magic = struct.unpack(">I", data[0:4])[0]
    if magic != ACK_MAGIC:
        return None
    byte_count = struct.unpack(">H", data[4:6])[0]
    return magic, byte_count


def human_rate(bytes_per_s):
    bits_per_s = bytes_per_s * 8.0
    if bits_per_s >= 1e9:
        return f"{bits_per_s / 1e9:.3f} Gbps"
    if bits_per_s >= 1e6:
        return f"{bits_per_s / 1e6:.3f} Mbps"
    if bits_per_s >= 1e3:
        return f"{bits_per_s / 1e3:.3f} Kbps"
    return f"{bits_per_s:.1f} bps"


def main():
    parser = argparse.ArgumentParser(description="Measure sustained UDP request/ACK bandwidth to the FPGA Ethernet link")
    parser.add_argument("--ip", default=DEFAULT_FPGA_IP, help=f"FPGA IP address (default: {DEFAULT_FPGA_IP})")
    parser.add_argument("--port", type=int, default=DEFAULT_FPGA_CTRL_PORT,
                        help=f"FPGA UDP port to test (default: {DEFAULT_FPGA_CTRL_PORT}, ctrl port)")
    parser.add_argument("--bind-ip", default=DEFAULT_BIND_IP,
                        help=f"Local source IP to bind for the test (default: {DEFAULT_BIND_IP})")
    parser.add_argument("--payload-bytes", type=int, default=1400,
                        help="UDP payload bytes per test packet (default: 1400)")
    parser.add_argument("--packets", type=int, default=2000,
                        help="Number of packets to send in the measured run (default: 2000)")
    parser.add_argument("--warmup", type=int, default=20,
                        help="Warmup packets to send before measurement (default: 20)")
    parser.add_argument("--timeout", type=float, default=1.0,
                        help="Per-packet ACK timeout in seconds (default: 1.0)")
    parser.add_argument("--pattern", type=lambda x: int(x, 0), default=0x5A,
                        help="Payload fill byte value 0..255 (default: 0x5A)")
    args = parser.parse_args()

    if not (1 <= args.payload_bytes <= 65507):
        print("ERROR: --payload-bytes must be in 1..65507", file=sys.stderr)
        return 2
    if args.packets <= 0:
        print("ERROR: --packets must be > 0", file=sys.stderr)
        return 2

    payload = bytes([args.pattern & 0xFF]) * args.payload_bytes
    dest = (args.ip, args.port)

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 4 * 1024 * 1024)
    sock.bind((args.bind_ip, 0))
    sock.settimeout(args.timeout)

    def send_and_wait(expect_bytes, check_timeout=True):
        sock.sendto(payload, dest)
        while True:
            try:
                resp, _ = sock.recvfrom(4096)
            except socket.timeout:
                if check_timeout:
                    return False
                raise
            ack = parse_ack(resp)
            if ack is None:
                continue
            if ack[1] != expect_bytes:
                print(f"ERROR: ACK byte count {ack[1]} != expected {expect_bytes}", file=sys.stderr)
                return False
            return True

    for _ in range(args.warmup):
        if not send_and_wait(args.payload_bytes):
            print("ERROR: warmup timed out waiting for ACK", file=sys.stderr)
            return 1

    t0 = time.time()
    ok = 0
    for i in range(args.packets):
        if not send_and_wait(args.payload_bytes):
            print(f"ERROR: timed out waiting for ACK at packet {i}", file=sys.stderr)
            break
        ok += 1
    elapsed = time.time() - t0
    sock.close()

    if ok == 0:
        print("No packets were ACKed", file=sys.stderr)
        return 1

    payload_total = ok * args.payload_bytes
    pps = ok / elapsed if elapsed > 0 else 0.0
    byte_rate = payload_total / elapsed if elapsed > 0 else 0.0

    print(f"Source:      {args.bind_ip}")
    print(f"Destination: {args.ip}:{args.port}")
    print(f"Payload:     {args.payload_bytes} bytes")
    print(f"Packets:     {ok}/{args.packets} ACKed")
    print(f"Elapsed:     {elapsed:.3f} s")
    print(f"Rate:        {pps:.1f} pkt/s")
    print(f"Payload BW:  {byte_rate / 1e6:.3f} MB/s  ({human_rate(byte_rate)})")

    return 0 if ok == args.packets else 1


if __name__ == "__main__":
    sys.exit(main())
