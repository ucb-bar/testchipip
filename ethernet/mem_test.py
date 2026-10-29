#!/usr/bin/env python3
"""
mem_test.py — Random write/readback memory test via TSI UDP interface.

Config file (JSON):
  base_addr   : hex string start address         (default "0x70000000")
  size_bytes  : total bytes to test              (default 65536)
  chunk_words : 64-bit words per TSI transfer    (default 16 = 128 B)
  seed        : integer seed, or null for random
  cflush_addr : hex string of L2 flush reg, or null if no flush needed
  fpga_ip     : FPGA IP                          (default "192.168.1.10")
  fpga_port   : FPGA UDP port                    (default 7000)

Usage:
  python3 mem_test.py                                        # use built-in defaults
  python3 mem_test.py --config mem_test_config.json
  python3 mem_test.py --base 0x70000000 --size 1048576
  python3 mem_test.py --config mem_test_config.json --out results.json
"""

import sys
import os
import json
import random
import struct
import time
import socket
import argparse

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from udp_tsi_host import (
    make_tsi_write_cmd, make_tsi_read_cmd,
    send_tsi_words, recv_response, parse_ack,
    flush_socket, FPGA_IP, FPGA_PORT, TIMEOUT, _align4,
)

DEFAULT_CONFIG = {
    "base_addr":   "0x70000000",
    "size_bytes":  65536,
    "chunk_words": 16,
    "seed":        None,
    "cflush_addr": None,
    "fpga_ip":     FPGA_IP,
    "fpga_port":   FPGA_PORT,
}


def make_sock(ip, port):
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.bind(("", 0))
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 8 * 1024 * 1024)
    actual = sock.getsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF)
    print(f"SO_RCVBUF = {actual} bytes")
    return sock, (ip, port)


def tsi_write_chunk(sock, dest, addr, values_u64):
    data = b"".join(struct.pack("<Q", v) for v in values_u64)
    words = make_tsi_write_cmd(addr, data)
    send_tsi_words(sock, words, dest)
    resp = recv_response(sock)
    return parse_ack(resp) is not None if resp else False


def tsi_read_chunk(sock, dest, addr, num_words):
    num_bytes = num_words * 8
    words, expected_words = make_tsi_read_cmd(addr, num_bytes)
    send_tsi_words(sock, words, dest)

    data = b""
    timeout = TIMEOUT
    while len(data) < expected_words * 4:
        resp = recv_response(sock, timeout=timeout)
        if resp is None:
            break
        timeout = 1.0
        if parse_ack(resp):
            continue
        data += resp

    if len(data) < num_bytes:
        return None
    return [struct.unpack_from("<Q", data, i * 8)[0] for i in range(num_words)]


def flush_cache_region(sock, dest, base_addr, size_bytes, cflush_addr):
    """Flush L2 cache lines covering [base_addr, base_addr+size_bytes)."""
    line_size = 64
    for offset in range(0, size_bytes, line_size):
        addr_to_flush = base_addr + offset
        data = struct.pack("<Q", addr_to_flush)
        words = make_tsi_write_cmd(cflush_addr, data)
        send_tsi_words(sock, words, dest)
        recv_response(sock)


def run_test(config):
    base_addr   = int(config["base_addr"], 16) if isinstance(config["base_addr"], str) else config["base_addr"]
    size_bytes  = config["size_bytes"]
    chunk_words = config["chunk_words"]
    seed        = config["seed"] if config["seed"] is not None else random.randint(0, 2**32 - 1)
    cflush_addr = int(config["cflush_addr"], 16) if config.get("cflush_addr") else None

    assert size_bytes % 8 == 0, "size_bytes must be a multiple of 8"
    num_words = size_bytes // 8

    rng = random.Random(seed)
    all_values = [rng.randint(0, 0xFFFFFFFFFFFFFFFF) for _ in range(num_words)]

    sock, dest = make_sock(config["fpga_ip"], config["fpga_port"])

    result = {
        "config": {**config, "base_addr": hex(base_addr), "seed": seed},
        "words_tested":  num_words,
        "write_errors":  [],
        "read_errors":   [],
        "mismatches":    [],
        "pass":          False,
        "write_time_s":  0.0,
        "read_time_s":   0.0,
        "error_count":   0,
    }

    # ---- Write phase --------------------------------------------------------
    print(f"\nWrite phase: {num_words} x 64-bit words to 0x{base_addr:08X} "
          f"({size_bytes // 1024} KB, seed=0x{seed:08X})")
    t0 = time.time()
    for start in range(0, num_words, chunk_words):
        end   = min(start + chunk_words, num_words)
        addr  = base_addr + start * 8
        chunk = all_values[start:end]
        ok = tsi_write_chunk(sock, dest, addr, chunk)
        if not ok:
            result["write_errors"].append(hex(addr))
            print(f"  WARN: no ACK at 0x{addr:08X}")
        if (start // chunk_words) % 64 == 0:
            pct = 100 * start / num_words
            print(f"  {start * 8 // 1024} KB / {size_bytes // 1024} KB ({pct:.0f}%)")
    result["write_time_s"] = round(time.time() - t0, 2)
    print(f"Write done in {result['write_time_s']}s")

    # ---- Optional cache flush -----------------------------------------------
    if cflush_addr:
        print(f"Flushing L2 cache (flush reg 0x{cflush_addr:08X})...")
        flush_cache_region(sock, dest, base_addr, size_bytes, cflush_addr)

    # ---- Read + verify phase ------------------------------------------------
    print(f"\nRead+verify phase...")
    errors = 0
    t0 = time.time()
    for start in range(0, num_words, chunk_words):
        end  = min(start + chunk_words, num_words)
        addr = base_addr + start * 8
        n    = end - start
        readback = tsi_read_chunk(sock, dest, addr, n)
        if readback is None:
            result["read_errors"].append(hex(addr))
            print(f"  ERROR: no readback at 0x{addr:08X}")
            errors += n
            continue
        for i, (exp, got) in enumerate(zip(all_values[start:end], readback)):
            if exp != got:
                word_addr = addr + i * 8
                xor = exp ^ got
                result["mismatches"].append({
                    "addr":     hex(word_addr),
                    "expected": hex(exp),
                    "got":      hex(got),
                    "xor":      hex(xor),
                    "bad_bits": bin(xor),
                })
                errors += 1
                print(f"  MISMATCH 0x{word_addr:08X}: "
                      f"exp=0x{exp:016X} got=0x{got:016X} xor=0x{xor:016X}")
        if (start // chunk_words) % 64 == 0:
            pct = 100 * start / num_words
            print(f"  {start * 8 // 1024} KB / {size_bytes // 1024} KB ({pct:.0f}%)")

    result["read_time_s"]  = round(time.time() - t0, 2)
    result["error_count"]  = errors
    result["pass"]         = errors == 0 and not result["write_errors"]

    sock.close()
    return result


def main():
    parser = argparse.ArgumentParser(description="TSI memory read/write test")
    parser.add_argument("--config", help="JSON config file")
    parser.add_argument("--base",   help="Base address (hex)")
    parser.add_argument("--size",   type=int, help="Size in bytes")
    parser.add_argument("--seed",   type=int, help="Random seed")
    parser.add_argument("--out",    help="Write JSON results to file")
    args = parser.parse_args()

    config = dict(DEFAULT_CONFIG)
    if args.config:
        with open(args.config) as f:
            config.update(json.load(f))
    if args.base:  config["base_addr"]  = args.base
    if args.size:  config["size_bytes"] = args.size
    if args.seed:  config["seed"]       = args.seed

    result = run_test(config)

    print("\n--- Result ---")
    print(json.dumps(result, indent=2))

    if args.out:
        with open(args.out, "w") as f:
            json.dump(result, f, indent=2)
        print(f"Saved to {args.out}")

    sys.exit(0 if result["pass"] else 1)


if __name__ == "__main__":
    main()
