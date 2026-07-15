#!/usr/bin/env python3
"""
eth_bw_test.py

Simple host-side UDP bandwidth probe for the FPGA Ethernet link.

By default this targets the FPGA TSI UDP port (7000) and sends TSI write
packets into the DDR fastpath region. Each packet is counted only after the
FPGA returns the standard 8-byte ACK payload.
"""

import argparse
import math
import socket
import struct
import sys
import time
import threading
from collections import deque

try:
    import plotext as plt
except ImportError:
    plt = None

ACK_MAGIC = 0xAC010001
DEFAULT_FPGA_IP = "192.168.1.10"
DEFAULT_FPGA_TSI_PORT = 7000
DEFAULT_FPGA_CTRL_PORT = 7001
DEFAULT_BIND_IP = "192.168.1.1"
CTRL_CMD_SET_FASTPATH_BASE_LO = 0x4650424C
CTRL_CMD_SET_FASTPATH_BASE_HI = 0x46504248
CTRL_CMD_SET_FASTPATH_SIZE_LO = 0x4650534C
CTRL_CMD_SET_FASTPATH_SIZE_HI = 0x46505348
CTRL_CMD_READ_MAX_OUTSTANDING = 0x4D584F54
FASTPATH_BASE = 0x80000000
FASTPATH_SIZE = 512 << 20
TSI_HEADER_BYTES = 20
CHIP_TEST_BASE = 0x00001000
_txn_id_counter = 1


def parse_ack(data):
    if not data or len(data) < 8:
        return None
    magic = struct.unpack(">I", data[0:4])[0]
    if magic != ACK_MAGIC:
        return None
    byte_count = struct.unpack(">H", data[4:6])[0]
    aux16 = struct.unpack(">H", data[6:8])[0] if len(data) >= 8 else 0
    cmd_word0 = struct.unpack(">I", data[8:12])[0] if len(data) >= 12 else None
    return magic, byte_count, cmd_word0, aux16


def next_tsi_txn_id():
    global _txn_id_counter
    txn_id = _txn_id_counter & 0x7FFFFFFF
    _txn_id_counter = (_txn_id_counter + 1) & 0x7FFFFFFF
    if _txn_id_counter == 0:
        _txn_id_counter = 1
    return txn_id


def make_tsi_cmd_word(is_write, txn_id=None):
    if txn_id is None:
        txn_id = next_tsi_txn_id()
    return ((txn_id & 0x7FFFFFFF) << 1) | (1 if is_write else 0)


def human_rate(bytes_per_s):
    bits_per_s = bytes_per_s * 8.0
    if bits_per_s >= 1e9:
        return f"{bits_per_s / 1e9:.3f} Gbps"
    if bits_per_s >= 1e6:
        return f"{bits_per_s / 1e6:.3f} Mbps"
    if bits_per_s >= 1e3:
        return f"{bits_per_s / 1e3:.3f} Kbps"
    return f"{bits_per_s:.1f} bps"


def summarize_latency(latencies_s):
    if not latencies_s:
        return None
    vals_ms = sorted(x * 1e3 for x in latencies_s)
    n = len(vals_ms)

    def pct(p):
        idx = min(max(int(math.ceil((p / 100.0) * n)) - 1, 0), n - 1)
        return vals_ms[idx]

    return {
        "min_ms": vals_ms[0],
        "avg_ms": sum(vals_ms) / n,
        "p50_ms": pct(50),
        "p90_ms": pct(90),
        "p99_ms": pct(99),
        "max_ms": vals_ms[-1],
    }


def render_latency_plot(latencies_s, title):
    if not latencies_s:
        return
    if plt is None:
        print("Latency plot: plotext not installed; skipping terminal plot")
        return

    x = list(range(1, len(latencies_s) + 1))
    y = [v * 1e3 for v in latencies_s]
    plt.clear_figure()
    plt.plotsize(100, 24)
    plt.title(title)
    plt.xlabel("Packet")
    plt.ylabel("RTT (ms)")
    plt.plot(x, y, marker="dot")
    plt.show()


def make_tsi_write_payload(addr, total_udp_payload_bytes, pattern_byte, txn_id=None):
    header_bytes = 5 * 4
    data_bytes = max(total_udp_payload_bytes - header_bytes, 4)
    data_bytes -= data_bytes % 4
    tsi_len = data_bytes // 4 - 1
    cmd_word0 = make_tsi_cmd_word(True, txn_id)
    words = [
        cmd_word0,
        addr & 0xFFFFFFFF,
        (addr >> 32) & 0xFFFFFFFF,
        tsi_len & 0xFFFFFFFF,
        (tsi_len >> 32) & 0xFFFFFFFF,
    ]
    data_word = ((pattern_byte & 0xFF) * 0x01010101) & 0xFFFFFFFF
    words.extend([data_word] * (data_bytes // 4))
    return b"".join(struct.pack("<I", w) for w in words), total_udp_payload_bytes, data_bytes, cmd_word0


def main():
    parser = argparse.ArgumentParser(description="Measure sustained UDP request/ACK bandwidth to the FPGA Ethernet link")
    parser.add_argument("--ip", default=DEFAULT_FPGA_IP, help=f"FPGA IP address (default: {DEFAULT_FPGA_IP})")
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
    parser.add_argument("--no-wait", action="store_true",
                        help="Send all packets back-to-back, then collect ACKs afterward")
    parser.add_argument("--credited", nargs="?", const=0, type=int, default=None,
                        help="Keep a limited number of TSI packets in flight; omit value to use FPGA-advertised MAX_OUTSTANDING, or pass an integer cap")
    parser.add_argument("--plot-latency", action="store_true",
                        help="In non-blast modes, render a per-packet RTT plot in the terminal with plotext")
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--ctrl-port", action="store_true",
                      help="Test raw UDP payload packets on control port 7001")
    mode.add_argument("--chip", action="store_true",
                      help="Test TSI write packets on data port 7000 below the fastpath base")
    mode.add_argument("--fastpath", action="store_true",
                      help="Test TSI write packets on data port 7000 in the DDR fastpath region (default)")
    args = parser.parse_args()

    if not (1 <= args.payload_bytes <= 65507):
        print("ERROR: --payload-bytes must be in 1..65507", file=sys.stderr)
        return 2
    if args.packets <= 0:
        print("ERROR: --packets must be > 0", file=sys.stderr)
        return 2
    if args.no_wait and args.credited is not None:
        print("ERROR: --no-wait and --credited are mutually exclusive", file=sys.stderr)
        return 2

    if args.ctrl_port:
        test_mode = "ctrl"
    elif args.chip:
        test_mode = "chip"
    elif args.fastpath:
        test_mode = "fastpath"
    elif args.no_wait:
        test_mode = "ctrl"
    else:
        test_mode = "fastpath"

    ctrl_dest = (args.ip, DEFAULT_FPGA_CTRL_PORT)
    if test_mode == "ctrl":
        dest = ctrl_dest
        payload = None
    else:
        dest = (args.ip, DEFAULT_FPGA_TSI_PORT)
        payload = None

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 4 * 1024 * 1024)
    sock.bind((args.bind_ip, 0))
    sock.settimeout(args.timeout)

    def send_and_wait(packet_payload, expect_bytes, expect_cmd_word0=None, check_timeout=True, require_id_match=False):
        send_t = time.perf_counter()
        sock.sendto(packet_payload, dest)
        while True:
            try:
                resp, _ = sock.recvfrom(4096)
            except socket.timeout:
                if check_timeout:
                    return False, None
                raise
            ack = parse_ack(resp)
            if ack is None:
                continue
            if require_id_match and expect_cmd_word0 is not None and ack[2] != expect_cmd_word0:
                got_id = "None" if ack[2] is None else f"0x{ack[2]:08X}"
                exp_id = f"0x{expect_cmd_word0:08X}"
                print(f"ACK ID mismatch: received {got_id}, expected {exp_id}", file=sys.stderr)
                continue
            if ack[1] != expect_bytes:
                print(f"ERROR: ACK byte count {ack[1]} != expected {expect_bytes}", file=sys.stderr)
                return False, None
            return True, (time.perf_counter() - send_t)

    def send_ctrl_and_wait(words):
        packet_payload = b"".join(struct.pack("<I", w) for w in words)
        sock.sendto(packet_payload, ctrl_dest)
        while True:
            try:
                resp, _ = sock.recvfrom(4096)
            except socket.timeout:
                return False
            ack = parse_ack(resp)
            if ack is None:
                continue
            return ack[1] == 8

    def send_ctrl_and_get_ack(words):
        packet_payload = b"".join(struct.pack("<I", w) for w in words)
        sock.sendto(packet_payload, ctrl_dest)
        while True:
            try:
                resp, _ = sock.recvfrom(4096)
            except socket.timeout:
                return None
            ack = parse_ack(resp)
            if ack is None:
                continue
            return ack

    def recv_one_ack(expected_acks):
        while True:
            try:
                resp, _ = sock.recvfrom(4096)
            except socket.timeout:
                return False, None, None
            ack = parse_ack(resp)
            if ack is None:
                continue
            cmd_word0 = ack[2]
            if cmd_word0 not in expected_acks:
                continue
            expect_bytes, send_t = expected_acks[cmd_word0]
            if ack[1] != expect_bytes:
                print(f"ERROR: ACK byte count {ack[1]} != expected {expect_bytes}", file=sys.stderr)
                return False, None, None
            latency_s = time.perf_counter() - send_t if send_t is not None else None
            return True, cmd_word0, latency_s

    def recv_one_ack_threaded(expected_acks, ack_queue, ack_cv, stop_evt, require_id_match=True):
        deadline = time.time() + args.timeout
        while time.time() < deadline:
            remaining = max(0.0, deadline - time.time())
            with ack_cv:
                if not ack_queue:
                    ack_cv.wait(timeout=remaining)
                while ack_queue:
                    ack = ack_queue.popleft()
                    cmd_word0 = ack[2]
                    if require_id_match:
                        if cmd_word0 not in expected_acks:
                            got_id = "None" if cmd_word0 is None else f"0x{cmd_word0:08X}"
                            if expected_acks:
                                oldest_expected = next(iter(expected_acks.keys()))
                                exp_id = f"0x{oldest_expected:08X}"
                            else:
                                exp_id = "<none pending>"
                            print(
                                f"ACK ID mismatch: received {got_id}, expected one of pending IDs "
                                f"(oldest {exp_id}), ack_queue depth now {len(ack_queue)}",
                                file=sys.stderr,
                            )
                            continue
                        matched_key = cmd_word0
                    else:
                        if not expected_acks:
                            continue
                        matched_key = next(iter(expected_acks.keys()))

                    expect_bytes, send_t = expected_acks[matched_key]
                    if ack[1] != expect_bytes:
                        print(f"ERROR: ACK byte count {ack[1]} != expected {expect_bytes}", file=sys.stderr)
                        return False, None, None
                    latency_s = time.perf_counter() - send_t if send_t is not None else None
                    return True, matched_key, latency_s
            if stop_evt.is_set():
                break
        return False, None, None

    def start_ack_rx_thread():
        ack_queue = deque()
        ack_cv = threading.Condition()
        stop_evt = threading.Event()

        def ack_rx_loop():
            while not stop_evt.is_set():
                try:
                    resp, _ = sock.recvfrom(4096)
                except socket.timeout:
                    continue
                ack = parse_ack(resp)
                if ack is None:
                    continue
                with ack_cv:
                    ack_queue.append(ack)
                    ack_cv.notify()

        ack_thread = threading.Thread(target=ack_rx_loop, name="eth-bw-ack-rx", daemon=True)
        ack_thread.start()
        return ack_queue, ack_cv, stop_evt, ack_thread

    def build_test_payload(packet_idx):
        if test_mode == "ctrl":
            cmd_word0 = make_tsi_cmd_word(True, packet_idx + 1)
            ctrl_payload = bytearray([args.pattern & 0xFF] * args.payload_bytes)
            ctrl_payload[0:4] = struct.pack("<I", cmd_word0)
            return bytes(ctrl_payload), args.payload_bytes, args.payload_bytes, cmd_word0

        addr_step = max(args.payload_bytes - TSI_HEADER_BYTES, 4)
        if test_mode == "chip":
            addr = CHIP_TEST_BASE + (packet_idx * addr_step)
        else:
            addr = FASTPATH_BASE + ((packet_idx * addr_step) % FASTPATH_SIZE)
        return make_tsi_write_payload(addr, args.payload_bytes, args.pattern)

    if test_mode == "fastpath":
        print("Testing DDR")
        print(f"Mode: fastpath TSI writes on port {DEFAULT_FPGA_TSI_PORT}")
        print(f"Assuming fastpath base = 0x{FASTPATH_BASE:08X}")
        print(f"Assuming fastpath size = 0x{FASTPATH_SIZE:08X} ({FASTPATH_SIZE >> 20} MB)")
        print(f"Programming fastpath over ctrl port {DEFAULT_FPGA_CTRL_PORT}")
        if not send_ctrl_and_wait([CTRL_CMD_SET_FASTPATH_BASE_LO, FASTPATH_BASE & 0xFFFFFFFF]):
            print("ERROR: failed to program fastpath base low", file=sys.stderr)
            return 1
        if not send_ctrl_and_wait([CTRL_CMD_SET_FASTPATH_BASE_HI, (FASTPATH_BASE >> 32) & 0xFFFFFFFF]):
            print("ERROR: failed to program fastpath base high", file=sys.stderr)
            return 1
        if not send_ctrl_and_wait([CTRL_CMD_SET_FASTPATH_SIZE_LO, FASTPATH_SIZE & 0xFFFFFFFF]):
            print("ERROR: failed to program fastpath size low", file=sys.stderr)
            return 1
        if not send_ctrl_and_wait([CTRL_CMD_SET_FASTPATH_SIZE_HI, (FASTPATH_SIZE >> 32) & 0xFFFFFFFF]):
            print("ERROR: failed to program fastpath size high", file=sys.stderr)
            return 1
    elif test_mode == "chip":
        print("Testing Chip")
        print(f"Mode: legacy/chip TSI writes on port {DEFAULT_FPGA_TSI_PORT}")
        print(f"Using addresses starting at 0x{CHIP_TEST_BASE:08X} (below fastpath base 0x{FASTPATH_BASE:08X})")
    else:
        if args.credited is not None:
            print("ERROR: --credited is only supported for TSI traffic on port 7000", file=sys.stderr)
            return 2
        print("Testing Ctrl Port")
        print(f"Mode: raw UDP payloads on port {DEFAULT_FPGA_CTRL_PORT}")

    credit_limit = None
    if args.credited is not None:
        ack = send_ctrl_and_get_ack([CTRL_CMD_READ_MAX_OUTSTANDING])
        if ack is None:
            print("ERROR: failed to read MAX_OUTSTANDING from FPGA", file=sys.stderr)
            return 1
        fpga_credit_limit = ack[3] & 0xFFFF
        credit_limit = fpga_credit_limit
        if args.credited not in (None, 0):
            if args.credited <= 0:
                print("ERROR: --credited value must be > 0", file=sys.stderr)
                return 2
            credit_limit = min(fpga_credit_limit, args.credited)
        if credit_limit <= 0:
            print(f"ERROR: invalid MAX_OUTSTANDING value {credit_limit}", file=sys.stderr)
            return 1
        if args.credited in (None, 0):
            print(f"Credited mode enabled: MAX_OUTSTANDING = {credit_limit}")
        else:
            print(f"Credited mode enabled: FPGA MAX_OUTSTANDING = {fpga_credit_limit}, host cap = {args.credited}, using {credit_limit}")

    for i in range(args.warmup):
        packet_payload, expect_bytes, _, expect_cmd_word0 = build_test_payload(i)
        ok_warmup, _ = send_and_wait(packet_payload, expect_bytes, expect_cmd_word0,
                                     require_id_match=(args.credited is not None))
        if not ok_warmup:
            print("ERROR: warmup timed out waiting for ACK", file=sys.stderr)
            return 1
    print("Warmup finished", flush=True)

    ok = 0
    expected_acks = {}
    ack_sizes = []
    tsi_data_sizes = []
    latencies_s = []
    send_phase_elapsed = None
    ack_phase_elapsed = None
    if args.no_wait:
        t0 = time.time()
        sent = 0
        ack_queue, ack_cv, stop_evt, ack_thread = start_ack_rx_thread()
        try:
            send_phase_start = time.time()
            for i in range(args.packets):
                packet_payload, expect_bytes, tsi_data_bytes, expect_cmd_word0 = build_test_payload(i)
                expected_acks[expect_cmd_word0] = (expect_bytes, None)
                sock.sendto(packet_payload, dest)
                ack_sizes.append(expect_bytes)
                tsi_data_sizes.append(tsi_data_bytes)
                sent += 1
            send_phase_elapsed = time.time() - send_phase_start

            ack_phase_start = time.time()
            for i in range(sent):
                got_ack, cmd_word0, _ = recv_one_ack_threaded(
                    expected_acks, ack_queue, ack_cv, stop_evt, require_id_match=False
                )
                if not got_ack:
                    print(f"ERROR: timed out waiting for ACK at packet {i}", file=sys.stderr)
                    break
                del expected_acks[cmd_word0]
                ok += 1
            ack_phase_elapsed = time.time() - ack_phase_start
        finally:
            stop_evt.set()
            with ack_cv:
                ack_cv.notify_all()
            ack_thread.join(timeout=0.2)
        elapsed = time.time() - t0
    elif args.credited is not None:
        t0 = time.time()
        sent = 0
        ack_queue, ack_cv, stop_evt, ack_thread = start_ack_rx_thread()
        try:
            while sent < args.packets or expected_acks:
                while sent < args.packets and len(expected_acks) < credit_limit:
                    packet_payload, expect_bytes, tsi_data_bytes, expect_cmd_word0 = build_test_payload(sent)
                    expected_acks[expect_cmd_word0] = (expect_bytes, time.perf_counter())
                    sock.sendto(packet_payload, dest)
                    ack_sizes.append(expect_bytes)
                    tsi_data_sizes.append(tsi_data_bytes)
                    sent += 1
                got_ack, cmd_word0, latency_s = recv_one_ack_threaded(expected_acks, ack_queue, ack_cv, stop_evt)
                if not got_ack:
                    print(f"ERROR: timed out waiting for ACK with {len(expected_acks)} outstanding", file=sys.stderr)
                    break
                del expected_acks[cmd_word0]
                if latency_s is not None:
                    latencies_s.append(latency_s)
                ok += 1
        finally:
            stop_evt.set()
            with ack_cv:
                ack_cv.notify_all()
            ack_thread.join(timeout=0.2)
        elapsed = time.time() - t0
    else:
        t0 = time.time()
        for i in range(args.packets):
            packet_payload, expect_bytes, tsi_data_bytes, expect_cmd_word0 = build_test_payload(i)
            got_ack, latency_s = send_and_wait(packet_payload, expect_bytes, expect_cmd_word0,
                                               require_id_match=False)
            if not got_ack:
                print(f"ERROR: timed out waiting for ACK at packet {i}", file=sys.stderr)
                break
            ack_sizes.append(expect_bytes)
            tsi_data_sizes.append(tsi_data_bytes)
            if latency_s is not None:
                latencies_s.append(latency_s)
            ok += 1
        elapsed = time.time() - t0
    sock.close()

    if ok == 0:
        print("No packets were ACKed", file=sys.stderr)
        return 1

    udp_payload_total = sum(ack_sizes[:ok])
    tsi_data_total = sum(tsi_data_sizes[:ok])
    pps = ok / elapsed if elapsed > 0 else 0.0
    udp_byte_rate = udp_payload_total / elapsed if elapsed > 0 else 0.0
    tsi_byte_rate = tsi_data_total / elapsed if elapsed > 0 else 0.0

    print(f"Source:      {args.bind_ip}")
    print(f"Destination: {dest[0]}:{dest[1]}")
    print(f"Payload:     {args.payload_bytes} bytes")
    if args.no_wait:
        ack_mode = "blast then drain ACKs"
    elif args.credited is not None:
        ack_mode = f"credit window ({credit_limit} outstanding)"
    else:
        ack_mode = "stop-and-wait ACK"
    print(f"ACK mode:    {ack_mode}")
    print(f"Packets:     {ok}/{args.packets} ACKed")
    print(f"Elapsed:     {elapsed:.3f} s")
    if args.no_wait and send_phase_elapsed is not None and ack_phase_elapsed is not None:
        print(f"Send phase:  {send_phase_elapsed:.3f} s")
        print(f"ACK phase:   {ack_phase_elapsed:.3f} s")
    print(f"Rate:        {pps:.1f} pkt/s")
    if test_mode in ("fastpath", "chip"):
        print(f"TSI header:  {TSI_HEADER_BYTES} bytes/packet")
        print(f"TSI data:    {args.payload_bytes - TSI_HEADER_BYTES} bytes/packet nominal")
        print(f"UDP Payload BW: {udp_byte_rate / 1e6:.3f} MB/s  ({human_rate(udp_byte_rate)})")
        print(f"TSI Data BW:    {tsi_byte_rate / 1e6:.3f} MB/s  ({human_rate(tsi_byte_rate)})")
    else:
        print(f"Payload BW:  {udp_byte_rate / 1e6:.3f} MB/s  ({human_rate(udp_byte_rate)})")

    if not args.no_wait:
        latency_stats = summarize_latency(latencies_s)
        if latency_stats is not None:
            print(
                "RTT:         "
                f"min {latency_stats['min_ms']:.3f} ms  "
                f"avg {latency_stats['avg_ms']:.3f} ms  "
                f"p50 {latency_stats['p50_ms']:.3f} ms  "
                f"p90 {latency_stats['p90_ms']:.3f} ms  "
                f"p99 {latency_stats['p99_ms']:.3f} ms  "
                f"max {latency_stats['max_ms']:.3f} ms"
            )
        if args.plot_latency:
            render_latency_plot(latencies_s, f"Per-packet RTT ({test_mode})")

    return 0 if ok == args.packets else 1


if __name__ == "__main__":
    sys.exit(main())
