#!/usr/bin/env python3
"""
udp_tsi_host.py

Host-side tool for communicating with a Chipyard SoC via the
fabric UDP-TSI bridge. Replaces uart_tsi.

This sends raw TSI commands over UDP and receives responses.
TSI is a simple read/write protocol over a serial interface.

TSI Command Format (as serial words):
  Word 0: Command header
    [63:48] = unused
    [47:32] = length (number of 64-bit data words - 1)
    [31:3]  = address >> 3 (64-bit aligned)
    [2:1]   = size (0=1B, 1=2B, 2=4B, 3=8B)
    [0]     = write (1) / read (0)

  For writes, the data words follow the command header.
  For reads, the response data comes back from the FPGA.

Usage:
    python udp_tsi_host.py ping                            # Check connectivity
    python udp_tsi_host.py load program.bin                # Load binary to memory
    python udp_tsi_host.py write 0x80000000 0xDEADBEEF     # Write word
    python udp_tsi_host.py read 0x80000000 64              # Read 64 bytes
"""

import socket
import struct
import os

try:
    from elftools.elf.elffile import ELFFile
    from elftools.elf.sections import SymbolTableSection
    _have_elftools = True
except ImportError:
    _have_elftools = False
import argparse
import time
import sys

try:
    import serial  # pyserial
except ImportError:
    serial = None

FPGA_IP      = "192.168.1.10"
FPGA_PORT    = 7000
TIMEOUT      = 2.0
POLL_SLEEP   = 0.01   # seconds between tohost polls
CFLUSH_ADDR  = 0x02010200  # Cache flush control register (InclusiveCache flush64 @ cache-controller base 0x02010000 + 0x200)
CLINT_BASE   = 0x02000000

ACK_MAGIC = 0xAC010001  # Must match ACK_PAYLOAD parameter in Verilog
CTRL_CMD_READ_WATCHDOG = 0x57444F47  # "WDOG" - must match udp_payload_to_tsi_serial.v
CTRL_CMD_SET_WATCHDOG_TIMEOUT = 0x57444F54  # "WDOT" - must match udp_payload_to_tsi_serial.v
CTRL_CMD_SET_SELECT_VALUE   = 0x53454C56  # "SELV" - must match udp_payload_to_tsi_serial.v
CTRL_CMD_SET_CHIP_RESET     = 0x52535443  # "RSTC" - must match udp_payload_to_tsi_serial.v
CTRL_CMD_SET_FASTPATH_BASE_LO = 0x4650424C  # "FPBL" - must match udp_payload_to_tsi_serial.v
CTRL_CMD_SET_FASTPATH_BASE_HI = 0x46504248  # "FPBH" - must match udp_payload_to_tsi_serial.v
CTRL_CMD_SET_FASTPATH_SIZE_LO = 0x4650534C  # "FPSL" - must match udp_payload_to_tsi_serial.v
CTRL_CMD_SET_FASTPATH_SIZE_HI = 0x46505348  # "FPSH" - must match udp_payload_to_tsi_serial.v

FASTPATH_BASE = 0x80000000        # DDR base address
FASTPATH_SIZE = 512 << 20         # DDR size (512 MB)

UART_ADDR_MDIO = 0x02
MDIO_OP_WRITE = 0x01
MDIO_OP_READ  = 0x02

class FESVR_SYSCALLS:
    write = 64
    exit  = 93

def _align4(n):
    return (n + 3) & ~3

def make_tsi_write_cmd(addr, data_bytes):
    """Build a TSI write command matching pyuartsi protocol:
       cmd(32) | addr(64) | len(64) | data...
       len = number of 32-bit words - 1
    """
    # pad to 4-byte boundary
    if len(data_bytes) % 4 != 0:
        data_bytes = data_bytes + b'\xff' * (4 - len(data_bytes) % 4)
    tsi_len = len(data_bytes) // 4 - 1
    words = [
        1,                              # cmd = write
        addr & 0xFFFFFFFF,              # addr[31:0]
        (addr >> 32) & 0xFFFFFFFF,      # addr[63:32]
        tsi_len & 0xFFFFFFFF,           # len[31:0]
        (tsi_len >> 32) & 0xFFFFFFFF,   # len[63:32]
    ]
    for i in range(0, len(data_bytes), 4):
        words.append(struct.unpack_from('<I', data_bytes, i)[0])
    return words

def make_tsi_read_cmd(addr, num_bytes):
    """Build a TSI read command matching pyuartsi protocol:
       cmd(32) | addr(64) | len(64)
       len = number of 32-bit words - 1
    """
    tsi_len = max(_align4(num_bytes) // 4 - 1, 0)
    words = [
        0,                              # cmd = read
        addr & 0xFFFFFFFF,              # addr[31:0]
        (addr >> 32) & 0xFFFFFFFF,      # addr[63:32]
        tsi_len & 0xFFFFFFFF,           # len[31:0]
        (tsi_len >> 32) & 0xFFFFFFFF,   # len[63:32]
    ]
    return words, _align4(num_bytes) // 4  # also return expected response word count

def flush_socket(sock):
    """Drain any stale packets from the receive buffer."""
    sock.settimeout(0)
    try:
        while True:
            sock.recvfrom(4096)
    except (socket.timeout, BlockingIOError):
        pass

def send_tsi_words(sock, words, dest):
    """Flush stale packets, then send TSI words as UDP payload."""
    flush_socket(sock)
    payload = b''.join(struct.pack('<I', w) for w in words)
    sock.sendto(payload, dest)

def recv_response(sock, timeout=TIMEOUT):
    """Receive a UDP response from the FPGA."""
    sock.settimeout(timeout)
    try:
        data, addr = sock.recvfrom(4096)
        return data
    except socket.timeout:
        return None

def parse_ack(data):
    """Parse an ACK response. Returns (magic, byte_count) or None."""
    if not data or len(data) < 8:
        return None
    magic = struct.unpack('>I', data[0:4])[0]
    if magic == ACK_MAGIC:
        byte_count = struct.unpack('>H', data[4:6])[0]
        return (magic, byte_count)
    return None

def load_binary(sock, dest, filepath, base_addr=0x80000000, chunk_delay=0.1):
    """Load a binary file into SoC memory via TSI write commands."""
    with open(filepath, 'rb') as f:
        binary = f.read()

    if len(binary) % 4 != 0:
        binary += b'\x00' * (4 - len(binary) % 4)

    chunk_size = 512
    total = len(binary)
    sent = 0

    print(f"Loading {filepath} ({total} bytes) to 0x{base_addr:08X}")
    t0 = time.time()

    while sent < total:
        end = min(sent + chunk_size, total)
        chunk = binary[sent:end]
        addr = base_addr + sent

        words = make_tsi_write_cmd(addr, chunk)
        send_tsi_words(sock, words, dest)

        resp = recv_response(sock)
        if resp is None:
            print(f"  WARNING: No ACK at 0x{addr:08X}, retrying...")
            send_tsi_words(sock, words, dest)
            resp = recv_response(sock)
            if resp is None:
                print(f"  ERROR: No ACK after retry at 0x{addr:08X}")
                return False

        sent += len(chunk)
        if chunk_delay and sent < total:
            time.sleep(chunk_delay)
        if (sent % (16 * 1024)) < chunk_size:
            pct = 100.0 * sent / total
            elapsed = time.time() - t0
            rate = (sent / elapsed) / 1024 if elapsed > 0 else 0
            print(f"  {sent // 1024} KB / {total // 1024} KB "
                  f"({pct:.0f}%) [{rate:.0f} KB/s]")

    elapsed = time.time() - t0
    rate = (total / elapsed) / 1024 if elapsed > 0 else 0
    print(f"Load complete: {total} bytes in {elapsed:.1f}s ({rate:.0f} KB/s)")
    return True

def write_word(sock, dest, addr, value):
    """Write a single 64-bit value."""
    data = struct.pack('<Q', value)
    words = make_tsi_write_cmd(addr, data)
    send_tsi_words(sock, words, dest)
    resp = recv_response(sock)
    if resp:
        ack = parse_ack(resp)
        if ack:
            print(f"Write 0x{value:016X} -> 0x{addr:08X} [ACK OK]")
        else:
            print(f"Write 0x{value:016X} -> 0x{addr:08X} [ACK: {resp.hex()}]")
    else:
        print(f"Write 0x{value:016X} -> 0x{addr:08X} [NO ACK]")
    # ACK confirms packet receipt, not write completion through TileLink/DRAM.
    time.sleep(0.05)

def read_words(sock, dest, addr, num_bytes, cflush_addr=CFLUSH_ADDR):
    """Read memory and print contents."""
    if cflush_addr:
        flush_cache_lines(sock, dest, addr, _align4(num_bytes), cflush_addr)

    words, expected_words = make_tsi_read_cmd(addr, num_bytes)
    send_tsi_words(sock, words, dest)

    data = b''
    got_ack = False
    timeout = TIMEOUT
    while len(data) < expected_words * 4:
        resp = recv_response(sock, timeout=timeout)
        if resp is None:
            break
        timeout = 1.0
        if parse_ack(resp):
            got_ack = True
            continue
        data += resp

    if not got_ack and not data:
        print("No response received")
        return
    if not data:
        print("Only ACK received, no read data")
        return

    print(f"Read {len(data)} bytes from 0x{addr:08X}:")
    for i in range(0, min(len(data), num_bytes), 4):
        if i + 4 <= len(data):
            w = struct.unpack_from('<I', data, i)[0]
            if (i % 16) == 0:
                print(f"  0x{addr + i:08X}:", end="")
            print(f" {w:08X}", end="")
            if (i % 16) == 12:
                print()
    print()

def ping_fpga(sock, dest):
    """Send a minimal packet to check connectivity."""
    sock.sendto(b'\x00' * 8, dest)
    print(sock.getsockname())
    resp = recv_response(sock, timeout=TIMEOUT)
    if resp:
        print(f"FPGA responded: {resp.hex()} ({len(resp)} bytes)")
        ack = parse_ack(resp)
        if ack:
            print(f"  ACK magic: 0x{ack[0]:08X}, byte count: {ack[1]}")
        return True
    else:
        print("No response from FPGA — check cable, IP, and bitstream")
        return False

def read_watchdog(sock, dest):
    """Query the udp_payload_to_tsi_serial RX watchdog status via the ctrl port.

    Sends CTRL_CMD_READ_WATCHDOG to UDP_PORT+1; the FPGA's ACK response
    carries the watchdog sticky bit + saturating fire count in bytes[6:7].
    """
    send_tsi_words(sock, [CTRL_CMD_READ_WATCHDOG], dest)
    resp = recv_response(sock)
    if resp is None or len(resp) < 8:
        print("No response from FPGA")
        return None
    ack = parse_ack(resp)
    if not ack:
        print(f"Unexpected response: {resp.hex()}")
        return None
    fired = bool(resp[6] & 0x80)
    fire_cnt = ((resp[6] & 0x7F) << 8) | resp[7]
    print(f"Watchdog fired: {fired}  fire count: {fire_cnt}")
    return fired, fire_cnt

def select_chip(sock, dest, chip_id):
    """Select which chip the UDP-TSI bridge talks to, by chip id.

    Sends [CTRL_CMD_SET_SELECT_VALUE, chip_id] to UDP_PORT+1. The FPGA latches
    chip_id[0] as the absolute chip-select value and holds it (recency mux)
    until the board switch (io_select) is toggled. chip_id 0 = chip 0, 1 = chip 1.
    """
    value = int(chip_id) & 0x1
    send_tsi_words(sock, [CTRL_CMD_SET_SELECT_VALUE, value], dest)
    resp = recv_response(sock)
    if resp is None or len(resp) < 8:
        print("No response from FPGA")
        return None
    ack = parse_ack(resp)
    if not ack:
        print(f"Unexpected response: {resp.hex()}")
        return None
    print(f"Selected chip {value}")
    return True

def set_chip_reset(sock, dest, mask):
    """Drive chip reset via the ctrl port.

    mask[0] = chip 0, mask[1] = chip 1.  1 = held in reset, 0 = running.
    Sends [CTRL_CMD_SET_CHIP_RESET, mask] to UDP_PORT+1.
    """
    send_tsi_words(sock, [CTRL_CMD_SET_CHIP_RESET, int(mask) & 0x3], dest)

def pulse_chip_reset(sock, dest, mask=0x3, hold_s=0.01):
    """Assert then deassert chip reset for the chips indicated by mask."""
    set_chip_reset(sock, dest, mask)
    time.sleep(hold_s)
    set_chip_reset(sock, dest, 0)
    time.sleep(hold_s)
    chips = [i for i in range(2) if mask & (1 << i)]
    print(f"Chip reset pulsed for chip(s) {chips}", flush=True)

def set_fastpath(sock, dest, base=FASTPATH_BASE, size=FASTPATH_SIZE):
    """Program the TSI fastpath window on the FPGA via the ctrl port.

    Sends four 2-word ctrl packets to set the 64-bit base and size registers
    in udp_payload_to_tsi_serial (FPBL/FPBH for base, FPSL/FPSH for size).
    """
    send_tsi_words(sock, [CTRL_CMD_SET_FASTPATH_BASE_LO,  base        & 0xFFFFFFFF], dest)
    send_tsi_words(sock, [CTRL_CMD_SET_FASTPATH_BASE_HI, (base >> 32) & 0xFFFFFFFF], dest)
    send_tsi_words(sock, [CTRL_CMD_SET_FASTPATH_SIZE_LO,  size        & 0xFFFFFFFF], dest)
    send_tsi_words(sock, [CTRL_CMD_SET_FASTPATH_SIZE_HI, (size >> 32) & 0xFFFFFFFF], dest)
    print(f"Fastpath: base=0x{base:016X}  size=0x{size:016X} ({size >> 20} MB)", flush=True)

def set_watchdog_timeout(sock, dest, cycles):
    """Set the udp_payload_to_tsi_serial RX watchdog timeout via the ctrl port.

    Sends [CTRL_CMD_SET_WATCHDOG_TIMEOUT, cycles] to UDP_PORT+1. `cycles` is
    treated as an unsigned value and truncated to 32 bits.
    """
    cycles = cycles & 0xFFFFFFFF
    send_tsi_words(sock, [CTRL_CMD_SET_WATCHDOG_TIMEOUT, cycles], dest)
    resp = recv_response(sock)
    if resp is None or len(resp) < 8:
        print("No response from FPGA")
        return None
    ack = parse_ack(resp)
    if not ack:
        print(f"Unexpected response: {resp.hex()}")
        return None
    print(f"Watchdog timeout set to {cycles} cycles")
    return True

def require_serial():
    if serial is None:
        print("ERROR: pyserial is not installed. Install with: pip install pyserial")
        return False
    return True

def open_uart(port, baud, timeout=0.2):
    if not require_serial():
        return None
    try:
        return serial.Serial(port=port, baudrate=baud, timeout=timeout)
    except Exception as e:
        print(f"ERROR: Failed to open UART {port}: {e}")
        return None

def mdio_send_cmd(uart, opcode, reg_addr, data=0):
    uart.reset_input_buffer()
    uart.write(bytes([UART_ADDR_MDIO]))
    uart.write(bytes([opcode & 0x3]))
    uart.write(bytes([reg_addr & 0x1F]))
    uart.write(bytes([(data >> 8) & 0xFF]))
    uart.write(bytes([data & 0xFF]))
    uart.flush()

def mdio_wait_for_ack(uart, opcode, reg_addr, timeout=1.0):
    """Wait for 3-byte ACK frame: A2, opcode[1:0], reg[4:0]."""
    deadline = time.time() + timeout
    buf = bytearray()
    while time.time() < deadline:
        chunk = uart.read(64)
        if chunk:
            buf.extend(chunk)
            for i in range(0, len(buf)-2):
                if buf[i] == 0xA2 and (buf[i+1] & 0x3) == (opcode & 0x3) and (buf[i+2] & 0x1F) == (reg_addr & 0x1F):
                    return True
    return False

def mdio_wait_for_read_data(uart, timeout=1.0):
    """Wait for 3-byte read-data frame: B2, data_lo, data_hi."""
    deadline = time.time() + timeout
    buf = bytearray()
    while time.time() < deadline:
        chunk = uart.read(64)
        if chunk:
            buf.extend(chunk)
            for i in range(0, len(buf)-2):
                if buf[i] == 0xB2:
                    return buf[i+1] | (buf[i+2] << 8)
    return None

def mdio_write(port, baud, reg_addr, data):
    uart = open_uart(port, baud)
    if uart is None:
        return 1
    try:
        mdio_send_cmd(uart, MDIO_OP_WRITE, reg_addr, data)
        if mdio_wait_for_ack(uart, MDIO_OP_WRITE, reg_addr):
            print(f"MDIO write: reg 0x{reg_addr:02X} <= 0x{data:04X} [ACK]")
            return 0
        print(f"MDIO write: reg 0x{reg_addr:02X} <= 0x{data:04X} [NO ACK]")
        return 1
    finally:
        uart.close()

def mdio_read(port, baud, reg_addr, quiet=False):
    uart = open_uart(port, baud)
    if uart is None:
        return None
    try:
        mdio_send_cmd(uart, MDIO_OP_READ, reg_addr, 0)
        deadline = time.time() + 1.0
        buf = bytearray()
        got_ack = False
        data = None

        # Parse ACK (A2 op reg) and DATA (B2 lo hi) from the same stream buffer.
        # This avoids losing read data when ACK+DATA arrive in one uart.read() call.
        while time.time() < deadline and (not got_ack or data is None):
            chunk = uart.read(64)
            if not chunk:
                continue

            buf.extend(chunk)
            i = 0
            while i <= len(buf) - 3:
                b0, b1, b2 = buf[i], buf[i+1], buf[i+2]
                if b0 == 0xA2:
                    if (b1 & 0x3) == (MDIO_OP_READ & 0x3) and (b2 & 0x1F) == (reg_addr & 0x1F):
                        got_ack = True
                    i += 3
                elif b0 == 0xB2:
                    data = b1 | (b2 << 8)
                    i += 3
                else:
                    i += 1

            if i > 0:
                del buf[:i]

        if not got_ack:
            if not quiet:
                print(f"MDIO read: reg 0x{reg_addr:02X} [NO ACK]")
            return None
        if data is None:
            if not quiet:
                print(f"MDIO read: reg 0x{reg_addr:02X} [NO DATA]")
            return None
        if not quiet:
            print(f"MDIO read: reg 0x{reg_addr:02X} = 0x{data:04X}")
        return data
    finally:
        uart.close()

def mdio_check_link(port, baud):
    # Clause 22 BMSR (reg 1), bit[2] = link status; read twice (latch-low behavior).
    # Do raw read here: collect response bytes first, then parse in one pass.
    uart = open_uart(port, baud)
    if uart is None:
        return 1

    def read_bmsr_once(timeout=2.0):
        mdio_send_cmd(uart, MDIO_OP_READ, 0x11, 0)
        deadline = time.time() + timeout
        buf = bytearray()

        # Collect a full response blob before parsing.
        # Typical response is ACK(3) + DATA(3), but we accept >=5 bytes per request.
        while time.time() < deadline and len(buf) < 6:
            chunk = uart.read(64)
            if chunk:
                buf.extend(chunk)

        return buf


    try:
        buf = read_bmsr_once()
        #buf.extend(read_bmsr_once())

        if len(buf) < 6:
            print(f"mdio_check_link timeout (got {len(buf)} bytes)")
            print(f"mdio_check_link rx buf: {buf.hex()}")
            print("MDIO link check failed (unable to read BMSR)")
            uart.close()
            return 1

        got_ack = False
        data = None
        #for i in range(0, max(0, len(buf)-2)):
        #for i in range(0, 6):
        i=0
        b0, b1, b2 = buf[i], buf[i+1], buf[i+2]
        if b0 == 0xA2 and (b1 & 0x3) == (MDIO_OP_READ & 0x3) and (b2 & 0x1F) == 0x11:
            got_ack = True
            print("MDIO link check ack received")
            if buf[i+3] == 0xB2:
                print("MDIO link check received data")
                data = buf[i+5] | (buf[i+4] << 8)
            else:
                print("MDIO link check not followed by data")
                return 1
        else:
            print("MDIO link check ack no received")
            return 1

       # if not got_ack or data is None:
       #     return None, bytes(buf)
       # return data, bytes(buf)
    finally:
        uart.close()

    #all_buf = b1 + b2

    print(f"mdio_check_link rx buf: {buf.hex()}")

    #if v1 is None and v2 is None:
    #    print("MDIO link check failed (unable to read BMSR)")
    #    return 1

    # Prefer second read (latch-low behavior), fall back to first if needed.
    link_word = data #v2 if v2 is not None else v1

    # Parse PHYSR fields (Realtek):
    #   bit11: link
    #   bit13: duplex (1=full, 0=half)
    #   bits15:14: speed (00=10M, 01=100M, 10=1000M)
    speed_sel = (link_word >> 14) & 0x3
    if speed_sel == 2:
        speed_str = "1000Mbps"
    elif speed_sel == 1:
        speed_str = "100Mbps"
    elif speed_sel == 0:
        speed_str = "10Mbps"
    else:
        speed_str = "Reserved"
    duplex_str = "Full" if (link_word & (1 << 13)) else "Half"

    link_up = bool(link_word & (1 << 11))
    print(f"BMSR #1: {'0x%04X' % data}")
    print(f"Speed: {speed_str}")
    print(f"Duplex: {duplex_str}")
    #print(f"BMSR #2: {('0x%04X' % v2) if v2 is not None else 'None'}")
    print(f"Link: {'UP' if link_up else 'DOWN'}")
    return 0 if link_up else 2

def read_word64(sock, dest, addr):
    """Read a single 64-bit word, return as integer."""
    words, expected = make_tsi_read_cmd(addr, 8)
    send_tsi_words(sock, words, dest)
    data = b''
    timeout = TIMEOUT
    while len(data) < expected * 4:
        resp = recv_response(sock, timeout=timeout)
        if resp is None:
            break
        timeout = 1.0
        if parse_ack(resp):
            continue
        data += resp
    if len(data) < 8:
        return None
    return struct.unpack('<Q', data[:8])[0]


def write_word64(sock, dest, addr, value):
    """Write a single 64-bit word silently."""
    data = struct.pack('<Q', value)
    words = make_tsi_write_cmd(addr, data)
    send_tsi_words(sock, words, dest)
    resp = recv_response(sock)
    return resp is not None


def flush_cache_lines(sock, dest, addr, size, cflush_addr=CFLUSH_ADDR):
    """Flush chip cache lines covering [addr, addr+size) back to DRAM (mirrors pyuartsi)."""
    if not cflush_addr:
        return
    cblock = 64
    base = addr & ~(cblock - 1)
    while base < addr + size:
        write_word64(sock, dest, cflush_addr, base)
        base += cblock


def read_bytes(sock, dest, addr, size, cflush_addr=CFLUSH_ADDR, chunk_size=512, chunk_delay=2.4):
    """Read `size` bytes from addr, with cache flush (mirrors pyuartsi read_bytes).

    Large reads are split into chunk_size-byte read commands, each fully
    drained before the next is issued. This caps the number of response
    packets the FPGA streams back per command, throttling the burst so it
    can't exceed ~chunk_size bytes in flight at once. chunk_delay adds a
    pause between chunks to give the FPGA time to settle.
    """
    if cflush_addr:
        flush_cache_lines(sock, dest, addr, _align4(size), cflush_addr)

    data = b''
    sent = 0
    while sent < size:
        this_size = min(chunk_size, size - sent)
        words, expected_words = make_tsi_read_cmd(addr + sent, this_size)
        send_tsi_words(sock, words, dest)
        chunk_data = b''
        timeout = TIMEOUT
        while len(chunk_data) < expected_words * 4:
            resp = recv_response(sock, timeout=timeout)
            if resp is None:
                break
            timeout = 1.0
            if parse_ack(resp):
                continue
            chunk_data += resp
        data += chunk_data
        if len(chunk_data) < expected_words * 4:
            break
        sent += this_size
        if chunk_delay and sent < size:
            time.sleep(chunk_delay)
    return data[:size]


def read_longword(sock, dest, addr, cflush_addr=CFLUSH_ADDR):
    """Read a 64-bit word with cache flush (mirrors pyuartsi read_longword)."""
    buf = read_bytes(sock, dest, addr, 8, cflush_addr)
    if len(buf) < 8:
        return None
    return struct.unpack('<Q', buf[:8])[0]


def write_longword_cached(sock, dest, addr, value, cflush_addr=CFLUSH_ADDR):
    """Flush cache line then write 64-bit word (mirrors pyuartsi: invalidate L1 before DDR write)."""
    if cflush_addr:
        flush_cache_lines(sock, dest, addr, 8, cflush_addr)
    write_word64(sock, dest, addr, value)


def get_symbol_addresses(filename, *symbol_names):
    """Return addresses for named symbols from ELF symbol table (mirrors pyuartsi --use_symbols)."""
    if not _have_elftools:
        raise RuntimeError("pyelftools not installed: pip install pyelftools")
    addrs = {name: None for name in symbol_names}
    with open(filename, 'rb') as f:
        elf = ELFFile(f)
        for section in elf.iter_sections():
            if not hasattr(section, 'iter_symbols'):
                continue
            for sym in section.iter_symbols():
                if sym.name in addrs and sym['st_value'] != 0:
                    addrs[sym.name] = sym['st_value']
    return tuple(addrs[name] for name in symbol_names)


def get_htif_base(filename):
    """Return htif_base from .htif section, defaulting to 0x80000000 (mirrors pyuartsi)."""
    if not _have_elftools:
        raise RuntimeError("pyelftools not installed: pip install pyelftools")
    htif_base = 0x80000000
    with open(filename, 'rb') as f:
        elf = ELFFile(f)
        for section in elf.iter_sections():
            if section.name == '.htif':
                htif_base = section['sh_addr']
                break
    return htif_base


def load_elf(sock, dest, filename, chunk_size=1400, chunk_delay=0.0001):
    """Load all SHT_PROGBITS sections from an ELF file."""
    if not _have_elftools:
        raise RuntimeError("pyelftools not installed: pip install pyelftools")
    with open(filename, 'rb') as f:
        elf = ELFFile(f)
        for section in elf.iter_sections():
            if section['sh_type'] != 'SHT_PROGBITS':
                continue
            if section['sh_addr'] == 0:
                continue
            data = section.data()
            if not data:
                continue
            addr = section['sh_addr']
            total = len(data)
            print(f"Loading {section.name} ({total} bytes) -> 0x{addr:08X}")
            sent = 0
            t_start = time.time()
            while sent < total:
                chunk = data[sent:sent + chunk_size]
                chunk_len = len(chunk)
                if chunk_len % 4 != 0:
                    chunk = chunk + b'\x00' * (4 - chunk_len % 4)
                words = make_tsi_write_cmd(addr + sent, chunk)

                ack_ok = False
                for attempt in range(1, 4):
                    send_tsi_words(sock, words, dest)
                    resp = recv_response(sock)
                    if resp is not None:
                        ack_ok = True
                        break
                    if attempt < 3:
                        print(f"\n\033[31m  WARNING: no ACK at 0x{addr+sent:08X}, retry attempt {attempt}/3\033[0m")

                if ack_ok and attempt > 1:
                    print(f"\033[32m  Retry succeeded at 0x{addr+sent:08X} on attempt {attempt}/3\033[0m")

                if not ack_ok:
                    print(f"\n  ERROR: no ACK after 3 attempts at 0x{addr+sent:08X}")
                    sys.exit(1)

                sent += chunk_len
                elapsed = time.time() - t_start
                speed = sent / elapsed if elapsed > 0 else 0
                if speed >= 1e6:
                    speed_str = f"{speed/1e6:.2f} MB/s"
                else:
                    speed_str = f"{speed/1e3:.1f} KB/s"
                pct = sent * 100 // total
                bar = '#' * (pct // 5) + '-' * (20 - pct // 5)
                print(f"\r\033[34m  [{bar}] {pct:3d}%  {sent}/{total} B  {speed_str}\033[0m", end='', flush=True)
                if chunk_delay and sent < total:
                    time.sleep(chunk_delay)
            print(flush=True)
    print("ELF load complete.")


def verify_elf_load(sock, dest, filename, cflush_addr=CFLUSH_ADDR):
    """Read back every PROGBITS section of `filename` via TSI and compare
    against the ELF's contents. Returns True if everything matches."""
    if not _have_elftools:
        raise RuntimeError("pyelftools not installed: pip install pyelftools")

    print("\nVerifying loaded sections against ELF ...")
    ok = True
    with open(filename, 'rb') as f:
        elf = ELFFile(f)
        for section in elf.iter_sections():
            if section['sh_type'] != 'SHT_PROGBITS':
                continue
            if section['sh_addr'] == 0:
                continue
            expected = section.data()
            if not expected:
                continue

            addr = section['sh_addr']
            size = len(expected)
            print(f"  Section {section.name}: 0x{addr:08X}, {size} bytes")

            actual = read_bytes(sock, dest, addr, size, cflush_addr=cflush_addr)

            if actual == expected:
                print(f"    OK: matches ELF contents")
                continue

            ok = False
            if len(actual) != len(expected):
                print(f"    MISMATCH: read {len(actual)} bytes, expected {len(expected)}")

            n = min(len(actual), len(expected))
            first_diff = None
            ndiffs = 0
            for i in range(n):
                if actual[i] != expected[i]:
                    ndiffs += 1
                    if first_diff is None:
                        first_diff = i

            print(f"    MISMATCH: {ndiffs}/{n} bytes differ")
            if first_diff is not None:
                off = first_diff
                ctx = 16
                lo = max(0, off - ctx)
                hi = min(n, off + ctx)
                print(f"    First diff at offset 0x{off:X} (addr 0x{addr+off:08X}):")
                print(f"      expected: {expected[lo:hi].hex()}")
                print(f"      actual:   {actual[lo:hi].hex()}")

    print("PASS: loaded sections match ELF" if ok else "FAIL: mismatches found, see above")
    return ok


def run_elf(sock, dest, filename, cflush_addr=CFLUSH_ADDR, use_symbols=True, verify=False,
            reset_mask=0x3, non_tsi_dest=None):
    """Load ELF and run with HTIF fesvr — mirrors pyuartsi --load --hart0_msip --fesvr."""
    if not _have_elftools:
        raise RuntimeError("pyelftools not installed: pip install pyelftools")

    # Assert (and HOLD) chip reset before loading. The chip must stay in reset
    # for the entire ELF load so a stale program already in DRAM (e.g. the
    # previous, already-finished run) cannot execute before the new binary is
    # in place. The chip is released only after the load completes (below),
    # then kicked via MSIP. Previously this PULSED reset (assert+release)
    # before the load, leaving the chip running during the load.
    held_reset = bool(reset_mask) and non_tsi_dest is not None
    if held_reset:
        set_chip_reset(sock, non_tsi_dest, mask=reset_mask)   # assert, hold
        print(f"Chip(s) held in reset (mask=0x{reset_mask:X}) for load", flush=True)

    # Program fastpath window so the FPGA router knows the DDR address range.
    if non_tsi_dest is not None:
        set_fastpath(sock, non_tsi_dest)

    # Resolve tohost/fromhost — prefer symbol table (default), fall back to .htif section
    if use_symbols:
        tohost_addr, fromhost_addr = get_symbol_addresses(filename, 'tohost', 'fromhost')
        if tohost_addr is not None:
            print(f"tohost=0x{tohost_addr:08X}  fromhost=0x{fromhost_addr:08X} (from symbols)")
        else:
            use_symbols = False
    if not use_symbols:
        htif_base     = get_htif_base(filename)
        tohost_addr   = htif_base
        fromhost_addr = htif_base + 8
        print(f"tohost=0x{tohost_addr:08X}  fromhost=0x{fromhost_addr:08X} (from .htif section)")

    load_elf(sock, dest, filename)

    if verify:
        if not verify_elf_load(sock, dest, filename, cflush_addr):
            print("Aborting run: loaded sections do not match ELF.")
            return 1

    # ELF is now fully loaded — release the chip(s) from reset so the bootrom
    # runs and waits for the MSIP kick below.
    if held_reset:
        set_chip_reset(sock, non_tsi_dest, mask=0)
        print("Chip(s) released from reset (load complete)", flush=True)

    # Write 0 to DDR tohost without flushing — chip may have already written tohost=P
    # to L1 dirty before the MSIP kick.  A flush here would invalidate L1, and since
    # htif_syscall only writes tohost once (then spins on fromhost), the chip would
    # never re-write it, leaving DDR[tohost]=0 forever.  No-flush preserves the dirty
    # L1 value; the first poll's cflush will ProbeAckData it back to DDR.
    write_word64(sock, dest, tohost_addr, 0)
    print("Write 0 to tohost (no flush)", flush=True)

    # Kick hart 0 via MSIP (mirrors pyuartsi --hart0_msip)
    # Write boot address to BOOTADDR_REG (0x1000) so the bootrom knows where to jump after MSIP.
    ok = write_word64(sock, dest, 0x1000, 0x80000000)
    print(f"Boot address (0x80000000) written to 0x1000 (ack={'ok' if ok else 'MISSING — chip did not respond'})", flush=True)
    readback = read_word64(sock, dest, 0x1000)
    if readback == 0x80000000:
        print("0x1000 readback 0x80000000 — confirmed chip BootAddrReg", flush=True)
    else:
        val_str = f"0x{readback:016X}" if readback is not None else "None"
        print(f"0x1000 readback {val_str} — unexpected value, may not be chip register", flush=True)
    input("Press Enter to kick hart 0 via MSIP...")
    words = make_tsi_write_cmd(CLINT_BASE, struct.pack('<I', 0x01))
    send_tsi_words(sock, words, dest)
    recv_response(sock)
    print("Hart 0 MSIP written", flush=True)

    print("Proxy FESVR started.", flush=True)
    _polls_per_warn = int(10 / POLL_SLEEP)  # print warning once per ~10 seconds
    _empty_polls = 0
    try:
        while True:
            time.sleep(POLL_SLEEP)
            raw = read_longword(sock, dest, tohost_addr, cflush_addr=0)        # no flush
            request_ptr = read_longword(sock, dest, tohost_addr, cflush_addr)  # with flush

            if request_ptr is None or request_ptr == 0:
                _empty_polls += 1
                if _empty_polls % _polls_per_warn == 0:
                    print(f"tohost DDR_raw=0x{raw or 0:016X}  after_flush=0x{request_ptr or 0:016X} — empty after {_empty_polls} polls, polling again", flush=True)
                continue

            # Known force-exit values (matches pyuartsi)
            if request_ptr in (1, 0x10000, 0x13030):
                print("DUT forcefully exited")
                return 0

            if request_ptr == 3:
                print("tohost=3 (malloc), ignoring and polling again", flush=True)
                continue  # malloc — ignore

            if request_ptr < 0x80000000:
                print(f"Invalid request pointer: {request_ptr:#x}")
                continue

            # Read syscall packet: syscall_id, a0, a1, a2 (4 x uint64 = 32 bytes)
            request_buffer = read_bytes(sock, dest, request_ptr, 8 * 4, cflush_addr)
            if len(request_buffer) < 32:
                print("Failed to read syscall packet")
                continue

            syscall_id, a0, a1, a2 = struct.unpack_from('<4Q', request_buffer)

            if syscall_id == FESVR_SYSCALLS.write:
                char_buffer = read_bytes(sock, dest, a1, a2, cflush_addr)
                try:
                    print(char_buffer.decode('utf-8'), end='')
                except UnicodeDecodeError:
                    print(char_buffer, end='')

            elif syscall_id == FESVR_SYSCALLS.exit:
                print("DUT exit.")
                return int(a0)

            else:
                print(f"Unknown syscall: {syscall_id}")
                print(f"  a0={a0:#x} a1={a1:#x} a2={a2:#x}")

            # Verify tohost still matches before clearing (mirrors pyuartsi)
            current = read_longword(sock, dest, tohost_addr, cflush_addr)
            if current != request_ptr:
                print(f"Warning: tohost changed {request_ptr:#x} -> {current:#x}")

            # Ack: clear tohost (no flush needed — write goes to DRAM),
            # then flush-before-write fromhost=1 so chip sees it (mirrors pyuartsi)
            write_word64(sock, dest, tohost_addr, 0)
            write_longword_cached(sock, dest, fromhost_addr, 1, cflush_addr)

    except KeyboardInterrupt:
        print("\nAborted.")
        return 1


def main():
    parser = argparse.ArgumentParser(
        description="UDP-TSI Host Tool")
    parser.add_argument("--ip", default=FPGA_IP)
    parser.add_argument("--port", type=int, default=FPGA_PORT)
    parser.add_argument("--uart", default="/dev/ttyUSB1",
                        help="UART device for MDIO control (default: /dev/ttyUSB1)")
    parser.add_argument("--baud", type=int, default=9600,
                        help="UART baud rate for MDIO control (default: 9600)")

    sub = parser.add_subparsers(dest="command")

    sub.add_parser("ping", help="Check FPGA connectivity")

    sub.add_parser("read-watchdog", help="Read RX watchdog sticky bit + fire count via ctrl port")


    p_reset_chip = sub.add_parser("reset-chip", help="Pulse chip reset via ctrl port")
    p_reset_chip.add_argument("--mask", type=lambda x: int(x, 0), default=0x3,
                              help="Bitmask of chips to reset: bit0=chip0, bit1=chip1 (default: 0x3 = both)")
    p_reset_chip.add_argument("--hold-ms", type=int, default=10,
                              help="Reset hold time in milliseconds (default: 50)")

    p_set_watchdog = sub.add_parser("set-watchdog-timeout", help="Set RX watchdog timeout (in clk cycles) via ctrl port")
    p_set_watchdog.add_argument("cycles", type=lambda x: int(x, 0), help="Timeout in clk cycles (unsigned, truncated to 32 bits)")

    p_select_chip = sub.add_parser("select-chip", help="Select which chip the UDP-TSI bridge talks to, by chip id (absolute select, held until board switch toggled)")
    p_select_chip.add_argument("chip", type=lambda x: int(x, 0), help="Chip id to select: 0 = chip 0, 1 = chip 1")

    p_load = sub.add_parser("load", help="Load raw binary to SoC memory")
    p_load.add_argument("file", help="Binary file to load")
    p_load.add_argument("--base", type=lambda x: int(x, 0), default=0x80000000)

    p_load_elf = sub.add_parser("load-elf", help="Load ELF sections to SoC memory")
    p_load_elf.add_argument("file", help="ELF file to load")
    p_load_elf.add_argument("--verify", action="store_true",
                       help="Read back loaded sections via TSI and compare against the ELF")
    p_load_elf.add_argument("--cflush-addr", type=lambda x: int(x, 0), default=CFLUSH_ADDR,
                       help=f"Cache flush control register address (default: {CFLUSH_ADDR:#x})")

    p_run = sub.add_parser("run", help="Load ELF and run with HTIF (fesvr-like)")
    p_run.add_argument("file", help="ELF file to load and run")
    p_run.add_argument("--cflush-addr", type=lambda x: int(x, 0), default=CFLUSH_ADDR,
                       help=f"Cache flush control register address (default: {CFLUSH_ADDR:#x})")
    p_run.add_argument("--no-use-symbols", action="store_true",
                       help="Use .htif section instead of symbol table for tohost/fromhost")
    p_run.add_argument("--verify", action="store_true",
                       help="Read back loaded sections via TSI and compare against the ELF before running")
    p_run.add_argument("--no-reset", action="store_true",
                       help="Skip chip reset pulse before loading ELF")
    p_run.add_argument("--reset-mask", type=lambda x: int(x, 0), default=0x3,
                       help="Bitmask of chips to reset before run: bit0=chip0, bit1=chip1 (default: 0x3)")

    p_write = sub.add_parser("write", help="Write a 64-bit value")
    p_write.add_argument("addr", type=lambda x: int(x, 0))
    p_write.add_argument("value", type=lambda x: int(x, 0))

    p_read = sub.add_parser("read", help="Read memory")
    p_read.add_argument("addr", type=lambda x: int(x, 0))
    p_read.add_argument("nbytes", type=lambda x: int(x, 0))

    p_mdio_read = sub.add_parser("mdio-read", help="Read MDIO register over UART")
    p_mdio_read.add_argument("reg", type=lambda x: int(x, 0), help="PHY register address (0-31)")

    p_mdio_write = sub.add_parser("mdio-write", help="Write MDIO register over UART")
    p_mdio_write.add_argument("reg", type=lambda x: int(x, 0), help="PHY register address (0-31)")
    p_mdio_write.add_argument("value", type=lambda x: int(x, 0), help="16-bit value")

    sub.add_parser("mdio-link", help="Critically check PHY link status (BMSR double-read)")

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        return 1

    if args.command in ("mdio-read", "mdio-write", "mdio-link"):
        if args.command == "mdio-read":
            reg = args.reg & 0x1F
            val = mdio_read(args.uart, args.baud, reg)
            return 0 if val is not None else 1
        elif args.command == "mdio-write":
            reg = args.reg & 0x1F
            value = args.value & 0xFFFF
            return mdio_write(args.uart, args.baud, reg, value)
        elif args.command == "mdio-link":
            return mdio_check_link(args.uart, args.baud)

    tsi_dest = (args.ip, args.port)
    non_tsi_dest = (args.ip, args.port + 1)
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 4 * 1024 * 1024)
    actual_rcvbuf = sock.getsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF)
    print(f"SO_RCVBUF = {actual_rcvbuf} bytes")
    sock.bind(('192.168.1.1', 0))

    try:
        if args.command == "ping":
            return 0 if ping_fpga(sock, non_tsi_dest) else 1
        elif args.command == "read-watchdog":
            return 0 if read_watchdog(sock, non_tsi_dest) is not None else 1
        elif args.command == "select-chip":
            return 0 if select_chip(sock, non_tsi_dest, args.chip) is not None else 1
        elif args.command == "set-watchdog-timeout":
            return 0 if set_watchdog_timeout(sock, non_tsi_dest, args.cycles) is not None else 1
        elif args.command == "reset-chip":
            pulse_chip_reset(sock, non_tsi_dest, mask=args.mask, hold_s=args.hold_ms / 1000.0)
            return 0
        elif args.command == "load":
            return 0 if load_binary(sock, tsi_dest, args.file, args.base) else 1
        elif args.command == "load-elf":
            load_elf(sock, tsi_dest, args.file)
            if args.verify:
                return 0 if verify_elf_load(sock, tsi_dest, args.file, args.cflush_addr) else 1
        elif args.command == "run":
            return run_elf(sock, tsi_dest, args.file,
                           cflush_addr=args.cflush_addr,
                           use_symbols=not args.no_use_symbols,
                           verify=args.verify,
                           reset_mask=0 if args.no_reset else args.reset_mask,
                           non_tsi_dest=non_tsi_dest)
        elif args.command == "write":
            write_word(sock, tsi_dest, args.addr, args.value)
        elif args.command == "read":
            read_words(sock, tsi_dest, args.addr, args.nbytes)
    finally:
        sock.close()

    return 0

if __name__ == "__main__":
    sys.exit(main() or 0)
