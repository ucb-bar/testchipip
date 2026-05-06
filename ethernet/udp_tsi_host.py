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

FPGA_IP   = "192.168.1.10"
FPGA_PORT = 7000
TIMEOUT   = 10.0

ACK_MAGIC = 0xAC010001  # Must match ACK_PAYLOAD parameter in Verilog

UART_ADDR_MDIO = 0x02
MDIO_OP_WRITE = 0x01
MDIO_OP_READ  = 0x02

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

def load_binary(sock, dest, filepath, base_addr=0x80000000):
    """Load a binary file into SoC memory via TSI write commands."""
    with open(filepath, 'rb') as f:
        binary = f.read()

    if len(binary) % 4 != 0:
        binary += b'\x00' * (4 - len(binary) % 4)

    chunk_size = 1024
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

def read_words(sock, dest, addr, num_bytes):
    """Read memory and print contents."""
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


def get_htif_tohost(filename):
    """Return (tohost_addr, fromhost_addr) from ELF symbol table or .htif section."""
    if not _have_elftools:
        raise RuntimeError("pyelftools not installed: pip install pyelftools")
    tohost = fromhost = None
    with open(filename, 'rb') as f:
        elf = ELFFile(f)
        # Try symbol table first
        for section in elf.iter_sections():
            if isinstance(section, SymbolTableSection):
                for sym in section.iter_symbols():
                    if sym.name == 'tohost':
                        tohost = sym['st_value']
                    elif sym.name == 'fromhost':
                        fromhost = sym['st_value']
        # Fall back to .htif section address
        if tohost is None:
            for section in elf.iter_sections():
                if section.name == '.htif':
                    tohost   = section['sh_addr']
                    fromhost = section['sh_addr'] + 8
    if tohost is None:
        tohost   = 0x80001000
        fromhost = 0x80001008
        print(f"Warning: tohost not found in ELF, using default 0x{tohost:08X}")
    return tohost, fromhost


def load_elf(sock, dest, filename, chunk_size=1024):
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
            print(f"Loading {section.name} ({len(data)} bytes) -> 0x{addr:08X}")
            sent = 0
            while sent < len(data):
                chunk = data[sent:sent + chunk_size]
                if len(chunk) % 4 != 0:
                    chunk = chunk + b'\x00' * (4 - len(chunk) % 4)
                words = make_tsi_write_cmd(addr + sent, chunk)
                send_tsi_words(sock, words, dest)
                resp = recv_response(sock)
                if resp is None:
                    print(f"  WARNING: no ACK at 0x{addr+sent:08X}")
                sent += len(data[sent:sent + chunk_size])
    print("ELF load complete.")


# HTIF syscall numbers (subset used by pk/bbl)
_FESVR_SYSCALL_WRITE = 64
_FESVR_SYSCALL_EXIT  = 93


def _htif_handle_syscall(sock, dest, fromhost_addr, pkt_addr):
    """Read a syscall packet, handle write/exit, ack via fromhost."""
    # Syscall packet: magic_mem[0]=syscall_num, [1]=a0..a4, [7]=return_val
    # We only need to read 8 x 8-byte words = 64 bytes
    words, expected = make_tsi_read_cmd(pkt_addr, 64)
    send_tsi_words(sock, words, dest)
    data = b''
    timeout = TIMEOUT
    while len(data) < expected * 4:
        resp = recv_response(sock, timeout=timeout)
        if resp is None:
            break
        timeout = 0.5
        if parse_ack(resp):
            continue
        data += resp
    if len(data) < 64:
        return False  # can't parse

    fields = struct.unpack_from('<8Q', data[:64])
    syscall = fields[0]

    if syscall == _FESVR_SYSCALL_WRITE:
        fd, buf_addr, count = fields[1], fields[2], fields[3]
        # Read the string from buf_addr
        aligned = (count + 7) & ~7
        words2, exp2 = make_tsi_read_cmd(buf_addr, aligned)
        send_tsi_words(sock, words2, dest)
        buf = b''
        t2 = TIMEOUT
        while len(buf) < exp2 * 4:
            r = recv_response(sock, timeout=t2)
            if r is None:
                break
            t2 = 0.5
            if parse_ack(r):
                continue
            buf += r
        text = buf[:count]
        os.write(fd if fd in (1, 2) else 1, text)
        # Write return value (count) into packet[7] then ack
        ret_addr = pkt_addr + 7 * 8
        write_word64(sock, dest, ret_addr, count)

    elif syscall == _FESVR_SYSCALL_EXIT:
        return fields[1]  # exit code in a0

    # Ack: write 1 to fromhost
    write_word64(sock, dest, fromhost_addr, 1)
    return False


def run_elf(sock, dest, filename, poll_interval=0.01):
    """Load ELF, release chip, then poll tohost and handle HTIF syscalls."""
    tohost_addr, fromhost_addr = get_htif_tohost(filename)
    print(f"tohost=0x{tohost_addr:08X}  fromhost=0x{fromhost_addr:08X}")

    load_elf(sock, dest, filename)

    # Clear tohost/fromhost before starting
    write_word64(sock, dest, tohost_addr,   0)
    write_word64(sock, dest, fromhost_addr, 0)

    # Kick hart 0 via MSIP (CLINT base)
    write_word64(sock, dest, 0x02000000, 1)
    print("MSIP written — hart 0 kicked")

    print("Polling tohost... (Ctrl-C to abort)")
    n = 0
    try:
        while True:
            val = read_word64(sock, dest, tohost_addr)
            n += 1
            if n % 100 == 0:
                print(f"  [poll {n}: tohost=0x{val or 0:016X}]", flush=True)
            if val is None or val == 0:
                time.sleep(poll_interval)
                continue

            # Clear tohost immediately
            write_word64(sock, dest, tohost_addr, 0)

            if val & 1:
                # Exit: upper bits = exit code
                code = (val >> 1) & 0x7FFFFFFF
                print(f"\nProgram exited with code {code}")
                return code

            # Syscall packet pointer in upper bits
            pkt_addr = val & ~1
            result = _htif_handle_syscall(sock, dest, fromhost_addr, pkt_addr)
            if result is not False:
                print(f"\nProgram exited with code {result}")
                return result

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

    p_load = sub.add_parser("load", help="Load raw binary to SoC memory")
    p_load.add_argument("file", help="Binary file to load")
    p_load.add_argument("--base", type=lambda x: int(x, 0), default=0x80000000)

    p_load_elf = sub.add_parser("load-elf", help="Load ELF sections to SoC memory")
    p_load_elf.add_argument("file", help="ELF file to load")

    p_run = sub.add_parser("run", help="Load ELF and run with HTIF (fesvr-like)")
    p_run.add_argument("file", help="ELF file to load and run")

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

    dest = (args.ip, args.port)
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.bind(('192.168.1.1', 0))

    try:
        if args.command == "ping":
            return 0 if ping_fpga(sock, dest) else 1
        elif args.command == "load":
            return 0 if load_binary(sock, dest, args.file, args.base) else 1
        elif args.command == "load-elf":
            load_elf(sock, dest, args.file)
        elif args.command == "run":
            return run_elf(sock, dest, args.file)
        elif args.command == "write":
            write_word(sock, dest, args.addr, args.value)
        elif args.command == "read":
            read_words(sock, dest, args.addr, args.nbytes)
    finally:
        sock.close()

    return 0

if __name__ == "__main__":
    sys.exit(main() or 0)
