# UDP DMA Fast Path Scala Plan

This note captures the intended Scala-side construction for adding a bulk-write
DMA path in addition to the existing UDP-TSI path.

## Goal

Keep both paths alive:

- `TSIToTileLink` for normal TSI traffic, debug, MMIO, and software tooling
- a new DMA master for bulk ELF/data loading

One Ethernet MAC/UDP stack should feed both.

## Current behavior to preserve

- `udp_tsi_top.sv` terminates UDP and already distinguishes:
  - `UDP_PORT` for TSI traffic
  - `UDP_PORT+1` for control traffic
- `udp_payload_to_tsi_serial.v` converts TSI payloads to the serial stream
- `TSIToTileLink.scala` converts serial TSI commands into TileLink traffic

The future DMA path should be additive, not a replacement.

## Architecture

Target split:

```text
UDP RX
  -> parser / demux
     -> TSI path  -> udp_payload_to_tsi_serial -> TSIToTileLink -> fbus
     -> DMA path  -> bulk FIFO -> DMA writer -> memory-facing bus
```

Recommended policy for the first DMA version:

- accept DMA writes only when the destination address is inside DDR memory
- reject or NACK non-DDR destinations

This is only a safety policy. The packet format itself should remain generic.

## Recommended Scala traits and files

Create a new Scala file, for example:

- `generators/testchipip/src/main/scala/tsi/UDPDMA.scala`

Define:

1. `UDPDMAParams`
2. `UDPDMAKey`
3. a DMA stream bundle between harness UDP RTL and the subsystem
4. a subsystem mix-in, for example `CanHavePeripheryUDPDMA`
5. config fragments, for example `WithUDPDMA(...)`

## Suggested DMA stream bundle

The UDP RTL should not expose raw Ethernet details into the subsystem.
Expose a decoded descriptor/data stream pair instead.

Suggested bundle split:

- descriptor channel:
  - `opcode`
  - `dst_addr`
  - `byte_len`
  - `seqno`
- data channel:
  - `data`
  - `keep` or byte-valid mask
  - `last`

This keeps the UDP packet parser in RTL and the memory writer in Chisel/Scala.

## Suggested subsystem mix-in

Pattern it after the existing UDP-TSI mix-in.

Inside `CanHavePeripheryUDPDMA { this: BaseSubsystem => ... }`:

1. choose the bus attachment point
2. instantiate a DMA engine LazyModule
3. export its decoded command/data ingress as chip-top IO

Recommended bus attachment:

- prefer a memory-facing bus, not the front bus
- use `MBUS` if the design has it
- otherwise use the external-memory-facing bus closest to DDR

Avoid routing bulk writes through the TSI front-bus path.

## DMA engine bus interface

Preferred:

- AXI4 master if the DDR path is naturally AXI-oriented

Alternative:

- TileLink client node if easier in this design

If using TileLink:

- make the DMA engine a `TLClientNode`
- issue writes with multiple outstanding transactions
- support burst-like streaming by keeping several requests in flight

If using AXI4:

- use `AXI4MasterNode`
- issue real write bursts
- allow multiple outstanding AW/W/B transactions

## Required DMA engine behavior

The DMA engine should:

1. accept a decoded descriptor
2. buffer payload data in a real FIFO
3. pack payload bytes up to the memory bus width
4. issue writes with more than one transaction outstanding
5. generate a completion status back to the UDP RTL

Important:

- this engine must not wait for one write response before launching the next
- that is the main limitation of the current `TSIToTileLink` path

## Chip-top IO strategy

Use the same general approach as `CanHavePeripheryUDPTSI`:

1. instantiate the DMA engine in a clock domain compatible with the memory path
2. export a cleaned-up stream IO bundle with `InModuleBody`
3. add an `IOBinder` punchthrough
4. connect that IO in the harness-side UDP shim

Do not export the entire memory bus to the harness.
Only export the decoded DMA ingress/egress bundle.

## Harness / UDP shim changes

The harness-side UDP shim should eventually grow from:

- one serial TSI path

to:

- one serial TSI path
- one DMA descriptor/data path
- one completion/response path

The UDP top-level RTL should demux packets by opcode or port and feed:

- TSI packets to `udp_payload_to_tsi_serial`
- DMA packets to the DMA ingress bundle

## Packet format recommendation

Use a generic DMA packet format, not a DDR-specific wire format.

Suggested fields:

1. opcode
2. destination address
3. byte count
4. sequence number
5. payload bytes

Then apply a policy check in FPGA logic:

- if address in DDR range: accept on DMA path
- else: reject or NACK

This keeps the protocol reusable while still keeping the first implementation
safe and DDR-focused.

## Incremental implementation order

1. keep the current UDP-TSI path intact
2. add the new DMA bundle and mix-in in Scala
3. add the DMA packet parser in UDP RTL
4. connect the DMA engine to the memory-facing bus
5. gate accepted addresses to DDR only
6. add host-side bulk-loader support

## What not to do first

- do not widen the front bus first
- do not try to make `TSIToTileLink` itself serve as the bulk DMA engine

Those changes are larger and do not address the core issue as directly as a
parallel DMA path.
