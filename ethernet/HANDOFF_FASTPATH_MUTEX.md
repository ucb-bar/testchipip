# Handoff Summary: UDP-TSI Fastpath Chisel Bring-up Debug

## Context
FPGA bring-up of a **dual-chiplet RISC-V SoC** on Nexys Video (Artix-7 `xc7a200tsbg484-1`). A **UDP-TSI** interface (TSI-over-UDP-Ethernet) lets a host load/run programs. A **fastpath write router** (`tsi_fastpath_write_router.v`) intercepts DDR-window (0x80000000+) TSI writes/reads and services them directly to DDR (MIG), bypassing the chip's slow TSI path; everything else replays to the selected chip's legacy TSI backend.

- **Repo:** `/scratch/kevinand/repos/kodiak-cy-sim` (branch `kodiak-bringup`, main `master`)
- **Config under test:** `NexysVideoChipletEthBringupHostConfig125` (125 MHz)
- **Harness:** `NexysVideoDualChipBringUpHarness`
- **Git commit style:** NO `Co-Authored-By` trailer (user preference).

## The Core Problem
User migrated the fastpath from a **hand-edited generated Verilog** design (`generated-src/`, **WORKS**) to a **Chisel-generated** design (`generated-src-fastpath/`, **BROKEN**). Symptoms in the Chisel build:
- Load (fastpath **writes**) works.
- Fastpath **reads** work **only before the chip runs**; once the chip is kicked and drives DDR, reads hang -> router wedged -> `tohost` never read -> **no program output**. (`ping` still ACKs because control-port packets bypass the router; TSI-port reads can't be accepted when the router's input FIFO backs up.)

## KEY INSIGHT (root cause -- confirmed by diffing working vs Chisel collateral)
**The RTL is identical and proven** (`generators/testchipip/ethernet/*.v` -- router, MAC). Router params are effectively identical (RTL defaults `ADDR_BITS=37`, `RX_FIFO_DEPTH=128`, `MAX_OUTSTANDING=8`, `SOURCE_BITS=5` = what Chisel passes). The shim (`UDPTSIStreamMuxShim.sv`) is functionally identical.

**The only real difference is the fastpath->DDR connection model:**
- **Working (generated-src):** a `fastActive` **MUX at the MIG's TL port** (harness Verilog lines ~323-432): when `fastActive` high, fastpath owns MIG **exclusively** and chip xbar A/D are **blocked**; when low, chip owns MIG. **Mutual exclusion -- never concurrent.** Fastpath address truncated `[31:0]`. Fastpath bypasses the xbar entirely.
- **Chisel (broken):** fastpath added as a **concurrent** `TLClientNode` -> `ddrTLXBar` -> MIG, arbitrated alongside the chip. **True concurrency at the MIG deadlocks** (the router/MAC RTL was only ever proven under mutual exclusion).

This explains everything: works when chip idle (no concurrency), hangs when chip active (concurrency).

## Dead-ends already ruled out (don't re-litigate)
- Router `S_DECODE2` timing pipeline (added, kept -- fixed real setup violations).
- `fast_d_ready` always-high (HOL-blocking hypothesis) -- **reverted** at user request; saved as memory note `fastpath-d-ready-hol-blocking.md` (latent, not root cause).
- `TLBuffer(BufferParams(128))` on the fastpath xbar edge -- **removed** (band-aid on wrong model).
- MAC ACK byte-count race (word0 combinational vs byte_count registered) -- real latent bug but a **separate** issue, not the current blocker.
- `ExtTLMem` is `None` for this config (via `WithExtMemMultiChip`), so `ddrTLXBar` is a real `TLXbar` (not identity) -- arbitration exists; not the bug.

## CURRENT WORK -- "Option A" (in progress, uncommitted)
Reproduce the working **mutual-exclusion mux** in Chisel so the generated build matches the proven design. Implemented as a `TLAdapterNode`-based `FastpathMigMux` spliced between `ddrBlockDuringReset` and the MIG; fastpath bypasses the xbar.

**Files changed (all UNCOMMITTED):**
1. `fpga/src/main/scala/nexysvideo/Harness.scala`:
   - Added `class FastpathMigMux` + named `class FastpathMigMuxImp(outer)` (mux logic: `fastActive` ? fastpath drives MIG (chip blocked) : chip xbar passthrough; `fit()` width-adapts address/source).
   - Replaced `fastpathClient` (TLClientNode) with `fastpathMux` (LazyModule).
   - MIG path: `ddrOverlay.ddr := mux.node := ddrBlockDuringReset.node := ddrTLXBar`.
2. `fpga/src/main/scala/nexysvideo/HarnessBinders.scala`: binder now drives `nexysTh.fastpathMux.get.module.io.fast`/`.fastActive` from `shim.io.fast_tl`/`shim.io.fastActive` (replaced the old `fastpathClient` wiring).

**Compile status (iterating):**
1. Anonymous `LazyModuleImp` subclass -> `module.io not a member` (refinement erased).
2. Top-level named `FastpathMigMuxImp` class -> elaboration error "Chisel compiler plugin is now required" (plugin didn't instrument the top-level Impl / its anonymous `io` Bundle).
3. **Current:** nested `class Impl extends LazyModuleImp(this)` inside `FastpathMigMux` (canonical chipyard pattern) -- preserves `mux.module.io` type access AND gets the plugin applied. **Awaiting user's re-compile.**

## IMMEDIATE NEXT STEP
1. User re-runs `sbt compile` (make target `chipyard_fpga.jar`). If errors, fix (watch: `out <> in` + per-field override last-connect; `TLAdapterNode()` identity; `mux.module.io` access).
2. Re-elaborate `NexysVideoChipletEthBringupHostConfig125` + rebuild bitstream.
3. **Test hypothesis:** reads should now survive the chip running (mutual exclusion), and `tohost` program output should appear. This confirms concurrency was the sole regression.

## Also uncommitted (separate, working)
- `generators/testchipip/ethernet/tsi_fastpath_write_router.v`: `S_DECODE2` pipeline (timing fix -- keep).
- Earlier session: MDIO-over-UART restore + `ENABLE_RGMII_RX_IDELAY` in `fpga/Makefile` (timing closed, load works -- confirmed good on hardware).

## Key locations
- Router RTL: `generators/testchipip/ethernet/tsi_fastpath_write_router.v` (read path `S_FREAD_*` ~616-673; `fast_d_ready` ~243).
- MAC RTL: `generators/testchipip/ethernet/udp_payload_to_tsi_serial.v` (ACK gen ~547-575; RX gating line 171).
- Shim Chisel: `generators/testchipip/src/main/scala/tsi/UDPTSIRGMIIMuxShim.scala`, blackbox `.../tsi/UDPToTSI.scala`.
- Host tools: `generators/testchipip/ethernet/udp_tsi_host.py`, `eth_bw_test.py`.
- Working reference collateral: `fpga/generated-src/.../gen-collateral/NexysVideoDualChipBringUpHarness.sv` (the `fastActive` MIG mux is the ground truth to match).

## Diagnostic facts to reuse
- `ping` works but read no-ACK => router wedged (TSI-port RX FIFO full); ctrl-port bypasses router.
- Router read path uses **source 0** (same as write pool) + source-blind `S_FREAD_RECV` capture -- fine under mutual exclusion, would collide under concurrency (another reason concurrency is wrong for this RTL).
- Fastpath reads DDR directly (bypasses chip cache) -> `tohost`/`fromhost` use host **force-legacy** for coherency (magic addrs `0x1FFFFFE00`/`08`).
