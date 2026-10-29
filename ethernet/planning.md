# Ethernet / UDP-TSI Fast-Path Planning Notes

## `tsi_fastpath_write_router` — moved out of generated collateral (MUST be done in Chisel)

The `tsi_fastpath_write_router` module currently lives in
`ethernet/tsi_fastpath_write_router.v`. It was extracted out of the generated
file
`fpga/generated-src/.../gen-collateral/UDPTSIStreamMuxShim.sv`
because a bug fix had to be applied directly to the built RTL (see below).

**This is a stopgap.** `UDPTSIStreamMuxShim.sv` is generated collateral and will
be overwritten on the next elaboration/firtool run, so any edit there (including
the extraction itself) is not durable. The module must be defined in the Chisel
source that produces `UDPTSIStreamMuxShim`
(`generators/testchipip/src/main/scala/tsi/UDPTSIRGMIIMuxShim.scala`), e.g. as a
blackbox wrapping `ethernet/tsi_fastpath_write_router.v`, or reimplemented in
Chisel — so that the fix survives regeneration.

### Bug that was fixed in the extracted file

FSM deadlock in state 7 (`S_FAST_FILL`). `S_FAST_PLAN` plans only ONE burst
(max `MAX_BURST_BEATS * TL_CHUNKS_PER_BEAT * TSI_BYTES = 8 * 2 * 4 = 64 B`), and
it is entered only once, from `S_CLASSIFY`. After a burst issues with words still
remaining, `S_FAST_ISSUE` returned to `S_FAST_FILL` instead of re-running the
planner, so `burst_capture_words_left_reg` / `burst_word_count_reg` /
`burst_first_chunk_reg` were stale. The next burst never hit the issue condition
(`burst_capture_words_left_reg == 1`), the counter underflowed,
`fill_words_left_reg` walked to 0, `fifo_out_ready_reg` deasserted, and `do_pop`
died — hard hang.

This bites ANY fast-path write larger than one 64 B burst (or any write the
planner splits, e.g. due to alignment). Example: 512 B `.text` chunks at
`0x80000000` deadlock on the second burst, 64 B in (`0x80000040`).

**Fix:** in `S_FAST_ISSUE`, the non-terminal exit re-plans:

```verilog
    if (transaction_done_reg)
      state <= S_FAST_WAIT;
    else
      state <= S_FAST_PLAN;   // was S_FAST_FILL
```

The same fix must be carried into the Chisel implementation.

## Bug 2: hang in S_FAST_WAIT — `outstanding_reg` double-driven (also must be in Chisel)

`outstanding_reg` was updated by two separate non-blocking assignments in the
same `always` block: a decrement in the D-response handler and an increment in
`S_FAST_ISSUE` when a burst's final beat is accepted. When a D response retires
in the same cycle a new burst commits, the textually-later increment wins and
the decrement is dropped, so the counter over-counts and never reaches 0 — the
FSM hangs in `S_FAST_WAIT`. This only became reachable once Bug 1 was fixed and
multi-burst writes started overlapping issue with responses.

**Fix:** drive `outstanding_reg` from a single coherent update that handles the
simultaneous case (net no-op):

```verilog
wire fast_d_consume = fast_d_valid && (fast_d_source < MAX_OUTSTANDING) && !free_sources_reg[fast_d_source];
wire burst_issue_commit = (state == S_FAST_ISSUE) && fast_a_valid_reg && fast_a_ready &&
                          ((issue_beat_idx_reg + 1'b1) == burst_beats_reg);
...
case ({burst_issue_commit, fast_d_consume})
  2'b10: outstanding_reg <= outstanding_reg + 1'b1;
  2'b01: if (outstanding_reg != 0) outstanding_reg <= outstanding_reg - 1'b1;
  default: ;   // 2'b11 net zero, 2'b00 unchanged
endcase
```

The Chisel implementation must compute the next `outstanding` value once from
both events rather than `+1`/`-1` in separate `when` blocks.

## Feature: absolute chip-select override (recency mux) — Chisel changes required

The chip select is now an absolute value written by the host arbitrated against
the board switch by recency. The old `select_invert` feature has been **removed
entirely** (host ctrl commands, register, ACK readback, and the startup FSM's
boot-time assertion). Implemented entirely in the ethernet RTL:

* `udp_payload_to_tsi_serial.v`: new ctrl command `CTRL_CMD_SET_SELECT_VALUE`
  (`"SELV"` = `32'h53454C56`), a 2-word ctrl-port packet
  `[CTRL_CMD_SET_SELECT_VALUE, value]`. Latches `value[0]` into `select_value`
  and pulses `select_value_wr` for one cycle. New module outputs: `select_value`,
  `select_value_wr`. **Removed:** `CTRL_CMD_SET_SELECT_INVERT`,
  `CTRL_CMD_READ_SELECT_INVERT`, the `select_invert` output, `select_invert_reg`,
  and the `ack_select_query` ACK readback.
* `udp_tsi_top.sv`: new top-level port `input select_switch` (the raw board
  switch, i.e. `io_select`) and `output select_resolved` (effective select).
  Recency mux: a host write holds the register value; a change on the board
  switch reverts to and holds the switch. `select_resolved = select_use_value ?
  ctrl_select_value : select_switch` (no invert). **Removed:** the `select_invert`
  output port, `ctrl_select_invert`, and the ctrl|fsm OR.
* `startup_fsm.v`: its old `select_invert` output (boot step 8) is replaced by a
  one-shot `select_use_switch` pulse that forces the recency mux to default to
  the board switch (`io_select`) once bring-up completes. State `S_SELECT_INV`
  renamed `S_SELECT_SW`.

**Chisel changes still needed (RTL above is the source of truth):**

1. `src/main/scala/tsi/UDPToTSI.scala` — in the `udp_tsi_top` BlackBox IO,
   **remove** `val select_invert = Output(Bool())` and **add**:
   ```scala
   val select_switch   = Input(Bool())
   val select_resolved = Output(Bool())
   ```

2. `src/main/scala/tsi/UDPTSIRGMIIMuxShim.scala` — drive the new switch input and
   use the resolved select; drop the XOR:
   ```scala
   mac.io.select_switch := io.select
   val select = mac.io.select_resolved        // was: io.select ^ mac.io.select_invert
   ```

Until the collateral is regenerated, the generated
`gen-collateral/UDPTSIStreamMuxShim.sv` `mac` (udp_tsi_top) instance must drop
`.select_invert (...)`, connect `.select_switch (io_select)` and
`.select_resolved (...)`, and set `_select` to that resolved net (remove the
`_mac_select_invert` XOR and its wire).

## Debug ILA consolidated to a single core (new IP `ila_fastpath`)

The module previously instantiated four ILAs (`ila_8 udp_tsi_fastpath_ila` +
three `ila_12` counter/plan ILAs). These have been merged into one
`ila_fastpath udp_tsi_fastpath_ila` carrying all 21 unique signals. A matching
ILA IP core named `ila_fastpath` must be generated with 21 probes of these
widths:

| probe | width | signal |
|-------|-------|--------|
| 0  | 4   | state |
| 1  | 64  | hdr_len_reg |
| 2  | 1   | tsi_in_valid |
| 3  | 1   | tsi_in_ready |
| 4  | 32  | tsi_in_bits |
| 5  | 1   | fast_active |
| 6  | 1   | do_pop |
| 7  | 1   | do_push |
| 8  | 32  | cmd_reg |
| 9  | 1   | fast_a_valid |
| 10 | 1   | fast_a_ready |
| 11 | 115 | {fast_a_opcode,fast_a_size,fast_a_address,fast_a_mask,fast_a_data} |
| 12 | 1   | fifo_out_valid |
| 13 | 1   | fifo_out_ready_reg |
| 14 | 64  | words_left_reg |
| 15 | 64  | burst_capture_words_left_reg |
| 16 | 64  | fill_words_left_reg |
| 17 | 64  | planned_first_chunk_tmp |
| 18 | 64  | planned_words_aligned_tmp |
| 19 | 64  | planned_words_capped_tmp |
| 20 | 64  | planned_words_tmp |

The old `ila_8` / `ila_12` cores are no longer referenced by this module. This
ILA setup also needs to be reflected in the Chisel flow.

## Chiplet reset: software can only PULSE, not HOLD — fix + immediate workaround

### Symptom
Program runs once after bitstream config; after asserting the FPGA-reset and
chip-reset GPIOs (simultaneously) and reloading, the host load completes but the
chiplet never writes `tohost`. Only a bitstream reprogram recovers. Ethernet is
fine throughout, and the host-side fastpath load completes — so the chiplet is
simply never parked/restarted onto the freshly loaded program; it keeps running
the stale program already in DRAM.

### Root cause (RTL location)
Chiplet reset is generated by `CanHaveHarnessChipReset` → module
`ClockSinkDomain_1` (`gen-collateral/ClockSinkDomain_1.sv`), instantiated as
`reset_module` in `DigitalTop.sv`. Pin path:
`reset_u_0_chip_rst` (NexysVideoDualChipBringUpHarness.sv) → ChipTop
`top_chip_rst` → DigitalTop `dig_top_chip_reset` → `io_chip_rst`.

```verilog
// ClockSinkDomain_1.sv
wire pulse = <full TL write to register offset 0, data ignored>;
always @(posedge clk)
  if (reset)      begin state<=0; counter<=0; end
  else if (state) counter <= counter - 1;      // counts down
  else if (pulse) counter <= 5'h13;            // load 19 on a write
assign io_chip_rst = |{io_btn, counter};       // btn (level) OR counter (one-shot)
```

So a TL write only loads a fixed ~19-cycle one-shot `counter` — the software/TL
path can only **pulse**. Only `io_btn` (the physical button GPIO, combinational
in the OR, NOT gated by the FPGA design reset) can **hold** reset as a level.
=> The host cannot keep the core parked during the ELF load with this RTL.

### Immediate change — APPLIED in generated RTL (OR `udp_tsi_top.chip_reset` into `btn`)
`udp_tsi_top` already drives `output [1:0] chip_reset` — a host level-hold register
set via `CTRL_CMD_SET_CHIP_RESET` (`udp_payload_to_tsi_serial.v`, resets to 0).
Because `io_chip_rst = io_btn | counter`, feeding the host reset into the `btn`
input gives a software HOLD with zero change to the pulse logic. This is now wired
directly in the generated collateral:

1. `gen-collateral/UDPTSIStreamMuxShim.sv`: added `output [1:0] io_chipReset` and
   connected the `mac` (udp_tsi_top) instance `.chip_reset (io_chipReset)`.
2. `gen-collateral/NexysVideoDualChipBringUpHarness.sv`: declared
   `_UDPTSIStreamMuxShim_io_chipReset`, connected `.io_chipReset(...)` on the shim
   instance, and OR'd it into chip 0's ChipTop button:
   `.top_chip_rst_btn (reset_u_0_btn | _UDPTSIStreamMuxShim_io_chipReset[0])`.
   (chip 1's `reset_u_1_chip_rst` is tied to `1'b0`, so only chip 0 is wired.)

Full path: host `CTRL_CMD_SET_CHIP_RESET` → `chip_reset_reg` → `udp_tsi_top.chip_reset`
→ shim `io_chipReset` → harness OR into `top_chip_rst_btn` → `ClockSinkDomain_1.io_btn`
→ `io_chip_rst = io_btn | counter` → `reset_u_0_chip_rst`.

**Chisel equivalent still needed (so it survives regeneration):**
1. `UDPTSIStreamMuxShim` (Scala shim): `val chipReset = Output(UInt(2.W))`,
   `io.chipReset := mac.io.chip_reset`; add `chip_reset` to the `udp_tsi_top`
   BlackBox IO in `UDPToTSI.scala`.
2. `WithNexysVideoChipReset` (`fpga/src/main/scala/nexysvideo/HarnessBinders.scala`):
   `port.io.btn := harnessIO.btn | shimChipReset(chipId)` (thread the shim's
   `io.chipReset` bit to the reset binder).

Caveats:
- `reset_u_1_chip_rst` is tied to `1'b0` in the harness today (chip 1 reset
  unused); only chip 0 has a real reset path. Confirm which chiplet you drive.
- Verify that holding `chip_rst` does NOT also reset the chiplet's TL/DRAM
  subsystem — if it does, the fastpath load can't land while held, and the
  sequence must be reset→release→load→MSIP instead of hold-through-load.

### Proper fix (sketch — `CanHaveHarnessChipReset.scala`; "scala later")
Replace the one-shot counter with a level-hold register driven by the written
value (keep the pulse path too if other callers rely on it):
```scala
val held = RegInit(false.B)                 // software hold-in-reset
node.regmap(
  0x0 -> Seq(RegField(1, held))             // RW bit: write 1 = hold, 0 = release
  // (optionally keep the existing write-only pulse field at another offset)
)
io.chip_rst := io.btn | held | (counter =/= 0.U)
```
Host: write 1 (hold) → load ELF → write 0 (release).

### Host script (already changed)
`run_elf` in `udp_tsi_host.py` now asserts `CTRL_CMD_SET_CHIP_RESET` (hold) before
`load_elf` and clears it (release) after the load, replacing the pre-load
`pulse_chip_reset`. This only reaches the chiplet once the immediate-change OR (or
the proper fix) is wired in.
