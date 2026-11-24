"""Ramulator2.1 configuration for the Chipyard external frontend."""

import ramulator

frontend = ramulator.frontend.External(
    clock_ratio=8,
)

ddr4 = ramulator.dram.DDR4(
    org_preset="DDR4_8Gb_x8",
    timing_preset="DDR4_2400R",
    rank=2,
    verbose=True,
)

controller = ramulator.controller.GenericDDR(
    dram=ddr4,
    scheduler=ramulator.scheduler.FRFCFS(),
    refresh_manager=ramulator.refresh_manager.AllBank(),
    row_policy=ramulator.row_policy.Open(),
    addr_mapper=ramulator.addr_mapper.RoBaRaCoCh(),
)

memory_system = ramulator.memory_system.GenericDRAM(
    clock_ratio=3,
    controllers=[controller],
    channel_mapper=ramulator.channel_mapper.CacheLineInterleave(),
)

simulation = ramulator.Simulation(frontend, memory_system)