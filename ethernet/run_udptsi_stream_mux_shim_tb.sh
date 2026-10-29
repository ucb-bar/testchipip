#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHIPYARD_ROOT="$(cd "${ROOT_DIR}/../../.." && pwd)"
GEN_DIR="${GEN_DIR:-${CHIPYARD_ROOT}/fpga/generated-src/chipyard.fpga.nexysvideo.NexysVideoDualChipBringUpHarness.NexysVideoChipletEthBringupHostConfig125/gen-collateral}"
SIMV_DIR="${ROOT_DIR}/simv_udptsi_stream_mux_shim"
VE_RTL_DIR="${ROOT_DIR}/verilog-ethernet/rtl"
AXIS_RTL_DIR="${ROOT_DIR}/verilog-ethernet/lib/axis/rtl"
ENABLE_FSDB="${ENABLE_FSDB:-1}"

rm -rf "${SIMV_DIR}" csrc simv.daidir ucli.key

SOURCES=(
  "${ROOT_DIR}/sim_xilinx_prims.v"
  "${ROOT_DIR}/tb_udptsi_stream_mux_shim.sv"
  "${GEN_DIR}/UDPTSIStreamMuxShim.sv"
  "${ROOT_DIR}/udp_tsi_top.sv"
  "${ROOT_DIR}/udp_payload_to_tsi_serial.v"
  "${ROOT_DIR}/tsi_fastpath_write_router.v"
  "${ROOT_DIR}/startup_fsm.v"
  "${ROOT_DIR}/udp_ctrl.v"
  "${ROOT_DIR}/uart_rx_simple.v"
  "${ROOT_DIR}/uart_tx_simple.v"
  "${ROOT_DIR}/uart_idelay_ctrl.v"
  "${VE_RTL_DIR}/mdio_master.v"
  "${VE_RTL_DIR}/eth_mac_1g_rgmii_fifo.v"
  "${VE_RTL_DIR}/eth_mac_1g_rgmii.v"
  "${VE_RTL_DIR}/eth_mac_1g.v"
  "${VE_RTL_DIR}/rgmii_phy_if.v"
  "${VE_RTL_DIR}/axis_gmii_rx.v"
  "${VE_RTL_DIR}/axis_gmii_tx.v"
  "${VE_RTL_DIR}/axis_eth_fcs.v"
  "${VE_RTL_DIR}/axis_eth_fcs_check.v"
  "${VE_RTL_DIR}/axis_eth_fcs_insert.v"
  "${VE_RTL_DIR}/lfsr.v"
  "${VE_RTL_DIR}/iddr.v"
  "${VE_RTL_DIR}/oddr.v"
  "${VE_RTL_DIR}/ssio_ddr_in.v"
  "${VE_RTL_DIR}/eth_axis_rx.v"
  "${VE_RTL_DIR}/eth_axis_tx.v"
  "${VE_RTL_DIR}/udp_complete.v"
  "${VE_RTL_DIR}/udp.v"
  "${VE_RTL_DIR}/udp_ip_rx.v"
  "${VE_RTL_DIR}/udp_ip_tx.v"
  "${VE_RTL_DIR}/udp_checksum_gen.v"
  "${VE_RTL_DIR}/ip_complete.v"
  "${VE_RTL_DIR}/ip.v"
  "${VE_RTL_DIR}/ip_eth_rx.v"
  "${VE_RTL_DIR}/ip_eth_tx.v"
  "${VE_RTL_DIR}/ip_arb_mux.v"
  "${VE_RTL_DIR}/eth_arb_mux.v"
  "${VE_RTL_DIR}/arp.v"
  "${VE_RTL_DIR}/arp_eth_rx.v"
  "${VE_RTL_DIR}/arp_eth_tx.v"
  "${VE_RTL_DIR}/arp_cache.v"
  "${AXIS_RTL_DIR}/axis_async_fifo_adapter.v"
  "${AXIS_RTL_DIR}/axis_async_fifo.v"
  "${AXIS_RTL_DIR}/axis_fifo.v"
  "${AXIS_RTL_DIR}/axis_adapter.v"
  "${AXIS_RTL_DIR}/arbiter.v"
  "${AXIS_RTL_DIR}/priority_encoder.v"
)

VCS_ARGS=(
  -full64
  -sverilog
  -kdb
  -timescale=1ns/1ps
  -debug_access+all
  -top tb_udptsi_stream_mux_shim
  -o "${SIMV_DIR}"
)

if [[ "${ENABLE_FSDB}" != "0" ]]; then
  if [[ -z "${VERDI_HOME:-}" ]]; then
    echo "ERROR: ENABLE_FSDB=${ENABLE_FSDB} but VERDI_HOME is not set" >&2
    exit 2
  fi
  VCS_ARGS+=(
    +define+ENABLE_FSDB
    -P "${VERDI_HOME}/share/PLI/VCS/LINUX64/novas.tab"
       "${VERDI_HOME}/share/PLI/VCS/LINUX64/pli.a"
  )
fi

vcs "${VCS_ARGS[@]}" "${SOURCES[@]}"

"${SIMV_DIR}"
