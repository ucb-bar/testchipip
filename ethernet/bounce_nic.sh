#!/usr/bin/env bash
set -euo pipefail

IFACE="enx00249b3f135a"
ADDR_CIDR="192.168.1.1/24"

echo "Bringing ${IFACE} down"
sudo ip link set dev "${IFACE}" down

echo "Forcing ${IFACE} to 10 Mbps, half duplex, autoneg off"
sudo ethtool -s "${IFACE}" speed 10 duplex half autoneg off

echo "Clearing old IPv4 addresses on ${IFACE}"
sudo ip -4 addr flush dev "${IFACE}"

echo "Assigning ${ADDR_CIDR} to ${IFACE}"
sudo ip addr add "${ADDR_CIDR}" dev "${IFACE}"

echo "Bringing ${IFACE} up"
sudo ip link set dev "${IFACE}" up

echo
echo "Final link status:"
ethtool "${IFACE}" | sed -n '1,12p'

echo
echo "Final IPv4 addresses:"
ip -4 addr show dev "${IFACE}"
