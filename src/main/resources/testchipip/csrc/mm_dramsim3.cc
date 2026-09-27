// See LICENSE for license details.

#include "mm_dramsim3.h"
#include "mm.h"
#include <iostream>
#include <fstream>
#include <list>
#include <queue>
#include <cstring>
#include <cstdlib>
#include <cassert>

using namespace dramsim3;

void mm_dramsim3_t::read_complete(uint64_t address)
{
  assert(!rreq[address].empty());
  auto req = rreq[address].front();
  uint64_t start_addr = (req.addr / word_size) * word_size;
  for (size_t i = 0; i < req.len; i++) {
    auto dat = read(start_addr + i * word_size);
    rresp.push(mm_rresp_t(req.id, dat, (i == req.len - 1)));
  }
  read_id_busy[req.id] = false;
  rreq[address].pop();
}

void mm_dramsim3_t::write_complete(uint64_t address)
{
  assert(!wreq[address].empty());
  auto b_id = wreq[address].front();
  bresp.push(b_id);
  write_id_busy[b_id] = false;
  wreq[address].pop();
}

mm_dramsim3_t::mm_dramsim3_t(size_t mem_base, size_t mem_sz, size_t word_sz, size_t line_sz, backing_data_t& dat, std::string config_file, std::string output_dir, int axi4_ids, uint64_t cpu_hz, int channel_id) :
  mm_t(mem_base, mem_sz, word_sz, line_sz, dat),
  read_id_busy(axi4_ids, false),
  write_id_busy(axi4_ids, false),
  cpu_hz(cpu_hz),
  channel_id(channel_id) {

  assert(line_sz == 64);
  assert(mem_sz % (1024*1024) == 0);
  
  auto read_cb = std::bind(&mm_dramsim3_t::read_complete, this, std::placeholders::_1);
  auto write_cb = std::bind(&mm_dramsim3_t::write_complete, this, std::placeholders::_1);
  fprintf(stderr, "== DRAMSim3: Loading config file '%s' ==\n", config_file.c_str());
  mem = GetMemorySystem(config_file, output_dir, read_cb, write_cb);

  // Helper to parse INI for logging without modifying DRAMSim3
  auto get_ini_val = [&](std::string field) -> uint64_t {
    std::ifstream ifile(config_file);
    std::string line;
    while (std::getline(ifile, line)) {
      size_t pos = line.find("=");
      if (pos != std::string::npos) {
        std::string key = line.substr(0, pos);
        // Trim whitespace from key
        key.erase(0, key.find_first_not_of(" \t"));
        key.erase(key.find_last_not_of(" \t") + 1);
        if (key == field) {
          std::string val = line.substr(pos + 1);
          // Trim whitespace from val
          val.erase(0, val.find_first_not_of(" \t"));
          val.erase(val.find_last_not_of(" \t\r\n") + 1);
          try { return std::stoull(val); } catch (...) { return 0; }
        }
      }
    }
    return 0;
  };

  uint64_t dram_cap_mb = get_ini_val("channel_size");
  uint64_t bus_width   = get_ini_val("bus_width");
  uint64_t dev_width   = get_ini_val("device_width");
  uint64_t ranks       = get_ini_val("ranks");
  if (ranks == 0) ranks = 1; // Default to 1 rank if not specified
  
  uint64_t devices = (dev_width > 0) ? (bus_width / dev_width) : 0;

  // Print initialization summary analogous to DRAMSim2's startup messages
  int bus_bits    = mem->GetBusBits();
  int burst_len   = mem->GetBurstLength();
  double tCK_ns   = mem->GetTCK();
  uint64_t soc_mem_mb = mem_sz / (1024*1024);

  fprintf(stderr, "===== MemorySystem %d =====\n", channel_id);
  fprintf(stderr, "CH. %d SoC REQUESTED MEM SIZE : %lluMB\n", channel_id, (unsigned long long)soc_mem_mb);
  fprintf(stderr, "CH. %d DRAM TOTAL STORAGE     : %lluMB | %llu Ranks | %llu Devices per rank\n",
          channel_id, (unsigned long long)dram_cap_mb, (unsigned long long)ranks, (unsigned long long)devices);
  fprintf(stderr, "DRAMSim3 tCK: %.3f ns (%.0f MHz) | CPU Frequency: %llu Hz\n", 
          tCK_ns, 1000.0 / tCK_ns, (unsigned long long)cpu_hz);
  fprintf(stderr, "===========================\n");
  fflush(stderr);
}

mm_dramsim3_t::~mm_dramsim3_t() {
  fprintf(stderr, "DRAMSim3: Printing Statistics\n");
  mem->PrintStats();
  delete mem;
}

bool mm_dramsim3_t::ar_ready() {
  return rreq_queue.size() < 64;
}

bool mm_dramsim3_t::aw_ready() {
  return wreq_queue.size() < 64 && !store_inflight;
}

void mm_dramsim3_t::tick(
  bool reset,

  bool ar_valid,
  uint64_t ar_addr,
  uint64_t ar_id,
  uint64_t ar_size,
  uint64_t ar_len,

  bool aw_valid,
  uint64_t aw_addr,
  uint64_t aw_id,
  uint64_t aw_size,
  uint64_t aw_len,

  bool w_valid,
  uint64_t w_strb,
  void *w_data,
  bool w_last,

  bool r_ready,
  bool b_ready)
{
  bool ar_fire = !reset && ar_valid && ar_ready();
  bool aw_fire = !reset && aw_valid && aw_ready();
  bool w_fire = !reset && w_valid && w_ready();
  bool r_fire = !reset && r_valid() && r_ready;
  bool b_fire = !reset && b_valid() && b_ready;

  for (auto it = rreq_queue.begin(); it != rreq_queue.end(); ) {
    if (mem->WillAcceptTransaction(it->addr, false)) {
      if (!read_id_busy[it->id]) {
        read_id_busy[it->id] = true;
        auto transaction = *it;
        rreq[transaction.addr].push(transaction);
        mem->AddTransaction(transaction.addr, false);
        it = rreq_queue.erase(it);
        continue;
      }
    }
    ++it;
  }

  for (auto it = wreq_queue.begin(); it != wreq_queue.end(); ) {
    if (mem->WillAcceptTransaction(it->first, true)) {
      mem->AddTransaction(it->first, true);
      wreq[it->first].push(it->second);
      it = wreq_queue.erase(it);
    } else {
      ++it;
    }
  }

  if (ar_fire) {
    rreq_queue.push_back(mm_dramsim3_req_t(ar_id, 1 << ar_size, ar_len + 1, ar_addr));
  }

  if (aw_fire) {
    store_addr = aw_addr;
    store_id = aw_id;
    store_count = aw_len + 1;
    store_size = 1 << aw_size;
    store_inflight = true;
  }

  if (w_fire) {
    write(store_addr, (uint8_t*)w_data, w_strb, store_size);
    store_addr += store_size;
    store_count--;

    if (store_count == 0) {
      store_inflight = false;
      wreq_queue.push_back({store_addr, store_id});
      assert(w_last);
    }
  }

  if (b_fire)
    bresp.pop();

  if (r_fire)
    rresp.pop();

  mem->ClockTick();
  cycle++;

  if (reset) {
    while (!bresp.empty()) bresp.pop();
    while (!rresp.empty()) rresp.pop();
    cycle = 0;
  }
}
