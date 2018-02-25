// See LICENSE for license details.

#include "mm_dramsim2.h"
#include "mm.h"
#include <DRAMSim.h>
#include <iostream>
#include <fstream>
#include <list>
#include <queue>
#include <cstring>
#include <cstdlib>
#include <cassert>

//#define DEBUG_DRAMSIM2

using namespace DRAMSim;

mm_reorder_buffer_t::mm_reorder_buffer_t()
{
  this->cur_id = -1;
  this->next_reqnum = 0;
}

void mm_reorder_buffer_t::init(int word_size)
{
  std::vector<char> dummy_data;
  dummy_data.resize(word_size);
  this->dummy_resp = mm_rresp_t(0, dummy_data, false);
}

void mm_reorder_buffer_t::add(uint64_t line_addr, mm_rresp_t resp)
{
  mm_reorder_entry_t entry(next_reqnum, resp, false);
  entries[resp.id].push_back(entry);

  if (resp.last) {
    references[line_addr].push(mm_reorder_ref_t(next_reqnum, resp.id));
    next_reqnum++;
  }
}

void mm_reorder_buffer_t::complete(uint64_t line_addr)
{
  auto ref = references[line_addr].front();
  for (auto &entry : entries[ref.id]) {
    if (entry.reqnum == ref.reqnum)
      entry.valid = true;
  }
  references[line_addr].pop();
}

bool mm_reorder_buffer_t::valid(void)
{
  if (cur_id >= 0)
    return !entries[cur_id].empty() && entries[cur_id].front().valid;

  for (auto &pair : entries) {
    if (!pair.second.empty() && pair.second.front().valid)
      return true;
  }
  return false;
}

mm_rresp_t& mm_reorder_buffer_t::front(void)
{
  if (cur_id >= 0)
    return entries[cur_id].front().resp;

  for (auto &pair : entries) {
    if (!pair.second.empty() && pair.second.front().valid)
      return pair.second.front().resp;
  }

  return dummy_resp;
}

void mm_reorder_buffer_t::pop(void)
{
  if (cur_id >= 0) {
    bool last = entries[cur_id].front().resp.last;
    entries[cur_id].pop_front();
    if (last)
        cur_id = -1;
    return;
  }

  for (auto &pair : entries) {
    if (!pair.second.empty() && pair.second.front().valid) {
      auto resp = pair.second.front().resp;
      if (!resp.last)
        cur_id = resp.id;
      pair.second.pop_front();
      return;
    }
  }
}

void mm_dramsim2_t::read_complete(unsigned id, uint64_t address, uint64_t clock_cycle)
{
  rreorder.complete(address);
}

void mm_dramsim2_t::write_complete(unsigned id, uint64_t address, uint64_t clock_cycle)
{
  auto b_id = wreq[address].front();
  bresp.push(b_id);
  wreq[address].pop();
}

void power_callback(double a, double b, double c, double d)
{
    //fprintf(stderr, "power callback: %0.3f, %0.3f, %0.3f, %0.3f\n",a,b,c,d);
}

void mm_dramsim2_t::init(size_t sz, int wsz, int lsz)
{
  assert(lsz == 64); // assumed by dramsim2
  mm_t::init(sz, wsz, lsz);

  rreorder.init(word_size);

  assert(size % (1024*1024) == 0);
  mem = getMemorySystemInstance("DDR3_micron_64M_8B_x4_sg15.ini", "system.ini", "dramsim2_ini", "results", size/(1024*1024));

  TransactionCompleteCB *read_cb = new Callback<mm_dramsim2_t, void, unsigned, uint64_t, uint64_t>(this, &mm_dramsim2_t::read_complete);
  TransactionCompleteCB *write_cb = new Callback<mm_dramsim2_t, void, unsigned, uint64_t, uint64_t>(this, &mm_dramsim2_t::write_complete);
  mem->RegisterCallbacks(read_cb, write_cb, power_callback);

#ifdef DEBUG_DRAMSIM2
  fprintf(stderr,"Dramsim2 init successful\n");
#endif
}

void mm_dramsim2_t::tick(
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
  bool ar_fire = ar_valid && ar_ready();
  bool aw_fire = aw_valid && aw_ready();
  bool w_fire = w_valid && w_ready();
  bool r_fire = r_valid() && r_ready;
  bool b_fire = b_valid() && b_ready;

  if (ar_fire) {
    uint64_t start_addr = (ar_addr / word_size) * word_size;
    uint64_t line_addr = (ar_addr / line_size) * line_size;
    for (int i = 0; i <= ar_len; i++) {
      auto dat = read(start_addr + i * word_size);
      rreorder.add(line_addr, mm_rresp_t(ar_id, dat, (i == ar_len)));
    }
    mem->addTransaction(false, line_addr);
  }

  if (aw_fire) {
    store_addr = aw_addr;
    store_id = aw_id;
    store_count = aw_len + 1;
    store_size = 1 << aw_size;
    store_inflight = true;
  }

  if (w_fire) {
    uint64_t line_addr = (store_addr / line_size) * line_size;

    write(store_addr, (uint8_t *) w_data, w_strb, store_size);
    store_addr += store_size;
    store_count--;

    if (store_count == 0) {
      store_inflight = false;
      mem->addTransaction(true, line_addr);
      wreq[line_addr].push(store_id);
      assert(w_last);
    }
  }

  if (b_fire)
    bresp.pop();

  if (r_fire)
    rreorder.pop();

  mem->update();
  cycle++;
}
