// See LICENSE for license details.

#ifndef _MM_EMULATOR_DRAMSIM2_H
#define _MM_EMULATOR_DRAMSIM2_H

#include "mm.h"
#include <DRAMSim.h>
#include <map>
#include <queue>
#include <list>
#include <stdint.h>

struct mm_reorder_entry_t
{
  uint64_t reqnum;
  mm_rresp_t resp;
  bool valid;

  mm_reorder_entry_t(uint64_t reqnum, mm_rresp_t resp, bool valid)
  {
    this->reqnum = reqnum;
    this->resp = resp;
    this->valid = valid;
  }

  mm_reorder_entry_t()
  {
    this->reqnum = 0;
    this->valid = false;
  }
};

struct mm_reorder_ref_t
{
  uint64_t reqnum;
  uint64_t id;

  mm_reorder_ref_t(uint64_t reqnum, uint64_t id)
  {
    this->reqnum = reqnum;
    this->id = id;
  }
};

class mm_reorder_buffer_t
{
 public:
  mm_reorder_buffer_t();
  void init(int word_size);

  void add(uint64_t line_addr, mm_rresp_t resp);
  void complete(uint64_t line_addr);
  bool valid(void);
  mm_rresp_t& front(void);
  void pop(void);

 private:
  std::map<uint64_t, std::list<mm_reorder_entry_t> > entries;
  std::map<uint64_t, std::queue<mm_reorder_ref_t> > references;
  uint64_t next_reqnum;
  int64_t cur_id;
  mm_rresp_t dummy_resp;
};

class mm_dramsim2_t : public mm_t
{
 public:
  mm_dramsim2_t() : store_inflight(false) {}

  virtual void init(size_t sz, int word_size, int line_size);

  virtual bool ar_ready() { return mem->willAcceptTransaction(); }
  virtual bool aw_ready() { return mem->willAcceptTransaction() && !store_inflight; }
  virtual bool w_ready() { return store_inflight; }
  virtual bool b_valid() { return !bresp.empty(); }
  virtual uint64_t b_resp() { return 0; }
  virtual uint64_t b_id() { return b_valid() ? bresp.front() : 0; }
  virtual bool r_valid() { return rreorder.valid(); }
  virtual uint64_t r_resp() { return 0; }
  virtual uint64_t r_id() { return rreorder.front().id; }
  virtual void *r_data() { return &rreorder.front().data[0]; }
  virtual bool r_last() { return rreorder.front().last; }

  virtual void tick
  (
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
    bool b_ready
  );


 protected:
  DRAMSim::MultiChannelMemorySystem *mem;
  uint64_t cycle;

  bool store_inflight;
  uint64_t store_addr;
  uint64_t store_id;
  uint64_t store_size;
  uint64_t store_count;
  std::queue<uint64_t> bresp;
  std::map<uint64_t, std::queue<uint64_t> > wreq;

  mm_reorder_buffer_t rreorder;

  void read_complete(unsigned id, uint64_t address, uint64_t clock_cycle);
  void write_complete(unsigned id, uint64_t address, uint64_t clock_cycle);
};

#endif
