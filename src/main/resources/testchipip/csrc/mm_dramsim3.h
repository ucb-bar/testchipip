// See LICENSE for license details.

#ifndef _MM_EMULATOR_DRAMSIM3_H
#define _MM_EMULATOR_DRAMSIM3_H

#include "mm.h"
#include <dramsim3.h>
#include <map>
#include <queue>
#include <list>
#include <stdint.h>
#include <string>

struct mm_dramsim3_req_t {
  uint64_t id;
  uint64_t size;
  uint64_t len;
  uint64_t addr;

  mm_dramsim3_req_t(uint64_t id, uint64_t size, uint64_t len, uint64_t addr)
  {
    this->id = id;
    this->size = size;
    this->len = len;
    this->addr = addr;
  }

  mm_dramsim3_req_t()
  {
    this->id = 0;
    this->size = 0;
    this->len = 0;
    this->addr = 0;
  }
};

class mm_dramsim3_t : public mm_t
{
 public:
  mm_dramsim3_t(size_t mem_base, size_t mem_sz, size_t word_sz, size_t line_sz, backing_data_t& dat, std::string config_file, std::string output_dir, int axi4_ids, uint64_t cpu_hz, int channel_id = 0);
  ~mm_dramsim3_t();

  virtual bool ar_ready();
  virtual bool aw_ready();
  virtual bool w_ready() { return store_inflight; }
  virtual bool b_valid() { return !bresp.empty(); }
  virtual uint64_t b_resp() { return 0; }
  virtual uint64_t b_id() { return b_valid() ? bresp.front() : 0; }
  virtual bool r_valid() { return !rresp.empty(); }
  virtual uint64_t r_resp() { return 0; }
  virtual uint64_t r_id() { return r_valid() ? rresp.front().id: 0; }
  virtual void *r_data() { return r_valid() ? (void*) &rresp.front().data[0] : data; }
  virtual bool r_last() { return r_valid() ? rresp.front().last : false; }

  virtual void tick
  (
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
    bool b_ready
  );


 protected:
  dramsim3::MemorySystem *mem;
  uint64_t cycle;
  uint64_t cpu_hz;
  int channel_id;

  bool store_inflight = false;
  uint64_t store_addr;
  uint64_t store_id;
  uint64_t store_size;
  uint64_t store_count;
  std::queue<uint64_t> bresp;

  std::map<uint64_t, std::queue<uint64_t>> wreq;
  std::map<uint64_t, std::queue<mm_dramsim3_req_t>> rreq;
  std::queue<mm_rresp_t> rresp;

  std::vector<bool> read_id_busy;
  std::vector<bool> write_id_busy;
  std::list<mm_dramsim3_req_t> rreq_queue;
  std::list<std::pair<uint64_t, uint64_t>> wreq_queue;

  void read_complete(uint64_t address);
  void write_complete(uint64_t address);
};

#endif
