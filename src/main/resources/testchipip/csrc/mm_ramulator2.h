// See LICENSE for license details.

#ifndef _MM_EMULATOR_RAMULATOR2_H
#define _MM_EMULATOR_RAMULATOR2_H

#include "mm.h"

#include <map>
#include <queue>
#include <list>
#include <vector>
#include <stdint.h>
#include <string>
#include <utility>

#include "ramulator/base/config.h"
#include "ramulator/base/factory.h"
#include "ramulator/base/request.h"
#include "ramulator/frontend/i_frontend.h"
#include "ramulator/memory_system/i_memory_system.h"

struct mm_ramulator2_req_t {
  uint64_t id;
  uint64_t size;
  uint64_t len;
  uint64_t addr;

  mm_ramulator2_req_t(uint64_t id, uint64_t size, uint64_t len, uint64_t addr)
  {
    this->id = id;
    this->size = size;
    this->len = len;
    this->addr = addr;
  }

  mm_ramulator2_req_t()
  {
    this->id = 0;
    this->size = 0;
    this->len = 0;
    this->addr = 0;
  }
};

class mm_ramulator2_t : public mm_t
{
 public:
  mm_ramulator2_t(size_t mem_base, size_t mem_sz, size_t word_sz, size_t line_sz, backing_data_t& dat, std::string config_path, int axi4_ids, size_t clock_hz);
  virtual ~mm_ramulator2_t();
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
  Ramulator::IFrontEnd* ramulator2_frontend;
  Ramulator::IMemorySystem* ramulator2_memorysystem;
  int ramulator2_tx_bytes;
  std::string config_path;
  uint64_t cycle;


  std::queue<std::pair<uint64_t, uint64_t>> pending_wreq_queue;

  bool store_inflight = false;
  uint64_t store_addr;
  uint64_t store_base_addr;
  uint64_t store_id;
  uint64_t store_size;
  uint64_t store_count;
  std::queue<uint64_t> bresp;

  // Keep a FIFO of IDs that made reads/writes to an address since the memory
  // backend does not track AXI IDs. Reads or writes to the same address from
  // different IDs can collide.
  std::map<uint64_t, std::queue<uint64_t>> wreq;
  std::map<uint64_t, std::queue<mm_ramulator2_req_t>> rreq;
  std::queue<mm_rresp_t> rresp;
  //std::map<uint64_t, std::queue<mm_rresp_t> > rreq;


  // Track inflight requests by putting indexes to their positions in the
  // stimulus vector in queues for each AXI channel
  std::vector<bool> read_id_busy;
  std::vector<bool> write_id_busy;
  std::list<mm_ramulator2_req_t> rreq_queue;

  uint64_t clock_hz = 0;

  void read_complete(uint64_t address);
  void write_complete(uint64_t address);
};

#endif
