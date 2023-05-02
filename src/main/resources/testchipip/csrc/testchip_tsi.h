#ifndef __TESTCHIP_TSI_H
#define __TESTCHIP_TSI_H

#include <stdexcept>

#include <fesvr/tsi.h>
#include <fesvr/htif.h>

struct init_access_t {
  uint64_t address;
  uint32_t stdata;
  bool store;
};

class testchip_tsi_t : public tsi_t
{
 public:
  testchip_tsi_t(int argc, char** argv, bool has_loadmem);
  virtual ~testchip_tsi_t() {};

  void write_chunk(addr_t taddr, size_t nbytes, const void* src) override;
  void read_chunk(addr_t taddr, size_t nbytes, void* dst) override;
  void load_program() {
    switch_to_target();
    is_loadmem = has_loadmem;
    tsi_t::load_program();
    is_loadmem = false;
  }
  void idle() { switch_to_target(); }
  bool addr_should_be_coherent(addr_t taddr);

 protected:
  virtual void load_mem_write(addr_t taddr, size_t nbytes, const void* src) { };
  virtual void load_mem_read(addr_t taddr, size_t nbytes, void* dst) { };
  void reset() override;
  bool has_loadmem;

 private:

  bool is_loadmem;
  bool write_hart0_msip;
  std::vector<init_access_t> init_accesses;

  // By default, all accesses from tsi are incoherent. This
  // breaks communication with coherent private caches.
  //
  // The user can specify a address region which must be accessed
  // through a coherent side channel, the region is given by
  // (coherent_base, coherent_size).
  //
  // The user can also specify a shadow offset that is coherent
  // (coherent_offset).
  // If all three are specified, accesses within the coherent region
  // will be redirected via the offset
  addr_t coherent_base;
  reg_t coherent_size;
  reg_t coherent_offset;
};
#endif
