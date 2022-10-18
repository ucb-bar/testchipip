#ifndef __TESTCHIP_TSI_H
#define __TESTCHIP_TSI_H

#include <stdexcept>

#include <fesvr/tsi.h>
#include <fesvr/htif.h>

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
  memif_endianness_t get_target_endianness() const { return memif_endianness_little; }

 protected:
  virtual void load_mem_write(addr_t taddr, size_t nbytes, const void* src) { };
  virtual void load_mem_read(addr_t taddr, size_t nbytes, void* dst) { };
  void reset() override;
  bool has_loadmem;

 private:

  bool is_loadmem;
  bool write_hart0_msip;
  std::vector<std::pair<uint64_t, uint32_t>> init_writes;
};
#endif
