#ifndef __TESTCHIP_DTM_H
#define __TESTCHIP_DTM_H

#include <string>
#include <fesvr/dtm.h>

class testchip_dtm_t : public dtm_t
{
 public:
  testchip_dtm_t(int argc, char** argv, bool has_loadmem);
  virtual ~testchip_dtm_t() {};

  void write_chunk(addr_t taddr, size_t nbytes, const void* src) override;
  void read_chunk(addr_t taddr, size_t nbytes, void* dst) override;
  void load_program() {
    is_loadmem = has_loadmem;
    dtm_t::load_program();
    is_loadmem = false;
  };
  void reset() override;

 protected:
  virtual void load_mem_write(addr_t taddr, size_t nbytes, const void* src) { };
  virtual void load_mem_read(addr_t taddr, size_t nbytes, void* dst) { };
  bool has_loadmem;

 private:
  bool is_loadmem;
  std::string loadarch_file;

  void loadarch_restore_csr(uint32_t regno, reg_t reg);
  void loadarch_restore_reg(uint32_t regno, reg_t reg);
  void loadarch_restore_freg(uint32_t regno, reg_t reg);
};

#endif
