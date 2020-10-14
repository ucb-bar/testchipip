#include "testchip_tsi.h"

testchip_tsi_t::testchip_tsi_t(int argc, char** argv, bool can_have_loadmem) : tsi_t(argc, argv)
{
  has_loadmem = false;
  std::vector<std::string> args(argv + 1, argv + argc);
  for (auto& arg : args) {
    if (arg.find("+loadmem=") == 0)
      has_loadmem = can_have_loadmem;
  }
}


void testchip_tsi_t::write_chunk(addr_t taddr, size_t nbytes, const void* src)
{
  if (is_loadmem) {
    load_mem_write(taddr, nbytes, src);
  } else {
    tsi_t::write_chunk(taddr, nbytes, src);
  }
}

void testchip_tsi_t::read_chunk(addr_t taddr, size_t nbytes, void* dst)
{
  if (is_loadmem) {
    load_mem_read(taddr, nbytes, dst);
  } else {
    tsi_t::read_chunk(taddr, nbytes, dst);
  }
}
