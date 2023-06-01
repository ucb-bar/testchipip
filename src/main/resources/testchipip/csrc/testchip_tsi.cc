#include "testchip_tsi.h"
#include <stdexcept>

testchip_tsi_t::testchip_tsi_t(int argc, char** argv, bool can_have_loadmem) : tsi_t(argc, argv)
{
  has_loadmem = false;
  init_accesses = std::vector<init_access_t>();
  write_hart0_msip = true;
  is_loadmem = false;

  std::vector<std::string> args(argv + 1, argv + argc);
  for (auto& arg : args) {
    if (arg.find("+loadmem=") == 0)
      has_loadmem = can_have_loadmem;
  }

  testchip_htif_t::parse_htif_args(args);
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

void testchip_tsi_t::reset()
{
  testchip_htif_t::perform_init_accesses();
  if (write_hart0_msip)
    tsi_t::reset();
}
