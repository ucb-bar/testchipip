#include <vpi_user.h>
#include <svdpi.h>
#include <cstdlib>
#include <cstring>
#include <cinttypes>
#include <cstdint>

#include "SPIFlashMem.h"

// We are returning the pointer to the SPIFlashMem object so that we can have
// multiple SPI flash memories in one sim. The verilog simulator seems to be OK
// with this
extern "C" long long spi_flash_init(const char *filename, int max_addr)
{
  SPIFlashMem *mem = new SPIFlashMem(filename, max_addr);
  return (long long)mem;
}

extern "C" void spi_flash_tick(
  long long ptr,
  unsigned char sck,
  unsigned char cs,
  unsigned char reset,
  char dq_in,
  char *dq_out,
  char *dq_drive)
{
  // Cast the pointer back to SPIFlashMem pointer
  ((SPIFlashMem *)ptr)->tick((bool)sck, (bool)cs, (bool)reset, (uint8_t)dq_in, (uint8_t *)dq_out, (uint8_t *)dq_drive);
}
