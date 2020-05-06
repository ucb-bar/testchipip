#include <vpi_user.h>
#include <svdpi.h>
#include <cstdlib>
#include <cstring>
#include <cinttypes>
#include <cstdint>
#include <vector>
#include <queue>
#include <fesvr/context.h>

#include "SimSPIFlashModel.h"

// We are returning the pointer to the SPIFlashMem object so that we can have
// multiple SPI flash memories in one sim. The verilog simulator seems to be OK
// with this
extern "C" long long spi_flash_init(const char *filename, int max_addr)
{
  SPIFlashMem *mem = new SPIFlashMem(filename, max_addr);
  return (long long)mem;
}

extern "C" void spi_flash_read(long long mem, int address, char *data)
{
  // Cast the pointer back to SPIFlashMem pointer
  ((SPIFlashMem *)mem)->read((uint32_t)address, (uint8_t *)data);
}

SPIFlashMem::SPIFlashMem(const char *filename, uint32_t max_addr)
{
  long size;
  _max_addr = max_addr;

  _file = fopen(filename, "r");
  if (!_file)
  {
    fprintf(stderr, "Could not open %s\n", filename);
    abort();
  }
  if (fseek(_file, 0, SEEK_END))
  {
    perror("fseek");
    abort();
  }
  size = ftell(_file);
  if (size < 0)
  {
    perror("ftell");
    abort();
  }
}

SPIFlashMem::~SPIFlashMem(void)
{
  fclose(_file);
}

void SPIFlashMem::read(uint32_t address, uint8_t *data)
{

  uint8_t buf[0];

  if (address > _max_addr)
  {
    fprintf(stderr, "Read out of bounds: 0x%016x >= 0x%016x\n", address, _max_addr);
    abort();
  }

  if (fseek(_file, address, SEEK_SET))
  {
    fprintf(stderr, "Could not seek to 0x%016x\n", address);
    abort();
  }

  if (fread(data, 1, 1, _file) == 0)
  {
    fprintf(stderr, "Cannot read data at 0x%016x\n", address);
    abort();
  }

}
