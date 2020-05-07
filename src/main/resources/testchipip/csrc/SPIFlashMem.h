#ifndef __SPI_FLASH_H__
#define __SPI_FLASH_H__

// Command constants
// 3 address byte reads
#define SPI_CMD_READ                0x03
#define SPI_CMD_FAST_READ           0x0B
#define SPI_CMD_QUAD_O_FAST_READ    0x6B
#define SPI_CMD_QUAD_IO_FAST_READ   0xEB
// 4 address byte reads
#define SPI_CMD_READ4               0x13
#define SPI_CMD_FAST_READ4          0x0C
#define SPI_CMD_QUAD_O_FAST_READ4   0x6C
#define SPI_CMD_QUAD_IO_FAST_READ4  0xEC

#define SPI_CMD_4BYTE_ADDR(x) \
  ((x == SPI_CMD_READ4) || \
   (x == SPI_CMD_FAST_READ4) || \
   (x == SPI_CMD_QUAD_O_FAST_READ4) || \
   (x == SPI_CMD_QUAD_IO_FAST_READ4))

#define SPI_CMD_HAS_DUMMY(x) \
  ((x == SPI_CMD_FAST_READ) || \
   (x == SPI_CMD_FAST_READ4) || \
   (x == SPI_CMD_QUAD_O_FAST_READ) || \
   (x == SPI_CMD_QUAD_O_FAST_READ4) || \
   (x == SPI_CMD_QUAD_IO_FAST_READ) || \
   (x == SPI_CMD_QUAD_IO_FAST_READ4))

#define SPI_CMD_QUAD_I(x) \
  ((x == SPI_CMD_QUAD_IO_FAST_READ) || \
   (x == SPI_CMD_QUAD_IO_FAST_READ4))

#define SPI_CMD_QUAD_O(x) \
  ((x == SPI_CMD_QUAD_O_FAST_READ) || \
   (x == SPI_CMD_QUAD_IO_FAST_READ) || \
   (x == SPI_CMD_QUAD_O_FAST_READ4) || \
   (x == SPI_CMD_QUAD_IO_FAST_READ4))

#define SPI_CMD_VALID(x) \
  ((x == SPI_CMD_READ) || \
   (x == SPI_CMD_FAST_READ) || \
   (x == SPI_CMD_QUAD_O_FAST_READ) || \
   (x == SPI_CMD_QUAD_IO_FAST_READ) || \
   (x == SPI_CMD_READ4) || \
   (x == SPI_CMD_FAST_READ4) || \
   (x == SPI_CMD_QUAD_O_FAST_READ4) || \
   (x == SPI_CMD_QUAD_IO_FAST_READ4))

// SPI flash model parameters
#define SPI_DUMMY_CYCLES 8

// States
#define SPI_STATE_STANDBY  0
#define SPI_STATE_GET_CMD  1
#define SPI_STATE_GET_ADDR 2
#define SPI_STATE_DUMMY    3
#define SPI_STATE_PUT_DATA 4
#define SPI_STATE_ERROR    5

class SPIFlashMem
{
  public:
    SPIFlashMem(const char *filename, uint32_t max_addr);
    ~SPIFlashMem(void);

    uint8_t read(uint32_t address);
    void tick(bool sck, bool cs, bool reset, uint8_t dq_in, uint8_t *dq_out, uint8_t *dq_drive);

  private:
    FILE *_file;
    uint32_t _max_addr;

    // hardware model values
    // posedge registers
    uint32_t _data_buf;
    uint32_t _data_count;
    uint8_t _state;
    uint8_t _dummy_count;
    uint8_t _addr;
    uint8_t _cmd;
    // negedge registers
    uint8_t _data_out;
    bool _drive_dq;
};

#endif /* __SPI_FLASH_H__ */
