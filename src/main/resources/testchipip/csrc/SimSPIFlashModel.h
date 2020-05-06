#ifndef __SPI_FLASH_H__
#define __SPI_FLASH_H__

class SPIFlashMem
{
  public:
    SPIFlashMem(const char *filename, uint32_t max_addr);
    ~SPIFlashMem(void);

    void read(uint32_t address, uint8_t *data);

  private:
    FILE *_file;
    uint32_t _max_addr;
};

#endif /* __SPI_FLASH_H__ */
