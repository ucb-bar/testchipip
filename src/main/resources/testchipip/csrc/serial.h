#ifndef __TESTCHIPIP_SERIAL_H__
#define __TESTCHIPIP_SERIAL_H__

#include <fesvr/htif.h>
#include <fesvr/tsi.h>
#include "mm_dramsim2.h"

class chipyard_tsi_t : public tsi_t
{
  public:
    chipyard_tsi_t(int argc, char **argv,
		   int nchannels, unsigned long mem_size,
		   int word_bytes, int line_bytes, int id_bits);
    ~chipyard_tsi_t();

    void load_program() {
        if (fastload)
            printf("Using fastload to load program\n");
        loadprog = fastload;
        tsi_t::load_program();
        loadprog = false;
    }
    void write_chunk(reg_t taddr, size_t nbytes, const void *src);
    void read_chunk(reg_t taddr, size_t nbytes, void *dst);
    mm_t *get_mem(int channel) { return mems[channel]; }

  private:
    mm_t **mems;
    unsigned long mem_size;
    int nchannels;
    int line_bytes;
    bool loadprog;
    bool fastload;

    void write_chunk_fast(reg_t addr, size_t nbytes, char *src);
    void read_chunk_fast(reg_t addr, size_t nbytes, char *dst);
};

#endif
