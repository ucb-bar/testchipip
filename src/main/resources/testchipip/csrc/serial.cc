#include "serial.h"

chipyard_tsi_t::chipyard_tsi_t(
        int argc, char **argv,
        int nchannels, unsigned long mem_size,
        int word_bytes, int line_bytes, int id_bits)
    : tsi_t(argc, argv)
{
    std::string ini_dir = "dramsim2_ini";
    bool dramsim = false;

    this->nchannels = nchannels;
    this->line_bytes = line_bytes;
    this->mems = (mm_t **) malloc(sizeof(mm_t *) * nchannels);
    this->mem_size = mem_size;
    this->loadprog = false;
    this->fastload = false;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "+dramsim") == 0)
            dramsim = true;
        if (std::string(argv[i]).find("+dramsim_ini_dir=") == 0)
            ini_dir = argv[i] + strlen("+dramsim_ini_dir=");
        if (strcmp(argv[i], "+fastload") == 0)
            this->fastload = true;
    }

    if (dramsim) {
        for (int i = 0; i < nchannels; i++)
            this->mems[i] = (mm_t *) (new mm_dramsim2_t(ini_dir, 1 << id_bits));
    } else {
        for (int i = 0; i < nchannels; i++)
            this->mems[i] = (mm_t *) (new mm_magic_t);
    }

    for (int i = 0; i < nchannels; i++)
        this->mems[i]->init(mem_size, word_bytes, line_bytes, nchannels);
}

chipyard_tsi_t::~chipyard_tsi_t()
{
    for (int i = 0; i < nchannels; i++)
        delete mems[i];
    free(mems);
}

#define min(a, b) ((a < b) ? (a) : (b))

void chipyard_tsi_t::write_chunk_fast(reg_t addr, size_t nbytes, char *src)
{
    while (nbytes > 0) {
        reg_t lineno = addr / line_bytes;
        char *mem = (char *) mems[lineno % nchannels]->get_data();
        reg_t offset = addr % line_bytes;
        reg_t to_copy = min(line_bytes - offset, nbytes);
        char *dst = mem + (lineno / nchannels) * line_bytes + offset;

        memcpy(dst, src, to_copy);

        src += to_copy;
        addr += to_copy;
        nbytes -= to_copy;
    }
}

void chipyard_tsi_t::read_chunk_fast(reg_t addr, size_t nbytes, char *dst)
{
    while (nbytes > 0) {
        reg_t lineno = addr / line_bytes;
        char *mem = (char *) mems[lineno % nchannels]->get_data();
        reg_t offset = addr % line_bytes;
        reg_t to_copy = min(line_bytes - offset, nbytes);
        char *src = mem + (lineno / nchannels) * line_bytes + offset;

        memcpy(dst, src, to_copy);

        dst += to_copy;
        addr += to_copy;
        nbytes -= to_copy;
    }
}

void chipyard_tsi_t::write_chunk(reg_t taddr, size_t nbytes, const void *src)
{
    char *chsrc = (char *) src;
    reg_t addr = taddr % mem_size;

    if (loadprog && nchannels > 0)
        write_chunk_fast(addr, nbytes, chsrc);
    else
        tsi_t::write_chunk(taddr, nbytes, src);
}

void chipyard_tsi_t::read_chunk(reg_t taddr, size_t nbytes, void *dst)
{
    char *chdst = (char *) dst;
    reg_t addr = taddr % mem_size;

    if (loadprog && nchannels > 0)
        read_chunk_fast(addr, nbytes, chdst);
    else
        tsi_t::read_chunk(taddr, nbytes, dst);
}
