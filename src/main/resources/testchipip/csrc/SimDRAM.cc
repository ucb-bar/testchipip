#include <vpi_user.h>
#include <svdpi.h>
#include <stdint.h>
#include <cassert>
#include <sys/mman.h>
#include <fesvr/memif.h>
#include <fesvr/elfloader.h>

#include "mm_dramsim2.h"

bool use_dramsim = false;
std::string ini_dir = "dramsim2_ini";
std::string loadmem_file = "";
std::map<long long int, backing_data_t> backing_mem_data;

// TODO FIX: This doesn't properly handle striped memory across multiple channels
// The full memory range is duplicated across each channel
extern "C" void *memory_init(
        long long int mem_size,
        long long int word_size,
        long long int line_size,
        long long int id_bits,
        long long int clock_hz,
        long long int mem_base
			     )
{
    mm_t *mm;
    s_vpi_vlog_info info;

    std::string memory_ini = "DDR3_micron_64M_8B_x4_sg15.ini";
    std::string system_ini = "system.ini";
    std::string ini_dir = "dramsim2_ini";

    if (!vpi_get_vlog_info(&info))
      abort();

    for (int i = 1; i < info.argc; i++) {
      std::string arg(info.argv[i]);

      if (arg == "+dramsim")
        use_dramsim = true;
      if (arg.find("+dramsim_ini_dir=") == 0)
        ini_dir = arg.substr(strlen("+dramsim_ini_dir="));
      if (arg.find("+loadmem=") == 0)
        loadmem_file = arg.substr(strlen("+loadmem="));
    }

    if (backing_mem_data.find(mem_base) != backing_mem_data.end()) {
      assert(backing_mem_data[mem_base].size == mem_size);
    } else {
      uint8_t* data = (uint8_t*) mmap(NULL, mem_size, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);

      class loadmem_memif_t : public memif_t {
      public:
        loadmem_memif_t(uint8_t* _data, size_t _start) : memif_t(nullptr), data(_data), start(_start) {}
        void write(addr_t taddr, size_t len, const void* src) override
        {
          addr_t addr = taddr - start;
          memcpy(data + addr, src, len);
        }
        void read(addr_t taddr, size_t len, void* bytes) override {
          assert(false);
        }
        endianness_t get_target_endianness() const override {
          return endianness_little;
        }
      private:
        uint8_t* data;
        size_t start;
      } loadmem_memif(data, mem_base);
      if (loadmem_file != "") {
        reg_t entry;
        load_elf(loadmem_file.c_str(), &loadmem_memif, &entry);
      }

      backing_mem_data[mem_base] = {data, mem_size};
    }

    if (use_dramsim)
      mm = (mm_t *) (new mm_dramsim2_t(mem_base, mem_size, word_size, line_size,
                                       backing_mem_data[mem_base],
                                       memory_ini, system_ini, ini_dir,
                                       1 << id_bits, clock_hz));
    else
      mm = (mm_t *) (new mm_magic_t(mem_base, mem_size, word_size, line_size,
                                    backing_mem_data[mem_base]));


    return mm;
}

extern "C" void memory_tick(
        void *channel,

        unsigned char reset,

        unsigned char ar_valid,
        unsigned char *ar_ready,
        long long int ar_addr,
        int ar_id,
        int ar_size,
        int ar_len,

        unsigned char aw_valid,
        unsigned char *aw_ready,
        long long int aw_addr,
        int aw_id,
        int aw_size,
        int aw_len,

        unsigned char w_valid,
        unsigned char *w_ready,
        int w_strb,
        long long w_data,
        unsigned char w_last,

        unsigned char *r_valid,
        unsigned char r_ready,
        int *r_id,
        int *r_resp,
        long long *r_data,
        unsigned char *r_last,

        unsigned char *b_valid,
        unsigned char b_ready,
        int *b_id,
        int *b_resp)
{
    mm_t *mm = (mm_t *) channel;

    mm->tick(
        reset,

        ar_valid,
        ar_addr,
        ar_id,
        ar_size,
        ar_len,

        aw_valid,
        aw_addr,
        aw_id,
        aw_size,
        aw_len,

        w_valid,
        w_strb,
        &w_data,
        w_last,

        r_ready,
        b_ready);

    *ar_ready = mm->ar_ready();
    *aw_ready = mm->aw_ready();
    *w_ready = mm->w_ready();
    *r_valid = mm->r_valid();
    *r_id = mm->r_id();
    *r_resp = mm->r_resp();
    *r_data = *((long *) mm->r_data());
    *r_last = mm->r_last();
    *b_valid = mm->b_valid();
    *b_id = mm->b_id();
    *b_resp = mm->b_resp();
}
