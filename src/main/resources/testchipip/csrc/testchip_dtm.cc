// See LICENSE.SiFive for license details.

#include "testchip_dtm.h"
#include <riscv/encoding.h>
#include <fesvr/dtm.h>
#include <riscv/debug_defines.h>
#include <vpi_user.h>
#include <svdpi.h>
#include <fstream>
#include <vector>

#define RV_X(x, s, n) \
  (((x) >> (s)) & ((1 << (n)) - 1))
#define ENCODE_ITYPE_IMM(x) \
  (RV_X(x, 0, 12) << 20)
#define LOAD(xlen, dst, base, imm) \
  (((xlen) == 64 ? 0x00003003 : 0x00002003) \
   | ((dst) << 7) | ((base) << 15) | (uint32_t)ENCODE_ITYPE_IMM(imm))
#define FLOAD(flen, dst, base, imm)                                     \
  (((flen) == 64 ? 0x00003007 : 0x00002007)                             \
   | ((dst) << 7) | ((base) << 15) | (uint32_t)ENCODE_ITYPE_IMM(imm))
#define EBREAK  0x00100073
#define X0 0
#define S0 8
#define S1 9
#define WRITE 1
#define SET 2
#define CLEAR 3
#define CSRRx(type, dst, csr, src) (0x73 | ((type) << 12) | ((dst) << 7) | ((src) << 15) | (uint32_t)((csr) << 20))

#define AC_AR_REGNO(x) ((0x1000 | x) << AC_ACCESS_REGISTER_REGNO_OFFSET)
#define AC_AR_SIZE(x)  (((x == 128)? 4 : (x == 64 ? 3 : 2)) << AC_ACCESS_REGISTER_AARSIZE_OFFSET)

#define AC_RUN_OR_ABORT(a, b, c, d, e) {                                \
  uint32_t cmderr = run_abstract_command(a, b, c, d, e);                \
  if (cmderr) {                                                         \
    printf("Error %x\n", cmderr);                                       \
    abort();                                                            \
  }                                                                     \
  }

#define MSIP_BASE 0x2000000

testchip_dtm_t* dtm;

extern "C" int debug_tick
(
  unsigned char* debug_req_valid,
  unsigned char  debug_req_ready,
  int*           debug_req_bits_addr,
  int*           debug_req_bits_op,
  int*           debug_req_bits_data,
  unsigned char  debug_resp_valid,
  unsigned char* debug_resp_ready,
  int            debug_resp_bits_resp,
  int            debug_resp_bits_data
)
{
  if (!dtm) {
    s_vpi_vlog_info info;
    if (!vpi_get_vlog_info(&info))
      abort();
    dtm = new testchip_dtm_t(info.argc, info.argv, true); // TODO: only suppor tloadmem if we have loadmem
  }

  dtm_t::resp resp_bits;
  resp_bits.resp = debug_resp_bits_resp;
  resp_bits.data = debug_resp_bits_data;

  dtm->tick
  (
    debug_req_ready,
    debug_resp_valid,
    resp_bits
   );

  *debug_resp_ready = dtm->resp_ready();
  *debug_req_valid = dtm->req_valid();
  *debug_req_bits_addr = dtm->req_bits().addr;
  *debug_req_bits_op = dtm->req_bits().op;
  *debug_req_bits_data = dtm->req_bits().data;

  return dtm->done() ? (dtm->exit_code() << 1 | 1) : 0;
}

testchip_dtm_t::testchip_dtm_t(int argc, char** argv, bool can_have_loadmem) : dtm_t(argc, argv)
{
  has_loadmem = false;
  is_loadmem = false;
  loadarch_file = "";
  std::vector<std::string> args(argv + 1, argv + argc);
  for (auto& arg : args) {
    if (arg.find("+loadmem=") == 0)
      has_loadmem = can_have_loadmem;
    if (arg.find("+loadarch=") == 0)
      loadarch_file = arg.substr(strlen("+loadarch="));
  }
}


void testchip_dtm_t::write_chunk(addr_t taddr, size_t nbytes, const void* src)
{
  if (is_loadmem) {
    load_mem_write(taddr, nbytes, src);
  } else {
    dtm_t::write_chunk(taddr, nbytes, src);
  }
}

void testchip_dtm_t::read_chunk(addr_t taddr, size_t nbytes, void* dst)
{
  if (is_loadmem) {
    load_mem_read(taddr, nbytes, dst);
  } else {
    dtm_t::read_chunk(taddr, nbytes, dst);
  }
}

reg_t read_priv(std::string priv_str) {
  reg_t prv;
  if (priv_str == "U") {
    prv = 0;
  } else if (priv_str == "S") {
    prv = 1;
  } else if (priv_str == "M") {
    prv = 3;
  } else {
    printf("loadarch illegal privilege mode %s\n", priv_str.c_str());
    abort();
  }
  return prv;
}


void testchip_dtm_t::loadarch_restore_reg(uint32_t regno, reg_t reg) {
  uint32_t data[2];
  data[0] = (uint32_t) reg;
  data[1] = (uint32_t) (reg >> 32);
  uint32_t cmd = (AC_ACCESS_REGISTER_TRANSFER |
                  AC_ACCESS_REGISTER_WRITE |
                  AC_AR_SIZE(64) |
                  AC_AR_REGNO(regno));
  printf("loadarch restoring reg %d=%lx\n", regno, reg);
  AC_RUN_OR_ABORT(cmd, 0, 0, data, 2);
}

void testchip_dtm_t::loadarch_restore_freg(uint32_t regno, reg_t reg) {
  uint32_t prog[2] = {
    FLOAD(64, regno, X0, get_data_base()),
    EBREAK
  };
  uint32_t data[2];
  data[0] = (uint32_t) reg;
  data[1] = (uint32_t) (reg >> 32);
  uint32_t cmd = (AC_ACCESS_REGISTER_POSTEXEC |
                  AC_ACCESS_REGISTER_TRANSFER |
                  AC_ACCESS_REGISTER_WRITE |
                  AC_AR_SIZE(64) |
                  AC_AR_REGNO(0));
  printf("loadarch restoring freg %d=%lx\n", regno, reg);
  AC_RUN_OR_ABORT(cmd, prog, 2, data, 2);
}

void testchip_dtm_t::loadarch_restore_csr(uint32_t regno, reg_t reg) {
  uint32_t prog[5] = {
    CSRRx(WRITE, S0, CSR_DSCRATCH0, S0),
    LOAD(64, S0, X0, get_data_base()),
    CSRRx(WRITE, S0, regno, S0),
    CSRRx(WRITE, S0, CSR_DSCRATCH0, S0),
    EBREAK
  };
  uint32_t data[2];
  data[0] = (uint32_t) reg;
  data[1] = (uint32_t) (reg >> 32);
  uint32_t cmd = (AC_ACCESS_REGISTER_POSTEXEC |
                  AC_ACCESS_REGISTER_TRANSFER |
                  AC_ACCESS_REGISTER_WRITE |
                  AC_AR_SIZE(64) |
                  AC_AR_REGNO(0));
  printf("loadarch restoring csr %x=%lx\n", regno, reg);
  AC_RUN_OR_ABORT(cmd, prog, 5, data, 2);
}

void testchip_dtm_t::reset()
{
  if (loadarch_file != "") {
    printf("loadarch attempting to load architectural state from %s\n", loadarch_file.c_str());
    std::string line;
    std::ifstream in(loadarch_file);

    if (!in.is_open()) {
      printf("loadarch could not load architectural state file %s\n", loadarch_file.c_str());
      abort();
    }

    std::vector<std::string> lines;
    while (std::getline(in, line)) {
      lines.push_back(line);
    }

    const size_t LOADARCH_LINES_PER_HART = 87;
    lines.erase(lines.begin()); // first line is garbage ':' character

    // TODO FIX THIS BRITTLE CODE
    if (lines.size() % LOADARCH_LINES_PER_HART != 0 || lines.size() == 0) {
      printf("loadarch improper file format %s\n", loadarch_file.c_str());
      abort();
    }

    size_t nharts = lines.size() / LOADARCH_LINES_PER_HART;

    size_t id = 0;
    for (size_t hartsel = 0; hartsel < nharts; hartsel++) {
      reg_t pc       = std::stoull(lines[id++], nullptr, 0);
      reg_t prv      = read_priv(lines[id++]);

      reg_t fcsr     = std::stoull(lines[id++], nullptr, 0);

      reg_t stvec    = std::stoull(lines[id++], nullptr, 0);
      reg_t sscratch = std::stoull(lines[id++], nullptr, 0);
      reg_t sepc     = std::stoull(lines[id++], nullptr, 0);
      reg_t scause   = std::stoull(lines[id++], nullptr, 0);
      reg_t stval    = std::stoull(lines[id++], nullptr, 0);
      reg_t satp     = std::stoull(lines[id++], nullptr, 0);

      reg_t mstatus  = std::stoull(lines[id++], nullptr, 0);
      reg_t medeleg  = std::stoull(lines[id++], nullptr, 0);
      reg_t mideleg  = std::stoull(lines[id++], nullptr, 0);
      reg_t mie      = std::stoull(lines[id++], nullptr, 0);
      reg_t mtvec    = std::stoull(lines[id++], nullptr, 0);
      reg_t mscratch = std::stoull(lines[id++], nullptr, 0);
      reg_t mepc     = std::stoull(lines[id++], nullptr, 0);
      reg_t mcause   = std::stoull(lines[id++], nullptr, 0);
      reg_t mtval    = std::stoull(lines[id++], nullptr, 0);
      reg_t mip      = std::stoull(lines[id++], nullptr, 0);

      reg_t mcycle   = std::stoull(lines[id++], nullptr, 0);
      reg_t minstret = std::stoull(lines[id++], nullptr, 0);

      reg_t mtime    = std::stoull(lines[id++], nullptr, 0);
      reg_t mtimecmp = std::stoull(lines[id++], nullptr, 0);

      reg_t fregs[32];
      for (size_t i = 0; i < 32; i++) {
        // Spike prints 128b-wide floats, which this doesn't support
	fregs[i] = std::stoull(lines[id++].substr(18), nullptr, 16);
      }
      reg_t regs[32];
      for (size_t i = 0; i < 32; i++) {
	regs[i] = std::stoull(lines[id++], nullptr, 0);
      }


      // mtime goes to CLINT
      write_chunk(0x2000000 + 0xbff8, 8, &mtime);
      // mtimecmp goes to CLINT
      write_chunk(0x2000000 + 0x4000 + hartsel * 8, 8, &mtimecmp);


      halt(hartsel);

      // Restore FREG has to happen first to FS bit gets set
      // correctly by fcsr. Enable FPU first
      loadarch_restore_csr(CSR_MSTATUS , MSTATUS_FS | MSTATUS_XS | MSTATUS_VS);
      for (size_t i = 0; i < 32; i++) {
	loadarch_restore_freg(i, regs[i]);
      }

      loadarch_restore_csr(CSR_FCSR     , fcsr);
      loadarch_restore_csr(CSR_STVEC    , stvec);
      loadarch_restore_csr(CSR_SSCRATCH , sscratch);
      loadarch_restore_csr(CSR_SEPC     , sepc);
      loadarch_restore_csr(CSR_STVAL    , stval);
      loadarch_restore_csr(CSR_SATP     , satp);
      loadarch_restore_csr(CSR_MSTATUS  , mstatus);
      loadarch_restore_csr(CSR_MEDELEG  , medeleg);
      loadarch_restore_csr(CSR_MIDELEG  , mideleg);
      loadarch_restore_csr(CSR_MIE      , mie);
      loadarch_restore_csr(CSR_MTVEC    , mtvec);
      loadarch_restore_csr(CSR_MSCRATCH , mscratch);
      loadarch_restore_csr(CSR_MEPC     , mepc);
      loadarch_restore_csr(CSR_MCAUSE   , mcause);
      loadarch_restore_csr(CSR_MTVAL    , mtval);
      loadarch_restore_csr(CSR_MIP      , mip);
      loadarch_restore_csr(CSR_MCYCLE   , mcycle);
      loadarch_restore_csr(CSR_MINSTRET , minstret);

      loadarch_restore_csr(CSR_DCSR     , prv);
      loadarch_restore_csr(CSR_DPC      , pc);

      for (size_t i = 0; i < 32; i++) {
	loadarch_restore_reg(i, regs[i]);
      }
    }
    assert(id == lines.size());

    printf("loadarch resuming harts\n");
    for (size_t hartsel = 0; hartsel < nharts; hartsel++) {
      resume(hartsel);
    }
  } else {
    // The dtm_t::reset skips the rest of the bootrom
    // Use CLINT to interrupt the core instead
    //dtm_t::reset();
    uint32_t one = 1;
    write_chunk(MSIP_BASE, sizeof(uint32_t), &one);
  }
}
