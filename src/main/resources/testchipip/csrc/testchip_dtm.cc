// See LICENSE.SiFive for license details.

#include "testchip_dtm.h"
#include <riscv/encoding.h>
#include <fesvr/dtm.h>
#include <riscv/debug_defines.h>
#include <fstream>
#include <vector>
#include "loadarch.h"

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
#define ADDI(dst, src, imm) (0x13 | ((dst) << 7) | ((src) << 15) | (uint32_t)ENCODE_ITYPE_IMM(imm))
#define VL1R(dst, base) (0x02800007 | ((dst) << 7) | (base << 15))
#define VSETVL(dst, rs1, rs2) (0x80007057 | ((dst) << 7) | ((rs1) << 15) | ((rs2) << 20))
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

#define MAX_VLEN (8 * 32) // Limited by debug module capacity

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
    dtm = new testchip_dtm_t(info.argc, info.argv, true); // TODO: only support loadmem if we have loadmem
  }

  dtm_t::resp resp_bits;
  resp_bits.resp = debug_resp_bits_resp;
  resp_bits.data = debug_resp_bits_data;

  dtm->tick(debug_req_ready,
            debug_resp_valid,
            resp_bits);

  *debug_resp_ready = dtm->resp_ready();
  *debug_req_valid = dtm->req_valid();
  *debug_req_bits_addr = dtm->req_bits().addr;
  *debug_req_bits_op = dtm->req_bits().op;
  *debug_req_bits_data = dtm->req_bits().data;

  return dtm->done() ? (dtm->exit_code() << 1 | 1) : 0;
}

testchip_dtm_t::testchip_dtm_t(int argc, char** argv, bool can_have_loadmem) : dtm_t(argc, argv), loadarch_done(false)
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
  testchip_htif_t::parse_htif_args(args);
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

// TODO: Check that DM_ABSTRACTCS_DATACOUNT is large enough for a vreg
void testchip_dtm_t::loadarch_restore_vreg(uint32_t regno, unsigned char* reg, size_t bytes) {
  uint32_t data[MAX_VLEN / (8 * 4)];
  for (size_t i = 0; i < bytes; i += 4) {
    memcpy(&data[i/4], reg + i, 4);
  }
  uint32_t prog[3] = {
    ADDI(S0, X0, get_data_base()),
    VL1R(regno, S0),
    EBREAK
  };
  uint32_t cmd = (AC_ACCESS_REGISTER_POSTEXEC |
                  AC_ACCESS_REGISTER_TRANSFER |
                  AC_ACCESS_REGISTER_WRITE |
                  AC_AR_SIZE(64) |
                  AC_AR_REGNO(0));
  printf("loadarch restoring vreg %d=", regno);
  for (size_t i = bytes; i-- > 0; ) {
    printf("%02x", reg[i]);
  }
  printf("\n");
  AC_RUN_OR_ABORT(cmd, prog, 3, data, bytes / 4);
}

void testchip_dtm_t::loadarch_restore_vtype(uint32_t vtype) {
  uint32_t prog[2] = {
    VSETVL(X0, X0, S0),
    EBREAK
  };
  uint32_t data[1] = {vtype};
  uint32_t cmd = (AC_ACCESS_REGISTER_POSTEXEC |
                  AC_ACCESS_REGISTER_TRANSFER |
                  AC_ACCESS_REGISTER_WRITE |
                  AC_AR_SIZE(64) |
                  AC_AR_REGNO(S0));
  printf("loadarch restoring vtype=%x\n", vtype);
  AC_RUN_OR_ABORT(cmd, prog, 2, data, 1);
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
  testchip_htif_t::perform_init_accesses();
  if (loadarch_file != "") {
    auto parsed_loadarch_file = loadarch_from_file(loadarch_file);
    auto loadarch_state = parsed_loadarch_file.first;
    auto nharts = parsed_loadarch_file.second;
    for (size_t hartsel = 0; hartsel < nharts; hartsel++) {
      loadarch_state_t &state = loadarch_state[hartsel];
      // mtime goes to CLINT
      write_chunk(0x2000000 + 0xbff8, 8, &state.mtime);
      // mtimecmp goes to CLINT
      write_chunk(0x2000000 + 0x4000 + hartsel * 8, 8, &state.mtimecmp);

      halt(hartsel);

      loadarch_restore_csr(CSR_MSTATUS , MSTATUS_FS | MSTATUS_XS | MSTATUS_VS);
      if (state.mstatus & MSTATUS_FS) {
        for (size_t i = 0; i < 32; i++) {
          loadarch_restore_freg(i, state.FPR[i]);
        }
        loadarch_restore_csr(CSR_FCSR     , state.fcsr);
      }
      if (state.mstatus & MSTATUS_VS) {
        for (size_t i = 0; i < 32; i++) {
          loadarch_restore_vreg(i, state.VPR[i], (size_t)state.VLEN / 8);
        }
        loadarch_restore_csr(CSR_VSTART , state.vstart);
        loadarch_restore_csr(CSR_VXSAT  , state.vxsat);
        loadarch_restore_csr(CSR_VXRM   , state.vxrm);
        loadarch_restore_csr(CSR_VCSR   , state.vcsr);
        loadarch_restore_vtype(state.vtype); // vsetvl
      }

      loadarch_restore_csr(CSR_STVEC    , state.stvec);
      loadarch_restore_csr(CSR_SSCRATCH , state.sscratch);
      loadarch_restore_csr(CSR_SEPC     , state.sepc);
      loadarch_restore_csr(CSR_STVAL    , state.stval);
      loadarch_restore_csr(CSR_SATP     , state.satp);
      loadarch_restore_csr(CSR_MSTATUS  , state.mstatus);
      loadarch_restore_csr(CSR_MEDELEG  , state.medeleg);
      loadarch_restore_csr(CSR_MIDELEG  , state.mideleg);
      loadarch_restore_csr(CSR_MIE      , state.mie);
      loadarch_restore_csr(CSR_MTVEC    , state.mtvec);
      loadarch_restore_csr(CSR_MSCRATCH , state.mscratch);
      loadarch_restore_csr(CSR_MEPC     , state.mepc);
      loadarch_restore_csr(CSR_MCAUSE   , state.mcause);
      loadarch_restore_csr(CSR_MTVAL    , state.mtval);
      loadarch_restore_csr(CSR_MIP      , state.mip);
      loadarch_restore_csr(CSR_MCYCLE   , state.mcycle);
      loadarch_restore_csr(CSR_MINSTRET , state.minstret);

      loadarch_restore_csr(CSR_DCSR     , state.prv);
      loadarch_restore_csr(CSR_DPC      , state.pc);

      for (size_t i = 0; i < 32; i++) {
        loadarch_restore_reg(i, state.XPR[i]);
      }
    }

    printf("loadarch resuming harts\n");
    loadarch_done = true;
    for (size_t hartsel = 0; hartsel < nharts; hartsel++) {
      resume(hartsel);
    }
  } else if (write_hart0_msip) {
    // The dtm_t::reset skips the rest of the bootrom
    // Use CLINT to interrupt the core instead
    //dtm_t::reset();
    uint32_t one = 1;
    write_chunk(MSIP_BASE, sizeof(uint32_t), &one);
  }
}
