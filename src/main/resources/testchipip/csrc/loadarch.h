#ifndef __TESTCHIP_LOADARCH_H
#define __TESTCHIP_LOADARCH_H

#include <string>
#include <vector>
#include <riscv/processor.h>

#define MAX_VLEN (8 * 32) // Limited by debug module capacity

extern "C" {
  typedef struct {
    reg_t pc;
    reg_t prv;

    reg_t fcsr;

    reg_t vstart;
    reg_t vxsat;
    reg_t vxrm;
    reg_t vcsr;
    reg_t vtype;

    reg_t stvec;
    reg_t sscratch;
    reg_t sepc;
    reg_t scause;
    reg_t stval;
    reg_t satp;

    reg_t mstatus;
    reg_t medeleg;
    reg_t mideleg;
    reg_t mie;
    reg_t mtvec;
    reg_t mscratch;
    reg_t mepc;
    reg_t mcause;
    reg_t mtval;
    reg_t mip;

    reg_t mcycle;
    reg_t minstret;
    reg_t mtime;
    reg_t mtimecmp;

    reg_t XPR[32];
    reg_t FPR[32];

    reg_t VLEN;
    reg_t ELEN;
    unsigned char* VPR[32];
  } loadarch_state_t;
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

// Returns the loadarch state and # of harts
std::pair<std::vector<loadarch_state_t>, size_t> loadarch_from_file(std::string loadarch_file)
{
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

  const size_t LOADARCH_LINES_PER_HART = 125;
  lines.erase(lines.begin()); // first line is garbage ':' character

  // TODO FIX THIS BRITTLE CODE
  if (lines.size() % LOADARCH_LINES_PER_HART != 0 || lines.size() == 0) {
    printf("loadarch improper file format %s\n", loadarch_file.c_str());
    abort();
  }

  std::vector<loadarch_state_t> loadarch_state;

  size_t nharts = lines.size() / LOADARCH_LINES_PER_HART;
  loadarch_state.resize(nharts);

  size_t id = 0;
  for (size_t hartsel = 0; hartsel < nharts; hartsel++) {
    loadarch_state_t &state = loadarch_state[hartsel];
    state.pc       = std::stoull(lines[id++], nullptr, 0);
    state.prv      = read_priv(lines[id++]);

    state.fcsr     = std::stoull(lines[id++], nullptr, 0);

    state.vstart   = std::stoull(lines[id++], nullptr, 0);
    state.vxsat    = std::stoull(lines[id++], nullptr, 0);
    state.vxrm     = std::stoull(lines[id++], nullptr, 0);
    state.vcsr     = std::stoull(lines[id++], nullptr, 0);
    state.vtype    = std::stoull(lines[id++], nullptr, 0);

    state.stvec    = std::stoull(lines[id++], nullptr, 0);
    state.sscratch = std::stoull(lines[id++], nullptr, 0);
    state.sepc     = std::stoull(lines[id++], nullptr, 0);
    state.scause   = std::stoull(lines[id++], nullptr, 0);
    state.stval    = std::stoull(lines[id++], nullptr, 0);
    state.satp     = std::stoull(lines[id++], nullptr, 0);

    state.mstatus  = std::stoull(lines[id++], nullptr, 0);
    state.medeleg  = std::stoull(lines[id++], nullptr, 0);
    state.mideleg  = std::stoull(lines[id++], nullptr, 0);
    state.mie      = std::stoull(lines[id++], nullptr, 0);
    state.mtvec    = std::stoull(lines[id++], nullptr, 0);
    state.mscratch = std::stoull(lines[id++], nullptr, 0);
    state.mepc     = std::stoull(lines[id++], nullptr, 0);
    state.mcause   = std::stoull(lines[id++], nullptr, 0);
    state.mtval    = std::stoull(lines[id++], nullptr, 0);
    state.mip      = std::stoull(lines[id++], nullptr, 0);

    state.mcycle   = std::stoull(lines[id++], nullptr, 0);
    state.minstret = std::stoull(lines[id++], nullptr, 0);

    state.mtime    = std::stoull(lines[id++], nullptr, 0);
    state.mtimecmp = std::stoull(lines[id++], nullptr, 0);

    for (size_t i = 0; i < 32; i++) {
      // Spike prints 128b-wide floats, which this doesn't support
      state.FPR[i] = std::stoull(lines[id++].substr(18), nullptr, 16);
    }
    reg_t regs[32];
    for (size_t i = 0; i < 32; i++) {
      state.XPR[i] = std::stoull(lines[id++], nullptr, 0);
    }

    std::string vlen = lines[id].substr(lines[id].find("VLEN="));
    vlen = vlen.substr(0, vlen.find(" ")).substr(strlen("VLEN="));
    std::string elen = lines[id].substr(lines[id].find("ELEN="));
    elen = elen.substr(0, elen.find(" ")).substr(strlen("ELEN="));

    state.VLEN = std::stoull(vlen, nullptr, 0);
    state.ELEN = std::stoull(elen, nullptr, 0);
    id++;
    if (state.VLEN > MAX_VLEN) {
      printf("Loadarch VLEN %d > %d. Aborting\n", state.VLEN, MAX_VLEN);
      abort();
    }

    for (size_t i = 0; i < 32; i++) {
      state.VPR[i] = (unsigned char*) malloc(state.VLEN / 8);
      std::string elems_s = lines[id].substr(lines[id].find("0x"));
      for (size_t j = state.VLEN / state.ELEN; j-- > 0;) {
        reg_t elem = std::stoull(elems_s.substr(0, elems_s.find(' ')), nullptr, 0);
    if (j > 0)
      elems_s = elems_s.substr(elems_s.find("0x", 2));
        memcpy(state.VPR[i] + j * (state.ELEN / 8), &elem, state.ELEN / 8);
      }
      id++;
    }
  }
  assert(id == lines.size());
  return std::make_pair(loadarch_state, nharts);
}

extern "C" void loadarch_from_file
(
  int* nharts,
  loadarch_state_t* loadarch_state,
  char* loadarch_file
)
{
  auto retval = loadarch_from_file(std::string(loadarch_file));
}

#endif
