#include <string>
#include <vector>
#include <riscv/processor.h>
#include "loadarch.h"

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

void print_array_32(reg_t arr[], std::string name) {
  printf("%s:'{", name.c_str());
  for (int i = 0; i < 32; ++i) {
    printf("'h%lx", arr[i]);
    if (i != 31) printf(", ");
  }
  printf("}");
}

void print_array_32(unsigned char* arr[], std::string name) {
  printf("%s:'{", name.c_str());
  for (int i = 0; i < 32; ++i) {
    printf("\"%s\"", arr[i]);
    if (i != 31) printf(", ");
  }
  printf("}");
}

void print_loadarch_state(loadarch_state_t state) {
  printf("Loadarch struct: '{pc:'h%lx, prv:'h%lx, fcsr:'h%lx, vstart:'h%lx, vxsat:'h%lx, vxrm:'h%lx, vcsr:'h%lx, vtype:'h%lx, stvec:'h%lx, sscratch:'h%lx, sepc:'h%lx, scause:'h%lx, stval:'h%lx, satp:'h%lx, mstatus:'h%lx, medeleg:'h%lx, mideleg:'h%lx, mie:'h%lx, mtvec:'h%lx, mscratch:'h%lx, mepc:'h%lx, mcause:'h%lx, mtval:'h%lx, mip:'h%lx, mcycle:'h%lx, minstret:'h%lx, mtime:'h%lx, mtimecmp:'h%lx, ",
      state.pc,
      state.prv,
      state.fcsr,
      state.vstart,
      state.vxsat,
      state.vxrm,
      state.vcsr,
      state.vtype,
      state.stvec,
      state.sscratch,
      state.sepc,
      state.scause,
      state.stval,
      state.satp,
      state.mstatus,
      state.medeleg,
      state.mideleg,
      state.mie,
      state.mtvec,
      state.mscratch,
      state.mepc,
      state.mcause,
      state.mtval,
      state.mip,
      state.mcycle,
      state.minstret,
      state.mtime,
      state.mtimecmp
  );
  print_array_32(state.XPR, "XPR");
  printf(" , ");
  print_array_32(state.FPR, "FPR");
  printf(" , ");
  printf("VLEN:'h%lx, ELEN:'h%lx, ", state.VLEN, state.ELEN);
  print_array_32(state.VPR, "VPR");
  printf(" }\n");
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
      printf("Loadarch VLEN %ld > %d. Aborting\n", state.VLEN, MAX_VLEN);
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
  char* loadarch_file,
  loadarch_state_t* loadarch_state
)
{
  auto retval = loadarch_from_file(std::string(loadarch_file));
  assert(retval.second == 1); // assume only 1 hart since passing a std::vector into SystemVerilog is iffy
  auto state = retval.first[0]; // `state` is stack allocated
  print_loadarch_state(state);
  // `loadarch_state` is allocated in the SystemVerilog simulator memory space
  // need to deep copy `state` to `loadarch_state`, which I assume this does
  *loadarch_state = state;
}
