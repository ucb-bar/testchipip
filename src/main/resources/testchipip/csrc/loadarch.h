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

  void loadarch_from_file(
    char* loadarch_file,
    loadarch_state_t* loadarch_state
  );
}

std::pair<std::vector<loadarch_state_t>, size_t> loadarch_from_file(std::string loadarch_file);

#endif
