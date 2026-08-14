#ifndef __COSPIKE_IMPL_H
#define __COSPIKE_IMPL_H

#include <vector>
#include <string>

void cospike_set_sysinfo(
  char* isa,
  char* priv,
  int pmpregions,
  int maxpglevels,
  unsigned long long int mem0_base,
  unsigned long long int mem0_size,
  unsigned long long int mem1_base,
  unsigned long long int mem1_size,
  unsigned long long int mem2_base,
  unsigned long long int mem2_size,
  int nharts,
  char* bootrom,
  std::vector<std::string> &args);

int cospike_cosim(
  unsigned long long int cycle,
  unsigned long long int hartid,
  int has_wdata,
  int valid,
  unsigned long long int iaddr,
  unsigned long int insn,
  int raise_exception,
  int raise_interrupt,
  unsigned long long int cause,
  unsigned long long int wdata,
  int priv);

void cospike_register_memory(
  unsigned long long int base,
  unsigned long long int size);

// All three are optional and must be called before the first cospike_cosim.
// A caller that uses none of them gets the previous behaviour.

// A non-memory region of the address map. Reads take the target's value, since
// what an I/O region returns is defined by the platform, not the ISA.
void cospike_register_device(
  unsigned long long int base,
  unsigned long long int size);

// A CSR the target implements and spike does not. mask and init are the target's.
void cospike_register_csr(
  unsigned long long int addr,
  unsigned long long int mask,
  unsigned long long int init);

// Widths that WARL narrowing depends on. Zero means "not supplied".
void cospike_set_target_params(
  int paddrbits,
  int vaddrbitsextended,
  int npmpcsrs);

#endif // __COSPIKE_IMPL_H
