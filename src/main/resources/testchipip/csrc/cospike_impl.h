#ifndef __COSPIKE_IMPL_H
#define __COSPIKE_IMPL_H

#include <vector>
#include <string>

void cospike_set_sysinfo(
  char* isa,
  int vlen,
  char* priv,
  int pmpregions,
  long long int mem0_base,
  long long int mem0_size,
  long long int mem1_base,
  long long int mem1_size,
  long long int mem2_base,
  long long int mem2_size,
  int nharts,
  char* bootrom,
  std::vector<std::string> &args);

int cospike_cosim(
  long long int cycle,
  long long int hartid,
  int has_wdata,
  int valid,
  long long int iaddr,
  unsigned long int insn,
  int raise_exception,
  int raise_interrupt,
  unsigned long long int cause,
  unsigned long long int wdata,
  int priv);

#endif // __COSPIKE_IMPL_H
