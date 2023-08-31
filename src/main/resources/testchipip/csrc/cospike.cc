#include <vector>
#include <string>
#include <vpi_user.h>
#include <svdpi.h>

#include "cospike_impl.h"

extern "C" void cospike_set_sysinfo_wrapper(char* isa, int vlen, char* priv, int pmpregions,
                                    long long int mem0_base, long long int mem0_size,
                                    int nharts,
                                    char* bootrom
                                    )
{
  s_vpi_vlog_info vinfo;
  if (!vpi_get_vlog_info(&vinfo))
    abort();
  std::vector<std::string> args;
  for (int i = 1; i < vinfo.argc; i++) {
    std::string arg(vinfo.argv[i]);
    args.push_back(arg);
  }

  cospike_set_sysinfo(
    isa,
    vlen,
    priv,
    pmpregions,
    mem0_base,
    mem0_size,
    nharts,
    bootrom,
    args
  );
}

extern "C" void cospike_cosim_wrapper(long long int cycle,
                              long long int hartid,
                              int has_wdata,
                              int valid,
                              long long int iaddr,
                              unsigned long int insn,
                              int raise_exception,
                              int raise_interrupt,
                              unsigned long long int cause,
                              unsigned long long int wdata,
                              int priv)
{
  int rval = cospike_cosim(
    cycle,
    hartid,
    has_wdata,
    valid,
    iaddr,
    insn,
    raise_exception,
    raise_interrupt,
    cause,
    wdata,
    priv
  );
  if (rval) exit(rval);
}
