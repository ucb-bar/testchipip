#include <vpi_user.h>
#include <svdpi.h>

#include <stdio.h>
#include <unistd.h>

#include "dromajo_wrapper.h"

extern dromajo_t *dromajo;

extern "C" int dromajo_override_mem(
    uint32_t dut_addr,
    uint8_t  dut_size,
    uint64_t dut_wdata)
{
    if (dromajo) {
        printf("[DEBUG] ExtWrite (0x%x, %d, 0x%016lx)\n", dut_addr, dut_size, dut_wdata);
        // TODO: Only works for single hart systems
        return dromajo->override_mem(0, dut_addr, dut_size, dut_wdata);
    } else {
        return 0;
    }
}
