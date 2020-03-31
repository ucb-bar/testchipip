#include <vpi_user.h>
#include <svdpi.h>

#include <stdio.h>
#include <unistd.h>

#include "dromajo_wrapper.h"

// gotten from elaboration
#include "dromajo_params.h"

#define MAX_ARGS 24
#define MAX_STR_LEN 24

dromajo_t *dromajo = 0;

extern "C" int dromajo_init(
    const char* bootrom_file,
    const char* dtb_file,
    const char* binary_file)
{
    // setup arguments
    char *local_argv[MAX_ARGS];
    char local_argc = 0;
    char mmio_range[MAX_STR_LEN] = "";
    char plic_params[MAX_STR_LEN] = "";
    char clint_params[MAX_STR_LEN] = "";

    local_argv[local_argc] = (char*)"./dromajo";
    local_argc += 1;
    local_argv[local_argc] = (char*)"--compact_bootrom";
    local_argc += 1;
    local_argv[local_argc] = (char*)"--custom_extension";
    local_argc += 1;
    local_argv[local_argc] = (char*)"--reset_vector";
    local_argc += 1;
    local_argv[local_argc] = (char*)DROMAJO_RESET_VECTOR;
    local_argc += 1;
    local_argv[local_argc] = (char*)"--bootrom";
    local_argc += 1;
    local_argv[local_argc] = (char*)bootrom_file;
    local_argc += 1;
    local_argv[local_argc] = (char*)"--mmio_range";
    local_argc += 1;
    strcat(mmio_range, (char*)DROMAJO_MMIO_START);
    strcat(mmio_range, ":");
    strcat(mmio_range, (char*)DROMAJO_MMIO_END);
    local_argv[local_argc] = (char*)mmio_range;
    local_argc += 1;
    local_argv[local_argc] = (char*)"--plic";
    local_argc += 1;
    strcat(plic_params, (char*)DROMAJO_PLIC_BASE);
    strcat(plic_params, ":");
    strcat(plic_params, (char*)DROMAJO_PLIC_SIZE);
    local_argv[local_argc] = (char*)plic_params;
    local_argc += 1;
    local_argv[local_argc] = (char*)"--clint";
    local_argc += 1;
    strcat(clint_params, (char*)DROMAJO_CLINT_BASE);
    strcat(clint_params, ":");
    strcat(clint_params, (char*)DROMAJO_CLINT_SIZE);
    local_argv[local_argc] = (char*)clint_params;
    local_argc += 1;
    local_argv[local_argc] = (char*)"--memory_size";
    local_argc += 1;
    local_argv[local_argc] = (char*)DROMAJO_MEM_SIZE;
    local_argc += 1;

    if (strlen(dtb_file) != 0) {
        local_argv[local_argc] = (char*)"--dtb";
        local_argc += 1;
        local_argv[local_argc] = (char*)dtb_file;
        local_argc += 1;
    }

    local_argv[local_argc] = (char*)binary_file;
    local_argc += 1;

    if (MAX_ARGS < local_argc) {
        printf("[DRJ_ERR] Too many arguments\n");
        exit(1);
    }

    printf("[DRJ_INFO] Dromajo command: ");
    for (char i = 0; i < local_argc; ++i)
        printf("%s ", local_argv[i]);
    printf("\n");

    dromajo = new dromajo_t(local_argc, local_argv);
    if (!(dromajo->valid_state())) {
        printf("[DRJ_ERR] Failed Dromajo initialization\n");
        return 1;
    }

    return 0;
}

extern "C" int dromajo_step(
    int      hartid,
    long long dut_pc,
    int dut_insn,
    long long dut_wdata,
    long long mstatus,
    bool     check)
{
    return dromajo->step(hartid, dut_pc, dut_insn, dut_wdata, mstatus, check);
}

extern "C" void dromajo_raise_trap(
    int     hartid,
    long long cause)
{
    dromajo->raise_trap(hartid, cause);
}
