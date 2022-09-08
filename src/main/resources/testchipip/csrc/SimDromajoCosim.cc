#include <vpi_user.h>
#include <svdpi.h>

#include <stdio.h>
#include <unistd.h>
#include <deque>

#include "dromajo_wrapper.h"

// gotten from chipyard elaboration (originally <long_name>.dromajo_params.h)
#include "dromajo_params.h"

#define MAX_ARGS 24

dromajo_t *dromajo = 0;

extern "C" int dromajo_init(
    const char* bootrom_file,
    const char* dtb_file,
    const char* binary_file)
{
    // setup arguments
    char local_argc = 18;
    char* local_argv[MAX_ARGS] = {
        "./dromajo",
        "--compact_bootrom",
        "--custom_extension",
        "--clear_ids",
        "--reset_vector",
        DROMAJO_RESET_VECTOR,
        "--bootrom",
        (char*)bootrom_file,
        "--mmio_range",
        DROMAJO_MMIO_START ":" DROMAJO_MMIO_END,
        "--plic",
        DROMAJO_PLIC_BASE ":" DROMAJO_PLIC_SIZE,
        "--clint",
        DROMAJO_CLINT_BASE ":" DROMAJO_CLINT_SIZE,
        "--memory_size",
        DROMAJO_MEM_SIZE,
        "--save",
        "dromajo_snap"
    };

    if (strlen(dtb_file) != 0) {
        local_argv[local_argc] = "--dtb";
        local_argv[local_argc+1] = (char*)dtb_file;
        local_argc += 2;
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

struct instrElem {
  bool      wdata_valid;
  long long dut_wdata;
  int      insn_wdata_dest;
  int      insn_writes_back;
  int      hartid;
  long long dut_pc;
  int dut_insn;
  long long mstatus;
  bool     check;
};

void showElem(instrElem i)
{
    printf("wdata_valid     %d\n", i.wdata_valid);
    printf("dut_wdata       %llx\n", i.dut_wdata);
    printf("insn_wdata_dest %x\n", i.insn_wdata_dest);
    printf("insn_writes_back%x\n", i.insn_writes_back);
    printf("hartid          %x\n", i.hartid);
    printf("dut_pc          %llx\n", i.dut_pc);
    printf("dut_insn        %x\n", i.dut_insn);
    printf("mstatus         %llx\n", i.mstatus);
    printf("check           %x\n", i.check);
 }

std::deque<instrElem> instruction_queue;

void showQueue(std::deque<instrElem> q)
{
    printf("PRINTING QUEUE:\n");
    for(auto &i: q)
    {   
        printf("ELEMENT--------------\n");
        showElem(i);
        printf("---------------------\n");
    }
 }

extern "C" int dromajo_step(
    bool     valid,
    int      hartid,
    long long dut_pc,
    int dut_insn,
    long long dut_wdata,
    long long mstatus,
    bool     check,
    bool     wdata_valid,
    int      wdata_dest,
    bool     insn_writes_back,
    int      insn_wdata_dest)
{
    printf("RECEIVEDDDDD");
    printf(".................\n");
    printf("dut_pc          %llx\n", dut_pc);
    printf("dut_insn        %x\n", dut_insn);
    printf("valid           %d\n", valid);
    printf("hartid          %x\n", hartid);
    printf("mstatus         %llx\n", mstatus);
    printf("check           %x\n", check);

    printf("wdata_valid     %d\n", wdata_valid);
    printf("dut_wdata       %llx\n", dut_wdata);
    printf("insn_wdata_dest %x\n", insn_wdata_dest);
    printf("insn_writes_back%x\n", insn_writes_back);
    printf(".................\n");
    // return dromajo->step(hartid, dut_pc, dut_insn, dut_wdata, mstatus, check);
    
    
    if (valid) {
        instrElem instruction_element;
        instruction_element.wdata_valid = false; //init does not have data
        instruction_element.dut_wdata = 0; //init null
        instruction_element.insn_wdata_dest = insn_wdata_dest;
        instruction_element.insn_writes_back = insn_writes_back;

        instruction_element.hartid = hartid;
        instruction_element.dut_pc = dut_pc;
        instruction_element.dut_insn = dut_insn;
        instruction_element.mstatus = mstatus;
        instruction_element.check = check;

        instruction_queue.push_back(instruction_element);
        printf("inputted PC %llx, insn_dest %x, writes back %x \n", dut_pc, insn_wdata_dest, insn_writes_back);
    } else {
        printf("not valid\n");
    }

    if (wdata_valid) {
        for(auto &i: instruction_queue)
        {
            if (i.insn_writes_back && (i.insn_wdata_dest == wdata_dest) && !i.wdata_valid) {
                i.wdata_valid = true;
                i.dut_wdata = dut_wdata;
                printf("inputted dataaa %lx into register %x completing PC %llx\n", i.dut_wdata, i.insn_wdata_dest, i.dut_pc);
                break;
            }
        }
    }

    int ret;
    while (!instruction_queue.empty()) {
        // showQueue(instruction_queue);
        printf("queue not emptyyyyy\n");
        instrElem instr_elem = instruction_queue.front();
        printf("front of queue is PC %llx, writes back %d, dest %x\n", instr_elem.dut_pc, instr_elem.insn_writes_back, instr_elem.insn_wdata_dest);
        if (!instr_elem.insn_writes_back) {
            // call dromajo step
            printf("first case, stepped PC %llx\n", instr_elem.dut_pc);
            printf(">>>>>>>>>>>>>>>\n");
            printf("dut_pc          %llx\n", instr_elem.dut_pc);
            printf("dut_insn        %x\n", instr_elem.dut_insn);
            printf("dut_wdata       %llx\n", instr_elem.dut_wdata);
            printf("hartid          %x\n", instr_elem.hartid);
            printf("mstatus         %llx\n", instr_elem.mstatus);
            printf("check           %x\n", instr_elem.check);
            printf(">>>>>>>>>>>>>>>\n");
            ret = dromajo->step(instr_elem.hartid, instr_elem.dut_pc, instr_elem.dut_insn, instr_elem.dut_wdata, instr_elem.mstatus, instr_elem.check);
            instruction_queue.pop_front();
            if (ret != 0) {
                return ret;
            }
        } else if (instr_elem.insn_writes_back && instr_elem.wdata_valid) {
            // call dromajo step
            printf("second case, stepped PC %llx\n", instr_elem.dut_pc);
            printf(">>>>>>>>>>>>>>>\n");
            printf("dut_pc          %llx\n", instr_elem.dut_pc);
            printf("dut_insn        %x\n", instr_elem.dut_insn);
            printf("dut_wdata       %llx\n", instr_elem.dut_wdata);
            printf("hartid          %x\n", instr_elem.hartid);
            printf("mstatus         %llx\n", instr_elem.mstatus);
            printf("check           %x\n", instr_elem.check);
            printf(">>>>>>>>>>>>>>>\n");
            ret = dromajo->step(instr_elem.hartid, instr_elem.dut_pc, instr_elem.dut_insn, instr_elem.dut_wdata, instr_elem.mstatus, instr_elem.check);
            instruction_queue.pop_front();
            if (ret != 0) {
                return ret;
            }
        } else {
            printf("didn't step OR stopped stepping\n");
            return 0;
        }
    }
    printf("went thru everything\n");
    return 0;
    // instrElem instruction_element;
    // instruction_element.wdata_valid = 0;
    // instruction_element.dut_wdata = 0; //init null
    // instruction_element.insn_wdata_dest = insn_wdata_dest;

    // instruction_element.hartid = hartid;
    // instruction_element.dut_pc = dut_pc;
    // instruction_element.dut_insn = dut_insn;
    // instruction_element.mstatus = mstatus;
    // instruction_element.check = check;

    // instruction_queue.push_back(instruction_element);
    // printf("queue size!!!!!: %d \n", instruction_queue.size());
    // printQueue(instruction_queue);
    // if (valid) {
    //     return dromajo->step(hartid, dut_pc, dut_insn, dut_wdata, mstatus, check);
    // } else {
    //     printf("it not valid!!");
    // }
}

extern "C" void dromajo_raise_trap(
    int     hartid,
    long long cause)
{
    dromajo->raise_trap(hartid, cause);
}
