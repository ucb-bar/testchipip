#include <vpi_user.h>
#include <svdpi.h>

#include <stdio.h>
#include <string.h>

#include "uart.h"

uart_t *uart = NULL;

extern "C" void uart_init(
        const char *filename,
        int uartno)
{
  bool use_pty = false;
  s_vpi_vlog_info vinfo;
  if (!vpi_get_vlog_info(&vinfo))
    abort();
  for (int i = 1; i < vinfo.argc; i++) {
    std::string arg(vinfo.argv[i]);
    if (arg == "+uart-pty")
      use_pty = true;
    }

    if (strlen(filename) != 0)
      uart = new uart_t(filename, uartno, use_pty);
    else
      uart = new uart_t(0, uartno, use_pty);
}

extern "C" void uart_tick(
        unsigned char out_valid,
        unsigned char *out_ready,
        char out_bits,

        unsigned char *in_valid,
        unsigned char in_ready,
        char *in_bits)
{
    if (uart == NULL) {
        *out_ready = 0;
        *in_valid = 0;
        return;
    }

    uart->tick(
        out_valid,
        out_ready,
        out_bits,

        in_valid,
        in_ready,
        in_bits);
}
