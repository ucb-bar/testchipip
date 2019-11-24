#include <vpi_user.h>
#include <svdpi.h>

#include "uart.h"
#include <stdio.h>

uart_t *uart = 0;

extern "C" void uart_init(
        const char *filename,
        int uartno)
{
    uart = new uart_t(filename, uartno);
}

extern "C" void uart_tick(
        unsigned char out_valid,
        unsigned char *out_ready,
        char out_bits,

        unsigned char *in_valid,
        unsigned char in_ready,
        char *in_bits)
{
    if (uart == 0) {
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
