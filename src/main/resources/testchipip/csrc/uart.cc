//See LICENSE for license details

#include "uart.h"
#include <sys/stat.h>
#include <fcntl.h>

#define _XOPEN_SOURCE
#include <stdlib.h>
#include <stdio.h>

#ifndef _WIN32
#include <unistd.h>

// name length limit for ptys
#define SLAVENAMELEN 256

/* There is no "backpressure" to the user input for sigs. only one at a time
 * non-zero value represents unconsumed special char input.
 *
 * Reset to zero once consumed.
 */

// This is fine for multiple UARTs because UARTs > uart 0 will use pty, not stdio
char specialchar = 0;

void sighand(int s) {
    switch (s) {
        case SIGINT:
            // ctrl-c
            specialchar = 0x3;
            break;
        default:
            specialchar = 0x0;
    }
}
#endif

uart_t::uart_t(const char* filename_prefix, int uartno)
{
    if (filename_prefix) {
        printf("[UART_DEBUG] Setup UART%d with filename_prefix: %s\n", uartno, filename_prefix);
    }

    this->inputfd = 0;
    this->outputfd = 0;
    this->print_file = false;

    if (uartno == 0) {
        // signal handler so ctrl-c doesn't kill simulation when UART is attached
        // to stdin/stdout
        struct sigaction sigIntHandler;
        sigIntHandler.sa_handler = sighand;
        sigemptyset(&sigIntHandler.sa_mask);
        sigIntHandler.sa_flags = 0;
        sigaction(SIGINT, &sigIntHandler, NULL);
        printf("[UART] UART0 is here (stdin/stdout).\n");
        this->inputfd = STDIN_FILENO;
        this->outputfd = STDOUT_FILENO;
    } else {
        // for UARTs that are not UART0, use a PTY
        char slavename[SLAVENAMELEN];
        int ptyfd = posix_openpt(O_RDWR | O_NOCTTY);
        grantpt(ptyfd);
        unlockpt(ptyfd);
        ptsname_r(ptyfd, slavename, SLAVENAMELEN);

        // create symlink for reliable location to find uart pty
        std::string symlinkname = std::string("uartpty") + std::to_string(uartno);
        // unlink in case symlink already exists
        unlink(symlinkname.c_str());
        if(symlink(slavename, symlinkname.c_str())) {
            printf("[UART] Failed to created symlink with slave %s to symlink name %s\n", slavename, symlinkname.c_str());
            exit(1);
        }
        printf("[UART] UART%d is on PTY: %s, symlinked at %s\n", uartno, slavename, symlinkname.c_str());
        printf("[UART] Attach to this UART with 'sudo screen %s' or 'sudo screen %s'\n", slavename, symlinkname.c_str());
        this->inputfd = ptyfd;
        this->outputfd = ptyfd;
    }

    // if filename exists then put to file
    if (filename_prefix) {
        print_file = true;
        std::string uartlogname = std::string(filename_prefix) + std::to_string(uartno);
        printf("[UART] UART stdout being redirected to %s\n", uartlogname.c_str());
        this->outputfd = open(uartlogname.c_str(), O_RDWR | O_CREAT, 0644);
    }

    // Don't block on reads if there is nothing typed in
    fcntl(inputfd, F_SETFL, fcntl(inputfd, F_GETFL) | O_NONBLOCK);
}

uart_t::~uart_t() {
    if (print_file)
        close(this->outputfd);
}

void uart_t::tick(
        unsigned char out_valid,
        unsigned char *out_ready,
        char out_bits,

        unsigned char *in_valid,
        unsigned char in_ready,
        char *in_bits)
{
    *out_ready = true;
    *in_valid = false;

    if (in_ready) {
        char inp;
        int readamt;
        if (specialchar) {
            // send special character (e.g. ctrl-c)
            // for stdin handling
            //
            // PTY should never trigger this
            inp = specialchar;
            specialchar = 0;
            readamt = 1;
        } else {
            // else check if we have input
            readamt = ::read(inputfd, &inp, 1);
        }

        if (readamt > 0) {
            *in_bits = inp;
            *in_valid = true;
        }
    }

    if (*out_ready && out_valid) {
        if (::write(outputfd, &out_bits, 1) == -1) {
            printf("[UART] Failed to write to output fd\n");
            exit(1);
        }
    }
}
