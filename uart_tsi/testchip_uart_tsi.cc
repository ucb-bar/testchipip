#include "testchip_uart_tsi.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <termios.h>


testchip_uart_tsi_t::testchip_uart_tsi_t(int argc, char** argv, char* ttyfile, bool verbose) : testchip_tsi_t(argc, argv, false), verbose(verbose) {
  ttyfd = open(ttyfile, O_RDWR);
  if (ttyfd < 0) {
    printf("Error %i from open: %s\n", errno, strerror(errno));
    exit(1);
  }

  // https://blog.mbedded.ninja/programming/operating-systems/linux/linux-serial-ports-using-c-cpp/
  struct termios tty;

  if (tcgetattr(ttyfd, &tty) != 0) {
    printf("Error %i from tcgetaddr: %s\n", errno, strerror(errno));
    exit(1);
  }

  tty.c_cflag &= ~PARENB; // Clear parity bit, disabling parity (most common)
  tty.c_cflag &= ~CSTOPB; // Clear stop field, only one stop bit used in communication (most common)
  tty.c_cflag |= CSTOPB;  // Set stop field, two stop bits used in communication
  tty.c_cflag &= ~CSIZE;  // Clear all the size bits
  tty.c_cflag &= CS8; // 8 bits per byte (most common)
  tty.c_cflag &= ~CRTSCTS; // Disable RTS/CTS hardware flow control (most common)
  tty.c_cflag |= CREAD | CLOCAL; // Turn on READ & ignore ctrl lines (CLOCAL = 1)

  tty.c_lflag &= ~ICANON;
  tty.c_lflag &= ~ECHO; // Disable echo
  tty.c_lflag &= ~ECHOE; // Disable erasure
  tty.c_lflag &= ~ECHONL; // Disable new-line echo
  tty.c_lflag &= ~ISIG; // Disable interpretation of INTR, QUIT and SUSP

  tty.c_iflag &= ~(IXON | IXOFF | IXANY); // Turn off s/w flow ctrl
  tty.c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|IGNCR|ICRNL); // Disable any special handling of received bytes

  tty.c_oflag &= ~OPOST; // Prevent special interpretation of output bytes (e.g. newline chars)
  tty.c_oflag &= ~ONLCR; // Prevent conversion of newline to carriage return/line feed

  tty.c_cc[VTIME] = 0;
  tty.c_cc[VMIN] = 0;

  // Set in/out baud rate to be B115200
  cfsetispeed(&tty, B115200);
  cfsetospeed(&tty, B115200);

  // Save tty settings, also checking for error
  if (tcsetattr(ttyfd, TCSANOW, &tty) != 0) {
    printf("Error %i from tcsetattr: %s\n", errno, strerror(errno));
  }
};

bool testchip_uart_tsi_t::handle_uart() {
  if (data_available()) {
    uint32_t d = recv_word();
    write(ttyfd, &d, sizeof(d));
    if (verbose) printf("Wrote %x\n", d);
  }

  uint8_t read_buf[256];
  int n = read(ttyfd, &read_buf, sizeof(read_buf));
  if (n < 0) {
    printf("Error %i from read: %s\n", errno, strerror(errno));
    exit(1);
  }
  for (int i = 0; i < n; i++) {
    read_bytes.push_back(read_buf[i]);
  }

  if (read_bytes.size() >= 4) {
    uint32_t out_data = 0;
    uint8_t* b = ((uint8_t*)&out_data);
    for (int i = 0; i < (sizeof(uint32_t) / sizeof(uint8_t)); i++) {
      b[i] = read_bytes.front();
      read_bytes.pop_front();
    }
    if (verbose) printf("Read %x\n", out_data);
    send_word(out_data);
  }
  return data_available() || n > 0;
}

bool testchip_uart_tsi_t::check_connection() {
  sleep(1); // sleep for 1 second
  uint8_t rdata = 0;
  int n = read(ttyfd, &rdata, 1);
  if (n > 0) {
    printf("Error: Reading unexpected data %c from UART. Abort.\n", rdata);
    exit(1);
  }
  return true;
}

int main(int argc, char* argv[]) {
  printf("Starting UART-based TSI\n");
  printf("Usage: ./uart_tsi +tty=/dev/pts/xx <PLUSARGS> bin\n");
  printf("       ./uart_tsi +tty=/dev/ttyxx  <PLUSARGS> bin\n");
  printf("       ./uart_tsi +tty=/dev/ttyxx  +no_hart0_msip +init_write=0x80000000:0xdeadbeef none\n");
  printf("       ./uart_tsi +tty=/dev/ttyxx  +no_hart0_msip +init_read=0x80000000 none\n");

  // Add the permissive flags in manually here
  std::vector<std::string> args;
  for (int i = 0; i < argc; i++) {
    bool is_plusarg = argv[i][0] == '+';
    if (is_plusarg) {
      args.push_back("+permissive");
      args.push_back(std::string(argv[i]));
      args.push_back("+permissive-off");
    } else {
      args.push_back(std::string(argv[i]));
    }
  }

  std::string tty;
  bool verbose = false;
  for (std::string& arg : args) {
    if (arg.find("+tty=") == 0) {
      tty = std::string(arg.c_str() + 5);
    }
    if (arg.find("+verbose") == 0) {
      printf("Running in verbose mode\n");
      verbose = true;
    }
  }

  if (tty.size() == 0) {
    printf("ERROR: Must use +tty=/dev/ttyxx to specify a tty\n");
    exit(1);
  }

  printf("Attempting to open TTY at %s\n", tty.c_str());
  std::vector<std::string> tsi_args(args);
  char* tsi_argv[args.size()];
  for (int i = 0; i < args.size(); i++)
    tsi_argv[i] = tsi_args[i].data();

  testchip_uart_tsi_t tsi(args.size(), tsi_argv, tty.data(), verbose);
  printf("Constructed uart_tsi_t\n");
  printf("Checking connection status with %s\n", tty.c_str());
  if (!tsi.check_connection()) {
    printf("Connection failed\n");
    exit(1);
  } else {
    printf("Connection succeeded\n");
  }
  while (!tsi.done()) {
    tsi.switch_to_host();
    tsi.handle_uart();
  }
  printf("Done, shutting down, flushing UART\n");
  while (tsi.handle_uart()) {
    tsi.switch_to_host();
  }; // flush any inflight reads or writes
  printf("WARNING: You should probably reset the target before running this program again\n");
  return tsi.exit_code();
}