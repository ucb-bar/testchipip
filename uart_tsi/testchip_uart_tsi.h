#ifndef __TESTCHIP_UART_TSI_H
#include "testchip_tsi.h"

class testchip_uart_tsi_t : public testchip_tsi_t
{
public:
  testchip_uart_tsi_t(int argc, char** argv, char* tty);
  virtual ~testchip_uart_tsi_t() {};

  void handle_uart();

private:
  int ttyfd;
  std::deque<uint8_t> read_bytes;
};
#endif
