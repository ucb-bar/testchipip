#include <riscv/mmio_plugin.h>
#include <fesvr/term.h>

#define UART_TXFIFO (0x00)
#define UART_RXFIFO (0x04)
#define UART_TXCTRL (0x08)
#define UART_TXMARK (0x0a)
#define UART_RXCTRL (0x0c)
#define UART_RXMARK (0x0e)

#define UART_IE     (0x10)
#define UART_IP     (0x14)
#define UART_DIV    (0x18)
#define UART_PARITY (0x1c)
#define UART_WIRE4  (0x20)
#define UART_EITHER8OR9 (0x24)

// This is a very poor model of the sifive uart
// It only supports redirecting TX to terminal
// Other features may not behave correctly
struct sifive_uart
{
  sifive_uart(const std::string& args) { }
  ~sifive_uart() { }

  bool load(reg_t addr, size_t len, uint8_t* bytes) {
    if (addr >= 0x1000) return false;
    if (len == 4) {
      switch (addr) {
      case UART_TXFIFO:
        *((uint32_t*)bytes) = 0x0;
        return true;
      case UART_RXFIFO:
        *((uint32_t*)bytes) = 0xf0000000;
        return true;
      case UART_TXCTRL:
      case UART_TXMARK:
      case UART_RXCTRL:
      case UART_RXMARK:
      case UART_IE:
        *((uint32_t*)bytes) = 0x0;
        return true;
      }
    }
    printf("LOAD -- ADDR=0x%lx LEN=%lu\n", addr, len);
    abort();
  }

  bool store(reg_t addr, size_t len, const uint8_t* bytes)
  {
    if (addr >= 0x1000) return false;
    uint8_t byte = *bytes;
    if (len == 4) {
      switch (addr) {
      case UART_TXFIFO:
        canonical_terminal_t::write(byte);
        return true;
      case UART_TXCTRL:
      case UART_TXMARK:
      case UART_RXCTRL:
      case UART_RXMARK:
      case UART_IE:
      case UART_DIV:
        return true;
      }
    }
    printf("STORE -- ADDR=0x%lx LEN=%lu\n", addr, len);
    abort();
  }
};

static mmio_plugin_registration_t<sifive_uart> sifive_uart_registration("sifive_uart");
