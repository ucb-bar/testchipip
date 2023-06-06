#include <riscv/abstract_device.h>
#include <riscv/dts.h>
#include <riscv/sim.h>
#include <fesvr/term.h>
#include <fdt/libfdt.h>

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

#define UART_GET_TXCNT(txctrl)   ((txctrl >> 16) & 0x7)
#define UART_GET_RXCNT(rxctrl)   ((rxctrl >> 16) & 0x7)
#define UART_RX_FIFO_SIZE (8)

#define UART_IE_TXWM       (1)
#define UART_IE_RXWM       (2)

#define UART_IP_TXWM       (1)
#define UART_IP_RXWM       (2)

// This is a very poor model of the sifive uart
// It only supports redirecting TX to terminal
// Other features may not behave correctly
class sifive_uart_t : public abstract_device_t {
public:
  sifive_uart_t(abstract_interrupt_controller_t *intctrl, reg_t int_id) :
    ie(0), ip(0), txctrl(0), rxctrl(0), div(0), interrupt_id(int_id), intctrl(intctrl) {}

  bool load(reg_t addr, size_t len, uint8_t* bytes) override {
    if (addr >= 0x1000 || len > 4) return false;
    uint32_t r = 0;
    switch (addr) {
    case UART_TXFIFO: r = 0x0          ; break;
    case UART_RXFIFO: r = read_rxfifo(); break;
    case UART_TXCTRL: r = txctrl       ; break;
    case UART_RXCTRL: r = rxctrl       ; break;
    case UART_IE:     r = ie           ; break;
    case UART_IP:     r = read_ip()    ; break;
    case UART_DIV:    r = div          ; break;
    default: printf("LOAD -- ADDR=0x%lx LEN=%lu\n", addr, len); abort();
    }
    memcpy(bytes, &r, len);
    return true;
  }

  bool store(reg_t addr, size_t len, const uint8_t* bytes) override {
    if (addr >= 0x1000 || len > 4) return false;
    switch (addr) {
    case UART_TXFIFO: canonical_terminal_t::write(*bytes); return true;
    case UART_TXCTRL: memcpy(&txctrl, bytes, len); return true;
    case UART_RXCTRL: memcpy(&rxctrl, bytes, len); return true;
    case UART_IE:     memcpy(&ie, bytes, len); update_interrupts(); return true;
    case UART_DIV:    memcpy(&div, bytes, len); return true;
    default: printf("STORE -- ADDR=0x%lx LEN=%lu\n", addr, len); abort();
    }
  }

  void tick(reg_t UNUSED rtc_ticks) override {
    if (rx_fifo.size() >= UART_RX_FIFO_SIZE) return;
    int rc = canonical_terminal_t::read();
    if (rc < 0) return;
    rx_fifo.push((uint8_t)rc);
    update_interrupts();
  }

private:
  std::queue<uint8_t> rx_fifo;
  uint32_t ie;
  uint32_t ip;
  uint32_t txctrl;
  uint32_t rxctrl;
  uint32_t div;
  reg_t interrupt_id;
  abstract_interrupt_controller_t *intctrl;

  uint64_t read_ip() {
    uint64_t ret = 0;
    uint64_t txcnt = UART_GET_TXCNT(txctrl);
    uint64_t rxcnt = UART_GET_RXCNT(rxctrl);
    if (txcnt != 0) ret |= UART_IP_TXWM;
    if (rx_fifo.size() > rxcnt) ret |= UART_IP_RXWM;
    return ret;
  }

  uint32_t read_rxfifo() {
    if (!rx_fifo.size()) return 0x80000000;
    uint8_t r = rx_fifo.front();
    rx_fifo.pop();
    update_interrupts();
    return r;
  }

  void update_interrupts() {
    int cond = 0;
    if ((ie & UART_IE_TXWM) ||
        ((ie & UART_IE_RXWM) && rx_fifo.size())) {
      cond = 1;
    }
    intctrl->set_interrupt_level(interrupt_id, (cond) ? 1 : 0);
  }
};

int fdt_parse_sifive_uart(const void *fdt, reg_t *sifive_uart_addr,
			  const char *compatible)
{
  int nodeoffset, len, rc;
  const fdt32_t *reg_p;

  nodeoffset = fdt_node_offset_by_compatible(fdt, -1, compatible);
  if (nodeoffset < 0)
    return nodeoffset;

  rc = fdt_get_node_addr_size(fdt, nodeoffset, sifive_uart_addr, NULL, "reg");
  if (rc < 0 || !sifive_uart_addr)
    return -ENODEV;

  return 0;
}

sifive_uart_t* sifive_uart_parse_from_fdt(const void* fdt, const sim_t* sim, reg_t* base)
{
  if (fdt_parse_sifive_uart(fdt, base, "sifive,uart0") == 0) {
    printf("Found uart at %lx\n", *base);
    return new sifive_uart_t(sim->get_intctrl(), 1);
  } else {
    return nullptr;
  }
}


std::string sifive_uart_generate_dts(const sim_t* sim)
{
  return std::string();
}

REGISTER_DEVICE(sifive_uart, sifive_uart_parse_from_fdt, sifive_uart_generate_dts);
