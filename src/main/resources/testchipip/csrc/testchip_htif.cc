#include "testchip_htif.h"

// Parses the optional ":N" size suffix of +init_write/+init_read (N = 4 or 8 bytes).
static size_t parse_init_size(const std::string &arg, const std::string &s) {
  size_t nbytes = strtoull(s.c_str(), 0, 0);
  if (nbytes != 4 && nbytes != 8)
    throw std::invalid_argument("Access size must be 4 or 8 bytes: " + arg);
  return nbytes;
}

// +init_write=0xADDR:0xDATA[:N]  store N bytes (default 4) before hart 0 is released
// +init_read=0xADDR[:N]          load N bytes (default 4) before hart 0 is released
void testchip_htif_t::parse_htif_args(std::vector<std::string> &args) {
  for (auto& arg : args) {
    if (arg.find("+init_write=0x") == 0) {
      auto d = arg.find(":0x");
      if (d == std::string::npos) {
        throw std::invalid_argument("Improperly formatted +init_write argument");
      }
      auto s = arg.find(':', d + 1);
      uint64_t addr = strtoull(arg.substr(14, d - 14).c_str(), 0, 16);
      uint64_t val = strtoull(arg.substr(d + 3, s == std::string::npos ? std::string::npos : s - (d + 3)).c_str(), 0, 16);
      size_t nbytes = s == std::string::npos ? 4 : parse_init_size(arg, arg.substr(s + 1));
      if (nbytes == 4 && (val >> 32))
        throw std::invalid_argument("+init_write value does not fit in 4 bytes (append :8): " + arg);
      if (addr % nbytes)
        throw std::invalid_argument("+init_write address must be aligned to its size: " + arg);
      init_access_t access = { .address=addr, .stdata=val, .nbytes=nbytes, .store=true };
      init_accesses.push_back(access);
    }
    if (arg.find("+init_read=0x") == 0) {
      auto s = arg.find(':', 13);
      uint64_t addr = strtoull(arg.substr(13, s == std::string::npos ? std::string::npos : s - 13).c_str(), 0, 16);
      size_t nbytes = s == std::string::npos ? 4 : parse_init_size(arg, arg.substr(s + 1));
      if (addr % nbytes)
        throw std::invalid_argument("+init_read address must be aligned to its size: " + arg);
      init_access_t access = { .address=addr, .stdata=0, .nbytes=nbytes, .store=false };
      init_accesses.push_back(access);
    }
    if (arg.find("+no_hart0_msip") == 0)
      write_hart0_msip = false;
  }
}

void testchip_htif_t::perform_init_accesses() {
  for (auto p : init_accesses) {
    if (p.store) {
      fprintf(stderr, "Writing %lx with %lx (%zu bytes)\n", p.address, p.stdata, p.nbytes);
      write_chunk(p.address, p.nbytes, &p.stdata);
      fprintf(stderr, "Done writing %lx with %lx\n", p.address, p.stdata);
    } else {
      fprintf(stderr, "Reading %lx (%zu bytes) ...", p.address, p.nbytes);
      uint64_t rdata = 0;
      read_chunk(p.address, p.nbytes, &rdata);
      fprintf(stderr, " got %lx\n", rdata);
    }
  }
}
