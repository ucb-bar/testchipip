#include "loadarch.h"

// Simple program to test `loadarch_from_file`
// Usage: ./loadarch_main ../../../../../../../rv64ui-p-simple.0x80000000.10.loadarch/loadarch
int main(int argc, char* argv[]) {
  assert(argc == 2);
  std::string loadarch_file(argv[1]);
  auto r = loadarch_from_file(loadarch_file);
  auto l = r.first;
  auto n = r.second;
  print_loadarch_state(l[0]);
}
