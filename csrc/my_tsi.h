#include <fesvr/tsi.h>

class my_tsi_t: public tsi_t
{
public:
  my_tsi_t(int argc, char** argv):
    tsi_t(argc, argv), is_busy(true) { }

  bool busy() { return is_busy; }

protected:
  virtual void idle() {
    is_busy = false;
    switch_to_target();
    is_busy = true;
  }

private:
  bool is_busy;
};
