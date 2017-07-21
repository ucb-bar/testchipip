#include <queue>
#include <stdint.h>

#include "fesvr/context.h"

struct network_flit {
    uint64_t data;
    bool last;
};

class NetworkDevice {
  public:
    NetworkDevice(const char *dev);
    ~NetworkDevice();

    void tick(
            bool out_valid,
            uint64_t out_data,
            bool out_last,
            bool in_ready,
	    bool macaddr_valid,
	    uint64_t macaddr_bits);

    bool out_ready() { return true; }
    bool in_valid() { return !in_flits.empty(); }
    uint64_t in_data() { return (in_valid()) ? in_flits.front().data : 0; }
    bool in_last() { return (in_valid()) ? in_flits.front().last : false; }
    void switch_to_host(void) { host.switch_to(); }
    void send_out(struct network_flit &flt) { out_flits.push(flt); }
    struct network_flit recv_in(void) {
        struct network_flit flt = in_flits.front();
        in_flits.pop();
        return flt;
    }
    uint64_t macaddr() { return _macaddr; }

  private:
    std::queue<network_flit> out_flits;
    std::queue<network_flit> in_flits;

    static void host_thread(void *arg);
    void run(void);

    context_t* target;
    context_t host;

    int fd;
    uint64_t _macaddr;
};
