#include "network.h"

void NetworkDevice::host_thread(void *arg)
{
    NetworkDevice *netdev = static_cast<NetworkDevice*>(arg);
    netdev->run();

    while (true)
        netdev->target->switch_to();
}

NetworkDevice::NetworkDevice()
{
    target = context_t::current();
    host.init(host_thread, this);
}

NetworkDevice::~NetworkDevice()
{
}

void NetworkDevice::run(void)
{
    while (true) {
        while (!out_flits.empty()) {
            in_flits.push(out_flits.front());
            out_flits.pop();
        }

        target->switch_to();
    }
}


void NetworkDevice::tick(
            bool out_valid,
            uint64_t out_data,
            bool out_last,
            bool in_ready)
{
    if (out_valid && out_ready()) {
        struct network_flit flt;
        flt.data = out_data;
        flt.last = out_last;
        out_flits.push(flt);
    }

    if (in_valid() && in_ready) {
        in_flits.pop();
    }
}
