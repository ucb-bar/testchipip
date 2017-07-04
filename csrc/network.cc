#include "network.h"

#include <stdio.h>
#include <string.h>

#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <linux/if.h>
#include <linux/if_tun.h>

#define NET_IP_ALIGN 2
#define ETH_MAX_WORDS 190
#define ETH_MAX_BYTES 1518

static int tuntap_alloc(const char *dev, int flags)
{
	struct ifreq ifr;
	int fd, err;

	if ((fd = open("/dev/net/tun", O_RDWR | O_NONBLOCK)) < 0) {
		perror("open()");
		return fd;
	}

	memset(&ifr, 0, sizeof(ifr));

	ifr.ifr_flags = flags;
	strncpy(ifr.ifr_name, dev, IFNAMSIZ);

	if ((err = ioctl(fd, TUNSETIFF, &ifr)) < 0) {
		perror("ioctl()");
		close(fd);
		return err;
	}

	return fd;
}

void NetworkDevice::host_thread(void *arg)
{
    NetworkDevice *netdev = static_cast<NetworkDevice*>(arg);
    netdev->run();

    while (true)
        netdev->target->switch_to();
}

NetworkDevice::NetworkDevice(const char *dev)
{
    fd = tuntap_alloc(dev, IFF_TAP | IFF_NO_PI);
    if (fd < 0) {
        fprintf(stderr, "Could not open tap interface %s\n", dev);
        abort();
    }

    target = context_t::current();
    host.init(host_thread, this);
}

NetworkDevice::~NetworkDevice()
{
    close(fd);
}

#define ceil_div(n, d) (((n) - 1) / (d) + 1)

void NetworkDevice::run(void)
{
    uint64_t send_buffer[ETH_MAX_WORDS], recv_buffer[ETH_MAX_WORDS];
    void *send_frame = ((char *) send_buffer) + NET_IP_ALIGN;
    void *recv_frame = ((char *) recv_buffer) + NET_IP_ALIGN;
    int send_idx = 0, len;
    bool can_send = false;

    memset(send_buffer, 0, ETH_MAX_WORDS * sizeof(uint64_t));
    memset(recv_buffer, 0, ETH_MAX_WORDS * sizeof(uint64_t));

    while (true) {
        if (!can_send) {
            while (!out_flits.empty()) {
                send_buffer[send_idx] = out_flits.front().data;
                can_send = out_flits.front().last;
                out_flits.pop();
                send_idx++;
                if (can_send)
                    break;
            }
        }

        if (can_send) {
            len = send_idx * sizeof(uint64_t) - NET_IP_ALIGN;
            if (write(fd, send_frame, len) >= 0) {
                send_idx = 0;
                can_send = false;
            } else if (errno != EAGAIN) {
                perror("send()");
                abort();
            }
        }

        len = read(fd, recv_frame, ETH_MAX_BYTES);
        if (len >= 0) {
            int i, n = ceil_div(len + NET_IP_ALIGN, sizeof(uint64_t));
            for (i = 0; i < n; i++) {
                struct network_flit flt;
                flt.data = recv_buffer[i];
                flt.last = i == (n - 1);
                in_flits.push(flt);
            }
        } else if (errno != EAGAIN) {
            perror("recv()");
            abort();
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
