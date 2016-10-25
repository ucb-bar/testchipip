#include <vpi_user.h>
#include <svdpi.h>
#include <vector>
#include <string>
#include <fesvr/tsi.h>

tsi_t *tsi = NULL;

extern "C" int serial_tick(
        unsigned char out_valid,
        unsigned char *out_ready,
        int out_bits,

        unsigned char *in_valid,
        unsigned char in_ready,
        int *in_bits)
{
    bool out_fire = *out_ready && out_valid;
    bool in_fire = *in_valid && in_ready;
    bool in_free = !(*in_valid);

    if (!tsi) {
        s_vpi_vlog_info info;
        if (!vpi_get_vlog_info(&info))
          abort();
        tsi = new tsi_t(std::vector<std::string>(info.argv + 1, info.argv + info.argc));
    }

    // Take in out_bits if out.fire()
    if (out_fire) {
        tsi->send_word(out_bits);
    }
    *out_ready = true;

    // We can update in_valid and in_bits if nothing is there or we've just fired
    if (in_fire || in_free) {
        if (tsi->data_available()) {
            *in_valid = true;
            *in_bits = tsi->recv_word();
        } else {
            *in_valid = false;
        }
    }

    tsi->switch_to_host();

    return tsi->done() ? (tsi->exit_code() << 1 | 1) : 0;
}
