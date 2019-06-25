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

        tsi = new tsi_t(info.argc, info.argv);
    }

    tsi->tick(out_valid, out_bits, in_ready);
    tsi->switch_to_host();

    *in_valid = tsi->in_valid();
    *in_bits = tsi->in_bits();
    *out_ready = tsi->out_ready();

    return tsi->done() ? (tsi->exit_code() << 1 | 1) : 0;
}
