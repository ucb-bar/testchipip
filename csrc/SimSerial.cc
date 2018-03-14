#include <vpi_user.h>
#include <svdpi.h>
#include <vector>
#include <string>
#include <fesvr/tsi.h>

tsi_t *tsi = NULL;

static inline int copy_argv(int argc, char **argv, char **new_argv)
{
    int optind = 1;
    int new_argc = argc;

    new_argv[0] = argv[0];

    for (int i = 1; i < argc; i++) {
        if (argv[i][0] != '+') {
            optind = i - 1;
            new_argc = argc - i + 1;
            break;
        }
    }

    for (int i = 1; i < new_argc; i++)
        new_argv[i] = argv[i + optind];

    return new_argc;
}

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

        char **argv = (char **) malloc(sizeof(char*) * info.argc);
        int argc = copy_argv(info.argc, info.argv, argv);

        tsi = new tsi_t(argc, argv);
    }

    tsi->tick(out_valid, out_bits, in_ready);
    tsi->switch_to_host();

    *in_valid = tsi->in_valid();
    *in_bits = tsi->in_bits();
    *out_ready = tsi->out_ready();

    return tsi->done() ? (tsi->exit_code() << 1 | 1) : 0;
}
