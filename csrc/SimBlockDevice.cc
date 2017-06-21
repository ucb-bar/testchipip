#include <vpi_user.h>
#include <svdpi.h>

#include "blkdev.h"

BlockDevice *bdev = NULL;

extern "C" void block_device_init(
        const char *filename, int ntags, unsigned int *nsectors)
{
    bdev = new BlockDevice(filename, ntags);
    *nsectors = bdev->nsectors();
}

extern "C" void block_device_tick(
        unsigned char req_valid,
        unsigned char *req_ready,
        unsigned char req_bits_write,
        unsigned int  req_bits_offset,
        unsigned int  req_bits_len,
        unsigned int  req_bits_tag,

        unsigned char data_valid,
        unsigned char *data_ready,
        unsigned long data_bits_data,
        unsigned int  data_bits_tag,

        unsigned char *resp_valid,
        unsigned char resp_ready,
        unsigned long *resp_bits_data,
        unsigned int  *resp_bits_tag)
{
    if (bdev == NULL) {
        fprintf(stderr, "BlockDevice not initialized\n");
        abort();
    }

    bdev->tick(
            req_valid,
            req_bits_write,
            req_bits_offset,
            req_bits_len,
            req_bits_tag,
            data_valid,
            data_bits_data,
            data_bits_tag,
            resp_ready);
    bdev->switch_to_host();

    *req_ready = bdev->req_ready();
    *data_ready = bdev->req_ready();
    *resp_valid = bdev->resp_valid();
    *resp_bits_data = bdev->resp_data();
    *resp_bits_tag = bdev->resp_tag();
}
