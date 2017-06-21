#include <vector>
#include <queue>
#include <stdio.h>
#include <fesvr/context.h>
#include <stdint.h>

#define SECTOR_SIZE 512
#define SECTOR_SHIFT 9
#define SECTOR_BEATS (SECTOR_SIZE / 8)

struct blkdev_request {
    bool write;
    uint32_t offset;
    uint32_t len;
    uint32_t tag;
};

struct blkdev_data {
    uint64_t data;
    uint32_t tag;
};

struct blkdev_write_tracker {
    uint64_t offset;
    uint64_t count;
    uint64_t size;
    std::vector<uint64_t> data;
};

class BlockDevice {
  public:
    BlockDevice(const char *filename, int ntags);
    ~BlockDevice(void);

    uint32_t nsectors(void) { return _nsectors; }
    void tick(
        uint8_t  req_valid,
        uint8_t  req_bits_write,
        uint32_t req_bits_offset,
        uint32_t req_bits_len,
        uint32_t req_bits_tag,

        uint8_t  data_valid,
        uint64_t data_bits_data,
        uint32_t data_bits_tag,

        uint8_t  resp_ready);

    bool req_ready() { return true; }
    bool data_ready() { return true; }
    bool resp_valid() { return !responses.empty(); }
    uint64_t resp_data() { return responses.front().data; }
    uint32_t resp_tag() { return responses.front().tag; }

    void send_request(struct blkdev_request &req);
    void send_data(struct blkdev_data &data);
    struct blkdev_data recv_response(void);

    void switch_to_host() { host.switch_to(); }

  private:
    int _ntags;
    uint32_t _nsectors;
    FILE *_file;
    std::queue<blkdev_request> requests;
    std::queue<blkdev_data> req_data;
    std::queue<blkdev_data> responses;
    std::vector<blkdev_write_tracker> write_trackers;

    void do_read(struct blkdev_request &req);
    void do_write(struct blkdev_request &req);
    bool can_accept(struct blkdev_data &data);
    void handle_data(struct blkdev_data &data);

    static void host_thread(void *arg);
    void run(void);

    context_t* target;
    context_t host;
};
