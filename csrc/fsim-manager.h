#include <stdio.h>
#include <fesvr/context.h>
#include <queue>

class FSimManager {
	public:
		FSimManager();
		~FSimManager();
		void tick(
				uint8_t req_valid,
				uint32_t req_bits_query,
				uint8_t resp_ready);

		bool req_ready() { return true; }
		bool resp_valid() { return responded; }
		uint32_t resp_data() { return response; }

		void switch_to_host() { host.switch_to(); }
	private:
		// record time here
		uint32_t request;
		bool responded;
		uint32_t response;

		// to run host
		static void host_thread(void *arg);
		void run(void);

		context_t* target;
		context_t host;
};
