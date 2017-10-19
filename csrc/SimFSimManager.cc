#include <vpi_user.h>
#include <svdpi.h>

#include "fsim-manager.h"

FSimManager *dev = NULL;

extern "C" void fsim_manager_init()
{
	dev = new FSimManager();
}

extern "C" void fsim_manager_tick(
		unsigned char req_valid,
		unsigned char *req_ready,
		unsigned int req_bits_query,

		unsigned char *resp_valid,
		unsigned char resp_ready,
		unsigned int                    *resp_bits_data)
{
	if(dev == NULL) {
		*req_ready = 0;
		*resp_valid = 0;
		return;
	}

	dev->tick(
			req_valid,
			req_bits_query,
			resp_ready);
	dev->switch_to_host();

	*req_ready = dev->req_ready();
	*resp_valid = dev->resp_valid();
	*resp_bits_data = dev->resp_data();
}
