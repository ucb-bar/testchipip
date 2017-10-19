#include "fsim-manager.h"
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <time.h>

void FSimManager::host_thread(void *arg)
{
	FSimManager* dev = static_cast<FSimManager*>(arg);
	dev->run();

	while (true)
		dev->target->switch_to();
}

FSimManager::FSimManager()
{
	request = 0;
	responded = false;
	response = 0;
	target = context_t::current();
	host.init(host_thread, this);
}

FSimManager::~FSimManager()
{
}

void FSimManager::tick(
		uint8_t req_valid,
		uint32_t req_bits_query,
		uint8_t resp_ready)
{
	if( resp_valid() && resp_ready ) {
		responded = false;
	}

	if( req_valid && req_ready() ) {
		request = req_bits_query;
	}

}

#define FSIM_REQ_TIME      1

#define FSIM_RESP_ERROR			-1
#define FSIM_RESP_NOP				0
void FSimManager::run()
{
	while (true) {
		if( request ) {
			switch(request){
				case FSIM_REQ_TIME:
					{
						response = time (NULL);
						responded = true;
						request = 0;
						break;
					}
				default:
					{
						response = FSIM_RESP_ERROR;
						responded = true;
						request = 0;
					}
			}
		}
		this->target->switch_to();
	}
}
