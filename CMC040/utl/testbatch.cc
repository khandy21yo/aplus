//
// Source: ../../CMC030/utl/source/testbatch.bas
// Translated from Basic to C++ using btran
// on Thursday, October 19, 2017 at 17:21:25
//

#include <iostream>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"
#include "basicchannel.h"



//
// Function Prototypes
//
std::string date_today();
std::string time_now();
std::string assg_makebatch(const std::string &batchdate,
	const std::string &batchtime);
std::string assg_unmakebatch(std::string &this_batch,
	std::string &this_date, std::string &this_time);


int main(int argc, char **argv)
{
	std::string this_batch;
	std::string this_date;
	std::string this_time;

	// Try to unroll a batch number into the proper date/time
	this_date = date_today();
	this_time = time_now();
	this_batch = assg_makebatch(this_date, this_time);
	std::cout << "Current Batch: " << this_batch << std::endl;
	std::cout << "         Date: " << this_date << std::endl;
	std::cout << "         Time: " << this_time << std::endl;
L_110:;
	std::cout << std::endl;
	std::cout << " Batch Number: " << this_batch << std::endl;
	assg_unmakebatch(this_batch, this_date, this_time);
	std::cout << "        Date: " << this_date << std::endl;
	std::cout << "        Time: " << this_time << std::endl;
	std::cout << std::endl;
	std::cout << "For Batch Number: ";
	getline(std::cin, this_batch);
	if (this_batch != "")
	{
		goto L_110;
	}

	return EXIT_SUCCESS;
}
