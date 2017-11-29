//! \file
//! \brief Time Aging Function
//!/
#pragma module time_code "V3.6 Calico"

#include <string>
#include "cmcfun.h"

//!
//! \brief Time Aging Function
//!
//!	This function will convert a time into a long integer
//!	consisting of the number of seconds (since midnight).
//!
//! \returns	Returns a long integer containing the number of seconds
//!	(since midnight).
//!
//! Example:
//!
//!	GOOF_TIME% = TIME_CODE("002030")
//!--
//!/
long time_code(
	const std::string &datum)
		//!< The passed time in question in the form HHMMSS, or
		//! MMSS or SS.
{
	long total_time = 0;
	int loop;

	/*
	 * Loop through time string
	 */
	for (loop = 0; loop < datum.size(); loop += 2)
	{
		/*
		 * Add on next increment
		 */
		total_time = total_time * 60 +
			(datum[loop] - '0') * 10 +
			(datum[loop + 1] - '0');
	}

	return total_time;
}
