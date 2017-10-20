// Generate a date and a time given a batch number
// ASSG_UNMAKEBATCH
// V3.6a Calico
//
// ++
//
// Abstract:HELP
//	.b
//	.lm +5
//	This functon will generate
//	a date and time
//	when given a batch number.
//	.lm -5
//
// Index:
//
// Parameters:
//
//	BATCH$
//		This is the batch number to generate a date and time
//		for.
//
//	GIVEN_DATE$
//		This is the date returned from the batch number,
//		in YYYYMMDD format.
//
//	GIVEN_TIME$
//		This is the time to generate the batch number for,
//		in HHMMSS format.
//
// --
//
// Source: ../../CMC030/cmcfun/source/assg_unmakebatch.bas
// Translated from Basic to C++ using btran
// on Friday, October 20, 2017 at 16:33:18
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <cmath>
#include "basicfun.h"


#include "cmcfun.h"


void assg_unmakebatch(std::string &batch,
	std::string &given_date, std::string &given_time)
{
	double buildup;
	long char_V1;
	const std::string character = "23456789ABCDEFGHJKLMNPQRSTUVWXYZ";
	double cycle;
	long i;
	double todaybatch;
	double u1;
	long u1_V2;
	long u2;
	double whatcycle;

	//
	// External functions
	//
	//
	// Define the RADIX and the characters used to print that RADIX.
	// Note that some characters are missing, because trying to
	// identify them can be hard from a printout, i.e. (0 and O,
	// 1 and I)
	//
	char_V1 = character.size();
	//
	// Calculate the number of seconds in a single cycle
	//
	cycle = (pow(char_V1, 6.0));
	//
	// Convert the given batch number back into a numeric value
	//
	buildup = 0.0;
	for (i = 1; i <= batch.size(); i++)
	{
		buildup = buildup * char_V1 + (character.find(basic::mid(batch, i, 1), 0) + 1) - 1;
	}
	buildup = buildup - 1;
	//
	// Close approximation of a batch number that would be assigned
	// for tomorrow. Use tomorrow so can calculate on batches created
	// today.
	//
	todaybatch = (date_daycode(date_today()) + 1.0) * (24.0 * 60.0 * 60.0);
	//
	// Calculate which cycle it is likely to be in. We may guess one
	// cycle too far, so adjust back one cycle if necessary.
	//
	whatcycle = trunc(todaybatch / cycle + 0.0001);
	u1 = (buildup + whatcycle * cycle);
	if (u1 > todaybatch)
	{
		u1 = u1 - cycle;
	}
	//
	// Generate a daycode and timecode value for that date
	//
	u1_V2 = trunc(u1 / (24.0 * 60.0 * 60.0) + 0.00001);
	u2 = trunc(u1 - u1_V2 * (24.0 * 60.0 * 60.0) + 0.5);
	//
	// Return values back
	//
	given_date = date_invdcode(u1_V2);
	given_time = time_invcode(u2);
}
