//! \file
//! \brief Inverse of DATE_DAYCODE Function
//
#pragma module date_invdcode "V3.6 Calico"

#include <stdio.h>
#include <math.h>
#include <string>

#include "cmcfun.h"

//! \brief sgn (sign) function
#define sgn(x) ((x) > 0 ? 1 : 0)

//!
//! \brief Inverse of DATE_DAYCODE Function
//!
//!	Inverse of DATE__DAYCODE function.
//!
//! \returns A string in the form YYYYMMDD.
//
std::string date_invdcode(
	long daycode)
 		//!< The passed day code as returned from DATE_DAYCODE.
{
	long cd;
	long cent;
	long cent21;
	long yr;
	long end_yr;
	long ly;
	long mon;
	long day;
	long jul;

	char makestring[10];

	cd = daycode + 55773;

	cent = (cd - 1) / 36524;
	if (cd > 146097)
	{
		cent21 = 1;
	}
	else
	{
		cent21 = 0;
	}
	cd = cd - cent * 36524 - cent21;
	yr = floor((cd - cd / 1462.0)/365.0) + 100 * cent + 1600;

	end_yr = sgn(yr % 100);

	jul = daycode - (yr - 1600) * 365 -
		((yr - 1601) / 4) + 55757 +
		((yr - 1) / 100) -
		((yr - 1601) / 400);
	ly = 1 - end_yr * sgn(yr % 4) -
		(1 - end_yr) * sgn(yr % 400);

	if (jul < 32)
	{
		mon = 1;
	}
	else
	{
		mon = ((jul - ly - 59) / 30.59 + 2.98);
	}

	if (mon < 3)
	{
		day = jul - 31 * (mon - 1);
	}
	else
	{
		day = jul - floor((mon - 2.98) * 30.59 + 59) - ly;
	}

	/*
	 * Generate and return output string
	 */
	sprintf(makestring, "%04ld%02ld%02ld", yr, mon, day);
	return makestring;
}
