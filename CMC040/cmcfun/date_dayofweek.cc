//! \file
//! \brief DATE_DAYOFWEEK - Day of the Week Function
//!
#pragma module date_dayofweek "V3.6 Calico"

#include "preferences.h"
#include "cmcfun.h"

/*
 * ABSTRACT:
 *
 *	This function returns the day of the week as:
 *	.table
 *		1% = Monday
 *		2% = Tuesday
 *		3% = Wednesday
 *		4% = Thursday
 *		5% = Friday
 *		6% = Saturday
 *		7% = Sunday
 *	.endtable
 *	
 *	as an integer given the code (as generated from
 *	the DATE_DAYCODE function.
 *
 *
 * Parameters:
 *
 *	COD
 *		The passed date (as returned by DATE_DAYCODE) for which the
 *		day of the week is desired.
 *
 *	Returned value
 *		1 to 7 for the day of week.
 *	.table
 *		1% = Monday
 *		2% = Tuesday
 *		3% = Wednesday
 *		4% = Thursday
 *		5% = Friday
 *		6% = Saturday
 *		7% = Sunday
 *	.endtable
 *	
 *
 * Example:
 *
 *	WEEKDAY% = DATE_DAYOFWEEK(DATE_DAYCODE("19880101"))
 *
 * AUTHOR:
 *
 *	5/30/86 - Frank Starman
 *
 */
long date_dayofweek(long cod)
{
	long comp;

	comp = (cod + 3) % 7;
	if (comp == 0)
	{
		comp = 7;
	}

	return(comp);
}
