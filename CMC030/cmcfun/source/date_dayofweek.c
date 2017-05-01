/*	%TITLE "DATE_DAYOFWEEK - Day of the Week Function"
 */
#pragma module date_dayofweek "V3.6 Calico"

/*
 *		COPYRIGHT (C) 1985 BY
 *		Computer Management Center, Idaho Falls, Idaho.
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies therof may not be provided or otherwise made
 * available to any other person.  No title to and ownership of
 * the software is hereby transferred.
 *
 * The information in this software is subject to change without
 * notice and should not be construed as a committment by
 * Computer management Center.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
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
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:DATE_DAYOFWEEK
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP DATE_DAYOFWEEK
 *	$ DELETE DATE_DAYOFWEEK.OBJ;*
 *
 * AUTHOR:
 *
 *	5/30/86 - Frank Starman
 *
 * MODIFICATION HISTORY:
 *
 *	11/20/87 - Frank Starman
 *		Sunday = 7% (before was 0%)
 *
 *	04/27/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Coding standards.
 *
 *	05/26/99 - Kevin Handy
 *		Modified to compile clean with DEC-C
 *		(module)
 *--
 */

#include <math.h>

long date_dayofweek(long *cod)
{
	long comp;

	comp = (*cod + 3) % 7;
	if (comp == 0)
	{
		comp = 7;
	}

	return(comp);
}
