/*	TITLE "Inverse of DATE_DAYCODE Function"
 */
#pragma module date_invdcode "V3.6 Calico"

/*
 *
 *
 *		COPYRIGHT (C) 1988 BY
 *		Computer Management Center, Idaho Falls, Idaho.
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies thereof may not be provided or otherwise made
 * available to any other person.  No title to and ownership of
 * the software is hereby transferred.
 *
 * The information in this software is subject to change without
 * notice and should not be construed as a commitment by
 * Computer Management Center, Inc.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
 * Abstract:HELP
 *	.B
 *	Inverse of DATE__DAYCODE function.
 *
 * Index:
 *
 * Parameters:
 *
 *	DAYCODE
 *		The passed day code as returned from DATE_DAYCODE.
 *
 *	Returned value
 *		A string in the form YYYYMMDD.
 *
 * Compile:
 *
 *	$ cc/g_float FUNC_SOURCE:DATE_INVDCODE
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP DATE_INVDCODE
 *	$ DELETE DATE_INVDCODE.OBJ;*
 *
 * AUTHOR:
 *
 *	09/05/85 - Frank Starman
 *
 * MODIFICATION HISTORY:
 *
 *	12/08/85 - Frank Starman
 *		Correction calculation YR
 *
 *	05/13/89 - Kevin Handy
 *		Changed from using a real to using a long
 *		for the input parameter.
 *
 *	07/06/93 - Kevin Handy
 *		Modified to calculate end_yr without using a
 *		conversion to string and back to lose all but the
 *		last two digits.
 *		This should make this functions faster because
 *		a divide/multiply should be faster than a
 *		convert-to-string/convert-from-string.
 *
 *	07/07/93 - Kevin Handy
 *		Modified to do more of the work using integers
 *		instead of floating point math. Modified
 *		implied if to if-then-else. Lost several INT()'s.
 *		About 5%-10% faster now.
 *
 *	07/07/93 - Kevin Handy
 *		Converted to C, and really sped things up. Test
 *		program went from 5 seconds with basoc version
 *		to 3 seconds with C version. (20% faster)
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/26/99 - Kevin Handy
 *		Modified to compile clean with DEC-C
 *		(module, stdio.d, str$routines.h)
 *--
 */

#include <stdio.h>
#include <descrip.h>
#include <math.h>
#include <str$routines.h>

#define sgn(x) ((x) > 0 ? 1 : 0)

void date_invdcode(struct dsc$descriptor *returnstr, long *daycode)
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

	cd = *daycode + 55773;

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

	jul = *daycode - (yr - 1600) * 365 -
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
	str$copy_r(returnstr, &8l, &makestring);
}
