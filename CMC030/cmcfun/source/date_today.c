/*	%TITLE "Returns the Current Date in YYYYMMDD Form"
 */
#pragma module date_today "V3.6 Calico"

/*
 *
 *	COPYRIGHT (C) 1984 BY
 *	Computer Management Center, Inc.
 *	Idaho Falls, Idaho.
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
 *	Date>Today
 *	.b
 *	.lm +5
 *	Returns the current date in YYYYMMDD format (8 char).
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	Returned value
 *		The current date in YYYYMMDD format (8 char)
 *
 * Example:
 *
 *	DATUM$ = DATE_TODAY
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:DATE_TODAY
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP DATE_TODAY
 *	$ DELETE DATE_TODAY.OBJ;*
 *
 * AUTHOR:
 *
 *	03/20/86 - Kevin Handy
 *
 * MODIFICATION HISTORY:
 *
 *	08/14/90 - Kevin Handy
 *		Removed unecessary code.
 *
 *	07/11/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 standards.
 *
 *	05/26/99 - Kevin Handy
 *		Modified to compile cleanly wirh DEC-C
 *		(module, sprintf bug, str$routines.h, descrip.h)
 *--
 */
/*
 * Include files
 */
#include <stdio.h>
#include <time.h>
#include <str$routines.h>
#include <descrip.h>

/*
 * Main function
 */
void date_today(struct dsc$descriptor *returnstr)
{
	/*
	 * Local Variables
	 */
	char makestring[10];
	time_t timer;
	struct tm *tblock;

	/*
	 * Get date structure
	 */
	timer = time(NULL);
	tblock = localtime(&timer);

	/*
	 * Create output date
	 */
	sprintf(makestring, "%04d%02d%02d",
		tblock->tm_year + 1900,
		tblock->tm_mon + 1,
		tblock->tm_mday);

	/*
	 * Return output string
	 */
	str$copy_r(returnstr, &8l, &makestring);
}
