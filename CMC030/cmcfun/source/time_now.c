/*	%TITLE "Returns the Current Time in HHMMSS(24-Hour) Form"
 */
#pragma module time_now "V3.6 Calico"

/*
 * COPYRIGHT (C) 1987 BY
 *
 * Computer Management Center, Inc.
 * Idaho Falls, Idaho.
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
 *	.p
 *	Returns the current time in the HHMMSS (24-hour) form.
 *
 * Parameters:
 *
 *	Returns the current time in the HHMMSS form.
 *
 * Example:
 *
 *	NOW$ = TIME_NOW
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:TIME_NOW
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP TIME_NOW
 *	$ DELETE TIME_NOW.OBJ;*
 *
 * AUTHOR:
 *
 *	5/28/86 - B. Craig Larsen
 *
 * MODIFICATION HISTORY:
 *
 *	08/14/90 - Kevin Handy
 *		Removed unecessary code.
 *
 *	07/13/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, descrip.h, str$routines.h)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <time.h>
#include <descrip.h>
#include <str$routines.h>

/*
 * Main function
 */
void time_now(struct dsc$descriptor_s *returnstr)
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
	sprintf(makestring, "%02d%02d%02d",
		tblock->tm_hour,
		tblock->tm_min,
		tblock->tm_sec);

	/*
	 * Return output string
	 */
	str$copy_r(returnstr, &6l, &makestring);
}
