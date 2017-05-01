/*	%TITLE "Time Aging Function"
 */
#pragma module time_invcode "V3.6 Calico"

/*
 *	COPYRIGHT (C) 1985 BY
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
 *	.p
 *	This function will conver a number into a time string.
 *
 *	WARNING: When adding a length to a time, you must
 *	subtract off one second.  A one hour period starting
 *	at 07:00:00 ends at 07:59:59, not 08:00:00, so you
 *	must subtract off one second when adding a length
 *	to a time.
 *
 * Parameters:
 *
 *	DATUM%
 *		The passed time in question in seconds.
 *
 *	Returned value
 *		Returns a time in the format HH:MM:SS.  Any overflow
 *		above 60 hours is lost.
 *
 * Example:
 *
 *	GOOF_TIME$ = TIME_INVCODE(3600%)
 *
 * Compile
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:TIME_INVCODE
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP TIME_INVCODE
 *	$ DELETE TIME_INVCODE.OBJ;*
 *
 * Author:
 *
 *	11/27/87 - Kevin Handy
 *
 * Modification history:
 *
 *	10/24/92 - Kevin Handy
 *		Changed TEMP% to TEMP to change a int-to-dfloat
 *		conversion to an int-to-gfloat.
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
 *		(module, stdio.h, str$routines.h, sprintf, descrip.h)
 *--
 */

#include <stdio.h>
#include <descrip.h>
#include <str$routines.h>

/*
 * Main function
 */
void time_invcode(struct dsc$descriptor *returnstr, long *datum)
{
	/*
	 * Local Variables
	 */
	char makestring[10];

	/*
	 * Create output time
	 */
	sprintf(makestring, "%02d%02d%02d",
		(*datum / (60 * 60)),		/* Hours */
		(*datum / 60) % 60,		/* Minutes */
		(*datum) % 60);			/* Seconds */

	/*
	 * Return output string
	 */
	str$copy_r(returnstr, &6l, &makestring);
}
