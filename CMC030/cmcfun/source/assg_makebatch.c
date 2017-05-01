/* %TITLE "Generate a batch number given a date and a time"
 */
#pragma module assg_makebatch "V3.6 Calico"

/*
 * Copyright (C) 1994 by Software Solutions
 * Idaho Falls, Idaho  83402.
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
 * Software Solutions.
 *
 * Software Solutions assumes no responsibility for the use or
 * reliability of its software on equipment which is not supported
 * by Software Solutions.
 *
 *++
 *
 * Abstract:HELP
 *	.b
 *	.lm +5
 *	This functon will generate a batch number when given
 *	a date and time.
 *	.b
 *	The method used to generate a batch number is to calculate
 *	the number of seconds since the beginning of the system's
 *	date, and then stuff as much of that into a RADIX(32?)
 *	value as possible. The loss of the high order bits causes
 *	the value to cycle every 68 years.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	GIVEN_DATE$
 *		This is the date to generate the batch number for,
 *		in YYYYMMDD format.
 *
 *	GIVEN_TIME$
 *		This is the time to generate the batch number for,
 *		in HHMMSS format.
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:ASSG_MAKEBATCH
 *	$ LIB FUNC_LIB:CMCFUN/REP ASSG_MAKEBATCH
 *	$ DELETE ASSG_MAKEBATCH.OBJ;*
 *
 * Author:
 *
 *	09/28/94 - Kevin Handy
 *		Ripped code out of ASSG_POSTBATCH so there would
 *		only be one source for this code.
 *
 * Modification History:
 *
 *	09/28/94 - Kevin Handy
 *		Modified formula so that is uses (char% ^ 6)
 *		instead of (char% ^ 6 - 1) which is harder
 *		to reverse. This change causes approximately
 *		7 difference in the batch number.
 *
 *	03/07/95 - Kevin Handy
 *		Rewrote in C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Coding standards.
 *
 *	05/26/99 - Kevin Handy
 *		Modify to lose warnings from DEC-C
 *		(module, str$routines.h)
 *--
 */

/*
 * Include files
 */
#include <math.h>
#include <descrip.h>
#include <str$routines.h>

/*
 * External functions
 */
long date_daycode(struct dsc$descriptor_s *UseDate);
long time_code(struct dsc$descriptor_s *UseTime);

/*
 * local constants
 */
static const char character[] = "23456789ABCDEFGHJKLMNPQRSTUVWXYZ";

/*
 * Main function
 */
void assg_makebatch(struct dsc$descriptor_s *result,
	struct dsc$descriptor_s *given_date,
	struct dsc$descriptor_s *given_time)
{
	int alpha;
	int loop;
	char prior_number[7];
	double seconds;
	int radix;

	/*
	 * Define the RADIX and the characters used to print that RADIX.
	 * Note that some characters are missing, because trying to
	 * identify them can be hard from a printout, i.e. (0 and O,
	 * 1 and I)
	 */
	radix = sizeof(character) - 1;

	/*
	 * Generate the number of seconds since the beginning of time
	 * (according to the date function).
	 */
	seconds = date_daycode(given_date) * 60.0 * 60.0 * 24.0 +
		time_code(given_time);

	/*
	 * Trim it down to the maximum allowed for RADIX(char%)
	 */
	seconds = seconds -
		floor(seconds / pow(radix, 6.0)) * pow(radix, 6.0);

	/*
	 * Convert the number to RADIX(char%)
	 *
	 */
	prior_number[6] = '\0';
	for (loop = 5; loop >= 0; loop--)
	{
		alpha = seconds - floor(seconds / radix) * radix;
		seconds = floor(seconds / radix);
		prior_number[loop] = character[alpha];
	}

	/*
	 * Return output string
	 */
	str$copy_r(result, &6l, &prior_number);
}
