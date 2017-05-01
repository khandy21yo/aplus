/*	%TITLE "Time Aging Function"
 */
#pragma module time_code "V3.6 Calico"

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
 * notice and should not be construed as a committment by
 * Computer Management Center, Inc.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
 * Abstract:HELP
 *	.p
 *	This function will convert a time into a long integer
 *	consisting of the number of seconds (since midnight).
 *
 * Parameters:
 *
 *	DATUM$ 
 *		The passed time in question in the form HHMMSS, or
 *		MMSS or SS.
 *
 *	Returned value
 *		Returns a long integer containing the number of seconds
 *		(since midnight).
 *
 * Example:
 *
 *	GOOF_TIME% = TIME_CODE("002030")
 *
 * Compile
 *
 *	$ CC FUNC_SOURCE:TIME_CODE/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP TIME_CODE
 *	$ DELETE TIME_CODE.OBJ;*
 *
 * Author:
 *
 *	11/27/87 - Kevin Handy
 *
 * Modification history:
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module)
 *--
 */

#include <descrip.h>

long time_code(struct dsc$descriptor_s *datum)
{
	long total_time = 0;
	int loop;
	char *ptr;

	ptr = datum->dsc$a_pointer;

	/*
	 * Loop through time string
	 */
	for (loop = 0; loop < datum->dsc$w_length; loop += 2)
	{
		/*
		 * Add on next increment
		 */
		total_time = total_time * 60 +
			(ptr[loop] - '0') * 10 +
			(ptr[loop+1] - '0');
	}

	return(total_time);
}
