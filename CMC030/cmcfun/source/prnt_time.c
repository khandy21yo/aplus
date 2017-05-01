/*	%TITLE "Format Time"
 */
#pragma module prnt_time "V3.6 Calico"

/*
 * COPYRIGHT (C) 1987 BY
 *
 * Computer Management Center
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
 *	.b
 *	.lm +5
 *	This function formats a time into HH:MM:SS (24-hour) or
 *	into HH:MM:SS am/pm time depending on the FLAG% flag.
 *	.b
 *	AM/PM If FLAG% AND 2%
 *	24-hour Else
 *
 *	Strips off leading 00's if FLAG% and 8%
 *
 *	FLAG% AND 4096% - Displays only hours
 *
 *	FLAG% AND 2048% - Displays only hours and minutes
 *	.b
 *	It expects the time to be in HHMMSS (24-hour) form.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	INTIME$
 *		The passed variable that holds the time the user wants to
 *		have formatted.
 *
 *	Returned value
 *		It returns the value of the time in HH:MM:SS form.
 *
 * Example:
 *
 *	TIME$ = PRNT_TIME("113425",2048%)
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:PRNT_TIME
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP PRNT_TIME
 *	$ DELETE PRNT_TIME.OBJ;*
 *
 * Author:
 *
 *	05/28/86 - B. Craig Larsen
 *
 * Modification history:
 *
 *	01/19/87 - Kevin Handy
 *		Added ability to strip off leading 00's with
 *		FLAG% AND 8%
 *
 *	02/25/88 - B. Craig Larsen
 *		Modified to display only hours or only hours and minutes
 *
 *	07/06/93 - Kevin Handy
 *		Modified to use VAL%( instead of VAL( for a tiny
 *		bit more speed.
 *
 *	07/13/93 - Kevin Handy
 *		Converted to C. (Requires hours to be non-goofy)
 *
 *	09/10/94 - Kevin Handy
 *		Modifications to handle cases where users enter strings
 *		that are shorter than 4 characters long.
 *
 *	03/13/95 - Kevin Handy
 *		Fixed bug where it was putting seconds in the min
 *		variable.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, stdio.h, str$routines.h sprintf, string.h)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <descrip.h>
#include <str$routines.h>

/*
 * Function Definitions
 */
#define TODIGIT(x)	(x - '0')

/*
 * Main function
 */
void prnt_time(struct dsc$descriptor *returnstr,
	struct dsc$descriptor *intime,
	long *flag)
{
	/*
	 * Local Variables
	 */
	char makestring[16];
	char *ampm = "";
	char *xintime;
	int xinlen;
	int hrs;
	int min;
	int sec;
	long length;

	/*
	 * Pull off hours and minutes portion, since we need to make
	 * comparisons against these parts.
	 */
	xintime = intime->dsc$a_pointer;
	xinlen = intime->dsc$w_length;

	if (xinlen >= 2)
	{
		if (isdigit(*xintime) && isdigit(*(xintime+1)))
		{
			hrs = TODIGIT(*xintime) * 10 + TODIGIT(*(xintime+1));
		}
		else
		{
			hrs = 0;
		}
	}
	else
	{
		hrs = 0;
	}

	if (xinlen >= 4)
	{
		if (isdigit(*(xintime+2)) && isdigit(*(xintime+3)))
		{
			min = TODIGIT(*(xintime+2)) * 10 + TODIGIT(*(xintime+3));
		}
		else
		{
			min = 0;
		}
	}
	else
	{
		min = 0;
	}

	if (xinlen >= 6)
	{
		if (isdigit(*(xintime+4)) && isdigit(*(xintime+5)))
		{
			sec = TODIGIT(*(xintime+4)) * 10 + TODIGIT(*(xintime+5));
		}
		else
		{
			sec = 0;
		}
	}
	else
	{
		sec = 0;
	}

	/*
	 * Handle AM/PM problems
	 */
	if ((*flag & 2) != 0)
	{
		if ((hrs >= 12) && (hrs < 24))
		{
			ampm = " PM";
		}
		else
		{
			ampm = " AM";
		}

		if ((hrs >= 13) && (hrs <= 24))
		{
			hrs -= 12;
		}
	}

	/*
	 * Create working string
	 */
	sprintf(makestring, "%02d:%02d:%02d%s",
		hrs, min, sec, ampm);

	/*
	 * Blank out leading '00's
	 */
	if (hrs == 0)
	{
		makestring[0] = ' ';
		makestring[1] = ' ';

		if (min == 0)
		{
			makestring[3] = ' ';
			makestring[4] = ' ';
		}
	}

	/*
	 * Strip off leading "  :" or leading "  :  :"
	 */
	if ((*flag & 8) != 0)
	{
		if (hrs == 0)
		{
			makestring[2] = ' ';

			if (min == 0)
			{
				makestring[5] = ' ';
			}
		}
	}

	/*
	 * Figure out how many characters to return back
	 */
	length = strlen(makestring);

	if ((*flag & 2048) != 0)
	{
		length = 5;
	}

	if ((*flag & 4096) != 0)
	{
		length = 2;
	}

	/*
	 * Return output string
	 */
	str$copy_r(returnstr, &length, &makestring);
}
