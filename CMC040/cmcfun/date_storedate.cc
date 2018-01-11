//! \file
//! \brief Format a Date String into a Normal Looking Date
//!
#pragma module date_storedate "V3.6 Calico"

#include <string>

#include "preferences.h"
#include "cmcfun.h"

/*
 * Include files
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

/*
 * Abstract:HELP
 *	.b
 *	.lm +5
 *	This function mangles a users input, and comes up with
 *	a usable date formatted in YYYYMMDD format. It can
 *	handle conditions as the following:
 *	.table 3,25
 *	.te
 *	MMDD MM/DD MM.DD
 *	.te
 *	M/DD M.DD
 *	.te
 *	MMDDYY MM/DD/YY MM.DD.YY
 *	.te
 *	M/DD/YY M.DD.YY
 *	.te
 *	MM/D/YY MM.D.YY
 *	.te
 *	M/D/YY M.D.YY
 *	.te
 *	MMDDYYYY MM/DD/YYYY MM.DD.YYYY
 *	.te
 *	M/DD/YYYY M.DD.YYYY
 *	.te
 *	MM/D/YYYY MM.D.YYYY
 *	.te
 *	M/D/YYYY M.D.YYYY
 *	.end table
 *	If it returns a date with a length other than 8
 *	then it couldn't handle it properly.
 *	.lm -5
 *
 * Parameters:
 *
 *	XDEFLT$
 *		The passed string the user enters to change the date
 *		format into YYYYMMDD format.
 *
 *
 *	Returned value
 *		A date string that is more normal looking.
 *
 * Example:
 *
 *	DAY$ = DATE_STOREDATE("01121988")
 *
 * AUTHOR:
 *
 *	11/26/85 - Kevin Handy
 *
 *--
 */

/*
 * Main function
 */
std::string date_storedate(
	const std::string &xdeflt)
{
	/*
	 * Local Variables
	 */
	char makestring[10];
	char workstring[10];
	int worklen = 0;
	int loop;
	time_t timer;
	struct tm *tblock;
	int sepflag = 0;

	/*
	 * Initial pass, pull string from descriptor, while converting
	 * "L" to 1, "O" to 0, while forcing two-digit positions
	 * before '.', '\' and '/'.
	 */
	for (loop = 0; loop < xdeflt.size(); loop++)
	{
		switch (xdeflt[loop])
		{
		/*
		 * Switch o's to zeroes for those used to IBM typewriters
		 */
		case 'O':
		case 'o':
			workstring[worklen++] = '0';
			break;

		/*
		 * Switch L's to ones for those used to IBM typewriters
		 */
		case 'L':
		case 'l':
		case 'I':
		case 'i':
			workstring[worklen++] = '1';
			break;

		/*
		 * For a seperator, make sure we have correct positions
		 * for previous digits
		 */
		case '.':
		case '/':
		case '\\':
		case '-':
			sepflag = -1;
			if ((worklen == 1) || (worklen == 3))
			{
				workstring[worklen] = workstring[worklen-1];
				workstring[worklen-1] = '0';
				worklen++;
			}
			break;

		/*
		 * Append digits to output string
		 */
		default:
			if (isdigit(xdeflt[loop]))
			{
				workstring[worklen++] =
					xdeflt[loop];
			}
			break;
		}
	}

	/*
	 * One last cleanup, if date is mm/d
	 */
	if ((worklen == 3) && (sepflag != 0))
	{
		workstring[worklen] = workstring[worklen - 1];
		workstring[worklen-1] = '0';
		worklen++;
	}

	/*
	 * Handle string depending on how many characters it has in it
	 */
	switch (worklen)
	{
		/*
		 * Null date entered
		 */
		case 0:
			strcpy(workstring, "        ");
			break;

		/*
		 * Must be MMDD, append current year in YYYY format.
		 */
		case 4:
			timer = time(NULL);
			tblock = localtime(&timer);

			sprintf(workstring + 4, "%04d",
				tblock->tm_year + 1900);
			break;

		/*
		 * Must be MMDDYY, insert century, assuming that the
		 * century is the same one as we are currently in.
		 */
		case 6:
			timer = time(NULL);
			tblock = localtime(&timer);

			sprintf(makestring, "%02d%2.2s",
				(tblock->tm_year + 1900) / 100,
				workstring+4);
			strcpy(workstring+4, makestring);
			break;

		/*
		 * Otherwise fake it up to eight characters
		 */
		default:
			workstring[worklen] = '\0';
			sprintf(makestring, "%8.8s",
				workstring);
			strcpy(workstring, makestring);
			break;
	}

	/*
	 * At this point, we will have a date in MMDDYYY (hopefully)
	 * format, and if we don't we must have been given a really
	 * bad mess to start with. It will definately be eight characters
	 * due to above select statement.
	 */

	/*
	 * Now put the year in front
	 */
	sprintf(makestring, "%4.4s%4.4s", workstring + 4, workstring);

	/*
	 * Return output string
	 */
	return makestring;
}
