/*	%TITLE "Format a Date String into a Normal Looking Date"
 */
#pragma module date_storedate "V3.6 Calico"

/*
 *
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
 * Index:
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
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:DATE_STOREDATE
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP DATE_STOREDATE
 *	$ DELETE DATE_STOREDATE.OBJ;*
 *
 * AUTHOR:
 *
 *	11/26/85 - Kevin Handy
 *
 * MODIFICATION HISTORY:
 *
 *	12/04/85 - Kevin Handy
 *		Modified to handle M/D/ type dates.
 *
 *	09/24/91 - Frank F. Starman
 *		Return spaces if input is //.
 *
 *	07/16/93 - Kevin Handy
 *		Converted to C. Allowed '\' and '-' as seperators.
 *		Allowed 'I' as a stand in for 1. (I Hate Typewriters).
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/26/99 - Kevin Handy
 *		Modified to compile cleanly with DEC-C
 *		(module, stdlib.h, str$routines.h, string.h)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <descrip.h>
#include <str$routines.h>

/*
 * Main function
 */
void date_storedate(struct dsc$descriptor *returnstr,
	struct dsc$descriptor *xdeflt)
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
	for (loop = 0; loop < xdeflt->dsc$w_length; loop++)
	{
		switch (*(xdeflt->dsc$a_pointer+loop))
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
			if (isdigit(*(xdeflt->dsc$a_pointer+loop)))
			{
				workstring[worklen++] =
					*(xdeflt->dsc$a_pointer+loop);
			}
			break;
		}
	}

	/*
	 * One last cleanup, if date is mm/d
	 */
	if ((worklen == 3) && (sepflag != 0))
	{
		workstring[worklen] = workstring[worklen-1];
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

			sprintf(workstring+4, "%04d",
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
	sprintf(makestring, "%4.4s%4.4s", workstring+4, workstring);

	/*
	 * Return output string
	 */
	str$copy_r(returnstr, &8l, &makestring);
}
