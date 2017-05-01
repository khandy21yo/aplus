/*	%TITLE "Fancy Month Formatter"
 */
#pragma module prnt_monthyy "V3.6 Calico"

/*
 * COPYRIGHT (C) 1988 BY
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
 *	.b
 *	.lm +5
 *	This function takes a date in one of the DATE_STOREDATE formats
 *	and sets up a fancy print string like: September 1986.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	ADATE$
 *		The passed date the user enters to have printed in a fancy
 *		string.
 *
 *	Returned value
 *		This function formats a date so it prints out the month in
 *		letters instead of numbers.
 *
 * Example:
 *
 *	DATES$ = PRNT_MONTHYYYY('19881109')
 *
 * Index:
 *
 *	.x Date>Print
 *	.x Print>Date
 *	.x Print>Month
 *	.x Month>Print
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:PRNT_MONTHYYYY
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP PRNT_MONTHYYYY
 *	$ DELETE PRNT_MONTHYYYY.OBJ;*
 *
 * AUTHOR:
 *
 *	07/31/86 - Kevin Handy
 *
 * MODIFICATION HISTORY:
 *
 *	07/09/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, str$routines.h, stdlib.h, stdio.h)
 *--
 */


/*
 * Include files
 */
#include <stdio.h>
#include <stdlib.h>
#include <descrip.h>
#include <string.h>
#include <str$routines.h>

/*
 * Static variables
 */
static char *monthname[] = 
{
	"***",
	"January",
	"February",
	"March",
	"April",
	"May",
	"June",
	"July",
	"August",
	"September",
	"October",
	"November",
	"December"
};

/*
 * Main function
 */
void prnt_monthyyyy(struct dsc$descriptor *returnstr,
	struct dsc$descriptor *adate)
{
	/*
	 * Local Variables
	 */
	char playtext[32];
	int year;
	int month;
	int length;

	/*
	 * Get a date in eight character format into play area
	 */
	if (adate->dsc$w_length == 8)
	{
		playtext[0] = '\0';
	}
	else
	{
		strcpy(playtext, "19");
	}
	strncat(playtext, adate->dsc$a_pointer, adate->dsc$w_length);

	/*
	 * Pull numeric values of day, month, year
	 */
	playtext[6] = '\0';
	month = atoi(playtext+4);
	playtext[4] = '\0';
	year = atoi(playtext);

	if ((month < 0) || (month > 12))
	{
		month = 0;
	}

	/*
	 * Create output string
	 */
	sprintf(playtext, "%s %04d", monthname[month], year);

	/*
	 * Send string back to program
	 */
	length = strlen(playtext);
	str$copy_r(returnstr, &length, &playtext);
}
