//! \file
//! \brief "Fancy Month Formatter
#pragma module prnt_monthyy "V3.6 Calico"

/*
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
 * AUTHOR:
 *
 *	07/31/86 - Kevin Handy
 *--
 */


/*
 * Include files
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>

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
std::string prnt_monthyyyy(
	const std::string &adate)
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
	if (adate.size() == 8)
	{
		playtext[0] = '\0';
	}
	else
	{
		strcpy(playtext, "19");
	}
	strncat(playtext, adate.c_str(), adate.size());

	/*
	 * Pull numeric values of day, month, year
	 */
	playtext[6] = '\0';
	month = atoi(playtext + 4);
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

	return playtesxt;
}
