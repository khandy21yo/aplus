//! \file
//! \brief Fancy Date Formatter
//
#pragma module prnt_fancydate "V3.6 Calico"

/*
 * Include files
 */
#include <stdlib.h>
#include <string>

/*
 * Static variables
 */
static const char *monthname[] = 
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

//!
//! Abstract:HELP
//!	.b
//!	.lm +5
//!	This function takes a date in one of the DATE_STOREDATE formats
//!	and sets up a fancy print string like: September 1, 1986.
//!	This function differs from PRNT_MONTHYYYY in that it prints out
//!	the whole date, where PRNT_MONTHYYYY only prints the month and
//!	year.
//!	.lm -5
//!
//! Parameters:
//!
//!	ADATE$
//!		The passed date the user enters to have formatted into a
//!		fancy string.
//!
//!
//!	Returned value
//!		This function formats the date the user specifies into a
//!		fancy date by changing the month into a readable word and
//!		pulling it all together.
//!
//! Example:
//!
//!	DATES$ = PRNT_FANCYDATE('10231988')
//!
//!
//! \author 07/31/86 - Kevin Handy
//!
//!
std::string prnt_fancydate(
	const std::string &adate)
{
	/*
	 * Local Variables
	 */
	std::string playtext;
	int year;
	int month;
	int day;
	int length;

	/*
	 * Get a date in eight character format into play area
	 */
	if (adate.size() == 8)
	{
		playtext = adate;
	}
	else
	{
		playtext = "19" + adate;
	}

	/*
	 * Pull numeric values of day, month, year
	 */
	day = std::strtol(playtext.substr(6, 2).c_str(), 0, 10);
	month =std::strtol(playtext.substr(4,2).c_str(), 0, 10);
	year = std::strtol(playtext.substr(0, 4).c_str(), 0, 10);

	if ((month < 0) || (month > 12))
	{
		month = 0;
	}

	/*
	 * Create output string
	 */
	playtext = std::string(monthname[month]) + " " +
		std::to_string(day) + "," +
		std::to_string(year);

	return playtext;
}
