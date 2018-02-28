//! \file
//! \brief Format Time into HH:MM:SS (24-hour)
// %SBTTL "PRNT_TIME"
// %IDENT "V3.3"
//
// Source: ../../CMC030/cmcfun/source/prnt_time.bas_old
// Translated from Basic to C++ using btran
// on Wednesday, January 17, 2018 at 12:02:06
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"
#include "pusing.h"

#include "preferences.h"
#include "cmcfun.h"


//!
//! Abstract:HELP
//!	.b
//!	.lm +5
//!	This function formats a time into HH:MM:SS (24-hour) or
//!	into HH:MM:SS am/pm time depending on the FLAG% flag.
//!	.b
//!	AM/PM If FLAG% AND 2%
//!	24-hour Else
//!	Strips off leading 00's if FLAG% and 8%
//!	FLAG% AND 4096% - Displays only hours
//!	FLAG% AND 2048% - Displays only hours and minutes
//!	.b
//!	It expects the time to be in HHMMSS (24-hour) form.
//!	.lm -5
//!
//! Parameters:
//!
//!	INTIME$
//!		The passed variable that holds the time the user wants to
//!		have formatted.
//!
//!	Returned value
//!		It returns the value of the time in HH:MM:SS form.
//!
//! Example:
//!
//!	TIME$ = PRNT_TIME("113425",2048%)
//!
//!
//! Author:
//!
//!	05/28/86 - B. Craig Larsen
//!
std::string prnt_time(
	const std::string &intime,
	long flag)
{
	std::string Result;
	std::string ampm;
	std::string thrs;
	std::string tmin;
	std::string tsec;
	std::string ttim;

	// ++
	//
	// Variables:
	//
	//	AMPM$
	//		String that tells if time is AM or PM.
	//
	//	THRS$
	//		String that extracts the hours.
	//
	//	TMIN$
	//		String that extracts the minutes.
	//
	//	TSEC$
	//		String that extracts the seconds.
	//
	//	TTIM$
	//		String is used to format the time.
	//
	// --
	ampm = "";
	thrs = intime.substr(0, 2);
	tmin = basic::mid(intime, 3, 2);
	tsec = basic::mid(intime, 5, 2);
	if (flag & 2)
	{
		if (thrs >= "12" && thrs < "24")
		{
			ampm = " PM";
		}
		if ((thrs < "12" && thrs >= "00") || thrs == "24")
		{
			ampm = " AM";
		}
		if (thrs >= "13" && thrs <= "24")
		{
			thrs = basic::Format(std::stol(thrs) - 12, "<0>");
		}
	}
	if (thrs == "")
	{
		thrs = "  ";
	}
	if (tmin == "")
	{
		tmin = "  ";
	}
	if (tsec == "")
	{
		tsec = "  ";
	}
	ttim = thrs + ":" + tmin + ":" + tsec + ampm;
	if (flag & 4096)
	{
		ttim = ttim.substr(0, 2);
	}
	if (flag & 2048)
	{
		ttim = ttim.substr(0, 5);
	}
	//
	// Strip off leading "00" or leading "00:00"
	//
	if (flag & 8)
	{
		if (ttim.substr(0, 5) == "00:00" || ttim.substr(0, 5) == "  :  ")
		{
			ttim = std::string("     ") + basic::right(ttim, 6);
		}
		else
		{
			if (ttim.substr(0, 3) == "00:")
			{
				ttim = std::string("   ") + basic::right(ttim, 4);
			}
		}
	}
	return ttim;
}
