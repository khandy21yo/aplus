//! \file
//! \brief Format a Time String into 24-Hour Time.
// %SBTTL "TIME_STORETIME"
// %IDENT "V3.3"
//
// Source: ../../CMC030/cmcfun/source/time_storetime.bas_old
// Translated from Basic to C++ using btran
// on Wednesday, January 17, 2018 at 12:11:58
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"




//!
//! Abstract:HELP
//!	.p
//!	This function takes a users input, and comes up with
//!	a usable time formatted in HHMMSS(24-hour) format.  It can
//!	handle conditions as the following:
//!	.b
//!	.list 0,"*"
//!	.le
//!	H	AM/PM	H	(24-HOUR)
//!	.le
//!	HH	AM/PM	HH	(24-HOUR)
//!	.le
//!	HHM	AM/PM	HHM	(24-HOUR)
//!	.le
//!	HHMM	AM/PM	HHMM	(24-HOUR)
//!	.le
//!	HHMMS	AM/PM	HHMMS	(24-HOUR)
//!	.le
//!	HHMMSS	AM/PM	HHMMSS	(24-HOUR)
//!	.endlist
//!
//!	Where ':' is any non-numeric character.
//!	Those times that are short of 6 characters will be padded
//!	with '0'(zeros).  It will also handle elapse time up to
//!	99:59:59 hours.  Elapse time may be entered as follows:
//!	.b
//!	.list 0,"*"
//!	.le
//!		HH:MM:SS  if KIND$ = 'H' for hours(up to 995959).
//!	.le
//!		HHMMSS
//!	.le
//!		HHMMS
//!	.le
//!		HHMM
//!	.le
//!		HHM
//!	.le
//!		HH
//!	.le
//!		H
//!	.le
//!		MM:SS     if KIND$ = 'M' for minutes(up to 5959).
//!	.le
//!		MMSS
//!	.le
//!		MMS
//!	.le
//!		MM
//!	.le
//!		M
//!	.le
//!		SS	if KIND$ = 'S' for seconds(up to 59).
//!	.le
//!		S
//!	.endlist
//!
//!		Real time if KIND$ = ''
//!
//! Parameters:
//!
//!	XDEFLT$
//!		The passed time string the user wishes to have formatted.
//!
//!	KIND$
//!		The passed way the user wishes the time to be formatted.
//!
//!		Options are:
//!		.table
//!			'H' for hours(up to 995959).
//!			'M' for minutes(up to 5959).
//!			'S' for seconds(up to 59).
//!		.endtable
//!
//!	This function returns a usable time formatted in
//!	HHMMSS(24-hour) format.
//!
//! Author:
//!
//!	05/27/86 - B. Craig Larsen
//!
std::string time_storetime(
	const std::string &xdeflt,
	std::string kind)
{
	std::string Result;
	std::string ampm;
	std::string atime[11];
	std::string gets;
	std::string hms;
	long hr;
	long i;
	long k;
	std::string newt;
	std::string test;
	std::string x;

	// --
	// ++
	//
	// Variables:
	//
	//	HMS$
	//		String that holds the format for the hours, minutes,
	//		and seconds.
	//
	//	TEST$
	//		String used to test for numbers and colons.
	//
	//	NEWT$
	//		String that holds the formatted time value.
	//
	//	GETS$
	//		String that holds the origional time value.
	//
	//	I%
	//		Integer used to chose the correct format for the time.
	//
	//	HR%
	//		Integer that holds the hours value.
	//
	//	AMPM$
	//		String that splits the time to A.M. or P.M.
	//
	// --
	//
	// Initial Value
	//
	newt = gets = basic::edit(xdeflt, -1);
	if ((newt.find("AM", 0) + 1))
	{
		newt = newt.substr(0, newt.size() - 2);
	}
	if ((newt.find("PM", 0) + 1))
	{
		newt = newt.substr(0, newt.size() - 2);
	}
	// ** Converted from a select statement **
	//		CASE	'MIDNIGHT', 'MID', 'MI', '12MI', '12MID',
	//			'12MIDNIGHT', '1200MI', '1200MID', '120000MI',
	//			'120000MID', '12:00MI', '12:00MID', '12:00:00MI'
	//
	//			NEWT$	= '240000'
	//		CASE	'NOON', '12M', '1200M', '120000M', '12:00M', '12:00:00M'
	if ((newt == "12M") || ((newt == "1200M") || ((newt == "120000M") || ((newt == "12:00M") || (newt == "12:00:00M")))))
	{
		newt = "120000";
	}
	hms = basic::edit(kind, -1);
	//
	// Anything besides numbers and colons is illegal
	//
#if 0
//! \todo Xlate code
	test = basic::Xlate(newt, basic::Qstring(46, 0) + ":" + basic::Qstring(11, 0) + ":");
	if ((test.size() > 2) || (test == newt) || (basic::Xlate(newt, basic::Qstring(46, 0) + "." + 0 + "0123456789:") != newt))
#endif
	{
		goto L_10000;
	}
	//
	// Establish hours, minutes and seconds in that order
	//
	x = newt + "";
	for (k = 0; k <= 2; k++)
	{
		i = (x.find(":", 0) + 1);
		if (i == 0)
		{
			i = (x.find(".", 0) + 1);
		}
		switch (i)
		{
		// Colon at 1
		case 1:

			atime[k] = "00";
			x = basic::right(x, 2);
			// Colon at 2
			break;

		case 2:

			atime[k] = std::string("0") + x.substr(0, 1);
			x = basic::right(x, 3);
			// Colon at 3
			break;

		case 3:

			atime[k] = x.substr(0, 2);
			x = basic::right(x, 4);
			// No Colon or Colon > 3
			break;

		default:
			atime[k] = x.substr(0, 2);
			switch (atime[k].size())
			{
			case 0:

				atime[k] = "00";
				break;

			case 1:

				atime[k] = std::string("0") + atime[k];
				break;

			}
			x = basic::right(x, 3);
			break;

		}
	}
	newt = atime[0] + atime[1] + atime[2];
	//
	//	Strip non-numeric and calculate elaspe times
	//
	if (hms == "M")
	{
		newt = std::string("00") + newt.substr(0, 4);
	}
	if (hms == "S")
	{
		newt = std::string("0000") + newt.substr(0, 2);
	}
	hr = std::stol(newt.substr(0, 2));
	if (hr > 99 || std::stol(basic::mid(newt, 3, 2)) > 59 || std::stol(basic::mid(newt, 5, 2)) > 59 || (hms == "" && hr > 24))
	{
		goto L_10000;
	}
	else
	{
		if (hms == "")
		{
			ampm = basic::mid(gets, (gets.find("M", 0) + 1) - 1, 2);
			if ((ampm == "PM" && hr < 12) || (ampm == "AM" && hr == 12))
			{
				hr = hr + 12;
			}
			if (hr > 24)
			{
				hr = hr - 24;
			}
			newt = (basic::right(std::to_string(hr + 100), 2) + basic::right(newt, 3)).substr(0, 6);
		}
	}
L_10000:
	return newt.substr(0, 6);
}
