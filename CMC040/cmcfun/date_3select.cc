//! \file
//! \brief Select a Date from a Calendar
// %SBTTL "DATE_3SELECT"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/date_3select.bas
// Translated from Basic to C++ using btran
// on Thursday, January 11, 2018 at 15:56:09
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"
#include "pusing.h"

#include "preferences.h"
#include "cmcfun.h"
#include "smg/smg.h"
#include "scopedef.h"


// Abstract:COMMAND
//	^*Select a Date from a Calendar\*
//	^*CALENDAR\*
//	.b
//	.lm +5
//	^*Calendar\* displays a calendar by month from the selected starting date.
//	.b
//	^*Format: CALENDAR [date]\*
//	.b
//	^*Example:\*
//	.table 3,25
//	.te
//	Menu Command Level> /CALENDAR 19900809
//	.te
//	Menu Command Level> /CALENDAR
//	.end table
//	.lm -10
//
// Parameters:
//
//	SCOPE
//		Structure created by window initilization.
//
//	STARTING_DATE
//		The passed date at which to position into the calendar.
//		Uses current date if this is blank.
//		Must be in YYYYMMDD format.
//
//	The function returns the selected date in YYYYMMDD format.
//
// Example:
//
//	DATE_3SELECT("19880214")
//
// Author:
//
//	05/24/88 - Kevin Handy
//
// --
std::string date_3select(
	scope_struct &scope,
	const std::string &starting_date)
{
	std::string Result;
	long loop;
	std::string new_date;
	long next_month;
	long next_year;
	long offset;
	smg_display_id smg_calendar;
	long st;
	long starting_date_V2;
	std::string temp_ident;
	std::string temp_item;
	std::string temp_program;
	long this_age;
	long this_col;
	std::string this_date;
	long this_day;
	long this_line;
	std::string this_month;
	long this_month_V3;
	long this_year;
	long total_days;

	//
	// Save original help items
	//
	temp_ident = scope.prg_ident + "";
	temp_program = scope.prg_program + "";
	temp_item = scope.prg_item + "";
	scope.prg_ident = "H";
	scope.prg_program = "DATE_3SELECT";
	scope.prg_item = "HELP";
	//
	// Use either the date passes in, or the current date
	// if that is blank.
	//
	if (starting_date == "")
	{
		this_date = date_today();
	}
	else
	{
		this_date = basic::edit(starting_date, -1);
	}
	//
	// Create a window to use to display this calendar
	//
	st = smg$create_virtual_display(9, 29, smg_calendar, SMG$M_BORDER, SMG$M_REVERSE);
	st = smg$put_chars(smg_calendar, "Sun Mon Tue Wed Thr Fri Sat", 3, 2);
	st = smg$begin_pasteboard_update(scope.smg_pbid);
	st = smg$paste_virtual_display(smg_calendar, scope.smg_pbid, 4, 25);
	goto L_1010;
	//*******************************************************************
L_1000:;
	// Load in current month's calendar
	//*******************************************************************
	st = smg$begin_pasteboard_update(scope.smg_pbid);
	//
L_1010:;
	// Pull off the initial month, day, and year
	//
	this_year = std::stol(this_date.substr(0, 4));
	this_month_V3 = std::stol(basic::mid(this_date, 5, 2));
	this_day = std::stol(basic::mid(this_date, 7, 2));
	//
	// Erase current date information
	//
	st = smg$erase_display(smg_calendar, 4, 1);
	//
	// Calculate number of days in this month
	//
	if (this_month_V3 == 12)
	{
		next_month = 1;
		next_year = this_year + 1;
	}
	else
	{
		next_month = this_month_V3 + 1;
		next_year = this_year;
	}
	total_days = date_daycode(basic::Format(next_year, "<0>###") + basic::Format(next_month, "<0>#") + "01") - date_daycode(basic::Format(this_year, "<0>###") + basic::Format(this_month_V3, "<0>#") + "01");
	//
	// Find the day of week this month starts on
	//
	starting_date_V2 = date_dayofweek(date_daycode(basic::Format(this_year, "<0>###") + basic::Format(this_month_V3, "<0>#") + "01"));
	if (starting_date_V2 == 7)
	{
		starting_date_V2 = 0;
	}
	//
	// Place title of month across the top
	//
	this_month = prnt_monthyyyy(basic::Format(this_year, "<0>###") + basic::Format(this_month_V3, "<0>#") + "01");
	this_month = std::string((29 - this_month.size()) / 2, ' ') + this_month;
	st = smg$erase_line(smg_calendar, 1, 1);
	st = smg$put_chars(smg_calendar, this_month, 1, 1);
	//
	// Place all days on the screen
	//
	for (loop = 1; loop <= total_days; loop++)
	{
		this_line = (loop + starting_date_V2 - 1) / 7;
		this_col = 3 + (loop + starting_date_V2 - 1 - this_line * 7) * 4;
		st = smg$put_chars(smg_calendar, basic::Format(loop, "##"), this_line + 4, this_col);
	}
	//
	// Now let user see the changes
	//
	st = smg$end_pasteboard_update(scope.smg_pbid);
	//*******************************************************************
L_2000:;
	// Main loop inside of a month
	//*******************************************************************
	//
	// Force this date to be legal
	//
	if (this_day > total_days)
	{
		this_day = total_days;
	}
	//
	// Highlight current date
	//
	this_line = (this_day + starting_date_V2 - 1) / 7;
	this_col = 3 + (this_day + starting_date_V2 - 1 - this_line * 7) * 4;
	st = smg$put_chars(smg_calendar, basic::Format(this_day, "##"), this_line + 4, this_col, 0, 0, SMG$M_REVERSE);
	//
	// Enter a character from the user
	//
	entr_3messagenewwindow(scope, "Press <Exit> to exit, <Help> for help.", 0);
	//
	// Un-highlight current date
	//
	st = smg$put_chars(smg_calendar, basic::Format(this_day, "##"), this_line + 4, this_col);
	//
	// Handle the possibilities
	//
	// ** Converted from a select statement **
	//
	// Select this date
	//
	if (scope.scope_exit == SMG$K_TRM_SELECT)
	{
		goto L_8000;
		//
		// Uparrow
		//
	}
	else if (scope.scope_exit == SMG$K_TRM_UP)
	{
		offset = -7;
		goto offsetdate;
		//
		// Down-arrow
		//
	}
	else if (scope.scope_exit == SMG$K_TRM_DOWN)
	{
		offset = 7;
		goto offsetdate;
		//
		// left arrow
		//
	}
	else if (scope.scope_exit == SMG$K_TRM_LEFT)
	{
		offset = -1;
		goto offsetdate;
		//
		// Right arrow
		//
	}
	else if (scope.scope_exit == SMG$K_TRM_RIGHT)
	{
		offset = 1;
		goto offsetdate;
		//
		// Prev screen
		//
	}
	else if (scope.scope_exit == SMG$K_TRM_PREV_SCREEN)
	{
		offset = -1;
		goto offsetmonth;
		//
		// Next screen
		//
	}
	else if (scope.scope_exit == SMG$K_TRM_NEXT_SCREEN)
	{
		offset = 1;
		goto offsetmonth;
		//
		// Exit
		//
	}
	else if ((scope.scope_exit == SMG$K_TRM_F8) ||
		(scope.scope_exit == SMG$K_TRM_F10) ||
		(scope.scope_exit == SMG$K_TRM_CTRLC) ||
		(scope.scope_exit == SMG$K_TRM_CTRLZ))
	{
		Result = starting_date;
		goto exitfunction;
		//
		// Unused keys
		//
	}
	else
	{
		entr_3badkey(scope, scope.scope_exit);
	}
	goto L_2000;
	//*******************************************************************
	// Subroutine to move around by a specified number of days
	//*******************************************************************
offsetdate:;
	this_age = date_daycode(basic::Format(this_year, "<0>###") + basic::Format(this_month_V3, "<0>#") + basic::Format(this_day, "<0>#"));
	new_date = date_invdcode(this_age + offset);
	if (new_date.substr(0, 6) != this_date.substr(0, 8))
	{
		this_date = new_date;
		goto L_1000;
	}
	else
	{
		this_day = std::stol(basic::mid(this_date, 7, 2));
		goto L_2000;
	}
	//*******************************************************************
	// Handle offset by a number of months (Max 12)
	//*******************************************************************
offsetmonth:;
	this_month_V3 = this_month_V3 + offset;
	if (this_month_V3 > 12)
	{
		this_month_V3 = this_month_V3 - 12;
		this_year = this_year + 1;
	}
	if (this_month_V3 < 1)
	{
		this_month_V3 = this_month_V3 + 12;
		this_year = this_year - 1;
	}
	this_date = basic::Format(this_year, "<0>###") + basic::Format(this_month_V3, "<0>#") + basic::Format(this_day, "<0>#");
	goto L_1000;
	//*******************************************************************
L_8000:;
	// Return back the date
	//*******************************************************************
	Result = basic::Format(this_year, "<0>###") + basic::Format(this_month_V3, "<0>#") + basic::Format(this_day, "<0>#");
exitfunction:;
	st = smg$delete_virtual_display(smg_calendar);
	//
	// Restore original help items
	//
	scope.prg_ident = temp_ident;
	scope.prg_program = temp_program;
	scope.prg_item = temp_item;
	return Result;
}
