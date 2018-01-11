//! \file
//! \brief Function to Enter a Date
// %SBTTL "ENTR_3DATE"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/entr_3date.bas
// Translated from Basic to C++ using btran
// on Tuesday, January 09, 2018 at 15:24:19
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"

//!
//! Abstract:HELP
//!	.b
//!	.lm +5
//!	This function enters a date.
//!	OPT_CPOS '' - Do not display on top area
//!	OPT_FLAG
//!	.table 3,25
//!	.te
//!	1	Don't enter data (display only?)
//!	.te
//!	4	Force keypunch input(no <CR> after input)
//!	.te
//!	8	Indicates a timeout on input will occur
//!	.te
//!	32	Use default value
//!	.te
//!	64	Don't display
//!	.te
//!	128	Return fincal value in default
//!	.end table
//!	.lm -5
//!
//! Parameters:
//!
//!	SCOPE
//!		Structure created during window initilization.
//!
//!	XX_VDID
//!		 Passed variable that creates or deletes the window that holds the string.
//!
//!	OPT_CPOS
//!		Position to place on XX_VDID.
//!		Does not display on XX_VDID if blank.
//!
//!	OPT_PROMPT
//!		The passed string used for the prompt and its initialization
//!
//!	OPT_XDFLT
//!		One of the passed defaults for the date.
//!
//!	OPT_FLAG
//!		.TABLE
//!		  1 - Don't enter data (display only?)
//!
//!		  4 - Force keypunch input(no <CR> after input)
//!
//!		  8 - Indicates a timeout on input will occur
//!
//!		 32 - Use default value
//!
//!		 64 - Don't display
//!
//!		128 - Return final value in default
//!		.END TABLE
//!
//!	OPT_XFORMAT
//!		The passed format for the date.
//!
//!	OPT_DEFLT
//!		The passed default form the string size.
//!
//!
//!	This function enters a date on the screen.
//!
//! AUTHOR:
//!
//!	07/02/85 - Kevin Handy
//!
std::string entr_3date(
	scope_struct &scope,
	smg_display_id &xx_vdid,
	const std::string &opt_cpos,
	const std::string &opt_prompt,
	std::string &opt_xdflt,
	long opt_flag,
	const std::string &opt_xformat,
	std::string &opt_deflt,
	long length)
{
	std::string Result;
	long def_len;
	std::string gets;
	long smg_status;
	std::string t_gets;
	long x;
	long xformat;
	long y;
	long y1pos;
	long Temp;
	long Temp2;

	long xpos;
	long ypos;
	//
	// ++

	// Seperate cursor position
	//
	dspl_splitcursor(opt_cpos, x, y);
	xpos = x;
	ypos = y;
	//
	// Choose the default to use
	//
	if ((opt_flag & 32) == 0)
	{
		gets = opt_xdflt;
	}
	else
	{
		gets = opt_deflt +
			std::string(opt_xdflt.size() - opt_deflt.size(), ' ');
	}
	//
	// Check the size - make it 6 or 8
	//
	if (gets.size() <= 6)
	{
		gets = (gets + std::string(6, ' ')).substr(0, 6);
		def_len = 6;
	}
	else
	{
		gets = (gets + std::string(2, ' ')).substr(0, 8);
		def_len = 8;
	}
	//
	// Calculate correct size for result
	//
	xformat = gets.size();
	if (opt_xformat == "6")
	{
		xformat = 6;
	}
	if (opt_xformat == "8")
	{
		xformat = 8;
	}
	//
	// Display only?
	//
	if ((opt_flag & 1) != 0)
	{
		goto l3000;
	}
	//
	// Display original
	//
	if ((opt_cpos != "") && ((opt_flag & 64) == 0))
	{
		smg_status = smg$put_chars(xx_vdid, prnt_date(gets, xformat),
			xpos, ypos, 0, SMG$M_REVERSE);
	}
	scope.scope_exit = 0;
	//
	// Initialization/prompt
	//
	smg_status = smg$erase_display(scope.smg_option);
	smg_status = smg$put_chars(scope.smg_option, opt_prompt + " <date>:",
		1, 1, 1, 0);
	y1pos = opt_prompt.size() + 10;
	//
l1100:;
	// Normal entry
	//
	t_gets = gets;
	gets = basic::right(gets, def_len - 3) + gets.substr(0, def_len - 4);
l1110:;
	// ** Converted from a select statement **
	//
	// Pick out all of the exit keys
	//
	Temp2 = -1;
	Temp = entr_3enter(scope, scope.smg_option, 1, y1pos, gets, Temp2,
		opt_flag);
	if ((Temp == SMG$K_TRM_CTRLC) ||
		(Temp == SMG$K_TRM_F8) ||
		(Temp == SMG$K_TRM_F10) ||
		(Temp == SMG$K_TRM_CTRLZ))
	{
		gets = t_gets;
		goto l3000;
	}
	t_gets = gets + "";
	gets = boost::erase_all_copy(gets, " ");
	//
	// Check date input for valid format
	//
	if (boost::trim_right_copy(gets) != "")
	{
//		if (gets != basic::Xlate(gets, basic::Qstring(48, 0) + "0123456789"))
		if (gets.find_first_not_of("0123456789") != gets.npos)
		{
			goto L_3500;
		}
	}
	gets = date_storedate(gets);
	if (boost::trim_right_copy(gets) != "")
	{
		if (boost::trim_right_copy(gets).size() != 8)
		{
			help_34message(scope, "invalid date format", "W",
				"ENTR_3DATE", "", "INVDATE");
			gets = t_gets;
			goto l1110;
		}
	}
	// ++
	// Warning:INVDATE
	//	^*Invalid Date\*
	//	.b
	//	.lm +5
	//	^*Explanation\*
	//	.b
	//	Invalid date or illegal date format.
	//	.b
	//	^*User Action\*
	//	.b
	//	The format for date entry is MMDDYY or MMDDYYYY, where
	//	MM is the month, DD is the day, YY is the year without entering the
	//	century (default is the current century), and YYYY is a full year.
	//	.b
	//	Check for the correct date or format and re-enter the input.
	//	.lm -5
	//
	// Index:
	//	.x date
	//
	// --
	//
	// Now Check out the date (Allow a blank date)
	//
	if (boost::trim_right_copy(gets) != "")
	{
		if (gets != date_invdcode(date_daycode(gets)))
		{
			gets = t_gets;
			goto L_3500;
		}
	}
	//
	// Handle user pressing list choices
	//
	if ((scope.scope_exit == SMG$K_TRM_F14) && ((opt_flag & 1024) == 0))
	{
		gets = date_3select(scope, gets);
		goto l1100;
	}
	if (def_len == 6)
	{
		gets = basic::right(gets, 3);
	}
	//
l3000:;
	// Display result
	//
	if ((opt_cpos != "") && ((opt_flag & 64) == 0))
	{
		smg_status = smg$put_chars(xx_vdid, prnt_date(gets, xformat), xpos, ypos, 0, SMG$M_BOLD);
	}
	Result = gets;
	//
	// Return text in default if supposed to
	//
	if (opt_flag & 128)
	{
		opt_deflt = gets;
	}
	return Result;
L_3500:;
	help_34message(scope, "invalid date", "W", "ENTR_3DATE", "", "INVDATE");
	gets = gets + std::string(def_len - gets.size(), ' ');
	goto l1110;
	return Result;
}
