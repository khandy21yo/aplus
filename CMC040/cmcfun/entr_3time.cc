//! \file
//! \brief Function to Enter a Time
// %SBTTL "ENTR_3TIME"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/entr_3time.bas
// Translated from Basic to C++ using btran
// on Wednesday, January 17, 2018 at 11:43:37
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
//!
//! Parameters:
//!
//!	OPT_CPOS
//!		Position to display data.
//!		"" - Do not display on top area
//!
//!	OPT_FLAG
//!	.table
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
//!		128 - Return value in OPT_DEFLT
//!
//!	       2048	Display only hours and minutes
//!
//!	       4096	Display only hours
//!	.endtable
//!
//!	XX_VDID
//!		Passed variable that creates or deletes the
//!		window that holds the string.
//!
//!	OPT_PROMPT
//!		The passed string used for the prompt and its initialization
//!
//!	OPT_XDFLT
//!		One of the passed defaults for the time format.
//!
//!	OPT_KIND
//!		The passed format for the time.
//!
//!	OPT_DEFLT
//!		The default form of the time size.
//!
//!
//!	This function enters a time on the screen in the user's format.
//!
//! Author:
//!
//!	05/29/86 - B. Craig Larsen
//!
std::string entr_3time(
	scope_struct &scope,
	smg_display_id &xx_vdid,
	const std::string &opt_cpos,
	const std::string &opt_prompt,
	const std::string &opt_xdflt,
	long opt_flag,
	const std::string &opt_kind,
	std::string &opt_deflt)
{
	std::string Result;
	std::string gets;
	std::string gets1;
	long smg_status;
	long x;
	long y;
	long y1pos;

	long xpos;
	long ypos;
	long Temp;
	long Temp2;
	//
	// Split cursor position
	//
	dspl_splitcursor(opt_cpos, x, y);
	xpos = x;
	ypos = y;
	//
	//	Limit the OPT_XDFLT to 10 char
	//
	gets = std::string(10, ' ');
	if (opt_flag & 32)
	{
		Lset(gets, opt_deflt);
	}
	else
	{
		Lset(gets, opt_xdflt);
	}
	//
	//	Check for no input
	//
	if (opt_flag & 1)
	{
		goto l3000;
	}
	if ((opt_cpos != "") && ((opt_flag & 64) == 0))
	{
		smg_status = smg$put_chars(xx_vdid, prnt_time(gets, opt_flag), xpos, ypos, 0, SMG$M_REVERSE);
	}
	//
l1000:;
	// Initialization/prompt
	//
	smg_status = smg$erase_display(scope.smg_option);
	// Window
	// Message to display
	// Line
	// Column
	// Erase line
	// Attributes
	smg_status = smg$put_chars(scope.smg_option, opt_prompt + " <time>:", 1, 1, 1, 0, 0);
	y1pos = opt_prompt.size() + 10;
	//
	// Hours minutes
	//
	if (opt_flag & 4096)
	{
		gets = gets.substr(0, 2);
	}
	if (opt_flag & 2048)
	{
		gets = gets.substr(0, 5);
	}
	//
	// Normal entry
	//
	// ** Converted from a select statement **
	Temp2 = -1;
	Temp = entr_3enter(scope, scope.smg_option, 1, y1pos, gets, Temp2, opt_flag);
	if ((Temp == SMG$K_TRM_CTRLC) ||
		(Temp == SMG$K_TRM_F8) ||
		(Temp == SMG$K_TRM_F10) ||
		(Temp == SMG$K_TRM_CTRLZ))
	{
		gets = opt_xdflt;
		goto l3000;
	}
	gets = boost::erase_all_copy(gets, " ");
	switch (gets.size())
	{
	// Convert - its good
	case 1:
	case 2:
	case 3:
	case 4:
	case 5:
	case 6:
	case 7:
	case 8:
	case 9:
	case 10:

		gets1 = gets;
		gets = time_storetime(gets, opt_kind);
		if (gets == "")
		{
			gets = gets1 + std::string(10 - gets1.size(), ' ');
			help_34message(scope, "invalid time", "W", "ENTR_3TIME", "", "INVTIME");
			// ++
			// Warning:INVTIME
			//	^*Invalid Time\*
			//	.b
			//	.lm +5
			//	^*Explanation\*
			//	.b
			//	Invalid time or illegal time format.
			//	.b
			//	^*User Action\*
			//	.b
			//	Check for right time or format and re-enter input.
			//	.lm -5
			//
			// Index:
			//	.x Time
			//
			// --
			goto l1000;
		}
		// Return blank string on C/R
		break;

	default:
		gets = std::string(opt_xdflt.size(), ' ');
		break;

	}
	//
l3000:;
	// Exit function
	//
	if ((opt_cpos != "") && ((opt_flag & 64) == 0))
	{
		// Window
		// Message to display
		// Line
		// Column
		// Erase screen
		// Attributes
		smg_status = smg$put_chars(xx_vdid, prnt_time(gets, opt_flag), xpos, ypos, 0, SMG$M_BOLD, 0);
	}
	Result = gets;
	if (opt_flag & 128)
	{
		opt_deflt = gets;
	}
	return Result;
}
