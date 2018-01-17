//! \file
//! \brief Function to Enter Yes/No Response
// %SBTTL "ENTR_3YESNO"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/entr_3yesno.bas
// Translated from Basic to C++ using btran
// on Wednesday, January 17, 2018 at 01:46:41
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"
#include "pusing.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"


//!
//! Abstract:HELP
//!	.b
//!	.lm +5
//!	Enters Yes or No.
//!	.lm -5
//!
//! Parameters:
//!
//!	OP_CPOS
//!		Position to display data.
//!		"" - Do not display on top area
//!
//!	OP_FLAG
//!	.table
//!		  1 - Print data without entry
//!		  4 - Keypunch mode
//!		  8 - Timeout
//!		 32 - Use default values
//!		 64 - Don't display
//!		128 - Return result in OP_XDEFLT
//!	.endtable
//!
//!	XX_VDID
//!		Passed variable that creates or deletes the window
//!		that holds the string.
//!
//!	OP_PROMPT
//!		The passed string used for the prompt and its initialization
//!
//!	OP_DEFLT
//!		One of the passed defaults for the data.
//!
//!	OP_XFORMAT
//!		The passed format for the data.
//!
//!	OP_XDEFLT
//!		The passed default form the data size.
//!
//!
//!	This function returns a yes or no value to the screen.
//!
//! AUTHOR:
//!
//!	07/09/85 - Kevin Handy
//!
std::string entr_3yesno(
	scope_struct &scope,
	smg_display_id &xx_vdid,
	const std::string &op_cpos,
	const std::string &op_prompt,
	const std::string &op_deflt,
	long op_flag,
	const std::string &op_xformat,
	std::string &op_xdeflt)
{
	std::string Result;
	std::string gets;
	long gets_V2;
	std::string junk;
	long smg_status;
	long x;
	long y;

	long xpos;
	long ypos;
	long y1pos;
	std::vector<std::string> vtext {
		"2",
		"Y    Yes",
		"N    No"};

	//
	// Function To Expand 'Y' and 'N' into three characters YES/NO
	//
	auto fnyn = [&](std::string strx)
	{
		std::string Result;

		// ** Converted from a select statement **
		if (strx == "Y")
		{
			Result = "Yes";
		}
		else if (strx == "N")
		{
			Result = "No ";
		}
		else
		{
			Result = strx + "  ";
		}
		return Result;
	};
	//
	// Split out cursor positioning function
	//
	dspl_splitcursor(op_cpos, x, y);
	xpos = x;
	ypos = y;
	//
	// Decide which value to use
	//
	if (op_flag & 32)
	{
		gets = op_xdeflt;
	}
	else
	{
		gets = op_deflt;
	}
	//
	// Don't enter
	//
	if (op_flag & 1)
	{
		goto L_3000;
	}
	//
	// Display original value
	//
	// Window
	// Data
	// Line
	// Column
	// Erase flag
	// Attributes
	if ((op_cpos != "") && ((op_flag & 64) == 0))
	{
		smg_status = smg$put_chars(xx_vdid, basic::Format(fnyn(gets), op_xformat), xpos, ypos, 0, SMG$M_REVERSE);
	}
	//
	// Initilization/prompt
	//
	smg_status = smg$erase_display(scope.smg_option);
	smg_status = smg$put_chars(scope.smg_option, op_prompt + " <Yes/No>:", 1, 1, 1);
	//
	// Unhide cursor
	//
	smg_status = smg$set_cursor_mode(scope.smg_pbid, 0);
	y1pos = op_prompt.size() + 12;
	//
L_1100:;
	// Normal entry
	//
	smg_status = smg$put_chars(scope.smg_option, fnyn(gets), 1, y1pos, 0);
	smg_status = smg$set_cursor_abs(scope.smg_option, 1, y1pos);
	junk = gets;
	gets_V2 = entr_4entry(scope, scope.smg_option, 256);
	gets_V2 = entr_4specialkeys(scope, scope.smg_option, 256, gets_V2);
	if (gets_V2 == 0)
	{
		goto L_1100;
	}
	if ((gets_V2 < 32) || (gets_V2 > 126))
	{
		gets = junk;
		scope.scope_exit = gets_V2;
	}
	else
	{
		gets = boost::to_upper_copy(std::string(1, (char)gets_V2));
		scope.scope_exit = 0;
	}
	if (gets != "Y")
	{
		gets = "N";
	}
	//
	// Test Input
	//
	// ** Converted from a select statement **
	//
	// Normal character typed
	//
	if (gets_V2 >= 32 && gets_V2 <= 126)
	{
		if ((op_flag & 4) == 0)
		{
			goto L_1100;
		}
		smg_status = smg$put_chars(scope.smg_option, fnyn(gets), 1, y1pos, 0);
		smg_status = smg$set_cursor_abs(scope.smg_option, 1, y1pos);
		//
		// List Choices
		//
	}
	else if (gets_V2 == SMG$K_TRM_F14)
	{
		x = entr_3choice(scope, "", "", vtext, gets, 138, "Value  Description", "006", 0);
		if (x > 0)
		{
			gets = basic::edit(vtext[x].substr(0, 5 - 3), 4 + 8 + 32 + 128 + 256);
		}
		goto L_1100;
	}
	if ((gets != "Y") && (gets != "N"))
	{
		entr_3message(scope, "Please enter 'Y' or 'N'", 1);
		goto L_1100;
	}
	//
L_3000:;
	// Exit function
	//
	// Window
	// Data
	// Line
	// Column
	// Erase flag
	// Attributes
	if ((op_cpos != "") && ((op_flag & 64) == 0))
	{
		smg_status = smg$put_chars(xx_vdid, basic::Format(fnyn(gets), op_xformat), xpos, ypos, 0, SMG$M_BOLD);
	}
	smg_status = smg$flush_buffer(scope.smg_pbid);
	//
	// Hide cursor
	//
	smg_status = smg$set_cursor_mode(scope.smg_pbid, 1);
	Result = gets;
	if (op_flag & 128)
	{
		op_xdeflt = gets;
	}
	return Result;
}
