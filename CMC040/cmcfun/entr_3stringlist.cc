//! \file
//! \brief Function to Enter a String with Table Verification
// %SBTTL "ENTR_3STRINGLIST"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/entr_3stringlist.bas
// Translated from Basic to C++ using btran
// on Monday, January 15, 2018 at 12:20:00
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


//!
//! Abstract:HELP
//!	.b
//!	.lm +5
//!	Enters a string and matches against a list of valid
//!	strings.
//!	.b
//!	List choices allows selecting from the list of strings.
//!	.b
//!	This function requires that the length of the array
//!	list in the function be passed in the zero element of
//!	that array list. The array list is now labeled as
//!	OP_VTEXT(), the number of entries in that table would
//!	be entered in the OP_VTEXT(0%) element as a number.
//!	.lm -5
//!
//! Parameter:
//!
//!	OP_CPOS
//!		Position to display string.
//!		"" - Do not display on top area
//!
//!	OP_FLAG
//!	.table
//!		  1 - Don't enter data (display only?)
//!
//!		  2 - Rset string instead of lset
//!
//!		  4 - Force keypunch input(no <CR> after input)
//!
//!		  8 - Indicates a timeout on input will occur
//!
//!		 16 - Convert from lowercase to uppercase
//!
//!		 32 - Use default value
//!
//!		 64 - Don't display
//!
//!		128 - Return value in OP_XDEFLT
//!
//!		2048 - Remove leading spaces in list
//!
//!		16384 - Allow up-arrow to exit
//!	.endtable
//!
//!	XX_VDID
//!		The passed variable that creates or deletes the
//!		window that holds the string.
//!
//!	OP_PROMPT
//!		The passed string used for the prompt and its initialization
//!
//!	OP_XSTART
//!		Passed variable used to decide between the default values
//!		and normal values.
//!
//!	OP_XFORMAT
//!		The passed format for the string.
//!
//!	OP_XDEFLT
//!		The default form the string size.
//!
//!	OP_VTEXT()
//!		Used to check against tables.
//!
//!	OP_THETITLE
//!		The title for the choice menu.
//!
//!	OP_DRAWCOLS
//!		Used to draw the columns of the menu.
//!
//!
//!	This function enters a string with a table verification.
//!
//! AUTHOR:
//!
//!	06/24/85 - Kevin Handy
//!
std::string entr_3stringlist(
	scope_struct &scope,
	smg_display_id &xx_vdid,
	const std::string &op_cpos,
	const std::string &op_prompt,
	const std::string &op_xstart,
	long &op_flag,
	const std::string &op_xformat,
	std::string &op_xdeflt,
	const std::vector<std::string> &op_vtext,
	const std::string &op_thetitle,
	const std::string &op_drawcols)
{
	std::string Result;
	std::string draw_c;
	std::string gets;
	long i;
	long smg_status;
	long temp;
	long test_seg;
	long tt;
	long x;
	long y;
	long y1pos;
	long Temp;

	long xpos;
	long ypos;
	//

	//
	// Split out cursor positioning function
	//
	dspl_splitcursor(op_cpos, x, y);
	xpos = x;
	ypos = y;
	//
	// Decide between default value and normal value
	//
	if (op_flag & 32)
	{
		gets = std::string(op_xstart.size(), ' ');
		Lset(gets, op_xdeflt);
	}
	else
	{
		gets = op_xstart;
	}
	//
	// If display only
	//
	if (op_flag & 1)
	{
		goto l3200;
	}
	//
	// Initial display of item in reverse video
	//
	if ((op_cpos != "") && ((op_flag & 64) == 0))
	{
		// Window
		// Message to display
		// Line
		// Column
		// Erase screen
		// Attributes
		smg_status = smg$put_chars(xx_vdid, basic::Format(gets, op_xformat), xpos, ypos, 0, SMG$M_REVERSE, 0);
	}
	//
	// Initilization/prompt
	//
	smg_status = smg$erase_display(scope.smg_option);
	// Window
	// Message to display
	// Line
	// Column
	// Erase line
	// Attributes
	smg_status = smg$put_chars(scope.smg_option, op_prompt + " <alpha>:", 1, 1, 1, 0, 0);
	y1pos = op_prompt.size() + 11;
	//
	// Handle RSET string
	//
	if (op_flag & 2)
	{
		Lset(gets, boost::trim_left_copy(gets));
	}
	//
l2000:;
	// Normal entry
	//
	Temp = -1;
	temp = entr_3enter(scope, scope.smg_option, 1, y1pos, gets,
		Temp, op_flag);
	//
	// Handle RSET string
	//
	if (op_flag & 2)
	{
		Rset(gets, boost::trim_right_copy(gets));
	}
	//
l3000:;
	// Abort out on exits
	//
	// ** Converted from a select statement **
	if ((scope.scope_exit == SMG$K_TRM_F10) ||
		(scope.scope_exit == SMG$K_TRM_CTRLC) ||
		(scope.scope_exit == SMG$K_TRM_TIMEOUT) ||
		(scope.scope_exit == SMG$K_TRM_CTRLZ))
	{
		//
		// Let's make it easier on other programs
		// by forcing to s apecific return code.
		//
		scope.scope_exit = SMG$K_TRM_F10;
		goto l3200;
	}
	else if (scope.scope_exit == SMG$K_TRM_F14)
	{
		//
		// Attempt to enter through choice menu
		//
		draw_c = basic::edit(op_drawcols, 2 + 4);
		x = entr_3choice(scope, "", "", op_vtext, gets, 138, op_thetitle, draw_c, 0);
		if (x < 0)
		{
			goto l2000;
		}
		if (op_flag & 2048)
		{
			//
			// Remove leading spaces
			//
			gets = boost::trim_left_copy(op_vtext[x].substr(0, op_xstart.size()));
		}
		else
		{
			gets = op_vtext[x].substr(0, op_xstart.size());
		}
		goto l3000;
	}
	else if (scope.scope_exit == SMG$K_TRM_UP)
	{
		if (op_flag & 16384)
		{
			goto l3200;
		}
	}
	//
	// Determine test segement length
	//
	test_seg = std::stol(op_drawcols.substr(0, 3)) - 3;
	if (test_seg <= 0 || gets.size() < test_seg)
	{
		test_seg = gets.size();
	}
	//
	// Check against tables
	//
	tt = std::stol(op_vtext[0]);
	//
	// If table element count is set then loop otherwise while next
	//
	if (tt)
	{
		for (i = 1; i <= tt; i++)
		{
			if (op_flag & 2048)
			{
				if (boost::trim_copy(op_vtext[i]).substr(0, test_seg) == boost::trim_right_copy(gets))
				{
					goto l3200;
				}
			}
			else
			{
				if (boost::trim_right_copy(op_vtext[i].substr(0, test_seg)) == boost::trim_right_copy(gets))
				{
					goto l3200;
				}
			}
		}
	}
	else
	{
		i = 1;
		while (op_vtext[i] != "")
		{
			if (op_flag & 2048)
			{
				if (boost::trim_copy(op_vtext[i]).substr(0, test_seg) == boost::trim_right_copy(gets))
				{
					goto l3200;
				}
			}
			else
			{
				if (boost::trim_right_copy(op_vtext[i].substr(0, test_seg)) == boost::trim_right_copy(gets))
				{
					goto l3200;
				}
			}
			i = i + 1;
		}
	}
	help_34message(scope, "invalid choice", "W", "ENTR_3STRINGLIST",
		"", "INVCHOICE");
	// ++
	// Warning:INVCHOICE
	//	^*Invalid Choice\*
	//	.b
	//	.lm +5
	//	^*Explanation\*
	//	.b
	//	An invalid input has been entered.
	//	.b
	//	^*User Action\*
	//	.b
	//	Press the ^*<List Choices>\* key or refer to the documentation
	//	manual for valid entries, and re-enter the input.
	//	.lm -5
	//
	// Index:
	//
	// --
	// SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
	goto l2000;
	//
l3200:;
	// Pad GETS$ with spaces if necessary
	//
	gets = (gets +
		std::string(op_xstart.size(), ' ')).substr(0, op_xstart.size());
	//
	// Re-display data on screen
	//
	if ((op_cpos != "") && ((op_flag & 64) == 0))
	{
		// Window
		// Message to display
		// Line
		// Column
		// Erase screen
		// Attributes
		smg_status = smg$put_chars(xx_vdid, basic::Format(gets,
			op_xformat), xpos, ypos, 0, SMG$M_BOLD, 0);
	}
	//
	// Return value
	//
	Result = gets;
	//
	// Return value in OP_XDEFLT if supposed to
	//
	if (op_flag & 128)
	{
		op_xdeflt = gets;
	}
	return Result;
}
