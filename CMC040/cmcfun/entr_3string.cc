//! \file
//! \brief Function to Enter a String
// %SBTTL "ENTR_3STRING"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/entr_3string.bas
// Translated from Basic to C++ using btran
// on Tuesday, January 16, 2018 at 20:17:28
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
//!	Enters/Displays a string on the screen. This function
//!	uses the ENTRY function so all of it's editing features
//!	are available to the user.
//!	.lm -5
//!
//! Parameter:
//!
//!	XX_VDID
//!		The virtual display ID number.
//!
//!	POS$
//!		The  passed final position of the data on screen.
//!		Format as 'ROW;COL'.  If POS$='' (blank), data will
//!		be entered but not displayed above on screen.
//!
//!	OP_PROMPT
//!		Passed prompt string. (Will be followed by
//!		'ALPHA:', i.e. 'ADD' will generate 'ADD ALPHA:')
//!
//!	OP_FLAG
//!		An integer flag word.
//!		.table
//!			1 - Don't enter data (display only?)
//!			2 - Rset string instead of lset
//!			4 - Force keypunch input(no <CR> after input)
//!			8 - Indicates a timeout on input will occur
//!			16 - Convert from lowercase to uppercase
//!			32 - Use default value
//!			64 - Don't display
//!			128 - Return final value in default
//!		.endtable
//!
//!	FORMAT$
//!		A passed BASIC+2 print-using format for string.
//!		It may also start with ~ to indicate a CMC
//!		format as follows:
//!		.table
//!			"~abx"
//!		where	~	indicates CMC format
//!			a	L or R for Pad to the left or right
//!			b	the pad character
//!			x	the normal BASIC print-using format
//!					as required.
//!		.endtable
//!
//!		Example:  "~LZ'E" means pad on the left side with
//!			the letter Z and use "'E" for the print-using.
//!
//!	DEFAULT$
//!		Returned default data value to use if <CR> is typed.
//!
//!
//!	Returns the string entered.
//!
//!	Returns DEFAULT$ if bit one is set in OP_FLAG.
//!
//! Example:
//!
//!	CN$ = ENTR_3STRING(SMG_SCREEN_DATA%, '3;19',"Customer Number",
//!		CUSTOM.NUMBER$, OP_FLAG, "'E")
//!
//! Author:
//!
//!	06/24/85 - Kevin Handy
//!
//! --
std::string entr_3string(
	scope_struct &scope,
	smg_display_id &xx_vdid,
	const std::string &op_cpos,
	const std::string &op_prompt,
	const std::string &op_xstart,
	long op_flag,
	const std::string &op_xformat,
	std::string &op_deflt,
	long length)
		//!< Length of returned string
{
	std::string Result;
	std::string gets;
	long pad_c;
	long smg_status;
	long temp;
	std::string t_format;
	long xpos;
	long y1pos;
	long ypos;
	std::string zpp;
	long junk;

	//
	// Decide between default value and normal value
	//
	if (op_flag & 32)
	{
		gets = std::string(op_xstart.size(), ' ');
		Lset(gets, op_deflt);
	}
	else
	{
		gets = op_xstart;
	}
	if (op_xformat.substr(0, 1) == "~")
	{
		t_format = basic::right(op_xformat, 4);
	}
	else
	{
		t_format = op_xformat;
	}
	//
	// If display only
	//
	if (op_flag & 1)
	{
		goto l3000;
	}
	//
	// Handle RSET string
	//
	if (op_flag & 2)
	{
		Lset(gets, boost::trim_left_copy(gets));
	}
	//
	// Initial display of item in reverse video
	//
	if ((op_cpos != "") && ((op_flag & 64) == 0))
	{
		smg_status = smg$put_chars(xx_vdid,
			basic::Format(gets, t_format), xpos, ypos, 0,
			SMG$M_REVERSE);
	}
	//
	// Initilization/prompt
	//
	smg_status = smg$erase_display(scope.smg_option);
	smg_status = smg$put_chars(scope.smg_option, op_prompt + " <alpha>:", 1, 1, 1, 0);
	y1pos = op_prompt.size() + 11;
	//
	// Normal entry
	//
	junk = -1;
	temp = entr_3enter(scope, scope.smg_option, 1, y1pos, gets, junk, op_flag);
	//
	// Handle RSET string
	//
	if (op_flag & 2)
	{
		Rset(gets, boost::trim_right_copy(gets));
	}
	//
l3000:;
	// Exit function
	//
	// Lower to uppercase
	if (op_flag & 16)
	{
		gets = boost::to_upper_copy(gets);
	}
	//
	// Translate FORMAT$
	//
	if (op_xformat.substr(0, 1) == "~")
	{
		//
		// Pad Character
		//
		pad_c = basic::mid(op_xformat, 3, 1)[0];
		//
		// Pad Side
		//
		// ** Converted from a select statement **
		if (basic::mid(op_xformat, 2, 1) == "L")
		{
			zpp = basic::Qstring(gets.size(), pad_c) + boost::trim_copy(gets);
			gets = basic::right(zpp, zpp.size() - gets.size() + 1);
		}
		else if (basic::mid(op_xformat, 2, 1) == "R")
		{
			gets = (boost::trim_copy(gets) + basic::Qstring(gets.size(), pad_c)).substr(0, gets.size());
		}
	}
	//
	// Re-display data on screen
	//
	if ((op_cpos != "") && ((op_flag & 64) == 0))
	{
		smg_status = smg$put_chars(xx_vdid, basic::Format(gets, t_format), xpos, ypos, 0, SMG$M_BOLD);
	}
	//
	// Handle RSET string
	//
	if (op_flag & 2)
	{
		Rset(gets, boost::trim_right_copy(gets));
	}
	//
	// Return value
	//
	Result = gets;
	//
	// Return in default (string format) if flag is set
	//
	if (op_flag & 128)
	{
		op_deflt = gets;
	}
	return Result;
}
