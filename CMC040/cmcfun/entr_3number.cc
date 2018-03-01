//! \file
//! \brief Function to Enter a Real Number
// %SBTTL "ENTR_3NUMBER"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/entr_3number.bas
// Translated from Basic to C++ using btran
// on Tuesday, January 09, 2018 at 12:49:27
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <cmath>
#include "basicfun.h"
#include "pusing.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"

//!
//! Abstract:HELP
//!	.b
//!	.lm +5
//!	This function will enter numbers.
//!	.lm -5
//!
//! \returns Enters a formatted number on the screen.
//!
//! \author 06/24/85 - Kevin Handy
//!
double entr_3number(
	scope_struct &scope,
		//!< Program wide structure
	smg_display_id &xx_vdid,
		//!< Passed variable that creates or deletes the
		//!< window that holds the string.
	const std::string &op_cpos,
		//!< '' - Do not display on top area
	const std::string &op_prompt,
		//!< The passed string used for the prompt and its initialization
	double xdeflt,
		//!< One of the passed defaults for the number.
	long op_flag,
		//!<  1 - Don't enter data (display only?)
		//!<  4 - Force keypunch input(no <CR> after input)
		//!<  8 - Indicates a timeout on input will occur
		//!< 32 - Use default value
		//!< 64 - Don't display
		//!< 128 - Return fincal value in default
	const std::string &op_xformat,
		//!< The passed format for the number.
	std::string &op_deflt)
		//!< The default form of the number size.
{
	double gets;
	std::string gets_V2;
	std::string gets1;
	long i;
	long junk;
	long smg_status;
	long temp;
	std::string test_format;
	long xlen2;
	long y1pos;
	long zz;

	long xpos;
	long ypos;
	//
	// Split out cursor positioning function
	//
	dspl_splitcursor(op_cpos, xpos, ypos);
	//
	// Function to replace letters with other letters
	//

	auto fnrep = [&](std::string orgin, std::string lokfor, std::string repwit)
	{
l100:;
		i = (orgin.find(lokfor, 0) + 1);
		if (i)
		{
			orgin = orgin.substr(0, i - 1) + repwit + basic::right(orgin, i + 1);
			goto l100;
		}
		return orgin;
	};
	//
	// Select value to use
	//
	if (op_flag & 32)
	{
		try
		{
			gets = std::stod(op_deflt);
		}
		catch(...)
		{
			gets = 0.0;
			goto illnum;
		}
	}
	else
	{
		gets = xdeflt;
	}
	//
	// Handle if print only flag set
	//
L_130:;
	if (op_flag & 1)
	{
		goto l3000;
	}
	//
	// Set up a format string to enter a number. Allow maximum
	// display to be 999999999.<xlen>
	//
	junk = op_xformat.find(".", 0);
	xlen2 = 0;
	if (junk)
	{
		for (i = junk; i < op_xformat.size(); i++)
		{
			if (op_xformat[i] == '#')
			{
				xlen2++;
			}
		}
	}
	//
	// Display original value
	//
	if ((op_cpos != "") && ((op_flag & 64) == 0))
	{
		smg_status = smg$put_chars(xx_vdid,
			basic::Format(gets, op_xformat), xpos, ypos, 0,
			SMG$M_REVERSE);
	}
	//
	// Initilize default value
	//
	gets1 = std::to_string(long(func_round(gets * pow(10.0, xlen2), 0)));
	gets1.resize(op_xformat.size(), ' ');
	//
L_1000:;
	// Initilization/prompt
	//
	smg_status = smg$erase_display(scope.smg_option);
	smg_status = smg$put_chars(scope.smg_option, op_prompt + " <value>:", 1, 1, 1);
	y1pos = op_prompt.size() + 11;
	//
	// Normal entry
	//
	gets_V2 = gets1;
	zz = -1;
	temp = entr_3enter(scope, scope.smg_option, 1, y1pos, gets_V2, zz, op_flag);
	switch (scope.scope_exit)
	{
	// Cntrl C, PF1, Exit
	case 3:
	case 256:
	case 290:

		gets = xdeflt;
		goto l3000;
		break;

	}
	//
	// Clean up input string as necessary
	//
	// Remove garbage
	gets_V2 = basic::edit(gets_V2, -1);
	// Strip comma's
	gets_V2 = fnrep(gets_V2, ",", "");
	// Strip dollars
	gets_V2 = fnrep(gets_V2, "$", "");
	// Change O's to 0's
	gets_V2 = fnrep(gets_V2, "O", "0");
	// Change l's to 1's
	gets_V2 = fnrep(gets_V2, "L", "1");
	//
	// Take value entered
	//
	try
	{
		gets = std::stod(gets_V2);
	}
	catch(...)
	{
		goto illnum;
	}
	if (!((gets_V2.find(".", 0) + 1)))
	{
		gets = gets / (pow(10.0, xlen2));
	}
	gets = func_round(gets, xlen2);
	//
	// Test for range
	//
	test_format = basic::Format(gets, op_xformat);
	if (basic::edit(test_format, -1).substr(0, 1) == "%")
	{
		help_34message(scope, "number out of range", "W", "ENTR_3NUMBER", "", "ILLNUMBER");
		gets1 = gets_V2;
		gets1.resize(op_xformat.size(), ' ');
		goto L_1000;
	}
	//
l3000:;
	// Exit function
	//
	if ((op_cpos != "") && ((op_flag & 64) == 0))
	{
		smg_status = smg$put_chars(xx_vdid, basic::Format(gets, op_xformat), xpos, ypos, 0, SMG$M_BOLD);
	}
	//
	// Return value in OP_DEFLT if supposed to
	//
	if (op_flag & 128)
	{
		op_deflt = basic::Format(gets, op_xformat);
	}
	//
	// Erase message if there are any
	//
	smg_status = smg$erase_display(scope.smg_message);
	return gets;

illnum:;
	help_34message(scope, "illegal number format", "W", "ENTR_3NUMBER", "", "ILLNUMBER");
	// ++
	// Warning:ILLNUMBER
	//	^*Illegal Number\*
	//	.b
	//	.lm +5
	//	^*Explanation\*
	//	.b
	//	The number has an invalid format or out of range value.
	//	.b
	//	^*User Action\*
	//	.b
	//	Check the format or help message for the range, and
	//	re-enter the value.
	//	.lm -5
	//
	// Index:
	//	.x Number
	//
	// --
	gets1 = gets_V2;
	gets1.resize(op_xformat.size(), ' ');
	goto L_1000;
	//*******************************************************************
L_18900:;
	// Display error message
	//*******************************************************************
	help_34message(scope, std::to_string(0) + " " + basic::ert(0), "E", "ENTR_3NUMBER", "", std::to_string(0));
	return xdeflt;
}
