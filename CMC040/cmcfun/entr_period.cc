//! \file
//!  \brief Function to Enter a Period
// %SBTTL "ENTR_PERIOD"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/entr_period.bas
// Translated from Basic to C++ using btran
// on Wednesday, January 17, 2018 at 16:36:34
//

#include <string>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "database.h"


extern scope_struct scope;


//!
//! Abstract:HELP
//!	.b
//!	.lm +5
//!	This function enters a period
//!	CPOS$
//!	'' - Do not display on top area
//!	FLAG%
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
//! Input:
//!
//!	XX_VDID%
//!		The passed variable that creates or deletes the
//!		window that holds the string.
//!
//!	PROMPT$
//!		The passed string used for the prompt and its initialization
//!
//!	XDFLT$
//!		One of the passed defaults for the period.
//!
//!	XFORMAT$
//!		The string used to examine the format for the
//!		period.
//!
//!	DEFLT$
//!		The default form of the period size.
//!
//!
//!	This function enters a period on the screen.
//!
//! AUTHOR:
//!
//!	12/18/85 - Frank F. Starman
//!
std::string entr_period(
	smg_display_id &xx_vdid,
	const std::string &cpos,
	const std::string &prompt,
	const std::string &xdflt,
	long flag,
	const std::string &xformat,
	std::string &deflt)
{
	std::string Result;
	std::string gets;
	long smg_status;
	std::string stat;
	long x;
	long y;
	long y1pos;
	long Temp;
	long Temp2;
	std::string STemp;

	long xpos;
	long ypos;
	//
	// Seperate cursor position
	//
	dspl_splitcursor(cpos, x, y);
	xpos = x;
	ypos = y;
	//
	// Choose the default to use
	//
	if ((flag & 32) == 0)
	{
		gets = xdflt;
	}
	else
	{
		gets = deflt + std::string(xdflt.size() - deflt.size(), ' ');
	}
	//
	// Display only?
	//
	if ((flag & 1) != 0)
	{
		goto l3000;
	}
	//
	// Display original
	//
	if ((cpos != "") && ((flag & 64) == 0))
	{
		smg_status = smg$put_chars(xx_vdid, gets, xpos, ypos, 0, SMG$M_REVERSE);
	}
	scope.scope_exit = 0;
	//
	// Initialization/prompt
	//
	smg_status = smg$erase_display(scope.smg_option);
	smg_status = smg$put_chars(scope.smg_option, prompt + "<YYYYPP>: ", 1, 1, 1, 0);
	y1pos = prompt.size() + 10;
l1110:;
	// ** Converted from a select statement **
	//
	// Pick out all of the exit keys
	//
	Temp2 = -1;
	Temp = entr_3enter(scope, scope.smg_option, 1, y1pos, gets, Temp2, flag);
	if ((Temp == SMG$K_TRM_CTRLC) ||
		(Temp== SMG$K_TRM_F8) ||
		(Temp == SMG$K_TRM_F10) ||
		(Temp == SMG$K_TRM_CTRLZ))
	{
		goto l3000;
	}
	gets = boost::erase_all_copy(gets, " ");
	switch (gets.size())
	{
	case 0:

		gets = std::string(xdflt.size(), ' ');
		goto l3000;
		// Period given in YYYYPP format
		break;

	case 6:

		// (Don't need to do anything)
		break;

	default:
		entr_3message(scope, "Invalid period: use YYYYPP format", 0);
		gets = gets + std::string(xdflt.size() - gets.size(), ' ');
		goto l1110;
		break;

	}
	//
	// Examine period
	//
	if (read_period("READ", xformat, gets, STemp, stat, STemp, STemp, Temp2))
	{
		goto L_3500;
	}
	//
l3000:;
	// Display result
	//
	if ((cpos != "") && ((flag & 64) == 0))
	{
		smg_status = smg$put_chars(xx_vdid, gets, xpos, ypos, 0, SMG$M_BOLD);
	}
	Result = gets;
	//
	// Return text in default if supposed to
	//
	if (flag & 128)
	{
		deflt = gets;
	}
	return Result;
L_3500:;
	entr_3message(scope, "Undefined period!", 0);
	goto l1110;
	return Result;
}
