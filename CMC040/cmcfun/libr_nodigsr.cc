//! \file
//! \brief No Digital Standard Runoff for Text Array
// %SBTTL "LIBR_NODIGSR"
// %IDENT "V3.3"
//
// Source: ../CMC030/cmcfun/source/libr_nodigsr.bas_old
// Translated from Basic to C++ using btran
// on Monday, February 19, 2018 at 15:06:18
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "smg/lbr.h"



//!
//! Abstract:HELP
//!	.p
//!	This subroutine takes the filename and given array and
//!	opens the file and writes the file to the array without
//!	any conversions.
//!
//! Parameters:
//!
//!	LIB_NAME$
//!		The passed name of the library to pull the text from.
//!
//!	KEY_NAME$
//!		The passed name of the key to use to select the
//!		right text.
//!
//!	CODE$(0%)
//!		Contains the number of lines already in
//!		use in the CODE$() array.
//!
//!	CODE$()
//!		The text returned back in PRNT_ENCODED format.
//!	CODE$(0%) is modified to point to the last line.
//!
//!	Returns a status code.
//!
//! Example:
//!
//!	TEXT$(0%) = "0"
//!	ST% = LIBR_NODIGSR("HELP_GL", "ADDRESS", TEXT$())
//!
//! Author:
//!
//!	02/20/87 - B. Craig Larsen
//
long libr_nodigsr(
	const std::string &lib_name,
	const std::string &key_name,
	std::vector<std::string> &code)
{
	long Result;
	long curr_line;
	lbr_index_cdd lr_index;
	long st;
	std::string text;

	long txrfa;
	//
	// Assume success
	//
	Result = 1;
	//
	// Initilization
	//
	curr_line = 0;
	//
	// Set up the control structure if necessary
	//
	st = lbr$ini_control(lr_index, LBR$C_READ);
	if ((st & 1) == 0)
	{
		curr_line = 1;
		code[curr_line] =
			std::string("Unable to initilize library ") +
			std::to_string(st);
		Result = st;
		goto exitprogram;
	}
	st = lbr$open(lr_index, lib_name, 0, ".TLB");
	if ((st & 1) == 0)
	{
		Result = st;
		goto exitprogram;
	}
	st = lbr$lookup_key(lr_index, key_name, txrfa);
	if ((st & 1) == 0)
	{
		Result = st;
		goto exitprogram;
	}
	//
	// Load text into the array
	//
	for (curr_line = 1; curr_line <= 6000; curr_line++)
	{
		text = std::string(150, ' ');
		st = lbr$get_record(lr_index, text);
		if ((st & 1) == 0)
		{
			goto exitprogram;
		}
		code[curr_line] = boost::trim_right_copy(text);
	}
exitprogram:;
	code[0] = std::to_string(curr_line - 1);
	//
	// TEXT file(close)
	//
	st = lbr$close(lr_index);
	return Result;
}
