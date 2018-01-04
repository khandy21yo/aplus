//! \file
///*
 * %TITLE "Write Out a Terminal Control String"
 * %SBTTL "WRIT_STRING"
 * %IDENT "V3.6 Calico"
 */

 /*
 * Abstract:HELP
 *	.b
 *	This function is used to format a device comtrol string
 *	to prepare to write it out to a device.
 *	.lm -5
 *
 * Parameters:
 *
 *	TARGET$
 *		The passed string the user wants written out in terminal
 *		control.
 *
 *	RESULT$
 *		The returned result of the string written out in a terminal
 *		control string.
 *
 * Example:
 *
 *	CALL WRIT_STRING(SEQ$,RESULT$)
 *
 * AUTHOR:
 *
 *	12/13/85 - Kevin Handy
 *
 */

/*
 * Include files
 */
#include <iostream>
#include <string>

#include "preferences.h"
#include "cmcfun.h"

//!
//! \brief Write string
//
void writ_string(
	const std::string &Source,
	std::string &Result)
{
	int SourceLoop;
	int Character;
	int SourceLength = Source.size();

	/*
	 * Allocate a big enough buffer to handle string
	 */
	Result = "";

	/*
	 * Trim source string
	 */
	while ((SourceLength > 0) &&
		Source[SourceLength - 1] == ' ')
	{
		SourceLength--;
	}

	/*
	 * Scan through source string
	 */
	for (SourceLoop = 0; SourceLoop < SourceLength; SourceLoop++)
	{
		switch(Character = Source[SourceLoop])
		{
		case '\\':
		case '/':
			/*
			 * Process an encoded character
			 */
			Character =
				(Source[SourceLoop + 1] - '0') * 100 +
				(Source[SourceLoop + 2] - '0') * 10 +
				(Source[SourceLoop + 3] - '0');

			Result += char(Character);
			SourceLoop += 3;
			break;

		default:
			/*
			 * Process a normal character
			 */
			BuildString[BuildLength++] = char(Character);
			break;
		}
	}
}
