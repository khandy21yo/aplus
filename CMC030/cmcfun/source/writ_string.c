/*
 * %TITLE "Write Out a Terminal Control String"
 * %SBTTL "WRIT_STRING"
 * %IDENT "V3.6 Calico"
 *
 *		COPYRIGHT (C) 1985 BY
 *		Computer Management Center, Idaho Falls, Idaho.
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies thereof may not be provided or otherwise made
 * available to any other person.  No title to and ownership of
 * the software is hereby transferred.
 *
 * The information in this software is subject to change without
 * notice and should not be construed as a commitment by
 * Computer Management Center, Inc.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
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
 * Index:
 *
 *	.x Write>String
 *	.x String>Write
 *	.x Device>Write
 *	.x Write>Device
 *	.x Control>String
 *	.x String>Control
 *	.x Write>Control
 *	.x Control>Write
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:WRIT_STRING
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP WRIT_STRING
 *	$ DELETE WRIT_STRING.OBJ;*
 *
 * AUTHOR:
 *
 *	12/13/85 - Kevin Handy
 *
 * MODIFICATION HISTORY:
 *
 *	01/03/85 - Cal Rasmussen
 *		Allow either "/" or "\" in control string
 *
 *	04/12/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	10/27/95 - Kevin Handy
 *		Conversion to C.
 *
 *	10/31/95 - Kevin Handy
 *		Trim source string.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(str$routines.h)
 *--
 */

/*
 * Include files
 */
#include <stdlib.h>
#include <string.h>
#include <descrip.h>
#include <str$routines.h>

/*
 * Main Function
 */
void writ_string(struct dsc$descriptor *Source, struct dsc$descriptor *Result)
{
	char* BuildString;
	long BuildLength = 0;
	int SourceLoop;
	int Character;
	int SourceLength = Source->dsc$w_length;

	/*
	 * Allocate a big enough buffer to handle string
	 */
	BuildString = malloc(Source->dsc$w_length + 2);

	/*
	 * Trim source string
	 */
	while ((SourceLength > 0) &&
		((char*)Source->dsc$a_pointer)[SourceLength - 1] == ' ')
	{
		SourceLength--;
	}

	/*
	 * Scan through source string
	 */
	for (SourceLoop = 0; SourceLoop < SourceLength; SourceLoop++)
	{
		switch(Character = ((char*)Source->dsc$a_pointer)[SourceLoop])
		{
		case '\\':
		case '/':
			/*
			 * Process an encoded character
			 */
			Character =
				(((char*)Source->dsc$a_pointer)
					[SourceLoop + 1] - '0') * 100 +
				(((char*)Source->dsc$a_pointer)
					[SourceLoop + 2] - '0') * 10 +
				(((char*)Source->dsc$a_pointer)
					[SourceLoop + 3] - '0');

			BuildString[BuildLength++] = Character;
			SourceLoop += 3;
			break;

		default:
			/*
			 * Process a normal character
			 */
			BuildString[BuildLength++] = Character;
			break;
		}
	}

	/*
	 * Null terminate it
	 * (Not necessary to do this at this time)
	 */
#if 0
	BuildString[BuildLength] = '\0';
#endif

	/*
	 * Return string
	 */
	str$copy_r(Result, &BuildLength, BuildString);

	free(BuildString);
}
