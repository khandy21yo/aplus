/*	%TITLE "Alphnumeric Add Function"
 */
#pragma module func_numberadd "V3.6 Calico"

/*
 *
 * COPYRIGHT (C) 1988 BY
 * Computer Management Center, Inc.
 * Idaho Falls, Idaho.
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
 *	.p
 *	This function will take the given string,
 *	search for a set of numeric characters in it, add
 *	the given amount to this set of numbers, and replace
 *	it in the string.  This will be used in programs where a
 *	number needs to be incremented/decremented -- invoice numbers,
 *	WIP numbers, transaction numbers, check numbers, etc.
 *
 * Parameters:
 *
 *	THE_NUMBER
 *		When the function is first called, THE_NUMBER
 *		contains the value of the number needing to
 *		be incremented.  When control is returned to
 *		the main program, THE_NUMBER (or rather, the
 *		variable used in the function call) will
 *		contain the value of the number after it has
 *		been incremented.
 *
 *	THE_AMOUNT
 *		This is the amount that will be added to
 *		THE_NUMBER.
 *		This can be negative
 *
 *	Returned value
 *		The end status of the function::
 *			-1% = success
 *			 0% = error no number in THE_NUMBER to add to
 *			 1% = error THE_NUMBER went negitive
 *				and was wrapped around.
 *				* THIS MAY CAUSE BAD PROBLEMS
 *				SO CHECK FOR IT IN CALLING PROGRAM.
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:FUNC_NUMBERADD
 *	$ LIB FUNC_LIB:CMCFUN/REP FUNC_NUMBERADD
 *	$ DELETE FUNC_NUMBERADD.OBJ;*
 *
 * Author:
 *
 *	12/15/88 - B. Craig Larsen
 *
 * Modification history:
 *
 *	08/13/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module)
 *--
 */

/*
 * Include files
 */
#include <descrip.h>
#include <ctype.h>

/*
 * Main Function
 */
long func_numberadd(struct dsc$descriptor *the_number,
	long *the_amount)
{
	/*
	 * Local variables
	 */
	long stats = 0;			/* Status */
	int StringIndex;		/* Pointer into string to be adjusted */
	long Increment;			/* Absolute value of amount to increment by */
	int NegSign = 0;		/* Sign of increment */
	int CharacterValue;		/* Value of character */

	/*
	 * Determine increment and sign
	 */
	Increment = *the_amount;
	if (Increment < 0)
	{
		Increment = -Increment;
		NegSign = !0;
	}

	/*
	 * Start at the end of the_number, and doctor digits
	 * heading back towards the front
	 */
	for (StringIndex = the_number->dsc$w_length - 1; StringIndex >= 0;
		StringIndex--)
	{
		/*
		 * Get character, and determine if it is a digit
		 */
		CharacterValue = the_number->dsc$a_pointer[StringIndex];

		if (isdigit(CharacterValue))
		{
			/*
			 * Convert to digit value
			 */
			CharacterValue -= '0';

			/*
			 * Add/subtract this digit
			 */
			if (NegSign)
			{
				CharacterValue -= Increment % 10;
				Increment = Increment / 10;
				if (CharacterValue < 0)
				{
					Increment++;
					CharacterValue += 10;
				}
			}
			else
			{
				CharacterValue += Increment % 10;
				Increment = Increment / 10;
				if (CharacterValue >= 10)
				{
					Increment++;
					CharacterValue -= 10;
				}
			}

			/*
			 * Convert number back to string, and stuff it back
			 */
			CharacterValue += '0';
			the_number->dsc$a_pointer[StringIndex] = CharacterValue;
			stats = -1;
		}
	}

	/*
	 * Flag an error if we have any digits left
	 */
	if ((Increment != 0) && (NegSign))
	{
		stats = 1;
	}

	return(stats);
}
