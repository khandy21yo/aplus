/*	%TITLE "Number Incrementing Function"
 */
#pragma module func_increment "V3.6 Calico"

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
 * notice and should not be construed as a committment by
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
 *	1 to this set of numbers, and replace it in the
 *	string.  This will be used in programs where a
 *	number needs to be incremented -- invoice numbers,
 *	WIP numbers, transaction numbers, etc.
 *
 * Parameters:
 *
 *	the_number
 *		When the function is first called, the_number
 *		contains the value of the number needing to
 *		be incremented.  When control is returned to
 *		the main program, the_number (or rather, the
 *		variable used in the function call) will
 *		contain the value of the number after it has
 *		been incremented.
 *
 *	Returned value
 *		The end status of the function (needed because
 *		although there doesn't APPEAR to be any
 *		possibility of an error...).
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:FUNC_INCREMENT/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP FUNC_INCREMENT
 *	$ DELETE FUNC_INCREMENT.OBJ;*
 *
 * Author:
 *
 *	07/27/88 - Aaron Redd
 *
 * Modification history:
 *
 *	08/23/90 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, smg$routines.h)
 *--
 */

#include <ctype.h>
#include <descrip.h>

long func_increment(struct dsc$descriptor_s *the_number)
{
	int itm, flag=0;
	char *ptr;

	ptr = the_number->dsc$a_pointer;

	/*
	 * Plan loop to scan through entire string (but we will
	 * probibly finish early)
	 */
	for (itm = the_number->dsc$w_length-1; itm >= 0; itm--)
	{
		/*
		 * If this is a digit, lets add one to it
		 */
		if (isdigit(ptr[itm]))
		{
			/*
			 * Check for carry
			 */
			if (ptr[itm] == '9')
			{
				/*
				 * Carry. This digit goes to 0, keep looping.
				 */
				ptr[itm] = '0';
			}
			else
			{
				/*
				 * No carry, we are done. Be happy.
				 */
				ptr[itm]++;
				return(-1);
			}
			flag = -1;
		}
		else
		{
			if ((flag != 0) && (ptr[itm] == ' '))
			{
				ptr[itm] = '1';
				return(-1);
			}
		}
	}

	/*
	 * Unable to increment number, lets try shifting it over
	 * one character if possible.
	 */
	if ((flag != 0) && (ptr[the_number->dsc$w_length-1] == ' '))
	{
		for (itm = the_number->dsc$w_length-1; itm >= 1; itm--)
		{
			ptr[itm] = ptr[itm-1];
		}
		ptr[0] = '1';
		return(-1);
	}

	return(0);	/* Didn't finish incrementing */
}
