/*	%TITLE "Format Social Security Number"
 */
#pragma module prnt_ssn "V3.6 Calico"

/*
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
 *	.lm +5
 *	This function formats a social security number.
 *	The flag is not used at this time.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	SSN$
 *		This passed string is the number the user wants to have
 *		fomatted.
 *
 *	Returned value
 *		This function outputs the social security number in its
 *		new format.
 *
 * Example:
 *
 *	SOC$ =  PRNT_SSN("518764347", FLAG%)
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:PRNT_SSN
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP PRNT_SSN
 *	$ DELETE PRNT_SSN.OBJ;*
 *
 * AUTHOR:
 *
 *	04/13/88 - Robert Peterson
 *
 * MODIFICATION HISTORY:
 *
 *	07/19/93 - Kevin Handy
 *		Converted to C.
 *
 *	07/20/93 - Kevin Handy
 *		Added variable "length" so didn't pass address of
 *		a constant, wich Gnu CC doesn't like even though
 *		VAX C does.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, stdio.h, str$routines.h, sprintf)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <descrip.h>
#include <string.h>
#include <str$routines.h>

/*
 * Main Function
 */
void prnt_ssn(struct dsc$descriptor *returnstr,
	struct dsc$descriptor *ssn,
	long *flag)
{
	/*
	 * Local variables
	 */
	char outtext[12];
	int i;
	int fnd=0;
	long length;

	/*
	 * Look for formatting already
	 */
	for (i = 0; i < ssn->dsc$w_length; i++)
	{
		if (*(ssn->dsc$a_pointer+i) == '-')
		{
			fnd = -1;
			break;
		}
	}

	/*
	 * Reformat as necessary
	 */
	if ((fnd != 0) || (ssn->dsc$w_length == 0))
	{
		/*
		 * Pass through unchanged
		 */
		str$copy_dx(returnstr, ssn);
	}
	else
	{
		/*
		 * Throw in several dashes
		 */
		sprintf(outtext, "%3.3s-%2.2s-%4.4s",
			ssn->dsc$a_pointer,
			ssn->dsc$a_pointer+3,
			ssn->dsc$a_pointer+5);
		/*
		 * Return formatted string
		 */
		length = 11;
		str$copy_r(returnstr, &length, &outtext);
	}
}
