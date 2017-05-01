/*	%TITLE "Print a Phone Number as (Xxx)Xxx-xxxx"
 */
#pragma module prnt_phone "V3.6 Calico"

/*
 * COPYRIGHT (C) 1987 BY
 *
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
 *	.b
 *	.lm +5
 *	This function formats a phone number.
 *	It expects the phone number to be in 1234567890 (10 char.)
 *	string form.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	PHONE$
 *		The passed phone number the user wants formatted.
 *
 *	FLAG%
 *		Flag passed that tells the function the way the user
 *		wants the phone formatted.
 *
 *	.table
 *		FLAG%		MEANING
 *		0%		(123)456-7890
 *		1%		123-456-7890
 *	.endtable
 *
 *	This function returns a phone number in a specified format.
 *
 * Example:
 *
 *	PHONE$ = PRNT_PHONE("2085226729", 0%)
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:PRNT_PHONE
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP PRNT_PHONE
 *	$ DELETE PRNT_PHONE.OBJ;*
 *
 * AUTHOR:
 *
 *	5/15/87 - B. Craig Larsen
 *
 * MODIFICATION HISTORY:
 *
 *	07/14/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
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
void prnt_phone(struct dsc$descriptor *returnstr,
	struct dsc$descriptor *phone,
	long *flag)
{
	/*
	 * Local variables
	 */
	char worktext[12];
	char outtext[12];
	char *useformat;
	int worklen;
	int fixlen;
	long length;

	/*
	 * Trim up the phone number
	 */
	strncpy(worktext, phone->dsc$a_pointer, phone->dsc$w_length);
	worklen = phone->dsc$w_length;
	while ((worklen > 0) & (worktext[worklen-1] == ' '))
	{
		worklen--;
	}

	/*
	 * Make it exactly 10 characters long (right set it to 10)
	 */
	if (worklen != 10)
	{
		for (fixlen = 9; fixlen >= (10 - worklen); fixlen--)
		{
			worktext[fixlen] = worktext[fixlen - (10 - worklen)];
		}
		for (fixlen = 0; fixlen < (10-worklen); fixlen++)
		{
			worktext[fixlen] = ' ';
		}
	}


	/*
	 * Decide which format to use to print phone number
	 */
	if ((*flag  & 1) != 0)
	{
		useformat = "%3.3s-%3.3s-%4.4s";
	}
	else
	{
		useformat = "(%3.3s)%3.3s-%4.4s";
	}

	/*
	 * Create output string
	 */
	sprintf(outtext, useformat, worktext, worktext+3, worktext+6);

	/*
	 * Return string
	 */
	length = strlen(outtext);
	str$copy_r(returnstr, &length, &outtext);
}
