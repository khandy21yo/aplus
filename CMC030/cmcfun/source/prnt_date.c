/*	%TITLE "Format Date into MM/DD/YY or MM/DD/YYYY Format"
 */
#pragma module prnt_date "V3.6 Calico"

/*
 *	COPYRIGHT (C) 1985 BY
 *	Computer Management Center, Inc.
 *	Idaho Falls, Idaho.
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
 *	This function formats a date into MM/DD/YYYY format if
 *	INDATE% is 8% and into MM/DD/YY format otherwise.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	INDATE$
 *		The passed date that is to be formatted.
 *	INDATE%
 *		The passed integer that tells how the date should be
 *		formatted.
 *
 *	This function formats a date into either MM/DD/YYYY or
 *	MM/DD/YY, depending on the users choice.
 *
 * Example:
 *
 *	PRNT_DATE(AP_OPEN::INVDAT,8%)
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:PRNT_DATE
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP PRNT_DATE
 *	$ DELETE PRNT_DATE.OBJ;*
 *
 * Author:
 *
 *	08/02/85 - Kevin Handy
 *
 * Modification history:
 *
 *	06/25/85 - Frank Starman
 *		Print blank date ("  /  /    ") if the input date
 *		is a null string.
 *
 *	07/08/93 - Kevin Handy
 *		Converted to C. No longer can modify original string.
 *
 *	08/05/93 - Kevin Handy
 *		Handle weird length inputs better (non-8 and non-6).
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, stdio.h, str$routines.h)
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
void prnt_date(struct dsc$descriptor *returnstr,
	struct dsc$descriptor *indate,
	long *outlen)
{
	int loop;
	long length;
	char worktext[12];
	char outtext[12];

	if ((indate->dsc$w_length == 8) || (indate->dsc$w_length == 0))
	{
		worktext[0] = '\0';
		length = 0;
	}
	else
	{
		strcpy(worktext, "19");
		length = 2;
	}
	strncat(worktext, indate->dsc$a_pointer, indate->dsc$w_length);
	length += indate->dsc$w_length;

	/*
	 * Don't allow null's in input string
	 */
	for (loop = 0; loop < length; loop++)
	{
		if (worktext[loop] == '\0')
		{
			worktext[loop] = ' ';
		}
	}

	/*
	 * Fill string out to eight bytes with spaces
	 */
	for (loop = length; loop < 8; loop++)
	{
		worktext[loop] = ' ';
	}

	/*
	 * Generated formatted date
	 */
	if (*outlen == 8)
	{
		sprintf(outtext, "%2.2s/%2.2s/%4.4s",
			worktext+4, worktext+6, worktext);
	}
	else
	{
		sprintf(outtext, "%2.2s/%2.2s/%2.2s",
			worktext+4, worktext+6, worktext+2);
	}

	/*
	 * Return string
	 */
	length = strlen(outtext);
	str$copy_r(returnstr, &length, &outtext);
}
