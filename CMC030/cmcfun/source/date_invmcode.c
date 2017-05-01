/*	%TITLE "Aging Function (Month) to String"
 */
#pragma module date_invmcode "V3.6 Calico"

/*
 *
 * COPYRIGHT (C) 1988 BY
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
 *	Assumes 8 character date, YYYYMM(DD).
 *	This function is used to reverse the DATE_MONCODE
 *	calculation, which is used to calculate the number
 *	of months between two months.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	monthcode
 *		The passed date in the amount of months.
 *
 *
 *	Returned value
 *		The function changes monthcode% into a date in the form
 *		of YYYYMMDD.
 *
 * Example:
 *
 *	DATE_INVMCODE(19760314)
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:DATE_INVMCODE
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP DATE_INVMCODE
 *	$ DELETE DATE_INVMCODE.OBJ;*
 *
 * Author:
 *
 *	06/23/86 - Kevin Handy
 *
 * Modification history:
 *
 *	07/09/93 - Kevin Handy
 *		Convert to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/26/99 - Kevin Handy
 *		Modified to compile cleanly with DEC-C
 *		(module, stdio.h, str$routines.h, sprintf)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <descrip.h>
#include <str$routines.h>

/*
 * Main Function
 */
void date_invmcode(struct dsc$descriptor *returnstr, long *monthcode)
{
	/*
	 * Local Variables
	 */
	char makestring[10];

	/*
	 * Format String
	 */
	sprintf(makestring, "%04ld%02ld",
		*monthcode / 12,
		*monthcode % 12 + 1);

	/*
	 * Return String
	 */
	str$copy_r(returnstr, &6l, &makestring);
}
