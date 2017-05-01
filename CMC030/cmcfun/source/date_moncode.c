/*
 *	%TITLE "Aging Function for Certain Number of Days"
 */
#pragma module date_moncode "V3.6 Calico"

/*
 *
 *		COPYRIGHT (C) 1985 BY
 *		Computer Management Center, Idaho Falls, Idaho.
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies therof may not be provided or otherwise made
 * available to any other person.  No title to and ownership of
 * the software is hereby transferred.
 *
 * The information in this software is subject to change without
 * notice and should not be construed as a committment by
 * Computer management Center.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
 * Abstract:HELP
 *	.p
 *	This function is used to calculate the number of months
 *	between two months.
 *
 *	Requires 8 character format date YYYYMMDD, but
 *	only uses the first six characters.  Will also accept
 *	a date in the six character format YYYYMM.
 *
 *	Use DATE_INVMCODE to reverse back to a string.
 *
 *
 * Parameters:
 *
 *	DATUM$
 *		The passed date in question in the form YYYYMMDD OR YYMMDD.
 *
 *	Returned value
 *		The number of days since 14-Sept-1752.
 *
 * Example:
 *
 *	BASE.MONTH = date_moncode("198712")
 *
 * Compile
 *
 *	$ cc FUNC_SOURCE:date_moncode/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP date_moncode
 *	$ DELETE date_moncode.OBJ;*
 *
 * Author:
 *
 *	06/23/86 - Kevin Handy
 *
 * Modification history:
 *
 *	08/16/90 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/26/99 - Kevin Handy
 *		Modified to compile clean with DEC-C
 *		(module)
 *--
 */

#include <descrip.h>

long date_moncode(struct dsc$descriptor_s *datum)
{
	long mon, yr, code;
	char *dta;

	dta = datum->dsc$a_pointer;

	/*
	 * Pull off month/day/year
	 */
	mon	= (*(dta+4) - '0') * 10 + (*(dta+5) - '0');
	yr	= (*(dta) - '0') * 1000 + (*(dta+1) - '0') * 100 +
		(*(dta+2) - '0') * 10 + (*(dta+3) - '0');

	code	= yr * 12 + mon - 1;

	return(code);
}
