/*	TITLE "Get Current Protection Code"
 */
#pragma module read_curprotection "V3.6 Calico"

/*
 *	COPYRIGHT (C) 1986 BY
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
 *	It receives the current protection codes from the program.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	This function returns the current value of the protection code.
 *
 * Example:
 *
 *	PROT$ = READ_CURPROTECTION
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:READ_CURPROTECTION
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP READ_CURPROTECTION
 *	$ DELETE READ_CURPROTECTION.OBJ;*
 *
 * AUTHOR:
 *
 *	11/20/86 - Kevin Handy
 *
 * MODIFICATION HISTORY:
 *
 *	07/10/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, string.h, str$routines.h)
 *--
 */

/*
 * Include files
 */
#include <string.h>
#include <descrip.h>
#include <str$routines.h>

/*
 * External functions
 */
extern long sys$setdfprot();

/*
 * Local functions
 */
void parseunit(char *makestring, long stat);

/*
 * Main function
 */
void read_curprotection(struct dsc$descriptor *protect, long *status)
{
	long sys_status;
	long getvalue;
	long length;

	char makestring[32];

	makestring[0] = '\0';

	/*
	 * Get integer value of protection codes
	 */
	sys_status = sys$setdfprot(0l, &getvalue);
	*status = 0;

	if ((sys_status & 1) == 0)
	{
		*status = sys_status;
		str$free1_dx(protect);
		return;
	}

	/*
	 * Parse SYSTEM section
	 */
	strcat(makestring, "S:");
	parseunit(makestring, getvalue & 15);

	/*
	 * Parse OWNER section
	 */
	strcat(makestring, ",O:");
	parseunit(makestring, (getvalue / 16) & 15);

	/*
	 * Parse GROUP section
	 */
	strcat(makestring, ",G:");
	parseunit(makestring, (getvalue / 256) & 15);

	/*
	 * Parse WORLD section
	 */
	strcat(makestring, ",W:");
	parseunit(makestring, (getvalue / 4096) & 15);

	/*
	 * Set up return value and exit
	 */
	*status = 0;
	length = strlen(makestring);
	str$copy_r(protect, &length, &makestring);
}

/*
 * Parse one small unit of the protection code
 */
void parseunit(char *makestring, long stat)
{
	if ((stat & 1) == 0)
	{
		strcat(makestring, "R");
	}
	if ((stat & 2) == 0)
	{
		strcat(makestring, "W");
	}
	if ((stat & 4) == 0)
	{
		strcat(makestring, "E");
	}
	if ((stat & 8) == 0)
	{
		strcat(makestring, "D");
	}
}
