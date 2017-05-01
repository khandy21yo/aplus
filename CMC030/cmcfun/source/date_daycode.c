/*
 *	%TITLE "Aging Function for Certain Number of Days"
 */
#pragma module date_daycode "V3.6 Calico"

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
 *	This function is based on Sept 14,1752 as day 1 (the date of the
 *	United States conversion to Georgian calendar).
 *
 *	Assumes 8 character date YYYYMMDD.
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
 *	BASE.DAY = DATE_DAYCODE("19871220")
 *
 * Compile
 *
 *	$ cc FUNC_SOURCE:DATE_DAYCODE/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP DATE_DAYCODE
 *	$ DELETE DATE_DAYCODE.OBJ;*
 *
 * Author:
 *
 *	05/30/86 - Frank Starman
 *
 * Modification history:
 *
 *	05/13/89 - Kevin Handy
 *		Modified to return a long instead of a real.
 *
 *	04/24/90 - Frank F. Starman
 *		Add error trapping for ERR=52%
 *
 *	08/14/90 - Kevin Handy
 *		Converted to C.
 *
 *	09/16/92 - Kevin Handy
 *		Check length of incomming string.
 *
 *	09/22/92 - Kevin Handy
 *		Fix bug in length returned back from STR$ANALYZE_SDESC.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/26/99 - Kevin Handy
 *		Modified to compile cleanly in DEC-C
 *		(module, str$routines.h)
 *--
 */

#include <descrip.h>
#include <str$routines.h>

long date_daycode(struct dsc$descriptor_s *datum)
{
	long mon, day, yr, end_yr, code, ly;
	double jdy;
	char *dta;
	int length;

	/*
	 * Check for junk being passed in
	 */
	str$analyze_sdesc(datum, &length, &dta);
	length &= 32767;	/* trim off garbage */

	if (length != 8)
	{
		return(0);
	}

/*	dta = datum->dsc$a_pointer; */

	/*
	 * Pull off month/day/year
	 */
	mon	= (*(dta+4) - '0') * 10 + (*(dta+5) - '0');
	day	= (*(dta+6) - '0') * 10 + (*(dta+7) - '0');
	yr	= (*(dta) - '0') * 1000 + (*(dta+1) - '0') * 100 +
		(*(dta+2) - '0') * 10 + (*(dta+3) - '0');
	end_yr = (((yr % 100) == 0) ? 0 : 1);

	if (mon < 3)
	{
		jdy	= (mon - 1) * 31 + day;
	}
	else
	{
		ly	= 1 - end_yr * ((yr % 4 == 0) ? 0 : 1) -
			    (1-end_yr)*((yr % 400 == 0) ? 0 : 1);
		jdy	= ((mon-2.98)*30.59+59.)+ly+day;
	}

	code	= (yr-1600.)*365.+(long)((yr-1601.)/4.)-55757- 
			(long)((yr-1.)/100.)+(long)((yr-1601.)/400.)+jdy;

	return(code);
}
