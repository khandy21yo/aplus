/*
 *	%TITLE "Aging Function for Certain Number of Days"
 */
#pragma module date_daycode "V3.6 Calico"

/*
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
 * Compile
 *
 *	$ cc FUNC_SOURCE:DATE_DAYCODE/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP DATE_DAYCODE
 *	$ DELETE DATE_DAYCODE.OBJ;*
 *
 *--
 */

#include <string>

long date_daycode(const std::string &datum)
{
	long mon, day, yr, end_yr, code, ly;
	double jdy;
	const char *dta;
	int length;

	/*
	 * Check for junk being passed in
	 */
	length = datum.size();
	dta = datum.c_str();

	if (length != 8)
	{
		return(0);
	}

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
