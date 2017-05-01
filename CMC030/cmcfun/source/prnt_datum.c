/*	%TITLE "Format Date into DD MMM YYYY Format"
 */
#pragma module prnt_datum "V3.6 Calico"

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
 *	This function formats a date into following formats
 *	depending on FRM%
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	INDATE$
 *		The passed date the user wants to format.
 *
 *	FRM%
 *		The passed integer that tells how the date should be formatted.
 *
 *		1% - date given in MMDDYYYY format (1).
 *			date given in YYYYMMDD format (0).
 *
 *		2% - return MMMDDYYYY (2).
 *			return DDMMMYYYY (0).
 *
 *		4% - return MMM DD YYYY (0).
 *			return MMMDDYY (4).
 *
 *		8% - strip off leading spaces.
 *
 *	Returned value
 *		The date that was formatted to the user's
 *		specification.
 *
 * Example:
 *
 *	DATES$ = PRNT_DATUM("040588",1%)
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:PRNT_DATUM
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP PRNT_DATUM
 *	$ DELETE PRNT_DATUM.OBJ;*
 *
 * AUTHOR:
 *
 *	04/09/88 - Frank Starman
 *
 * MODIFICATION HISTORY:
 *
 *	03/26/93 - Kevin Handy
 *		Clean up (Check)
 *
 *	07/06/93 - Kevin Handy
 *		Modified to use VAL%( instead of VAL( which
 *		should be slightly faster.
 *
 *	07/28/93 - Kevin Handy
 *		Added comments about FRM%.
 *
 *	07/29/93 - Kevin Handy
 *		Converted to C.
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
#include <ctype.h>
#include <descrip.h>
#include <string.h>
#include <str$routines.h>

/*
 * Local functions
 */
static void zerodate(char *date);
static char *strnset(char *s, int ch, size_t n);

/*
 * Local defined functions
 */
#define TODIGIT(ch) (isdigit(ch) ? ((ch) - '0') : 0)

/*
 * String constants local to module
 */
static char *monthname[] =
{
	"   ",
	"JAN",
	"FEB",
	"MAR",
	"APR",
	"MAY",
	"JUN",
	"JUL",
	"AUG",
	"SEP",
	"OCT",
	"NOV",
	"DEC",
	"???"
};
static char spaces[] = "           ";

/*
 * Main function
 */
void prnt_datum(struct dsc$descriptor *returnstr,
	struct dsc$descriptor *indate,
	long *frm)
{
	/*
	 * Local variables
	 */
	int month;
	int day;
	int year;
	int month1;
	char worktext[16];
	long length;
	int offset;
	char datecopy[16];

	/*
	 * Move date into working area (no memory management violations
	 * this way)
	 */
	strnset(datecopy, '0', 8);
	strncpy(datecopy, indate->dsc$a_pointer, indate->dsc$w_length);

	/*
	 * Get month/day/year
	 */
	if ((*frm & 1) != 0)
	{
		month = TODIGIT(datecopy[0]) * 10 + 
			TODIGIT(datecopy[1]);
		day = TODIGIT(datecopy[2]) * 10 + 
			TODIGIT(datecopy[3]);
		year = TODIGIT(datecopy[4]) * 1000 + 
			TODIGIT(datecopy[5]) * 100 +
			TODIGIT(datecopy[6]) * 10 + 
			TODIGIT(datecopy[7]);
	}
	else
	{
		year = TODIGIT(datecopy[0]) * 1000 + 
			TODIGIT(datecopy[1]) * 100 +
			TODIGIT(datecopy[2]) * 10 + 
			TODIGIT(datecopy[3]);
		month = TODIGIT(datecopy[4]) * 10 + 
			TODIGIT(datecopy[5]);
		day = TODIGIT(datecopy[6]) * 10 + 
			TODIGIT(datecopy[7]);
	}


	if (month > 12)
	{
		month1 = 13;
	}
	else
	{
		month1 = month;
	}

	if ((*frm & 4) != 0)
	{
		if ((*frm & 2) != 0)
		{
			/*
			 * MMMDDYYYY
			 */
			sprintf(worktext, "%s%2d%4d", monthname[month1],
				day, year);
			zerodate(worktext + 3);
			zerodate(worktext + 7);
		}
		else
		{
			/*
			 * DDMMMYYYY
			 */
			sprintf(worktext, "%2d%s%4d", day, monthname[month1],
				year);
			zerodate(worktext);
			zerodate(worktext + 7);
		}
	}
	else
	{
		if ((*frm & 2) != 0)
		{
			/*
			 * MMM DD YYYY
			 */
			sprintf(worktext, "%s %2d %4d", monthname[month1],
				day, year);
			zerodate(worktext + 4);
			zerodate(worktext + 9);
		}
		else
		{
			/*
			 * DD MMM YYYY
			 */
			sprintf(worktext, "%2d %s %4d", day, monthname[month1],
				year);
			zerodate(worktext);
			zerodate(worktext + 9);
		}
	}

	/*
	 * Lose leading spaces (and keep same length)
	 */
	if ((*frm & 8) == 0)
	{
		offset = 0;
		while (worktext[offset] == ' ')
		{
			offset++;
		}
		if (offset != 0)
		{
			strcpy(worktext, worktext+offset);
			strcat(worktext, spaces + sizeof(spaces) - offset - 1);
		}
	}

	/*
	 * Return string
	 */
	length = strlen(worktext);
	str$copy_r(returnstr, &length, &worktext);
}

/*
 * Function to convert " 0" to "  "
 */
static void zerodate(char *date)
{
	if ((*date == ' ') && (*(date+1) == '0'))
	{
		*(date+1) = ' ';
	}
}

#ifndef MSDOS
/*
 * Not quite exactly as MS-DOS defines this, because it doesn't stop
 * at a null character.
 */
static char *strnset(char *s, int ch, size_t n)
{
	char *s1 = s;

	while (n--)
	{
		*s1++ = ch;
	}
	return(s);
}
#endif
