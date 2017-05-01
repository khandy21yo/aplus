/*	%TITLE "Format a Time String into 24-Hour Time."
 */
#pragma module time_storetime "V3.6 Calico"

/*
 * COPYRIGHT (C) 1987 BY
 * Computer Management Center
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
 *	.p
 *	This function takes a users input, and comes up with
 *	a usable time formatted in HHMMSS(24-hour) format.  It can
 *	handle conditions as the following:
 *	.b
 *	.list 0,"*"
 *	.le
 *	H	AM/PM	H	(24-HOUR)
 *	.le
 *	HH	AM/PM	HH	(24-HOUR)
 *	.le
 *	HHM	AM/PM	HHM	(24-HOUR)
 *	.le
 *	HHMM	AM/PM	HHMM	(24-HOUR)
 *	.le
 *	HHMMS	AM/PM	HHMMS	(24-HOUR)
 *	.le
 *	HHMMSS	AM/PM	HHMMSS	(24-HOUR)
 *	.endlist
 *
 *	Where ':' is any non-numeric character.
 *	Those times that are short of 6 characters will be padded
 *	with '0'(zeros).  It will also handle elapse time up to
 *	99:59:59 hours.  Elapse time may be entered as follows:
 *	.b
 *	.list 0,"*"
 *	.le
 *		HH:MM:SS  if KIND$ = 'H' for hours(up to 995959).
 *	.le
 *		HHMMSS
 *	.le
 *		HHMMS
 *	.le
 *		HHMM
 *	.le
 *		HHM
 *	.le
 *		HH
 *	.le
 *		H
 *	.le
 *		MM:SS     if KIND$ = 'M' for minutes(up to 5959).
 *	.le
 *		MMSS
 *	.le
 *		MMS
 *	.le
 *		MM
 *	.le
 *		M
 *	.le
 *		SS	if KIND$ = 'S' for seconds(up to 59).
 *	.le
 *		S
 *	.endlist
 *
 *		Real time if KIND$ = ''
 *
 * Parameters:
 *
 *	XDEFLT$
 *		The passed time string the user wishes to have formatted.
 *
 *	KIND$
 *		The passed way the user wishes the time to be formatted.
 *
 *		Options are:
 *		.table
 *			'H' for hours(up to 995959).
 *			'M' for minutes(up to 5959).
 *			'S' for seconds(up to 59).
 *		.endtable
 *
 *	This function returns a usable time formatted in
 *	HHMMSS(24-hour) format.
 *
 * Example:
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:TIME_STORETIME
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP TIME_STORETIME
 *	$ DELETE TIME_STORETIME.OBJ;*
 *
 * Author:
 *
 *	05/27/86 - B. Craig Larsen
 *
 * Modification history:
 *
 *	04/23/87 - Kevin Handy
 *		Modified to handle bad dates better.
 *
 *	04/30/87 - B. Craig Larsen
 *		Modified to handle more bad dates.
 *
 *	12/10/87 - Kevin Handy
 *		Modified to allow dots as well as colons.
 *
 *	06/13/89 - Kevin Handy
 *		Removed ENTR_MESSAGE call so that function
 *		is more independent, and more compatible
 *		with DATE_STOREDATE.
 *
 *	07/06/93 - Kevin Handy
 *		Modified to use VAL%( instead of VAL( for
 *		a timy bit more speed.
 *
 *	07/14/93 - Kevin Handy
 *		Disabled goofiness allowing such things as
 *		"midnight" and "12 midnight".  They are not
 *		allowed by the description above, and are
 *		just confusing.
 *
 *	07/17/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, str$routines.h)
 *--
 */


/*
 * Include files
 */
#include <stdio.h>
#include <ctype.h>
#include <descrip.h>
#include <str$routines.h>

/*
 * Main function
 */
void time_storetime(struct dsc$descriptor *returnstr,
	struct dsc$descriptor *xdeflt,
	struct dsc$descriptor *kind)
{
	/*
	 * Local Variables
	 */
	char workstring[10];
	int worklen = 0;
	int loop;
	int sepflag = 0;
	int ampm = 0;
	int kindcode;
	int hour;

	/*
	 * Figure out what KIND user wants
	 */
	if (kind->dsc$w_length != 0)
	{
		switch (*kind->dsc$a_pointer)
		{
		case 'S':
		case 's':
			kindcode = 4;
			break;

		case 'M':
		case 'm':
			kindcode = 2;
			break;

		default:
			kindcode = 0;
			break;
		}
	}
	else
	{
		kindcode = 0;
	}

	/*
	 * Initial pass, pull string from descriptor, while converting
	 * "L" to 1, "O" to 0, while forcing two-digit positions
	 * before '.' and ':'.
	 */
	for (loop = 0; loop < xdeflt->dsc$w_length; loop++)
	{
		switch (*(xdeflt->dsc$a_pointer+loop))
		{
		/*
		 * Switch o's to zeroes for those used to IBM typewriters
		 */
		case 'O':
		case 'o':
			workstring[worklen++] = '0';
			break;

		/*
		 * Switch L's to ones for those used to IBM typewriters
		 */
		case 'L':
		case 'l':
		case 'I':
		case 'i':
			workstring[worklen++] = '1';
			break;

		/*
		 * Notice a 'AM' attached to string
		 */
		case 'A':
		case 'a':
			ampm = 1;
			break;

		/*
		 * Notice a 'PM' attached to string
		 */
		case 'P':
		case 'p':
			ampm = 2;
			break;

		/*
		 * For a seperator, make sure we have correct positions
		 * for previous digits
		 */
		case '.':
		case ':':
			sepflag = -1;
			if ((worklen == 1) || (worklen == 3))
			{
				workstring[worklen] = workstring[worklen-1];
				workstring[worklen-1] = '0';
				worklen++;
			}
			break;

		/*
		 * Append digits to output string
		 */
		default:
			if (isdigit(*(xdeflt->dsc$a_pointer+loop)))
			{
				workstring[worklen++] =
					*(xdeflt->dsc$a_pointer+loop);
			}
			break;
		}
	}

	/*
	 * return blank if nothing entered
	 */
	if (worklen == 0)
	{
		str$copy_r(returnstr, &6l, "      ");
		return;
	}

	/*
	 * One last cleanup, set to even number of digits
	 */
	if ((worklen == 3) || (worklen == 1) || (worklen == 5))
	{
		workstring[worklen] = workstring[worklen-1];
		workstring[worklen-1] = '0';
		worklen++;
	}


	/*
	 * Fill in to six characters
	 */
	for (loop = worklen; loop < 6; loop++)
	{
		workstring[loop] = '0';
	}

	/*
	 * if AMPM says to shift, then shift
	 *
	 * Problem is, it can be complex. Time goes as follows:
	 *	01:00:00 AM -> 010000
	 *	...
	 *	11:59:59 AM -> 115959
	 *	12:00:00 PM -> 120000
	 *	...
	 *	12:59:59 PM -> 125959
	 *	01:00:00 PM -> 130000	Here and below add 12.
	 *	...
	 *	11:59:59 PM -> 235959
	 *	12:00:00 AM -> 240000	(Should this be 000000?)
	 *	...
	 *	12:59:59 AM -> 245959
	 */
	if (ampm != 0)
	{

		hour = (workstring[0] - '0') * 10 +
			(workstring[1] - '0');
		if (((ampm == 1) && (hour == 12)) ||
			((ampm == 2) && (hour < 12)))
		{
			hour += 12;
			workstring[0] = '0' + hour / 10;
			workstring[1] = '0' + hour % 10;
		}
	}

	/*
	 * Now, depending on KIND, shift everything over
	 */
	if (kindcode != 0)
	{
		for (loop = 5; loop >= kindcode; loop--)
		{
			workstring[loop] = workstring[loop - kindcode];
		}

		for (loop = 0; loop < kindcode; loop++)
		{
			workstring[loop] = '0';
		}
	}

	/*
	 * Return output string
	 */
	str$copy_r(returnstr, &6l, &workstring);
}
