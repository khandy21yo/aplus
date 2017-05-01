/*	%TITLE "Convert a zip code to the ZipBarCode format
 */
#pragma module zipc_barcode "V3.6 Calico"

/*
 *	COPYRIGHT (C) 1995 BY
 *	Software Solutions
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
 * Software Solutions.
 *
 * Software Solutions assumes no responsibility for the use or
 * reliability of its software on equipment which is not supported
 * by Software Solutions.
 *
 *++
 *
 * Abstract:HELP
 *	.b
 *	.lm +5
 *	This function converts a zipcode into the Post Offices BarCode
 *	format.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	ZIPCODE$
 *		The passed zip code that is to be formatted.
 * Example:
 *
 *	BARCODE$ = ZIPC_BARCODE("83404-1328")
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:ZIPC_BARCODE
 *	$ LIB FUNC_LIB:CMCFUN/REP ZIPC_BARCODE
 *	$ DELETE ZIPC_BARCODE.OBJ;*
 *
 * Author:
 *
 *	05/19/95 - Kevin Handy
 *
 * Modification history:
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, str$routines.h)
 *--
 */

/*
 * Include files
 */
#include <stdlib.h>
#include <ctype.h>
#include <descrip.h>
#include <string.h>
#include <str$routines.h>

/*
 * The definitions for ZERO and ONE indicate which characters are
 * used and accepted for the bar code bits. '.' is best for ZERO and
 * '|' for ONE.
 */
#define ZERO            '.'
#define ONE             '|'

static char ZipBarCode[10][6] =
{
	{ ONE,  ONE,  ZERO, ZERO, ZERO, 0 },	/* 0 = "||..." */
	{ ZERO, ZERO, ZERO, ONE,  ONE,  0 },	/* 1 = "...||" */
	{ ZERO, ZERO, ONE,  ZERO, ONE,  0 },	/* 2 = "..|.|" */
	{ ZERO, ZERO, ONE,  ONE,  ZERO, 0 },	/* 3 = "..||." */
	{ ZERO, ONE,  ZERO, ZERO, ONE,  0 },	/* 4 = ".|..|" */
	{ ZERO, ONE,  ZERO, ONE,  ZERO, 0 },	/* 5 = ".|.|." */
	{ ZERO, ONE,  ONE,  ZERO, ZERO, 0 },	/* 6 = ".||.." */
	{ ONE,  ZERO, ZERO, ZERO, ONE,  0 },	/* 7 = "|...|" */
	{ ONE,  ZERO, ZERO, ONE,  ZERO, 0 },	/* 8 = "|..|." */
	{ ONE,  ZERO, ONE,  ZERO, ZERO, 0 }	/* 9 = "|.|.." */
};

void zipc_barcode(struct dsc$descriptor *returnstr,
	struct dsc$descriptor *zipcode)
{
	int checksum;
	int source;
	char *dest;
	static char barcode[500];
	char digit;
	long length;

	/*
	 * Initialize
	 */
	dest = &barcode[0];
	checksum = 0;

	/*
	 * Start with a start bit (1)
	 */
	*(dest++) = ONE;

	/*
	 * Handle all digits in zip code
	 */
	for (source = 0; source < zipcode->dsc$w_length; source++)
	{
		/*
		 * Process one digit
		 */
		digit = zipcode->dsc$a_pointer[source];
		if (isdigit(digit))
		{
			strcpy(dest, ZipBarCode[digit - '0']);
			dest += 5;
			checksum += (digit - '0');
		}
	}

	/*
	 * Append checksum
	 */
	checksum = 10 - checksum % 10;
	strcpy(dest, ZipBarCode[checksum]);
	dest += 5;

	/*
	 * Append a stop bit, and terminate string
	 */
	*(dest++) = ONE;
	*dest = '\0';

	/*
	 * Return string
	 */
	length = strlen(barcode);
	str$copy_r(returnstr, &length, &barcode);
}
