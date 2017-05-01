/*	%TITLE "Convert Number into Text String for Check"
 */
#pragma module prnt_numbertext "V3.6 Calico"

/*
 * Copyright (C) 1995 by Software Solutions
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
 *	These function converts an amount
 *	into a verbose text string.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	AMOUNT%
 *		A passed number (max 999,999,999.99, min 0)
 *
 *	The return is a string of the format:
 *
 *		Four Thousand Eighty Seven
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:PRNT_NUMBERTEXT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP PRNT_NUMBERTEXT
 *	$ DELETE PRNT_NUMBERTEXT.OBJ;*
 *
 * Author:
 *
 *	10/09/87 - Kevin Handy
 *
 * Modification history:
 *
 *	09/18/89 - Kevin Handy
 *		Cleaned up some odd looking code, decreased
 *		size slightly.
 *
 *	07/09/93 - Kevin Handy
 *		Converted to C.
 *
 *	03/10/95 - Kevin Handy
 *		Merged in PRNT_CHECKPROTECT so that the two functions
 *		could share code and data.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	04/25/95 - Kevin Handy
 *		Remove PRNT_CHECKPROTECT funtion, since it is in the
 *		charable library and this one isn't.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, str$routines.h)
 *--
 */

/*
 * Include files
 */
#include <descrip.h>
#include <string.h>
#include <str$routines.h>

/*
 * Local functions
 */
static void dosegment(char *outtext, long segment);

/*
 * Create array for numbers
 */
static char *onedigit[] =
{
	"Zero ",
	"One ",
	"Two ",
	"Three ",
	"Four ",
	"Five ",
	"Six ",
	"Seven ",
	"Eight ",
	"Nine ",
	"Ten ",
	"Eleven ",
	"Twelve ",
	"Thirteen ",
	"Fourteen ",
	"Fifteen ",
	"Sixteen ",
	"Seventeen ",
	"Eighteen ",
	"Nineteen ",
};

static char *tendigit[] =
{
	"Zero ",
	"Ten ",
	"Twenty ",
	"Thirty ",
	"Forty ",
	"Fifty ",
	"Sixty ",
	"Seventy ",
	"Eighty ",
	"Ninety "
};


/*
 * Main Function
 */
void prnt_numbertext(struct dsc$descriptor *returnstr,
	long *amount)
{
	char outtext[128];
	long threedigit;
	long length;

	/*
	 * Zero output string
	 */
	outtext[0] = '\0';

	/*
	 * Make sure it is a legal check amount
	 */
	if ((*amount < 999999999) && (*amount > 0))
	{
		/*
		 * Do conversion to text for whole part,
		 * three digits at a time
		 */
		threedigit = (*amount / 1000000) % 1000;
		if (threedigit)
		{
			dosegment(outtext, threedigit);
			strcat(outtext, "Million ");
		}

		threedigit = (*amount / 1000) % 1000;
		if (threedigit)
		{
			dosegment(outtext, threedigit);
			strcat(outtext, "Thousand ");
		}

		threedigit = (*amount) % 1000;
		if (threedigit)
		{
			dosegment(outtext, threedigit);
		}

	}

	/*
	 * Return answer (Lose trailing space while at it)
	 */
	length = strlen(outtext);

	if (length > 0)
	{
		length--;
	}
	str$copy_r(returnstr, &length, &outtext);
}

/*
 * Handle a segment of three digits
 */
static void dosegment(char *outtext, long segment)
{
	long digit;

	/*
	 * Handle hundreds
	 */
	digit = segment / 100;
	if (digit != 0)
	{
		strcat(outtext, onedigit[digit]);
		strcat(outtext, "Hundred ");
	}

	/*
	 * Handle 99s
	 */
	digit = segment % 100;
	if (digit != 0)
	{
		if ((digit > 0) && (digit < 20))
		{
			strcat(outtext, onedigit[digit]);
		}
		else
		{
			digit = (segment / 10) % 10;
			strcat(outtext, tendigit[digit]);

			digit = segment % 10;
			if (digit != 0)
			{
				strcat(outtext, onedigit[digit]);
			}
		}
	}
}

