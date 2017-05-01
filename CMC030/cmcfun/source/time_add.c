/*	%TITLE "Add Time"
 */
#pragma module time_add "V3.6 Calico"

/*
 *	COPYRIGHT (C) 1987 BY
 *	Computer Management Center
 *	Idaho Falls, Idaho
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
 * Computer Management Center
 *
 * Computer Management Center assumes no responsibility for the use
 * or reliability of its software on equipment which is not supported
 * by Computer Management Center.
 *
 *++
 *
 * Abstract:HELP
 *	.p
 *	This function calculates total of two times
 *
 * Parameters:
 *
 *	INT_TIME
 *
 *	ADD_TIME in format hours:minuts:seconds
 *
 *	OPERATION "+" or "-"
 *
 *	TIME_ADD in format HH:MM:SS
 *
 * Example:
 *
 *	INT_TIME = 10:40:20
 *	ADD_TIME = :100:10
 *	OPERATION = "+"
 *	TIME_ADD = 12:20:30
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:TIME_ADD
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP TIME_ADD
 *	$ DELETE TIME_ADD.OBJ;*
 *
 * Author:
 *
 *	09/30/87 - Frank Starman
 *
 * Modification history:
 *
 *	03/26/93 - Kevin Handy
 *		Clean up (Check)
 *
 *	07/28/93 - Kevin Handy
 *		Converted to C. This is more robust than the basic
 *		version since it doesn't crash and burn when add_time
 *		is like "1:2" instead of "0:1:2".
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, stdio.h, str$routines.h, sprintf)
 *--
 */

/*
 * Disable debug code (asserts)
 */
#define NDEBUG

/*
 * Include files
 */
#include <stdio.h>
#include <assert.h>
#include <descrip.h>
#include <str$routines.h>

/*
 * Local functions
 */
static long ConvertTime(struct dsc$descriptor *time);

/*
 * Main function
 */
void time_add(struct dsc$descriptor *returnstr,
	struct dsc$descriptor *int_time,
	struct dsc$descriptor *operation,
	struct dsc$descriptor *add_time)
{
	/*
	 * Local variables
	 */
	long int_seconds;
	long add_seconds;
	char makestring[10];

	/*
	 * Diagnostic tests on input
	 */
	assert(operation->dsc$w_length != 0);
	assert(int_time->dsc$w_length != 0);
	assert(add_time->dsc$w_length != 0);

	/*
	 * Convert times to seconds
	 */
	int_seconds = ConvertTime(int_time);
	add_seconds = ConvertTime(add_time);

	/*
	 * Do operation
	 */
	switch(operation->dsc$a_pointer[0])
	{
	case '+':
		int_seconds = int_seconds + add_seconds;
		break;

	case '-':
		int_seconds = int_seconds - add_seconds;
		while (int_seconds < 0)
		{
			int_seconds += 86400;
		}
		break;
	}

	/*
	 * Create output time
	 */
	sprintf(makestring, "%02d:%02d:%02d",
		((int_seconds / (60 * 60)) % 24),	/* Hours */
		(int_seconds / 60) % 60,		/* Minutes */
		(int_seconds) % 60);			/* Seconds */

	/*
	 * Return output string
	 */
	str$copy_r(returnstr, &8l, &makestring);
}

/*
 * Convert a time into seconds.
 * Cannot use time_code function because the time entered here may
 * have a format of 0:100:0, which time_code doesn't like.
 */
static long ConvertTime(struct dsc$descriptor *time)
{
	/*
	 * Local variables
	 */
	int loop;
	long fullresult;
	long partialresult;
	char ch;

	/*
	 * Process entire time string
	 */
	fullresult = 0;
	partialresult = 0;
	for (loop = 0; loop < time->dsc$w_length; loop++)
	{
		ch = time->dsc$a_pointer[loop];

		switch (ch)
		{
		case ':':
			fullresult = fullresult * 60 + partialresult;
			partialresult = 0;
			break;

		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			partialresult = partialresult * 10 + ch - '0';
			break;
		}
	}

	/*
	 * Finish up last partial result
	 */
	fullresult = fullresult * 60 + partialresult;

	/*
	 * Return answer
	 */
	return(fullresult);
}
