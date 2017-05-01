/*
 *	%TITLE "GL Account Overlay Mask"
 */
#pragma module gl_assg_accmask "V3.6 Calico"

/*
 * COPYRIGHT (C) 1987, 1988 BY     
 *
 * Computer Management Center, Inc.
 * Idaho Falls, Idaho
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies thereof may not be provided or otherwise made
 * available to any other person.  No title to and ownership of
 * the software is hereby transferred.
 *
 * The information in this software is subject to change without
 * notice and should not be construed as a committment by
 * Computer Managemant Center, Inc.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
 * Abstract:HELP	
 *	.p
 *	This function will take a template (??1-??) account,
 *	a target (100-00), and convert to a result account (101-00)
 *	The purpose of this function is to take an expense account
 *	from the payroll and convert that account to an overhead
 *	expense account.
 *
 * Index:
 * Option:
 *
 *
 * Input:
 *
 *	ACCT_TEMPLATE$	= Account template (??1-??)
 *	ACCT_TARGET$	= Account to be converted (100-00)
 *
 * Output:
 *
 *	ACCT_RESULT$	= Resulting account (101-00)
 *
 * Example:
 *
 *	CALL GL_ASSG_ACCMASK("??1-??", &
 *		"100-00", &
 *		ACCT_RESULT$)
 *
 * Environment:
 *
 *	VAX/VMS                                           
 *
 * Compile:
 *
 *	$ CC GL_SOURCE:GL_ASSG_ACCMASK/G_FLOAT
 *	$ LIB FUNC_LIB:CMCFUN/REP GL_ASSG_ACCMASK
 *	$ DELETE GL_ASSG_ACCMASK.OBJ;*
 *
 * Author:
 *
 *	12/4/87 - Robert Peterson
 *
 * Modification history:
 *
 *	05/23/88 - Kevin Handy
 *		Modified so that result can be a map element.
 *
 *	08/08/90 - Kevin Handy
 *		Converted to C
 *
 *	05/07/91 - Kevin Handy
 *		Added '+' to increment number, but did not add '-'
 *		to subtract number because some people want a '-'
 *		in the account numbers.
 *
 *	11/05/92 - Kevin Handy
 *		Added '~' to decrement numbers, to match with the
 *		'-' operation.  Modified to use a select statement
 *		instead of heavily embeded if-then-else.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	06/01/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, str$routines.h)
 *--
 */

/*
 * Include files
 */
#include <descrip.h>
#include <str$routines.h>

/*
 * Define things that should be there, but may not be
 */
#ifndef min
#define min(a,b)	((a)>(b) ? (b) : (a))
#endif

/*
 * Main function
 */
void gl_assg_accmask(struct dsc$descriptor_s *acct_template,
	struct dsc$descriptor_s *acct_target,
	struct dsc$descriptor_s *acct_result)
{
	int loop, loop1;
	int count;
	long stat;

	char *c1, *c2;
	char ch;

	/*
	 * Is the template blank?
	 */
	if (acct_template->dsc$w_length == 0)
	{
		/*
		 * Copy over initial string
		 */
		stat = str$copy_dx(acct_result, acct_target);
	}
	else
	{
		/*
		 * Copy over initial string
		 */
		stat = str$copy_dx(acct_result, acct_template);

		/*
		 * Scan through this string, and replace all question
		 * marks with characters from the target string.
		 */
		count = min(acct_result->dsc$w_length, acct_target->dsc$w_length);

		c1 = acct_result->dsc$a_pointer;
		c2 = acct_target->dsc$a_pointer;

		for (loop = 0; loop < count; loop++)	/* Don't forget that the */
							/* pointer starts at 0, not 1 */
		{
			switch(c1[loop])
			{
			/*
			 * Take character from target string
			 */
			case '?':
				c1[loop] = c2[loop];
				break;
			/*
			 * Take character from target string, increment digit
			 * by one.
			 */
			case '+':
				c1[loop] = c2[loop]+1;
				if (c1[loop] == ':')
					c1[loop] = '0';
				break;
			/*
			 * Take character from target string, dincrement digit
			 * by one.
			 */
			case '~':
				c1[loop] = c2[loop]-1;
				if (c1[loop] == '/')
					c1[loop] = '9';
				break;
			/*
			 * Mapping.  "xy? will examine the input string
			 * and replace an occurence of y in the template
			 * string with x.  ("ab"cd"ef? is ligit)
			 * Assumes a ? character will follow.
			 */
			case '"':
				ch = c2[loop];
				while (c1[loop] == '"')
				{
					if (c2[loop] == c1[loop+1])
					{
						ch = c1[loop+2];
					}
					for (loop1=loop; loop1 < count-3; loop1++)
					{
						c1[loop1] = c1[loop1+3];
					}
					c1[count-1] = '?';
					c1[count-2] = '?';
					c1[count-3] = '?';
				}
				c1[loop]=ch;
				break;
			}
		}
	}
}
