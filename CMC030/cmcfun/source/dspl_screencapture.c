/*	%TITLE "AST function to capture screen into a structure"
 */
#pragma module dspl_screencaptire "V3.6 Calico"

/*
 *
 * COPYRIGHT (C) 1989 BY
 * Computer Management Center, Inc.
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
 * notice and should not be construed as a committment by
 * Computer Management Center, Inc.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
 * Abstract:
 *
 *	This is an AST function used to capture the current
 *	screen image for variuos functions (mainly
 *	documentation functions).
 *
 *	This is not a general purpose routine.  It will only
 *	work with another function designed specifically to
 *	interface with it.
 *
 * Parameters:
 *	Defined by AST processing.
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:DSPL_SCREENCAPTURE/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP DSPL_SCREENCAPTURE
 *	$ DELETE DSPL_SCREENCAPTURE.OBJ;*
 *
 * Author:
 *
 *	12/01/89 - Kevin Handy
 *
 * Modification history:
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/26/99 - Kevin Handy
 *		Modified to compile cleanly with DEC-C
 *		(module, descrip.h, screencapture_struct)
 *--
 */

/*
 * Include files
 */
#include <smgdef.h>
#include <descrip.h>
#include "func_include:cmcfun.h"

#include <stdio.h>

long dspl_screencapture(struct dsc$descriptor *mesg,
	long param)
{
	char *from;
	char *to;
	long count;
	struct screencapture_struct *screencapture =
		(struct screencapture_struct *)param;

	/*
	 * Something is really wrong here, since the address is not
	 * being passed in properly, so exit quickly
	 */
	if (screencapture == 0)
	{
		return 1;
	}

	/*
	 * Copy all of this line into the structure, and increment
	 * the current line number.
	 */
	count = mesg->dsc$w_length;
	if (screencapture->lines < 24)
	{
		from = mesg->dsc$a_pointer;
		to = &(screencapture->screen[screencapture->lines++][0]);

		while (count--)
		{
			*to++ = *from++;
		}
	}

	/*
	 * This AST must return (1) back, or else the calling routine
	 * assumes that something went wrong.
	 */
	return(1);
}
