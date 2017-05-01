/*	%TITLE "Subroutine to Split the Cursor Move String"
 */
#pragma module dspl_splitcursor "V3.6 Calico"

/*
 *
 *		COPYRIGHT (C) 1986 BY
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
 *	This subroutine will take the CURSTR$('row;col') and 
 *	break it into the row and col integers.
 *
 * Parameters:
 *
 *	CURSTR$ 
 *		The passed cursor movement string (row and col).
 *
 *	RXXX%
 *		The returned row from the CURSTR$.
 *
 *	CYYY% 
 *		The returned column from the CURSTR$.
 *
 * Example:
 *
 *	CALL DSPL_SPLITCURSOR('0;5',XPOS%,YPOS%)
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:DSPL_SPLITCURSOR/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP DSPL_SPLITCURSOR
 *	$ DELETE DSPL_SPLITCURSOR.OBJ;*
 *
 * Author:
 *
 *	07/09/86 - B. Craig Larsen
 *
 * Modification history:
 *
 *	08/17/90 - Kevin Handy
 *		Modified to use VAL% instead of VAL (for more speed).
 *
 *	08/17/90 - Kevin Handy
 *		Removed EDIT$(..,4+8+128)   (for more speed).
 *
 *	08/21/90 - Kevin Handy
 *		Removed error trapping, because that is a programming
 *		problem, and not an operating problem, and should be
 *		fixed in the source code.
 *
 *	08/23/90 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modified to compile with DEC-C without errors.
 *		(module)
 *--
 */

#include <descrip.h>

void dspl_splitcursor(struct dsc$descriptor_s *curstr, long *rxxx, long *cyyy)
{
	register long loop,x=0,y=0;
	char *ptr = curstr->dsc$a_pointer;

	for (loop=0; loop<curstr->dsc$w_length; loop++)
	{
		if (*ptr == ';')
		{
			x=y;
			y=0;
		}
		else
		{
			y = y * 10 + *ptr - '0';
		}
		ptr++;
	}
	*rxxx	= x;
	*cyyy	= y;
}
