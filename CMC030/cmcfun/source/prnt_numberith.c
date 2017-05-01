/*	%TITLE "Figure out follower for a number (st,nd,...)"
 */
#pragma module prnt_numberith "V3.6 Calico"

/*
 * COPYRIGHT (C) 1992 BY
 * Computer Management Center, Inc.
 * Idaho Falls, Idaho  83402
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
 *	This function determines the code to stick on the
 *	end of a number to make it read right, (1st,2nd,
 *	40897th, ...)
 *
 * Parameters:
 *
 *	AMOUNT%
 *		A passed number (max 999,999,999.99, min 0)
 *
 *	The return is a string of the format:
 *
 *		st,nd,rd,th
 *
 * Example:
 *
 *	PRNT_NUMBERITH(100)
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:PRNT_NUMBERITH
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP PRNT_NUMBERITH
 *	$ DELETE PRNT_NUMBERITH.OBJ;*
 *
 * Author:
 *
 *	04/09/92 - Kevin Handy
 *
 * Modification history:
 *
 *	07/08/93 - Kevin Handy
 *		Convert to C.
 *
 *	07/08/93 - Kevin Handy
 *		Fixed problen with 11-13.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	10/20/95 - Kevin Handy
 *		Put into sharable library.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, descrip.h, str$routines.h)
 *--
 */

#include <descrip.h>
#include <str$routines.h>

void prnt_numberith(struct dsc$descriptor *returnstr, long *amount)
{
	if ((((*amount) % 100) >= 11) && (((*amount) % 100) <= 13))
	{
		str$copy_r(returnstr, &2l, &"th");
	}
	else
	{
		/*
		 * Determine correct ending
		 */
		switch ((*amount) % 10)
		{
		case 1:
			str$copy_r(returnstr, &2, &"st");
			break;

		case 2:
			str$copy_r(returnstr, &2l, &"nd");
			break;

		case 3:
			str$copy_r(returnstr, &2l, &"rd");
			break;

		default:
			str$copy_r(returnstr, &2l, &"th");
			break;
		}
	}
}
