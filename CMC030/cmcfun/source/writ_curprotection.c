/*	%TITLE "Set Current Protection Code"
 */
#pragma module writ_curprotection "V3.6 Calico"

/*
 *		COPYRIGHT (C) 1986 BY
 *		Computer Management Center, Idaho Falls, Idaho.
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
 *	This subroutine writes the current protection code to the file.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	NEWPRO$
 *		The passed new protection code the user wants to enter.
 *
 *	Returned value
 *		This subroutine sets the new protection code.
 *
 * Example:
 *
 *	CALL WRIT_CURPROTECTION('65555')
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:WRIT_CURPROTECTION
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP WRIT_CURPROTECTION
 *	$ DELETE WRIT_CURPROTECTION.OBJ;*
 *
 * AUTHOR:
 *
 *	11/20/86 - Kevin Handy
 *
 * MODIFICATION HISTORY:
 *
 *	07/24/89 - Kevin Handy
 *		Added to sharable library.
 *
 *	07/11/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, smg$routines.h)
 *--
 */

/*
 * Include files
 */
#include <descrip.h>

/*
 * External functions
 */
extern long sys$setdfprot();

void writ_curprotection(struct dsc$descriptor *newpro, long *stat)
{
	/*
	 * Local variables
	 */
	long workstat = 65535l;		/* No Access to start */
	int i;
	int section = 0;
	char ch;
	long group = 0;

	/*
	 * Make sure they are trying to set it, and not just living
	 * with the defaults.
	 */
	if (newpro->dsc$w_length == 0)
	{
		return;
	}

	for (i = 0; i < newpro->dsc$w_length; i++)
	{
		/*
		 * Pull off current character
		 */
		ch = *(newpro->dsc$a_pointer + i);

		/*
		 * If it defines a new group, then set up for that group
		 */
		if (section == 0)
		{
			/*
			 * Figure out which group is being referenced
			 */
			switch (ch)
			{
			case 'S':	/* System */
			case 's':
				group = 1;
				section = 1;
				break;

			case 'O':	/* Owner */
			case 'o':
				group = 16;
				section = 1;
				break;

			case 'G':	/* Group */
			case 'g':
				group = 256;
				section = 1;
				break;

			case 'W':	/* World */
			case 'w':
				group = 4096;
				section = 1;
				break;
			}
		}
		else
		{
			/*
			 * Must be a fill character,
			 * or a protection code character
			 */
			switch (ch)
			{
			case 'R':	/* Read */
			case 'r':
				workstat = workstat & ~(group * 1);
				break;

			case 'W':	/* Write */
			case 'w':
				workstat = workstat & ~(group * 2);
				break;

			case 'E':	/* Execute */
			case 'e':
				workstat = workstat & ~(group * 4);
				break;

			case 'D':	/* Delete */
			case 'd':
				workstat = workstat & ~(group * 8);
				break;

			case ',':
				section = 0;
				break;
			}
		}

	}

	if (workstat != 65535l)
	{
		sys$setdfprot(&workstat, 0l);
	}
	*stat = workstat;
}
