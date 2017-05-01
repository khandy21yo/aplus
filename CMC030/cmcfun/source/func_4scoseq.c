/*
 *	%TITLE "func_4scoseq - Convert Entered Characters to Desired Character"
 */
#pragma module func_4scoseq "V3.6 Calico"

/*
 *
 *	COPYRIGHT (C) 1987 BY
 *	Computer Management Center, Idaho Falls, Idaho.
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
 * ABSTRACT:
 *
 *
 * Parameters:
 *
 *	XCHAR 
 *		The passed character the user wants converted.
 *
 *
 *	Returned values
 *		Converts the characters the user entered
 *		to the desired characters.
 *
 * Example:
 *
 *	CALL func_4scoseq(*)
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:func_4scoseq/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP func_4scoseq
 *	$ DELETE func_4scoseq.OBJ;*
 *
 * AUTHOR:
 *
 *
 * MODIFICATION HISTORY:
 *
 *	11/09/88 - Kevin Handy
 *		Converted to C
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module)
 *--
 */

#include <smgdef.h>

static const int tvalue[] =
{
	SMG$K_TRM_KP0, SMG$K_TRM_KP1, SMG$K_TRM_KP2, SMG$K_TRM_KP3,
	SMG$K_TRM_KP4, SMG$K_TRM_KP5, SMG$K_TRM_KP6, SMG$K_TRM_KP7,
	SMG$K_TRM_KP8, SMG$K_TRM_KP9, SMG$K_TRM_MINUS, SMG$K_TRM_COMMA,
	SMG$K_TRM_PERIOD, SMG$K_TRM_ENTER,
	0
};

static const int rvalue[] =
{
	'0', '1', '2', '3',
	'4', '5', '6', '7',
	'8', '9', '-', ',',
	'.', SMG$K_TRM_CTRLM,
	0
};

int func_4scoseq(int xchar)
{
	register int loop;

	/*
	 * Translate using table only items in the correct range
	 */
	if ((xchar >= 260) && (xchar <= 273))
	{
		for (loop = 0; tvalue[loop] != 0; loop++)
		{
			if (tvalue[loop] == xchar)
			{
				return(rvalue[loop]);
			}
		}
	}
	return(xchar);
}
