
/*	%TITLE "Function to Allocate a Channel"
 */
#pragma module assg_channel "V3.6 Calico"
/*
 *		COPYRIGHT (C) 1987 BY
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
 *	This function is used to allocate a channel for a
 *	file.
 *	.b
 *	.list 0,"*"
 *	.le
 *	If the passed in channel is positive, it
 *	will be used as is.
 *	.le
 *	If the passed channel is zero, it will
 *	allocate a channel from the 1 to 99 range.
 *	.le
 *	If the passed channel is negitive, it will
 *	allocate the positive value of that channel
 *	if possible, else it will allocate a channel
 *	from the 1 to 99 range.
 *	.endlist
 *
 * Parameters:
 *
 *	CH
 *		Used to pass in a prototype channel, and
 *		return the allocated channel.
 *	
 *	ST%
 *		Returns zero (0) if successful, otherwise
 *		returns a non-zero for failure.
 *
 * Example:
 *
 *	CALL ASSG_CHANNEL(TEMP.CH%, ST%)
 *	IF ST%
 *	THEN
 *		PRINT "Unable to allocate a channel"
 *	END IF
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:ASSG_CHANNEL/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP ASSG_CHANNEL
 *	$ DELETE ASSG_CHANNEL.OBJ;*
 *
 * Author:
 *
 *	07/01/88 - Kevin Handy
 *
 * Modification history:
 *
 *	07/01/88 - Frank F. Starman
 *	 	Display message if cannot assign a channel
 *
 *	07/18/88 - Kevin Handy
 *		Modified to make into a SUB, and use channels
 *		1 to 99 instead of LUN's.
 *
 *	04/30/90 - Frank F. Starman
 *		Call HELP_34MESSAGE function instead ENTR_3MESSAGE.
 *
 *	05/13/90 - Kevin Handy
 *		MANY fixes so that it would compile without errors.
 *
 *	05/14/90 - Kevin Handy
 *		Another fix to return back a 0 for succussful
 *		completion instead of the 1 that Frank changed it to.
 *
 *	08/16/90 - Kevin Handy
 *		Converted to C.
 *
 *	09/09/91 - Kevin Handy
 *		Modified documentation to match call.
 *
 *	11/04/92 - Kevin Handy
 *		Modified to lose SCOPE structure, and to abort program
 *		when unable to allocate channels, since nothing handles
 *		the errors properly anyway.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 standards.
 *
 *	10/19/95 - Kevin Handy
 *		Merge assg_freechannel into same code, so that
 *		channel_stat could be made static.
 *
 *	05/26/99 - Kevin Handy
 *		Modified to compile with DEC-C without warnings
 *		(pragma, stdlib.h, stdio.h)
 *--
 */

#include <stdio.h>
#include <stdlib.h>
#include <descrip.h>
/* #include "func_include:scope.h" */

static char channel_stat[99];

void assg_channel(long *ch, long *exit_status)
{
	long i;

/*	static $DESCRIPTOR(p2,"unable to allocate a channel");	*/
/*	static $DESCRIPTOR(p3,"E");				*/
/*	static $DESCRIPTOR(p4,"ASSG_CHANNEL");			*/
/*	static $DESCRIPTOR(p5,"");				*/
/*	static $DESCRIPTOR(p6,"CHANNOTALL");			*/


	/*
	 * Assume success
	 */
	*exit_status = 0;

	if (abs(*ch) > 99)
	{
		goto FError;
	}

	/*
	 * If passed a negative number, make it positive and handle as
	 * a positive channel.  If that channel is already allocated,
	 * change the channel.
	 */
	if (*ch < 0)
	{
		*ch = -(*ch);

		if (channel_stat[*ch] != 0)
		{
			*ch = 0;
		}
		else
		{
			channel_stat[*ch] = 1;
			goto ExitProgram;
		}
	}

	/*
	 * If passed a positive number, flag it as being used if it is
	 * in the proper range, otherwise ignore.
	 */
	if (*ch > 0)
	{
		channel_stat[*ch] = 1;
		goto ExitProgram;
	}

	/*
	 * Search for a free channel, starting at 99 and working down.
	 */
	for (i = 99; i >=1; i--)
	{
		if (channel_stat[i] == 0)
		{
			channel_stat[i] = 1;
			*ch = i;
			goto ExitProgram;
		}
	}

 FError:
	/*
	 * Otherwise, we have an error
	 */		
	printf("\n       \nNo more channels available   \n     \n");
	exit(1);

 ExitProgram:;

}

void assg_freechannel(long *ch)
{
	/*
	 * If passed a positive number, flag it as being available if it is
	 * in the proper range, otherwise ignore.
	 */
	if ((0 < *ch) && (*ch <= 99))
	{
		channel_stat[*ch] = 0;
		*ch = 0;
	}
}
