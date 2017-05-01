/*	%TITLE "Function to Free a Channel"
 */
#pragma module assg_freechannel "V3.6 Calico"

/*
 *	COPYRIGHT (C) 1988 BY
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
 * Abstract:HELP
 *	.p
 *	This function is used to free a channel allocated with
 *	the ASSG_CHANNEL function.
 *
 * Parameters:
 *
 *	CH%
 *		Used to pass in a prototype channel, and
 *		return the allocated channel.
 *	
 * Example:
 *
 *	CALL ASSG_FREECHANNEL(TEMP.CH%)
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:ASSG_FREECHANNEL/G_FLOAT
 *	$ LIB FUNC_LIB:CMCFUN/REP ASSG_FREECHANNEL
 *	$ DELETE ASSG_FREECHANNEL.OBJ;*
 *
 *
 * Author:
 *
 *	07/18/88 - Kevin Handy
 *
 * Modification history:
 *
 *	08/16/90 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 standards.
 *
 *	10/19/95 - Kevin Handy
 *		Moved into assg_channel.c, so that channel_stat could
 *		be made static.
 *
 *	05/27/99 - Kevin Hamdy
 *		Modified to lose warnings with DEC-C
 *		(module)
 *--
 */

#if 0
char channel_stat[99];

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

#endif
