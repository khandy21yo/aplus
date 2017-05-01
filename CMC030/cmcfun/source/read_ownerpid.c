/*	%TITLE "READ_OWNERPID - Test to See if This Process is Spawned"
 */
#pragma module read_ownerpid "V3.6 Calico"

/*
 *	COPYRIGHT (C) 1987, 1988 BY
 *	Computer Management Center, Inc.
 *	Idaho Falls, Idaho.
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
 *	This function is used to determine if a process
 *	is a spawned process, or the main process.
 *
 *
 * Index:
 *
 *	SUBR_SPAWN
 *
 * Parameters:
 *
 *	PID%
 *	.list
 *		zero (0) for the current process.
 *		nonzero (PID) for a specified process.
 *	.endlist
 *
 *	Returns zero (0) if the process is NOT spawned.
 *
 *	Returns non-zero if it is spawned (The owner PID).
 *
 * Example:
 *
 *	READ_OWNERPID(0%)
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:READ_OWNERPID
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP READ_OWNERPID
 *	$ DELETE READ_OWNERPID.OBJ;*
 *
 * Author:
 *
 *	05/06/88 - Kevin Handy
 *
 * Modification history:
 *
 *	02/24/89 - Kevin Handy
 *		Coverted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module)
 *--
 */

#include <jpidef.h>

extern long lib$getjpi();

long read_ownerpid(long *pid)
{
	long owner_pid;

	lib$getjpi(&JPI$_OWNER, pid, 0, &owner_pid);

	return(owner_pid);
}
