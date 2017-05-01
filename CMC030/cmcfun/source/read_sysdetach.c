/*
 *	%TITLE "READ_SYSDETACH - Is It A Detached Process??"
 */
#pragma module read_sysdetach "V3.6 Calico"

/*
 * COPYRIGHT (C) 1986 BY
 * Computer Management Center
 * Idaho Falls, Idaho.
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
 *	This function returns -1% if the process is detached.
 *
 * Index:
 *
 * Parameters:
 *
 *	This program returns an integer value telling if the process
 *	is detached or not.
 *
 * Example:
 *
 *	ANS% = READ_SYSDETACH
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:READ_SYSDETACH
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP READ_SYSDETACH
 *	$ DELETE READ_SYSDETACH.OBJ;*
 *
 * AUTHOR:
 *
 *	12/12/86 - B Craig Larsen
 *
 * MODIFICATION HISTORY:
 *
 *	07/06/92 - Kevin Handy
 *		Modified to use PRC$M_INTER instead of PRC$M_DETACH because
 *		it didn't work correctly on a VAX Station (X terminal).
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, lib$routines.h, case)
 *--
 */

#include <ssdef.h>
#include <jpidef.h>
#include <prcdef.h>
#include <lib$routines.h>

long int read_sysdetach()
{
	long sys_stat, in_val;
	long sysdetach = 0;

	/*
	 * Get the sys call and convert to integer
	 */
	sys_stat = lib$getjpi
	(
		&JPI$_CREPRC_FLAGS, 0L, 0L,
		&in_val, 0L, 0L
	);

	if ((sys_stat & 1) != 0)
	{
#if 0
		if (in_val & PRC$M_DETACH)
			sysdetach = -1;
#else
		if ((in_val & PRC$M_INTER) == 0)
			sysdetach = -1;
#endif
	}

	return(sysdetach);
}
