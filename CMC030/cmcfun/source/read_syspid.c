/*	%TITLE "Return the System ID Number."
 */
#pragma module read_syspid "V3.6 Calico"

/*
 *
 * COPYRIGHT (C) 1986 BY
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
 *	This function returns the system ID number.
 *
 * Index:
 *
 * Parameters:
 *
 *	This function returns the system ID number as a string.
 *
 * Example:
 *
 *	ID$ = READ_SYSPID
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:READ_SYSPID
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP READ_SYSPID
 *	$ DELETE READ_SYSPID.OBJ;*
 *
 * Author:
 *
 *	09/22/86 - B. Craig Larsen
 *
 * Modification history:
 *
 *	07/08/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, lib$routines.h, str$routines.h)
 *--
 */

/*
 * Include files
 */
#include <descrip.h>
#include <syidef.h>
#include <ssdef.h>
#include <lib$routines.h>
#include <str$routines.h>

/*
 * Main function
 */
void read_syspid(struct dsc$descriptor *returnstr)
{
	/*
	 * Local variables
	 */
	long sys_stat;
	struct dsc$descriptor in_val;

	/*
	 * Create a blank DYNAMIC string (Don't forget to clean up)
	 */
	in_val.dsc$a_pointer = 0;
	in_val.dsc$w_length = 0;
	in_val.dsc$b_class = DSC$K_CLASS_D;
	in_val.dsc$b_dtype = DSC$K_DTYPE_T;

	/*
	 * Get the sys call
	 */
	sys_stat = lib$getsyi(&SYI$_SID, 0l, &in_val);

	/*
	 * Make sure there ws no errors
	 */
	switch(sys_stat)
	{
	case SS$_NORMAL:
		str$copy_dx(returnstr, &in_val);
		break;

	default:
		str$free1_dx(returnstr);
		break;
	}

	/*
	 * Clean up
	 */
	str$free1_dx(&in_val);
}
