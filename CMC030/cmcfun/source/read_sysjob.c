/*	%TITLE "Return the Job Number of the Current Image"
 */
#pragma module read_sysjob "V3.6 Calico"

/*
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
 *	.lm +5
 *	.b
 *	This function returns the name of the current image.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	This function returns the job name of the current image.
 *
 * Example:
 *
 *	JOB$ = READ_SYSJOB
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:READ_SYSJOB
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP READ_SYSJOB
 *	$ DELETE READ_SYSJOB.OBJ;*
 *
 * Author:
 *
 *	09/18/86 - B. Craig Larsen
 *
 * Modification history:
 *
 *	07/07/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, str$routines.h, lib$routines.h)
 *--
 */

/*
 * Include files
 */
#include <descrip.h>
#include <jpidef.h>
#include <ssdef.h>
#include <str$routines.h>
#include <lib$routines.h>

/*
 * Main function
 */
void read_sysjob(struct dsc$descriptor *returnstr)
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
	 * Get the sys call and convert to integer
	 */
	sys_stat = lib$getjpi(&JPI$_PID, 0l, 0l, 0l, &in_val);

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
