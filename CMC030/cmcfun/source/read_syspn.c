/*	%TITLE "Return the File Name of the Current Image"
 *	%SBTTL "READ_SYSPN"
 *	%IDENT "V3.3"
 *
 *
 * COPYRIGHT (C) 1987 BY
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
 *	This function returns the name of the current image.
 *
 * Index:
 * Parameters:
 *
 *	This function returns the file name of the current image.
 *
 * Example:
 *
 *	IMAGE$ = READ_SYSPN
 *
 * Environment:
 *
 *	VAX/VMS BASIC V4.4
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:READ_SYSPN
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP READ_SYSPN
 *	$ DELETE READ_SYSPN.OBJ;*
 *
 * Author:
 *
 *	09/18/86 - B. Craig Larsen
 *
 * Modification history:
 *
 *	07/03/89 - Kevin Handy
 *		Modified so that map's were not needed, and
 *		the function can reside in the sharable library.
 *
 *	07/15/93 - Kevin Handy
 *		Converted to C.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, str$routines.h)
 *--
 */

/*
 * Include files
 */
#include <descrip.h>
#include <jpidef.h>
#include <ssdef.h>
#include <str$routines.h>

/*
 * Define constants
 */
#define FSCN$_NAME	6

/*
 * External functions
 */
extern long lib$getjpi();
extern long sys$filescan();

/*
 * Define structires
 */
struct item_list_entry
{
	short int buffer_length;
	short int item_code;
	int *buffer_add;
};


/*
 * Main function
 */
void read_syspn(struct dsc$descriptor *returnstr)
{
	long sys_stat;
	long sys1_status;
	struct item_list_entry iobuf[2];
	char name_buffer[50];
	struct dsc$descriptor in_val;

	/*
	 * Get the sys call and convert to integer
	 */
	in_val.dsc$a_pointer = 0;
	in_val.dsc$w_length = 0;
	in_val.dsc$b_class = DSC$K_CLASS_D;
	in_val.dsc$b_dtype = DSC$K_DTYPE_T;

	sys_stat = lib$getjpi(&JPI$_IMAGNAME, 0l, 0l, 0l, &in_val);

	if (sys_stat != SS$_NORMAL)
	{
		/*
		 * Return null
		 */
		str$free1_dx(returnstr);
	}
	else
	{
		/*
		 * Strip off all but the program name
		 */
		iobuf[0].item_code	= FSCN$_NAME;
		iobuf[0].buffer_length	= 0;
		iobuf[0].buffer_add	= 0;

		iobuf[1].item_code	= 0;
		iobuf[1].buffer_length	= 0;

		sys_stat = sys$filescan(&in_val, &iobuf, 0l);

		/*
		 * Return the program name
		 */
		str$copy_r(returnstr, &(iobuf[0].buffer_length),
			iobuf[0].buffer_add);
	}

	/*
	 * Clean up working areas
	 */
	str$free1_dx(&in_val);
}
