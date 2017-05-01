/*	%TITLE "Delete a Key from a Library"
 */
#pragma module libe_delete "V3.6 Calico"

/*
 *	COPYRIGHT (C) 1987 BY
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
 *	This function will delete a key from a library,
 *	and will delete the text associated with the key
 *	if there are no other keys pointing to it.
 *
 * Parameters:
 *
 *	LIB_NAME$
 *		The passed library to remove key from.
 *
 *	KEY_NAME$
 *		The passed name of the key to delete.
 *
 *	Returns a status code.
 *
 * Example:
 *
 *	ST% = LIBR_DELETE("HELP_GL", "REMOVE_KEY")
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:LIBR_DELETE
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP LIBR_DELETE
 *	$ DELETE LIBR_DELETE.OBJ;*
 *
 * Author:
 *
 *	01/07/87 - Kevin Handy
 *
 * Modification history:
 *
 *	07/19/88 - Kevin Handy
 *		Removed defaulting to REF:
 *
 *	11/30/88 - Kevin Handy
 *		Modified to close library if open fails.
 *
 *	06/16/89 - Kevin Handy
 *		Modified so would work in sharable library.
 *
 *	03/14/92 - Kevin Handy
 *		Clean up vars (checkvar)
 *
 *	07/30/93 - Kevin Handy
 *		Converted to C.
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

/*
 * Include files
 */
#include <stdio.h>
#include <descrip.h>
#include <string.h>
#include "func_include:library.h"

/*
 * Local variables
 */
static $DESCRIPTOR(tlb_text, ".TLB");

/*
 * Main function
 */
long libr_delete(struct dsc$descriptor *lib_name,
	struct dsc$descriptor *key_name)
{
	/*
	 * Local variables
	 */
	long txrfa[2];
	long status;
	long retstatus = 1;
	unsigned long lr_index;

	/*
	 * Set up the control structure if necessary
	 */
	status = lbr$ini_control(&lr_index, &LBR$C_UPDATE);

	if ((status & 1) == 0)
	{
		return(status);
	}

	/*
	 * Open the library function
	 */
	status = lbr$open(&lr_index, lib_name, 0l, &tlb_text);

	if ((status & 1) == 0)
	{
		retstatus = status;
		goto CloseLibrary;
	}


	/*
	 * Delete key if it already exists
	 */
	status = lbr$lookup_key(&lr_index, key_name, &txrfa);

	if ((status & 1) == 0)
	{
		retstatus = status;
		goto CloseLibrary;
	}

	/*
	 * Delete the primary key
	 */
	retstatus = lbr$delete_key(&lr_index, key_name);

	if ((retstatus & 1) == 1)
	{
		/*
		 * Delete the data
		 */
		lbr$delete_data(&lr_index, &txrfa);
	}

	/*
	 * Close library file
	 */
 CloseLibrary:
	lbr$close(&lr_index);

	return(retstatus);
}
