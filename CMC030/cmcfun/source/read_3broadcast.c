/*	%TITLE "Handle Broadcast Trapping."
 */
#pragma module read_3broadcast "V3.6 Calico"

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
 * Parameters:
 *
 *	The input is five parameters passed through by the
 *	AST trapping routine.
 *
 * Example:
 *
 *	DO NOT CALL AS A NORMAL SUB.  This function is
 *	special and called by AST's.
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:READ_3BROADCAST
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP READ_3BROADCAST
 *	$ DELETE READ_3BROADCAST.OBJ;*
 *
 *
 * Author:
 *
 *	09/01/87 - Kevin Handy
 *
 * Modification history:
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, str$routines.h, smg$routines.h)
 *--
 */

/*
 * Include files
 */
#include <descrip.h>
#include <smgdef.h>
#include "func_include:cmcfun.h"
#include <str$routines.h>
#include <smg$routines.h>

/*
 * Main function
 */
void read_3broadcast(struct scope_struct *scope,
	long *a2, long *a3, long *a4, long *a5)
{
	long status;

	struct dsc$descriptor text;

	/*
	 * Clear out buffering
	 */
	smg$flush_buffer(&scope->smg_pbid);

	/*
	 * Create string variable to work with
	 */
	text.dsc$a_pointer = 0;
	text.dsc$w_length = 0;
	text.dsc$b_class = DSC$K_CLASS_D;
	text.dsc$b_dtype = DSC$K_DTYPE_T;

	/*
	 * Read broadcast messages
	 */
	status = smg$get_broadcast_message(&scope->smg_pbid, &text);

	while (status == 1)
	{
		/*
		 * Write broadcast messages
		 */
		smg$put_chars(&scope->smg_message, &text, &1l, &1l);
		status = smg$get_broadcast_message(&scope->smg_pbid, &text);
	}

	/*
	 * Deallocate string
	 */
	str$free1_dx(&text);
}
