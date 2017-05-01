/*	%TITLE "Exit from a Program"
 */
#pragma module subr_3exitprogram "V3.6 Calico"

/*
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
 *	This function is used to exit a program.
 *	It closes out all necessary SMG stuff, and executes
 *	either a given program or goes to menu.
 *
 * Input:
 *
 *	OPT_DOCOMMAND
 *		The passed command the user enters to enter another
 *		program.
 *
 *	OPT_CORECOMMON
 *		The passed string the user enters to pass to the
 *		core common.
 *
 *
 *	Returned value
 *		It closes out all necessary information, and goes
 *		to either a given program or to the menu.
 *
 * Example:
 *
 *	CALL SUBR_EXIT("",CORE$)
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:SUBR_3EXITPROGRAM
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP SUBR_3EXITPROGRAM
 *	$ DELETE SUBR_3EXITPROGRAM.OBJ;*
 *
 * AUTHOR:
 *
 *	03/06/87 - Kevin Handy
 *
 * MODIFICATION HISTORY:
 *
 *	12/22/89 - Kevin Handy
 *		Threw in sharable library.
 *
 *	03/27/90 - Frank F. Starman
 *		Remove SMG$ERASE_PASTEBOARD
 *		Change argument in ENTR_3MESSAGE to 1%+16%
 *
 *	07/16/91 - Kevin Handy
 *		Unwound external definitions.
 *
 *	07/22/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, stdlib.h, smg$routines.h)
 *--
 */

/*
 * Include files
 */
#include <stdlib.h>
#include <descrip.h>
#include <string.h>
#include <smg$routines.h>
#include <smgdef.h>
#include "func_include:cmcfun.h"

/*
 * External functions
 */
long lib$set_symbol();
long lib$do_command();

/*
 * Local variables
 */
static $DESCRIPTOR(p1,"");
static $DESCRIPTOR(p2,"CMC$CORECOMMON");
static $DESCRIPTOR(p3,"MENU");

/*
 * Main function
 */
void subr_3exitprogram(struct scope_struct *scope,
	struct dsc$descriptor *opt_docommand,
	struct dsc$descriptor *opt_corecommon)
{
	/*
	 * Set up 'core' common
	 */
	lib$set_symbol(&p2, opt_corecommon);

	smg$pop_virtual_display(&scope->smg_option, &scope->smg_pbid);

	/*
	 * Print message to Please Wait
	 */
	entr_3message(scope, &p1, &17l);

	/*
	 * Re-establish cursor
	 */
	smg$set_cursor_mode(&scope->smg_pbid, &0l);

	/*
	 * Exit out by doing a DCL command
	 */
	if (opt_docommand->dsc$w_length == 0)
	{
		lib$do_command(&p3);
	}
	else
	{
		lib$do_command(opt_docommand);
	}

	/*
	 * If all else fails, crash and burn
	 */
	exit(EXIT_SUCCESS);
}
