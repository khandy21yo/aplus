/*
 *	%TITLE "subr_3spawn - Function to Spawn A New Job In Foreground"
 */
#pragma module subr_3spawn "V3.6 Calico"

/*
 *		COPYRIGHT (C) 1986 BY
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
 * Abstract:
 *
 *	Function to spawn a new process
 *
 * Parameters:
 *
 *	PNAME$ 
 *		Passed command/program to execute.
 *
 *	Returned value
 *		A value to spawn a new process.
 * 
 * Example:
 *
 *	subr_3spawn("TEMP.COM")
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:subr_3spawn/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP subr_3spawn
 *	$ DELETE subr_3spawn.OBJ;*
 *
 * Author:
 *
 *	Kevin Handy
 *
 * Modification history:
 *
 *	07/30/87 - Kevin Handy
 *		Modified to clear the screen on spawn, reset
 *		scrolling regions on return, make cursor visable
 *		in spawn.
 *
 *	10/13/87 - Kevin Handy
 *		Modified to not have a prompt, since this is now
 *		called through an interrupt menu.
 *
 *	03/26/91 - Frank F. Starman
 *		Remember menu path before spawn, blank it and put it 
 *		back after spawn, then crash the program.
 *
 *	04/17/91 - Kevin Handy
 *		Disable the changes made as of 03/26/91 so that
 *		programs would not crash and die.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	05/28/99 - Kevin Handy
 *		Modified to compile with DEC-C
 *		(module, string.h, smg$routines.h, lib$routines.h)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <string.h>
#include <descrip>
#include <smgdef.h>
#include <smg$routines.h>
#include <lib$routines.h>

#include "func_include:cmcfun.h"

extern read_3broadcast();

/*
 * Define sume necessary hard-coded text descriptor strings
 */
static $DESCRIPTOR(dsc_subname, "Subjob$ ");
static $DESCRIPTOR(dsc_mesg1, "SORRY! I am unable to spawn a new job!");
static $DESCRIPTOR(dsc_mesg2, "You may have too many spawned jobs.");
static $DESCRIPTOR(dsc_menupath, "CMC$MENUPATH");


void subr_3spawn(struct scope_struct *scope, struct dsc$descriptor *pname)
{
	long one = 1;
	long sys_status, smg_spawn;
	long hide_vdid;

	struct dsc$descriptor_s dsc_mesg,dsc_emtstr,dsc_pathsymbol;

	char temp_ident[4];
	char temp_program[40];
	char temp_item[16];

	/*
	 * Set up the DCL command to execute
	 */
	strncpy(temp_ident, scope->prg_ident, sizeof(scope->prg_ident));
	strncpy(temp_program, scope->prg_program, sizeof(scope->prg_program));
	strncpy(temp_item, scope->prg_item, sizeof(scope->prg_item));

	strncpy(scope->prg_ident, "H", sizeof(scope->prg_ident));
	strncpy(scope->prg_program, "SUBR_3SPAWN",
		sizeof(scope->prg_program));
	strncpy(scope->prg_item, "HELP", sizeof(scope->prg_item));

	/*
	 * Tell them what is going on
	 */
	smg$set_cursor_mode(&(scope->smg_pbid), &one);

	/*
	 * Clear the screen
	 */
	smg$create_virtual_display(&24L, &132L, &hide_vdid);
	smg$paste_virtual_display(&hide_vdid, &(scope->smg_pbid), &one, &one);

	/*
	 * Make a valiant effort to create detached job
	 */
	smg$disable_broadcast_trapping( &(scope->smg_pbid) );
	smg$set_cursor_mode(&(scope->smg_pbid), &0L);

	dsc_emtstr.dsc$w_length = 0;
	dsc_emtstr.dsc$a_pointer = "";
	dsc_emtstr.dsc$b_class = DSC$K_CLASS_S;
	dsc_emtstr.dsc$b_dtype = DSC$K_DTYPE_T;

/*	lib$get_symbol(&dsc_menupath, &dsc_pathsymbol,0L,0L); */
/*	sys_status = lib$set_symbol(&dsc_menupath,&dsc_emtstr); */

	sys_status = lib$spawn(pname, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
		&dsc_subname, 0L);

/*	sys_status = lib$set_symbol(&dsc_menupath,&dsc_pathsymbol); */

	smg$set_cursor_mode(&(scope->smg_pbid), &one);
	smg$set_broadcast_trapping(&(scope->smg_pbid), &read_3broadcast, scope);

	/*
	 * Couldn't create
	 */
	if ((sys_status & 1) == 0)
	{
		smg$create_virtual_display(&3L, &50L, &(smg_spawn),
			&SMG$M_BORDER, &(SMG$M_REVERSE + SMG$M_BOLD));

		smg$put_chars(&(smg_spawn), &dsc_mesg1, &one, &one, &one);
		smg$put_chars(&(smg_spawn), &dsc_mesg2, &2L, &one, &one);

		smg$delete_virtual_display(&hide_vdid);

		dsc_mesg.dsc$a_pointer = "";
		dsc_mesg.dsc$w_length = strlen(dsc_mesg.dsc$a_pointer);
		dsc_mesg.dsc$b_class = DSC$K_CLASS_S;
		dsc_mesg.dsc$b_dtype = DSC$K_DTYPE_T;
		entr_3message(scope, &dsc_mesg, &0L);

		smg$delete_virtual_display(&(smg_spawn));
		goto ExitSub1;
	}

	/*
	 * Return from normal create
	 */
	smg$begin_pasteboard_update(&(scope->smg_pbid));
	smg$delete_virtual_display(&hide_vdid);
	smg$repaint_screen(&(scope->smg_pbid));
	smg$end_pasteboard_update(&(scope->smg_pbid));

 ExitSub1:
	strncpy(scope->prg_ident, temp_ident, sizeof(scope->prg_ident));
	strncpy(scope->prg_program, temp_program, sizeof(scope->prg_program));
	strncpy(scope->prg_item, temp_item, sizeof(scope->prg_item));
}
