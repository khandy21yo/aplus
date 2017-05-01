/*
 *	%TITLE "entr_3messagenewwindow - Display Message And Pause At Bottom Of Screen"
 */
#pragma module entr_3messagenewwindow "V3.6 Calico"

/*
 * COPYRIGHT (C) 1987 BY
 * Computer Management Center
 *
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
 *	This subroutine will display a message at the bottom
 *	of the screen, and pause for user input.  This routine
 *	differs from entr_3messagenewwindow in that it creates its own new
 *	SMG_MESSAGE window for its own use and deletes them when
 *	this routine is exited.
 *
 * Index:
 *
 *	Entry>Messagenewwindow
 *	Messagenewwindow>Entry
 *
 * Parameters:
 *
 *	MESG$ - The passed message that the user wants to display.
 *
 *	FLAG%
 *	.table
 *		1 - Don't ask for continuence
 *		2 - Wide menu (132 columns)
 *		4 - Print message on line 23 instead of 24
 *			for long messages.
 *		8 - Dont erase the message (when bit 1 = 0)
 *	       16 - Display mesasage Please wait
 *	.endtable
 *
 *
 *	The return is a string message that is displayed at the 
 *	bottom of the screen.
 *
 * Example:
 *
 *	entr_3messagenewwindow("HI",2%)
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:entr_3messagenewwindow/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP entr_3messagenewwindow
 *	$ DELETE entr_3messagenewwindow.OBJ;*
 *
 * AUTHOR:
 *
 *	07/09/85 - Kevin Handy
 *
 * MODIFICATION HISTORY:
 *
 *	08/16/85 - Kevin Handy
 *		Skip out if scope->CH% = -1
 *
 *	02/03/87 - Kevin Handy
 *		Modified for SMG
 *
 *	08/05/88 - Frank Starman
 *		Add Flag 16%
 *
 *	12/21/88 - B. Craig Larsen
 *		Converted to 'C'
 *
 *	04/17/89 - Kevin Handy
 *		Fixed bug in parameter passing to smg (by value instead of
 *		by reference)
 *
 *	12/20/89 - Kevin Handy
 *		Fixed bug calling entr_4entry.
 *
 *	01/26/90 - Kevin Handy
 *		Fixed bug calling entr_4specialkeys that caused help to
 *		die a horrible death.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, smg$routines.h)
 *--
 */


/*
 * Include files
 */
#include <smgdef.h>
#include <descrip>
#include <smg$routines.h>
#include "func_include:cmcfun.h"


void entr_3messagenewwindow(struct scope_struct *scope, 
	struct dsc$descriptor *mesg, long int *flag)
{
	long int the_line, c_width, old_message;
	long int smg_status;
	long int junk;
	long int zero = 0L, one = 1L;
	int junkint;

	struct dsc$descriptor_s dsc_mesg;

	/*
	 * Save the MESSAGE virtual display
	 */
	old_message = scope->smg_message;
	smg$create_virtual_display( &2L, &132L, &(scope->smg_message) );
	smg$paste_virtual_display( &(scope->smg_message), &(scope->smg_pbid),
		&23L, &one );

	/*
	 * Get the current pasteboard width
	 */
	smg$change_pbd_characteristics(&(scope->smg_pbid), 0L, &c_width);


	/*
	 * Display message
	 */
	if (*flag & 4)
		the_line = 1;
	else
		the_line = 2;

	smg$begin_display_update(&(scope->smg_message));

	smg$erase_display(&(scope->smg_message));

	smg$put_chars(
		&(scope->smg_message), 	/* Window */
		mesg,	 		/* Message to display */
		&the_line, 		/* Line */
		&one,			/* Column */
		&zero,
		&SMG$M_BOLD		/* Attributes */
	);


	/*
	 * Exit unless 'Press <RETURN> to continue'
	 */
	if (*flag & 1)
	{
		if (*flag & 16)
		{
			dsc_mesg.dsc$a_pointer = "Please wait.";
			dsc_mesg.dsc$w_length = 12;
			/* dsc_mesg.dsc$w_length = strlen(dsc_mesg.dsc$a_pointer); */
			dsc_mesg.dsc$b_class = DSC$K_CLASS_S;
			dsc_mesg.dsc$b_dtype = DSC$K_DTYPE_T;

			junk = SMG$M_BLINK + SMG$M_BOLD;

			smg$put_chars(&(scope->smg_message),
				&dsc_mesg,
				&one, &one, &one,
				&junk);
		}
		smg$end_display_update(&(scope->smg_message));
	}
	else
	{
		/***************************************************
		 * Query user
		 ***************************************************/

		/*
		 * Paint press message
		 */
		junk = c_width - 30L;

		dsc_mesg.dsc$a_pointer ="  Press <RETURN> to continue ";
		dsc_mesg.dsc$w_length = 29;
		/* dsc_mesg.dsc$w_length = strlen(dsc_mesg.dsc$a_pointer); */
		dsc_mesg.dsc$b_class = DSC$K_CLASS_S;
		dsc_mesg.dsc$b_dtype = DSC$K_DTYPE_T;

		smg$put_chars(
			&(scope->smg_message), 	/* Window */
			&dsc_mesg,		/* Message to display */
			&2L,			/* Line */
			&junk,			/* Column */
			&zero,			/* Don't Erase screen */
			&SMG$M_BOLD		/* Attributes */
		);

		smg$end_display_update(&(scope->smg_message));

		/*
		 * Get input
		 */
		junkint = 0;
		while (junkint == 0)
		{
			junkint = entr_4entry(scope, scope->smg_message, 0);
			junkint = entr_4specialkeys(scope, scope->smg_message, 0, junkint);
		}

		if ((junkint < 32) || (junkint > 254) || (junkint > 126) && (junkint < 160))
		{
			scope->scope_exit = junkint;
		}
		else
		{
			scope->scope_exit = 0;
		}

		if ((*flag & 8) == 0)
			smg$erase_display(&(scope->smg_message));
	}

	/*
	 * Delete all displays
	 */
	smg$pop_virtual_display( &(scope->smg_message), &(scope->smg_pbid) );
	scope->smg_message = old_message;

}
