//!
//!	%TITLE "entr_3messagenewwindow - Display Message And Pause At Bottom Of Screen"
//!/
#pragma module entr_3messagenewwindow "V3.6 Calico"

/*
 * Include files
 */
#include <string>

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "smg/smg.h"

//!
//!
//! ABSTRACT:
//!
//!	This subroutine will display a message at the bottom
//!	of the screen, and pause for user input.  This routine
//!	differs from entr_3messagenewwindow in that it creates its own new
//!	SMG_MESSAGE window for its own use and deletes them when
//!	this routine is exited.
//!
//!
//! Parameters:
//!
//!	MESG$ - The passed message that the user wants to display.
//!
//!	FLAG%
//!	.table
//!		1 - Don't ask for continuence
//!		2 - Wide menu (132 columns)
//!		4 - Print message on line 23 instead of 24
//!			for long messages.
//!		8 - Dont erase the message (when bit 1 = 0)
//!	       16 - Display mesasage Please wait
//!	.endtable
//!
//!
//!	The return is a string message that is displayed at the 
//!	bottom of the screen.
//!
//! Example:
//!
//!	entr_3messagenewwindow("HI",2%)
//!
//! AUTHOR:
//!
//!	07/09/85 - Kevin Handy
//!
//!
void entr_3messagenewwindow(
	scope_struct &scope, 
	const std::string &mesg, 
	long int flag)
{
	long int the_line, c_width;
	smg_display_id old_message;
	long int smg_status;
	long int junk;
	long int zero = 0L, one = 1L;
	int junkint;

	std::string dsc_mesg;

	/*
	 * Save the MESSAGE virtual display
	 */
	old_message = scope.smg_message;
	smg$create_virtual_display(2, 132, scope.smg_message);
	smg$paste_virtual_display(scope.smg_message,
		scope.smg_pbid,
		23, one);

	/*
	 * Get the current pasteboard width
	 */
	smg$change_pbd_characteristics(scope.smg_pbid, 0, &c_width);


	/*
	 * Display message
	 */
	if (flag & 4)
	{
		the_line = 1;
	}
	else
	{
		the_line = 2;
	}

	smg$begin_display_update(scope.smg_message);

	smg$erase_display(scope.smg_message);

	smg$put_chars(
		scope.smg_message, 	/* Window */
		mesg,	 		/* Message to display */
		the_line, 		/* Line */
		one,			/* Column */
		zero,
		SMG$M_BOLD		/* Attributes */
	);


	/*
	 * Exit unless 'Press <RETURN> to continue'
	 */
	if (flag & 1)
	{
		if (flag & 16)
		{
			dsc_mesg = "Please wait.";

			junk = SMG$M_BLINK + SMG$M_BOLD;

			smg$put_chars(scope.smg_message,
				dsc_mesg,
				one, one, one,
				junk);
		}
		smg$end_display_update(scope.smg_message);
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

		dsc_mesg ="  Press <RETURN> to continue ";

		smg$put_chars(
			scope.smg_message, 	/* Window */
			dsc_mesg,		/* Message to display */
			2,			/* Line */
			junk,			/* Column */
			zero,			/* Don't Erase screen */
			SMG$M_BOLD		/* Attributes */
		);

		smg$end_display_update(scope.smg_message);

		/*
		 * Get input
		 */
		junkint = 0;
		while (junkint == 0)
		{
			junkint = entr_4entry(scope, scope.smg_message, 0);
			junkint = entr_4specialkeys(scope, scope.smg_message, 0, junkint);
		}

		if ((junkint < 32) || (junkint > 254) || ((junkint > 126) && (junkint < 160)))
		{
			scope.scope_exit = junkint;
		}
		else
		{
			scope.scope_exit = 0;
		}

		if ((flag & 8) == 0)
			smg$erase_display(scope.smg_message);
	}

	/*
	 * Delete all displays
	 */
	smg$pop_virtual_display(scope.smg_message, scope.smg_pbid );
	scope.smg_message = old_message;

}
