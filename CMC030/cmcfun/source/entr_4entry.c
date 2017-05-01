/*	%TITLE "entr_4entry - Subroutine to Enter Data From Scope/KB"
 */
#pragma module entr_4entry "V3.6 Calico"

/*
 *
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
 * Abstract:
 *
 *	This function enters information from the screen
 *	using block mode entry.  It will also return in
 *	the variable scope.scope_exit% the exit status, as
 *	defined by the FUNC_SCOSEQ function.
 *
 * Parameters:
 *
 *	smg_option%
 *		The passed window number being used for input.
 *
 *	XFLAG% 
 *		A flag word as follows:
 *	.table
 *	 	  8 - Use a timeout
 *
 *		 256 - Display cursor on exit from help, etc.
 *	.endtable
 *
 *	scope.scope_exit% (passed through the SCOPE.COM definitons)
 *	contains the character that terminated the input.
 *	It contains 0 as the default normal character typed.
 *
 *	Returned through the function is the character(s), if
 *	any, that were typed.
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:entr_4entry/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP entr_4entry
 *	$ DELETE entr_4entry.OBJ;*
 *
 * Author:
 *
 *	06/20/85 - Kevin Handy
 *
 *	09/24/89 - Kevin Handy
 *		Taken from ENTR_ENTRY and greatly modified so that
 *		the function does not call out when special keys
 *		are typed.  That is now handled by ENTR_4SPECAILKEYS.
 *
 * Modification history:
 *
 *	10/10/86 - B. Craig Larsen
 *		Added a timeout option using (XFLAG% AND 8%) to 
 *		indicate a timeout
 *
 *	02/03/87 - Kevin Handy
 *		Modified for SMG routines
 *
 *	06/24/87 - Kevin Handy
 *		Modified for a gold key, so that VT100 users
 *		could make use of all of the features
 *
 *	07/30/87 - Kevin Handy
 *		Removed two garbage parameters.  Added XFLAG%
 *		value of 256.
 *
 *	09/15/87 - Kevin Handy
 *		Fixed timeout operation so it doesn't crash.
 *
 *	10/05/87 - Kevin Handy
 *		Modified to ignore partial escape error.
 *
 *	10/07/87 - Kevin Handy
 *		Modified to call interupt menu instead of
 *		only spawning menu.
 *
 *	01/11/88 - Kevin Handy
 *		Modified to allow Gold-4 (Bottom) and Gold-5 (top)
 *		(numbers on numeric keypad) to work as it does in EDT.
 *
 *	02/03/88 - Kevin Handy
 *		Added FLAG% of 1024, in an attempt to speed menu.
 *
 *	02/04/88 - Kevin Handy
 *		Modified so timeout returns SMG$K_TRM_TIMEOUT
 *		instead of SMG$K_TRM_F10.
 *
 *	07/11/88 - Kevin Handy
 *		Added <Gold/7> as an interrupt key.
 *
 *	08/12/89 - Kevin Handy
 *		Fixed problem where func_scoseq was not always
 *		returning usefull information.
 *
 *	09/30/92 - Kevin Handy
 *		Modified to be able to return any gold/? function
 *		key, instead of ignoring them.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	10/07/98 - Kevin Handy
 *		Allow 'Control-G/Control-x' as well as the 'Control-G/x'
 *		key combinations. Maybe I'll get fewer complaints from
 *		PC users with this change.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, descrip.h, smg$routines.h, retchar, stdlib.h)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <stdlib.h>
#include <descrip.h>
#include <smg$routines.h>
#include <smgdef.h>

#include "func_include:cmcfun.h"

long entr_4entry(struct scope_struct *scope, long smg_option, long xflag)
{
	short int retchar;		/* MUST be a word */
	long timeout;
	long value;
	long smg_status;

	/*
	 * Set the timeout if requested else set to zero
	 */
	if (xflag & 8)
	{
		if (scope->scope_timeout < 1)
		{
			timeout = 121;
		}
		else
		{
			timeout = scope->scope_timeout + 1;
		}
	}
	else
	{
		timeout = 0;
	}

	if (scope->macroflag > 2)
	{
		goto ExitFunction;
	}
	

	/*
	 * Get input character
	 */
 l6000:
	if (timeout != 0)
	{
		smg_status = smg$read_keystroke(
			&(scope->smg_kbid),	/* Keyboard ID */
			&retchar,		/* Return character */
			0L,			/* Prompt */
			&timeout,		/* Time out */
			&smg_option,		/* Window */
			0L,			/* Attribute */
			0L			/*   "   " */
		);
	}
	else
	{
		smg_status = smg$read_keystroke(
			&(scope->smg_kbid),	/* Keyboard ID */
			&retchar,		/* Return character */
			0L,			/* Prompt */
			0L,			/* Time out */
			&smg_option,		/* Window */
			0L,			/* Attribute */
			0L			/*   "   " */
		);
	}

	/*
	 * Handle odd return values
	 */
	switch (smg_status)
	{
		/*
		 * Partial escape
		 */
		case 508:
			goto l6000;

		/*
		 * Handle Timeout
		 */
		case 556:
			/*
			 * Make it look like a exit key was pressed
			 */
			retchar = SMG$K_TRM_TIMEOUT;
			smg_status = 1;
			break;

		/*
		 * Handle control/Z
		 */
		case 1213442L:
			retchar = 26L;
			smg_status = 1;
			break;
	}

	/*
	 * Handle any errors
	 */
	if ((smg_status & 1) == 0)
	{
		switch (smg_status)
		{
			case 60L:
			case 2104L:
				goto l6000;

			default:
				printf("Input error (ENTRY): %ld\n", smg_status);
				abort();
		}
	}

	if ((retchar == SMG$K_TRM_CANCELLED) ||
		(retchar == SMG$K_TRM_BUFFER_FULL) ||
		(retchar == SMG$K_TRM_UNKNOWN))
	{
		goto l6000;
	}

	retchar = func_4scoseq(retchar);

	/*
	 * Handle the Gold-Key redefinitions
	 */
	if ((retchar == SMG$K_TRM_PF1) || (retchar == 7))
	{
		smg_status == smg$read_keystroke(
			&(scope->smg_kbid),	/* Keyboard ID */
			&retchar,		/* Return character */
			0,			/* Prompt */
			0,			/* Time out */
			&smg_option,		/* Window */
			0L,			/* Attribute */
			0L			/*   "   " */
		);

		switch (retchar)
		{
			/*
			 * Interrupt (F6), Gold-I
			 */
			case 'I':
			case 'i':
			case SMG$K_TRM_CTRLI:
			case SMG$K_TRM_KP7:
				retchar = SMG$K_TRM_F6;
				break;

			/*
			 * Resume (F7), Gold-R
			 */
			case 'R':
			case 'r':
			case SMG$K_TRM_CTRLR:
				retchar = SMG$K_TRM_F7;
				break;

			/*
			 * Cancel (F8), Gold-C
			 */
			case 'C':
			case 'c':
			case SMG$K_TRM_CTRLC:
				retchar = SMG$K_TRM_F8;
				break;

			/*
			 * Main screen, (F9), Gold-M
			 */
			case 'M':
			case 'm':
			case SMG$K_TRM_CTRLM:
				retchar = SMG$K_TRM_F9;
				break;

			/*
			 * eXit (F10), Gold-X
			 */
			case 'X':
			case 'x':
			case SMG$K_TRM_CTRLX:
				retchar = SMG$K_TRM_F10;
				break;

			/*
			 * List choices (F14), Gold-L
			 */
			case 'L':
			case 'l':
			case SMG$K_TRM_CTRLL:
				retchar = SMG$K_TRM_F14;
				break;

			/*
			 * Help (Help, F15), Gold-H
			 */
			case 'H':
			case 'h':
			case SMG$K_TRM_CTRLH:
				retchar = SMG$K_TRM_HELP;
				break;

			/*
			 * Do (Do, F16), Gold-D
			 */
			case 'D':
			case 'd':
			case SMG$K_TRM_CTRLD:
				retchar = SMG$K_TRM_DO;
				break;

			/*
			 * Magic key (F17), Gold-*
			 */
			case '*':
				retchar = SMG$K_TRM_F17;
				break;

			/*
			 * Top key (F18), Gold-T, Gold-5(num)
			 */
			case 'T':
			case 't':
			case SMG$K_TRM_KP5:
			case SMG$K_TRM_CTRLT:

				retchar = SMG$K_TRM_F18;
				break;

			/*
			 * Bottom key (F19), Gold-B, Gold-4(num)
			 */
			case 'B':
			case 'b':
			case SMG$K_TRM_KP4:
			case SMG$K_TRM_CTRLB:
				retchar = SMG$K_TRM_F19;
				break;

			/*
			 * Find (Find), Gold-F
			 */
			case 'F':
			case 'f':
			case SMG$K_TRM_CTRLF:
				retchar = SMG$K_TRM_FIND;
				break;

			/*
			 * Insert here, Gold-+
			 */
			case '+':
				retchar = SMG$K_TRM_INSERT_HERE;
				break;

			/*
			 * Remove, Gold--
			 */
			case '-':
				retchar = SMG$K_TRM_REMOVE;
				break;

			/*
			 * Select, Gold-S
			 */
			case 'S':
			case 's':
			case SMG$K_TRM_CTRLS:
				retchar = SMG$K_TRM_SELECT;
				break;

			/*
			 * Prev-screen, Gold-P
			 */
			case 'P':
			case 'p':
			case SMG$K_TRM_CTRLP:
				retchar = SMG$K_TRM_PREV_SCREEN;
				break;

			/*
			 * Next screen, Gold-N
			 */
			case 'N':
			case 'n':
			case SMG$K_TRM_CTRLN:
				retchar = SMG$K_TRM_NEXT_SCREEN;
				break;

			/*
			 * Dead key, Gold-<space>
			 */
			case ' ':
				goto l6000;

			/*
			 * Otherwise bad key
			 */
			default:
				retchar = -retchar;
				break;
		}
	}

 ExitFunction:
	return(retchar);
}
