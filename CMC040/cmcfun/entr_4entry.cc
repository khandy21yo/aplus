//! \file
//! \brief entr_4entry - Subroutine to Enter Data From Scope/KB
//!
#pragma module entr_4entry "V3.6 Calico"

/*
 * Include files
 */
#include <string>
#include <cstdlib>

#include "preferences.h"
#include "cmcfun.h"
#include <smg/smg.h>
#include "scopedef.h"

//!
//!! \brief entr_4entry - Subroutine to Enter Data From Scope/KB
//!
//!	This function enters information from the screen
//!	using block mode entry.  It will also return in
//!	the variable scope.scope_exit% the exit status, as
//!	defined by the FUNC_SCOSEQ function.
//!
//! Parameters:
//!
//!	scope.scope_exit% (passed through the SCOPE.COM definitons)
//!	contains the character that terminated the input.
//!	It contains 0 as the default normal character typed.
//!
//! \returns Returned through the function is the character(s), if
//!	any, that were typed.
//!
//! \autjor 06/20/85 - Kevin Handy
//!
//! \author 09/24/89 - Kevin Handy
//!		Taken from ENTR_ENTRY and greatly modified so that
//!		the function does not call out when special keys
//!		are typed.  That is now handled by ENTR_4SPECAILKEYS.
//!
long entr_4entry(
	scope_struct &scope,
		//< scope strucyure
	smg_display_id &smg_option,
		//!> The passed window number being used for input.
	long xflag)
		//!< XFLAG% 
		//!		A flag word as follows:
		//!
		//!	 	8 - Use a timeout
		//!		256 - Display cursor on exit from help, etc.
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
		if (scope.scope_timeout < 1)
		{
			timeout = 121;
		}
		else
		{
			timeout = scope.scope_timeout + 1;
		}
	}
	else
	{
		timeout = 0;
	}

	if (scope.macroflag > 2)
	{
		goto ExitFunction;
	}
	

	/*
	 * Get input character
	 */
 l6000:
	if (timeout != 0)
	{
		retchar = getch();
	}
	else
	{
		retchar = getch();
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
	 *  (Will be 0 for now)
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
		retchar = getch();

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
