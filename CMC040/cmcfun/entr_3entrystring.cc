/*	%TITLE "ENTR_3ENTRYSTRING - Subroutine to Enter Data From Scope/KB"
 */
#pragma module entr_3entrystring "V3.6 Calico"

/*
 * Include files
 */
#include <stdio.h>
#include <stdlib.h>

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "smg/smg.h"

/*
 *
 * Abstract:
 *
 *	This function enters information from the screen
 *	using block mode entry.  It will also return in
 *	the variable SCOPE.EXIT% the exit status, as
 *	defined by the FUNC__SCOSEQ function.
 *
 *	This function is much line ENTR__3ENTRY except that
 *	it tries to enter more than one character at a time.
 *
 * Parameters:
 *
 *	SCOPE
 *		Structure created during window initilization.
 *
 *	SMG_OPTION%
 *		The passed window number being used for input.
 *
 *	XFLAG%
 *		A flag word as follows:
 *		.table
 *		   8 - Use a timeout
 *
 *		  16 - Convert to upper case
 *
 *		 256 - Display cursor on exit from help, etc.
 *
 *		1024 - Don't handle help, interrupt in function.
 *		.endtable
 *
 *	XLEN%
 *
 *	RETSTRING$
 *		through the function is the character(s), if
 *		any, that were typed.
 *
 *	SCOPE.EXIT% (passed through the SCOPE.COM definitons)
 *		contains the character that terminated the input.
 *		It contains 12 or 0 as the default normal character
 *		typed.
 *
 * Author:
 *
 *	05/29/90 - Kevin Handy
 *
 *--
 */

long entr_3entrystring(
	scope_struct &scope,
	smg_display_id &smg_option,
	long xflag,
	long xlen,
	std::string &retstring)
{
	short int retchar;		/* MUST be a word */
	long timeout;
	long value;
	long smg_status;
	long modifier;
	std::string term_set = "\377\377\377\377\0\0\0\0\0\0\0\0\0\0\0\200");
	long data_length;
	char addbyte = ' ';
	std::string addstring = addbyte;
	addstring.dsc$b_dtype = DSC$K_DTYPE_T;

	/*
	 * Set the timeout if requested else set to zero
	 */
	if ((*xflag) & 8)
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

	/*
	 * Get input until terminater
	 *	256 = cvtlow
	 *	512 = nofiltr
	 *	4096 = trmnoecho
	 *	32768 = noedit
	 *	65536 = norecall
	 */
	modifier = 512 + 4096 + 32768 + 65536;

	if ((*xflag) & 16)
	{
		modifier = modifier | 256;
	}

l6000:
	if (timeout != 0)
	{
		smg_status = smg$read_string(
			&(scope->smg_kbid),	/* Keyboard ID */
			retstring,		/* Returned string */
			0L,			/* Prompt */
			xlen,
			&modifier,
			&timeout,		/* Time out */
			&term_set,
			&data_length,
			&retchar,		/* Return terminator */
			smg_option
		);
	}
	else
	{
		smg_status = smg$read_string(
			&(scope->smg_kbid),	/* Keyboard ID */
			retstring,		/* Returned string */
			0L,			/* Prompt */
			xlen,
			&modifier,
			0L,		/* Time out */
			&term_set,
			&data_length,
			&retchar,		/* Return terminator */
			smg_option
		);
	}

/*???	DATA_STRING$ = LEFT(DATA_STRING$, DATA_LENGTH%) ???*/

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
		(retchar == SMG$K_TRM_UNKNOWN))
	{
		goto l6000;
	}

	if (retchar == SMG$K_TRM_BUFFER_FULL)
	{
		retchar = 12;
	}

	retchar = func_4scoseq(retchar);

	if ((retchar >= 32) && (retchar < 127))
	{
		*(addstring.dsc$a_pointer) = retchar;
		str$append(retstring, &addstring);
		retchar = 12;
	}

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
			smg_option,		/* Window */
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
			case SMG$K_TRM_KP7:
			case SMG$K_TRM_CTRLI:
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
/*				goto l6000; */
		}
	}

 ExitFunction:
	return(retchar);
}
