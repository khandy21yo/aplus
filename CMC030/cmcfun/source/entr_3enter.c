/*	%TITLE "Enter (on Screen) with Edit"
 */
#pragma module entr_3enter "V3.6 Calico"

/*
 *
 *	COPYRIGHT (C) 1986 BY
 *	Computer Management Center, Idaho Falls, Idaho.
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies thereof may not be provided or otherwise made
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
 * Abstract:HELP
 *	.b
 *	.lm +5
 *	This function is used to enter/edit a string on the screen
 *	using the cursor movement keys (left and right) to
 *	position the cursor inside of the string.
 *	.lm -5
 *
 * Index:
 *
 * Parameter:
 *
 *	SCOPE
 *		CMC Environment structure.
 *
 *	XX_VDID
 *		Creates or deletes the window that holds the string.
 *
 *	CPOSY
 *		The Y position (Horozontal) at which to start the entry.
 *
 *	CPOSX
 *		The X position (Vertical) at which to start the entry.
 *
 *	XSTR
 *		The string passed between the program and the user.
 *		The program passes a default, and the user can edit it.
 *
 *	START
 *		The passed variable that tells where to position the
 *		cursor. (-1 defaults to 0).
 *
 *	FLAG
 *		.table
 *		4 - Adds a forced keypunch mode
 *
 *		8 - Sets a timeout.
 *
 *		16 - Force to upper case.
 *
 *		4096 - Use blanks instead of underlines
 *		.endtable
 *
 *	Returned value
 *		Enters a string on the screen and
 *		positions the cursor inside of the string.
 *
 * Example:
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:ENTR_3ENTER
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_3ENTER
 *	$ DELETE ENTR_3ENTER.OBJ;*
 *
 * Author:
 *
 *	06/21/84 - Kevin Handy
 *
 * Modification history:
 *
 *	08/30/84 - Kevin Handy
 *		Added CTRL/U, delete line, delete word
 *
 *	06/26/86 - B. Craig Larsen
 *		Added forced keypunch mode when (FLAG AND 4) = 4
 *		and fixed the extra char bug.  Added a default key(F14)
 *		which returns the original value of the field.
 *		*****************************************************
 *		** Note ** Default key changed to Control/R, since **
 *		** Note ** F14 is defined as List Choices.	   **
 *		*****************************************************
 *
 *	10/10/86 - B. Craig Larsen
 *		FLAG AND 8 indicates a timeout requested and it is
 *		passed into ENTRY where it is taken care of.
 *
 *	02/03/87 - Kevin Handy
 *		Modified for SMG
 *
 *	06/24/87 - Kevin Handy
 *		Modified for gold key
 *
 *	07/27/87 - Kevin Handy
 *		Fixed problem with entering spaces.
 *
 *	09/15/87 - Kevin Handy
 *		Fixed problem with timeouts.
 *
 *	01/20/88 - Kevin Handy
 *		Added Control/H, Control/E to go to the front
 *		or the end of the text.
 *
 *	02/03/88 - Kevin Handy
 *		Fixed call to ENTRY so that all flag values
 *		may pass through (this for flag and 1024).
 *
 *	02/09/89 - Kevin Handy
 *		Modified to return SCOPE.EXIT as function value,
 *		and return string through default value.
 *		Also passes curser position as two integers
 *		instead of a string.
 *
 *	09/22/89 - Kevin Handy
 *		Modified for 4096 flag, in order to do away
 *		with the function ENTR_NOLSTRING.
 *		These two functions are similiar, and I didn't
 *		like having two functions that were almost
 *		identical.
 *
 *	11/15/89 - Kevin Handy
 *		Taken from ENTR_ENTER.
 *
 *	05/29/90 - Kevin Handy
 *		Modified to use ENTR_3ENTRYSTRING to be more
 *		efficient.
 *
 *	06/01/90 - Kevin Handy
 *		Fixed bug introduced in 05/29/90 that caused the
 *		space bar on the first character position to act
 *		funny.
 *
 *	08/21/90 - Frank F. Starman
 *		Add 256 to the argument in ENTR_4SPECIALKEYS function.
 *
 *	07/17/93 - Kevin Handy
 *		Converted to C.
 *
 *	07/26/93 - Kevin Handy
 *		Modified to reduce cursor flicker by reducing number
 *		of times that DsiplayText is called. (Only call if
 *		text changes)
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modified to compile in DEC-C without errors
 *		(module, ctype.h, str$routines.h, DisplayText->void,
 *		descriptors)
 *
 *	06/08/99 - Kevin Handy
 *		Use SMG$ROUTINES.H instead of homegrown header
 *--
 */

/*
 * Include files
 */
#include <descrip.h>
#include <string.h>
#include <ctype.h>
#include <str$routines.h>
/* #include "gnu_cc_include:[000000]smgfun.h" */
#include <smg$routines.h>
#include <smgdef.h>
#include "func_include:cmcfun.h"

/*
 * External functions
 */
/* int entr_4entry(); */
/* int entr_3entrystring(); */
/* int entr_4specialkeys(); */

/*
 * Prototypes
 */
static void DisplayText(char *work, int xlen, char blank_char,
	long xx_vdid, long cposy, long cposx, long cxpos);
static strinsert(char str[], int shift, int xlen);


#ifndef MSDOS
/*
 * Some useful functions that exist on MSDOS but not here
 */
static char *strnset(char *s, int ch, size_t n);
#endif


/*
 * Declare constants
 */
#define WORDMARK " ,.!?;:"


/*
 * Main entry point
 */
long entr_3enter(struct scope_struct *scope,
	long *xx_vdid, long *cposy, long *cposx,
	struct dsc$descriptor_s *xstr,
	long *start, long *flag)
{
	int xlen;
	int clearflag;
	int cxpos;
	int nocr;
	char blank_char;
	char work[132];
	int i;
	long getc;
	long junk;
	long readcount;
	struct dsc$descriptor_s xwork1;
	char work1[132];

 l500:
	/*
	 * Initilize information
	 */
	xlen = xstr->dsc$w_length;			/* Length of string */
	strncpy(work, xstr->dsc$a_pointer, xlen);
							/* Force different copy */
	clearflag = (*start == -1);			/* Zero input string? */

	if (*start == -1)				/* Cursor position */
	{
		cxpos = 0;
	}
	else
	{
		cxpos = *start;
	}

	if (*flag & 4)					/* Forced keypunch mode */
	{
		nocr = 128;
	}
	else
	{
		nocr = 0;
	}

	if (*flag & 4096)				/* Space or underline */
	{
		blank_char = ' ';
	}
	else
	{
		blank_char = '_';
	}

	/*
	 * Make the cursor appear
	 */
	smg$set_cursor_mode(&(scope->smg_pbid), &0l);

 l1000:
	/*
	 * Format text to display to user
	 */
	DisplayText(work, xlen, blank_char, *xx_vdid, *cposy, *cposx, cxpos);

 l1100:
	/*
	 * Set current cursor position
	 */
	junk = *cposx + cxpos;
	smg$set_cursor_abs(
		xx_vdid,
		cposy,
		&junk
	);

 l1110:
	/*
	 * Determine how many characters we can snatch at one time
	 */
	readcount = xlen - cxpos;

	for (junk = cxpos; junk < xlen; junk++)
	{
		if (work[junk] != ' ')
		{
			readcount = 1;
			break;
		}
	}

	if (readcount <= 1)
	{
		/*
		 * Get one character
		 */
		getc = entr_4entry(scope, *xx_vdid, (256 | *flag));

		/*
		 * Handle special characters
		 */
		if ((getc < 32) ||
			((getc >= 127) && (getc < 160)) || (getc >= 255))
		{
			/* Do Nothing (if statement backwards now) */
		}
		else
		{
			/*
			 * First character typed may blank out entire field
			 */
			if (clearflag)
			{
				/*
				 * Blank it out
				 */
				strnset(work, ' ', xlen);
			}

			/*
			 * If too many characters have been typed
			 */
			if (cxpos >= xlen)
			{
				smg$ring_bell(xx_vdid);
			}
			else
			{
				strinsert(work + cxpos, 1, xlen - cxpos);
				if (*flag & 16)
				{
					work[cxpos++] = toupper(getc);
				}
				else
				{
					work[cxpos++] = getc;
				}
			}
			getc = 0;

			/*
			 * Display changed text
			 */
			DisplayText(work, xlen, blank_char, *xx_vdid,
				*cposy, *cposx, cxpos);

		}
	}
	else
	{
		/*
		 * Allocate dynamic string descriptor
		 */
		xwork1.dsc$a_pointer = 0;
		xwork1.dsc$w_length = 0;
		xwork1.dsc$b_class = DSC$K_CLASS_D;
		xwork1.dsc$b_dtype = DSC$K_DTYPE_T;

		str$get1_dx(&readcount, &xwork1);

		/*
		 * Grab a bunch of characters
		 */
		junk = 256 | *flag;

		getc = entr_3entrystring(scope, xx_vdid,
			&junk, &readcount, &xwork1);

		readcount = xwork1.dsc$w_length;

		/*
		 * First character typed may blank out entire field
		 */
		if (clearflag != 0)
		{
			/*
			 * Blank it out
			 */
			strnset(work, ' ', xlen);
		}

		/*
		 * Insert characters into string (If non-zero)
		 */
		if (xwork1.dsc$w_length != 0)
		{
			strinsert(work + cxpos,
				readcount,
				xlen - cxpos);
			for (i = 0; i < readcount; i++)
			{
				work[cxpos++] = *(xwork1.dsc$a_pointer + i);
			}

		}

		if ((clearflag != 0) || (xwork1.dsc$w_length != 0))
		{
			/*
			 * Display changed text
			 */
			DisplayText(work, xlen, blank_char, *xx_vdid,
				*cposy, *cposx, cxpos);
		}

		/*
		 * Deallocate string
		 */
		str$free1_dx(&xwork1);
	}

	/*
	 * Handle any special cases
	 */
	if ((*flag & 1024) == 0)
	{
		getc = entr_4specialkeys(scope, *xx_vdid, (256 | *flag), getc);
	}

	/*
	 * Don't clear string next character
	 */
	clearflag = 0;

 NotAChar:
	/*
	 * Handle the exit character
	 */
	clearflag = 0;
	scope->scope_exit = getc;

	switch(getc)
	{
		/*
		 * Control/E - end of line
		 */
		case SMG$K_TRM_CTRLE:
			cxpos = xlen;
			while ((cxpos > 0) && (work[cxpos-1] == ' '))
			{
				cxpos--;
			}
			goto l1100;

		/*
		 * Control/H - Front of line
		 */
		case SMG$K_TRM_CTRLH:
			cxpos = 0;
			goto l1100;

		/*
		 * Handle TAB (beep and ignore them)
		 */
		case SMG$K_TRM_CTRLI:
			goto l1200;

		/*
		 * If it was a normal character, handle adding it to string
		 */
		case 0:
		case 12:
			if ((cxpos < xlen) || (nocr == 0))
				goto l1100;
			break;

		/*
		 * Test for left arrow
		 */
		case SMG$K_TRM_LEFT:
			if (cxpos <= 0)
				goto l1200;
			cxpos--;
			goto l1100;

		/*
		 * Test for right arrow
		 */
		case SMG$K_TRM_RIGHT:
			if (cxpos >= xlen)
				goto l1200;
			cxpos++;
			goto l1100;

		/*
		 * Delete
		 */
		case SMG$K_TRM_DELETE:
			if (cxpos > 0)
			{
				for (i = cxpos-1; i<=xlen-2; i++)
				{
					work[i] = work[i+1];
				}
				work[xlen-1] = ' ';

				cxpos--;

				DisplayText(work, xlen, blank_char, 
					*xx_vdid, *cposy, *cposx, cxpos);
			}
			goto l1100;

		/*
		 * Rub Line
		 */
		case SMG$K_TRM_F12:
		case SMG$K_TRM_CTRLU:
			for (i = cxpos; i<=xlen-1; i++)
			{
				work[i-cxpos] = work[i];
			}
			strnset(work + xlen - cxpos, ' ', cxpos + 1);
			cxpos = 0;
			goto l1000;

		/*
		 * Rub Word
		 */
		case SMG$K_TRM_F13:
			if (cxpos <= 0)
			{
				 goto l1200;
			}
			junk = cxpos - 1;
			while ((junk > 0) && (strchr(WORDMARK, work[junk]) == 0))
			{
				junk--;
			}
			for (i = junk; i<=cxpos; i++)
			{
				work[i] = work[i+cxpos-junk];
			}
			strnset(work + xlen - (cxpos - junk), ' ', cxpos - junk);

			cxpos = junk;
			goto l1000;

		/*
		 * Restore the default
		 */
		case SMG$K_TRM_CTRLR:
			goto l500;

	}

	/*
	 * Fall through to here means it is time to exit.
	 */
	strncpy(xstr->dsc$a_pointer, work, xlen);

#if 0
/********* This causes cursor to return to left margin, but since
	 * the cursor is usually hidden, or gets moved immediately
	 * anyway, I'm disabling this for now till I see if anyone
	 * notices. */

	smg$set_cursor_abs(xx_vdid, 0l, &1l);
#endif

	smg$set_cursor_mode(&(scope->smg_pbid), &1l);

	*start = cxpos;

	return(scope->scope_exit);

 l1200:
	/*
	 * Error condition
	 */
	smg$ring_bell(&(scope->smg_kbid));

	goto l1100;
}


/*
 * Internal function to output the text onto the screen, converting
 * the trailing characters to underscores if necessary.
 */
static void DisplayText(char *work, int xlen, char blank_char,
	long xx_vdid, long cposy, long cposx, long cxpos)
{
	char work1[132];
	struct dsc$descriptor_s xwork1;
	int i;

	/*
	 * Create a working copy
	 */
	strncpy(work1, work, xlen);

	/*
	 * Convert trailing spaces to the specified blank charactwr
	 */
	if (blank_char != ' ')
	{
		for (i = xlen-1; i>=0; i--)
		{
			if ((work1[i] == ' ') && (i >= cxpos))
				work1[i] = blank_char;
			else
				break;
		}
	}

	/*
	 * Create string descriptor
	 */
	xwork1.dsc$a_pointer = work1;
	xwork1.dsc$w_length = xlen;
	xwork1.dsc$b_class = DSC$K_CLASS_S;
	xwork1.dsc$b_dtype = DSC$K_DTYPE_T;

	/*
	 * Write out string
	 */
	smg$put_chars(
		&xx_vdid,
		&xwork1,
		&cposy,
		&cposx
	);
}


#ifndef MSDOS
/*
 * Not quite exactly as MS-DOS defines this, because it doesn't stop
 * at a null character.
 */
static char *strnset(char *s, int ch, size_t n)
{
	char *s1 = s;

	while (n--)
	{
		*s1++ = ch;
	}
	return(s);
}
#endif

/*
 * Shift characters in string to the right
 */
static strinsert(char str[], int shift, int xlen)
{
	int i;

	/*
	 * Shift over existing characters (shift) bytes
	 */
	for (i = xlen-1; i>=shift; i--)
	{
		str[i] = str[i-shift];
	}
}
