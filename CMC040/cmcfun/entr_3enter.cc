/*	%TITLE "Enter (on Screen) with Edit"
 */
#pragma module entr_3enter "V3.6 Calico"

/*
 * Include files
 */
#include <string>
#include <cstring>
#include <ctype.h>

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "smg/smg.h"

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
	smg_display_id &xx_vdid, long cposy, long cposx, long cxpos);
static void strinsert(char str[], int shift, int xlen);


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
 * Author:
 *
 *	06/21/84 - Kevin Handy
 *
 */


/*
 * Main entry point
 */
long entr_3enter(
	struct scope_struct &scope,
	smg_display_id &xx_vdid,
	long cposy,
	long cposx,
	std::string &xstr,
	long &start,
	long flag)
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
	std::string xwork1;
	char work1[132];

 l500:
	/*
	 * Initilize information
	 */
	xlen = xstr.size();			/* Length of string */
	strcpy(work, xstr.c_str());
						/* Force different copy */
	clearflag = (start == -1);		/* Zero input string? */

	if (start == -1)			/* Cursor position */
	{
		cxpos = 0;
	}
	else
	{
		cxpos = start;
	}

	if (flag & 4)				/* Forced keypunch mode */
	{
		nocr = 128;
	}
	else
	{
		nocr = 0;
	}

	if (flag & 4096)			/* Space or underline */
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
	smg$set_cursor_mode(scope.smg_pbid, 0);

 l1000:
	/*
	 * Format text to display to user
	 */
	DisplayText(work, xlen, blank_char, xx_vdid, cposy, cposx, cxpos);

 l1100:
	/*
	 * Set current cursor position
	 */
	junk = cposx + cxpos;
	smg$set_cursor_abs(
		xx_vdid,
		cposy,
		junk
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
		getc = entr_4entry(scope, xx_vdid, (256 | flag));

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
				smg$ring_bell(scope.smg_kbid);
			}
			else
			{
				strinsert(work + cxpos, 1, xlen - cxpos);
				if (flag & 16)
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
			DisplayText(work, xlen, blank_char, xx_vdid,
				cposy, cposx, cxpos);

		}
	}
	else
	{
		/*
		 * Allocate dynamic string descriptor
		 */
		xwork1 = std::string(readcount, ' ');

		/*
		 * Grab a bunch of characters
		 */
		junk = 256 | flag;

		getc = entr_3entrystring(scope, xx_vdid,
			junk, readcount, xwork1);

		readcount = xwork1.size();

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
		if (xwork1.size() != 0)
		{
			strinsert(work + cxpos,
				readcount,
				xlen - cxpos);
			for (i = 0; i < readcount; i++)
			{
				work[cxpos++] = xwork1[i];
			}

		}

		if ((clearflag != 0) || (xwork1.size() != 0))
		{
			/*
			 * Display changed text
			 */
			DisplayText(work, xlen, blank_char, xx_vdid,
				cposy, cposx, cxpos);
		}
	}

	/*
	 * Handle any special cases
	 */
	if ((flag & 1024) == 0)
	{
		getc = entr_4specialkeys(scope, xx_vdid, (256 | flag), getc);
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
	scope.scope_exit = getc;

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
					xx_vdid, cposy, cposx, cxpos);
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
	xstr = work;

	smg$set_cursor_mode(scope.smg_pbid, 1);

	start = cxpos;

	return(scope.scope_exit);

 l1200:
	/*
	 * Error condition
	 */
	smg$ring_bell(scope.smg_kbid);

	goto l1100;
}


/*
 * Internal function to output the text onto the screen, converting
 * the trailing characters to underscores if necessary.
 */
static void DisplayText(char *work, int xlen, char blank_char,
	smg_display_id &xx_vdid, long cposy, long cposx, long cxpos)
{
	char work1[132];
	std::string xwork1;
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
	xwork1 = work1;

	/*
	 * Write out string
	 */
	smg$put_chars(
		xx_vdid,
		xwork1,
		cposy,
		cposx
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
static void strinsert(char str[], int shift, int xlen)
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
