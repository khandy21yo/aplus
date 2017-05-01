/*	%TITLE "Subroutine to Scroll Window One Column"
 */
#pragma module dspl_scroll "V3.6 Calico"

/*
 *
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
 *	.b
 *	.lm +5
 *	This function is used to scroll text within a
 *	virtual window. It will scroll up, down, go to
 *	next screen, previous screen, top of text,
 *	bottom of text, and find a line in the text.
 *	.b
 *	SMG_SCROLL::SMG_FLAG MEANING( If Set )
 *	.table 3,25
 *	.te
 *	1% Encode the lines
 *	.te
 *	2% Draw lines
 *	.te
 *	8% No Begin/End updates on scroll
 *	.te
 *	128% Advance one line at a time if no prompt
 *	.end table
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	SMG_SCROLL
 *		A structure defining the scrolling window.
 *
 *	SMG_ARRAY()
 *		The returned array that holds the Fun Scroll values.
 *
 *	SMG_SCOPE.EXIT
 *		A function key that is assigned a command.
 *
 *	SMG_SCROLL_OPTION
 *		The returned user's options.
 *
 * Example:
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:DSPL_SCROLL
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP DSPL_SCROLL
 *	$ DELETE DSPL_SCROLL.OBJ;*
 *
 * AUTHOR:
 *
 *	04/23/87 - B. Craig Larsen
 *
 * MODIFICATION HISTORY:
 *
 *	11/17/89 - Kevin Handy
 *		Turned off error trapping because all it did
 *		was an on error goto 0 in effect.
 *
 *	11/17/89 - Kevin Handy
 *		Put in sharable library (Possible now because
 *		of disabling ENTR_MESSAGE call)
 *
 *	08/06/91 - Kevin Handy
 *		Removed goofball code that would print a '+'
 *		on the screen.  I don't know who added this
 *		junk code (Frank?) but they were too embarrased
 *		to say they did it in the edit history.
 *
 *	03/14/92 - Kevin Handy
 *		Clean up vars (checkvar)
 *
 *	06/01/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modified to compile with DEC-C without errors
 *		(module, sspace, prompt, smg$routines.h, casts)
 *--
 */

/*
 * Include files
 */
#include <descrip.h>
#include <string.h>
#include <ctype.h>
#include <smg$routines.h>
#include <smgdef.h>
#include "func_include:cmcfun.h"
/* #include "func_include:scroll.h" */

/*
 * Defined functions
 */
#define max(x,y) ((x)>(y)?(x):(y))
#define min(x,y) ((x)<(y)?(x):(y))

/*
 * Local functions
 */
#if 0
static int calcscroll(struct smg_scroll_struct *smg_scroll, int moveflag, int lines);
#else
#define calcscroll(smg_scroll, moveflag, lines)	\
	dofind(smg_scroll, moveflag, smg_scroll->cur_line + lines);
#endif

static int dofind(struct smg_scroll_struct *smg_scroll, int moveflag, int line);
static void doscroll(struct smg_scroll_struct *smg_scroll, int newtop);
static void dopaint(struct smg_scroll_struct *smg_scroll,
	struct dsc$descriptor_a *smg_array, long leader);
static struct dsc$descriptor_s *getelement(struct dsc$descriptor_a *array,
	long item);
static void drawline(struct smg_scroll_struct *smg_scroll,
	int height, int lineloop);

/*
 * Local constants
 */
static $DESCRIPTOR(nullstring, "");
static char sspace[] = "    ";

/*
 * Main function
 */
long dspl_scroll(
	struct smg_scroll_struct *smg_scroll,
	struct dsc$descriptor_a *smg_array,
	long *smg_scope_exit,
	struct dsc$descriptor_s *smg_scroll_option)
{
	/*
	 * Local variables
	 */
	char prompt[8];		/* Prompting character */
	int leader;		/* Length of prompt */
	int win_siz;		/* Lines in scrolling region */
	struct dsc$descriptor_s spaces;
	long line;
	int scrollamount;
	int newtop;
	int moveflag;		/* Movement flag (curent fixed at top?) */

	/*
	 * Play games with the prompt to see if we have one (length > 0)
	 * after stripping off spaces
	 */
	strncpy(prompt, smg_scroll->prompt, sizeof(smg_scroll->prompt));
	leader = sizeof(smg_scroll->prompt);
	while ((leader > 0) && (prompt[leader - 1] == ' '))
	{
		leader--;
	}
	prompt[leader] = '\0';

	/*
	 * Determine moveflag
	 */
	if ((leader != 0) || ((smg_scroll->smg_flag & 128) != 0))
	{
		moveflag = 1;
	}
	else
	{
		moveflag = 0;
	}

	/*
	 * Determine size of scrolling region
	 */
	win_siz = smg_scroll->scroll_bot - smg_scroll->scroll_top + 1;


	/*
	 * Remove the Prompt
	 */
	if (leader != 0)
	{
		spaces.dsc$a_pointer = sspace;
		spaces.dsc$w_length = leader;
		spaces.dsc$b_class = DSC$K_CLASS_S;
		spaces.dsc$b_dtype = DSC$K_DTYPE_T;

		line = smg_scroll->scroll_top +
			(smg_scroll->cur_line - smg_scroll->top_line);

		smg$put_chars(&smg_scroll->window, &spaces,
			&line, &1l);
	}

	/*
	 * Handle options
	 */
	switch (*smg_scope_exit)
	{
	case SMG$K_TRM_DOWN:

		scrollamount = 1;
		newtop = calcscroll(smg_scroll, moveflag, scrollamount);
		doscroll(smg_scroll, newtop);
		dopaint(smg_scroll, smg_array, leader);
		break;

	case SMG$K_TRM_UP:

		scrollamount = -1;
		newtop = calcscroll(smg_scroll, moveflag, scrollamount);
		doscroll(smg_scroll, newtop);
		dopaint(smg_scroll, smg_array, leader);
		break;

	case SMG$K_TRM_PREV_SCREEN:

		scrollamount = -win_siz + 1;
		newtop = calcscroll(smg_scroll, moveflag, scrollamount);
		doscroll(smg_scroll, newtop);
		dopaint(smg_scroll, smg_array, leader);
		break;

	case SMG$K_TRM_NEXT_SCREEN:

		scrollamount = win_siz - 1;
		newtop = calcscroll(smg_scroll, moveflag, scrollamount);
		doscroll(smg_scroll, newtop);
		dopaint(smg_scroll, smg_array, leader);
		break;

	case SMG$K_TRM_F18:

		newtop =  dofind(smg_scroll, moveflag, smg_scroll->top_array);
		doscroll(smg_scroll, newtop);
		dopaint(smg_scroll, smg_array, leader);
		break;

	case SMG$K_TRM_F19:

		newtop =  dofind(smg_scroll, moveflag, smg_scroll->bot_array);
		doscroll(smg_scroll, newtop);
		dopaint(smg_scroll, smg_array, leader);
		break;

	case SMG$K_TRM_SELECT:

		if ((smg_scroll->v_select_line <= 0) && (leader != 0))
		{
			smg_scroll->v_select_line = smg_scroll->cur_line;
		}
		dopaint(smg_scroll, smg_array, leader);
		break;

	case SMG$K_TRM_REMOVE:

		if (smg_scroll->v_select_line > 0)
		{
			smg_scroll->v_select_line = 0;
		}
		newtop = smg_scroll->top_line;
		dopaint(smg_scroll, smg_array, leader);
		break;

	default:

		if (smg_scroll_option->dsc$w_length != 0)
		{
			switch (smg_scroll_option->dsc$a_pointer[0])
			{
			case 'P':	/* 'PAINT' */
				dopaint(smg_scroll, smg_array, leader);
				break;

			case 'F':	/* 'FIND' */
				newtop =  dofind(smg_scroll, moveflag, smg_scroll->find_line);
				doscroll(smg_scroll, newtop);
				dopaint(smg_scroll, smg_array, leader);
				break;
			}
		}
	}

	/*
	 * Calculate current line position
	 */
	smg_scroll->cur_w_row = smg_scroll->scroll_top +
		smg_scroll->find_line - smg_scroll->top_line;

	/*
	 * Paint the Prompt
	 */
	if (leader != 0)
	{
		spaces.dsc$a_pointer = prompt;
		spaces.dsc$w_length = leader;
		spaces.dsc$b_class = DSC$K_CLASS_S;
		spaces.dsc$b_dtype = DSC$K_DTYPE_T;

		line = smg_scroll->scroll_top +
			smg_scroll->cur_line - smg_scroll->top_line;

		smg$put_chars(&smg_scroll->window, &spaces,
			&line, &1l, 0l, &SMG$M_BOLD);
	}

	return(0);
}

#if 0
/*
 * Calculations for scrolling the screen.
 * sets current line in scroll structure.
 * returns new top line.
 */
static int calcscroll(struct smg_scroll_struct *smg_scroll, int moveflag, int lines)
{
	/*
	 * Adjust current line pointer
	 */
	return(dofind(smg_scroll, moveflag, smg_scroll->cur_line + lines));
}
#endif

/*
 * Calculations for scrolling the screen.
 * sets current line in scroll structure.
 * returns new top line.
 */
static int dofind(struct smg_scroll_struct *smg_scroll, int moveflag, int line)
{
	/*
	 * local variables
	 */
	int newtop;
	int newline;
	int screenlines;

	/*
	 * Handle differently if doing with pointer, or without
	 */
	if (moveflag != 0)
	{
		/*
		 * A pointer is used, so calculate position of pointer
		 * on the screen.
		 */

		/*
		 * Adjust current line pointer
		 */
		newline = line;
		newline = max(newline, smg_scroll->top_array);
		newline = min(newline, smg_scroll->bot_array);
		smg_scroll->cur_line = newline;
		smg_scroll->find_line = newline;

		/*
		 * Calculate a new top line
		 */
		if (newline < smg_scroll->top_line)
		{
			newtop = newline;
		}
		else
		{
			screenlines = (smg_scroll->scroll_bot - smg_scroll->scroll_top);
			if (newline > smg_scroll->top_line + screenlines)
			{
				newtop = newline - screenlines;
			}
			else
			{
				newtop = smg_scroll->top_line;
			}
		}
	}
	else
	{
		/*
		 * No pointer is used, so scroll as much as possible
		 */
		screenlines = (smg_scroll->scroll_bot - smg_scroll->scroll_top);
		newline = line;
		newline = max(newline, smg_scroll->top_array);
		newline = min(newline,
			max(smg_scroll->top_array, smg_scroll->bot_array - screenlines));
		smg_scroll->cur_line = newline;
		smg_scroll->find_line = newline;
		newtop = newline;
	}
	return(newtop);
}

/*
 * Repaint the screen,
 * changing the top line.
 */
static void doscroll(struct smg_scroll_struct *smg_scroll, int newtop)
{
	/*
	 * Local variables
	 */
	int change;		/* Number of lines to scroll */
	long screenlines;

	/*
	 * Decide how to scroll
	 */
	change = newtop - smg_scroll->top_line;
	screenlines = (smg_scroll->scroll_bot - smg_scroll->scroll_top) + 1;

	/*
	 * Push lines upward
	 */
	if ((change > 0) && (change <= screenlines))
	{
		smg$scroll_display_area(&smg_scroll->window,
			&smg_scroll->scroll_top,
			&1l,
			&screenlines,
			0l,
			&SMG$M_UP,
			&change);
	}

	/*
	 * Push lines downward
	 * Must keep at least two lines, because backwards scroll
	 *  can look really funny (scrolls forewards sometimes)
	 */
	if ((change < 0) && (-change <= screenlines))
	{
		change = -change;
		smg$scroll_display_area(&smg_scroll->window,
			&smg_scroll->scroll_top,
			&1l,
			&screenlines,
			0l,
			&SMG$M_DOWN,
			&change);
	}

	/*
	 * Set the top line
	 */
	smg_scroll->top_line = newtop;
}

static void dopaint(struct smg_scroll_struct *smg_scroll,
	struct dsc$descriptor_a *smg_array,
	long leader)
{
	/*
	 * Local variables
	 */
	int lineloop;			/* Loop for lines */
	int bottomline;			/* Bottom line of screen */
	long video_set;			/* Special display mode */
	struct dsc$descriptor_s *text;	/* Pointer to array element */
	long leaderone = leader + 1;	/* Leader + 1 */
	long row;			/* row to display on */
	long height;
	long width;
	long length;
	long status;

	/*
	 * Turn on display update
	 */
	smg$begin_display_update(&smg_scroll->window);

	/*
	 * Paint all lines
	 */
	bottomline = (smg_scroll->scroll_bot - smg_scroll->scroll_top);

	for (lineloop = 0; lineloop <= bottomline; lineloop++)
	{
		/*
		 * Get text to display
		 */
		if (lineloop + smg_scroll->top_line <= smg_scroll->bot_array)
		{
			text = getelement(smg_array, lineloop + smg_scroll->top_line);
		}
		else
		{
			text = &nullstring;
		}

		/*
		 * Calculate position
		 */
		row = lineloop + smg_scroll->scroll_top;

		/*
		 * Handle video set
		 */
		video_set = 0;
		if (smg_scroll->v_select_line != 0)
		{
			/*
			 * If line is in select range
			 */
			if ((lineloop + smg_scroll->top_line >=
				smg_scroll->v_select_line) &&
				(lineloop +smg_scroll->top_line <=
				smg_scroll->cur_line))
			{
				video_set = smg_scroll->video_set;
			}

			if ((lineloop + smg_scroll->top_line <=
				smg_scroll->v_select_line) &&
				(lineloop +smg_scroll->top_line >=
				smg_scroll->cur_line))
			{
				video_set = smg_scroll->video_set;
			}
		}

		/*
		 * Output the line
		 */
		status = smg$erase_line(&smg_scroll->window, &row, &1l);

		if (((smg_scroll->smg_flag & 1) != 0) && (text->dsc$w_length != 0))
		{

			length = text->dsc$w_length;

			smg_put_virtual_display_encoded(
				&smg_scroll->window,
				&length,
				text,
				&row,
				&leaderone,
				&0l,
				&0l,
				&video_set);
		}
		else
		{
			status = smg$put_chars(&smg_scroll->window,
				text,
				&row,
				&leaderone,
				&0l,
				&video_set,
				&smg_scroll->video_comp,
				&smg_scroll->charset);
		}
	}

	/*
	 * Paint vertical lines if so desired
	 */
	if (smg_scroll->draw_cols[0] != ' ')
	{
		smg$get_display_attr(&smg_scroll->window, &height, &width);

		for (lineloop = 0;
			(lineloop < sizeof(smg_scroll->draw_cols)) &&
				(smg_scroll->draw_cols[lineloop] != ' ');
			lineloop += 4)
		{
			drawline(smg_scroll, height, lineloop);
		}
	}

	/*
	 * Turn off display update
	 */
	smg$end_display_update(&smg_scroll->window);
}

/*
 * Get a descriptor element
 */
static struct dsc$descriptor_s *getelement(
	struct dsc$descriptor_a *array,
	long item)
{
	/*
	 * Static local variables
	 */
	static struct dsc$descriptor_s temp_desc;

	/*
	 * Local Variables
	 */
	struct dsc$descriptor_s *str;
	long length;
	long elem;

	/*
	 * Can we fit it in?
	 */
	elem = item * (long)array->dsc$w_length;

	if (elem < array->dsc$l_arsize)
	{
		if (array->dsc$b_dtype == DSC$K_DTYPE_DSC)
		{
			/*
			 * Calculate position of descriptor
			 */
			str = (struct dsc$descriptor_s*)(array->dsc$a_pointer + elem);
		}
		else
		{
			temp_desc.dsc$a_pointer = array->dsc$a_pointer + elem;
			temp_desc.dsc$w_length = array->dsc$w_length;
			temp_desc.dsc$b_class = DSC$K_CLASS_S;
			temp_desc.dsc$b_dtype = array->dsc$b_dtype;
			str = &temp_desc;
		}
	}
	else
	{
		str = &nullstring;
	}
	return(str);
}

static void drawline(struct smg_scroll_struct *smg_scroll,
	int height,
	int lineloop)
{
	/*
	 * Local variables
	 */
	int column;

	/*
	 * Calculate column to display on
	 */
	column = (smg_scroll->draw_cols[lineloop] - '0') * 100 +
		(smg_scroll->draw_cols[lineloop+1] - '0') * 10 +
		(smg_scroll->draw_cols[lineloop+2] - '0');

	/*
	 * Display line
	 */
	smg$draw_line(&smg_scroll->window, &1l, &column, &height, &column);
}
