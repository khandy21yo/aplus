//!\file
////! \brief Subroutine to Scroll Window One Column
//!/
#pragma module dspl_scroll "V3.6 Calico"

/*
 * Include files
 */
#include <string>
#include <string.h>
#include <ctype.h>

#include "preferences.h"
#include "cmcfun.h"
#include "smg/smg.h"
#include "cmcfun/scroll.h"

/*
 * Defined functions
 */
#define max(x,y) ((x)>(y)?(x):(y))
#define min(x,y) ((x)<(y)?(x):(y))

/*
 * Local functions
 */
#define calcscroll(smg_scroll, moveflag, lines)	\
	dofind(smg_scroll, moveflag, smg_scroll.cur_line + lines);

static int dofind(smg_scroll_cdd &smg_scroll, int moveflag, int line);
static void doscroll(smg_scroll_cdd &smg_scroll, int newtop);
static void dopaint(smg_scroll_cdd &smg_scroll,
	std::vector<std::string> &smg_array, long leader);
static void drawline(smg_scroll_cdd &smg_scroll,
	int height, int lineloop);

/*
 * Local constants
 */
static char sspace[] = "    ";

//!
////! \brief Subroutine to Scroll Window One Column
//!
//!	This function is used to scroll text within a
//!	virtual window. It will scroll up, down, go to
//!	next screen, previous screen, top of text,
//!	bottom of text, and find a line in the text.
//!	.b
//!	SMG_SCROLL::SMG_FLAG MEANING( If Set )
//!	.table 3,25
//!	.te
//!	1% Encode the lines
//!	.te
//!	2% Draw lines
//!	.te
//!	8% No Begin/End updates on scroll
//!	.te
//!	128% Advance one line at a time if no prompt
//!	.end table
//!	.lm -5
//!
//! \author 04/23/87 - B. Craig Larsen
//!
long dspl_scroll(
	smg_scroll_cdd &smg_scroll,
		//!< A structure defining the scrolling window.
	std::vector<std::string> &smg_array,
		//!< The returned array that holds the Fun Scroll values.
	long &smg_scope_exit,
		//< A function key that is assigned a command.
	std::string &smg_scroll_option)
		//!< The returned user's options.
{
	/*
	 * Local variables
	 */
	std::string prompt;	/* Prompting character */
	int leader;		/* Length of prompt */
	int win_siz;		/* Lines in scrolling region */
	std::string spaces;
	long line;
	int scrollamount;
	int newtop;
	int moveflag;		/* Movement flag (curent fixed at top?) */

	/*
	 * Play games with the prompt to see if we have one (length > 0)
	 * after stripping off spaces
	 */
	prompt = smg_scroll.prompt;
	leader = smg_scroll.prompt.size();
	while ((leader > 0) && (prompt[leader - 1] == ' '))
	{
		leader--;
	}
	prompt[leader] = '\0';

	/*
	 * Determine moveflag
	 */
	if ((leader != 0) || ((smg_scroll.smg_flag & 128) != 0))
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
	win_siz = smg_scroll.scroll_bot - smg_scroll.scroll_top + 1;


	/*
	 * Remove the Prompt
	 */
	if (leader != 0)
	{
		spaces = std::string(leader,' ');

		line = smg_scroll.scroll_top +
			(smg_scroll.cur_line - smg_scroll.top_line);

		smg$put_chars(smg_scroll.window, spaces,
			line, 1);
	}

	/*
	 * Handle options
	 */
	switch (smg_scope_exit)
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

		newtop =  dofind(smg_scroll, moveflag, smg_scroll.top_array);
		doscroll(smg_scroll, newtop);
		dopaint(smg_scroll, smg_array, leader);
		break;

	case SMG$K_TRM_F19:

		newtop =  dofind(smg_scroll, moveflag, smg_scroll.bot_array);
		doscroll(smg_scroll, newtop);
		dopaint(smg_scroll, smg_array, leader);
		break;

	case SMG$K_TRM_SELECT:

		if ((smg_scroll.v_select_line <= 0) && (leader != 0))
		{
			smg_scroll.v_select_line = smg_scroll.cur_line;
		}
		dopaint(smg_scroll, smg_array, leader);
		break;

	case SMG$K_TRM_REMOVE:

		if (smg_scroll.v_select_line > 0)
		{
			smg_scroll.v_select_line = 0;
		}
		newtop = smg_scroll.top_line;
		dopaint(smg_scroll, smg_array, leader);
		break;

	default:

		if (smg_scroll_option.size() != 0)
		{
			switch (smg_scroll_option[0])
			{
			case 'P':	/* 'PAINT' */
				dopaint(smg_scroll, smg_array, leader);
				break;

			case 'F':	/* 'FIND' */
				newtop =  dofind(smg_scroll, moveflag, smg_scroll.find_line);
				doscroll(smg_scroll, newtop);
				dopaint(smg_scroll, smg_array, leader);
				break;
			}
		}
	}

	/*
	 * Calculate current line position
	 */
	smg_scroll.cur_w_row = smg_scroll.scroll_top +
		smg_scroll.find_line - smg_scroll.top_line;

	/*
	 * Paint the Prompt
	 */
	if (leader != 0)
	{
		spaces = prompt;

		line = smg_scroll.scroll_top +
			smg_scroll.cur_line - smg_scroll.top_line;

		smg$put_chars(smg_scroll.window, spaces,
			line, 1, 0, SMG$M_BOLD);
	}

	return(0);
}

//!
//! Calculations for scrolling the screen.
//! sets current line in scroll structure.
//! returns new top line.
//!/
static int dofind(smg_scroll_cdd &smg_scroll, int moveflag, int line)
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
		newline = max(newline, smg_scroll.top_array);
		newline = min(newline, smg_scroll.bot_array);
		smg_scroll.cur_line = newline;
		smg_scroll.find_line = newline;

		/*
		 * Calculate a new top line
		 */
		if (newline < smg_scroll.top_line)
		{
			newtop = newline;
		}
		else
		{
			screenlines = (smg_scroll.scroll_bot - smg_scroll.scroll_top);
			if (newline > smg_scroll.top_line + screenlines)
			{
				newtop = newline - screenlines;
			}
			else
			{
				newtop = smg_scroll.top_line;
			}
		}
	}
	else
	{
		/*
		 * No pointer is used, so scroll as much as possible
		 */
		screenlines = (smg_scroll.scroll_bot - smg_scroll.scroll_top);
		newline = line;
		newline = max(newline, smg_scroll.top_array);
		newline = min(newline,
			max(smg_scroll.top_array, smg_scroll.bot_array - screenlines));
		smg_scroll.cur_line = newline;
		smg_scroll.find_line = newline;
		newtop = newline;
	}
	return(newtop);
}

//!
//! Repaint the screen,
//! changing the top line.
//!/
static void doscroll(smg_scroll_cdd &smg_scroll, int newtop)
{
	/*
	 * Local variables
	 */
	int change;		/* Number of lines to scroll */
	long screenlines;

	/*
	 * Decide how to scroll
	 */
	change = newtop - smg_scroll.top_line;
	screenlines = (smg_scroll.scroll_bot - smg_scroll.scroll_top) + 1;

#if 0 // curses doesn't scroll like this, so just force a redraw
	/*
	 * Push lines upward
	 */
	if ((change > 0) && (change <= screenlines))
	{
		smg$scroll_display_area(smg_scroll.window,
			smg_scroll.scroll_top,
			1,
			screenlines,
			0,
			SMG$M_UP,
			change);
	}

	/*
	 * Push lines downward
	 * Must keep at least two lines, because backwards scroll
	 *  can look really funny (scrolls forewards sometimes)
	 */
	if ((change < 0) && (-change <= screenlines))
	{
		change = -change;
		smg$scroll_display_area(smg_scroll.window,
			smg_scroll.scroll_top,
			1,
			screenlines,
			0,
			SMG$M_DOWN,
			change);
	}
#endif
	/*
	 * Set the top line
	 */
	smg_scroll.top_line = newtop;
}

static void dopaint(smg_scroll_cdd &smg_scroll,
	std::vector<std::string> &smg_array,
	long leader)
{
	/*
	 * Local variables
	 */
	int lineloop;			/* Loop for lines */
	int bottomline;			/* Bottom line of screen */
	long video_set;			/* Special display mode */
	std::string text;		/* Pointer to array element */
	long leaderone = leader + 1;	/* Leader + 1 */
	long row;			/* row to display on */
	long height;
	long width;
	long length;
	long status;

	/*
	 * Turn on display update
	 */
	smg$begin_display_update(smg_scroll.window);

	/*
	 * Paint all lines
	 */
	bottomline = (smg_scroll.scroll_bot - smg_scroll.scroll_top);

	for (lineloop = 0; lineloop <= bottomline; lineloop++)
	{
		/*
		 * Get text to display
		 */
		if (lineloop + smg_scroll.top_line <= smg_scroll.bot_array)
		{
			text = smg_array[lineloop + smg_scroll.top_line];
		}
		else
		{
			text = "";
		}

		/*
		 * Calculate position
		 */
		row = lineloop + smg_scroll.scroll_top;

		/*
		 * Handle video set
		 */
		video_set = 0;
		if (smg_scroll.v_select_line != 0)
		{
			/*
			 * If line is in select range
			 */
			if ((lineloop + smg_scroll.top_line >=
				smg_scroll.v_select_line) &&
				(lineloop +smg_scroll.top_line <=
				smg_scroll.cur_line))
			{
				video_set = smg_scroll.video_set;
			}

			if ((lineloop + smg_scroll.top_line <=
				smg_scroll.v_select_line) &&
				(lineloop +smg_scroll.top_line >=
				smg_scroll.cur_line))
			{
				video_set = smg_scroll.video_set;
			}
		}

		/*
		 * Output the line
		 */
		status = smg$erase_line(smg_scroll.window, row, 1);

		if (((smg_scroll.smg_flag & 1) != 0) && (text.size() != 0))
		{

			length = text.size();

			smg_put_virtual_display_encoded(
				smg_scroll.window,
				length,
				text,
				row,
				leaderone,
				0,
				0,
				video_set);
		}
		else
		{
			status = smg$put_chars(smg_scroll.window,
				text,
				row,
				leaderone,
				0,
				video_set,
				smg_scroll.video_comp,
				smg_scroll.charset);
		}
	}

	/*
	 * Paint vertical lines if so desired
	 */
	if (smg_scroll.draw_cols[0] != ' ')
	{
		smg$get_display_attr(smg_scroll.window, &height, &width);

		for (lineloop = 0;
			(lineloop < smg_scroll.draw_cols.size()) &&
				(smg_scroll.draw_cols[lineloop] != ' ');
			lineloop += 4)
		{
			drawline(smg_scroll, height, lineloop);
		}
	}

	/*
	 * Turn off display update
	 */
	smg$end_display_update(smg_scroll.window);
}

static void drawline(smg_scroll_cdd &smg_scroll,
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
	column = (smg_scroll.draw_cols[lineloop] - '0') * 100 +
		(smg_scroll.draw_cols[lineloop+1] - '0') * 10 +
		(smg_scroll.draw_cols[lineloop+2] - '0');

	/*
	 * Display line
	 */
	smg$draw_line(smg_scroll.window, 1, column, height, column);
}
