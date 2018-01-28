//! \file
//! \brief Subroutine to Scroll Window Circular
//! %SBTTL "DSPL_SCROLLCIR"
//! %IDENT "V3.6 Calico"
//!
#pragma module dspl_scrollcir "V3.6 Calico"


/*
 * Include files
 */
#include <string>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "preferences.h"
#include "cmcfun.h"
#include "smg/smg.h"
#include "cmcfun/scroll.h"

/*
 * Local constants
 */
static std::string nullstring = "";
static char const sspace[] = "    ";
static long zero = 0;
static long const one = 1;
static long const smgmdown = SMG$M_DOWN;
static long const smgmup = SMG$M_UP;
static long const smgmbold = SMG$M_BOLD;

/*
 * Local functions
 */
static int fnmap_av(long theline, smg_scroll_cdd &smg_scroll);
static int fnmap_va(long theline, smg_scroll_cdd &smg_scroll);
static void paintwindow(smg_scroll_cdd &smg_scroll,
	long vfind, long* vtopl, long win_siz, long vbeg, long vend,
	int width, int leader, std::vector<std::string> &smg_array,
	long height);
static void printline(smg_scroll_cdd &smg_scroll,
	long rows, long leader, long i, std::vector<std::string> &smg_array,
	long height, long width);
static void drawline(smg_scroll_cdd &smg_scroll,
	int height, int lineloop);
static std::string getelement(std::vector<std::string> &array,
	long item);

//!
//! Abstract:HELP
//!	.b
//!	.lm +5
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
//!	4% Request for more data/text
//!	.te
//!	8% No Begin/End updates on scroll
//!	.end table
//!	.lm -5
//!
//! Parameters:
//!
//!	SMG_SCROLL
//!		The passed file used to get the scroll window.
//!
//!	SMG_ARRAY()
//!		The returned array that holds the Fun Scroll values.
//!
//!	SMG_SCOPE.EXIT
//!		A passed function key that is assigned a command.
//!
//!	SMG_SCROLL_OPTION
//!		The user's options. NO LONGER USED.
//!
//! AUTHOR:
//!
//!	04/21/87 - B. Craig Larsen
//!
//!--
long dspl_scrollcir(
	smg_scroll_cdd &smg_scroll,
	std::vector<std::string> &smg_array,
	long smg_scope_exit,
	const std::string &smg_scroll_option)
{
	std::string prompt;		/* Prompting character */
	long leader;		/* Length of prompt */
	long line;
	std::string spaces;

	int smg_status;
	int i;

	long rows;
	long height;
	long width;
	long vbeg;
	long vend;
	long vtopl;
	long vcurl;
	long vfind;
	long win_siz;


	/*
	 * Set Fun Scroll Variables
	 */
	prompt = smg_scroll.prompt;
	leader = smg_scroll.prompt.size();
	while ((leader > 0) && (prompt[leader - 1] == ' '))
	{
		prompt.erase(leader - 1);
		leader--;
	}

	win_siz = smg_scroll.scroll_bot - smg_scroll.scroll_top + 1;

	vbeg = fnmap_av(smg_scroll.beg_element, smg_scroll);
	vtopl = fnmap_av(smg_scroll.top_line, smg_scroll);
	vcurl = fnmap_av(smg_scroll.cur_line, smg_scroll);
	vfind = fnmap_av(smg_scroll.find_line, smg_scroll);

	if (smg_scroll.end_element > smg_scroll.bot_array)
	{
		vend = fnmap_av(smg_scroll.bot_array, smg_scroll);
	}
	else
	{
		vend = fnmap_av(smg_scroll.end_element, smg_scroll);
	}

	assert(vend >= vbeg);

	/*
	 * Return the attributes of a virtual display
	 */
	smg_status = smg$get_display_attr(*smg_scroll.window,
		&height, &width);

	/*
	 * Remove the Prompt
	 */
	if (leader != 0)
	{
		spaces = std::string(leader, ' ');

		line = vcurl - vtopl + smg_scroll.scroll_top;

		smg$put_chars(*smg_scroll.window, spaces,
			line, one);
	}

	/*
	 * Convert escape sequence to commands
	 */
	switch (smg_scope_exit)
	{
	case SMG$K_TRM_DOWN:
		if (leader)
		{
			vfind = vcurl + 1;
		}
		else
		{
			vfind = vtopl + win_siz;
		}
		break;

	case SMG$K_TRM_UP:
		if (leader)
		{
			vfind = vcurl - 1;
		}
		else
		{
			vfind = vtopl - 1;
		}
		break;

	case SMG$K_TRM_PREV_SCREEN:
		if (leader)
		{
			vfind = vcurl - win_siz + 1;
		}
		else
		{
			vfind = vtopl - win_siz + 1;
		}
		break;

	case SMG$K_TRM_NEXT_SCREEN:
		if (leader)
		{
			vfind = vcurl + win_siz - 1;
		}
		else
		{
			vfind = vtopl + 2 * (win_siz - 1);
		}
		break;

	case SMG$K_TRM_F18:
		/*
		 * Top
		 */
		vfind = fnmap_av(smg_scroll.top_array, smg_scroll);
		paintwindow(smg_scroll, vfind, &vtopl, win_siz, vbeg,
			vend, width, leader, smg_array, height);
		goto exitprogram;

	case SMG$K_TRM_F19:
		/*
		 * Bottom
		 */
		vfind = vend;
		paintwindow(smg_scroll, vfind, &vtopl, win_siz, vbeg,
			vend, width, leader, smg_array, height);
		goto exitprogram;

	case SMG$K_TRM_FIND:
		goto findline;

	default:
		paintwindow(smg_scroll, vfind, &vtopl, win_siz, vbeg,
			vend, width, leader, smg_array, height);
		goto exitprogram;
	}

findline:
	/* =======================================================
	 * Find a line
	 * ======================================================= */
	if (vfind > vend)
	{
		/*
		 * Positive
		 */
		if (smg_scroll.smg_flag & 4)
		{
			return vfind - vend;
		}
		else
		{
			vfind = vend;
			goto findline;
		}
	}

	if (vfind < vbeg)
	{
		if (smg_scroll.smg_flag & 4)
		{
			return vfind - vbeg;
		}
		else
		{
			vfind = vbeg;
			goto findline;
		}
	}

	/*
	 * Otherwise, repaint all lines, with current
	 * line at the top.
	 */
	paintwindow(smg_scroll, vfind, &vtopl, win_siz, vbeg,
		vend, width, leader, smg_array, height);

exitprogram:
	/*
	 * Paint the Prompt
	 */
	if (leader != 0)
	{
		spaces = std::string(leader, ' ');

		line = vfind - vtopl + smg_scroll.scroll_top;

		smg$put_chars(*smg_scroll.window, spaces,
			line, one, zero, smgmbold);
	}

	smg_scroll.cur_line = fnmap_va(vfind, smg_scroll);
	smg_scroll.cur_w_row = vfind - vtopl + smg_scroll.scroll_top;
	smg_scroll.top_line = fnmap_va(vtopl, smg_scroll);
	smg_scroll.find_line = fnmap_va(vfind, smg_scroll);

	return 0;
}

/*
 * Subroutine to painty up the screen
 */
static void paintwindow(smg_scroll_cdd &smg_scroll,
	long vfind, long* vtopl, long win_siz, long vbeg, long vend,
	int width, int leader, std::vector<std::string> &smg_array,
	long height)
{
	long smg_status;
	long bot;
	long rows;
	long i;

	if ((vfind < *vtopl) | (vfind > *vtopl + win_siz - 1))
	{
		*vtopl = vfind;
		if (*vtopl > vend - win_siz)
		{
			*vtopl = vend - win_siz + 1;
		}
		if (*vtopl < vbeg)
		{
			*vtopl = vbeg;
		}
	}
	bot = *vtopl + win_siz - 1;
	if (bot > vend)
	{
		bot = vend;
	}
	smg_status = smg$begin_display_update(*smg_scroll.window);
	smg_status = smg$erase_display(*smg_scroll.window);

	for (i = *vtopl; i <= bot; i++)
	{
		rows = (i - *vtopl + smg_scroll.scroll_top);
		printline(smg_scroll, rows, leader, i, smg_array,
			height, width);
	}
	smg_status = smg$end_display_update(*smg_scroll.window);
}

/*
 * Print the line and optionally draw the vertical lines
 */
static void printline(smg_scroll_cdd &smg_scroll,
	long rows, long leader, long i, std::vector<std::string> &smg_array,
	long height, long width)
{
	long smg_status;
	std::string text;	/* Pointer to array element */
	long leader1 = leader + 1;
	long lineloop;
	long tlen;

	text = getelement(smg_array, fnmap_va(i, smg_scroll));

	if (smg_scroll.smg_flag & 1)
	{
		tlen = text.size();
		smg_status =
			smg_put_virtual_display_encoded(*smg_scroll.window, 
			tlen,
			text, rows, leader1, zero, zero, zero);
	}
	else
	{
		smg_status = smg$put_chars(*smg_scroll.window,
			text,
			rows,
			leader1,
			zero,
			smg_scroll.video_set,
			smg_scroll.video_comp,
			smg_scroll.charset);
	}

	/*
	 * Paint vertical lines if so desired
	 */
	if (smg_scroll.smg_flag & 2)
	{
		smg$get_display_attr(*smg_scroll.window, &height, &width);

		for (lineloop = 0;
			(lineloop < sizeof(smg_scroll.draw_cols)) &&
				(smg_scroll.draw_cols[lineloop] != ' ');
			lineloop += 4)
		{
			drawline(smg_scroll, height, lineloop);
		}
	}
}

/*
 * Get a descriptor element
 */
static std::string getelement(
	std::vector<std::string> &array,
	long item)
{
	/*
	 * Static local variables
	 */
	std::string temp_desc;
	std::string str;
	long elem;

	/*
	 * Can we fit it in?
	 */
	elem = item * array.size();

	if (elem < array.size())
	{
		/*
		 * Calculate position of descriptor
		 */
		str = array[elem];
	}
	else
	{
		str = "";
	}
	return(str);
}

/* =====================================================================
 * Map the actual line into the virtual line
 * ================================================================== */

static int fnmap_av(long theline, smg_scroll_cdd &smg_scroll)
{
	if ((smg_scroll.end_element - smg_scroll.beg_element) >= 0)
	{
		/*
		 * Beg - End
		 */
		return theline - smg_scroll.beg_element + 1;
	}
	else
	{
		/*
		 * End - Beg
		 */
		if (theline >= smg_scroll.beg_element)
		{
			return theline - smg_scroll.beg_element + 1;
		}
		else
		{
			return theline - smg_scroll.top_array +
				smg_scroll.bot_array -
				smg_scroll.beg_element + 2;
		}
	}
}

/* =====================================================================
 * Map the virtual line into the actual line
 * ================================================================== */

static int fnmap_va(long theline, smg_scroll_cdd &smg_scroll)
{
	if ((smg_scroll.end_element - smg_scroll.beg_element) >= 0)
	{
		/*
		 * Beg - End
		 */
		return smg_scroll.beg_element + theline - 1;

	}
	else
	{
		/*
		 * End - Beg
		 */
		if (theline <= smg_scroll.bot_array -
			smg_scroll.beg_element + 1)
		{
			return smg_scroll.beg_element + theline - 1;
		}
		else
		{
			return smg_scroll.top_array + theline -
				(smg_scroll.bot_array -
				smg_scroll.beg_element + 1) - 1;
		}
	}
}

/*
 * Draw a line down the screen
 */
static void drawline(
	smg_scroll_cdd &smg_scroll,
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
	smg$draw_line(*smg_scroll.window, one, column, height, column);
}
