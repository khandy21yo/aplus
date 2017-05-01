/*
 * %TITLE "Subroutine to Scroll Window Circular"
 * %SBTTL "DSPL_SCROLLCIR"
 * %IDENT "V3.6 Calico"
 */
#pragma module dspl_scrollcir "V3.6 Calico"

/*
 * COPYRIGHT (C) 1987 BY
 * Computer Management Center
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
 *	4% Request for more data/text
 *	.te
 *	8% No Begin/End updates on scroll
 *	.end table
 *	.lm -5
 *
 * Parameters:
 *
 *	SMG_SCROLL
 *		The passed file used to get the scroll window.
 *
 *	SMG_ARRAY()
 *		The returned array that holds the Fun Scroll values.
 *
 *	SMG_SCOPE.EXIT
 *		A passed function key that is assigned a command.
 *
 *	SMG_SCROLL_OPTION
 *		The user's options. NO LONGER USED.
 *
 * Index:
 *
 *	.x Scroll>Circular
 *	.x Circular>Scroll
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:DSPL_SCROLLCIR
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP DSPL_SCROLLCIR
 *	$ DELETE DSPL_SCROLLCIR.OBJ;*
 *
 * AUTHOR:
 *
 *	04/21/87 - B. Craig Larsen
 *
 * MODIFICATION HISTORY:
 *
 *	03/14/92 - Kevin Handy
 *		Clean up vars (checkvar)
 *
 *	06/30/93 - Kevin Handy
 *		Couple of mods for a tiny bit more speed.
 *		Added IF statements to print prompt,
 *		moved code into IF statements,
 *		Used TRM$ instead of EDIT$ on PROMPT$,
 *		Deleted commented out code.
 *
 *	07/19/93 - Kevin Handy
 *		Modified to allow SMG$K_TRM_FIND to stand in
 *		for "FOND" so we can phase out the "FIND"
 *		option, which will allow only one command input
 *		instead of having to device between two different
 *		commands.
 *
 *	04/15/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards
 *
 *	11/01/95 - Kevin Handy
 *		Lose handling of SMG_SCROLL_OPTION, because it
 *		was only causing confusion wherever it was used.
 *
 *	11/08/95 - Kevin Handy
 *		Converted to C (using btran and lots and lots of editing)
 *
 *	05/27/99 - Kevin Handy
 *		Modified to compile with DEC-C without errors.
 *		(module, smg$routines.h, smgdef.h)
 *		added 'tlen' variable
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
#include <assert.h>

/*
 * Local constants
 */
static $DESCRIPTOR(nullstring, "");
static char const sspace[] = "    ";
static long zero = 0;
static long const one = 1;
static long const smgmdown = SMG$M_DOWN;
static long const smgmup = SMG$M_UP;
static long const smgmbold = SMG$M_BOLD;

/*
 * Local functions
 */
static int fnmap_av();
static int fnmap_va();
static void paintwindow(struct smg_scroll_struct *smg_scroll,
	long vfind, long* vtopl, long win_siz, long vbeg, long vend,
	int width, int leader, struct dsc$descriptor_a *smg_array,
	long height);
static void printline(struct smg_scroll_struct *smg_scroll,
	long rows, long leader, long i, struct dsc$descriptor_a *smg_array,
	long height, long width);
static void drawline(struct smg_scroll_struct *smg_scroll,
	int height, int lineloop);
static struct dsc$descriptor_s *getelement(struct dsc$descriptor_a *array,
	long item);

/*
 * Main function
 */
long dspl_scrollcir(
	struct smg_scroll_struct *smg_scroll,
	struct dsc$descriptor_a *smg_array,
	long *smg_scope_exit,
	struct dsc$descriptor_s *smg_scroll_option)
{
	char prompt[8];		/* Prompting character */
	long leader;		/* Length of prompt */
	long line;
	struct dsc$descriptor_s spaces;

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
	strncpy(prompt, smg_scroll->prompt, sizeof(smg_scroll->prompt));
	leader = sizeof(smg_scroll->prompt);
	while ((leader > 0) && (prompt[leader - 1] == ' '))
	{
		leader--;
	}
	prompt[leader] = '\0';

	win_siz = smg_scroll->scroll_bot - smg_scroll->scroll_top + 1;

	vbeg = fnmap_av(smg_scroll->beg_element, smg_scroll);
	vtopl = fnmap_av(smg_scroll->top_line, smg_scroll);
	vcurl = fnmap_av(smg_scroll->cur_line, smg_scroll);
	vfind = fnmap_av(smg_scroll->find_line, smg_scroll);

	if (smg_scroll->end_element > smg_scroll->bot_array)
	{
		vend = fnmap_av(smg_scroll->bot_array, smg_scroll);
	}
	else
	{
		vend = fnmap_av(smg_scroll->end_element, smg_scroll);
	}

	assert(vend >= vbeg);

	/*
	 * Return the attributes of a virtual display
	 */
	smg_status = smg$get_display_attr(&smg_scroll->window,
		&height, &width);

	/*
	 * Remove the Prompt
	 */
	if (leader != 0)
	{
		spaces.dsc$a_pointer = (void*)&sspace;
		spaces.dsc$w_length = leader;
		spaces.dsc$b_class = DSC$K_CLASS_S;
		spaces.dsc$b_dtype = DSC$K_DTYPE_T;

		line = vcurl - vtopl + smg_scroll->scroll_top;

		smg$put_chars(&smg_scroll->window, &spaces,
			&line, &one);
	}

	/*
	 * Convert escape sequence to commands
	 */
	switch (*smg_scope_exit)
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
		vfind = fnmap_av(smg_scroll->top_array, smg_scroll);
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
		if (smg_scroll->smg_flag & 4)
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
		if (smg_scroll->smg_flag & 4)
		{
			return vfind - vbeg;
		}
		else
		{
			vfind = vbeg;
			goto findline;
		}
	}

	if ((vfind >= (vtopl - win_siz)) && (vfind <= (vtopl - 1)))
	{
		/*
		 * Is it close enough to scroll the screen down
		 */
		rows = smg_scroll->scroll_top;
		if (smg_scroll->smg_flag & 8)
		{
			smg_status = smg$begin_display_update(
				&smg_scroll->window);
		}
		for (i = (vtopl - 1); i >= vfind; i--)
		{
			smg_status =
				smg$scroll_display_area(&smg_scroll->window,
				&rows, &one, &win_siz, 0l, &smgmdown,
				&one);
			smg_status = smg$begin_display_update(
				&smg_scroll->window);
			printline(smg_scroll, rows, leader, i, smg_array,
				height, width);
			smg_status = smg$end_display_update(&smg_scroll->window);
		}
		if (smg_scroll->smg_flag & 8)
		{
			smg_status = smg$end_display_update(&smg_scroll->window);
		}
		vtopl = vfind;
		goto exitprogram;
	}

	if ((vfind >= vtopl + win_siz) && (vfind <= vtopl + 2 * win_siz - 1))
	{
		/*
		 * Is it close enough to scroll the screen up for
		 */
		rows = smg_scroll->scroll_bot;
		if (smg_scroll->smg_flag & 8)
		{
			smg_status = smg$begin_display_update(
				&smg_scroll->window);
		}
		for (i = (vtopl + win_siz); i <= vfind; i++)
		{
			smg_status =
				smg$scroll_display_area(&smg_scroll->window,
				&smg_scroll->scroll_top, &one, &win_siz, 0l,
				&smgmup, &one);
			smg_status = smg$begin_display_update(
				&smg_scroll->window);
			printline(smg_scroll, rows, leader, i, smg_array,
				height, width);
			smg_status = smg$end_display_update(
				&smg_scroll->window);
		}
		if (smg_scroll->smg_flag & 8)
		{
			smg_status = smg$end_display_update(
				&smg_scroll->window);
		}
		vtopl = vfind - win_siz + 1;

		goto exitprogram;
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
		spaces.dsc$a_pointer = (void*)&prompt;
		spaces.dsc$w_length = leader;
		spaces.dsc$b_class = DSC$K_CLASS_S;
		spaces.dsc$b_dtype = DSC$K_DTYPE_T;

		line = vfind - vtopl + smg_scroll->scroll_top;

		smg$put_chars(&smg_scroll->window, &spaces,
			&line, &one, &zero, &smgmbold);
	}

	smg_scroll->cur_line = fnmap_va(vfind, smg_scroll);
	smg_scroll->cur_w_row = vfind - vtopl + smg_scroll->scroll_top;
	smg_scroll->top_line = fnmap_va(vtopl, smg_scroll);
	smg_scroll->find_line = fnmap_va(vfind, smg_scroll);

	return 0;
}

/*
 * Subroutine to painty up the screen
 */
static void paintwindow(struct smg_scroll_struct *smg_scroll,
	long vfind, long* vtopl, long win_siz, long vbeg, long vend,
	int width, int leader, struct dsc$descriptor_a *smg_array,
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
	smg_status = smg$begin_display_update(&smg_scroll->window);
	smg_status = smg$erase_display(&smg_scroll->window,
		&smg_scroll->scroll_top, &one, &smg_scroll->scroll_bot, &width);

	for (i = *vtopl; i <= bot; i++)
	{
		rows = (i - *vtopl + smg_scroll->scroll_top);
		printline(smg_scroll, rows, leader, i, smg_array,
			height, width);
	}
	smg_status = smg$end_display_update(&smg_scroll->window);
}

/*
 * Print the line and optionally draw the vertical lines
 */
static void printline(struct smg_scroll_struct *smg_scroll,
	long rows, long leader, long i, struct dsc$descriptor_a *smg_array,
	long height, long width)
{
	long smg_status;
	struct dsc$descriptor_s *text;	/* Pointer to array element */
	long leader1 = leader + 1;
	long lineloop;
	long tlen;

	text = getelement(smg_array, fnmap_va(i, smg_scroll));

	if (smg_scroll->smg_flag & 1)
	{
		tlen = text->dsc$w_length;
		smg_status =
			smg_put_virtual_display_encoded(&smg_scroll->window, 
			&tlen,
			text, &rows, &leader1, &zero, &zero, &zero);
	}
	else
	{
		smg_status = smg$put_chars(&smg_scroll->window,
			text,
			&rows,
			&leader1,
			&zero,
			&smg_scroll->video_set,
			&smg_scroll->video_comp,
			&smg_scroll->charset);
	}

	/*
	 * Paint vertical lines if so desired
	 */
	if (smg_scroll->smg_flag & 2)
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
	struct dsc$descriptor_s *str;
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

/* =====================================================================
 * Map the actual line into the virtual line
 * ================================================================== */

static int fnmap_av(long theline, struct smg_scroll_struct *smg_scroll)
{
	if ((smg_scroll->end_element - smg_scroll->beg_element) >= 0)
	{
		/*
		 * Beg - End
		 */
		return theline - smg_scroll->beg_element + 1;
	}
	else
	{
		/*
		 * End - Beg
		 */
		if (theline >= smg_scroll->beg_element)
		{
			return theline - smg_scroll->beg_element + 1;
		}
		else
		{
			return theline - smg_scroll->top_array +
				smg_scroll->bot_array -
				smg_scroll->beg_element + 2;
		}
	}
}

/* =====================================================================
 * Map the virtual line into the actual line
 * ================================================================== */

static int fnmap_va(long theline, struct smg_scroll_struct *smg_scroll)
{
	if ((smg_scroll->end_element - smg_scroll->beg_element) >= 0)
	{
		/*
		 * Beg - End
		 */
		return smg_scroll->beg_element + theline - 1;

	}
	else
	{
		/*
		 * End - Beg
		 */
		if (theline <= smg_scroll->bot_array -
			smg_scroll->beg_element + 1)
		{
			return smg_scroll->beg_element + theline - 1;
		}
		else
		{
			return smg_scroll->top_array + theline -
				(smg_scroll->bot_array -
				smg_scroll->beg_element + 1) - 1;
		}
	}
}

/*
 * Draw a line down the screen
 */
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
	smg$draw_line(&smg_scroll->window, &one, &column, &height, &column);
}
