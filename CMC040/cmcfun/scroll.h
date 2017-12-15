//! \file
//! \btief  Scrolling structure
//!

#ifndef _scroll_h_
#define _scroll_h_

#include <vector>

//!
//! Scrolling structure
//!
class smg_scroll_cdd
{
public:
	/* This is the window Virtual Display ID */
	smg_display_id window;

	/* The top row of the scrolling region within the window. */
	long scroll_top;

	/* The bottom row of the scrolling region within the window. */
	long scroll_bot;

	/* The top element number of the array */
	long top_array;

	/* The number of lines in the array */
	long bot_array;

	/* The array element number of the line at the top of the window */
	long top_line;

	/* The array element number of the line to begin with */
	long beg_element;

	/* The array element number of the line to end with */
	long end_element;

	/* The array element number of the current line of the window */
	long cur_line;

	/* The number of the current window COLUMN - RETURNED */
	long cur_w_col;

	/* The number of the current window ROW    - RETURNED */
	long cur_w_row;

	/* The line to put on the screen in the find option */
	long find_line;

	/* Flag for various options */
	long smg_flag;

	/* The characters to use to point to the current line */
	std::string prompt;

	/* The list of columns that need a column line drawn */
	std::string draw_cols;

	/* The number of columns in the window */
	long num_colm;

	/* Video set. */
	long video_set;

	/* Video compare. */
	long video_comp;

	/* Character set type */
	long charset;

	/* Virtual selected line number for use with Select/Remove_select */
	long v_select_line;
};

long dspl_scroll(
	smg_scroll_cdd &smg_scroll,
	std::vector<std::string> &smg_array,
	long &smg_scope_exit,
	const std::string &smg_scroll_option);

#endif
