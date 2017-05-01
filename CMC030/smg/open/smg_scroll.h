/*
 * File Layout for: SMG.SMG_SCROLL
 *
 * \Scroller
 */

struct SMG_SCROLL_cdd
{
/* Element =
   Description = This is the window Virtual Display ID */
	long window;
/* Element =
   Description = The top row of the scrolling region with */
	long scroll_top;
/* Element =
   Description = The bottom row of the scrolling region w */
	long scroll_bot;
/* Element =
   Description = The top element number of the array */
	long top_array;
/* Element =
   Description = The number of lines in the array */
	long bot_array;
/* Element =
   Description = The array element number of the line at */
	long top_line;
/* Element =
   Description = The array element number of the line to */
	long beg_element;
/* Element =
   Description = The array element number of the line to */
	long end_element;
/* Element =
   Description = The array element number of the current */
	long cur_line;
/* Element =
   Description = The number of the current window COLUMN */
	long cur_w_col;
/* Element =
   Description = The number of the current window ROW - R */
	long cur_w_row;
/* Element =
   Description = The line to put on the screen in the fin */
	long find_line;
/* Element =
   Description = Flag for various options */
	long smg_flag;
/* Element =
   Description = The characters to use to point to the cu */
	char prompt[4];
/* Element =
   Description = The list of columns that need a column l */
	char draw_cols[160];
/* Element =
   Description = The number of columns in the window */
	long num_colm;
/* Element =
   Description = Video set. */
	long video_set;
/* Element =
   Description = Video compare. */
	long video_comp;
/* Element =
   Description = Character set type */
	long charset;
/* Element =
   Description = Virtual selected line number for use wit */
	long v_select_line;
};
