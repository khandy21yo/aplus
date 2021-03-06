/*
 * Scrolling structure
 */ 
struct smg_scroll_struct
{
	/* This is the window Virtual Display ID */
	long window;

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
	char prompt[4];

	/* The list of columns that need a column line drawn */
	char draw_cols[160];

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
