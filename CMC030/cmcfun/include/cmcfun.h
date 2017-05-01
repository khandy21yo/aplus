/********************************************************************
 * cmcfun.h - Definitions common for all CMC software.
 *
 * History:
 *
 *	06/05/2001 - Kevin Handy
 *		Added the member_alignment code around the structures
 *		passed between C and BASIC (since BASIC defaults to this,
 *		as does Vax/C, but not Dec/C)
 */
#ifndef CMCFUN_DEF
#define CMCFUN_DEF 1

/********************************************************************
 * Define Constants
 */
#define CMC$_ABORT		10
#define CMC$_DATEOUT		11
#define CMC$_LEFT		0
#define CMC$_MACRO		19
#define CMC$_NOOPTION		4
#define CMC$_NORMAL		1
#define CMC$_RIGHT		1
#define CMC$_TERMINATED		8
#define CMC$_UNDEFINED		3
#define CMC$_UNTERROR		2
#define CMC$_WARNING		0

#define OPTION_ABORT		1
#define OPTION_ADDREC		2
#define OPTION_ASSIGN		3
#define OPTION_CHECK		4
#define OPTION_CLOSEFILE	5
#define OPTION_COMPLETE		6
#define OPTION_CONFIRM		7
#define OPTION_HHMMSS		0
#define OPTION_INTERRUPT	8
#define OPTION_MARKFILE		9
#define OPTION_MMDDYY		0
#define OPTION_OPENFILE		10
#define OPTION_POSTFILE		11
#define OPTION_REPORT		14
#define OPTION_RESTART		12
#define OPTION_SUMMARY		13

#define OPTION_INITIALIZE	37
#define OPTION_OPTLIST		38
#define OPTION_BACKGROUND	1
#define OPTION_ENTRY		2
#define OPTION_TESTENTRY	3
#define OPTION_LOG		4
#define OPTION_SETOLD		5
#define OPTION_RESETOLD		6
#define OPTION_SETDEFAULT	7
#define OPTION_RESETDEFAULT	8
#define OPTION_VIEW		9
#define OPTION_FIND		11
#define OPTION_DISPLAY		12
#define OPTION_TESTOPTION	15
#define OPTION_AFTEROPTION	16
#define OPTION_WINDOW		90
#define OPTION_MOREMENU		91
#define OPTION_ARRAY		17
#define OPTION_EXIT		18
#define OPTION_SUBWINDOW	19

#define SUBOPTION_CHECK		65536
#define SUBOPTION_DETAIL	131072
#define SUBOPTION_FINAL		262114
#define SUBOPTION_LEDGER	1
#define SUBOPTION_LINEITEM	2
#define SUBOPTION_NOOPT		0
#define SUBOPTION_REGISTER	3
#define SUBOPTION_REVERSE	524288


#define MAX_NAME 20


/********************************************************************
 * Define structures
 */

/*
 * Structure defining interface for terminal/window information
 */
#pragma member_alignment save
#pragma nomember_alignment

struct scope_struct
{
	long scope_exit;	/* Exit status of entry routines */
	long scope_timeout;	/* Time for input timeout in seconds */
	long smg_kbid;		/* Keyboard buffer name */
	long smg_pbid;		/* Pastboard buffer name */
	long smg_option;	/* Option display */
	long smg_message;	/* Message display */
	long macroflag;		/* Macro command flag*/
	char prg_company[64];	/* Company name */
	char prg_ident[4];	/* Message type */
	char prg_program[40];	/* Program Name */
	char prg_item[16];	/* Item for help */
	long screen_width;	/* Width of created screen */
	long imenu_levels;	/* Interupt Menu Levels */
};

#pragma member_alignment restore

/*
 * Structire used for screen captures
 */
#pragma member_alignment save
#pragma nomember_alignment

struct screencapture_struct
{
	long lines;
	char screen[25][132];
};

#pragma member_alignment restore

/*
 * Used for option lists
 */
#pragma member_alignment save
#pragma nomember_alignment

struct option_list_struct
{
	unsigned char option;	/* Character to type to select */
	int optionptr;		/* Pointer to option character in name */
	char name[MAX_NAME];	/* Entire word for option */
	int curoppos;		/* Position in original string */
	int line;		/* Line option will display on */
	int column;		/* Column will display on */
};

#pragma member_alignment restore

/*
 * Scrolling structure
 */ 
#pragma member_alignment save
#pragma nomember_alignment

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

#pragma member_alignment restore

/********************************************************************
 * Prototype functions
 */

/*
 * NOTE: A function that returns a string in basic is defined in
 *   C as a void function with a string as thr first parameter through
 *   which the return value is passed.
 */

void assg_channel(long *ch, long *exit_status);
void assg_freechannel(long *ch);
long copy_copyrecords(struct dsc$descriptor_s *source_file,
	struct dsc$descriptor_s *dest_file);
long date_daycode(struct dsc$descriptor_s *datum);
long date_dayofweek(long *cod);
void date_invdcode(struct dsc$descriptor *returnstr, long *daycode);
void date_invmcode(struct dsc$descriptor *returnstr, long *monthcode);
long date_moncode(struct dsc$descriptor_s *datum);
void date_storedate(struct dsc$descriptor *returnstr,
	struct dsc$descriptor *xdeflt);
void date_today(struct dsc$descriptor *returnstr);
long dspl_screencapture(struct dsc$descriptor *mesg,
	long param);
long dspl_scroll(
	struct smg_scroll_struct *smg_scroll,
	struct dsc$descriptor_a *smg_array,
	long *smg_scope_exit,
	struct dsc$descriptor_s *smg_scroll_option);
void dspl_splitcursor(struct dsc$descriptor_s *curstr, long *rxxx, long *cyyy);
long entr_3entrystring(struct scope_struct *scope,
	long *smg_option, long *xflag, long *xlen,
	struct dsc$descriptor_s *retstring);
void entr_3message(struct scope_struct *scope,
	struct dsc$descriptor_s *mesg, long int *flag);
long entr_4entry(struct scope_struct *scope, long smg_option, long xflag);
long entr_4specialkeys(struct scope_struct *scope, long smg_option, long xflag, long retchar);
long entr_macro(struct scope_struct *scope);
int func_4scoseq(int xchar);
void help_34message(struct scope_struct *scope,
	struct dsc$descriptor_s *messages,
	struct dsc$descriptor_s *help_severity,
	struct dsc$descriptor_s *help_progname,
	struct dsc$descriptor_s *help_filename,
	struct dsc$descriptor_s *help_item);
void menu_3interrupt(struct scope_struct *scope);
unsigned long smg_put_virtual_display_encoded(
	long *pvdid, long *full_len, struct dsc$descriptor_s *coded_text,
	long *prow,  long *pcol, long *pwrap, long *pcset, long *vset);

#endif
