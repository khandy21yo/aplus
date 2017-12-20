//! \file
//! \bried Functions to emulate SMG using curses and panels
//!
#ifndef _smg_h_
#define _smg_h_

#include <string>
#include <curses.h>
#include <panel.h>

//
// Constants
//
static const long TT$M_MECHFORM = 1;	//!< Terminal mechanical form feed
static const long TT$M_WRAP = 2;	//!< Terminal setting wraparound

static const int SMG$M_BOLD = 1;	//< Display in Bold
static const int SMG$M_BLINK = 4;	//< Blink
static const int SMG$M_BORDER = 2;	//< Display border around window
static const int SMG$M_CURSOR_OFF = 0;	//< Turn off cursor
static const int SMG$M_CURSOR_ON = 1;	//< Turn on cursor
static const int SMG$M_DOWN = 2;	//< scroll down
static const int SMG$M_NORMAL = 0;	//< Display in Normal
static const int SMG$M_UP = 1;		//< scroll up

static const int SMG$K_TRM_BUFFER_FULL = -9997;	//!< Buffer full error
static const int SMG$K_TRM_CANCELLED = -9998;	//!< Cancelled error
static const int SMG$K_TRM_COMMA = -9985;	//!< Keypad comma
static const int SMG$K_TRM_CTRLB = ('B' & 0x1f);	//!< Control/B
static const int SMG$K_TRM_CTRLC = ('C' & 0x1f);	//!< Control/C
static const int SMG$K_TRM_CTRLD = ('D' & 0x1f);	//!< Control/D
static const int SMG$K_TRM_CTRLE = ('E' & 0x1f);	//!< Control/E
static const int SMG$K_TRM_CTRLF = ('F' & 0x1f);	//!< Control/F
static const int SMG$K_TRM_CTRLH = ('H' & 0x1f);	//!< Control/H
static const int SMG$K_TRM_CTRLI = ('I' & 0x1f);	//!< Control/I
static const int SMG$K_TRM_CTRLL = ('L' & 0x1f);	//!< Control/L
static const int SMG$K_TRM_CTRLM = ('M' & 0x1f);	//!< Control/M
static const int SMG$K_TRM_CTRLN = ('N' & 0x1f);	//!< Control/N
static const int SMG$K_TRM_CTRLP = ('P' & 0x1f);	//!< Control/P
static const int SMG$K_TRM_CTRLR = ('R' & 0x1f);	//!< Control/R
static const int SMG$K_TRM_CTRLS = ('S' & 0x1f);	//!< Control/S
static const int SMG$K_TRM_CTRLT = ('T' & 0x1f);	//!< Control/T
static const int SMG$K_TRM_CTRLU = ('U' & 0x1f);	//!< Control/U
static const int SMG$K_TRM_CTRLW = ('W' & 0x1f);	//!< Control/W
static const int SMG$K_TRM_CTRLX = ('X' & 0x1f);	//!< Control/X
static const int SMG$K_TRM_CTRLZ = ('Z' & 0x1f);	//!< Control/Z
static const int SMG$K_TRM_DELETE = KEY_BACKSPACE;	//!< Delete key
static const int SMG$K_TRM_DO = KEY_F(16);	//!< DO (F16) key
static const int SMG$K_TRM_DOWN = KEY_DOWN;	//!< Down arrow
static const int SMG$K_TRM_ENTER = -9983;	//!< Keypad enter
static const int SMG$K_TRM_F6 = KEY_F(6);	//!< F6 key
static const int SMG$K_TRM_F7 = KEY_F(7);	//!< F7 key
static const int SMG$K_TRM_F8 = KEY_F(8);	//!< F8 key
static const int SMG$K_TRM_F9 = KEY_F(9);	//!< F9 key
static const int SMG$K_TRM_F10 = KEY_F(10);	//!< F10 key
static const int SMG$K_TRM_F11 = KEY_F(11);	//!< F11 key
static const int SMG$K_TRM_F12 = KEY_F(12);	//!< F12 key
static const int SMG$K_TRM_F13 = KEY_F(13);	//!< F13 key
static const int SMG$K_TRM_F14 = KEY_F(14);	//!< F14 key
static const int SMG$K_TRM_F15 = KEY_F(15);	//!< F15 key
static const int SMG$K_TRM_F17 = KEY_F(17);	//!< F17 key
static const int SMG$K_TRM_F18 = KEY_F(18);	//!< F18 key
static const int SMG$K_TRM_F19 = KEY_F(19);	//!< F19 key
static const int SMG$K_TRM_F20 = KEY_F(20);	//!< F20 key
static const int SMG$K_TRM_FIND = KEY_FIND;	//!< FIND key
static const int SMG$K_TRM_HELP = KEY_HELP;	//!< HELP key
static const int SMG$K_TRM_INSERT_HERE = KEY_IC;	//!< Insert key
static const int SMG$K_TRM_KP0 = -9987;		//!< Keypad 0
static const int SMG$K_TRM_KP1 = KEY_C1;	//!< Keypad 1
static const int SMG$K_TRM_KP2 = -9996;		//!< Keypad 2
static const int SMG$K_TRM_KP3 = KEY_C3;	//!< Keypad 3
static const int SMG$K_TRM_KP4 = -9995;		//!< Keypad 4
static const int SMG$K_TRM_KP5 = KEY_B2;	//!< Keypad 5
static const int SMG$K_TRM_KP6 = -9994;		//!< Keypad 6
static const int SMG$K_TRM_KP7 = KEY_A1;	//!< Keypad 7
static const int SMG$K_TRM_KP8 = -9993;		//!< Keypad 8
static const int SMG$K_TRM_KP9 = KEY_A3;	//!< Keypad 9
static const int SMG$K_TRM_LEFT = KEY_LEFT;	//!< Left arrow
static const int SMG$K_TRM_MINUS = -9986;	//!< Keypad minus
static const int SMG$K_TRM_NEXT_SCREEN = KEY_NEXT;	//!< Next
static const int SMG$K_TRM_PERIOD = -9984;	//!< Keypad period
static const int SMG$K_TRM_PF1 = -9992;		//!< Keypad 8
static const int SMG$K_TRM_PF2 = -9991;		//!< Keypad 8
static const int SMG$K_TRM_PF3 = -9989;		//!< Keypad 8
static const int SMG$K_TRM_PF4 = -9988;		//!< Keypad 8
static const int SMG$K_TRM_PREV_SCREEN = KEY_PREVIOUS;	//!< Previous
static const int SMG$K_TRM_REMOVE = KEY_DC;	//!< Remove key
static const int SMG$K_TRM_RIGHT = KEY_RIGHT;	//!< Right arrow
static const int SMG$K_TRM_SELECT = KEY_SELECT;	//!< Select Key
static const int SMG$K_TRM_TIMEOUT = -9999;	//!< Timeout error
static const int SMG$K_TRM_UNKNOWN = -9997;	//!< Unknown error
static const int SMG$K_TRM_UP = KEY_UP;		//!< Up arrow

//
//! \brief Pasteboard class
//
class smg_pasteboard_id
{
private:
};

//!
//! \brief \brief Virtual display class
//!
class smg_display_id
{
public:
	long border;	//!< 0 if no border, 1 if border
	WINDOW *win;	//!< curses window id
	PANEL *panel;	//!< curses panel id
	long rows;	//!< Number of rows
	long cols;	//!< Number of columns
	std::string label;	//!< Border label
};

//
//! \brief Keyboard id
//
class smg_keyboard_id
{
};

//
// smg Function names
//
long smg$begin_display_update(
	smg_display_id display);
int smg$change_pbd_characteristics(
	smg_pasteboard_id &pbid, 
	long screen_width, 
	long *smg_cols = 0, 
	long a = 0, 
	long *smg_rows = 0);
int smg$create_pasteboard(
	smg_pasteboard_id &smg_pbid,
	long a,
	long b,
	long *rows,
	long *cols);
long smg$create_virtual_display(
	long rows,
	long cols,
	smg_display_id &display,
	long flag = 0,
	long b = 0,
	long c = 0);
long smg$create_virtual_keyboard(
	smg_keyboard_id &kbid);
long smg$cursor_column(
	smg_display_id gisplay);
long smg$cursor_row(
	smg_display_id gisplay);
long smg$draw_line(
	smg_display_id &display,
	long row1,
	long column1,
	long row2,
	long column2);
long smg$end_display_update(
	smg_display_id display);
long smg$erase_display(
	smg_display_id &display);
long smg$erase_line(
	smg_display_id &window,
	int row,
	int col);
long smg$flush_buffer(
	smg_pasteboard_id &pbid);
long smg$get_broadcast_message(
	smg_pasteboard_id &pbid,
	std::string &text);
long smg$get_display_attr(
	smg_display_id &display,
	long *height = 0,
	long *width = 0);
long smg$label_border(
	smg_display_id &display,
	const std::string &&label);
long smg$paste_virtual_display(
	smg_display_id &display,
	smg_pasteboard_id &pbid,
	long row,
	long col,
	long a = 0);
long smg$pop_virtual_display(
	smg_display_id &display,
	smg_pasteboard_id &pbid);
long smg$put_chars(
	smg_display_id &display,
	const std::string &text,
	long row,
	long col,
	long eline = 0,
	long flags = 0,
	long comp = 0,
	long charset = 0);
long smg_put_virtual_display_encoded(
	smg_display_id &display,
	long length,
	const std::string &text,
	long row,
	long col,
	long a,
	long b,
	long charset);
long  smg$read_keystroke(
	smg_keyboard_id kbid,
	int &retchar,
	int a,
	int b,
	smg_display_id &display,
	int c,
	int d);
long smg$read_string(
	smg_keyboard_id &kbid,
	std::string &retstring,
	long a,
	long xlen,
	long modifier,
	long timeout,
	const char *term_set,
	long data_length,
	int &retchar,
	smg_display_id &option);
long smg$repaint_screen(
	smg_pasteboard_id &pbid);
long smg$ring_bell(
	smg_keyboard_id &vdid);
long smg$set_broadcast_trapping(
	smg_pasteboard_id &pbid,
	void (*fun)(void*,void*,void*,void*),
	void *);
long smg$set_cursor_abs(
	smg_display_id &display,
	long col_row,
	long col_col);
long smg$set_cursor_mode(
	smg_pasteboard_id &pbid,
	long mode);
long smg$set_term_characteristics(
	smg_pasteboard_id &pbid,
	long a,
	long b,
	long c,
	long d);


#endif

