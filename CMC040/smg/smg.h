//
// Functions to emulate SMG using curses and panels
//
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
static const int SMG$M_NORMAL = 0;	//< Display in Normal
static const int SMG$M_BORDER = 2;	//< Display border around window

static const int SMG$K_TRM_CTRLZ = ('Z' & 0x1f);	//!< Control/Z
static const int SMG$K_TRM_DOWN = KEY_DOWN;	//!< Down arrow
static const int SMG$K_TRM_F10 = KEY_F(10);	//!< F10 key
static const int SMG$K_TRM_UP = KEY_UP;		//!< Up arrow

//
//! \brief Pasteboard class
//
class smg_pasteboard_id
{
private:
};

//
//! \brief Virtual display class
//
class smg_display_id
{
public:
	long border;	// 0 if no border, 1 if border
	WINDOW *win;	// curses window id
	PANEL *panel;	// curses panel id
	long rows;	// Number of rows
	long cols;	// Number of columns
};

//
//! \brief Keyboard id
//
class smg_keyboard_id
{
};

//
// Key definitions
//


//
// Function names
//
int smg$change_pbd_characteristics(
	smg_pasteboard_id &pbid, 
	long screen_width, 
	long *smg_cols, 
	long a, 
	long *smg_rows);
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
	long a,
	long b,
	long c);
long smg$paste_virtual_display(
	smg_display_id &display,
	smg_pasteboard_id &pbid,
	long row,
	long col,
	long a);
long smg$create_virtual_keyboard(
	smg_keyboard_id &kbid);
long smg$flush_buffer(
	smg_pasteboard_id &pbid);
long smg$get_broadcast_message(
	smg_pasteboard_id &pbid,
	std::string &text);
long smg$put_chars(
	smg_display_id &display,
	const std::string &text,
	long row,
	long col);
long smg$set_broadcast_trapping(
	smg_pasteboard_id &pbid,
	void (*fun)(void*,void*,void*,void*),
	void *);
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

