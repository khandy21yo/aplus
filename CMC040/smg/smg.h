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
static const long TT$M_MECHFORM = 1;
static const long TT$M_WRAP = 2;

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

