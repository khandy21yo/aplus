//
// Functions to emulate SMG using curses and panels
//
#ifndef _smg_h_
#define _smg_h_

#include <curses.h>
#include <panel.h>

//
// Pasteboard class
//
class smg_pasteboard_id
{
private:
};

//
// Virtual display class
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
// Keyboard id
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
long smg$set_cursor_mode(
	smg_pasteboard_id &pbid,
	long mode);



#endif

