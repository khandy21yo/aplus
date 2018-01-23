//! \file
//! \brief set cursor position
//!

#include "smg/smg.h"

//!
//! \brief set vursor position
//1
long smg$set_cursor_abs(
	smg_display_id &display,
	long row,
	long col)
{
	wmove(display.win, row + display.border, col + display.border);
	update_panels();
	doupdate();
}


