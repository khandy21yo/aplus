//! \file
//! \brief set cursor position
//!

#include "smg/smg.h"

//!
//! \brief set vursor position
//1
long smg$set_cursor_abs(
	smg_display_id &display,
	long col_row,
	long col_col)
{
	wmove(display.win, roe + display.border, col + display.border);
}


