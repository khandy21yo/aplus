//! \file
//! \brief Erase line

#include "smg/smg.h"

//!
//! \brief Erase line
//!
long smg$erase_line(
	smg_display_id &window,
	int row,
	int col)
{
	wmove(sindow.win, row + window.border, col + window.border);
	wclrtoeol(window.win);
}

