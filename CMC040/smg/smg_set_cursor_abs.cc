//! \file
//! \brief set cursor position
//!

#include <iostream>
#include <assert.h>
#include "smg/smg.h"

//!
//! \brief set vursor position
//1
long smg$set_cursor_abs(
	smg_display_id &display,
	long row,
	long col)
{
	int status;

	assert(display.win != 0);

	status = wmove(display.win, row + display.border - 1,
		col + display.border - 1);
	top_panel(display.panel);

	assert (status != ERR);

	update_panels();
	doupdate();

	return 1;
}


