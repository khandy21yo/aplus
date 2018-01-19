//! \file
//! \brief Paste virtual display
//!

#include "smg/smg.h"

//!
//! \brief Paste virtual display
//!
//! NOTE: We acutally do the curses window creatiion here
//!
long smg$paste_virtual_display(
	smg_display_id &display,	//!< Display being pastes
	smg_pasteboard_id &pbid,	//!< Pasteboard
	long row,			//!< Row to paste to
	long col,			//!< Column to paste to
	long a)				//!< ??
{
	display.win = newwin(display.rows + display.border * 2,
		display.cols + display.border * 2,
		row - display.border - 1,
		col - display.border - 1);
	display.panel = new_panel(display.win);

	smg_xxx_draw_border(display);

	return 1;
}

//!
//! \brief draw border around screen when necessary
//!
long smg_xxx_draw_border(
	smg_display_id &display)	//!< Display being pastes
{
	if (display.border)
	{
		box(display.win, 0, 0);
	}
}

