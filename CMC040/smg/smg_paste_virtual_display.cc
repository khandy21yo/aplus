//! \file
//! \btief Paste virtual display
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
		row - display.border,
		col - display.border);
	display.panel = new_panel(display.win);

	return 1;
}

