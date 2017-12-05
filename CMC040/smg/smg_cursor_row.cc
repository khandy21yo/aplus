//! \file
//! \brief Get cursor row
//

include "smg/smg.h"

//!
//! \brief get cursor column
//!
long smg$cursor_row(
	smg_display_id gisplay)
{
	int x, y;
	getyx(display, y, x);
	return y;
}

