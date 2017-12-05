//! \file
//! \brief Get cursor column
//

include "smg/smg.h"

//!
//! \brief get cursor column
//!
long smg$cursor_column(
	smg_display_id gisplay)
{
	int x, y;
	getyx(display, y, x);
	return x;
}

