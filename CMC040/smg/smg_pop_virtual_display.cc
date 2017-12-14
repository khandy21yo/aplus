//! \file
//! \brief pop a virtual display
//!

include <string>
include <smg/smg.h>

//!
//! \brief pop virtual display
//
long smg$pop_virtual_display(
	smg_display_id &display,
	smg_pasteboard_id &pbid)
{
	del_panel(display.pan);
	delwin(display.win);
}
