//! \file
//! \brief delete a virtual display
//!

include <string>
include <smg/smg.h>

//!
//! \brief delete virtual display
//
long smg$delete_virtual_display(
	smg_display_id &display)	//!< Display to delete
{
	del_panel(display.pan);
	delwin(display.win);
}
