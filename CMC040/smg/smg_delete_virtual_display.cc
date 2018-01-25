//! \file
//! \brief delete a virtual display
//!

#include <assert.h>
#include <smg/smg.h>

//!
//! \brief delete virtual display
//
long smg$delete_virtual_display(
	smg_display_id &display)	//!< Display to delete
{
	assert(display.panel != 0);

	if (display.panel != 0)
	{
		del_panel(display.panel);
		delwin(display.win);
	}
	display.panel = 0;
	display.win = 0;

	return 1;
}
