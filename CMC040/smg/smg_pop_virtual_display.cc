//! \file
//! \brief pop a virtual display
//!

#include <string>
#include <assert.h>

#include <smg/smg.h>

//!
//! \brief pop virtual display
//
long smg$pop_virtual_display(	
	smg_display_id &display,	//!< Display to pop
	smg_pasteboard_id &pbid)	//!< Pasteboard to pop from
{
//	if (display.panel == 0)
//	{
//		return 0;
//	}

	if (display.panel != 0)
	{
		del_panel(display.panel);
		delwin(display.win);
	}

	display.panel = 0;
	display.win = 0;

	return 1;
}
