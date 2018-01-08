//! \file
//! \brief pop a virtual display
//!

#include <string>
#include <smg/smg.h>

//!
//! \brief pop virtual display
//
long smg$pop_virtual_display(	
	smg_display_id &display,	//!< Display to pop
	smg_pasteboard_id &pbid)	//!< Pasteboard to pop from
{
	del_panel(display.panel);
	delwin(display.win);
}
