//! \file
//! \brief Refresh screen
//!

#include "smg/smg.h"

//!
//! \brief refresh screen
//
long smg$repaint_screen(
	smg_pasteboard_id &pbid)
{
	// \TODO How do you force curses to redraw everything
	update_panels();
}
