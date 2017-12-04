//! \file
//! \brief erase display
//

#include "smg/smg.h"

//!
//! \brief Erase display
//!
long smg$erase_display(
	smg_display_id &display)	//!< Display to erase
{
	werase(display.win);
}
