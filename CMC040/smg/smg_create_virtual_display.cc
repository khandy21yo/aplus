//! \file
//! \brief Create virtual Display
//!
#include "smg/smg.h"


//!
//! \brief Create a virtual display
//!
//! NOTE: Actual display creation is delayed until the display is pasted.
//!
long smg$create_virtual_display(
	long rows,		//!< Number of rows (lines, height)
	long cols,		//!< Number of columns (characters, widrh)
	smg_display_id &display,	//!< Returned Display  created
	long flags,		//!< ??
	long b,			//!< ??
	long c)			//!< Flags applied to screm
{
	display.win = 0;
	display.panel = 0;
	display.rows = rows;
	display.cols = cols;
	display.label = "";

	if (flags & SMG$M_BORDER)
	{
		display.border = 1;
	}
	else
	{
		display.border = 0;
	}
	return 1;
}


