//! \file
//! \brief get display attributes

#include "smg/smg.h"

//!
//! \brief get display attributes
//!
long smg$get_display_attr(
	smg_display_id &display,
	long *height,
	long *width)
{
	if (height)
	{
		*height = display.rows;
	}
	if (width
	{
		width = display.cols;
	}

	return 1;
}


