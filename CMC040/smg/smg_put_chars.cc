//! \file
//! \btief  Put characters
//!
#include <string>
#include "smg/smg.h"

//!
//! \brief Put characters
//!
long smg$put_chars(
	smg_display_id &display,	//!< Display to write to
	const std::string &text,	//!< Text to write
	long row,			//!< Row to dosplay at
	long col,			//! Column to display at
	long eline,			//! erase line
	long flags,			//!< Flags
	long a)				//< ???
{
	if (flags & SMG$M_BOLD)
	{
		attron(A_STANDOUT);
	}

	mvwaddstr(display.win,
		row + display.border,
		col + display.border,
		text.c_str());

	if (flags & SMG$M_BOLD)
	{
		attroff(A_STANDOUT);
	}
}
