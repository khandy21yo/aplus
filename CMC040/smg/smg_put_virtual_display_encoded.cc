//! \file
//! \btief  Put characters encoded
//!
#include <string>
#include "smg/smg.h"

//!
//! \brief Put characters encoded
//!
//! \todo Currently ignores the "encoded" part.
//!
long smg_put_virtual_display_encoded(
	smg_display_id &display,	//!< Display to write to
	long length,
	const std::string &text,	//!< Text to write
	long row,			//!< Row to dosplay at
	long col,			//!< Column to display at
	long a,
	long b,
	long charset)
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



