//! \file
//! \brief  Put characters
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
	long col,			//!< Column to display at
	long eline,			//!< erase line
	long flags,			//!< Flags
	long comp,			//!< ???
	long charset)			//!< Character set
{
	if (flags & SMG$M_BOLD)
	{
		wattron(display.win, A_BOLD);
	}

#if 1
	mvwaddstr(display.win,
		row + display.border - 1,
		col + display.border - 1,
		text.c_str());
#else
	wmove(display.win,
		row + display.border - 1,
		col + display.border - 1);
	waddstr(display.win,
		text.c_str());
#endif

	if (flags & SMG$M_BOLD)
	{
		wattroff(display.win, A_BOLD);
	}
}
