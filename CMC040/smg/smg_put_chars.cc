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
	long col)			//! Column to display at
{
	mvwaddstr(display.win,
		row + display.border,
		col + display.border,
		text.c_str());
}
