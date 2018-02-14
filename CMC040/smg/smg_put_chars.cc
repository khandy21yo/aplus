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
	smg_xxx_set_attrs(display, flags);

	mvwaddstr(display.win,
		row + display.border - 1,
		col + display.border - 1,
		text.c_str());

	smg_xxx_reset_attrs(display, flags);

	return 1;
}

//!
//! \brief Turn on attribues according to the flags set
//!
//! Broken out from smg$put_chars because this is needed in
//! multiple places.
//!
void smg_xxx_set_attrs(
	smg_display_id &display,	//!< Display to write to
	long flags)			//!< Flags
{
	if (flags & SMG$M_BOLD)
	{
		wattron(display.win, A_BOLD);
	}
	if (flags & SMG$M_REVERSE)
	{
		wattron(display.win, A_REVERSE);
	}
	if (flags & SMG$M_UNDERLINE)
	{
		wattron(display.win, A_UNDERLINE);
	}
}
//!
//! \brief Turn off attribues according to the flags set
//!
//! Broken out from smg$put_chars because this is needed in
//! multiple places.
//!
void smg_xxx_reset_attrs(
	smg_display_id &display,	//!< Display to write to
	long flags)			//!< Flags
{
	if (flags & SMG$M_BOLD)
	{
		wattroff(display.win, A_BOLD);
	}
	if (flags & SMG$M_REVERSE)
	{
		wattroff(display.win, A_REVERSE);
	}
	if (flags & SMG$M_UNDERLINE)
	{
		wattroff(display.win, A_UNDERLINE);
	}
}
