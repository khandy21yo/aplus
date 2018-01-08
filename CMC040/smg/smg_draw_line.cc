//! \file
//! \brief draw line
//!

#include "smg/smg.h"

//!
//! \brief dra line
//!
long smg$draw_line(
	smg_display_id &display,
	long row1,
	long column1,
	long row2,
	long column2)
{
	if (row1 == row2)
	{
		mvwhline(display.win,
			row1 + display.border, column1 + display.border,
			ACS_HLINE, column2 - column1 + 1);
	}
	else if (column1 == column2)
	{
		mvwvline(display.win,
			row1 + display.border, column1 + display.border,
			ACS_VLINE, row2 - row1 + 1);
	}
	else
	{
		return -99;
	}

	return 1;
}

