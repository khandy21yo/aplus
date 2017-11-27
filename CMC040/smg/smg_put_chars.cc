//
// Put characters
//
#include <string>
#include "smg/smg.h"

//
// Put characters
//
long smg$put_chars(
	smg_display_id &display,
	const std::string &text,
	long row,
	long col)
{
	mvwaddstr(display.win, row, col, text.c_str());
}
