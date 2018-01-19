//
// Create pasteboards
//
#include "smg/smg.h"

//
// Create pasteboard
//
// curses doesn't give us the anility to change rows/columns like smg does.
//
long smg$create_pasteboard(
	smg_pasteboard_id &smg_pbid,
	long a,
	long b,
	long *smg_rows,
	long *smg_cols)
{
	initscr();
	cbreak();
	noecho();

	if (smg_rows != 0)
	{
		*smg_rows = LINES;
	}
	if (smg_cols != 0)
	{
		*smg_cols = COLS;
	}

	return 1;
}
