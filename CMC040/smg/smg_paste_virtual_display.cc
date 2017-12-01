//
// Paste virtual display
//

#include "smg/smg.h"

//
// Paste virtual display
//
// NOTE: We acutally do the curses window creatiion here
//
long smg$paste_virtual_display(
	smg_display_id &display,
	smg_pasteboard_id &pbid,
	long row,
	long col,
	long a)
{
	display.win = newwin(display.rows, display.cols, row, col);
	display.panel = new_panel(display.win);

	return 1;
}

