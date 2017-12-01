//
// Change pasteboard characteristics
//

#include "smg/smg.h"

//
// Change pasteboard characteristics
//
// NOTEL Curses doesn't have a swict screen width command.
//
int smg$change_pbd_characteristics(
	smg_pasteboard_id &pbid, 
	long screen_width, 
	long *smg_cols, 
	long a, 
	long *smg_rows)
{
	if (smg_rows != 0)
	{
		*smg_rows = LINES;
	}
	if (smg_cols != 0)
	{
		*smg_cols = COLS;
	}
}

