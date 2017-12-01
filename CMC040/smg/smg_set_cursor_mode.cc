// \file
// \brief Set cursor mode
//

#include "smg/smg.h"

//
// \brief Set cursor mode
//
long smg$set_cursor_mode(
	smg_pasteboard_id &pbid,
	long mode)
{
	if (mode == 1)
	{
		curs_set(0);
	}
	else
	{
		curs_set(1);
	}

	return 1;
}

