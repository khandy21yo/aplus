//
// Flush smg buffers
//

#include "smg/smg.h"

//
// Flush buffers
//
long smg$flush_buffer(
	smg_pasteboard_id &pbid)
{
	update_panels();
	doupdate();
	return 1;
}
