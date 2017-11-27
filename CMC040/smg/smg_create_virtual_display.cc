//
// Create virtual Display
//
#include "smg/smg.h"


//
//Create a virtual display
//
// NOTE: Actual display creation is delayed until the display is pasted.
//
long smg$create_virtual_display(
	long rows,
	long cols,
	smg_display_id &display,
	long a,
	long b,
	long c)
{
	display.rows = rows;
	display.cols = cols;

	return 1;
}


