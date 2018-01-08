//! \file
////! \brief end display update
////!

#include <string>

#include "preferences.h"
#include "cmcfun.h"
#include "smg/smg.h"

//!
//! \brief Begin display update
//!
long smg$end_display_update(
	smg_display_id &display)		//! Display to update
{
	update_panels();
}



