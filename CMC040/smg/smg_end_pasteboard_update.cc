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
long smg$end_pasteboard_update(
	smg_pasteboard_id &pasteboard)		//! Display to update
{
	update_panels();
	doupdate();
}



