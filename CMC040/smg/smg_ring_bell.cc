//! \file
//! \brief ring be;;
//!

#include "smg/smg.h"

//!
//! \brief ring bell
//!
long smg$ring_bell(
	smg_keyboard_id &vdid)
{
	beep();
	return 1;
}

