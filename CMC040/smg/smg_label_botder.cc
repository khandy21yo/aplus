//! \file
//! \brief Label a virtual display
//!!

#include "smg/smg.h"

//!
//! \brief label a virtual display
//!
//! Needs the SMG$M_BORDER attribute
//!
long smg$label_border(
	smg_display_id &display,	//!< Display to be labeled
	const std::string &label,	//!< Label to apply
	long label_pos)			//!< Position of label
{
	display.label = label;
	return 1;
}

