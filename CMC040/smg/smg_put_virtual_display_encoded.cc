//! \file
//! \brief  Put characters encoded
//!
#include <string>
#include "smg/smg.h"

//!
//! \brief Put characters encoded
//!
//! The passed text string contains:
//! - The text to be displayed
//! - Encoded attribute flags
//!   - 2 bytes for start position
/?!   - 2 bytes for length
//!   - 1 byte for attributes
//! - Lenfth of encoded parts
//!   - 2 bytes for length of encoded portion, including these two butes.
//!
//! \todo Currently ignores the "encoded" part.
//!
long smg_put_virtual_display_encoded(
	smg_display_id &display,	//!< Display to write to
	long length,			//!< Length
	const std::string &text,	//!< Text to write
	long row,			//!< Row to dosplay at
	long col,			//!< Column to display at
	long a,				//!< ???
	long b,				//!< ???
	long charset)			//!< Character set
{
	//!
	//! \todo Needs to handle the encoded part
	//!
	mvwaddstr(display.win,
		row + display.border,
		col + display.border,
		text.c_str());
	return 1;
}



