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
//!   - 2 bytes for length
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
	long text_size= text.size();
	long emb_size =
		(text[text_size - 2] & 255) +
		((text[text_size - 1] & 255) << 8);
	long text_len = text_size -emb_size;

	//
	// Display text in normal mode
	//
	mvwaddstr(display.win,
		row + display.border,
		col + display.border,
		text.substr(0,text_len).c_str());

	//
	// Handle embeded sections
	//
	for (int loop = 0; loop < emb_size - 2; loop += 5)
	{
		long part_start =
			(text[text_len  + loop + 0] & 255) +
			((text[text_len + loop + 1] & 255) << 8);
		long part_len =
			(text[text_len  + loop + 2] & 255) +
			((text[text_len + loop + 3] & 255) << 8);
		long part_flags =
			(text[text_len  + loop + 4] & 255);

		smg_xxx_set_attrs(display, part_flags);

		mvwaddstr(display.win,
			row + display.border,
			col + display.border + part_start - 1,
			text.substr(part_start - 1,part_len).c_str());

		smg_xxx_reset_attrs(display, part_flags);
	}

	return 1;
}



