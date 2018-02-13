//! \file
//! \brief Get Record
//!

#include <string>

#include "smg/lbr.h"

//!
//! \brief Get Record
//!
long lbr$get_record(
	lbr_index_cdd &lr_index,
	std::string &text)
{
	//
	// Nothing left
	//
	if (lr_index.datum.size() == 0)
	{
		text = "";
		return 0;
	}

	//
	// Pull off one line of text
	//
	int pos = lr_index.datum.find('\n');
	if (pos == std::string::npos)
	{
		text = lr_index.datum;
		lr_index.datum.empty();
	}
	else
	{
		text = lr_index.datum.substr(0, pos);
		lr_index.datum.erase(0, pos + 1);
	}

	return 1;
}
