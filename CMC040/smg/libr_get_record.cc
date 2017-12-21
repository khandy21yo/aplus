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
	if lr_index.value.size() == 0)
	{
		text = "";
		return 0;
	}

	//
	// Pull off one line of text
	//
	int pos = lr_index.find('\n');
	if (pos == std::string::npos)
	{
		text = lr_index.value;
		lr_index.value.empty();
	}
	else
	{
		text = lr_index.substr(0, pos - 1);
		lr_index.erase(0, npos);
	}

	return 1;
}
