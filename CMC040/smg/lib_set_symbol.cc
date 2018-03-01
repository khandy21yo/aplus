//! \file
//! \brief Set symbol
//!

#include <cstdlib>
#include <string>

#include "smg/smg.h"

//!
//! \brief set symbol
//!
long lib$set_symbol(
	const std::string &symbol,
		//!< Symbol to set
	const std::string &value)
		//!< Value to set
{
	setenv(symbol.c_str(), value.c_str(), 1);
	return 1;
}
