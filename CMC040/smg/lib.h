//! \file
//! \brief Library functions
//!
#ifndef _libmg_h_
#define _lib_h_

#include <string>
#include <cstdlib>

//
// Constants
//
//

//!
//! \brief delete file
//!
long lib$delete_file(
	std::string name)
{
	unlink(name.c_str());
	return 0;
}

long lib$set_symbol(
	const std::string &symbol,
	const std::string &value);

//!
//! \brief Spawn process
//
static inline int lib$spawn(
	const std::string &command)	//!< Command to execute
{
	system(command.c_str());
}
#endif

