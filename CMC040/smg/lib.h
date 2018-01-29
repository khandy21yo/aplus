//! \file
//! \brief Library functions
//!
#ifndef _lib_h_
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
static long lib$delete_file(
	std::string name)
{
	unlink(name.c_str());
	return 0;
}

//!
//! \brief Get sumbol
//!
static long lib$get_symbol(
	const std::string &symbol,
	std::string &value,
	int a,
	int b)
{
	char *result = getenv(symbol.c_str());
	if (result != 0)
	{
		value = result;
		return 1;
	}
	else
	{
		value = "";
		return 0;
	}
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
	return 1;
}
#endif

