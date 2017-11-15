/*	%TITLE "Insert a Text File into a Library"
 */
#pragma module libr_3insert "V3.6 Calico"

/*
 *
 *++
 *
 * Abstract:HELP
 *	.p
 *	This function will insert text into a library,
 *	and append a key to that text.
 *
 *	WARNING: This version should only be used when there is
 *	no chance of connected items.
 *
 * Parameter:
 *
 *	LIB_NAME$
 *		Passed name of library to insert text into.
 *
 *	FILE_NAME$
 *		Passed name of file containing text to insert
 *		into the library.
 *
 *	KEY_NAME$
 *		Passed name of key to append to text.
 *
 *	Returns a status code.
 *
 * Example:
 *
 *	ST% = LIBR_3INSERT("HELP_GL", "SOURCE.FILE", "ADDRESS")
 *
 *--
 */

/*
 * Include files
 */
#include <iostream>
#include <string>
#include "cmcfun.h"

#include "preferences.h"

/*
 * Main function
 */
long libr_3insert(const std::string &lib_name,
	const std::string &file_name,
	const std::string &key_name)
{
	int st = 0;		// Return status

	//
	// Just dump out a message for the moment
	//
	std::cerr << "libr_3insert: Lib=" << lib_name <<
		", source=" << file_name <<
		", key=" << key_name <<
		std::endl;

	/*
	 * Return status
	 */
	return(st);
}
