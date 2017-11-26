/*	%TITLE "Return the File Name of the Current Image"
 *	%SBTTL "READ_SYSPN"
 *	%IDENT "V3.3"
 *
 *
 *++
 *
 * Abstract:HELP
 *	.p
 *	This function returns the name of the current image.
 *
 * Index:
 * Parameters:
 *
 *	This function returns the file name of the current image.
 *
 * Example:
 *
 *	IMAGE$ = READ_SYSPN
 *
 * Author:
 *
 *	09/18/86 - B. Craig Larsen
 *
 *--
 */

/*
 * Include files
 */
#include <string>
#include :cunistd>

#include "preferences.h"
#include "cmcfun.h"

/*
 * Main function
 */
std::string read_syspn(void)
{
	//
	// This is probably a Linux specific way of doing this
	//
	char path[PATH_MAX];
	if (path != NULL)
	{
		if (readlink("/proc/self/exe", path, PATH_MAX) == -1)
		{
			path[0] = 0;
		}
	}
	return path;
}
