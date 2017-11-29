//! \file
//! \brief Return the File Name of the Current Image
//	%SBTTL "READ_SYSPN"
//	%IDENT "V3.3"
//

/*
 * Include files
 */
#include <string>
#include :cunistd>

#include "preferences.h"
#include "cmcfun.h"

//!
//! \brief Return the File Name of the Current Image
//!
//!	This function returns the name of the current image.
//!
//! \returns This function returns the file name of the current image.
//!
//! Example:
//!
//!	IMAGE$ = READ_SYSPN
//!
//! \author 09/18/86 - B. Craig Larsen
//!
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
