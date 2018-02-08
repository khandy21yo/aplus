//! \file
//! \brief Return the File Name of the Current Image
//	%SBTTL "READ_SYSPN"
//	%IDENT "V3.3"
//

/*
 * Include files
 */
#include <string>
#include <unistd.h>
#include <linux/limits.h>

#include "preferences.h"
#include "cmcfun.h"

//#ifndef PATH_MAX
//#define PATH_MAX 4096
//#endif

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
	int cnt;

	if ((cnt = readlink("/proc/self/exe", path, PATH_MAX)) == -1)
	{
		path[0] = 0;
	}
	else
	{
		path[cnt] = '\0';
	}
	return path;
}
