/*	%TITLE "Subroutine to Search for a Group of Files"
 */
#pragma module find_file "V3.6 Calico"

/*
 *
 *++
 *
 * Abstract:HELP
 *	.b
 *	.lm +5
 *	This subroutine will locate files of the requested group
 *	(wildcards allowed). It will put the info on each file in
 *	string form in the array ALIST(). The number of items found
 *	will be in ALIST(0). LPREF is the length of the prefix of
 *	the filename and LSUFF is the length of the suffix of the
 *	filename. If LPREF and/or LSUFF are greater than 0 and FLAGs
 *	are set to match the length of LPREF and/or LSUFF.
 *	.b
 *	FLAG RESULT
 *	========================================================
 *	.table 3,25
 *	.te
 *	0	Full file specification(FSCN$_FILESPEC)
 *	.te
 *	1	Node name; includes two colons and the access control string(if specified).
 *	.te
 *	2	Device name; includes colon.
 *	.te
 *	4	Root directory; includes colon.
 *	.te
 *	8	Directory name; includes brackets (or angle brackets).
 *	.te
 *	16	File name; includes quotation marks (if any).
 *	.te
 *	32	File type; includes period.
 *	.te
 *	64	Version number; includes semicolon (or period).
 *	.end table
 *	ALIST() is an array(no matter the size) passed in as follows:
 *	CALL FIND_FILE("[*]*.*;*", A$(), 0)
 *	.lm -5
 *
 *--
 */

/*
 * Include files
 */
#include <iostream>
#include <string>
#include <cstring>
#include <vector>
#include <glob.h>
#include <libgen.h>

/*
 * Main Function
 */
void find_file(
	const std::string &wildf,
	std::vector<std::string> &alist,
	int flag,
	const std::string &prefix,
	const std::string &suffix)
{
	/*
	 * Local variables
	 */
	long status;

	alist.clear();
	alist.push_back("0");

	/*
	 * Look up files until we run out of files, or run out of
	 * array elements.
	 */
	glob_t pglob;
	std::string file_name;

	glob(wildf.c_str(),
		GLOB_MARK | GLOB_TILDE,
		0 /* errfunc */,
		&pglob);

	for (int loop = 0; loop < pglob.gl_pathc; loop++)
	{
		file_name = pglob.gl_pathv[loop];

// std::cerr << "Glob filename: " << file_name << std::endl;

		/*
		 * If we only want directory portion,
		 * or only the name portion.
		 * This is an approximation of the VMS stuff.
		 */
		if ((flag != 0) &&
			((flag & (1 | 2 | 4 | 8)) != 0) &&
			((flag & (16 | 32 | 64)) == 0))
		{
			//
			// We need a local copy necause dirname can modify it
			//
			char temp_name[file_name.size() + 1];
			strcpy(temp_name, file_name.c_str());
			file_name = dirname(temp_name);
		}
		else if ((flag != 0) &&
			((flag & (1 | 2 | 4 | 8)) == 0) &&
			((flag & (16 | 32 | 64)) != 0))
		{
			//
			// We need a local copy necause basename can modify it
			//
			char temp_name[file_name.size() + 1];
			strcpy(temp_name, file_name.c_str());
			file_name = basename(temp_name);
		}
		//
		// Maybe strip off pregix.
		//
		if (prefix.size() != 0 &&
			file_name.compare(0, prefix.size(), prefix) == 0)
		{
			file_name.erase(0, prefix.size());
		}
		//
		// Maybe strip off suffix
		//
		if (suffix.size() != 0 &&
			file_name.size() >= suffix.size() &&
			file_name.compare(file_name.size() - suffix.size(), 
			suffix.size(), suffix) == 0)
		{
			file_name.erase(file_name.size() - suffix.size(), 
				suffix.size());
		}
// std::cerr << "Glob filename2: " << file_name << std::endl;
		alist.push_back(file_name);
	}

	/*
	 * Finish up directory call
	 */
	globfree(&pglob);

 ExitProgram:
	/*
	 * Code in number of files loaded
	 *
	 * NOTE: They caller could use the .size() operator, but
	 * the old basic source didn't have that option.
	 */
	alist[0] = std::to_string(alist.size() - 1);
}

