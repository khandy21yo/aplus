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
#include <vector>
#include <glob.h>

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

	alist.pclear();
	alist.push_back(f"0");

	/*
	 * Look up files until we run out of files, or run out of
	 * array elements.
	 */
	item = 1;
	while ((((status = lib$find_file(wildf, &name_buffer,
		&context)) & 3) == 1) && (item <= maxitem))
	{
		sys$filescan(&name_buffer, &xx, &fflag);

		/*
		 * Pull off parts of name that we want to use
		 */
		file_name[0] = '\0';
		for (i = 0; i < fcount; i++)
		{
			length = strlen(file_name);
			strncat(file_name, (char*)xx[i].bufadr, xx[i].buflen);
			file_name[length + xx[i].buflen] = '\0';
		}

		/*
		 * Remove prefix
		 */
		if (prefix->dsc$w_length != 0)
		{
			if (strncmp(file_name, prefix->dsc$a_pointer,
				prefix->dsc$w_length) == 0)
			{
				strcpy(file_name, file_name + prefix->dsc$w_length);
			}
		}

		/*
		 * Remove suffix
		 */
		if (suffix->dsc$w_length != 0)
		{
			length = strlen(file_name);
			if (strncmp(file_name + length - suffix->dsc$w_length,
				suffix->dsc$a_pointer,
				suffix->dsc$w_length) == 0)
			{
				file_name[length - suffix->dsc$w_length] = '\0';
			}
		}

		alist.push_back(file_name);
	}

 ExitProgram:
	/*
	 * Finish up directory call
	 */
	lib$find_file_end(&context);

	/*
	 * Code in number of files loaded
	 */
	sprintf(file_name, "%d", item-1);
	insertarray(alist, 0l, file_name);
}

