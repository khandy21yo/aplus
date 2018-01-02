/*	%TITLE "Subroutine to Search for a Files Existance"
 */
#pragma module find_fileexists "V3.6 Calico"

#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

/*
 *
 * Abstract:HELP
 *	.p
 *	This function will tell if a file exists matching
 *	a given filename or not.  If wildcards are passes,
 *	will tell if any file matches specification.
 *
 * Parameters:
 *
 *	FWILD
 *		Passed file name to search for.
 *
 *	FLAGS
 *		Special flags.  Currently not used.
 *
 *
 *	FIND_FILEEXISTS
 *		Returns 1 if file exists, 0 otherwise.
 *
 * Author:
 *
 *	10/26/87 - Kevin Handy
 *
 *--
 */


long find_fileexists(
	const std::string wildf,
	long flag)
{
	struct stat buffer;
	long xstat = stat (wildf.c_str(), &buffer); 

	if (xstat == 0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}
