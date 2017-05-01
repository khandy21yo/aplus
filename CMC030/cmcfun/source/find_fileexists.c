/*	%TITLE "Subroutine to Search for a Files Existance"
 */
#pragma module find_fileexists "V3.6 Calico"

/*
 *
 * COPYRIGHT (C) 1987 BY
 * Computer Management Center
 * Idaho Falls, Idaho.
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies therof may not be provided or otherwise made
 * available to any other person.  No title to and ownership of
 * the software is hereby transferred.
 *
 * The information in this software is subject to change without
 * notice and should not be construed as a committment by
 * Computer management Center.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
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
 * Compile:
 *
 *	$ CC FUNC_SOURCE:FIND_FILEEXISTS/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP FIND_FILEEXISTS
 *	$ DELETE FIND_FILEEXISTS.OBJ;*
 *
 * Author:
 *
 *	10/26/87 - Kevin Handy
 *
 * Modification history:
 *
 *	10/28/87 - Kevin Handy
 *		Removed map of NAME.BUFFER$.  Unecessary.
 *
 *	08/20/90 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, lib$routines.h)
 *--
 */

#include <lib$routines.h>
#include <descrip.h>

long find_fileexists(struct dsc$descriptor_s *wildf, long *flag)
{
	long context, sys_status, ffl;
	struct dsc$descriptor_s name_buffer = {0,DSC$K_DTYPE_T,DSC$K_CLASS_S,0};

	context = 0;

	/*
	 * Look up one file
	 */
	sys_status = lib$find_file(wildf, &name_buffer, &context);

	if ((sys_status & 1) == 0)
	{
		ffl = 0;
	}
	else
	{
		ffl = 1;
	}

	sys_status = lib$find_file_end(&context);

	return(ffl);
}
