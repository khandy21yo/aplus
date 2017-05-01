/* %TITLE "Copy file function"
 */
#pragma module copy_copyrecords "V3.6 Calico"

/*
 * Copyright (C) 1995 by Software Solutions
 * Idaho Falls, Idaho  83402
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies thereof may not be provided or otherwise made
 * available to any other person.  No title to and ownership of
 * the software is hereby transferred.
 *
 * The information in this software is subject to change without
 * notice and should not be construed as a commitment by
 * Software Solutions.
 *
 * Software Solutions  assumes no responsibility for the use or
 * reliability of its software on equipment which is not supported
 * by Software Solutions.
 *
 * ++
 *
 * Abstract:HELP
 *	.b
 *	.lm +5
 *	This function copies one file into a ^*new\* file
 *	on a record by record basis (restructuring the file).
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	SOURCE_FILE$
 *		The file to read the records from.
 *
 *	DEST_FILE$
 *		The file to be created.
 *
 *	Returned value
 *		A status of 1 if returned when sucussful, and
 *		a VMS status code otherwise.
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:COPY_COPYRECORDS
 *	$ LIB FUNC_LIB:CMCFUN/REP COPY_COPYRECORDS
 *	$ DELETE COPY_COPYRECORDS.OBJ;*
 *
 * AUTHOR:
 *
 *	11/19/93 - Kevin Handy
 *
 * MODIFICATION HISTORY:
 *
 *	03/07/95 - Kevin Handy
 *		Rewrote under C
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Coding Standards.
 *
 *	05/26/99 - Kevin Handy
 *		Modified to compile cleanly with DEC-C
 *		(#module)
 * --
 */

/*
 * Include files
 */
#include <descrip.h>

/*
 * External functions
 */
#if 1
extern long conv$pass_files(struct dsc$descriptor_s *source,
	struct dsc$descriptor_s *dest);
extern long conv$pass_options();
extern long conv$convert();
#else
#include <conv$routines.h>
#include <convdef.h>
#endif

/*
 * Main function
 */
long copy_copyrecords(struct dsc$descriptor_s *source_file,
	struct dsc$descriptor_s *dest_file)
{
	long status;

	/*
	 * Set up file names to be copied
	 */
	status = conv$pass_files(source_file, dest_file);
	if ((status & 1) == 0)
	{
		return status;
	}

	/*
	 * Set up convert options (I'm just going to use the defaults)
	 */
	status = conv$pass_options();
	if ((status & 1) == 0)
	{
		return status;
	}

	/*
	 * Do the actual copy
	 */
	status = conv$convert();
	return status;
}
