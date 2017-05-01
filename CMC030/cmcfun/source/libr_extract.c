/*	%TITLE "Extract a File from a Text Library"
 */
#pragma module libr_extract "V3.6 Calico"

/*
 *	COPYRIGHT (C) 1987 BY
 *	Computer Management Center, Inc.
 *	Idaho Falls, Idaho.
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
 * Computer Management Center, Inc.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
 * Abstract:HELP
 *	.p
 *	This function pulls text out of a library, and puts
 *	it into a specified text file.
 *
 * Parameters:
 *
 *	LIB_NAME$
 *		Passed name of the library to pull text from.
 *
 *	FILE_NAME$
 *		Passed name of file to write text into.
 *
 *	KEY_NAME$
 *		Passed key for text to be pulled.
 *
 *	Returns a status code.
 *
 * Example:
 *
 *	ST% = LIBR_EXTRACT("HELP_GL", "TEXT.FILE", "ADDRESS")
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:LIBR_EXTRACT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP LIBR_EXTRACT
 *	$ DELETE LIBR_EXTRACT.OBJ;*
 *
 * Author:
 *
 *	07/01/87 - Kevin Handy
 *
 * Modification history:
 *
 *	07/19/88 - Kevin Handy
 *		Removed defaulting to REF:
 *
 *	11/30/88 - Kevin Handy
 *		Modified to close library if open fails.
 *
 *	03/14/92 - Kevin Handy
 *		Clean up vars (checkvar)
 *
 *	07/14/93 - Kevin Handy
 *		Convert to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, smg$routines.h)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <descrip.h>
#include <string.h>

#include "func_include:library.h"

/*
 * Local variables
 */
static $DESCRIPTOR(tlb_text, ".TLB");

/*
 * Main Function
 */
long libr_extract(struct dsc$descriptor *lib_name,
	struct dsc$descriptor *file_name,
	struct dsc$descriptor *key_name)
{
	/*
	 * Local variables
	 */
	long txrfa[2];
	long retstatus;
	long st;
	unsigned long lr_index;
	char outputname[64];
	FILE *outputfile;

	struct dsc$descriptor inputdesc;
	struct dsc$descriptor actualdesc;
	char inputbuf[256];

	retstatus = 1;

	/*
	 * Set up the control structure if necessary
	 */
	st = lbr$ini_control(&lr_index, &LBR$C_READ);

	if ((st & 1) == 0)
	{
		return(st);
	}

	/*
	 * Open the library function
	 */
	st = lbr$open(&lr_index, lib_name, 0l, &tlb_text);

	if ((st & 1) == 0)
	{
		retstatus = st;
		goto CloseLibrary;
	}

	/*
	 * Search for key in file
	 */
	st = lbr$lookup_key(&lr_index, key_name, &txrfa);

	if ((st & 1) == 0)
	{
		retstatus = st;
		goto CloseLibrary;
	}

	/*
	 * Open up output file
	 */
	strncpy(outputname, file_name->dsc$a_pointer, file_name->dsc$w_length);
	outputname[file_name->dsc$w_length] = '\0';
	if ((outputfile = fopen(outputname, "w", "rfm=var", "rat=cr")) == 0)
	{
		retstatus = 0;
		goto CloseLibrary;
	}

	/*
	 * Copy over text
	 */
	inputdesc.dsc$a_pointer = inputbuf;
	inputdesc.dsc$w_length = sizeof(inputbuf);
	inputdesc.dsc$b_class = DSC$K_CLASS_S;
	inputdesc.dsc$b_dtype = DSC$K_DTYPE_T;

	while (((lbr$get_record(&lr_index, &inputdesc, &actualdesc)) & 1) != 0)
	{
		inputbuf[actualdesc.dsc$w_length] = '\n';
		inputbuf[actualdesc.dsc$w_length + 1] = '\0';
		fputs(inputbuf, outputfile);
	}

/*	str$free1_dx(&inputdesc); */

	/*
	 * Close library file
	 */
 CloseLibrary:
	lbr$close(&lr_index);
	fclose(outputfile);

	return(retstatus);
}
