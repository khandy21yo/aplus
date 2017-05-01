/*	%TITLE "No Digital Standard Runoff for Text Array"
 */
#pragma module libr_nodigsr "V3.6 Calico"

/*
 * COPYRIGHT (C) 1987 BY
 *
 * Computer Management Center, Inc.
 * Idaho Falls, Idaho.
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
 *	This subroutine takes the filename and given array and
 *	opens the file and writes the file to the array without
 *	any conversions.
 *
 * Parameters:
 *
 *	LIB_NAME$
 *		The passed name of the library to pull the text from.
 *
 *	KEY_NAME$
 *		The passed name of the key to use to select the
 *		right text.
 *
 *	CODE$(0%)
 *		Contains the number of lines already in
 *		use in the CODE$() array.
 *
 *	CODE$()
 *		The text returned back in PRNT_ENCODED format.
 *	CODE$(0%) is modified to point to the last line.
 *
 *	Returns a status code.
 *
 * Example:
 *
 *	TEXT$(0%) = "0"
 *	ST% = LIBR_NODIGSR("HELP_GL", "ADDRESS", TEXT$())
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:LIBR_NODIGSR
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP LIBR_NODIGSR
 *	$ DELETE LIBR_NODIGSR.OBJ;*
 *
 * Author:
 *
 *	02/20/87 - B. Craig Larsen
 *
 * Modification history:
 *
 *	07/08/87 - Kevin Handy
 *		Modified to pull text out of a library.
 *
 *	09/30/87 - Kevin Handy
 *		Added code for LIST ELEMENT stuff.
 *
 *	10/01/87 - Kevin Handy
 *		Fixed bug where too many parameters on a line
 *		caused the program to crash.
 *
 *	07/19/88 - Kevin Handy
 *		Removed defaulting to REF:
 *
 *	06/16/89 - Kevin Handy
 *		Modified so it would fit in sharable library.
 *
 *	10/17/89 - Kevin Handy
 *		Extended maximum length of a line.
 *
 *	08/02/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, smg$routines.h, str$routines.h)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <descrip.h>
#include <string.h>
#include <str$routines.h>

#include "func_include:library.h"

/*
 * Local functions
 */
static void insertarray(struct dsc$descriptor_a *array, long item, char *text);

/*
 * Local variables
 */
static $DESCRIPTOR(tlb_text, ".TLB");

/*
 * Main Function
 */
long libr_nodigsr(struct dsc$descriptor *lib_name,
	struct dsc$descriptor *key_name,
	struct dsc$descriptor_a *code)
{
	/*
	 * Local variables
	 */
	long txrfa[2];
	long retstatus = 1;
	long st;
	int curr_line = 0;
	struct dsc$descriptor inputdesc;
	struct dsc$descriptor actualdesc;
	char inputbuf[256];
	unsigned long lr_index;
	long length;

	/*
	 * Set up the control structure if necessary
	 */
	st = lbr$ini_control(&lr_index, &LBR$C_READ);

	if ((st & 1) == 0)
	{
		retstatus = st;
		insertarray(code, ++curr_line, "Unable to initialize library!");
		goto ExitProgram;
	}

	/*
	 * Open the library function
	 */
	st = lbr$open(&lr_index, lib_name, 0l, &tlb_text);

	if ((st & 1) == 0)
	{
		retstatus = st;
		goto ExitProgram;
	}

	/*
	 * Search for key in file
	 */
	st = lbr$lookup_key(&lr_index, key_name, &txrfa);

	if ((st & 1) == 0)
	{
		retstatus = st;
		goto ExitProgram;
	}

	/*
	 * Copy over text
	 */
	inputdesc.dsc$a_pointer = inputbuf;
	inputdesc.dsc$w_length = sizeof(inputbuf);
	inputdesc.dsc$b_class = DSC$K_CLASS_S;
	inputdesc.dsc$b_dtype = DSC$K_DTYPE_T;

	while (((st = lbr$get_record(&lr_index, &inputdesc, &actualdesc)) & 1) != 0)
	{
		inputbuf[actualdesc.dsc$w_length] = '\n';
		inputbuf[actualdesc.dsc$w_length] = '\0';
		insertarray(code, ++curr_line, inputbuf);
	}

 ExitProgram:
	/*
	 * Close library
	 */
	st = lbr$close(&lr_index);

	/*
	 * Code in number of files loaded
	 */
	sprintf(inputbuf, "%d", curr_line);
	insertarray(code, 0l, inputbuf);

	/*
	 * Exit
	 */
	return(retstatus);
}

/*
 * Function to insert a C string into a VAX Descriptor array
 */
static void insertarray(struct dsc$descriptor_a *array, long item, char *text)
{
	/*
	 * Local Variables
	 */
	struct dsc$descriptor_s *str;
	long length;
	long elem;

	/*
	 * Can we fit it in?
	 */
	elem = item * (long)array->dsc$w_length;

	if (elem < array->dsc$l_arsize)
	{
		/*
		 * Calculate position of descriptor
		 */
		str = (struct dsc$descriptor_s*)(array->dsc$a_pointer + elem);

		/*
		 * Insert into array
		 */
		length = strlen(text);
		str$copy_r(str, &length, text);
	}
}
