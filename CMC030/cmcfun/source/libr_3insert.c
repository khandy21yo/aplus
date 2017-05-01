/*	%TITLE "Insert a Text File into a Library"
 */
#pragma module libr_3insert "V3.6 Calico"

/*
 *
 *	COPYRIGHT (C) 1987 BY
 *	Computer Management Center, Idaho Falls, Idaho.
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
 *	This function will insert text into a library,
 *	and append a key to that text.
 *
 *	WARNING: This version should only be used when there is
 *	no chance of connected items.
 *
 * Parameter:
 *
 *	LIB_NAME$
 *		Passed name of library to insert text into.
 *
 *	FILE_NAME$
 *		Passed name of file containing text to insert
 *		into the library.
 *
 *	KEY_NAME$
 *		Passed name of key to append to text.
 *
 *	Returns a status code.
 *
 * Example:
 *
 *	ST% = LIBR_3INSERT("HELP_GL", "SOURCE.FILE", "ADDRESS")
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:LIBR_3INSERT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP LIBR_3INSERT
 *	$ DELETE LIBR_3INSERT.OBJ;*
 *
 * Author:
 *
 *	01/07/87 - Kevin Handy
 *
 * Modification history:
 *
 *	07/19/88 - Kevin Handy
 *		Removed defaulting to REF:
 *
 *	01/03/89 - Kevin Handy
 *		Changed LIBR_EDIT return to LIBR_3INSERT so that
 *		the status will really be returned.
 *
 *	12/2/89 - Kevin Handy
 *		Modified to get into sharable library.
 *		This version doesn't copy over connected keys,
 *		because DEC didn't give me any of the necessary
 *		options with LBR$SEARCH.
 *
 *	04/29/92 - Kevin Handy
 *		Modified to lose whitespace at end of lines,
 *		so that they do not end up in source code.
 *
 *	07/27/93 - Kevin Handy
 *		Removed commented out debug lines.
 *
 *	07/27/93 - Kevin Handy
 *		Converted to C.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, string.h, l_index, lbr$c_create)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <string.h>
#include <descrip.h>
#include <ctype.h>
#include "func_include:library.h"

/*
 * External functions
 */
long lib$get_lun();
long lib$free_lun();

/*
 * Local functions
 */
static long CreateLibrary(struct dsc$descriptor_s *lib_name,
	unsigned long *l_index);

/*
 * Local variables
 */
static $DESCRIPTOR(p2, ".TLB");

/*
 * Main function
 */
long libr_3insert(struct dsc$descriptor_s *lib_name,
	struct dsc$descriptor_s *file_name,
	struct dsc$descriptor_s *key_name)
{
	long st;
	long test_rfa[2];
	unsigned long l_index;
	FILE *input_ch;
	char input_name[64];
	int line_flag;
	char in_text[256];
	struct dsc$descriptor_s in_desc;
	long length;

	/*
	 * Initialize
	 */
	st = 1;

 l100:	/*
	 * Set up the control structure if necessary
	 */
	st = lbr$ini_control(&l_index, &LBR$C_UPDATE);

	if ((st & 1) == 0)
	{
		return(st);
	}

	/*
	 * Open the library function
	 */
	st = lbr$open(&l_index, lib_name, 0l, &p2);

	if (st == 98962l)
	{
		/*
		 * It doesn't exist, so lets try to create it
		 */
		st = CreateLibrary(lib_name, &l_index);
	}

	if ((st & 1) == 0)
	{
		goto l900;
	}

	/*
	 * Open the source file
	 */
	strncpy(input_name, file_name->dsc$a_pointer, file_name->dsc$w_length);
	input_name[file_name->dsc$w_length] = '\0';

	if ((input_ch = fopen(input_name, "r")) == NULL)
	{
		st = 6;
		goto l900;
	}

	/*
	 * Delete key if it already exists
	 */
	st = lbr$lookup_key(&l_index, key_name, &test_rfa);

	if ((st & 1) != 0)
	{
		/*
		 * Delete the primary key
		 */
		st = lbr$delete_key(&l_index, key_name);

		/*
		 * Delete the data
		 */
		st = lbr$delete_data(&l_index, &test_rfa);
	}

	/*
	 * Load in text information
	 */
	test_rfa[0] = 0;
	test_rfa[1] = 0;

	line_flag = 0;

	in_desc.dsc$a_pointer = in_text;
	in_desc.dsc$b_class = DSC$K_CLASS_S;
	in_desc.dsc$b_dtype = DSC$K_DTYPE_T;

	while (fgets(in_text, sizeof(in_text), input_ch) != NULL)
	{
		/*
		 * Lose junk whitespace at end of string
		 */
		length = strlen(in_text);

		while ((length > 0) && (isspace(in_text[length-1])))
		{
			length--;
		}
		in_text[length] = '\0';

		line_flag = -1;

		in_desc.dsc$w_length = length;

		st = lbr$put_record(&l_index, &in_desc, &test_rfa);
	}

	if (line_flag == 0)
	{
		st = 2;
		goto l900;
	}

	/*
	 * Mark end of text
	 */
	st = lbr$put_end(&l_index);

	/*
	 * Insert main key pointing to text
	 */
	st = lbr$insert_key(&l_index, key_name, &test_rfa);

l900:	/*
	 * Close all files
	 */
	lbr$close(&l_index);
	fclose(input_ch);

	/*
	 * Return status
	 */
	return(st);
}

/********************************************************************
 * Create a library file
 ********************************************************************/

static long CreateLibrary(struct dsc$descriptor_s *lib_name,
	unsigned long *l_index)
{
	/*
	 * Local variables
	 */
	long c_array[19];
	long st;

	/*
	 * Set up the control structure if necessary
	 */
	st = lbr$ini_control(l_index, &LBR$C_CREATE, &LBR$C_TYP_TXT);

	if ((st & 1) == 0)
	{
		return(st);
	}

	/*
	 * Open/Create the library file
	 */
	c_array[ 0] = LBR$C_TYP_TXT;		/* Library type */
	c_array[ 1] = 39;			/* Key length */
	c_array[ 2] = 11;			/* Initial allocation */
	c_array[ 3] = 1;			/* Number of keys */
	c_array[ 4] = 0;			/* Additional chars in header */
	c_array[ 5] = 200;			/* Preallocated indexes */
	c_array[ 6] = 0;			/* History records */
	c_array[ 7] = 3;			/* Format of library */
	c_array[ 8] = 0;			/* Index casing */

	st = lbr$open(l_index, lib_name, &c_array, &p2);

	return(st);
}
