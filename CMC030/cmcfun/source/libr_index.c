/*	%TITLE "Insert a Text File into a Library"
 */
#pragma module libr_index "V3.6 Calico"

/*
 *		COPYRIGHT (C) 1987 BY
 *		Computer Management Center, Idaho Falls, Idaho.
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
 *	This function will generate an index of what modules
 *	are in a library matching a key.
 *
 * Index:
 *
 * Parameter:
 *
 *	LIB_NAME$
 *		Passed name of library to insert text into.
 *
 *	KEY_NAME$
 *		Passed name of key to append to text.
 *
 *	LIBR_INDEX$()
 *		List of modules matching the key, with
 *		subscript (0) set to the number of items therin.
 *
 *	Returns a status code.
 *
 * Example:
 *
 *	ST% = LIBR_INDEX("HELP_GL", "*", A$())
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:LIBR_INDEX
 *	$ LIB FUNC_LIB:CMCFUN/REP LIBR_INDEX
 *	$ DELETE LIBR_INDEX.OBJ;*
 *
 * Author:
 *
 *	01/07/87 - Kevin Handy
 *
 * Modification history:
 *
 *	04/22/88 - Kevin Handy
 *		Modified to give user progam control if a library
 *		doesn't exist.
 *
 *	07/19/88 - Kevin Handy
 *		Removed defaulting to REF:
 *
 *	11/30/88 - Kevin Handy
 *		Modified to close library if open fails.
 *
 *	05/29/90 - Frank F. Starman
 *		Increase library key dimension from 1000 to 1500.
 *
 *	03/13/92 - Kevin Handy
 *		Clean up vars (checkvar)
 *
 *	06/25/93 - Kevin Handy
 *		Increase key dimension from 1500 to 3000.
 *		Conversion will allow this to be added to the sharable
 *		library at a later time. (Don't want to fight it right
 *		now).
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, descriptors, l_index, stdio.h, str$routines.h,
 *		string.h)
 *		Make temp->_s instead of _a
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <string.h>
#include <descrip.h>
#include <str$routines.h>
#include "func_include:library.h"

/*
 * External functions
 */
static long libr_index_a(struct dsc$descriptor_s *key_name, long *text_rfa);
static void insertarray(struct dsc$descriptor_a *array, long item, char *text);

/*
 * Local data
 */
static struct dsc$descriptor_a *name_array;
static struct dsc$descriptor_a *rfa_array;
static int name_count;

static $DESCRIPTOR(p1, "*");
static $DESCRIPTOR(p2, ".TLB");

/*
 * Main function
 */
long libr_index(struct dsc$descriptor_s *lib_name,
	struct dsc$descriptor_s *key_name,
	struct dsc$descriptor_a *libr_index,
	struct dsc$descriptor_a *lib_rfa)
{
	long st;
	unsigned long l_index;
	char textcount[10];
	struct dsc$descriptor_s *temp;

	/*
	 * Initialize pointers
	 */
	name_array = libr_index;
	rfa_array = lib_rfa;
	name_count = 0;

	/*
	 * Set up the control structure if necessary
	 */
	st = lbr$ini_control(&l_index, &LBR$C_READ);

	if ((st & 1) == 0)
	{
		return(st);
	}

	/*
	 * Open the library function
	 */
	st = lbr$open(&l_index, lib_name, 0l, &p2);

	if ((st & 1) != 0)
	{
		/*
		 * Get the index
		 */
		if (key_name->dsc$w_length == 0)
		{
			temp = &p1;
		}
		else
		{
			temp = key_name;
		}

		st = lbr$get_index(&l_index, &1l, libr_index_a, temp);
	}

	/*
	 * Code in number of files loaded
	 */
	sprintf(textcount, "%d", name_count);
	insertarray(name_array, 0l, textcount);

	/*
	 * Close all files
	 */
	lbr$close(&l_index);

	return(st);
}


/*
 * This sub-function is used to find all of the key names
 * associated with the given text record.
 */
static long libr_index_a(struct dsc$descriptor_s *key_name,
	long *text_rfa)
{
	/*
	 * Local Variables
	 */
	struct dsc$descriptor_s *str;
	long length;
	long elem;
	long *value;

	/*
	 * Point to next element
	 */
	name_count++;

	/*
	 * Can we fit it into string array?
	 */
	elem = name_count * name_array->dsc$w_length;

	if (elem < name_array->dsc$l_arsize)
	{
		/*
		 * Calculate position of descriptor
		 */
		str = (struct dsc$descriptor_s *)(name_array->dsc$a_pointer +
			elem);

		/*
		 * Insert into array
		 */
		str$copy_dx(str, key_name);
	}

	/*
	 * Can we fit it into rfa array?
	 */
	elem = name_count * (long)rfa_array->dsc$w_length;

	if (elem < rfa_array->dsc$l_arsize)
	{
		/*
		 * Calculate position of descriptor
		 */
		value = (long*)(rfa_array->dsc$a_pointer + elem);

		/*
		 * Insert into array
		 */
		*value = *text_rfa;
	}

	return(1);
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
		str = (struct dsc$descriptor_s *)(array->dsc$a_pointer + elem);

		/*
		 * Insert into array
		 */
		length = strlen(text);
		str$copy_r(str, &length, text);
	}
}

