/*	%TITLE "Subroutine to Search for a Group of Files"
 */
#pragma module find_file "V3.6 Calico"

/*
 *
 * COPYRIGHT (C) 1987 BY
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
 * Index:
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:FIND_FILE
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP FIND_FILE
 *	$ DELETE FIND_FILE.OBJ;*
 *
 * Author:
 *
 *	01/15/87 - B. Craig Larsen
 *
 * Modification history:
 *
 *	12/28/89 - Kevin Handy
 *		Modified for sharable library.
 *
 *	07/16/91 - Kevin Handy
 *		Unwound EXTERNAL definitions.
 *
 *	03/14/92 - Kevin Handy
 *		Clean up vars (checkvar)
 *
 *	07/23/93 - Kevin Handy
 *		Convert to C.
 *
 *	11/22/93 - Kevin Handy
 *		Modified to handle when a fixed string array is passed
 *		instead a regular array of descriptors.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, descriptors, str$routines.h, stdio.h)
 *
 *	06/08/99 - Kevin Handy
 *		Use SMG$ROUTINES.H instead of homegrown headers
 *
 *	08/16/2001 - Kevin Handy
 *		Fixed bug where fcount wouldn't be initialized when
 *		the flag was zero (0).  (KBJ)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <descrip.h>
#include <string.h>
#include <ctype.h>
#include <str$routines.h>
#include <smg$routines.h>
#include <smgdef.h>
#include "func_include:cmcfun.h"

/*
 * External functions
 */
long lib$find_file();
long lib$find_file_end();
long sys$filescan();

/*
 * Local defines
 */
#ifndef min
#define min(x,y) ((x)<(y)?(x):(y))
#endif

/*
 * Local functions
 */
static void insertarray(struct dsc$descriptor_a *array, long item, char *text);

/*
 * Local structires
 */
struct itemlst_struct
{
	short buflen;
	short code;
	long bufadr;
};

/*
 * Main Function
 */
void find_file(
	struct dsc$descriptor_s *wildf,
	struct dsc$descriptor_a *alist,
	int *flag,
	struct dsc$descriptor_s *prefix,
	struct dsc$descriptor_s *suffix)
{
	/*
	 * Local variables
	 */
	long context;
	long fflag;
	struct dsc$descriptor_s name_buffer;
	char name_text[128];
	struct itemlst_struct xx[8];
	char file_name[64];
	int item;
	int fcount = 0;
	int i;
	long length;
	long status;
	long maxitem;

	/*
	 * Figure out output array size.
	 */
	maxitem = alist->dsc$l_arsize / alist->dsc$w_length - 1;

	/*
	 * Build up code table
	 */
	if ((*flag == 0) || (*flag == 128))
	{
		xx[0].code = 1;
		xx[0].buflen = 0;

		xx[1].code = 0;
		xx[1].buflen = 0;

		fcount = 2;
	}
	else
	{
		fcount = 0;
		for(i = 0; i <= 6; i++)
		{
			if ((*flag & (1<<i)) != 0)
			{
				xx[fcount].code = i + 2;
				xx[fcount].buflen = 0;
				fcount = fcount + 1;
			}
		}
		xx[fcount].code = 0;
		xx[fcount].buflen = 0;
	}

	context = 0;

	/*
	 * Create descriptor for use in filename loopup
	 */
	name_buffer.dsc$a_pointer = name_text;
	name_buffer.dsc$w_length = sizeof(name_text);
	name_buffer.dsc$b_class = DSC$K_CLASS_S;
	name_buffer.dsc$b_dtype = DSC$K_DTYPE_T;

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

		insertarray(alist, item++, file_name);
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

/*
 * Function to insert a C string into a VAX Descriptor array
 */
static void insertarray(struct dsc$descriptor_a *array, long item, char *text)
{
	/*
	 * Local Variables
	 */
	char *str;
	long length;
	long elem;
	long status;

	/*
	 * Can we fit it in?
	 */
	elem = item * (long)array->dsc$w_length;

#if 0
	if (elem < array->dsc$l_arsize)
#endif
	{
		/*
		 * Calculate position of descriptor
		 */
		str = array->dsc$a_pointer + elem;

		/*
		 * Insert into array
		 */
		switch (array->dsc$b_dtype)
		{

		/* Fixed length string */
		case 14:
			length = min(strlen(text), array->dsc$w_length);
			strncpy(str, text, array->dsc$w_length);
			while (length < array->dsc$w_length)
			{
				str[length++] = ' ';
			}
			break;

		/* Descriptors */
		case 24:
			length = strlen(text);
			status = str$copy_r(str, &length, text);
			break;
		}
	}
}
