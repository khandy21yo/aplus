/*
 * %TITLE "Create Print String to Be Run Through WRIT_STRING"
 * %SBTTL "OUTP_CREATESTR"
 * %IDENT "V3.6 Calico"
 *
 *	COPYRIGHT (C) 1995 BY
 *	Software Solutions, Inc.
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
 * Software Solutions, Inc.
 *
 * Software Solutions, Inc. assumes no responsibility for the use or
 * reliability of its software on equipment which is not supported
 * by Software Solutions, Inc.
 *
 *++
 *
 * Abstract:HELP
 *	.p
 *	Create a device control string.
 *
 * Parameters:
 *
 *	SEQ$
 *		The passed string that holds the print string to the run
 *		through WRIT_STRING.
 *
 *	ITEM$
 *		The passed string that holds the item to be run through.
 *
 *
 *	Returned value
 *		Creates a print string to be run through
 *		WRIT_STRING.
 *
 * Example:
 *
 * Index:
 *
 *	.x Create>String
 *	.x String>Create
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:OUTP_CREATESTR
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP OUTP_CREATESTR
 *	$ DELETE OUTP_CREATESTR.OBJ;*
 *
 * Author:
 *
 *	01/01/86 - Kevin Handy
 *
 * Modification history:
 *
 *	03/09/92 - Kevin Handy
 *		Added "*2" type.
 *
 *	07/06/93 - Kevin Handy
 *		Modified to use VAL%( instead of VAL( which
 *		should be slightly faster.
 *
 *	04/15/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards
 *
 *	10/27/95 - Kevin Handy
 *		Converted to C. (Complete re-write)
 *
 *	10/31/95 - Kevin Handy
 *		Trim strings.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(str$routines.h)
 *--
 */

/*
 * Include files
 */
#include <stdlib.h>
#include <string.h>
#include <descrip.h>
#include <str$routines.h>

/*
 * Main function
 */
void outp_createstr(struct dsc$descriptor *Result,
	struct dsc$descriptor *Source, struct dsc$descriptor *Item)
{
	char* BuildString;	/* Build area for new string */
	long BuildLength = 0;
	char* NullItem;		/* Null terminated Item */
	int NullLength;
	int SourceLoop;
	int Character;
	int Value;

	/*
	 * Allocate a big enough buffer to handle result string
	 */
	BuildString = malloc(Source->dsc$w_length + Item->dsc$w_length + 5);

	/*
	 * Make a version of Item that is NULL terminated so all the
	 * Standard C Library functions will work correctly.
	 */
	NullLength = Item->dsc$w_length;
	NullItem = malloc(NullLength + 2);
	strncpy(NullItem, Item->dsc$a_pointer, NullLength);
	NullItem[NullLength] = '\0';

	/*
	 * Trim off trailing spaces
	 */
	while ((NullLength > 0) && (NullItem[NullLength - 1] == ' '))
	{
		NullLength--;
		NullItem[NullLength] = '\0';
	}

	/*
	 * Scan through source string
	 */
	for (SourceLoop = 0; SourceLoop < Source->dsc$w_length; SourceLoop++)
	{
		switch(Character = ((char*)Source->dsc$a_pointer)[SourceLoop])
		{
		case '*':
			switch(((char*)Source->dsc$a_pointer)[SourceLoop+1])
			{
			case '0':
				/*
				 * Copy item in as-is
				 */
				strcpy(BuildString + BuildLength, NullItem);
				BuildLength += NullLength;
				SourceLoop++;
				break;

			case '1':
				/*
				 * Write out single character + 31
				 */
				Value = atoi(NullItem) + 31;
				BuildString[BuildLength++] = '/';
				BuildString[BuildLength++] =
					(Value / 100) % 10 + '0';
				BuildString[BuildLength++] =
					(Value / 10) % 10 + '0';
				BuildString[BuildLength++] =
					(Value) % 10 + '0';
				SourceLoop++;
				break;

			case '2':
				/*
				 * Write out single character
				 */
				Value = atoi(NullItem);
				BuildString[BuildLength++] = '/';
				BuildString[BuildLength++] =
					(Value / 100) % 10 + '0';
				BuildString[BuildLength++] =
					(Value / 10) % 10 + '0';
				BuildString[BuildLength++] =
					(Value) % 10 + '0';
				SourceLoop++;
				break;

			default:
				/*
				 * Process a normal character
				 */
				BuildString[BuildLength++] = '*';
				break;
			}
			break;

		default:
			/*
			 * Process a normal character
			 */
			BuildString[BuildLength++] = Character;
			break;
		}
	}

	/*
	 * Null terminate it
	 * (Not necessary to do this at this time)
	 */
#if 0
	BuildString[BuildLength] = '\0';
#endif
	/*
	 * Trim off trailing spaces
	 */
	while ((BuildLength > 0) && (BuildString[BuildLength - 1] == ' '))
	{
		BuildLength--;
#if 0
		BuildString[BuildLength] = '\0';
#endif
	}

	/*
	 * Return string
	 */
	str$copy_r(Result, &BuildLength, BuildString);

	free(BuildString);
	free(NullItem);
}
