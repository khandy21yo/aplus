/*
 * %TITLE "Placeholder in sharable library for Extinct Functions"
 * %SBTTL "XXXX_EXTINCT"
 * %IDENT "V3.6 Calico"
 *
 * COPYRIGHT (C) 1992 BY
 *
 * Computer Management Center, Inc.
 * Idaho Falls, Idaho. 83402
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
 * ++
 *
 * Abstract:HELP
 *	.b
 *	.lm +5
 *	Marks position for extinct functions in sharable library.
 *	This should only be used after all users have been updated
 *	so they do not use the old function.
 *
 * Index:
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:XXXX_EXTINCT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP XXXX_EXTINCT
 *	$ DELETE XXXX_EXTINCT.OBJ;*
 *
 * AUTHOR:
 *
 *	04/25/92 - Kevin Handy
 *
 * MODIFICATION HISTORY:
 *
 *	04/15/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(stdlib.h)
 *--
 */

/*
 * Inlcude files
 */
#include <stdio.h>
#include <stdlib.h>

/*
 * Main function
 */
void xxxx_extinct()
{
	printf("%Attempt to use non-existant Sharable Function\n");
	printf("%You need to update program to current revision\n");
	exit(0);
}
