/*	%TITLE "Function to Print Encoded"
 */
#pragma module prnt_encoded "V3.6 Calico"

/*
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
 *	This is one of Franks functions, and nobody seems to
 *	know what it really does.
 *	.b
 *	Output's an encoded string to the virtual display.
 *	This replaces DEC's version (which is apparently no longer
 *	supported, and does not work).
 *
 * Index:
 *
 * Parameters:
 *
 *	PVDID
 *		Passed variable to update the display.
 *
 *	FULL_LEN
 *		Passed variable to extract parts of the text.
 *
 *	CODED_TEXT
 *		The passed text to be print encoded.
 *
 *	PROW
 *		Used to put characters on the screen.
 *
 *	PCOL
 *		The passed column number.
 *
 *	PWRAP
 *		Used to wrap-around the screen.
 *
 *	PCSET
 *		Used to set the characters.
 *
 *	VSET
 *		Used to position the characters on the screen.
 *
 *	Returned value
 *		This function returns a long value for the print encoded
 *		text.
 *
 * Index:
 *
 *	.x Print>Encoded
 *	.x Encoded>Print
 *
 * COMPILER INSTRUCTIONS:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:PRNT_ENCODED
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP PRNT_ENCODED
 *	$ DELETE PRNT_ENCODED.OBJ;*
 *
 * AUTHOR:
 *
 *	04/15/87 - B. Craig Larsen
 *
 * MODIFICATION HISTORY:
 *
 *	07/03/89 - Kevin Handy
 *		Modified to remove MAP's, make function fit
 *		into sharable library, disable useless error
 *		trapping.
 *
 *	08/05/91 - Kevin Handy
 *		Trial to see if DEC fixed the function.
 *		(Nope.  Now it throws '+' in periodically.
 *		They have also deleted reference to this
 *		function in their documentation.)
 *
 *	08/12/91 - Kevin Handy
 *		Plus signs (see 08/05/91) were actually
 *		something Frank threw into DSPL_SCROLL and never
 *		told anybody, however the SMG$ version of this
 *		function causes "memory management violation"
 *		anyway.
 *
 *	07/08/93 - Kevin Handy
 *		Converted to C.
 *
 *	07/19/93 - Kevin Handy
 *		Added special test to skip out early if there is
 *		nothing in the string to print.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	09/19/98 - Kevin Handy
 *		Make function return 'unsigned long' instead of 'long'
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, smg$routines.h)
 *--
 */

/*
 * Include Files
 */
#include <descrip.h>
#include <smgdef.h>
#include <smg$routines.h>

/*
 * Main Function
 */
unsigned long smg_put_virtual_display_encoded(
	long *pvdid, long *full_len, struct dsc$descriptor_s *coded_text,
	long *prow,  long *pcol, long *pwrap, long *pcset, long *vset)
{
	short int *wpointer;
	long c_len;
	struct dsc$descriptor_s textpart;
	int pc;
	long cbeg;
	long clen;
	long catr;
	long position;

	/*
	 * Skip out if we aren't goint to do anything because of zero length.
	 */
	if (*full_len == 0)
	{
		return(1);
	}

	/*
	 * Find important points in string
	 */
	wpointer = (short int*)(coded_text->dsc$a_pointer + *full_len - 2);
	c_len = *full_len - *wpointer;

	/*
	 * Lock Display
	 */
	smg$begin_display_update(pvdid);

	/*
	 * Display plain string
	 */
	textpart.dsc$a_pointer = coded_text->dsc$a_pointer;
	textpart.dsc$w_length = c_len;
	textpart.dsc$b_class = DSC$K_CLASS_S;
	textpart.dsc$b_dtype = DSC$K_DTYPE_T;

	smg$put_chars(pvdid, &textpart, prow, pcol, 0l, vset, 0l, pcset);

	/*
	 * Replace parts using formatting
	 */
	if (*vset < 1)
	{
		for (pc = c_len; pc < (*full_len) - 2; pc += 5)
		{
			cbeg = *((short int *)(coded_text->dsc$a_pointer + pc));
			clen = *((short int *)(coded_text->dsc$a_pointer + pc + 2));
			catr = *((char *)(coded_text->dsc$a_pointer + pc + 4));

			textpart.dsc$a_pointer =
				coded_text->dsc$a_pointer + cbeg - 1;
			textpart.dsc$w_length = clen;
			textpart.dsc$b_class = DSC$K_CLASS_S;
			textpart.dsc$b_dtype = DSC$K_DTYPE_T;

			position = (cbeg + *pcol - 1);
			smg$put_chars(pvdid,
				&textpart, prow,
				&position, 0l,
				&catr, 0l, pcset);
		}
	}

	/*
	 * Release display
	 */
	smg$end_display_update(pvdid);
	return(1);
}
