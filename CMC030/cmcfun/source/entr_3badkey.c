/*	TITLE "Invalid Key"
 */
#pragma module entr_3badkey "V3.6 Calico"

/*
 *
 *	COPYRIGHT (C) 1987 BY
 *	Computer Management Center, Idaho Falls, Idaho.
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
 * abstract:HELP
 *	.p
 *	This subroutine displays warning about an invalid key
 *	having been pressed.
 *
 * Index:
 *
 * Parameters:
 *
 *	scope
 *		Structure created by window initilzation.
 *
 *	bad_value
 *		Enters a integer value of an error.
 *
 * Example:
 *
 *	ENTR_3BADKEY = 10
 *	
 * Compile:
 *
 *	$ CC FUNC_SOURCE:ENTR_3BADKEY/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_3BADKEY
 *	$ DELETE ENTR_3BADKEY.OBJ;*
 *
 * Author:
 *
 *	01/26/87 - Kevin Handy
 *
 * Modification history:
 *
 *	09/16/87 - Kevin Handy
 *		Modified to give CR and LF a real name, instead
 *		of saying "Control/J" and "Control/M".
 *
 *	04/27/90 - Frank F. Starman
 *		Call HELP_34MESSAGE function instead ENTR_3MESSAGE
 *
 *	08/14/90 - Kevin Handy
 *		Converted to C.
 *
 *	08/27/90 - Frank F. Starman
 *		Change key name MainScreen to MCL. 
 *
 *	10/19/90 - Frank F. Starman
 *		Associate all function keys with names.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modified to compile with DEC-C without errors
 *		(module, stdio.h, stdlib.h, str3, string.h, descriptors)
 *--
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <descrip.h>
#include "func_include:cmcfun.h"

void entr_3badkey(struct scope_struct *scope, long *bad_value)
{
	char *xkey;
	char str1[32], str2[32], str3[64];
	struct dsc$descriptor_s message;
	int bv;

	static $DESCRIPTOR(p3, "W");
	static $DESCRIPTOR(p4, "ENTR_3BADKEY");
	static $DESCRIPTOR(p5, "");
	static $DESCRIPTOR(p6, "INVKEY");

	bv = abs(*bad_value);

	switch(bv)
	{
		case 10:
			xkey = "Linefeed";
			break;

		case 13:
			xkey = "Return";
			break;

		case 27:
			xkey = "Escape";
			break;

		case 127:
			xkey = "Delete";
			break;

		case 256:
			xkey = "Gold";
			break;

		case 274:
			xkey = "UpArrow";
			break;

		case 275:
			xkey = "DownArrow";
			break;

		case 276:
			xkey = "LeftArrow";
			break;

		case 277:
			xkey = "RightArrow";
			break;

		case 286:
			xkey = "Interrupt";
			break;

		case 287:
			xkey = "Resume";
			break;

		case 288:
			xkey = "Cancel";
			break;

		case 289:
			xkey = "MenuCommandLevel";
			break;

		case 290:
			xkey = "Exit";
			break;

		case 291:
			xkey = "RefreshScreen";
			break;

		case 292:
			xkey = "RubLine";
			break;

		case 293:
			xkey = "RubWord";
			break;

		case 294:
			xkey = "ListChoices";
			break;

		case 295:
			xkey = "Help";
			break;

		case 296:
			xkey = "Do";
			break;

		case 297:
			xkey = "MagicKey";
			break;

		case 298:
			xkey = "Top";
			break;

		case 299:
			xkey = "Bottom";
			break;

		case 300:
			xkey = "InterruptCommandLevel";
			break;

		case 311:
			xkey = "Find";
			break;

		case 312:
			xkey = "InsertHere";
			break;

		case 313:
			xkey = "Remove";
			break;

		case 314:
			xkey = "Select";
			break;

		case 315:
			xkey = "PrevScreen";
			break;

		case 316:
			xkey = "NextScreen";
			break;

		default:
			if (bv >= 1 && bv <= 26)
			{
				sprintf(str1, "Control/%c", bv + 64);
				xkey = str1;
				break;
			}

			if (bv >= 32 && bv <= 126)
			{
				sprintf(str1, "%c", bv);
				xkey = str1;
				break;
			}

			if (bv >= 257 && bv <= 259)
			{
				sprintf(str1, "PF%d", bv-255);
				xkey = str1;
				break;
			}

			if (bv >= 281 && bv <= 300)
			{
				sprintf(str1, "F%d", bv-280);
				xkey = str1;
				break;
			}

			sprintf(str1, "Scancode %d", bv);
			xkey = str1;
			break;
	}

	if (*bad_value < 0)
	{
		sprintf(str2, "Gold/%s", xkey);
		xkey = str2;
	}


	sprintf(str3, "<%s> invalid key here", xkey);

	message.dsc$w_length = strlen(str3);
	message.dsc$a_pointer = str3;
	message.dsc$b_class = DSC$K_CLASS_S;
	message.dsc$b_dtype = DSC$K_DTYPE_T;

	help_34message(scope, &message, &p3, &p4, &p5, &p6);
/*
 *++
 * Warning:INVKEY
 *	^*Invalid Key\*
 *	.p
 *	^*Explanation:\*
 *	.p
 *	Invalid key has been pressed.
 *	.p
 *	^*User Action\*
 *	.p
 *	Check check validity of the special keys at this level.
 *
 * Index:
 *	.x Invalid>Command
 *	.x Invalid>Key
 *
 *--
 */
}
