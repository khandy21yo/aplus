//! \file
//! \brief Invalid Key
//!/
#pragma module entr_3badkey "V3.6 Calico"

#include <cstdio>
#include <cstdlib>
#include <string>

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"

//!
//!	This subroutine displays warning about an invalid key
//!	having been pressed.
//!
//! \author 01/26/87 - Kevin Handy
//!
void entr_3badkey(
	struct scope_struct &scope,
		//!i< Structure created by window initilzation.
	long bad_value)
		//!< Enters a integer value of an error.
{
	std::string xkey;
	char str1[32], str2[32], str3[64];
	std::string message;
	int bv;

	std::string p3 = "W";
	std::string p4 = "ENTR_3BADKEY";
	std::string p5 = "";
	std::string p6 = "INVKEY";

	bv = abs(bad_value);

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

	if (bad_value < 0)
	{
		sprintf(str2, "Gold/%s", xkey);
		xkey = str2;
	}


	sprintf(str3, "<%s> invalid key here", xkey);

	message = str3;

	help_34message(scope, message, p3, p4, p5, p6);
}
