//! \file
//! \brief func_4scoseq - Convert Entered Characters to Desired Character
//!/
#pragma module func_4scoseq "V3.6 Calico"

#include <smg/smg.h>

static const int tvalue[] =
{
	SMG$K_TRM_KP0, SMG$K_TRM_KP1, SMG$K_TRM_KP2, SMG$K_TRM_KP3,
	SMG$K_TRM_KP4, SMG$K_TRM_KP5, SMG$K_TRM_KP6, SMG$K_TRM_KP7,
	SMG$K_TRM_KP8, SMG$K_TRM_KP9, SMG$K_TRM_MINUS, SMG$K_TRM_COMMA,
	SMG$K_TRM_PERIOD, SMG$K_TRM_ENTER,
	0
};

static const int rvalue[] =
{
	'0', '1', '2', '3',
	'4', '5', '6', '7',
	'8', '9', '-', ',',
	'.', SMG$K_TRM_CTRLM,
	0
};

//!
//! \brief func_4scoseq - Convert Entered Characters to Desired Character
//!
//! =returns	Converts the characters the user entered
//!	to the desired characters.
//!
//! Example:
//!
//!	X% = func_4scoseq(Y%)
//!
int func_4scoseq(
	int xchar)
		//!< The passed character the user wants converted.
{
	int loop;

	/*
	 * Translate using table only items in the correct range
	 */
	if ((xchar >= 260) && (xchar <= 273))
	{
		for (loop = 0; tvalue[loop] != 0; loop++)
		{
			if (tvalue[loop] == xchar)
			{
				return(rvalue[loop]);
			}
		}
	}
	return(xchar);
}
