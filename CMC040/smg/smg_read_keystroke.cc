//! \file
//! \brief Read keystrole
//!

#include <string>>

#include "smg/smg.h"


//!
//! \brief Read keystroke
//!
long  smg$read_keystroke(
	smg_keyboard_id kbid,		//!< Keyboard ID
	int &retchar,			//!< Return character
	int a,				//!< Prompt
	int b,				//!< Time out
	smg_display_id &display,	//!< Window
	int c,				//!< Attribute
	int d)				//!< Attribute
{
	retchar = getch();
	return retchar;
}

