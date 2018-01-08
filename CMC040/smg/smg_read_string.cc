//! \file
//! \brief read string
//!

#include <string>
#include <ctype.h>
#include "smg/smg.h"


//!
//! \brief Read string
//!
long smg$read_string(
	smg_keyboard_id &kbid,		//!< Keyboard
	std::string &retstring,		//!< Returned sting
	long a,				//!< ???
	long xlen,			//!< Length toread
	long modifier,			//!< Modifiers
	long timeout,			//!< Timeout
	const char *term_set,		//!< Terminator character
	long data_length,		//!< Length of data
	int &retchar,			//!< Returned character
	smg_display_id &option)		//!< Display to use
{
	int ch;	// Character read

	retstring = "";

	//
	// This is a simple implementation.
	// Anything that is not printable is a terminator.
	//
	//!\todo What is the proper return vlue for this function
	//
	while (1)
	{
		ch = getch();

		if (ch <= 255 && isprint(ch))
		{
			retstring += ch;
			if (retstring.size() >= xlen)
			{
				retchar = ch;
				return 0;
			}
		}
		else
		{
			retchar = ch;
			return ch;
		}
	}
}

