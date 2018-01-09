//! \file
//! \brief Subroutine to Split the Cursor Move String
//!
#pragma module dspl_splitcursor "V3.6 Calico"

#include <string>

#include "preferences.h"
#include "cmcfun.h"

//!
//!
//! Abstract:HELP
//!	.p
//!	This subroutine will take the CURSTR$('row;col') and 
//!	break it into the row and col integers.
//!
//! Parameters:
//!
//!	CURSTR$ 
//!		The passed cursor movement string (row and col).
//!
//!	RXXX%
//!		The returned row from the CURSTR$.
//!
//!	CYYY% 
//!		The returned column from the CURSTR$.
//!
//! Example:
//!
//!	CALL DSPL_SPLITCURSOR('0;5',XPOS%,YPOS%)
//!
//! Author:
//!
//!	07/09/86 - B. Craig Larsen
//!--
void dspl_splitcursor(
	const std::string &curstr,
	long &rxxx,
	long &cyyy)
{
	long loop;
	long x=0;
	long y=0;

	for (loop=0; loop < curstr.size(); loop++)
	{
		if (curstr[loop] == ';')
		{
			x=y;
			y=0;
		}
		else
		{
			y = y * 10 + curstr[loop] - '0';
		}
	}
	rxxx	= x;
	cyyy	= y;
}
