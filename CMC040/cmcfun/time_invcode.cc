//! \file
//! \brief Time Aging Function
 */
#pragma module time_invcode "V3.6 Calico"

#include <cstdio>
#include <string>

//!
//! \brief Time Aging Function
//!
//!	This function will conver a number into a time string.
//!
//!	WARNING: When adding a length to a time, you must
//!	subtract off one second.  A one hour period starting
//!	at 07:00:00 ends at 07:59:59, not 08:00:00, so you
//!	must subtract off one second when adding a length
//!	to a time.
//!
//! \returns Returns a time in the format HH:MM:SS.  Any overflow
//!	above 60 hours is lost.
//!
//! Example:
//!
//!	GOOF_TIME$ = TIME_INVCODE(3600%)
//!
//!--
//!/
std::string time_invcode(
	long datum)
		//!< The passed time in question in seconds.
{
	/*
	 * Local Variables
	 */
	char makestring[10];

	/*
	 * Create output time
	 */
	sprintf(makestring, "%02d%02d%02d",
		(datum / (60 * 60)),		/* Hours */
		(datum / 60) % 60,		/* Minutes */
		(datum) % 60);			/* Seconds */

	/*
	 * Return output string
	 */
	return makestring;
}
