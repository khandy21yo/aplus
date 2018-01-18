//! \file
//! \brief Translate a Logical Name
// %SBTTL "READ_SYSLOG"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/read_syslog.bas
// Translated from Basic to C++ using btran
// on Wednesday, January 17, 2018 at 15:24:59
//

#include <string>

#include "preferences.h"
#include "cmcfun.h"



//!
//! Abstract:HELP
//!	.p
//!	This function returns a string containing the logical
//!	equivalence name of the given logical name or the given
//!	logical name if there is no equivalence.
//!
//! Parameters:
//!
//!	LOGNAM1$
//!		The passed logical name the user wants to translate.
//!
//!	Returned value
//!		This function returns a string containing the logical
//!		equivalence name of the given logical name.
//!
//! Example:
//!
//!	NAME$ = READ_SYSLOG("CMC$CLIPBOARD")
//!
//! AUTHOR:
//!
//!	12/12/86 - B. Craig Larsen
//!
std::string read_syslog(
	const std::string &lognam1)
{
	std::string Result = lognam1;

	//! \todo Linux logicals are noothing like what VMS has.
	//! This needs some work.

	if (Resullt == "TT:")
	{
		Result = "/dev/tty";
	}

	return Result;
}
