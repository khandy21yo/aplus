//! \file
//! \brief Exit from a Program
// %SBTTL "SUBR_3EXITPROGRAM"
// %IDENT "V3.3"
//
// Source: ../../CMC030/cmcfun/source/subr_3exitprogram.bas_old
// Translated from Basic to C++ using btran
// on Monday, January 08, 2018 at 12:03:20
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "smg/lib.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"

//!
//! Abstract:HELP
//!	.p
//!	This function is used to exit a program.
//!	It closes out all necessary SMG stuff, and executes
//!	either a given program or goes to menu.
//!
//! \returns It closes out all necessary information, and goes
//!	to either a given program or to the menu.
//!
//! Example:
//!
//!	CALL SUBR_EXIT("",CORE$)
//!
//! AUTHOR:
//!
//!	03/06/87 - Kevin Handy
//!
void subr_3exitprogram(
	scope_struct &scope,
		//!< Program wide strucyure
	const std::string &opt_docommand,
		//!< The passed command the user enters to enter another
		//!< program.
	const std::string &opt_corecommon)
		//!< The passed string the user enters to pass to the
		//!> core common.
{
	long smg_status;

	//
	// Set up 'core' common
	//
	smg_status = lib$set_symbol("CMC$CORECOMMON", opt_corecommon);
	smg_status = smg$pop_virtual_display(scope.smg_option, scope.smg_pbid);
	//
	// Print message to Please Wait
	//
	entr_3message(scope, "", 1 + 16);
	//
	// Re-establish cursor
	//
	smg_status = smg$set_cursor_mode(scope.smg_pbid, 0);
	endwin();

	//
	// Exit out by doing a DCL command
	//
	if (opt_docommand == "")
	{
		system("menu");
	}
	else
	{
		system(opt_docommand.c_str());
	}
	//
	// If all else fails, crash and burn
	//
	exit(EXIT_FAILURE);
}
