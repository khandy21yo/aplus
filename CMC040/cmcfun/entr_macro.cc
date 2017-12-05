//! \file
//! \brief Menu (Macro) Command Level
// %SBTTL "ENTR_MACRO"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/entr_macro.bas
// Translated from Basic to C++ using btran
// on Monday, December 04, 2017 at 21:15:14
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"



//!
//! Abstract:HELP
//!	.b
//!	.lm +5
//!	The ^*Menu Command Level (MCL)\* from a process allows
//!	to enter CMC or user defined macro commands.
//!	.b
//!	The following is list of valid CMC commands:
//!	.table 3,25
//!	.te
//!	^*AMORTIZATION\*
//!	.te
//!	^*BATCH\*,/MONITOR
//!	.te
//!	^*BYE\*
//!	.te
//!	^*CALENDAR\*
//!	.te
//!	^*COMPRESS\*
//!	.te
//!	^*COUNTRY\*,/LIST
//!	.te
//!	^*DEVICE\*,/LIST
//!	.te
//!	^*END\*
//!	.te
//!	^*EXIT\*
//!	.te
//!	^*HELP\*
//!	.te
//!	^*LOGOUT\*,/FULL
//!	.te
//!	^*MACRO\*,/DELETE,/LIST
//!	.te
//!	^*PROFILE\*,/PERIOD,/STRUCTURE
//!	.te
//!	^*QUIT\*
//!	.te
//!	^*REPORT\*,/DESTINATION,/PRINT,/SETTINGS
//!	.te
//!	^*STRING\*,/LIST,/PRINT
//!	.te
//!	^*SYSTEM\*,/INSTALL,/LIST
//!	.te
//!	^*TIMEOUT\*
//!	.end table
//!	.lm -5
//!
//! \author 04/06/90 - Frank F. Starman
//!
long entr_macro(
	scope_struct &scope)
{
	long Result;
	long exit_status;
	smg_display_id macro_V2;
	long smg_status;

	long save_timeout;
	std::string save_ident;
	std::string save_program;
	std::string save_item;
	std::string macro;
	//
	// Save the prior help key
	//
	save_ident = scope.prg_ident + "";
	save_program = scope.prg_program + "";
	save_item = scope.prg_item + "";
	save_timeout = scope.scope_timeout;
	if (boost::trim_right_copy(scope.prg_program) == "ENTR_MACRO")
	{
		goto restorescope;
	}
	scope.prg_program = "ENTR_MACRO";
	scope.prg_item = "HELP";
	scope.prg_ident = "H";
	//
	// Assume a good macro command
	//
	smg_status = smg$create_virtual_display(2, 132, macro_V2, SMG$M_BORDER, 0, 0);
	smg_status = smg$paste_virtual_display(macro_V2, scope.smg_pbid, 21, 1);
	smg_status = smg$put_chars(macro_V2, "Menu Command Level> ", 1, 1);
	macro = std::string(50, ' ');
	scope.scope_timeout = 0;
entermacro:;
	exit_status = entr_3enter(scope, macro_V2, 1, 21, macro, -1, 8 + 16 + 1024 + 4096);
	// ** Converted from a select statement **
	//
	// Help keys
	//
	if (exit_status == SMG$K_TRM_F15)
	{
		help_34message(scope, "", "H", scope.prg_program, "", "HELP");
		goto entermacro;
		//
		// Good key
		//
	}
	else if ((exit_status == 0) ||
		((exit_status == 10) ||
		((exit_status == 12) ||
		((exit_status == 13) ||
		(exit_status == SMG$K_TRM_DO)))))
	{
		if (macro != "")
		{
			exit_status = SMG$K_TRM_F10;
		}
		else
		{
			exit_status = 0;
			goto exitfunction;
		}
		//
		// Repaint screen
		//
	}
	else if ((exit_status == SMG$K_TRM_F11) ||
		(exit_status == SMG$K_TRM_CTRLW))
	{
		goto exitfunction;
		//
		// Go to MCL
		//
	}
	else if ((exit_status == SMG$K_TRM_F10) ||
		(exit_status == SMG$K_TRM_CTRLZ))
	{
		exit_status = SMG$K_TRM_F10;
	}
	else
	{
		entr_3badkey(scope, scope.scope_exit);
		goto entermacro;
	}
	macro = basic::edit(macro, 8 + 16 + 128);
	//
	// Set symbol, that menu will understand it
	//
	lib$set_symbol("CMC$COMMAND", macro);
	//
	// delete all virtual arrays
	//
	if (scope.macroflag == 0)
	{
		smg_status = smg$pop_virtual_display(scope.smg_option, scope.smg_pbid);
	}
	//
	// Assign macro flag to allow quick exit from process to the menu
	//
	scope.macroflag = 3;
exitfunction:;
	smg_status = smg$pop_virtual_display(macro_V2, scope.smg_pbid);
restorescope:;
	//
	// Restore back the original help key
	//
	scope.scope_timeout = save_timeout;
	scope.prg_ident = save_ident;
	scope.prg_program = save_program;
	scope.prg_item = save_item;
	Result = exit_status;
	return Result;
}
