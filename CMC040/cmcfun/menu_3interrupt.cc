//! \file
//! \brief Interrupt Command Level
//!
// %SBTTL "MENU_3INTERRUPT"
// %IDENT "V3.6a Calico"
//
// Source: ../CMC030/cmcfun/source/menu_3interrupt.bas
// Translated from Basic to C++ using btran
// on Monday, December 04, 2017 at 20:42:09
//

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "smg/lib.h"
#include "smg/smg.h"

//
// Include files
//
struct screencapture_struct
{
	long lines;
	std::string screen[25];
};

//!
//!	The ^*Interrupt Command Level\* is accessable from any process
//!	by pressing the interrupt key. After option has been selected,
//!	a subprocess is created and the current process waits for
//!	the subprocess termination.
//!	.p
//!	The following is a list of all Interrupt commands:
//!	.b
//!	.list 0,"*"
//!	.le
//!	^*DCL\*
//!	.le
//!	^*Document\*
//!	.le
//!	^*Menu\*
//!	.els
//!
//! Parameters:
//!
//!	The call of this program installs the menu.
//!
//!	The return of this subroutine is the interrupt menu.
//!
//! Example:
//!
//!	CALL MENU_3INTERRUPT
//!
// \author 12/01/89 - Kevin Handy
//!	Taken from MENU_INTERRUPT.
//!
void menu_3interrupt(
	scope_struct &scope)
{
	long i;
	std::string inp;
	std::ofstream interupt_ch;
	std::string interupt_file;
	std::string lib_key;
	std::string lib_name;
	std::string old_ident;
	std::string old_item;
	std::string old_program;
	std::string oplist;
	std::string opt;
	long opt_V1;
	long p1;
	long p2;
	long smg_status;
	std::string source;
	long st;
	long sys_status;
	std::string temp;
	long under;
	long x;
	long start;

	screencapture_struct screencapture;
	smg_display_id int_option;
	smg_display_id old_option;
	smg_display_id old_message;
	// --

	// Declare variables
	//
	//
	// Run entry
	//
	int_option = scope.smg_option;
	old_ident = scope.prg_ident + "";
	old_program = scope.prg_program + "";
	old_item = scope.prg_item + "";
	if (boost::trim_right_copy(scope.prg_program) == "MENU_3INTERRUPT")
	{
		goto restorevar;
	}
	scope.prg_ident = "H";
	scope.prg_program = "MENU_3INTERRUPT";
	scope.prg_item = "";
	//*******************************************************************
	// Initilizations
	//*******************************************************************
	//
	// Create the data display
	//
	smg_status = smg$create_virtual_display(2, 132, scope.smg_option, SMG$M_BORDER);
	smg_status = smg$paste_virtual_display(scope.smg_option, scope.smg_pbid, 21, 1);
	oplist = "Menu dcL Document Help eXit";
	opt_V1 = 0;
selectoption:;
	opt = entr_3option(scope, "Interrupt Command Level", oplist, opt_V1, 0);
	//
	// Check for special keys typed
	//
	// ** Converted from a select statement **
	//
	// Exit key typed
	//
	if ((scope.scope_exit == SMG$K_TRM_F10) ||
		(scope.scope_exit == SMG$K_TRM_CTRLZ))
	{
		goto exitfunction;
	}
	//
	// Good keys
	//
	else if ((scope.scope_exit == 0) ||
		((scope.scope_exit == 10) ||
		((scope.scope_exit == 12) ||
		((scope.scope_exit == 13) ||
		(scope.scope_exit == SMG$K_TRM_DO)))))
	{
		// Good key
	}
	else
	{
		//
		// Bad keys
		//
		entr_3badkey(scope, scope.scope_exit);
		goto selectoption;
	}
	//***************************************************************
	// Handle selected item
	//***************************************************************
	//
	// NOTE: The commands are hard coded to the item number, so
	// remember to change this select statement if you re-arrange
	// the menu.
	//
	// ** Converted from a select statement **
	// ++
	// Abstract:MENU
	//	^*Menu Command Level\*
	//	.p
	//	^*Menu Command Level\* allows to enter a macro command
	//	for the  spawn process. Return without any macro command
	//	will spawn directly into CMC Menu.
	//
	// --
	if (opt == "M")
	{
		smg_status = smg$erase_display(scope.smg_option);
		smg_status = smg$put_chars(scope.smg_option, "Menu Command Level: ", 1, 1, 1);
		inp = std::string(55, ' ');
macro:;
		// ** Converted from a select statement **
		// ^C, ^Z, Exit
		start = -1;
		int e3 = entr_3enter(scope, scope.smg_option, 1, 22, inp, start,
			16 + 4096);
	       if ((e3 == SMG$K_TRM_CTRLC) ||
			(e3 == SMG$K_TRM_F8) ||
			(e3 == SMG$K_TRM_F10) ||
			(e3 == SMG$K_TRM_CTRLZ))
		{
			smg_status = smg$pop_virtual_display(scope.smg_option, scope.smg_pbid);
			// Restore OPTION  virtual display
			scope.smg_option = old_option;
			// Restore MESSAGE virtual display
			scope.smg_message = old_message;
			goto exitfunction;
			// Good keys
		}
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto macro;
		}
		inp = basic::edit(inp, 4 + 8 + 128);
		sys_status = lib$set_symbol("CMC$COMMAND", inp);
		smg_status = smg$unpaste_virtual_display(scope.smg_option, scope.smg_pbid);
		subr_3spawn(scope, "menu");
		sys_status = lib$set_symbol("CMC$COMMAND", "");
	}
	else if (opt == "X")
	{
		//
		// Exit
		//
	}
	else if (opt == "H")
	{
		//
		// Help
		//
		help_34message(scope, "", "H", scope.prg_program, "", "HELP");
		goto selectoption;
	}
	else if (opt == "L")
	{
		//
		// DCL
		//
		// ++
		// Abstract:DCL
		//	^*DCL\*
		//	.p
		//	^*DCL\* spawns process into VMS command level.
		//	Prompt ^*$Subprocess\* indicates
		//	spawning process. After typing LOGOUT subprocess returns
		//	back to the current process.
		//
		// --
		smg_status = smg$unpaste_virtual_display(scope.smg_option, scope.smg_pbid);
		subr_3spawn(scope, "");
		//
		// Clipboard
		//
		//	CASE "C"
		// ++
		// Abstract:CLIPBOARD
		//	^*Clipboard\*
		//	.p
		//	^*Clipboard\* allows for selection of  any part of current screen and to paste
		//	it into a text file.
		//
		// --
	}
	else if (opt == "D")
	{
		//
		// Document the current screen
		//
		// ++
		// Abstract:DOCUMENT
		//	^*Document\*
		//	.p
		//	^*Document\* provides documentation of the current screen.
		//	After assigning a document key the screen will be placed
		//	into a text library on SIC: directory.
		//	.note
		//	The current process is temporarily interrupted.
		//	.end note
		//
		// --
		//
		// Save the OPTION  virtual display
		//
		old_option = scope.smg_option;
		smg_status = smg$create_virtual_display(2, 132, scope.smg_option);
		smg_status = smg$paste_virtual_display(scope.smg_option, scope.smg_pbid, 21, 1);
		//
		// Save the MESSAGE virtual display
		//
		old_message = scope.smg_message;
		smg_status = smg$create_virtual_display(2, 132, scope.smg_message);
		smg_status = smg$paste_virtual_display(scope.smg_message, scope.smg_pbid, 23, 1);
		//
		// Ask for a name to store it under
		//
docu:;
		if ((old_program.find("_", 0) + 1))
		{
			source = basic::edit(old_program, 2 + 4 + 32);
		}
		else
		{
			source = basic::edit(old_item, 2 + 4 + 32);
		}
		// ** Converted from a select statement **
		if (basic::edit(old_item, -1) == "VIEWLIST")
		{
			temp = source + "$SCREENVIEWLIST";
		}
		else if (basic::edit(old_item, -1) == "VIEWBY")
		{
			temp = source + "$SCREENVIEWBYE";
		}
		else
		{
			if (old_item.substr(0, 3) == "FLD")
			{
				temp = source + "$SCREENFLD";
			}
			else
			{
				temp = source + "$SCREEN";
			}
		}
		smg_status = smg$erase_display(scope.smg_option);
		smg_status = smg$put_chars(scope.smg_option, "Screen Description:  ", 1, 1, 1);
		inp = temp + std::string(55 - temp.size(), ' ');
		// ** Converted from a select statement **
		// ^C, ^Z, Exit
		long start = -01;
		int e3 = entr_3enter(scope, scope.smg_option, 1, 22, inp, start, 16 + 4096);
	       if (e3 == SMG$K_TRM_CTRLC) ||
			(e3 == SMG$K_TRM_F8) ||
			(e3 == SMG$K_TRM_F10) ||
			(e3  == SMG$K_TRM_CTRLZ))))
		{
			smg_status = smg$pop_virtual_display(scope.smg_option, scope.smg_pbid);
			// Restore OPTION  virtual display
			scope.smg_option = old_option;
			// Restore MESSAGE virtual display
			scope.smg_message = old_message;
			goto exitfunction;
			// Good keys
		}
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto docu;
		}
		inp = basic::edit(inp, 4 + 8 + 128);
		if (inp == "")
		{
			goto docu;
		}
		//
		// Modify name if necessary
		//
		if ((inp.find("$", 0) + 1))
		{
			lib_key = inp;
		}
		else
		{
			lib_key = source + "$" + inp;
		}
		smg_status = smg$pop_virtual_display(scope.smg_option, scope.smg_pbid);
		//
		// Restore OPTION virtual display
		//
		scope.smg_option = old_option;
		//
		// Restore MESSAGE virtual display
		//
		scope.smg_message = old_message;
		smg_status = smg$unpaste_virtual_display(scope.smg_option, scope.smg_pbid);
		//
		// Put the pasteboard out to the temp file
		//
		under = ((source + "_").find("_", 0) + 1);
		lib_name = std::string("SIC:WINDOWS_") + temp.substr(0, under - 1);
		//
		// Create text file to insert into window library
		//
		interupt_file = std::string("WIND_") + read_sysjob() + ".TMP";
		scr_dump(interupt_file.c_str());
		//
		// Insert file into the library
		//
		st = libr_3insert(lib_name, interupt_file, lib_key);
		//
		// Distroy text file
		//
		smg_status = lib$delete_file(interupt_file);
		//
		// Bad keys
		//
	}
	else
	{
		entr_3badkey(scope, scope.scope_exit);
		goto selectoption;
	}
exitfunction:;
	//********************************************************************
	// END PROGRAM
	//********************************************************************
	smg_status = smg$pop_virtual_display(scope.smg_option, scope.smg_pbid);
restorevar:;
	scope.smg_option = int_option;
	scope.prg_ident = old_ident;
	scope.prg_program = old_program;
	scope.prg_item = old_item;
}
