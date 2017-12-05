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
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "peferences.h"
#include "cmcfun.h"
#include 'scopedef.h"
#include "smg/smg.h"

//!
//!
//! Abstract:HELP
//!	.p
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
//! Author:
//!
// \author 12/01/89 - Kevin Handy
//!	Taken from MENU_INTERRUPT.
//!
void menu_3interrupt(
	scope_struct &scope)
{
	long i;
	std::string inp;
	long interupt_ch;
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

	screencapture_struct screencapture;
	long int_option;
	long old_option;
	long old_message;
	// --

	//
	// Include files
	//
	struct screencapture_struct
	{
		long lines;
		std::string screen[25];
	};
	//
	// External functions
	//
	extern long dspl_screencapture_V2([Unknown Node Type 416@169], [Unknown Node Type 351@169]);
	//
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
		//
		// Good keys
		//
	}
	else if ((scope.scope_exit == 0) ||
		((scope.scope_exit == 10) ||
		((scope.scope_exit == 12) ||
		((scope.scope_exit == 13) ||
		(scope.scope_exit == SMG$K_TRM_DO)))))
	{
		// Good key
		//
		// Bad keys
		//
	}
	else
	{
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
	// Index:
	//	.x Menu>Command
	//	.x Command>Menu
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
		if ((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == SMG$K_TRM_CTRLC) ||
			((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == SMG$K_TRM_F8) ||
			 ((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == SMG$K_TRM_F10) ||
			  (entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == SMG$K_TRM_CTRLZ))))
		{
			smg_status = smg$pop_virtual_display(scope.smg_option, scope.smg_pbid);
			// Restore OPTION  virtual display
			scope.smg_option = old_option;
			// Restore MESSAGE virtual display
			scope.smg_message = old_message;
			goto exitfunction;
			// Good keys
		}
		else if ((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == 0) ||
			((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == 10) ||
			((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == 12) ||
			((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == 13) ||
			((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == SMG$K_TRM_DO) ||
			(entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == SMG$K_TRM_F7))))))
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto macro;
		}
		inp = basic::edit(inp, 4 + 8 + 128);
		sys_status = lib$set_symbol("CMC$COMMAND", inp);
		smg_status = smg$unpaste_virtual_display(scope.smg_option, scope.smg_pbid);
		subr_3spawn(scope, "MENU");
		sys_status = lib$set_symbol("CMC$COMMAND", "");
		//
		// Exit
		//
	}
	else if (opt == "X")
	{
		//
		// Help
		//
	}
	else if (opt == "H")
	{
		help_34message(scope, "", "H", scope.prg_program, "", "HELP");
		goto selectoption;
		//
		// DCL
		//
	}
	else if (opt == "L")
	{
		// ++
		// Abstract:DCL
		//	^*DCL\*
		//	.p
		//	^*DCL\* spawns process into VMS command level.
		//	Prompt ^*$Subprocess\* indicates
		//	spawning process. After typing LOGOUT subprocess returns
		//	back to the current process.
		//
		// Index:
		//	.x DCL
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
		// Index:
		//	.x Clipboard>Command
		//	.x Command>Clipboard
		//
		// --
		//
		// Document the current screen
		//
	}
	else if (opt == "D")
	{
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
		// Index:
		//	.x Document>Command
		//	.x Command>Document
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
		if ((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == SMG$K_TRM_CTRLC) || ((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == SMG$K_TRM_F8) || ((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == SMG$K_TRM_F10) || (entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == SMG$K_TRM_CTRLZ))))
		{
			smg_status = smg$pop_virtual_display(scope.smg_option, scope.smg_pbid);
			// Restore OPTION  virtual display
			scope.smg_option = old_option;
			// Restore MESSAGE virtual display
			scope.smg_message = old_message;
			goto exitfunction;
			// Good keys
		}
		else if ((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == 0) || ((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == 10) || ((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == 12) || ((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == 13) || ((entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == SMG$K_TRM_DO) || (entr_3enter(scope, scope.smg_option, 1, 22, inp, -1, 16 + 4096) == SMG$K_TRM_F7))))))
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
		screencapture.lines = 0;
		for (x = 0; x <= 24; x++)
		{
			screencapture.screen[x] = "";
		}
		p1 = &(dspl_screencapture);
		p2 = &(screencapture);
		smg_status = smg$put_pasteboard(&scope.smg_pbid, p1, p2, &0);
		//
		// Insert text file into a library
		//
		under = ((source + "_").find("_", 0) + 1);
		lib_name = std::string("SIC:WINDOWS_") + temp.substr(0, under - 1);
		//
		// Create text file to insert into window library
		//
		interupt_file = std::string("WIND_") + read_sysjob + ".TMP";
		smg_status = lib$get_lun[interupt_ch];
		BasicChannel[interupt_ch].ForOutput();
		BasicChannel[interupt_ch].SetRecordSize(255);
		BasicChannel[interupt_ch].open(interupt_file);
		if (!BasicChannel[interupt_ch].is_open() { throw BasicError(5); }
		for (i = 0; i <= 23; i++)
		{
			BasicChannel[interupt_ch] << boost::trim_right_copy(screencapture.screen[i]) << std::endl;
		}
		BasicChannel[interupt_ch].close();
		smg_status = lib$free_lun[interupt_ch];
		//
		// Insert file into the library
		//
		st = libr_3insert(lib_name, interupt_file, lib_key);
		//
		// Distroy text file
		//
		smg_status = lib$delete_file(interupt_file + ";*");
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
