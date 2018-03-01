//! \file
//! \brief Help Function
// %SBTTL "HELP_3MESSAGE"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/help_3message.bas
// Translated from Basic to C++ using btran
// on Thursday, February 15, 2018 at 16:56:42
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <vector>
#include "basicfun.h"

#include "smg/smg.h"
#include "smg/lib.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "cmcfun/scroll.h"


//!
//! Abstract:HELP
//!	.p
//!	This function displays help messages to the screen.
//!
//! Parameters:
//!
//!	MESSAGES
//!		Passed variable that tells what type of error
//!		the user is receiving.
//!
//!	HELP_SEVERITY
//!		Passed variable showing that there is an error
//!		in the program.
//!
//!	HELP_PROGNAME
//!		The passed program name where the error is comming from.
//!
//!	HELP_ITEM
//!		Passed item tells the official name of the error.
//!
//!
//!	This function displays help messages to the screen,
//!	allowing the user to look deeper into the help files
//!	as necessary.
//!
//! Example:
//!
//!	CALL HELP_3MESSAGE("Unable to allocate a channel",
//!		"ERR", "ASSG_BATCH", "ERROR-GETCHAN")
//!
//! Author:
//!
//!	07/28/86 - Kevin Handy
//!
void help_3message(
	scope_struct &scope,
	const std::string &messages,
	const std::string &help_severity,
	const std::string &help_progname,
	const std::string &help_item)
{
	long curr_line;
	std::string default_lib;
	std::string key1;
	std::string key1_name;
	std::string key2;
	std::string key3;
	std::string lib1_name;
	std::string lib_file;
	std::string old_help_item;
	std::string old_help_severity;
	std::string old_program;
	std::string o_help_program;
	std::string severity;
	long smg_status;
	long st;
	std::string s_help_item;
	std::string s_help_program;
	std::string s_help_severity;
	std::string text;
	long under;
	long v;
	long junk;

	//
	// Size of the array
	//
	const long num_lines = 3000;
	std::vector<std::string> line_num;
	line_num.resize(num_lines + 1);
	smg_scroll_cdd smg_scroll;
	smg_display_id svd;
	smg_display_id old_option;
	smg_display_id old_message;

	//*******************************************************************
	// Load in the help text, and process it
	//*******************************************************************
	auto loadall = [&](void)
	{
loadall:
		//
		// Initialize the number of lines currently loaded
		//
		line_num[0] = "0";
		//
		// Try reading specific help file
		//
		st = libr_digsr(lib_file, key1, line_num);
		key1_name = key1;
		lib1_name = lib_file;
		if ((st & 1) == 0)
		{
			//
			// If text not found in main help file, check out
			// the default help file.
			//
			st = libr_digsr(lib_file, key2, line_num);
			if ((st & 1) != 0)
			{
				key1_name = key2;
			}
			else
			{
				//
				// Check only if KEY3 <> KEY2
				//
				if (key3 != key2)
				{
					st = libr_digsr(lib_file, key3, line_num);
				}
				if ((st & 1) != 0)
				{
					key1_name = key3;
				}
				else
				{
					if (lib_file != default_lib)
					{
						lib_file = default_lib;
						goto loadall;
					}
					else
					{
						st = libr_digsr(default_lib, "H$$NODETAIL", line_num);
					}
				}
			}
		}
		curr_line = std::stol(line_num[0]);
		smg_scroll.bot_array = smg_scroll.end_element = curr_line;
		smg_scroll.top_line = smg_scroll.beg_element;
	};

	//
	// Dimension statements
	//
	default_lib = "REF:HELP_DEFAULT";
	//
	// Create virtual display
	//
	smg_status = smg$create_virtual_display(20, 132, svd);
	// Save the OPTION  virtual display
	old_option = scope.smg_option;
	// Save the MESSAGE virtual display
	old_message = scope.smg_message;
	smg_status = smg$create_virtual_display(2, 132, scope.smg_option);
	smg_status = smg$paste_virtual_display(scope.smg_option, scope.smg_pbid, 21, 1);
	smg_status = smg$create_virtual_display(2, 132, scope.smg_message);
	smg_status = smg$paste_virtual_display(scope.smg_message, scope.smg_pbid, 23, 1);
	smg_scroll.window = &svd;
	smg_scroll.scroll_top = 3;
	smg_scroll.scroll_bot = 20;
	smg_scroll.top_array = 1;
	smg_scroll.bot_array = num_lines;
	smg_scroll.top_line = 1;
	smg_scroll.beg_element = 1;
	smg_scroll.end_element = num_lines;
	smg_scroll.cur_line = 1;
	smg_scroll.cur_w_col = 1;
	smg_scroll.cur_w_row = 1;
	smg_scroll.find_line = 1;
	smg_scroll.smg_flag = 1;
	smg_scroll.prompt = "";
	smg_scroll.draw_cols = "";
	// Make copies of the parameters, because if help is called using
	// map information, and help changes them, the calling variables
	// will (magically) change at the same time.
	//
	s_help_severity = basic::edit(help_severity, 2 + 4 + 32);
	o_help_program = s_help_program = basic::edit(help_progname, 2 + 4 + 32);
	s_help_item = basic::edit(help_item, 2 + 4 + 32);
	//
	// If message is not comming from a source code
	//
	{
		auto TempS = s_help_severity;
		if ((TempS == "ERR") ||
			((TempS == "E") ||
			((TempS == "F") ||
			((TempS == "I") ||
			((TempS == "S") ||
			(TempS == "W"))))))
		{
			severity = s_help_severity.substr(0, 1);
		}
		else
		{
			severity = "H";
		}
	}
	text = boost::trim_right_copy(messages);
	if (text == "")
	{
		{
			auto TempS = severity;
			if (TempS == "E")
			{
				text = "error message";
			}
			else if (TempS == "F")
			{
				text = "fatal, or severe error";
			}
			else if (TempS == "H")
			{
				text = "help message";
			}
			else if (TempS == "I")
			{
				text = "information message";
			}
			else if (TempS == "S")
			{
				text = "successful processing";
			}
			else if (TempS == "W")
			{
				text = "warning message";
			}
		}
	}
	//
	// Check for help inside of help
	//
	// Save the COMMON Ident
	old_help_severity = scope.prg_ident;
	// Save the COMMON program name
	old_program = scope.prg_program;
	// Save the COMMON item
	old_help_item = scope.prg_item;
	if (boost::trim_right_copy(scope.prg_program) == "help_3message")
	{
		goto exitprogram;
	}
	scope.prg_program = "help_3message";
	scope.prg_item = "HELP";
	scope.prg_ident = "H";
	under = ((s_help_program + "_").find("_", 0) + 1);
	//
	// Which library is it in?
	//
	if (under == 1)
	{
		lib_file = s_help_item;
		under = ((s_help_item + "_").find("_", 0) + 1);
		// LIB_FILE$ = "help_" + LEFT(S_HELP_ITEM$, UNDER% - 1%)
		o_help_program = s_help_item.substr(0, under - 1);
	}
	else
	{
		lib_file = s_help_program;
	}
	lib_file = std::string("help_") + lib_file.substr(0, under - 1);
	//
	// What is the key name
	//
	key1 = (severity + "$" + s_help_program + "$" + s_help_item).substr(0, 39);
	key2 = (severity + "$$" + s_help_item).substr(0, 39);
	key3 = (severity + "$$" + s_help_item).substr(0, 39);
	//
	// Load array and print it to the virtual display
	//
	loadall();
	junk = 0;
	v = dspl_scroll(smg_scroll, line_num, junk, "PAINT");
	//
	// Paste virtual displays to pasteboard
	//
	if (severity == "H")
	{
		smg_status = smg$paste_virtual_display(svd, scope.smg_pbid, 1, 1);
	}
menu:;
	//
	// Enter desired option
	//
	entr_3message(scope,
		std::string("%") + o_help_program + "-" + severity +
		"-" + s_help_item + ", " + text, 4 + 8);
	{
		auto TempS = scope.scope_exit;
		//
		// ^C
		//
		if ((TempS == SMG$K_TRM_CTRLC) ||
			(TempS == SMG$K_TRM_F11))
		{
			//
			// Print the array
			//
			smg_scroll.top_line = smg_scroll.beg_element;
			junk = 0;
			v = dspl_scroll(smg_scroll, line_num, junk, "PAINT");
		}
		//
		// Movement
		//
		// Uparrow
		// Downarrow
		// Left arrow
		// Right arrow
		// Previous screen
		// Next screen
		// Top
		// Bottom
		else if ((TempS == SMG$K_TRM_UP) ||
			((TempS == SMG$K_TRM_DOWN) ||
			((TempS == SMG$K_TRM_LEFT) ||
			((TempS == SMG$K_TRM_RIGHT) ||
			((TempS == SMG$K_TRM_PREV_SCREEN) ||
	(		(TempS == SMG$K_TRM_NEXT_SCREEN) ||
			((TempS == SMG$K_TRM_F18) ||
			(TempS == SMG$K_TRM_F19))))))))
		{
			v = dspl_scroll(smg_scroll, line_num, scope.scope_exit, "");
			//
			// F16 - Detail
			//
		}
		else if (TempS == SMG$K_TRM_F16)
		{
			smg_status = smg$paste_virtual_display(svd, scope.smg_pbid, 1, 1);
			//
			// F17 - Magic key to edit APP file
			//
		}
		else if (TempS == SMG$K_TRM_F17)
		{
			//*******************************************************************
			// Magic key functions
			//*******************************************************************
			lib$set_symbol("CMC$HELP_LIBRARY", lib1_name);
			lib$set_symbol("CMC$HELP_KEY", key1_name);
			smg_status = lib$spawn("RUN CMC:UT_SPEC_HELP");
			smg_status = smg$set_cursor_mode(scope.smg_pbid, SMG$M_CURSOR_OFF);
			smg_status = smg$repaint_screen(scope.smg_pbid);
			lib$set_symbol("CMC$HELP_LIBRARY", "");
			lib$set_symbol("CMC$HELP_KEY", "");

			loadall();
			junk = 0;
			v = dspl_scroll(smg_scroll, line_num, junk, "PAINT");
			//
			// Other keys are Exit keys
			//
		}
		else
		{
			goto exitprogram;
		}
	}
	goto menu;
exitprogram:;
	//
	// Delete all displays
	//
	smg_status = smg$pop_virtual_display(scope.smg_option, scope.smg_pbid);
	// Restore OPTION  virtual display
	scope.smg_option = old_option;
	// Restore MESSAGE virtual display
	scope.smg_message = old_message;
	scope.prg_ident = old_help_severity + "";
	scope.prg_program = old_program + "";
	scope.prg_item = old_help_item + "";
	return;
}

