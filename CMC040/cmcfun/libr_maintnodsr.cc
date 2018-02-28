//! \file
//! \btief Library Maintenance without DSR
// %SBTTL "LIBR_MAINTNODSR"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/libr_maintnodsr.bas
// Translated from Basic to C++ using btran
// on Friday, February 16, 2018 at 19:33:33
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "smg/smg.h"
#include "smg/lib.h"
#include "scopedef.h"
#include "cmcfun/scroll.h"


extern scope_struct scope;



//!
//! Abstract:HELP
//!	.p
//!	This function displays text from a library about
//!	a specific key, and allows maintenance of that
//!	text. It will not format it using DSR.
//!
//! Parameters:
//!
//!	LIB_FILE$
//!		Passed name of library file to use
//!
//!	KEY_NAME$
//!		Full passed key name for text
//!
//!	PTITLE$
//!		Passed title to show on top of screen
//!
//!	PFLAG%
//!		Flag for special reasons.
//!		Not currently used.
//!
//!	Returned value
//!		A status in same format as DEC's functions.
//!		(Currently set to 1 only).
//!
//! Author:
//!
//!	07/08/87 - Kevin Handy
//!
long libr_maintnodsr(
	const std::string &lib_file,
	const std::string &key_name,
	const std::string &ptitle,
	long pflag)
{
	long Result;
	std::string app_file;
	long curr_line;
	std::string finame;
	std::string old_ident;
	std::string old_item;
	std::string old_program;
	std::string oplist;
	std::string opt;
	long opt_V2;
	long optflag;
	std::string preff;
	long ps;
	std::string rinp;
	long smg_status;
	long st;
	std::string temp_file;
	long v;
	std::string xyz;
	long junk;
	std::string sjunk;

	BStack(20);
	smg_scroll_cdd smg_scroll;
	smg_display_id svd;

	//
	// --
	//
	// Declarations
	//
	// Size of the array
	const long num_lines = 600;
	std::vector<std::string> line_num;
	line_num.resize(num_lines + 1);
	//
	// Dimension statements
	//
	//
	// Assume successful status
	//
	Result = 1;
	//
	// Think up temp file name
	//
	temp_file = std::string("/tmp/") + read_sysjob() + ".tmp";
	//
	// Create virtual display
	//
	smg_status = smg$create_virtual_display(18, 78, svd);
	//
	// Label the borders of system and user
	//
	smg_status = smg$label_border(svd, ptitle);
	smg_scroll.window = &svd;
	smg_scroll.scroll_top = 1;
	smg_scroll.scroll_bot = 18;
	smg_scroll.top_array = 1;
	smg_scroll.bot_array = num_lines;
	smg_scroll.top_line = 1;
	smg_scroll.beg_element = 1;
	smg_scroll.end_element = num_lines;
	smg_scroll.cur_line = 1;
	smg_scroll.cur_w_col = 1;
	smg_scroll.cur_w_row = 1;
	smg_scroll.find_line = 1;
	smg_scroll.smg_flag = 0;
	smg_scroll.prompt = "";
	smg_scroll.draw_cols = "";
	//
	// Set up for help
	//
	old_ident = scope.prg_ident;
	old_program = scope.prg_program;
	old_item = scope.prg_item;
	scope.prg_ident = "H";
	scope.prg_program = "LIBR_MAINTNODSR";
	scope.prg_item = "HELP";
	//
	// Option list
	//
	opt_V2 = 0;
	//
	// Load array and print it to the virtual display
	//
	BGosub(loadall);
	junk = 0;
	v = dspl_scroll(smg_scroll, line_num, junk, "PAINT");
	//
	// Paste virtual displays to pasteboard
	//
	smg_status = smg$paste_virtual_display(svd, scope.smg_pbid, 2, 2);
	goto menu;
initarray:;
	//
	// Print the array
	//
	smg_scroll.top_line = smg_scroll.beg_element;
	junk = 0;
	v = dspl_scroll(smg_scroll, line_num, junk, "PAINT");
	//*******************************************************************
	// Main menu
	//*******************************************************************
menu:;
	//
	// Enter desired option
	//
	oplist = "Edit Delete exTract Help Print eXit";
	opt = entr_3option(scope, "COMMAND", oplist, opt_V2, optflag);
	{
		auto TempS = scope.scope_exit;
		//
		// ^C
		//
		if (TempS == SMG$K_TRM_CTRLC)
		{
			goto initarray;
			//
			// Exit key
			//
		}
		else if ((TempS == SMG$K_TRM_F8) ||
			((TempS == SMG$K_TRM_F10) ||
			 (TempS == SMG$K_TRM_CTRLZ)))
		{
			goto exitprogram;
			//
			// Find key
			//
			// Find
		}
		else if (TempS == SMG$K_TRM_FIND)
		{
			opt = "F";
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
		}
		else if ((TempS == SMG$K_TRM_UP) ||
			(TempS == SMG$K_TRM_DOWN) ||
			(TempS == SMG$K_TRM_LEFT) ||
			(TempS == SMG$K_TRM_RIGHT) ||
			(TempS == SMG$K_TRM_PREV_SCREEN) ||
			(TempS == SMG$K_TRM_NEXT_SCREEN) ||
			(TempS == SMG$K_TRM_F18) ||
			(TempS == SMG$K_TRM_F19))
		{
			v = dspl_scroll(smg_scroll, line_num, scope.scope_exit, "");
			goto menu;
			//
			// Good keys
			//
		}
		else if ((TempS == 0) ||
			(TempS == 10) ||
			(TempS == 12) ||
			(TempS == 13) ||
			(TempS == SMG$K_TRM_DO) ||
			(TempS == SMG$K_TRM_F7))
		{
			//
			// Bad keys
			//
		}
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto menu;
		}
	}
	//
	// Decide what to do with the option
	//
	{
		auto TempS = opt;
		//
		// Delete text
		//
		if (TempS == "D")
		{
			//
			// Confirm deletion
			//
			opt = entr_3yesno(scope, scope.smg_option, "", "Really delete text", "N", 0, "", sjunk);
			{
				auto TempS = scope.scope_exit;
				//
				// Exit keys
				//
				if ((TempS == SMG$K_TRM_F8) ||
					((TempS == SMG$K_TRM_F10) ||
					(TempS == SMG$K_TRM_CTRLZ)))
				{
					goto menu;
					//
					// Good keys
					//
				}
				else if ((TempS == 0) ||
					((TempS == 10) ||
					((TempS == 12) ||
					((TempS == 13) ||
					(TempS == SMG$K_TRM_DO)))))
				{
					//
					// Bad keys
					//
				}
				else
				{
					entr_3badkey(scope, scope.scope_exit);
					goto menu;
				}
			}
			//
			// Didn't answer "Y"
			//
			if (opt != "Y")
			{
				goto menu;
			}
			//
			// Actual delete command
			//
			st = libr_delete(lib_file, key_name);
			if ((st & 1) == 0)
			{
				entr_3message(scope, std::string("Error in delete ") + std::to_string(st), 0);
			}
			//
			// Reload text
			//
			BGosub(loadall);
			//
			// Re-display text
			//
			junk = 0;
			v = dspl_scroll(smg_scroll, line_num, junk, "PAINT");
		}
		//
		// Edit text
		//
		else if (TempS == "E")
		{
			//
			// Edit file
			//
			st = libr_edit(lib_file, key_name);
			//
			// Refresh screen
			//
			st = smg$repaint_screen(scope.smg_pbid);
			//
			// Reload text
			//
			BGosub(loadall);
			//
			// Re-display text
			//
			junk = 0;
			v = dspl_scroll(smg_scroll, line_num, junk, "PAINT");
		}
		//
		// extract file
		//
		else if (TempS == "T")
		{
			finame = boost::trim_right_copy(entr_3string(scope, scope.smg_option, "", "File to write into", std::string(32, ' '), 0, "", sjunk,32));
			{
				auto TempS = scope.scope_exit;
				//
				// Exit keys
				//
				if ((TempS == SMG$K_TRM_F8) ||
					((TempS == SMG$K_TRM_F10) ||
					(TempS == SMG$K_TRM_CTRLZ)))
				{
					goto menu;
					//
					// Good keys
					//
				}
				else if ((TempS == 0) ||
					((TempS == 10) ||
					((TempS == 12) ||
					((TempS == 13) ||
					(TempS == SMG$K_TRM_DO)))))
				{
					//
					// Bad keys
					//
				}
				else
				{
					entr_3badkey(scope, scope.scope_exit);
					goto menu;
				}
			}
			//
			// Null file name
			//
			if (finame == "")
			{
				goto menu;
			}
			//
			// Extract file
			//
			st = libr_extract(lib_file, finame, key_name);
			if ((st & 1) == 0)
			{
				entr_3message(scope, std::string("Error in extract ") + std::to_string(st), 0);
				goto menu;
			}
		}
		//
		// Help
		//
		else if (TempS == "H")
		{
			ps = ((key_name + "$").find("$", 0) + 1);
			preff = key_name.substr(0, ps - 1);
			help_34message(scope, "", "H", key_name.substr(0, 2) + "_FORM", "", preff);
		}
		//
		// Print the Help file
		//
		else if (TempS == "P")
		{
printfile:;
			smg_status = smg$put_chars(scope.smg_option, "Output To: ", 1, 1, 1);
			rinp = std::string(67, ' ');
			// ** Converted from a select statement **
			{
				junk = -1;
				auto TempS = entr_3enter(scope, scope.smg_option, 1, 13, rinp, junk, 16 + 4096);
				//
				// ^C, ^Z, Exit
				//
				if ((TempS == SMG$K_TRM_CTRLC) ||
					((TempS == SMG$K_TRM_F8) ||
					((TempS == SMG$K_TRM_F10) ||
					(TempS == SMG$K_TRM_CTRLZ))))
				{
					goto menu;
				}
				//
				// Good keys
				//
				else if ((TempS == 0) ||
					((TempS == 10) ||
					((TempS == 12) ||
					((TempS == 13) ||
					((TempS == SMG$K_TRM_DO) ||
					(TempS == SMG$K_TRM_F7))))))
				{
				}
				//
				// Bad keys
				//
				else
				{
					entr_3badkey(scope, scope.scope_exit);
					goto printfile;
				}
			}
			rinp = basic::edit(rinp, 4 + 8 + 128 + 256);
			//
			// Extract help from library
			//
			app_file = "";
			st = libr_extract(lib_file, temp_file, key_name);
			if ((st & 1) == 1)
			{
				app_file = temp_file;
			}
			xyz = std::string("TYPE/OUTPUT=") + rinp + " " + app_file;
			smg_status = lib$spawn(xyz);
			sleep(1);
			smg_status = smg$repaint_screen(scope.smg_pbid);
			BGosub(L_8000);
		}
		//
		// Exit
		//
		else if (TempS == "X")
		{
			goto exitprogram;
		}
	}
	optflag = 0;
	goto menu;
exitprogram:;
	//
	// Finish out
	//
	//
	// Delete all displays
	//
	smg_status = smg$delete_virtual_display(svd);
	scope.prg_ident = old_ident;
	scope.prg_program = old_program;
	scope.prg_item = old_item;
	return Result;
loadall:;
	//
	line_num[0] = "0";
	st = libr_nodigsr(lib_file, key_name, line_num);
	curr_line = std::stol(line_num[0]);
	smg_scroll.bot_array = smg_scroll.end_element = curr_line;
	if (curr_line < 0)
	{
		smg_scroll.bot_array = smg_scroll.end_element = 1;
	}
	smg_scroll.top_line = smg_scroll.beg_element;
	BReturn;

	//*******************************************************************
L_8000:;
	// Try to kill all versions of the temp file
	//*******************************************************************
	smg_status = lib$delete_file(temp_file);
	BReturn;

	return Result;
}
// +-+-+
// ++
// Abstract:DELETE
//	^*Delete\*
//	.p
//	The ^*Delete\* is used to remove a form that is unecessary or
//	unwanted.
//
// Index:
//	.x Delete>Form
//	.x Form>Delete
//
// --
