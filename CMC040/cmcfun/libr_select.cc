//! \file
//! \brief "Select Item Out of Library, or Edit Library
// %SBTTL "LIBR_SELECT"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/libr_select.bas
// Translated from Basic to C++ using btran
// on Thursday, January 18, 2018 at 17:06:29
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"


#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "cmcfun/scroll.h"
#include "smg/lbr.h"




extern scope_struct scope;

//!
//!
//! Abstract:HELP
//!	.p
//!	This function will select an item out of the library,
//!	edit an existing item into a library, or edit an
//!	item in the library.
//!
//! Parameter:
//!
//!	LIB_NAME$
//!
//!	LIB_TITLE$
//!
//!	LIB_HELP$
//!
//!	OPTLIST$
//!
//! \returns A selected item (Item with an arrow on it).
//!
//! Author:
//!
//!	08/24/87 - Kevin Handy
//!
std::string libr_select(
	const std::string &lib_name,
		//!< The passed name of the library file to edit.
	const std::string &lib_title,
		//!< Passed title to display on select window.
	const std::string &lib_help,
		//!< Variable passed which is used for generating help messages.
	const std::string &xoptlist)
		//!<	Application can select which options
		//!<	to make available to the user.  Valid
		//!<	options are :
		//!<	.table
		//!<		"S" - Select a record in library
		//!<
		//!<		"M" - Edit a record in library
		//!<
		//!<		"C" - Create a new record in library
		//!<
		//!<		"H" - Help message for user
		//!<
		//!<		"X" - Exit to exit from function
		//!<	.endtable
		//!<	OPTLIST$ will default to all options if
		//!<<	value is blank.
{
	std::string Result;
	std::string finame;
	long flagw;
	std::string inp;
	std::string lib_key;
	std::string lib_store_item;
	std::string lib_store_program;
	long loop;
	long opt2;
	long select_count;
	smg_display_id smg_select;
	long smg_status;
	long st;
	std::string temp;
	std::string temp_text;
	long test_count;
	long v;
	long width;
	long junk;
	std::string junks;
	std::string optlist = xoptlist;

	BStack(20);
	smg_scroll_cdd select_scroll;
	std::vector<std::string> zlibr_index;
	zlibr_index.resize(2001);
	//
	// Declare variables
	//
	lib_store_program = scope.prg_program;
	lib_store_item = scope.prg_item;
	scope.prg_program = lib_help;
	//*******************************************************************
	// Step 1 - Get an index of the file
	//*******************************************************************
	opt2 = 0;
	zlibr_index[0] = "0";
	libr_index(lib_name, "*", zlibr_index);
	select_count = std::stol(zlibr_index[0]);
	//*******************************************************************
	// Step 2 - Create a scrolling region to display junk in
	//*******************************************************************
	width = lib_title.size() + 2;
	if (width < 20)
	{
		width = 20;
	}
	if (width > 40)
	{
		width = 40;
	}
	smg_status = smg$create_virtual_display(15, width, smg_select, SMG$M_BORDER);
	smg_status = smg$label_border(smg_select, lib_title);
	select_scroll.window = &smg_select;
	select_scroll.top_array = 1;
	if (select_count == 0)
	{
		select_scroll.bot_array = 1;
	}
	else
	{
		select_scroll.bot_array = select_count;
	}
	select_scroll.scroll_top = 1;
	select_scroll.scroll_bot = 15;
	select_scroll.beg_element = 1;
	select_scroll.end_element = select_scroll.bot_array;
	select_scroll.top_line = 1;
	select_scroll.cur_line = 1;
	select_scroll.cur_w_row = 1;
	select_scroll.cur_w_col = 1;
	select_scroll.find_line = 1;
	select_scroll.smg_flag = 0;
	select_scroll.prompt = "->";
	select_scroll.video_comp = 0;
	select_scroll.charset = 0;
	select_scroll.draw_cols = "";
	junk = 0;
	v = dspl_scroll(select_scroll, zlibr_index, junk, "PAINT");
	smg_status = smg$paste_virtual_display(smg_select, scope.smg_pbid, 4, 43);
	//*******************************************************************
L_3000:;
	// Now, give the user his options
	//*******************************************************************
	//
	// Ask for option
	//
	scope.prg_item = "";
	if (boost::trim_right_copy(optlist) == "")
	{
		optlist = "Select Maintain Create Getmaster Putmaster Help eXit";
	}
	inp = entr_3option(scope, "COMMAND", optlist, opt2, flagw);
	//
	// Handle funny terminators
	//
	{
		auto TempS = scope.scope_exit;
		//
		// Cursor movement
		//
		if ((TempS == SMG$K_TRM_UP) ||
			(TempS == SMG$K_TRM_DOWN) ||
			(TempS == SMG$K_TRM_PREV_SCREEN) ||
			(TempS == SMG$K_TRM_NEXT_SCREEN) ||
			(TempS == SMG$K_TRM_F18) ||
			(TempS == SMG$K_TRM_F19))
		{
			v = dspl_scroll(select_scroll, zlibr_index, scope.scope_exit, "PAINT");
			goto L_3000;
		}
		//
		// Exit characters
		//
		else if ((TempS == SMG$K_TRM_F10) || (TempS == SMG$K_TRM_CTRLZ))
		{
			goto exitprogram;
		}
		//
		// Select character
		//
		else if (TempS == SMG$K_TRM_SELECT)
		{
			inp = "M";
		}
		//
		// Good characters
		//
		else if ((TempS == 0) || ((TempS == 10) || ((TempS == 12) || ((TempS == 13) || (TempS == SMG$K_TRM_DO)))))
		{
		}
		//
		// Bad characters
		//
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto L_3000;
		}
	}
	//
	// Process user options
	//
	{
		auto TempS = inp;
		//
		// Select record
		//
		if (TempS == "S")
		{
			if (boost::trim_right_copy(zlibr_index[select_scroll.cur_line]) == "")
			{
				entr_3message(scope, "A form must be created before selecting it.", 0);
				goto L_3000;
			}
			goto exitselect;
		}
		//
		// Edit current record
		//
		else if (TempS == "M")
		{
			if (boost::trim_right_copy(zlibr_index[select_scroll.cur_line]) == "")
			{
				entr_3message(scope, "A form must be created before maintaining it.", 0);
				goto L_3000;
			}
			lib_key = zlibr_index[select_scroll.cur_line];
			v = libr_maintnodsr(lib_name, lib_key, lib_title + " - " + lib_key, 2);
			BGosub(reload);
		}
		//
		// Create new text file
		//
		else if (TempS == "C")
		{
L_4100:;
			junks = "";
			lib_key = entr_3string(scope, scope.smg_option, "",
				"File to create", std::string(49, ' '),
				0, "", junks, 49);
			//
			// Handle funny terminators
			//
			{
				auto TempS = scope.scope_exit;
				//
				// Exit characters
				//
				if ((TempS == SMG$K_TRM_F10) ||
					(TempS == SMG$K_TRM_CTRLZ))
				{
					goto L_3000;
					//
					// Good characters
					//
				}
				else if ((TempS == 0) ||
					(TempS == 10) ||
					(TempS == 12) ||
					(TempS == 13) ||
					(TempS == SMG$K_TRM_DO))
				{
					//
					// Bad characters
					//
				}
				else
				{
					entr_3badkey(scope, scope.scope_exit);
					goto L_4100;
				}
			}
			if (boost::trim_right_copy(lib_name) == "")
			{
				entr_3message(scope, "A form name must be given before process can continue", 0);
				goto L_3000;
			}
			v = libr_maintnodsr(lib_name, lib_key, lib_title, 2);
			BGosub(reload);
		}
		//
		// Getmaster
		//
		else if (TempS == "G")
		{
			//
			// Get name of system library
			//
			BGosub(getmastername);
			//
			// Ask for item to get
			//
			junks = "";
			finame = boost::trim_right_copy(entr_3string(scope,
				scope.smg_option, "", "Form to get",
				std::string(32, ' '), 0, "", junks,  32));

			switch(scope.scope_exit)
			{
			//
			// Exit keys
			//
			case SMG$K_TRM_F8:
			case SMG$K_TRM_F10:
			case SMG$K_TRM_CTRLZ:
				goto L_3000;
			//
			// Good keys
			//
			case 0:
			case 10:
			case 12:
			case 13:
			case SMG$K_TRM_DO:
				break;
			//
			// Bad keys
			//
			default:
				entr_3badkey(scope, scope.scope_exit);
				goto L_3000;
			}
			//
			// Extract file
			//
			st = libr_extractvar(temp, temp_text, finame);
			if ((st & 1) == 0)
			{
				entr_3message(scope, std::string("Error in extract ") + std::to_string(st), 0);
				goto L_3000;
			}
			//
			// Insert file
			//
			st = libr_insertvar(lib_name, temp_text, finame);
			if ((st & 1) == 0)
			{
				entr_3message(scope, std::string("Error in Insert ") + std::to_string(st), 0);
				goto L_3000;
			}
			BGosub(reload);
		}
		//
		// Putmaster
		//
		else if (TempS == "P")
		{
			//
			// Get name of system library
			//
			BGosub(getmastername);
			//
			// Ask for item to get
			//
			finame = zlibr_index[select_scroll.cur_line];
			junks = "";
			finame = boost::trim_right_copy(entr_3string(scope,
				scope.smg_option, "", "Form to put", finame,
				0, "", junks, 32));

			switch(scope.scope_exit)
			{
			//
			// Exit keys
			//
			case SMG$K_TRM_F8:
			case SMG$K_TRM_F10:
			case SMG$K_TRM_CTRLZ:
				goto L_3000;
			//
			// Good keys
			//
			case 0:
			case 10:
			case 12:
			case 13:
			case SMG$K_TRM_DO:
				break;
			//
			// Bad keys
			//
			default:
				entr_3badkey(scope, scope.scope_exit);
				goto L_3000;
			}
			//
			// Extract file
			//
			st = libr_extractvar(lib_name, temp_text, finame);
			if ((st & 1) == 0)
			{
				entr_3message(scope, std::string("Error in extract ") + std::to_string(st), 0);
				goto L_3000;
			}
			//
			// Insert file
			//
			st = libr_insertvar(temp, temp_text, finame);
			if ((st & 1) == 0)
			{
				entr_3message(scope, std::string("Error in Insert ") + std::to_string(st), 0);
				goto L_3000;
			}
			BGosub(reload);
			//
			// Help
			//
		}
		else if (TempS == "H")
		{
			help_3message(scope, "", scope.prg_ident, scope.prg_program, "HELP");
			//
			// Exit
			//
		}
		else if (TempS == "X")
		{
			goto exitprogram;
		}
	}
	goto L_3000;
reload:;
	//
	// Set lib index to blank
	//
	test_count = select_count;
	libr_index(lib_name, "*", zlibr_index);
	select_count = std::stol(zlibr_index[0]);
	for (loop = select_count + 1; loop <= test_count; loop++)
	{
		zlibr_index[loop] = std::string(10, ' ');
	}
	if (test_count > select_count)
	{
		select_scroll.find_line = 1;
		junk = 0;
		v = dspl_scroll(select_scroll, zlibr_index, junk, "PAINT");
	}
	select_scroll.bot_array = select_count;
	if (select_count == 0)
	{
		select_scroll.bot_array = 1;
	}
	select_scroll.end_element = select_scroll.bot_array;
	if (test_count <= select_count)
	{
		junk = 0;
		v = dspl_scroll(select_scroll, zlibr_index, junk, "PAINT");
	}
	BReturn;

	//*******************************************************************
	// Figure out a good name for the master file.
	// Return as TEMP$.
	//*******************************************************************
getmastername:;
	temp = lib_name;
	//
	// Strip off device names
	//
getmastername1:;
	v = (temp.find(":", 0) + 1);
	if (v)
	{
		temp = basic::right(temp, v + 1);
		goto getmastername1;
	}
	//
	// Strip off account names
	//
getmastername2:;
	v = (temp.find("]", 0) + 1);
	if (v)
	{
		temp = basic::right(temp, v + 1);
		goto getmastername2;
	}
	//
	// Strip off version numbers
	//
	v = (temp.find(";", 0) + 1);
	if (v)
	{
		temp = temp.substr(0, v - 1);
	}
	temp = std::string("CMC:") + temp;
	BReturn;

	//*******************************************************************
	// Exit and select current record
	//*******************************************************************
exitselect:;
	Result = zlibr_index[select_scroll.cur_line];
exitprogram:;
	smg_status = smg$delete_virtual_display(smg_select);
	smg_status = smg$erase_display(scope.smg_option);
	smg_status = smg$erase_display(scope.smg_message);
	scope.prg_program = lib_store_program;
	scope.prg_item = lib_store_item;
	return Result;
}
