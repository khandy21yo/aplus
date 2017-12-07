//! \file
//! \brief Help Function
// %SBTTL "HELP_34MESSAGE"
// %IDENT "V3.6a Calico"

//
// Source: /home/kevin/aplus/CMC030/cmcfun/source/help_34message.bas
// Translated from Basic to C++ using btran
// on Monday, December 04, 2017 at 01:01:48
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "smg/smg.h"

//! \brief Help Function
//!
//!	This function displays messages to the screen.
//!	.p
//!	If severity of the message is ^*H\* (help message)
//!	then full message will be displayed. Otherwise just
//!	a brief highlighted two lines ares appears. By pressing
//!	^*Do\* key, full message shows up.
//!
//!	This function displays help messages to the screen,
//!	allowing the user to look deeper into the help files
//!	as necessary.
//!
//! Example:
//!
//!	CALL HELP_34MESSAGE("Unable to allocate a channel",
//!		"ERR", "ASSG_BATCH", "ERROR-GETCHAN")
//!
//! \author 07/28/86 - Kevin Handy
//!
void help_34message(
	scope_struct &scope,
	const std::string &messages,
		//!< Passed variable that tells what type of error
		//! the user is receiving.
	const std::string &help_severity,
		//!> Passed variable showing that there is an error
		//! in the program.
	const std::string &help_progname,
		//!> The passed program name where the error is comming from.
	const std::string &help_filename,
	const std::string &help_item)
		//!> Passed item tells the official name of the error.
{
	long curr_line;
	std::string default_lib;
	long hit;
	std::string key1;
	std::string key1_name;
	std::string key2;
	std::string key3;
	std::string lib1_name;
	std::string lib_file;
	long lr_index;
	std::string old_help_item;
	std::string old_help_severity;
	std::string old_program;
	std::string orig_lib;
	std::string o_help_program;
	std::string severity;
	long smg_status;
	long st;
	std::string s_help_filename;
	std::string s_help_item;
	std::string s_help_program;
	std::string s_help_severity;
	std::string text;
	long under;
	long v;

	BStack(20);
	smg_scroll_cdd smg_scroll;
	long svd;
	long old_option;
	long old_message;
	std::string line_num[num_lines + 1];
	//
	// Declarations
	//
	// Size of the array
	const long num_lines = 3000;
	//
	// Dimension statements
	//
	default_lib = "REF:HELP_DEFAULT";
	// Save the COMMON Ident
	old_help_severity = scope.prg_ident + "";
	// Save the COMMON program name
	old_program = scope.prg_program + "";
	// Save the COMMON item
	old_help_item = scope.prg_item + "";
	// Make copies of the parameters, because if help is called using
	// map information, and help changes them, the calling variables
	// will (magically) change at the same time.
	//
	s_help_severity = basic::edit(help_severity, 2 + 4 + 32);
	o_help_program = s_help_program = basic::edit(help_progname, 2 + 4 + 32);
	s_help_filename = basic::edit(help_filename, 2 + 4 + 32);
	s_help_item = basic::edit(help_item, 2 + 4 + 32);
	if (o_help_program == "")
	{
		o_help_program = s_help_item;
	}
	//
	// Don't display S_HELP_FILENAME$ if assumed to be a system name
	//
	if (s_help_filename.size() < 3)
	{
		text = boost::trim_right_copy(messages);
	}
	else
	{
		text = boost::trim_right_copy(s_help_filename) + " " + boost::trim_right_copy(messages);
	}
	severity = s_help_severity.substr(0, 1);
	// ** Converted from a select statement **
	if ((severity == "E") || (severity == "F"))
	{
		hit = 1;
	}
	else if ((severity == "I") || ((severity == "S") || (severity == "W")))
	{
		hit = 0;
	}
	else
	{
		severity = "H";
		hit = 1;
	}
	if (severity == "I")
	{
		smg_status = smg$set_cursor_mode(scope.smg_pbid, SMG$M_CURSOR_OFF);
		//
		// Just display (don't stop)
		entr_3message(scope, text, 1 + 16);
		goto restorescope;
	}
	scope.prg_program = "HELP_34MESSAGE";
	scope.prg_item = "HELP";
	scope.prg_ident = "H";
	// Save the OPTION  virtual display
	old_option = scope.smg_option;
	// Save the MESSAGE virtual display
	old_message = scope.smg_message;
	//
	// Create virtual display
	//
	smg_status = smg$create_virtual_display(20, 132, svd);
	smg_status = smg$create_virtual_display(1, 132, scope.smg_option);
	smg_status = smg$create_virtual_display(2, 132, scope.smg_message, SMG$M_BORDER);
	smg_scroll.window = svd;
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
	under = ((s_help_program + "_").find("_", 0) + 1);
	//
	// Which library is it in?
	//
	if (under == 1)
	{
		lib_file = s_help_item;
		under = ((s_help_item + "_").find("_", 0) + 1);
		o_help_program = s_help_item.substr(0, under - 1);
	}
	else
	{
		lib_file = s_help_program;
	}
	lib_file = orig_lib = std::string("REF:HELP_") + lib_file.substr(0, under - 1);
	//
	// What is the key name
	//
	key1 = (severity + "$" + s_help_program + "$" + s_help_filename + s_help_item).substr(0, 39);
	key2 = (severity + "$" + s_help_filename + "$" + s_help_item).substr(0, 39);
	key3 = (severity + "$$" + s_help_item).substr(0, 39);
	//
	// Load array and print it to the virtual display
	//
	//	GOSUB LoadAll
	//
	// Paste virtual displays to pasteboard
	//
	if (severity == "H" || severity == "E" || severity == "F")
	{
		BGosub(loadall);
		smg_status = smg$paste_virtual_display(svd, scope.smg_pbid, 1, 1);
		smg_status = smg$paste_virtual_display(scope.smg_option, scope.smg_pbid, 21, 1);
	}
	smg_status = smg$paste_virtual_display(scope.smg_message, scope.smg_pbid, 23, 1);
menu:;
	//
	// Display desired message
	//
	if (hit == 0)
	{
		if (severity == "W")
		{
			smg_status = smg$ring_bell(scope.smg_kbid);
			smg_status = smg$ring_bell(scope.smg_kbid);
		}
		entr_3message(scope, text, 4 + 8);
	}
	else
	{
		entr_3message(scope, std::string("%") + o_help_program + "-" + severity + "-" + s_help_item + ", " + text, 4 + 8);
	}
	// ** Converted from a select statement **
	//
	// ^C
	//
	if ((scope.scope_exit == smg$k_trm_ctrlc) || (scope.scope_exit == smg$k_trm_f11))
	{
		//
		// Print the array
		//
		smg_scroll.top_line = smg_scroll.beg_element;
		v = dspl_scroll(smg_scroll, line_num, 0, "PAINT");
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
	else if ((scope.scope_exit == smg$k_trm_up) || ((scope.scope_exit == smg$k_trm_down) || ((scope.scope_exit == smg$k_trm_left) || ((scope.scope_exit == smg$k_trm_right) || ((scope.scope_exit == smg$k_trm_prev_screen) || ((scope.scope_exit == smg$k_trm_next_screen) || ((scope.scope_exit == smg$k_trm_f18) || (scope.scope_exit == smg$k_trm_f19))))))))
	{
		v = dspl_scroll(smg_scroll, line_num, scope.scope_exit, "");
		//
		// F17 - Magic key to edit APP file
		//
	}
	else if (scope.scope_exit == smg$k_trm_f17)
	{
		entr_3message(scope, "", 1 + 16);
		//
		// Figure out the help key
		//
		if (hit == 0)
		{
			BGosub(loadall);
		}
		st = lbr$ini_control(lr_index, lbr$c_read);
		st = lbr$open(lr_index, lib1_name, 0, ".TLB");
		if ((st & 1) == 0)
		{
			lib1_name = default_lib;
		}
		st = lbr$close(lr_index);
		lib$set_symbol("CMC$HELP_LIBRARY", lib1_name);
		lib$set_symbol("CMC$HELP_KEY", key1_name);
		smg_status = smg$disable_broadcast_trapping(scope.smg_pbid);
		smg_status = lib$spawn("RUN CMC:UT_SPEC_HELP");
		smg_status = smg$set_cursor_mode(scope.smg_pbid, smg$m_cursor_off);
		smg_status = smg$set_broadcast_trapping(scope.smg_pbid, &(read_3broadcast), &(scope));
		smg_status = smg$repaint_screen(scope.smg_pbid);
		lib$set_symbol("CMC$HELP_LIBRARY", "");
		lib$set_symbol("CMC$HELP_KEY", "");
		BGosub(loadall);
	}
	else if (scope.scope_exit == smg$k_trm_f15)
	{
		if (hit == 0)
		{
			//
			// Display full message
			//
			BGosub(loadall);
			smg_status = smg$paste_virtual_display(svd, scope.smg_pbid, 1, 1);
			smg_status = smg$paste_virtual_display(scope.smg_option, scope.smg_pbid, 21, 1);
			hit = 1;
		}
		//
		// Other keys are Exit keys
		//
	}
	else
	{
		goto exitprogram;
	}
	goto menu;
exitprogram:;
	//
	// Delete all displays
	//
	smg_status = smg$pop_virtual_display(svd, scope.smg_pbid);
	smg_status = smg$pop_virtual_display(scope.smg_option, scope.smg_pbid);
	smg_status = smg$pop_virtual_display(scope.smg_message, scope.smg_pbid);
	// Restore OPTION  display
	scope.smg_option = old_option;
	// Restore MESSAGE display
	scope.smg_message = old_message;
restorescope:;
	scope.prg_ident = old_help_severity;
	scope.prg_program = old_program;
	scope.prg_item = old_help_item;
	return;
loadall:;
	//*******************************************************************
	// Load in the help text, and process it
	//*******************************************************************
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
					if (severity == "E")
					{
						st = libr_digsr(default_lib, "E$$CMCERR", line_num);
					}
					else
					{
						st = libr_digsr(default_lib, "H$$NODETAIL", line_num);
					}
					lib1_name = orig_lib;
				}
			}
		}
	}
	curr_line = std::stol(line_num[0]);
	smg_scroll.bot_array = smg_scroll.end_element = curr_line;
	smg_scroll.top_line = smg_scroll.beg_element;
	if (text == " ")
	{
		text = basic::edit(line_num[1], 1 + 4 + 8 + 16 + 128);
		text = text.substr(0, text.size() - 3);
	}
	v = dspl_scroll(smg_scroll, line_num, 0, "PAINT");
	BReturn;

}