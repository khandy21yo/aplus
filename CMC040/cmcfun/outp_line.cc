//! \file
//! \brief Handle Paging on Reports
// %SBTTL "OUTP_LINE"
// %IDENT "V3.6a Calico"

//
// Source: ../../CMC030/cmcfun/source/outp_line.bas
// Translated from Basic to C++ using btran
// on Wednesday, December 20, 2017 at 17:16:51
//

#include <iostream>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "smg/smg.h"

#include "preferences.h"
#include "cmcfun.h"
#include "database.h"
#include "scopedef.h"
#include "cmcfun/report.h"
#include "utl/utl_reportx.h"
#include "utl/utl_profile.h"
#include "scroll.h"

extern scope_struct scope;

//!
//! \brief Handle Paging on Reports
//!
//!	This subroutine handles paging for the report
//!	programs existing in all systems.
//!
//! Example:
//!
//!	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
//!
//!	TITLE$(1%) = "This is the report"
//!	TITLE$(2%) = "For the current period"
//!	TITLE$(3%) = ""
//!	TITLE$(4%) = "Key  Description          Date     Time  Flag"
//!	TITLE$(5%) = ""
//!
//!	LAYOUT$ = "$KEY:005,$DESCR:026,DDATE:035,TTIME:041,VFLAG:043"
//!
//!	LINE$ = KEY$ + " " +
//!		DESCRIPTION$ + " " +
//!		PRNT_DATE(DATE$, 0%) + " " +
//!		PRNT_TIME(TIME$, 2048%) + " " +
//!		FORMAT$(FLAG%, "#")
//!
//!	CALL OUTP_LINE(LAYOUT$, UTL_REPORTX, TITLES$(), LINE$, 0%)
//!
//! \author 03/23/87 - Kevin Handy
//!
void outp_line(
	const std::string &columns,
		//!< Passed specification for not only where the
		//!	columns are, but also labels for
		//!	these columns.  This string is used for shipping
		//!	the report to Graphics, a Spreadsheet, or a
		//!	Database.  Each column of data should have a
		//!	section in this string, in this form:
		//!
		//!	... [DataType][Label]:[EndingPos], ...
		//!
		//!	[DataType] is a single character describing
		//!		the type of data passed in that column:
		//!		($ = String, D = Date, P = Phone number,
		//!		T = Time, and V = Number value).
		//!	[Label] is a label for the data passed in that
		//!		column.
		//!	[EndingPos] is the horizontal position along
		//!		the line where this column ends.
	utl_reportx_cdd &utl_reportx,
		//!< This passed structure contains the definitions of
		//! various items for the report being printed.  These
		//! items are modified according to actions taken; line
		//! number, page number incremented, etc.
	const std::vector<std::string> &hdr,
		//!< Headers to be printed on the top of every page.
	const std::string &txt,
		//!< Passed text to print.
	long lineoff)
		//!< Variable used to control paging.  A positive value
		//! causes paging to occur that many lines earlier,
		//! and a negitive value causes it to occur that many
		//! lines later.  A special case of LINEOFF >= 1000
		//! exists that forces a new page, but the line is not
		//! counted/printed.
{
	long colonpos;
	std::string column;
	long commapos;
	std::string data;
	long endpos;
	long i;
	std::string junk;
	long junk_V2;
	long junk1;
	std::string junk3;
	std::string old_help_item;
	std::string old_help_severity;
	std::string old_program;
	long print_flag1;
	long print_flag2;
	long q_flag;
	long smg_status;
	long startpos;
	std::string temp;
	long temp_V4;
	std::string text;
	std::string this_col;
	std::string tolocal;
	std::string toscreen;
	long v;

	BStack(20);

	const long max_smg_text = 200;
	utl_profile_cdd utl_profile;
	smg_scroll_cdd smg_scroll;
	std::vector<std::string> smg_text;
	smg_text.resize(max_smg_text + 1);
	long rrr_flag;
	std::string title1;
	extern long outp_xunsol;

	//
	// ++
	//
	// Helper function for the offset business
	//

	auto fnoffset = [&](std::string source, long offset)
	{
		std::string Result;

		// ** Converted from a select statement **
		if (offset > 0)
		{
			Result = std::string(offset, ' ') + source;
		}
		else if (offset < 0)
		{
			Result = basic::right(source, -offset + 1);
		}
		else
		{
			Result = source;
		}
		return Result;
	};
	//
	// Save the key for help
	//
	old_help_severity = scope.prg_ident;
	old_program = scope.prg_program;
	old_help_item = scope.prg_item;
	scope.prg_program = "OUTP_LINE";
	scope.prg_item = "HELP";
	scope.prg_ident = "H";
	junk3 = "";
	//
	// Exit due to report request
	//
	if (utl_reportx.stat != 0)
	{
		goto exitsub;
	}
	if (boost::trim_right_copy(hdr[1]) != "")
	{
		title1 = boost::trim_right_copy(hdr[1]);
	}
	//
	// Special LINEOFF (-12345) for OUTP_FINISH when in display mode.
	// This is used so that when you hit the end of the report, you
	// can still arrow back up and down to your hearts content.
	// Q_FLAG% is used to change the flag used by DSPL_SCROLLCIR.
	//
	if ((lineoff == -12345) && (utl_reportx.printto == OUTP_TODISPLAY))
	{
		q_flag = 2;
		BGosub(L_2000);
		goto exitsub;
	}
	q_flag = 6;
	//
	// Output goes to printer if to printer port
	//
	if ((utl_reportx.printto == OUTP_TOLOCAL) && (utl_reportx.pageno >= utl_reportx.startp))
	{
		writ_string(utl_reportx.tolocal, tolocal);
		utl_reportx.chan << tolocal;
	}
	//
	// Force the title if this is the first time through,
	// or it is time to do a page.
	//
	if ((utl_reportx.pageno == 0) &&
		(utl_reportx.printto < 10) ||
		((utl_reportx.lineno + lineoff) >= (utl_reportx.pagelen - 7) &&
		(utl_reportx.printto != OUTP_TODISPLAY)) &&
		(utl_reportx.printto < 10) ||
		((utl_reportx.lineno) >= (utl_reportx.pagelen) &&
		(utl_reportx.printto == OUTP_TODISPLAY)))
	{
		BGosub(L_2000);
	}
	//
	// Count the line
	//
	if (lineoff < 1000 && utl_reportx.autoscroll == 0)
	{
		utl_reportx.lineno = utl_reportx.lineno + 1;
	}
	//
	// Will we be printing? (Check for start and end page, as
	// well as report request to exit)
	//
	if (!((utl_reportx.pageno >= utl_reportx.startp) && ((utl_reportx.pageno <= utl_reportx.endp) || (utl_reportx.endp == 0))))
	{
		goto L_1500;
	}
	if (utl_reportx.stat != 0)
	{
		goto L_1500;
	}
	//
	// If the lineoff<1000 then line will print otherwise line
	// will not print
	//
	if (lineoff < 1000)
	{
		//
		// Handle the various types of output
		//
		// ** Converted from a select statement **
		//
		// Display
		//
		if (utl_reportx.printto == OUTP_TODISPLAY)
		{
			smg_scroll.end_element = smg_scroll.end_element + 1;
			if (smg_scroll.end_element > smg_scroll.bot_array)
			{
				smg_scroll.end_element = smg_scroll.top_array;
			}
			if (smg_scroll.beg_element == smg_scroll.end_element)
			{
				smg_scroll.beg_element = smg_scroll.end_element + 1;
			}
			if (smg_scroll.beg_element > smg_scroll.bot_array)
			{
				smg_scroll.beg_element = smg_scroll.top_array;
			}
			smg_text[smg_scroll.end_element] = fnoffset(txt, utl_reportx.offset);
			smg_scroll.find_line = smg_scroll.end_element;
			junk_V2 = dspl_scrollcir(smg_scroll, smg_text, SMG$K_TRM_FIND, "");
			//
			// SuperComp 20/20, PlanPerfect
			//
		}
		else if ((utl_reportx.printto == OUTP_TO2020) || (utl_reportx.printto == OUTP_TOPL))
		{
			if (columns == "")
			{
				column = "$Line:255,";
			}
			else
			{
				column = columns + ",";
			}
			startpos = 0;
			print_flag2 = 0;
			temp = "";
			while (column != "")
			{
				commapos = (column.find(",", 0) + 1);
				this_col = column.substr(0, commapos - 1);
				column = basic::right(column, commapos + 1);
				colonpos = (this_col.find(":", 0) + 1);
				endpos = std::stol(basic::right(this_col, colonpos + 1));
				data = boost::trim_right_copy(basic::Qseg(txt, startpos, endpos));
				if (basic::edit(this_col.substr(0, 1), -1) == "V")
				{
					//
					// Clean up data a little
					//
					data = boost::trim_copy(data);
					if (basic::right(data, data.size()) == ")" && data.substr(0, 1) == "(")
					{
						data = std::string("-") + basic::Qseg(data, 2, data.size() - 1);
					}
					if (basic::right(data, data.size()) == "-")
					{
						data = std::string("-") + data.substr(0, data.size() - 1);
					}
					i = (data.find(",", 0) + 1);
					while (i)
					{
						data = data.substr(0, i - 1) + basic::right(data, i + 1);
						i = (data.find(",", 0) + 1);
					}
					i = (data.find("$", 0) + 1);
					if (i)
					{
						data = data.substr(0, i - 1) + basic::right(data, i + 1);
					}
				}
				if (utl_reportx.printto == OUTP_TOPL)
				{
					temp = temp + "\t" + data;
					if (data != "")
					{
						print_flag2 = -1;
					}
				}
				else
				{
					// ** Converted from a select statement **
					if (basic::edit(this_col.substr(0, 1), -1) == "V")
					{
						temp = temp + data + " #";
						if (data.find_first_of("123456789") + 1)
						{
							print_flag2 = -1;
						}
					}
					else
					{
						temp = temp + "\"" + data + "\" #";
						if (data != "")
						{
							print_flag2 = -1;
						}
					}
				}
				startpos = endpos + 1;
			}
			// ** Converted from a select statement **
			if (utl_reportx.printto == OUTP_TO2020)
			{
				temp = temp + "#";
			}
			else if (utl_reportx.printto == OUTP_TOPL)
			{
				temp = basic::right(temp, 2);
			}
			if (print_flag2)
			{
				utl_reportx.chan << temp << std::endl;
			}
			if (utl_reportx.lineno == 1)
			{
				entr_3message(scope, title1, 1 + 16);
			}
			//
			// Normal output
			//
		}
		else
		{
			utl_reportx.chan << fnoffset(txt, utl_reportx.offset) << std::endl;
		}
	}
	//
L_1500:;
	// Output goes to the screen if in printer port mode
	//
	if (utl_reportx.printto == OUTP_TOLOCAL)
	{
		writ_string(utl_reportx.toscreen, toscreen);
		utl_reportx.chan << toscreen;
	}
	if (junk3 != "")
	{
		//
		// Test to force a buffer flush
		//
		entr_3message(scope, junk3 + " of " + title1, 1 + 16);
		smg_status = smg$flush_buffer(scope.smg_pbid);
	}
	//
	// Flag to end program if we reach the end page number
	//
	if ((utl_reportx.pageno > utl_reportx.endp) && (utl_reportx.endp != 0))
	{
		utl_reportx.stat = -50;
	}
	//
	// Handle any special junk in RRR_FLAG%
	//
	// ** Converted from a select statement **
	//
	// Nothing
	//
	if (rrr_flag == 0)
	{
		// Nothing
		//
		// Autoscroll Toggle Key Typed
		//
	}
	else if (rrr_flag == 1)
	{
		if (utl_reportx.autoscroll == 0)
		{
			utl_reportx.autoscroll = -1;
		}
		else
		{
			utl_reportx.autoscroll = 0;
		}
		//
		// Exit keys
		//
	}
	else if ((rrr_flag == 3) || ((rrr_flag == SMG$K_TRM_F10) || ((rrr_flag == SMG$K_TRM_CTRLZ) || (rrr_flag == SMG$K_TRM_F8))))
	{
		utl_reportx.stat = rrr_flag;
		//
		// Else
		//
	}
	else
	{
		smg_status = entr_4specialkeys(scope, scope.smg_option, 256, rrr_flag);
	}
	rrr_flag = 0;
exitsub:;
	//
	// Restore the key for help
	//
	scope.prg_ident = old_help_severity;
	scope.prg_program = old_program;
	scope.prg_item = old_help_item;
	return;
	//*******************************************************************
L_2000:;
	// This subroutine does the actual paging
	//*******************************************************************
	//
	// Count one page
	//
	utl_reportx.pageno = utl_reportx.pageno + 1;
	//
	// Are we skipping this page? Check first for page range, but
	// force first title when printing to a clipboard, wp document,
	// or file cabinet.
	//
	print_flag1 = ((utl_reportx.pageno >= utl_reportx.startp) && ((utl_reportx.pageno <= utl_reportx.endp) || (utl_reportx.endp == 0)));
	//
	// On first page, enable unsolicited input trapping
	//
	if (utl_reportx.pageno == 1)
	{
		rrr_flag = 0;
	}
	//
	// While in display, title goes on line two and three
	//
	// ** Converted from a select statement **
	if (utl_reportx.printto == OUTP_TODISPLAY)
	{
		//
		// Pause
		//
		scope.scope_exit = 0;
		if ((utl_reportx.pageno != 1) && (utl_reportx.autoscroll == 0))
		{
pauseloop:;
			entr_3message(scope, title1, 0);
			// ** Converted from a select statement **
			//
			// Autoscroll Toggle Key Typed
			//
			if (scope.scope_exit == 1)
			{
				utl_reportx.autoscroll = -1;
				//
				// An exit key was typed
				//
			}
			else if ((scope.scope_exit == 3) || ((scope.scope_exit == SMG$K_TRM_CTRLZ) || ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_F8))))
			{
				utl_reportx.stat = scope.scope_exit;
				goto ret2000;
				//
				// Cursor positioning (up) key was typed
				//
			}
			else if ((scope.scope_exit == SMG$K_TRM_UP) || ((scope.scope_exit == SMG$K_TRM_F18) || (scope.scope_exit == SMG$K_TRM_PREV_SCREEN)))
			{
				smg_scroll.smg_flag = 2;
				temp_V4 = dspl_scrollcir(smg_scroll, smg_text, scope.scope_exit, "");
				goto pauseloop;
				//
				// Cursor positioning key (down) was typed
				//
			}
			else if ((scope.scope_exit == SMG$K_TRM_DOWN) || ((scope.scope_exit == SMG$K_TRM_F19) || (scope.scope_exit == SMG$K_TRM_NEXT_SCREEN)))
			{
				smg_scroll.smg_flag = q_flag;
				temp_V4 = dspl_scrollcir(smg_scroll, smg_text, scope.scope_exit, "");
				if (temp_V4 <= 0)
				{
					goto pauseloop;
				}
				utl_reportx.lineno = utl_reportx.lineno - temp_V4;
				//
				// Return, etc. act as next screen
				//
			}
			else if ((scope.scope_exit == 10) || ((scope.scope_exit == 12) || ((scope.scope_exit == 13) || ((scope.scope_exit == SMG$K_TRM_F7) || (scope.scope_exit == SMG$K_TRM_DO)))))
			{
				smg_scroll.smg_flag = q_flag;
				temp_V4 = dspl_scrollcir(smg_scroll, smg_text, SMG$K_TRM_NEXT_SCREEN, "");
				if (temp_V4 <= 0)
				{
					goto pauseloop;
				}
				utl_reportx.lineno = utl_reportx.lineno - temp_V4;
				//
				// Case else
				//
			}
			else
			{
				entr_3badkey(scope, scope.scope_exit);
				goto pauseloop;
			}
		}
		//
		// Search for header
		//
		for (junk_V2 = 1; !(hdr[junk_V2] == ""); junk_V2++)
		{
			v = 0;
		}
		for (junk1 = junk_V2 + 1; !(hdr[junk1] == ""); junk1++)
		{
			v = 0;
		}
		//
		// Clear the screen and print the title if necessary
		//
		if ((utl_reportx.pageno == 1) || (lineoff > 1000))
		{
			//
			// Display the header
			//
			for (i = junk_V2 + 1; i <= junk1 - 1; i++)
			{
				junk = hdr[i];
				if (junk == ".")
				{
					junk = "";
				}
				junk = fnoffset(junk, utl_reportx.offset);
				smg_status = smg$put_chars(utl_reportx.window, junk + std::string(utl_reportx.repwidth - junk.size(), ' '), (i - junk_V2), 1, 1, SMG$M_REVERSE);
			}
			//
			// Set up scrolling region
			//
//			smg_status = smg$set_display_scroll_region(utl_reportx.window, junk1 - junk_V2, 20);
			//
			// Create SMG_SCROLL array
			//
			smg_scroll.window = utl_reportx.window;
			smg_scroll.scroll_top = junk1 - junk_V2;
			smg_scroll.scroll_bot = 20;
			smg_scroll.top_array = 1;
			smg_scroll.bot_array = max_smg_text;
			smg_scroll.cur_line = smg_scroll.cur_w_col = 1;
			smg_scroll.find_line = smg_scroll.top_line = smg_scroll.cur_w_row = smg_scroll.beg_element = 1;
			smg_scroll.end_element = 1;
			smg_text[1] = "";
			smg_scroll.smg_flag = 6;
			smg_scroll.prompt = "";
			smg_scroll.num_colm = 1;
			//
			// Calculate number of lines available on the screen
			//
			utl_reportx.lineno = junk1 - junk_V2;
		}
	}
	else
	{
		//
		// Skip to the top of the next page
		//
		if (utl_reportx.pageno == 1)
		{
			utl_reportx.sdate = basic::Qdate(0);
			utl_reportx.stime = basic::Qtime(0);
		}
		//
		// Print the page title
		//
		junk3 = std::string("Page ") + std::to_string(utl_reportx.pageno);
		//
		// Switch to screen mode
		//
		//		SMG_STATUS% = SMG$FLUSH_BUFFER(SCOPE::SMG_PBID)
		//		IF UTL_REPORTX::PRINTTO = OUTP_TOLOCAL
		//		THEN
		//			CALL WRIT_STRING(UTL_REPORTX::TOSCREEN, TOSCREEN$)
		//			PRINT #UTL_REPORTX::CHAN, TOSCREEN$;
		//		END IF
		//
		//		CALL ENTR_3MESSAGE(SCOPE, JUNK3$ + " of " + TITLE1, 1% + 16%)
		utl_reportx.stat = scope.macroflag;
		//
		//		SMG_STATUS% = SMG$FLUSH_BUFFER(SCOPE::SMG_PBID)
		//		IF UTL_REPORTX::PRINTTO = OUTP_TOLOCAL
		//		THEN
		//			CALL WRIT_STRING(UTL_REPORTX::TOLOCAL, TOLOCAL$)
		//			PRINT #UTL_REPORTX::CHAN, TOLOCAL$;
		//		END IF
		//
		// Handle title while skipping pages (Use special loop
		// because it looks better than 1000 if statements in
		// the printing section that follows).
		//
		// Also, handle types 7, and 8 (Word Processing,
		// File cabinet), after the first header, so that the user
		// can actuall use start/end page.
		//
		if ((print_flag1 == 0) || ((utl_reportx.pageno != 1) && (utl_reportx.printto == OUTP_TOWP)))
		{
			//
			// Print page anyway if need a title for wp.
			//
			if ((utl_reportx.pageno == 1) && (utl_reportx.printto == OUTP_TOWP))
			{
				goto L_2050;
			}
			//
			// Special GOSUB 3000's in title
			//
			utl_reportx.lineno = 5;
			if (utl_reportx.repyn != "N")
			{
				utl_reportx.lineno = utl_reportx.lineno + 1;
			}
			//
			// Title
			//
			for (junk1 = 1; !(hdr[junk1] == ""); junk1++)
			{
				utl_reportx.lineno = utl_reportx.lineno + 1;
			}
			//
			// Header
			//
			for (junk1 = junk1 + 1; !(hdr[junk1] == ""); junk1++)
			{
				utl_reportx.lineno = utl_reportx.lineno + 1;
			}
			goto L_2100;
		}
		//
L_2050:;
		// Output the title
		//
		if (utl_reportx.pageno != 1)
		{
			outp_formff(utl_reportx);
			// (Hack)
			utl_reportx.pageno = utl_reportx.pageno - 1;
		}
		// Minimum number of lines in header
		utl_reportx.lineno = 4;
		// Modified to adjust for excesses in
		// paging section, and room for blank
		// lines at bottom of page.
		utl_reportx.chan << std::endl;
		utl_reportx.chan << std::endl;
		junk_V2 = 0;
		junk = boost::trim_right_copy(utl_profile.rep_name);
		BGosub(L_3000);
		for (junk1 = 1; !(hdr[junk1] == ""); junk1++)
		{
			junk = boost::trim_right_copy(hdr[junk1]);
			BGosub(L_3000);
		}
		if (utl_reportx.repyn != "N")
		{
			junk = boost::trim_right_copy(utl_reportx.repdate);
			BGosub(L_3000);
		}
		utl_reportx.chan << std::endl;
		utl_reportx.chan << fnoffset(basic::Qstring(utl_reportx.repwidth, '='), utl_reportx.offset) << std::endl;
		for (junk1 = junk1 + 1; !(hdr[junk1] == ""); junk1++)
		{
			junk = boost::trim_right_copy(hdr[junk1]);
			if (junk == ".")
			{
				junk = "";
			}
			utl_reportx.chan << fnoffset(junk, utl_reportx.offset) << std::endl;
			utl_reportx.lineno = utl_reportx.lineno + 1;
		}
	}
L_2100:;
	BReturn;

	//
L_3000:;
	// Print the heading stuff
	//
	junk_V2 = junk_V2 + 1;
	if (junk == ".")
	{
		junk = "";
	}
	utl_reportx.lineno = utl_reportx.lineno + 1;
	switch (junk_V2)
	{
	case 1:

		text = std::string("Date: ") + basic::Qdate(0);
		text = text + std::string(utl_reportx.repwidth / 2 - text.size() - junk.size() / 2, ' ') + junk;
		text = text + std::string(utl_reportx.repwidth - text.size() - junk3.size(), ' ') + junk3;
		utl_reportx.chan << fnoffset(text, utl_reportx.offset) << std::endl;
		break;

	case 2:

		text = std::string("Time: ") + basic::Qtime(0);
		text = text + std::string(utl_reportx.repwidth / 2 - text.size() - junk.size() / 2, ' ') + junk;
		text = text + std::string(utl_reportx.repwidth - text.size() - strlen("V3.6"), ' ') + "V3.6";
		utl_reportx.chan << fnoffset(text, utl_reportx.offset) << std::endl;
		break;

	default:
		text = "";
		text = text + std::string(utl_reportx.repwidth / 2 - text.size() - junk.size() / 2, ' ') + junk;
		utl_reportx.chan << fnoffset(text, utl_reportx.offset) << std::endl;
		break;

	}
ret2000:;
	BReturn;

}
// +-+-+
// ++
// Success:DISPLAY
//	^*Display Report\*
//	.b
//	.lm +5
//	Scrolling the report to the next screen, previous screen,
//	line by line or exit.
//	.lm -5
//
// Index:
//
// --
