//! \file
//! \brief Initilize REPORT Output Information
// %SBTTL "OUTP_36INITFROMFILE"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/outp_36initfromfile.bas
// Translated from Basic to C++ using btran
// on Thursday, January 04, 2018 at 12:53:35
//

#include <iostream>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "database.h"
#include "smg/smg.h"
#include "scopedef.h"
#include "report.h"
#include "utl/utl_reportx.h"


//!
//! \brief Initilize REPORT Output Information
//!
//! Abstract:HELP
//!	.p
//!	This function initilizes the REPORT output functions.
//!
//! Parameters:
//!
//!	UTL_REPORTX
//!		The file used to initilize the report functions.
//!
//!	XWIDTH
//!		The returned variable used for the report output.
//!
//!	Returned value
//!		Initilizes the report output functions and other
//!		information the file has.
//!
//! Author:
//!
//!	03/23/87 - Kevin Handy
//!
void outp_36initfromfile(
	scope_struct &scope,
	utl_reportx_cdd &utl_reportx,
	int xwidth)
{
	std::string inline_V1;
	std::string leftt;
	std::string printinit;
	std::string rightt;
	long rn_flag;
	long smg_status;
	long stat;
	long sys_status_V2;
	long temp1_long;
	std::string tempfile;
	long temp_long;
	std::string tolocal;
	std::string toscreen;

	OnErrorStack;
	long sys_status;
	long templong;

	OnErrorGoto(L_19000);
	//******************************************************************
	// Initilization
	//******************************************************************
	//
	// Assume no errors
	//
	utl_reportx.stat = 0;
	//
	// Set up paging information
	//
	// Start without a page number
	utl_reportx.pageno = 0;
	// Not on a line
	//
	// Open up report file and read information for this report
	//
	sys_status = lib$get_symbol("CMC$REPORT", tempfile, 0, 0);
	if ((sys_status & 1) == 0)
	{
		entr_3message(scope, "Unable to find WORK file name!", 0);
		utl_reportx.stat = sys_status;
		return;
	}
	tempfile = boost::trim_right_copy(tempfile);
	//
	// Allocate a channel for report
	//
	utl_reportx.chan.ForInput();
	utl_reportx.chan.open(tempfile);
	if (!BasicChannel[utl_reportx.chan].is_open()) { throw basic::BasicError(5); }
	//
	// Assume no errors
	//
	utl_reportx.stat = 0;
	rn_flag = 0;
	//
	// Set up some defaults
	//
	utl_reportx.autoscroll = 0;
	utl_reportx.optdef[0] = "";
	utl_reportx.printinit = "";
	utl_reportx.pronam = "";
	utl_reportx.offset = 0;
	//
L_700:;
	// Read in one line of source file
	//
	try
	{
		getline(BasicChannel[(long)(utl_reportx.chan)], inline_V1);
		if (BasicChannel[(long)(utl_reportx.chan)].eof()) { throw basic::BasicError(11); }	// End of file on device
		if (BasicChannel[(long)(utl_reportx.chan)].fail()) { throw basic::BasicError(12); }	// Fatal system I/O failure
	}
	catch(basic::BasicError &Be)
	{
		if (Be.err == 11)
		{
			inline_V1 = "";
			goto L_750;
		}
		goto crash;
	}
	leftt = inline_V1.substr(0, 2);
	rightt = basic::right(inline_V1, 4);
	//
	// Process one line of input
	//
	// ** Converted from a select statement **
	//
	// PG - Program name
	//
	if (leftt == "PG")
	{
		utl_reportx.pronam = rightt;
		utl_reportx.prodev = "";
		//
		// LP - Lines/page
		//
	}
	else if (leftt == "LP")
	{
		utl_reportx.pagelen = std::stol(rightt);
		//
		// SP - Start page
		//
	}
	else if (leftt == "SP")
	{
		utl_reportx.startp = std::stol(rightt);
		//
		// EP - End page
		//
	}
	else if (leftt == "EP")
	{
		utl_reportx.endp = std::stol(rightt);
		//
		// CP - Copies
		//
	}
	else if (leftt == "CP")
	{
		utl_reportx.copies = std::stol(rightt);
		//
		// AF - After
		//
	}
	else if (leftt == "AF")
	{
		utl_reportx.aftertime = rightt;
		//
		// BG - Background
		//
	}
	else if (leftt == "BG")
	{
		utl_reportx.background = rightt;
		//
		// OF - Offset
		//
	}
	else if (leftt == "OF")
	{
		utl_reportx.offset = std::stol(rightt);
		//
		// RD - Report date
		//
	}
	else if (leftt == "RD")
	{
		utl_reportx.repdate = rightt;
		//
		// PD - Print report date on report
		//
	}
	else if (leftt == "PD")
	{
		utl_reportx.repyn = rightt;
		//
		// AS - Auto Scroll
		//
	}
	else if (leftt == "AS")
	{
		utl_reportx.autoscroll = std::stol(rightt);
		//
		// SP - Spooler name
		//
	}
	else if (leftt == "SL")
	{
		utl_reportx.spool = rightt;
		//
		// SF - Spooler Form name
		//
	}
	else if (leftt == "SF")
	{
		utl_reportx.spoolform = rightt;
		//
		// OD - Output device
		//
	}
	else if (leftt == "OD")
	{
		rn_flag = -1;
		utl_reportx.defout = rightt;
		//
		// XX - PRINTTO definition
		//
	}
	else if (leftt == "XX")
	{
		utl_reportx.printto = std::stol(rightt);
		//
		// TL - TOLOCAL Output to local printer
		//
	}
	else if (leftt == "TL")
	{
		utl_reportx.tolocal = rightt;
		//
		// TS - TOSCREEN return control to screen
		//
	}
	else if (leftt == "TS")
	{
		utl_reportx.toscreen = rightt;
		//
		// Un - User entries
		//
	}
	else if (leftt == "U1")
	{
		rn_flag = -1;
		utl_reportx.optdef[0] = rightt;
	}
	else if (leftt == "U2")
	{
		utl_reportx.optdef[1] = rightt;
	}
	else if (leftt == "U3")
	{
		utl_reportx.optdef[2] = rightt;
	}
	else if (leftt == "U4")
	{
		utl_reportx.optdef[3] = rightt;
	}
	else if (leftt == "U5")
	{
		utl_reportx.optdef[4] = rightt;
	}
	else if (leftt == "U6")
	{
		utl_reportx.optdef[5] = rightt;
	}
	else if (leftt == "U7")
	{
		utl_reportx.optdef[6] = rightt;
	}
	else if (leftt == "U8")
	{
		utl_reportx.optdef[7] = rightt;
	}
	else if (leftt == "U9")
	{
		utl_reportx.optdef[8] = rightt;
	}
	else if (leftt == "U0")
	{
		utl_reportx.optdef[9] = rightt;
		//
		// RN - Next report
		//
	}
	else if (leftt == "RN")
	{
		if (rn_flag)
		{
			goto L_1100;
		}
		utl_reportx.repnum = rightt;
		//
		// PC - Printer control string
		//
	}
	else if (leftt == "PC")
	{
		utl_reportx.printinit = rightt;
		//
		// PT - Printer Type
		//
	}
	else if (leftt == "PT")
	{
		utl_reportx.printtype = rightt;
		//
		// ZZ - Printer control string
		//
	}
	else if (leftt == "ZZ")
	{
		utl_reportx.printfinish = rightt;
		//
		// NP - Next page control string
		//
	}
	else if (leftt == "NP")
	{
		utl_reportx.nextpage = rightt;
	}
	goto L_700;
	//
L_750:;
	// Finish up input file.
	//
	//
	// Set up paging information
	//
	// Start without a page number
	utl_reportx.pageno = 0;
	// Not on a line
	//
	// Initilize the width variable
	//
	if (utl_reportx.repwidth == 0)
	{
		utl_reportx.repwidth = 132;
	}
	if (xwidth != 0)
	{
		utl_reportx.repwidth = xwidth;
	}
	if (utl_reportx.pagelen == 0)
	{
		utl_reportx.pagelen = 66;
	}
	//
	// Keyboard open needed
	//
	if (utl_reportx.printto == OUTP_TODISPLAY)
	{
		scope.screen_width = utl_reportx.repwidth;
		if (scope.screen_width == 0)
		{
			scope.screen_width = 80;
		}
		smg_status = smg$change_pbd_characteristics(scope.smg_pbid, scope.screen_width);
	}
	entr_3message(scope, "", 1 + 16);
	//******************************************************************
	// Remove group from print file
	//******************************************************************
L_1100:;
	utl_reportx.nextrun = "";
	BasicChannel[utl_reportx.chan].close();
	//
	// Delete the old file
	//
	//	KILL TEMPFILE$
	smg_status = lib$delete_file(tempfile);
	if ((smg_status & 1) == 0)
	{
		entr_3message(scope, std::string("Error deleting temp print file (") + std::to_string(smg_status) + ")", 4);
	}
	//******************************************************************
	// Prepare for output of information
	//******************************************************************
	writ_string(utl_reportx.printinit, printinit);
	writ_string(utl_reportx.tolocal, tolocal);
	writ_string(utl_reportx.toscreen, toscreen);
	//
	// Initilize the flags
	//
	// ** Converted from a select statement **
	//
	// Display
	//
	if (utl_reportx.printto == OUTP_TODISPLAY)
	{
		//
		// No start/end pages
		//
		utl_reportx.startp = utl_reportx.endp = 0;
		//
		// Length of report is 18 lines
		//
		utl_reportx.pagelen = 20;
		//
		// Create display for this report
		//
		smg_status = smg$create_virtual_display(utl_reportx.pagelen, utl_reportx.repwidth * 1, utl_reportx.window);
		//
		// Paste on the virtual display
		//
		smg_status = smg$paste_virtual_display(utl_reportx.window, scope.smg_pbid, 1, 1);
		//
		// To terminal
		//
	}
	else if (utl_reportx.printto == OUTP_TODEVICE)
	{
		//
		// FIXME: Why the double open, first with RECORDTYPE NONE, then
		// without. I'm reverting this nonsense for now to see if it becomes
		// obvious.
		//
		// Apparently it is needed to keep from getting an extra line feed
		// before the initilization string is sent out, which causes many
		// printers to advance the page one line every time a report starts.
		// Yuck.
		//
		//		OPEN UTL_REPORTX::DEFOUT AS FILE UTL_REPORTX::CHAN,
		//			ORGANIZATION SEQUENTIAL,
		//			ACCESS APPEND,
		//			RECORDSIZE 511%
		BasicChannel[utl_reportx.chan].SetOrginization(SEQUENTIAL);
		BasicChannel[utl_reportx.chan].SetAccess(APPEND);
		BasicChannel[utl_reportx.chan].SetRecordSize(511);
		BasicChannel[utl_reportx.chan].SetRecordType(NONE);
		BasicChannel[utl_reportx.chan].open(utl_reportx.defout);
		if (!BasicChannel[utl_reportx.chan].is_open()) { throw basic::BasicError(5); }
		BasicChannel[(long)(utl_reportx.chan)] << printinit;
		BasicChannel[utl_reportx.chan].close();
		BasicChannel[utl_reportx.chan].SetOrginization(SEQUENTIAL);
		BasicChannel[utl_reportx.chan].SetAccess(APPEND);
		BasicChannel[utl_reportx.chan].SetRecordSize(511);
		BasicChannel[utl_reportx.chan].open(utl_reportx.defout);
		if (!BasicChannel[utl_reportx.chan].is_open()) { throw basic::BasicError(5); }
		//
		// Local printer
		//
	}
	else if (utl_reportx.printto == OUTP_TOLOCAL)
	{
		//
		// If we are going to the terminal, use the default terminal
		// channel to reduce buffering problems between SMG and
		// PRINT statements.
		//
		if (utl_reportx.defout == "TT:")
		{
			utl_reportx.chan = 0;
		}
		else
		{
			//
			// Open keyboard for output
			//
			BasicChannel[utl_reportx.chan].SetAccess(APPEND);
			BasicChannel[utl_reportx.chan].SetRecordSize(511);
			BasicChannel[utl_reportx.chan].open(utl_reportx.defout);
			if (!BasicChannel[utl_reportx.chan].is_open()) { throw basic::BasicError(5); }
		}
		BasicChannel[(long)(utl_reportx.chan)] << tolocal << printinit << toscreen;
		//
		// Else a file
		//
	}
	else if ((utl_reportx.printto == OUTP_TOWP) ||
		(utl_reportx.printto == OUTP_TODOCUMENT) ||
		(utl_reportx.printto == OUTP_TO2020) ||
		(utl_reportx.printto == OUTP_TOPL))
	{
		BasicChannel[utl_reportx.chan].SetOrginization(SEQUENTIAL);
		BasicChannel[utl_reportx.chan].SetAccess(APPEND);
		BasicChannel[utl_reportx.chan].SetRecordSize(511);
		BasicChannel[utl_reportx.chan].SetAllow(READ);
		BasicChannel[utl_reportx.chan].open(utl_reportx.defout);
		if (!BasicChannel[utl_reportx.chan].is_open()) { throw basic::BasicError(5); }
		//
		// Else a file
		//
	}
	else
	{
		BasicChannel[utl_reportx.chan].SetOrginization(SEQUENTIAL);
		BasicChannel[utl_reportx.chan].SetAccess(APPEND);
		BasicChannel[utl_reportx.chan].SetRecordSize(511);
		BasicChannel[utl_reportx.chan].SetAllow(READ);
		BasicChannel[utl_reportx.chan].open(utl_reportx.defout);
		if (!BasicChannel[utl_reportx.chan].is_open()) { throw basic::BasicError(5); }
		BasicChannel[(long)(utl_reportx.chan)] << printinit;
	}
	return;
	//
crash:;
	// Exit from function with an error
	//
	utl_reportx.stat = Be.err;
	entr_3message(scope, std::string("ERROR: OUTP_36INITFROMFILE (") + std::to_string(Be.err) + ") " + basic::ert(Be.err) + " at line " + std::to_string(Be.erl) + " in " + Be.ern, 4);
	entr_3message(scope, std::string("ERROR: OUTP_36INITFROMFILE (") + inline_V1 + ") ", 4);
	return;
	//*******************************************************************
L_19000:;
	// Trap errors
	//
	//
	// Untrapped error
	//
	OnErrorZero;
	goto crash;
}
