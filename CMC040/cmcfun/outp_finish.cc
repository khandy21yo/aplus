//! \file
//! \brief Close Out Any Open Report
// %SBTTL "OUTP_FINISH"
// %IDENT "V3.6a Calico"
////
// Source: ../../CMC030/cmcfun/source/outp_finish.bas
// Translated from Basic to C++ using btran
// on Monday, January 08, 2018 at 12:38:19
//

#include <iostream>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "smg/lib.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "database.h"
#include "cmcfun/report.h"
#include "utl/utl_reportx.h"

extern scope_struct scope;

//!
//! Abstract:HELP
//!	.b
//!	.lm +5
//!	This subroutine finishes up any REPORT currently open
//!	.lm -5
//!
//! Parameters:
//!
//!	UTL_REPORTX
//!		The created file that closes any other reports.
//!
//!	Returned value
//!		It finishes up any report that may still
//!		be currently open.
//!
//! Example:
//!
//!	CALL OUTP_FINISH(UTL_REPORTX)
//!
//! AUTHOR:
//!
//!	02/23/87 - Kevin Handy
//!
void outp_finish(
	utl_reportx_cdd &utl_reportx)
{
	std::vector<std::string> junk;
	std::string key_name;
	std::string lib_name;
	std::string printfinish;
	std::string prog_name;
	long smg_status;
	long st;
	long temp;
	std::string tolocal;
	std::string toscreen;

	//
	// Set up variables we will probibly need
	//
	writ_string(utl_reportx.printfinish, printfinish);
	writ_string(utl_reportx.tolocal, tolocal);
	writ_string(utl_reportx.toscreen, toscreen);
	//
	// Finish off report
	//
	// ** Converted from a select statement **
	//
	// Display
	//
	if (utl_reportx.printto == OUTP_TODISPLAY)
	{
		utl_reportx.autoscroll = 0;
		if (utl_reportx.stat == 0)
		{
reloop:;
			outp_line("", utl_reportx, junk, "", -12345);
			utl_reportx.autoscroll = 0;
			// ** Converted from a select statement **
			//
			// An exit type of key
			//
			if ((scope.scope_exit == SMG$K_TRM_F10) ||
				(scope.scope_exit == SMG$K_TRM_CTRLZ) ||
				(scope.scope_exit == 3) ||
				(scope.scope_exit == SMG$K_TRM_F8) ||
				(scope.scope_exit == SMG$K_TRM_DO))
			{
				//
				// All other keys cause a reloop
				//
			}
			else
			{
				goto reloop;
			}
		}
		//
		// Erase display screen
		//
		smg_status = smg$delete_virtual_display(utl_reportx.window);
		//
		// Spool
		//
	}
	else if (utl_reportx.printto == OUTP_TOSPOOL)
	{
		utl_reportx.chan << printfinish << std::endl;
		utl_reportx.chan.close();
		outp_spool(utl_reportx);
		//
		// File
		//
	}
	else if (utl_reportx.printto == OUTP_TOFILE)
	{
		utl_reportx.chan << printfinish << std::endl;
		utl_reportx.chan.close();
		//
		// Word Processing, S2020, DIF
		//
	}
	else if ((utl_reportx.printto == OUTP_TOWP) ||
		(utl_reportx.printto == OUTP_TO2020) ||
		(utl_reportx.printto == OUTP_TOPL))
	{
		utl_reportx.chan.close();
		//
		// Device
		//
	}
	else if (utl_reportx.printto == OUTP_TODEVICE)
	{
		utl_reportx.chan << printfinish << std::endl;
		outp_formff(utl_reportx);
		utl_reportx.chan.close();
		//
		// Local printer
		//
	}
	else if (utl_reportx.printto == OUTP_TOLOCAL)
	{
		utl_reportx.chan << tolocal << printfinish;
		outp_formff(utl_reportx);
		utl_reportx.chan << toscreen;
		utl_reportx.chan.close();
		//
		// Documentation
		//
	}
	else if (utl_reportx.printto == OUTP_TODOCUMENT)
	{
		utl_reportx.chan.close();
		prog_name = boost::trim_right_copy(utl_reportx.pronam);
		//
		// Create key name
		//
		key_name = boost::trim_right_copy(prog_name) + "$" + "REPORT";
		//
		// Create library name
		//
		temp = ((prog_name + "_").find("_", 0) + 1);
		lib_name = std::string("SIC:WINDOWS_") +
			prog_name.substr(0, temp - 1);
		//
		// Plop it into library
		//
		st = libr_3insert(lib_name, utl_reportx.defout, key_name);
		//
		// Delete the file (hope for only one)
		//
		smg_status = lib$delete_file(boost::trim_right_copy(utl_reportx.defout) + ";*");
	}
	//
	// Erase display (option and message)
	//
	if (scope.smg_option.win)
	{
		smg_status = smg$erase_display(scope.smg_option);
	}
	if (scope.smg_message.win)
	{
		smg_status = smg$erase_display(scope.smg_message);
	}
	//
	// Change the width
	//
	smg_status = smg$change_pbd_characteristics(scope.smg_pbid, 80);
}
