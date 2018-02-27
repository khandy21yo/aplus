//! \file
//! \\brief Initilize Forms Output Information
// %SBTTL "OUTP_INITFORM"
// %IDENT "V3.6a Calico"

//
// Source: ../../CMC030/cmcfun/source/outp_initform.bas
// Translated from Basic to C++ using btran
// on Wednesday, December 20, 2017 at 16:47:01
//

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <string>

#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "database.h"

#include "utl/utl_reportx.h"
#include "utl/utl_report.h"
#include "utl/utl_report_sys.h"
#include "cmcfun/report.h"
#include "smg/lib.h"
#include "smg/smg.h"

//
// Globals
//
extern scope_struct scope;

//!
//! \\brief Initilize Forms Output Information
//!
//!	This function initilizes the REPORT output functions
//!	for most forms.
//!
//!	This function was written to remove redundancies in the
//!	forms, and makes fixing some bugs in forms much easier.
//!
//! \returns	Initilizes the report output functions and other
//!		information the file has.
//!
//! \author 06/08/92 - Kevin Handy
//!
long outp_initform(
	utl_reportx_cdd &utl_reportx,
		//!< The file used to initilize the report functions.
	const std::string &reportnum,
		//!< The report number to look up
	const std::string &fixset)
		//!< String used to initialize UTL_REPORTX fields from
		//!< 0 to 9.
{
	long Result;
	long directflag;
	std::string filename;
	long flag;
	long i;
	std::string jj;
	std::ofstream prnt_ch;
	std::string setflg;
	long smg_status;
	long stat;
	long sys_status;
	std::string temp;
	long temp_V2;
	std::string tempfile;
	std::string temp_ident;
	std::string temp_program;
	long user_report;
	long xloop;
	std::string left_cmd;
	std::string right_cmd;

	utl_report_cdd utl_report;
	utl_report_sys_cdd utl_report_sys;
	printx_cdd printx;
	smg_display_id smg_blank;

	//******************************************************************
	// Delete temp file to prepare for exit
	//******************************************************************
	auto killtempfile = [&]()
	{
		prnt_ch.close();
		unlink(tempfile.c_str());
	};

	jj = read_sysjob();

	//
	// Get Report if it exists
	//
	user_report = 0;
	try
	{
		utl_report.Get(reportnum);
	}
	catch(basic::BasicError &Be)
	{
		goto L_350;
	}
	user_report = -1;
	//
	// Get System version of Report
	//
L_350:;
	try
	{
		utl_report_sys.Get(reportnum);
	}
	catch(basic::BasicError &Be)
	{
		filename = "utl_report_sys";
		goto helperror;
	}
	//
	// Put or update system version of report into users version
	//
	if (user_report)
	{
		for (i = 0; i <= 9; i++)
		{
			utl_report.optdef[i] = utl_report_sys.optdef[i];
			utl_report.itemgroup[i] = utl_report_sys.itemgroup[i];
			utl_report.item[i] = utl_report_sys.item[i];
		}
		utl_report.defout = utl_report_sys.defout;
		utl_report.printtype = utl_report_sys.printtype;
		utl_report.spoolform = utl_report_sys.spoolform;
		utl_report.lastrundate = utl_report_sys.lastrundate;
		utl_report.lastruntime = utl_report_sys.lastruntime;
		utl_report.baserundate = utl_report_sys.baserundate;
		try
		{
			utl_report.Update();
		}
		catch(basic::BasicError &Be)
		{
			filename = "UTL_REPORT";
			goto helperror;
		}
	}
	else
	{
		utl_report = utl_report_sys;
		try
		{
			utl_report.Put();
		}
		catch(basic::BasicError &Be)
		{
			filename = "UTL_REPORT";
			goto helperror;
		}
	}
	//******************************************************************
	// Set up the report settings screen
	//******************************************************************
	//
	// store original values for the help message
	//
	temp_ident = scope.prg_ident;
	temp_program = scope.prg_program;
	xloop = 0;
	//******************************************************************
L_510:;
	// Set up the report settings screen
	//******************************************************************
	xloop = xloop + 1;
	tempfile = std::string("/tmp/prnt") + jj + "_" +
		std::to_string(xloop) + ".tmp";
	if (find_fileexists(tempfile, flag))
	{
		goto L_510;
	}
	prnt_ch.close();
	try
	{
		prnt_ch.open(tempfile.c_str());
		if (!prnt_ch.is_open()) { throw basic::BasicError(5); }
	}
	catch(basic::BasicError &Be)
	{
		filename = "TEMP";
		goto helperror;
	}
	sys_status = lib$set_symbol("CMC$REPORT", tempfile);
	if ((sys_status & 1) == 0)
	{
		entr_3message(scope, std::string("Unable to declare symbol for work file. ") + std::to_string(sys_status), 0);
		goto exitprogram;
	}
	//
	// Get the report record
	//
	try
	{
		utl_report.Get(reportnum);
	}
	catch(basic::BasicError &Be)
	{
		if (Be.err == 154)
		{
			goto L_525;
		}
		filename = "UTL_REPORT";
		goto helperror;
	}
	goto L_530;
	//
L_525:;
	// Get the report record
	//
	utl_report.Get(reportnum);
	goto L_530;
	//
L_530:;
	// Because this is a form we must set the program name
	// in to the program field in the utl_report record
	//
	utl_report.pronam = scope.prg_program;
	//
	// Set up for the window
	//
	smg_status = smg$erase_display(scope.smg_message);
	utl_reportx.window = 0;
	//
	// Initilize defaults from report file
	//
	outp_initstructure(utl_report, utl_reportx);
	temp = fixset;
	//
	// Load up any special values to shove into fields
	//
	setflg = "";
	directflag = 0;
	while (temp != "")
	{
		i = (temp.find(",", 0) + 1);
		if (i == 0)
		{
			i = temp.size() + 1;
		}
		if (temp.substr(0, i - 1) == "DIRECT")
		{
			directflag = -1;
		}
		else
		{
			if ((std::string("01").find(temp.substr(0, 1), 0) + 1))
			{
				temp_V2 = std::stol(temp.substr(0, 2));
				utl_reportx.optdef[temp_V2] = basic::Qseg(temp, 3, i - 1);
			}
			else
			{
				setflg = setflg + temp.substr(0, 2) + " ";
			}
		}
		temp = basic::right(temp, i + 1);
	}
	if (directflag)
	{
		utl_reportx.spool = "DIRECT";
	}
	//
	// Ask user to change settings
	//
	left_cmd = std::string("DD SF SP EP CP AF OF AS ") + setflg;
	right_cmd = "PT ";
	outp_settings(utl_report, utl_reportx,
		left_cmd, right_cmd);
	//
	// Un-normal abort, exit, etc.
	//
	if ((scope.scope_exit == SMG$K_TRM_F10) ||
		(scope.scope_exit == SMG$K_TRM_CTRLC) ||
		(scope.scope_exit == SMG$K_TRM_F8))
	{
		if (directflag == 0)
		{
			smg_status = smg$delete_virtual_display(utl_reportx.window);
		}
		killtempfile();
		goto exitprogram;
	}
	//
	// Erase option and message window
	//
	smg_status = smg$erase_display(scope.smg_option);
	smg_status = smg$erase_display(scope.smg_message);
	if (directflag == 0)
	{
		//
		// Create a blank display window
		//
		smg_status = smg$create_virtual_display(20, 132, smg_blank);
		//
		// Paste on blank display to hide the width change
		//
		smg_status = smg$paste_virtual_display(smg_blank, scope.smg_pbid, 1, 1);
		//
		// Delete report window
		//
		smg_status = smg$delete_virtual_display(utl_reportx.window);
	}
	//
	// Write the data out to the ascii file
	//
	outp_3writestructure(utl_reportx, prnt_ch, printx);
	prnt_ch.close();
	//
	// Initilize for output
	//
	outp_initfromfile(utl_reportx, 132);
	if (utl_reportx.stat)
	{
		goto exitprogram;
	}
	if (directflag == 0)
	{
		smg_status = smg$delete_virtual_display(smg_blank);
	}
	//
	// Restore original values for the help message
	//
	scope.prg_ident = temp_ident;
	scope.prg_program = temp_program;
	killtempfile();
	Result = CMC$_NORMAL;
	return Result;
exitprogram:;
	//******************************************************************
	// Exit the program
	//******************************************************************
	killtempfile();
	//
	// Erase Display
	//
	smg_status = smg$erase_display(scope.smg_option);
	smg_status = smg$erase_display(scope.smg_message);
	//
	// Change the width
	//
	if (directflag == 0)
	{
		smg_status = smg$change_pbd_characteristics(scope.smg_pbid, 80);
	}
	Result = CMC$_ABORT;
	return Result;

helperror:;
	//
	// This moved from inside error to outside so that errors occuring
	// at lower levels could be trapped.  Basic will not allow any
	// error to occur inside of an error no matter if it is in a
	// different module.
	//
	help_34message(scope, std::to_string(0) + " " + "Error", "E", 0, filename, std::to_string(0));
	goto exitprogram;
}
