//! \file
//! \brief Report Settings
// %SBTTL "OUTP_SETTINGS"
// %IDENT "V3.6a Calico"

//
// Source: ../../CMC030/cmcfun/source/outp_settings.bas
// Translated from Basic to C++ using btran
// on Thursday, January 04, 2018 at 10:11:35
//

#include <string>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"
#include "datalist.h"
#include "pusing.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "cmcfun/report.h"


extern scope_struct scope;

//
// Data Statement
//
static const char* DataValue[] = {
	"1","1","(DD) Destination",
	"2","1","(SF) Spooler Form",
	"3","6","Paper Width        Inches",
	"4","6","Paper Length       Inches",
	"5","1","(SP) Start Page",
	"6","1","(EP) End Page",
	"7","1","(CP) Copies",
	"8","1","(AS) Autoscroll",
	"9","1","(AF) After Time",
	"10","1","(BG) Background",
	"11","1","(OF) Offset",
	"12","1","(RD) Report Date",
	"1","41","(PT) Printer Type",
	"0","0",
	"","","Display","Spooler",
	"","","Printer Port",
	"","Word Processing",
	"","Documentation",
	"2020","Plan Perfect",
	"1","DP","1","DISPLAY","2","SPOOL","2","SPOOLER","2","SP","5","PP",
	"5","PRINTER PORT","7","WP","7","WORD PROCESSING","9","DOCUMENT","9",
	"DOCUMENTATION","10","2020","10","S2020","11","PL","0","", NULL};
static basic::DataListClass DataList(DataValue);

//!
//! \brief Report Settings
//!
//!	The ^*Report Settings\* enables the user to setup input for reports, posting
//!	processes, and forms.
//!
//! Option:
//!	OUTP_SETTINGS$FLD*
//!
//! Parameters:
//!
//! \returns This subroutine changes the report settings to fit the user's
//!	choice.
//!
//! \author 05/05/87 - Kevin Handy
//!
//! Removed the utl_report parameterm as it became a duplicate of the
//! utl_reopirt parameter.
//
void outp_settings(
	utl_report_cdd &utl_report,
		//!< The revised report settings.
	utl_reportx_cdd &utl_reportx,
		//!< The definition of the file to be changed.
	std::string &left_side_cmd,
		//!<	The passed left side commands.
		//!<
		//!<	Left side commands can be:
		//!<	.table
		//!<		DD - Document Destination
		//!<		SF - Spooler Form Name
		//!<		SP - Start Page
		//!<		EP - End Page
		//!<		CP - Copies
		//!<		AS - Autoscroll
		//!<		RD - Report Date
		//!<	.endtable
	std::string &right_side_cmd)
		//!> The passed right side commands.
		//!>
		//!>	Right side commands can be:
		//!>		PT - Printer Type
{
#if 0
	abort();
//! \todo actually get this converted properly
#else
	long cbflag;
	long comma;
	long end_loop;
	std::string filename;
	long ftype;
	long i;
	long i9;
	std::string inp;
	std::string itm;
	long iz;
	long j;
	long k;
	long loop;
	double m;
	long m_V1;
	long ocount;
	std::string opt;
	long opt3;
	long opt7;
	std::string option;
	std::string otype;
	long otype_V3;
	std::string output;
	long pdn;
	long print_to;
	long print_len;
	long q_i;
	std::string report_keep_ident;
	std::string report_keep_item;
	std::string report_keep_prg;
	std::string reqitm;
	long skip;
	long smg_status;
	long st;
	long start_loop;
	long stat;
	std::string temp;
	long temp_V5;
	std::string temp1;
	std::string temp_printtype;
	std::string tmep;
	long tt;
	std::string v;
	long v_V6;
	std::string wild_file;
	long work;
	std::string work_cmd;
	long x;
	long xpos_V7;
	std::string xstr;
	long ypos_V8;

	BStack(20);
	printx_cdd printx;
	long devchar;
	long xpos;
	long ypos;
	long sys_status;
	long print_line[print_maxgrp + 1];
	std::vector<std::string> printer_printer;
	std::string xtype[21];
	long otype_V4[31];
	std::string otype_V2[31];
	std::vector<std::vector<std::string> > rdata1;
	std::string rdata2[101];
	double form_w;
	double form_vp;
	double form_l;
	double form_lp;
	double form_hp;
	std::string form_wp;
	std::string Junk;

	//
	// Dimension statements
	//
	report_keep_item = scope.prg_item;
	report_keep_ident = scope.prg_ident;
	report_keep_prg = scope.prg_program;
	scope.prg_program = "OUTP_SETTINGS";
	//
	// Skip paint if FORM_DIRECT
	//
	if (utl_reportx.spool == "DIRECT")
	{
		skip = -1;
	}
	else
	{
		skip = 0;
		//
		// Create new window
		//
		smg_status = smg$create_virtual_display(18, 78, utl_reportx.window, SMG$M_BORDER);
		smg_status = smg$label_border(utl_reportx.window, boost::trim_right_copy(utl_report.repnum) + " - " + boost::trim_right_copy(utl_report.repdes));
	}
	//
	// Force values for auto-fill fields
	//
	for (i = 0; i <= 9; i++)
	{
		// ** Converted from a select statement **
		if (utl_report.opttype[i] == "d")
		{
			utl_reportx.optdef[i] = prnt_date(date_today(), 8);
		}
	}
	//
	// Determine options for report settings
	//
	if (boost::trim_right_copy(left_side_cmd) == "")
	{
		left_side_cmd = "DD SF SP EP CP AS AF BG OF ";
		if (utl_reportx.repyn != "N")
		{
			left_side_cmd = left_side_cmd + "RD ";
		}
	}
	if (boost::trim_right_copy(right_side_cmd) == "")
	{
		right_side_cmd = "PT ";
	}
	DataList.Reset();
	DataList.Read(xpos);
	DataList.Read(ypos);
	DataList.Read(xstr);
	while (xpos)
	{
		if (((left_side_cmd + right_side_cmd).find(basic::mid(xstr, 2, 2), 0) + 1) | xstr.substr(0, 1) + basic::mid(xstr, 4, 1) != "()")
		{
			smg_status = smg$put_chars(utl_reportx.window, xstr, xpos, ypos);
		}
		DataList.Read(xpos);
		DataList.Read(ypos);
		DataList.Read(xstr);
	}
	//
	// Various output types that can be displayed (0	5)
	//
	for (i = 0; i <= 11; i++)
	{
		DataList.Read(xtype[i]);
	}
	//
	// Various output types that can be entered
	//
	ocount = 0;
	DataList.Read(otype_V3);
	DataList.Read(otype);
	while (otype_V3)
	{
		ocount = ocount + 1;
		otype_V4[ocount] = otype_V3;
		otype_V2[ocount] = otype;
		DataList.Read(otype_V3);
		DataList.Read(otype);
	}
	//
	// Force information about output device
	//
	output = boost::trim_right_copy(utl_reportx.defout) + " " + xtype[utl_reportx.printto];
	BGosub(L_6400);
	//
	// Do not ask for settings if FORM_DIRECT
	//
	if (skip)
	{
		BGosub(L_18200);
		goto L_4150;
	}
	else
	{
		BGosub(paintall);
		smg_status = smg$paste_virtual_display(utl_reportx.window, scope.smg_pbid, 2, 2);
	}
	//***************************************************************
	// Repaint screen...
	//***************************************************************
	//
L_4005:;
	// Display current settings
	//
	BGosub(paintall);
	//
L_4010:;
	// Handle users options
	//
	scope.prg_item = "HELP";
	scope.prg_ident = "H";
	scope.prg_program = boost::trim_right_copy(utl_report.pronam);
	reqitm = "";
	option = "Change Blank Store Go Wildcard Help eXit";
	opt = entr_3option(scope, "COMMAND", option, opt3, 0);
	// ** Converted from a select statement **
	if ((scope.scope_exit == 3) || ((scope.scope_exit == SMG$K_TRM_F8) || ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_CTRLZ))))
	{
		utl_reportx.printto = 0;
		goto L_4190;
	}
	else if ((scope.scope_exit == 0) || ((scope.scope_exit == 10) || ((scope.scope_exit == 12) || ((scope.scope_exit == 13) || (scope.scope_exit == SMG$K_TRM_DO)))))
	{
	}
	else
	{
		entr_3badkey(scope, scope.scope_exit);
		goto L_4010;
	}
checkoption:;
	// ** Converted from a select statement **
	//
	// Change
	//
	if (opt == "C")
	{
L_4040:;
		if (reqitm != "")
		{
			comma = ((reqitm + ",").find(",", 0) + 1);
			itm = reqitm.substr(0, comma - 1);
			reqitm = basic::right(reqitm, comma + 1);
			goto changee;
		}
		itm = basic::edit(entr_3string(scope, utl_reportx.window, "", "Change", "  ", 4, "", Junk, 2), -1);
		if (itm.size() < 2 && itm != "")
		{
			itm = std::string("0") + itm;
		}
		// ** Converted from a select statement **
		if ((scope.scope_exit == 3) || (scope.scope_exit == 290))
		{
			goto L_4010;
		}
		else if ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_CTRLZ))
		{
			goto L_4010;
		}
		else if ((scope.scope_exit == 0) || ((scope.scope_exit == 10) || ((scope.scope_exit == 12) || ((scope.scope_exit == 13) || (scope.scope_exit == SMG$K_TRM_DO)))))
		{
		}
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto L_4040;
		}
		if (itm == "")
		{
			goto L_4010;
		}
changee:;
		BGosub(enterone);
		work_cmd = left_side_cmd;
		for (i = 0; i <= 4; i++)
		{
			if (utl_report.optlen[i] & basic::edit(utl_report.descr[i], -1) != "")
			{
				work_cmd = work_cmd + basic::Format(i + 1, "<0>#") + " ";
			}
		}
		work_cmd = work_cmd + right_side_cmd;
		for (i = 1; i <= q_i; i++)
		{
			work_cmd = work_cmd + printx.groupx[print_line[i]] + " ";
		}
		for (i = 5; i <= 9; i++)
		{
			if (utl_report.optlen[i] & basic::edit(utl_report.descr[i], -1) != "")
			{
				work_cmd = work_cmd + basic::Format(i + 1, "<0>#") + " ";
			}
		}
		loop = (work_cmd.find(itm, 0) + 1);
		if (loop == 0)
		{
			goto L_4040;
		}
		switch (scope.scope_exit)
		{
		// Uparrow
		case 274:

			if (loop > 3)
			{
				itm = basic::mid(work_cmd, loop - 3, 2);
			}
			goto changee;
			// Downarrow
			break;

		case 275:

			if (loop < work_cmd.size() - 3)
			{
				itm = basic::mid(work_cmd, loop + 3, 2);
			}
			goto changee;
			break;

		}
		goto L_4040;
		//
		// Blank
		//
	}
	else if (opt == "B")
	{
L_4050:;
		itm = basic::edit(entr_3string(scope, utl_reportx.window, "", "Blank", "  ", 4, "", Junk, 2), -1);
		if (itm.size() < 2 && itm != "")
		{
			itm = std::string("0") + itm;
		}
		// ** Converted from a select statement **
		if ((scope.scope_exit == 3) || (scope.scope_exit == 290))
		{
			goto L_4010;
		}
		else if ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_CTRLZ))
		{
			goto L_4010;
		}
		else if ((scope.scope_exit == 0) || ((scope.scope_exit == 10) || ((scope.scope_exit == 12) || ((scope.scope_exit == 13) || (scope.scope_exit == SMG$K_TRM_DO)))))
		{
		}
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto L_4050;
		}
		if (itm == "")
		{
			goto L_4010;
		}
		loop = (std::string(" 01 02 03 04 05 06 07 08 09 10 ").find(itm, 0) + 1);
		if (loop == 0)
		{
			goto L_4050;
		}
		BGosub(enterone);
		goto L_4050;
		//
		// Help
		//
	}
	else if (opt == "H")
	{
		help_34message(scope, "", "H", scope.prg_program, "", "HELP");
		goto L_4005;
		//
		// Store Settings
		//
	}
	else if (opt == "S")
	{
		//
		// Load in the changed data
		//
		for (i = 0; i <= 9; i++)
		{
			utl_report.optdef[i] = utl_reportx.optdef[i];
			utl_report.itemgroup[i] = printx.groupx[i];
			utl_report.item[i] = printx.deflt[i];
		}
		utl_report.defout = boost::trim_right_copy(utl_reportx.defout) + " " + std::to_string(utl_reportx.printto);
		utl_report.printtype = utl_reportx.printtype;
		utl_report.spoolform = utl_reportx.spoolform;
		try
		{
			utl_report.Update();
		}
		catch(basic::BasicError &Be)
		{
			if (Be.err == 131)
			{
				goto notlocked;
			}
			filename = "UTL_REPORT";
			goto helperror;
		}
		help_34message(scope, "settings have been stored", "S", scope.prg_program, "", "SETTUPD");
		// ++
		// Success:SETTUPD
		//	^*The Settings Updated\*
		//	.b
		//	.lm +5
		//	All fields on the report setting screen has been
		//	stored (except (CP),(EP),(SP)).
		//	.lm -5
		//
		// Index:
		//
		// --
		//
		// Go
		//
	}
	else if (opt == "G")
	{
		goto L_4150;
	}
	else if (opt == "W")
	{
L_4080:;
		wild_file = basic::edit(entr_3string(scope, utl_reportx.window, "", "Wildcard Name", std::string(20, ' '), 4, "", Junk, 20), -1);
		// ** Converted from a select statement **
		if ((scope.scope_exit == 3) || (scope.scope_exit == 290))
		{
			goto L_4010;
		}
		else if ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_CTRLZ))
		{
			goto L_4010;
		}
		else if ((scope.scope_exit == 0) || ((scope.scope_exit == 10) || ((scope.scope_exit == 12) || ((scope.scope_exit == 13) || (scope.scope_exit == SMG$K_TRM_DO)))))
		{
			if (wild_file == "")
			{
				goto L_4010;
			}
		}
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto L_4080;
		}
//		st = edt$edit(wild_file + ".WLD", 0, 0, 0, 0, 0, 0, 0);
//		//!\todo selection of editor
		system(("vi " + wild_file).c_str());
		// Off
		st = smg$set_cursor_mode(scope.smg_pbid, 1);
		//
		// Refresh screen
		//
		st = smg$repaint_screen(scope.smg_pbid);
		//
		// eXit
		//
	}
	else if (opt == "X")
	{
		scope.scope_exit = SMG$K_TRM_F10;
		utl_reportx.printto = 0;
		goto L_4190;
	}
	goto L_4010;
	//***************************************************************
L_4150:;
	// TEMPORARY PRINT COMMAND FILE
	//***************************************************************
	//
	// Check for required values
	//
	temp = "";
	for (i = 0; i <= 9; i++)
	{
		if (basic::edit(utl_reportx.optdef[i], -1) == "" && utl_report.require[i] == "Y")
		{
			temp = temp + ", " + basic::Format(i + 1, "<0>#");
		}
	}
	if (temp != "")
	{
		help_34message(scope, std::string("required to setup the field(s) ") + basic::right(temp, 3), "W", scope.prg_program, "", "REQFIELD");
		// ++
		// Warning:REQFIELD
		//	^*Required to Setup a field\*
		//	.b
		//	.lm +5
		//	^*Explanation:\*
		//	.b
		//	It is required to assign a value for a field before
		//	running the report.
		//	.b
		//	^*User Action:\*
		//	.b
		//	Enter a valid value to the field and run the process again.
		//	.lm -5
		//
		// Index:
		//
		// --
		//
		// Force input to required field
		//
		opt = "C";
		reqitm = basic::edit(basic::right(temp, 3), -1);
		goto checkoption;
	}
	//
	// Set nodetach flag if necessary
	//
	if ((utl_report.candet == "N") || utl_reportx.printto == OUTP_TODISPLAY || utl_reportx.printto == OUTP_TOLOCAL)
	{
		utl_reportx.detach = -1;
	}
exitprogram:;
	//
L_4190:;
	//
	// Recover the help key
	//
	scope.prg_program = report_keep_prg;
	scope.prg_ident = report_keep_ident;
	scope.prg_item = report_keep_item;
	return;
	//***************************************************************
	// DATA ENTRY SECTION
	//***************************************************************
enterone:;
	if (opt == "C")
	{
		cbflag = 0;
	}
	else
	{
		cbflag = 33;
	}
	scope.prg_program = "OUTP_SETTINGS";
	scope.prg_item = std::string("FLD") + itm;
	// STORE_PRG_ITEM$ = SCOPE::PRG_ITEM
entertwo:;
	// ** Converted from a select statement **
	//
	// Enter new output device
	//
	if (itm == "DD")
	{
		// ++
		// Abstract:FLDDD
		//	^*(DD) Destination\*
		//	.b
		//	.lm +5
		//	This field is used to specify the device which is assigned
		//	to print the report.
		//	.b
		//	Possible values are:
		//	.b
		//	.lm +5
		//	^*DISPLAY\*	Display the report on the screen.
		//	.b
		//	^*DP\*	Mnemonic for screen display.
		//	.b
		//	^*PRINTER	PORT\*	Send output to printer port.
		//	.b
		//	^*PP\*	Mnemonic for printer port.
		//	.b
		//	^*SPOOLER\*	Send output to spooler.
		//	.b
		//	^*SP\*	Mnemonic for spooler
		//	.b
		//	^*Filename\*	Send output to a file.
		//	.b
		//	^*Device:\*	Send output to a device (Printer,
		//	etc.)
		//	.b
		//	^*Word processing\*	Send output to a word processing
		//	file
		//	.b
		//	^*WP\*	Mnemonic for word processing
		//	.lm -5
		//
		// Index:
		//	.x End Page
		//	.x Report Settings>End Page
		//
		// --
		output = boost::trim_right_copy(utl_reportx.defout) + " " + xtype[utl_reportx.printto];
		output = (output + std::string(64, ' ')).substr(0, 64);
		output = entr_3string(scope, utl_reportx.window, "1;19", "Destination", output, cbflag | 16, "'LLLLLLLLLLLLLLLLLLL", Junk, 64);
		// ** Converted from a select statement **
		if ((scope.scope_exit == 3) || (scope.scope_exit == 290))
		{
			goto L_6092;
		}
		else if ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_CTRLZ))
		{
			goto L_6092;
		}
		else if ((scope.scope_exit == 0) || ((scope.scope_exit == 10) || ((scope.scope_exit == 12) || ((scope.scope_exit == 13) || ((scope.scope_exit == SMG$K_TRM_DO) || ((scope.scope_exit == 274) || (scope.scope_exit == 275)))))))
		{
		}
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto enterone;
		}
		if (output == "")
		{
			goto L_6092;
		}
		BGosub(L_6400);
		BGosub(paintto);
		//
		// Enter Spooler Form Name
		//
	}
	else if (itm == "SF")
	{
		// ++
		// Abstract:FLDSF
		//	^*(SF) Spooler Form\*
		//	.b
		//	.lm +5
		//	The ^*Spooler Form\* field will allow selection of forms
		//	that have previously been defined through VMS/DCL. This
		//	field is only useful if a legitimate spooler has been
		//	selected in the (DD) field (see DD help).
		//	.b
		//	Currently, there are two items of concern when using
		//	various forms: form name and form stock. Form name
		//	is the name given to the form and form stock describes
		//	the paper required for that form. Form stock defaults
		//	to the form name if it is not specified.
		//	.b
		//	If the form name is the same as the form name currently
		//	associated (mounted on) with the requested queue then the
		//	job will be submitted and printed; otherwise if the form
		//	name is different and the form stock is different then
		//	the job will be pending in the queue until the form names
		//	match or until the form stocks match. The System Manager
		//	or a privileged user may change the form associated with
		//	the queue. If the form stock matches the form stock of
		//	the queue then the job will be submitted and printed even
		//	if the form names do not match.
		//	.b
		//	If your account has the privilege OPER or greater then
		//	the requested form will be mounted on the requested queue
		//	and the job will be submitted and printed provided the queue
		//	is a legitimate queue. This will be done automatically.
		//	.b
		//	^*NOTE:\* It is important to realize that if this occurs
		//	the queue will now have that new form associated with it
		//	until the System Manager or privileged user changes it.
		//	This means that everything sent to that queue (spooler) will
		//	not print unless the form name and/or form stock match as
		//	described above.
		//	.b
		//	Pressing ^*List Choices\* will provide a list of
		//	the available predefined forms.
		//	.lm -5
		//
		// Index:
		//	.x Start Page
		//	.x Report Settings>Start Page
		//
		// --
		utl_reportx.spoolform = entr_3string(scope, utl_reportx.window, "2;19", "Spooler Form", utl_reportx.spoolform, cbflag | 16, "'E", Junk, 16);
		//! \todo List of form names. Do I really want to care about this?
//		if (scope.scope_exit == SMG$K_TRM_F14)
//		{
//			read_queform("SYS$PRINT", rdata1, stat);
//			for (iz = 1; iz <= std::stol(rdata1[0][0]); iz++)
//			{
//				rdata2[iz] = (rdata1[iz][1] + std::string(31, ' ')).substr(0, 31) + " " + rdata1[iz][2] + " " + (rdata1[iz][3] + std::string(31, ' ')).substr(0, 31);
//			}
//			x = entr_3choice(scope, "", "", rdata2, "", 138, "  Form Name                        #  Form Stock", "034,038", 0);
//			scope.scope_exit = 0;
//			if (x > 0)
//			{
//				utl_reportx.spoolform = boost::trim_right_copy(rdata2[x].substr(0, 31));
//				goto entertwo;
//			}
//		}
		//
		// Enter the starting page
		//
	}
	else if (itm == "SP")
	{
		// ++
		// Abstract:FLDSP
		//	^*(SP) Start Page\*
		//	.b
		//	.lm +5
		//	^*(SP) Start Page\* allows the user to specify the first
		//	page of the report to be printed. The entire report is created,
		//	but will not print until the selected page number is reached.
		//	A value of ^*zero (0)\* or ^*one (1)\* will cause the report to
		//	start printing at the first page.
		//	.b
		//	^*Note:\*
		//	This setting is not functional when the
		//	report is displayed on the screen, since
		//	there are no page numbers on a displayed
		//	report.
		//	.lm -5
		//
		// Index:
		//	.x Start Page
		//	.x Report Settings>Start Page
		//
		// --
		utl_reportx.startp = entr_3number(scope, utl_reportx.window, "5;19", "Start Page", utl_reportx.startp * 1.0, cbflag, "#####", Junk);
		//
		// Enter the ending page
		//
	}
	else if (itm == "EP")
	{
		// ++
		// Abstract:FLDEP
		//	^*(EP) End Page\*
		//	.B
		//	.LM +5
		//	^*(EP) End Page\* allows the user to specify the last page
		//	number which is to be printed on the report. A value of ^*zero
		//	(0)\* causes the report to print to and including the last page.
		//	A value of ^*Ten (10)\* for example, would cause the report to
		//	discontinue after printing page l0.
		//	.B
		//	^*Note:\*  This setting is not functional when the report
		//	is displayed on the screen, since there are no
		//	page numbers on a displayed report.
		//	.lm -5
		//
		// Index:
		//	.x End Page
		//	.x Report Settings>End Page
		//
		// --
		utl_reportx.endp = entr_3number(scope, utl_reportx.window, "6;19", "End Page", utl_reportx.endp * 1.0, cbflag, "#####", Junk);
		//
		// Enter the number of copies
		//
	}
	else if (itm == "CP")
	{
		// ++
		// Abstract:FLDCP
		//	^*(CP) Copies\*
		//	.b
		//	.lm +5
		//	^*(CP) Copies\*, allows the user to specify the number of
		//	copies to be printed.
		//	.b
		//	^*Note:\* This setting functions only when a spooler has been
		//	selected. The document destination (^*DD\*) must be set
		//	to ^*SP\* (or ^*SPOOLER\*) before the spooler name otherwise
		//	it will not recognize the spooler.
		//	.lm -5
		//
		// Index:
		//	.x Copies
		//	.x Report Setting>Copies
		//
		// --
		utl_reportx.copies = entr_3number(scope, utl_reportx.window, "7;19", "Number of Copies", utl_reportx.copies * 1.0, cbflag, "#####", Junk);
		if (utl_reportx.copies > 19)
		{
			entr_3message(scope, "Have you considered a copy machine??", 1);
		}
		//
		// Autoscroll
		//
	}
	else if (itm == "AS")
	{
		// ++
		// Abstract:FLDAS
		//	^*(AS) Auto Scroll\*
		//	.b
		//	.lm +5
		//	The ^*(AS) Auto Scroll\* setting is functional only when a
		//	report is displayed on the screen as opposed to printing the
		//	report. A ^*Yes\* setting will cause a displayed report to
		//	scroll continuously on the screen.
		//	.b
		//	The scrolling can be halted by pressing the ^*Hold Screen\*
		//	key, otherwise the scrolling will be continuous until the end
		//	of the report is reached.
		//	.b
		//	The scrolling can be terminated by pressing the ^*Exit\*
		//	key.
		//	.lm -5
		//
		// Index:
		//	.x Auto Scroll
		//	.x Report Settings>Auto Scroll
		//
		// --
		if (utl_reportx.autoscroll)
		{
			temp = "Y";
		}
		else
		{
			temp = "N";
		}
		utl_reportx.autoscroll = entr_3yesno(scope, utl_reportx.window, "8;19", "Autoscroll", temp, cbflag, "'E", Junk) == "Y";
		//
		// Enter Run After Time
		//
	}
	else if (itm == "AF")
	{
		// ++
		// Abstract:FLDAF
		//	^*(AF) After\*
		//	.b
		//	.lm +5
		//	The ^*After\* field allows the user to specify the
		//	the time at which the report will run.
		//	.b
		//	NOTE: This field is only used when running the report
		//	in background mode (See field BG).
		//	.lm -5
		//
		// Index:
		//	.x After
		//	.x Report Settings>After
		//
		// --
		utl_reportx.aftertime = entr_3time(scope, utl_reportx.window, "9;19", "Run AFter Time", utl_reportx.aftertime, cbflag, "", Junk);
		//
		// Enter Background/forground mode
		//
	}
	else if (itm == "BG")
	{
		// ++
		// Abstract:FLDBG
		//	^*(BG) Background\*
		//	.b
		//	.lm +5
		//	The ^*Background\* field allows the user to specify
		//	if they want to run the report in background mode,
		//	which causes the report generation to be sent to the
		//	system que, freeing up his terminal.
		//	.b
		//	NOTE: The background mode will not work if the
		//	report is being displayed or printed through the
		//	printer port.
		//	.lm -5
		//
		// Index:
		//	.x Background
		//	.x Report Settings>Background
		//
		// --
		utl_reportx.background = entr_3yesno(scope, utl_reportx.window, "10;19", "Background", utl_reportx.background, cbflag, "'E", Junk);
		//
		// Enter offset
		//
	}
	else if (itm == "OF")
	{
		// ++
		// Abstract:FLDOF
		//	^*(OF) Offset\*
		//	.b
		//	.lm +5
		//	The ^*Offset\* field allows the user to specify
		//	additional spaces added to the right margin.
		//	.lm -5
		//
		// Index:
		//	.x Offset
		//	.x Report Settings>Offset
		//
		// --
		utl_reportx.offset = entr_3number(scope, utl_reportx.window, "11;19", "Offset", utl_reportx.offset * 1.0, cbflag, "####", Junk);
		//
		// Enter report date
		//
	}
	else if (itm == "RD")
	{
		// ++
		// Abstract:FLDRD
		//	^*(RD) Report Date\*
		//	.b
		//	.lm +5
		//	The ^*Report Date\* field allows the user to specify the
		//	date which will be printed on the report.  The system will
		//	automatically display the current date.  If a change in the
		//	date is needed, use the ^*Change\* function.  Any date format
		//	is acceptable.
		//	.lm -5
		//
		// Index:
		//	.x Report Date
		//	.x Report Settings>Report Date
		//
		// --
		utl_reportx.repdate = (utl_reportx.repdate + std::string(20, ' ')).substr(0, 20);
		utl_reportx.repdate = entr_3string(scope, utl_reportx.window, "12;19", "Report Date", utl_reportx.repdate, cbflag, "'E", Junk, 16);
		//
		// Printer type
		//
	}
	else if (itm == "PT")
	{
		// ++
		// Abstract:FLDPT
		//	^*(PT) Printer Type\*
		//	.b
		//	.lm +5
		//	^*(PT) Printer type\* defines the type of printer used to print
		//	the report. If the printer type is changed the system will search
		//	for the new printer type and read in the set command file.
		//	.b
		//	Currently defined printers include:
		//	.table 3,25
		//	.te
		//	^*DEFALT Default printer type
		//	.te
		//	FX286 Epson FX-286
		//	.te
		//	LA120 LA120 Decwriter IV\*
		//	.end table
		//
		// --
		temp_printtype = utl_reportx.printtype;
L_6020:;
		utl_reportx.printtype = entr_3string(scope, utl_reportx.window, "1;67", "Printer Type", utl_reportx.printtype, cbflag | 16, "'E", Junk, 6);
		// ** Converted from a select statement **
		//
		// Exit
		//
		if ((scope.scope_exit == 3) ||
			(scope.scope_exit == SMG$K_TRM_F10) ||
			(scope.scope_exit == SMG$K_TRM_CTRLZ))
		{
			goto L_6092;
			//
			// List choices
			//
		}
		else if (scope.scope_exit == SMG$K_TRM_F14)
		{
			temp = libr_select("print_type", "Printer Type", "LIBR_SELECT", "Maintain Create Help eXit");
			//
			// Valid command for LIBR_SELECT ARE:
			//	* Maintain
			//	* Create
			//	* Help
			//	* eXit
			//
			if (temp != "")
			{
				utl_reportx.printtype = temp;
			}
			BGosub(L_18200);
			if (printx.groups == 0)
			{
				entr_3message(scope, utl_reportx.printtype + " has no attributes Please retry.", 0);
				goto L_6020;
			}
			BGosub(L_6300);
			goto L_6020;
		}
		else if ((scope.scope_exit == 0) ||
			(scope.scope_exit == 10) ||
			(scope.scope_exit == 12) ||
			(scope.scope_exit == 13) ||
			(scope.scope_exit == SMG$K_TRM_DO) ||
			(scope.scope_exit == SMG$K_TRM_UP) ||
			(scope.scope_exit == SMG$K_TRM_DOWN))
		{
		}
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto enterone;
		}
		if (temp_printtype == utl_reportx.printtype)
		{
			goto L_6092;
		}
		BGosub(L_18200);
		if (printx.groups == 0)
		{
			entr_3message(scope, utl_reportx.printtype + " has no attributes Please retry.", 0);
			goto L_6020;
		}
		BGosub(L_6300);
		//
		// General data entry for Prog. Dep. input
		//
	}
	else if ((itm == "01") || ((itm == "02") || ((itm == "03") || ((itm == "04") || ((itm == "05") || ((itm == "06") || ((itm == "07") || ((itm == "08") || ((itm == "09") || (itm == "10"))))))))))
	{
		//
		// Set up for help key
		//
		scope.prg_program = boost::trim_right_copy(utl_report.pronam);
		//
		//
		pdn = std::stol(itm) - 1;
		//
		// If blank option and Required field then stop blank?
		//
		if (cbflag == 33)
		{
			if ((utl_report.require[pdn] == "Y") && (inp == ""))
			{
				entr_3message(scope, "This field cannot be set to blank!", 0);
				goto L_6092;
			}
		}
		//
		// Skip out if the length is 0 or if the description is blank
		//
		if (utl_report.optlen[pdn] == 0 || basic::edit(utl_report.descr[pdn], -1) == "")
		{
			goto L_6092;
		}
		//
		// Calculate the screen postion and print length
		//
		temp_V5 = utl_report.optlen[pdn];
		if (pdn <= 4)
		{
			xpos_V7 = 27;
			ypos_V8 = pdn + 14;
			print_len = 12;
			if (temp_V5 < print_len)
			{
				print_len = temp_V5;
			}
		}
		else
		{
			xpos_V7 = 66;
			ypos_V8 = pdn + 9;
			print_len = 14;
			if (temp_V5 < print_len)
			{
				print_len = temp_V5;
			}
		}
		temp = std::to_string(ypos_V8) + ";" + std::to_string(xpos_V7);
		//
L_6030:;
		// Enter data item
		//
		// ** Converted from a select statement **
		// Floating point
		if (utl_report.opttype[pdn] == "F")
		{
			temp1 = basic::Qstring(print_len - 3, '#') + ".##";
			m = entr_3number(scope, utl_reportx.window, temp, boost::trim_right_copy(utl_report.descr[pdn]), std::stod(utl_reportx.optdef[pdn]) * 1.0, cbflag, temp1, Junk);
			inp = basic::Format(m, temp1);
			work = (inp.find("%", 0) + 1);
			if (work)
			{
				entr_3message(scope, "Number is to large for format field.", 0);
				inp = basic::right(inp, work + 1);
			}
			// Date
		}
		else if ((utl_report.opttype[pdn] == "D") || (utl_report.opttype[pdn] == "d"))
		{
			if (temp_V5 == 6)
			{
				tt = 6;
			}
			else
			{
				tt = 8;
			}
			tmep = utl_reportx.optdef[pdn].substr(0, tt + 2);
			tmep = basic::mid(tmep, 7, tt - 4) + tmep.substr(0, 2) + basic::mid(tmep, 4, 2);
			inp = entr_3date(scope, utl_reportx.window, temp, boost::trim_right_copy(utl_report.descr[pdn]), tmep, cbflag, std::to_string(tt), Junk, 8);
			inp = basic::mid(inp, tt - 3, 2) + "/" + basic::mid(inp, tt - 1, 2) + "/" + inp.substr(0, tt - 4);
			// Integer
		}
		else if (utl_report.opttype[pdn] == "I")
		{
			temp1 = basic::Qstring(print_len, '#');
			m_V1 = entr_3number(scope, utl_reportx.window, temp, boost::trim_right_copy(utl_report.descr[pdn]), std::stod(utl_reportx.optdef[pdn]) * 1.0, cbflag, temp1, Junk);
			inp = basic::Format(m_V1, temp1);
			// Period
		}
		else if (utl_report.opttype[pdn] == "P")
		{
			inp = entr_period(utl_reportx.window, temp, boost::trim_right_copy(utl_report.descr[pdn]), utl_reportx.optdef[pdn].substr(0, temp_V5), cbflag, "", Junk);
			// Yes/No
		}
		else if (utl_report.opttype[pdn] == "Y")
		{
			temp1 = std::string("'") + basic::Qstring(print_len - 1, 'L');
			inp = entr_3yesno(scope, utl_reportx.window, temp, boost::trim_right_copy(utl_report.descr[pdn]), utl_reportx.optdef[pdn].substr(0, temp_V5), cbflag, temp1, Junk);
			// Upper case string
		}
		else if (utl_report.opttype[pdn] == "S")
		{
			temp1 = std::string("'") + basic::Qstring(print_len - 1, 'L');
			inp = entr_3string(scope, utl_reportx.window, temp, boost::trim_right_copy(utl_report.descr[pdn]), utl_reportx.optdef[pdn].substr(0, temp_V5), cbflag | 16, temp1, Junk, 20);
			// String
		}
		else
		{
			temp1 = std::string("'") + basic::Qstring(print_len - 1, 'L');
			inp = entr_3string(scope, utl_reportx.window, temp, boost::trim_right_copy(utl_report.descr[pdn]), utl_reportx.optdef[pdn].substr(0, temp_V5), cbflag, temp1, Junk, 20);
		}
		// ** Converted from a select statement **
		if ((scope.scope_exit == 3) || (scope.scope_exit == 290))
		{
			goto L_6092;
		}
		else if ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_CTRLZ))
		{
			goto L_6092;
		}
		else if ((scope.scope_exit == 0) || ((scope.scope_exit == 10) || ((scope.scope_exit == 12) || ((scope.scope_exit == 13) || ((scope.scope_exit == SMG$K_TRM_DO) || ((scope.scope_exit == 274) || (scope.scope_exit == 275)))))))
		{
		}
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto enterone;
		}
		//
		// Required field?
		//
		if ((utl_report.require[pdn] == "Y") && (inp == ""))
		{
			help_34message(scope, "value is required", "W", "OUTP_SETTINGS", "", "VALUEREQ");
			// ++
			// Warning:VALUEREQ
			//	^*Explanation\*
			//	.p
			//
			// --
			goto L_6030;
		}
		//
		// Test input
		//
		if (!(utl_report.valid[pdn] == "") && basic::edit(inp, -1) != "")
		{
			if (comp_string(inp, utl_report.valid[pdn]) == 0)
			{
				entr_3message(scope, std::string("Valid input is ") + utl_report.valid[pdn], 0);
				scope.scope_exit = 0;
				goto L_6030;
			}
		}
		utl_reportx.optdef[pdn] = inp;
		//
		// Print item
		//
		smg_status = smg$put_chars(utl_reportx.window, utl_reportx.optdef[pdn].substr(0, print_len), ypos_V8, xpos_V7, 0, SMG$M_BOLD);
		//
		// It might be one of the printer functions
		//
	}
	else
	{
		for (i = 1; i <= q_i; i++)
		{
			if (itm == printx.groupx[print_line[i]])
			{
				goto L_6080;
			}
		}
		goto L_6092;
		//
L_6080:;
		// We got a match
		//
		k = print_line[i];
		temp = entr_3string(scope, utl_reportx.window, std::to_string(i + 1) + ";67", boost::trim_right_copy(printx.descr[k]), printx.deflt[k], 0, "'E", Junk, 20);
		scope.prg_item = std::string("FLD") + printx.groupx[k];
		// ++
		// Abstract:FLDLP
		//	^*(LP) Lines per Page\*
		//	.b
		//	.lm +5
		//	^*Lines per page\* defines the number of vertical lines contained on a page.
		//	This number is user defined.
		//	.lm -5
		//
		// Index:
		//	.x Lines per Page
		//
		// --
		// ++
		// Abstract:FLDHP
		//	^*(HP) Horizontal Pitch\*
		//	.b
		//	.lm +5
		//	^*Horizontal pitch\* is defined as the number of characters per horizontal inch.
		//	Valid horizontal pitch settings vary, depending upon the printer type ^*(PT)\*
		//	setting. The pitch may be changed by entering a valid number associated with
		//	the printer type. A list of valid settings will be displayed by pressing the
		//	^*List Choices\* key, while the cursor is located at the ^*(HP)\* field. A
		//	choice may be made by positioning the arrow to the left of the desired choice
		//	and pressing the ^*Select\* key.
		//	.lm -5
		//
		// Index:
		//	.x Horizontal Pitch
		//
		// --
		// ++
		// Abstract:FLDVP
		//	^*(VP) Vertical Pitch\*
		//	.b
		//	.lm +5
		//	^*Vertical pitch\* is defined as the number of lines per vertical inch. Valid
		//	vertical pitch settings vary, depending on the printer type ^*(PT)\*
		//	settings. The pitch may be changed by entering a valid number associated with
		//	the printer type. A list of valid setting will be displayed by pressing the
		//	^*List Choices\* key, while the cursor is located at the ^*(VP)\* field.
		//	A choice may be made by positioning the arrow to the left of the desired
		//	choice and pressing the ^*Select\* key.
		//	.lm -5
		//
		// Index:
		//	.x Vertical Pitch
		//
		// --
		// ++
		// Abstract:FLDFT
		//	^*(FT) Font Selection\*
		//	.b
		//	.lm +5
		//	^*Font Selection\* is the type of printing available. Valid Font settings
		//	vary with the printer type ^*(PT)\* being used.  A valid setting may be
		//	entered or a list may be displayed by using the ^*<List Choices>\* key when
		//	positioned in the ^*(FT)\* field.  A selection may be issued from the list
		//	by positioning the arrow to the left of the selection and pressing the
		//	^*<Select>\* key.
		//
		// Index:
		//	.x Font Selection
		//
		// --
		// ++
		// Abstract:FLDQU
		//	^*(QU) Quality\*
		//	.b
		//	.lm +5
		//	^*Quality\* is the type of printing used by the printer. Quality may vary
		//	with the printer type ^*(PT)\* setting. A valid quality may be entered or
		//	a list of choices will be given when the cursor is placed in the ^*(QU)\*
		//	field. A choice may be made by positioning the arrow to the left of the
		//	desired choice and pressing the ^*Select\* key.
		//	.lm -5
		//
		// Index:
		//	.x Quality
		//
		// --
		// ++
		// Abstract:FLDCS
		//	^*(CS) Character Set\*
		//	.b
		//	.lm +5
		//	^*Character Set\* determines whether the printing will be normal or
		//	initialized.
		//	The valid code may be entered or the ^*List Choices\* key may be used.
		//	To access through the ^*List Choices\* key, place the cursor in the ^*
		//	Character Set\* field and press the ^*List Choices\* key. Move the arrow
		//	to the desired choice and press the ^*Select\* key
		//	.lm -5
		//
		// Index:
		//	.x Character Set
		//	.x Set>Character
		//	.x Report Settings>Character Set
		//
		// --
		// ++
		// Abstract:FLDFO
		//	^*(FO) Font\*
		//	.b
		//	.lM +5
		//	^*Font\* refers to the type of character style available. The options may
		//	vary with the ^*Printer Type (PT)\*. A valid choice may be entered or the
		//	^*<List Choices>\* key used. When using ^*<List Choices>\*, place
		//	the cursor in the ^*Font\* field and press the ^*<List Choices>\* key.  Move
		//	the arrow to the desired selection and press the ^*<Select>\* key.
		//
		// Index:
		//	.x Font>Report Settings
		//	.x Report Settings>Font
		//
		// --
		// ++
		// Abstract:FLDLQ
		//	^*(LQ) Letter Quality Mode\*
		//	.b
		//	.lm +5
		//	^*Letter Quality Mode\* refers to the quality of printing and depends
		//	on the ^*Printer Type (PT)\* selected. A valid code may be entered or the
		//	^*<List Choices>\* key may be used. To use the ^*<List Choices>\* key, place
		//	the cursor in the ^*Letter Quality Mode\* field and press the ^*<List
		//	Choices>\*
		//	key. Move the arrow to the desired selection and press the ^*<Select>\* key.
		//	.lm -5
		//
		// Index:
		//	.x Letter Quality Mode>Report Settings
		//	.x Report Settings>Letter Qualtiy Mode
		//	.x Mode>Letter Quality
		//
		// --
		// ++
		// Abstract:FLDPF
		//	^*(PF) Page Format\*
		//	.b
		//	.lm +5
		//	^*Page Format\* allows for the option of wide or normal print.  A valid
		//	code may be entered or the ^*<List Choices>\* key used.  In using the^*<List
		//	Choices>\* key, place the cursor in the ^*Page Format\* field and press the
		//	^*<List Choices>\* key. Move the arrow to the desired choice and press the
		//	^*<Select>\* key.
		//	.lm -5
		//
		// Index:
		//	.x Page Format>Report Settings
		//	.x Report Settings>Page Format
		//	.x Format>Page
		//
		// --
		// ++
		// Abstract:FLDPZ
		//	^*(PZ) Point Size\*
		//	.b
		//	.lm +5
		//	^*Point Size\* determines how big the characters will be. The ^*Point Size\*
		//	options change with the ^*Printer Type (PT)\*. A valid code may be entered
		//	or the selection may be made by using the ^*<List Choices>\* menu. To use
		//	the ^*<List Choices>\* menu, place the cursor in the ^*Point Size (PZ)\*
		//	field and press the ^*<List Choices>\* key. Move the arrow to the desired
		//	choice and press the ^*select\* key.
		//	.lm -5
		//
		// Index:
		//	.x Point Size>Report Settings
		//	.x Report Settings>Point Size
		//	.x Size>Point
		//
		// --
		// ++
		// Abstract:FLDST
		//	^*(ST) Character Style\*
		//	.b
		//	.lm +5
		//	^*Character Style\* refers to the added effects on the characters. A valid
		//	code may be entered or the ^*List Choices\* menu used. To use the ^*List
		//	Choices\* menu, place the cursor in the ^*Character Style\* file and press
		//	the ^*List Choices\* key. Move the arrow to the desired choice and press
		//	the ^*Select\* key.
		//	.lm -5
		//
		// Index:
		//	.x Character Style>Report Settings
		//	.x Report Settings>Character Style
		//	.x Style>Character
		//
		// --
		// ++
		// Abstract:FLDTF
		//	^*(TF) Typeface\*
		//	.b
		//	.lm +5
		//	^*Typeface\* is defined as the shape of the set of characters. The ^*Typeface\*
		//	settings vary with the ^*Printer Type (PT)\* being used. A valid code
		//	may be entered or the ^*List Choices\* menu may be accessed. To access the
		//	menu, place the cursor in the ^*Typeface (TF)\* field and press the ^*List
		//	Choices\* key.  Move the arrow to the desired selection and press the
		//	^*Select\* key.
		//	.lm -5
		//
		// Index:
		//	.x Typeface>Report Settings
		//	.x Report Settings>Typeface
		//
		// --
		// ** Converted from a select statement **
		if ((scope.scope_exit == 3) || ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_CTRLZ)))
		{
			goto L_6092;
		}
		else if (scope.scope_exit == SMG$K_TRM_F14)
		{
			BGosub(L_9000);
			goto L_6080;
		}
		else if ((scope.scope_exit == 0) || ((scope.scope_exit == 10) || ((scope.scope_exit == 12) || ((scope.scope_exit == 13) || ((scope.scope_exit == SMG$K_TRM_DO) || ((scope.scope_exit == SMG$K_TRM_UP) || (scope.scope_exit == SMG$K_TRM_DOWN)))))))
		{
		}
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto enterone;
		}
		//
		// Check for good input
		//
		if (find_3printgroupitem(printx.groupx[k], temp, printx) <= 0)
		{
			BGosub(L_9000);
			//
			// Bad input| Bad input| (Swat| Swat|)
			//
			goto L_6080;
		}
		else
		{
			//
			// Got a good value
			//
			printx.deflt[k] = temp;
			//
			// Update the Form values
			//
			BGosub(paintform);
		}
	}
	if ((scope.scope_exit == 0) ||
		(scope.scope_exit == 3) ||
		(scope.scope_exit == 10) ||
		(scope.scope_exit == 12) ||
		(scope.scope_exit == 13) ||
		(scope.scope_exit == SMG$K_TRM_DO) ||
		(scope.scope_exit == SMG$K_TRM_UP) ||
		(scope.scope_exit == SMG$K_TRM_DOWN) ||
		(scope.scope_exit == SMG$K_TRM_F10) ||
		(scope.scope_exit == SMG$K_TRM_CTRLZ))
	{
	}
	else
	{
		entr_3badkey(scope, scope.scope_exit);
		goto enterone;
	}
L_6092:;
	BReturn;

	//******************************************************************
	// Paint background
	//******************************************************************
paintall:;
	// Search out printer type
	BGosub(L_18200);
	BGosub(paintreport);
	BGosub(paintprint);
	BGosub(paintto);
	BReturn;

	//***************************************************************
	// Paint entire screen background
	//***************************************************************
paintreport:;
	scope.scope_exit = 0;
	cbflag = 1;
	for (loop = 1; loop <= left_side_cmd.size(); loop += 3)
	{
		itm = basic::mid(left_side_cmd, loop, 2);
		BGosub(entertwo);
	}
	for (loop = 1; loop <= right_side_cmd.size(); loop += 3)
	{
		itm = basic::mid(right_side_cmd, loop, 2);
		BGosub(entertwo);
	}
	BGosub(paintform);
	for (i = 0; i <= 4; i++)
	{
		if (utl_report.optlen[i] & basic::edit(utl_report.descr[i], -1) != "")
		{
			smg_status = smg$put_chars(utl_reportx.window, basic::Format(i + 1, "(<0>#) ") + utl_report.descr[i].substr(0, 20) + " ", i + 14, 1);
			smg_status = smg$put_chars(utl_reportx.window, utl_reportx.optdef[i].substr(0, 12), 0, 0, 0, SMG$M_BOLD);
		}
	}
	for (i = 5; i <= 9; i++)
	{
		if (utl_report.optlen[i] & basic::edit(utl_report.descr[i], -1) != "")
		{
			smg_status = smg$put_chars(utl_reportx.window, basic::Format(i + 1, "(<0>#) ") + utl_report.descr[i].substr(0, 20) + " ", i + 9, 40);
			smg_status = smg$put_chars(utl_reportx.window, utl_reportx.optdef[i].substr(0, 14), 0, 0, 0, SMG$M_BOLD);
		}
	}
	BReturn;

	//***************************************************************
	// Paint where report is going to
	//***************************************************************
paintto:;
	output = boost::trim_right_copy(utl_reportx.defout) + " " + xtype[utl_reportx.printto];
	output = (output + std::string(20, ' ')).substr(0, 20);
	smg_status = smg$put_chars(utl_reportx.window, output, 1, 19, 0, SMG$M_BOLD);
	BReturn;

	//***************************************************************
L_6300:;
	// Paint background data for reports
	//***************************************************************
paintprint:;
	q_i = 0;
	if (((left_side_cmd + right_side_cmd).find("PT", 0) + 1))
	{
		smg_status = smg$put_chars(utl_reportx.window, utl_reportx.printtype, 1, 67, 0, SMG$M_BOLD);
		for (i = 1; i <= printx.groups; i++)
		{
			//
			// Don't print if not allowed
			//
			if ((printx.groupp[i] & 2048) == 0)
			{
				q_i = q_i + 1;
				print_line[q_i] = i;
				smg_status = smg$put_chars(utl_reportx.window, std::string("(") + printx.groupx[i] + ") " + printx.descr[i] + " ", q_i + 1, 41);
				smg_status = smg$put_chars(utl_reportx.window, printx.deflt[i], 0, 0, 0, SMG$M_BOLD);
			}
		}
	}
	for (i = q_i + 1; i <= 11; i++)
	{
		smg_status = smg$erase_line(utl_reportx.window, i + 1, 41);
	}
	BReturn;

paintform:;
	//***************************************************************
	// Print the from width and length after calculating
	//***************************************************************
	for (i = 1; i <= printx.groups; i++)
	{
		if (printx.groupx[i] == "HP")
		{
			form_hp = std::stod(printx.deflt[i]);
		}
		if (printx.groupx[i] == "VP")
		{
			form_vp = std::stod(printx.deflt[i]);
		}
		if (printx.groupx[i] == "LP")
		{
			form_lp = std::stod(printx.deflt[i]);
		}
	}
	if (utl_report.repwid == 0)
	{
		utl_report.repwid = 132;
	}
	if (form_hp > 0.)
	{
		form_w = utl_report.repwid / form_hp;
	}
	else
	{
		form_w = 0.;
	}
	smg_status = smg$put_chars(utl_reportx.window, basic::Format(form_w, "##.#"), 3, 19);
	if (form_vp > 0.)
	{
		form_l = form_lp / form_vp;
	}
	else
	{
		form_l = 0.;
	}
	smg_status = smg$put_chars(utl_reportx.window, basic::Format(form_l, "##.#"), 4, 19);
	BReturn;

	//***************************************************************
L_6400:;
	// Figure out what an DD type is (name in OUTPUT$)
	//***************************************************************
	output = basic::edit(output, 4 + 8 + 16 + 32 + 128 + 256);
	temp = "TT:";
	// ==================================================================
	// Search the users input for PRINTER PORT, PP, DISPLAY, ...
	// ==================================================================
	for (i = 1; i <= ocount; i++)
	{
		if (basic::right(std::string(" ") + output, output.size() - otype_V2[i].size() + 1) == std::string(" ") + otype_V2[i])
		{
			temp1 = output.substr(0, output.size() - otype_V2[i].size() - 1);
			print_to = otype_V4[i];
			goto L_6405;
		}
	}
	//
	// Default case - output to a device
	//
	print_to = OUTP_TODEVICE;
	temp1 = output;
	//
L_6405:;
	// Spool
	//
	temp1 = basic::edit(temp1, 4 + 8 + 32 + 128 + 256);
	// ** Converted from a select statement **
	if (print_to == OUTP_TOSPOOL)
	{
		temp = "SYS$PRINT";
		if (temp1 != "")
		{
			temp = temp1;
		}
		utl_reportx.printto = OUTP_TOSPOOL;
		utl_reportx.defout = temp;
		utl_reportx.spool = temp;
		goto exitsub;
	}
	else if (print_to == OUTP_TODOCUMENT)
	{
		//
		// Send only the first page to the documentation
		// and force LA120 Printer
		//
		utl_reportx.startp = utl_reportx.endp = 1;
		utl_reportx.printtype = "LA120";
		if (temp1 == "")
		{
			temp1 = "TEMP_DOCUMENT.TMP";
		}
	}
	else if (print_to == OUTP_TO2020)
	{
		if (temp1 == "")
		{
			temp1 = "TEMP.S2020";
		}
	}
	else if (print_to == OUTP_TOPL)
	{
		if (temp1 == "")
		{
			temp1 = "TEMP.PL";
		}
	}
	if (temp1 != "")
	{
		temp = temp1;
	}
	// ==================================================================
	// First try at scanning name
	// ==================================================================
	//
	// FTYPE%
	//	3 - Printer/terminal
	//	4 - File
	//	5 - Users Terminal (printer port?)
	//
	//! \todo determine device type
//	sys_status = lib$getdvi(dvi$_devclass, 0, temp, devchar, 0, 0);
	//
	// Certain errors may mean that it is a file
	//
//	if ((sys_status == 324) || (sys_status == 2312))
//	{
		ftype = OUTP_TOFILE;
		goto L_6420;
//	}
	//
	// Error 2288 occurs when attempting to go over DECNET.
	// Attempt to guess where it is really going, and then
	// hope for the best.  Haven't yet figured out how to test
	// devices across networks.
	//
//	if (sys_status == 2288)
//	{
//		//
//		// If name ends with a colon, assume it is a printer
//		// othwewise assume it is a file.
//		//
//		if (basic::right(temp, temp.size()) == ":")
//		{
//			ftype = OUTP_TODEVICE;
//		}
//		else
//		{
//			ftype = OUTP_TOFILE;
//		}
//		goto L_6420;
//	}
//	//
//	// Invalid type?
//	//
//	if ((sys_status & 1) == 0)
//	{
//		entr_3message(scope, std::string("Invalid output device: ") + temp + " (" + std::to_string(sys_status) + ")", 0);
//		goto exitsub;
//	}
//	// ** Converted from a select statement **
//	if (devchar == dc$_disk)
//	{
//		ftype = OUTP_TOFILE;
//	}
//	else if ((devchar == dc$_lp) || ((devchar == dc$_mailbox) || (devchar == dc$_term)))
//	{
//		ftype = OUTP_TODEVICE;
//	}
//	else
//	{
//		entr_3message(scope, std::string("Invalid output device: ") + temp + " (" + std::to_string(sys_status) + ")", 0);
//		goto exitsub;
//	}
	//
L_6420:;
	// Handle output to users keyboard
	//
	if (ftype == OUTP_TODEVICE)
	{
		//
		// Is OD equal to the terminal this process is running
		// on if so then output to terminal or local printer?
		//
		if (read_syslog(temp) == read_syslog("TT:"))
		{
			ftype = OUTP_TOLOCAL;
		}
	}
	//
	// Is it going to a printable device?
	//
	if (ftype == OUTP_TOFILE)
	{
		//
		// Force extension of .PRT
		//
		temp1 = read_syslog(temp);
		if ((temp1.find(".", 0) + 1) == 0)
		{
			if (basic::right(temp1, temp1.size()) != ":")
			{
				temp = temp1 + ".PRT";
				if (print_to == OUTP_TO2020)
				{
					temp = temp1 + ".S2020";
				}
				if (print_to == OUTP_TOPL)
				{
					temp = temp1 + ".PL";
				}
			}
		}
	}
	// ==================================================================
	// Mix PRINT.TO and FTYPE to come up with the users most probible
	// desired output type.
	// ==================================================================
	// ** Converted from a select statement **
	//
	// Display
	//
	if (print_to == OUTP_TODISPLAY)
	{
		switch (ftype)
		{
		// Display on users terminal
		case OUTP_TOLOCAL:

			goto L_6450;
			break;

		}
		//
		// Normal output
		//
	}
	else if (print_to == OUTP_TODEVICE)
	{
		print_to = ftype;
		goto L_6450;
		//
		// Printer port
		//
	}
	else if (print_to == OUTP_TOLOCAL)
	{
		goto L_6450;
		//
		// Word Processor (Must go to a file)
		//
	}
	else if (print_to == OUTP_TOWP)
	{
		if (ftype == OUTP_TOFILE)
		{
			goto L_6450;
		}
		//
		// Documentation (Must go to a file)
		//
	}
	else if (print_to == OUTP_TODOCUMENT)
	{
		if (ftype == OUTP_TOFILE)
		{
			goto L_6450;
		}
		//
		// SuperComp 2020 (Must go to a file)
		//
	}
	else if ((print_to == OUTP_TO2020) || (print_to == OUTP_TOPL))
	{
		if (ftype == OUTP_TOFILE)
		{
			goto L_6450;
		}
	}
	entr_3message(scope, std::string("Unable to parse output specification '") + output + "'", 4);
	goto exitsub;
	// ==================================================================
L_6450:;
	// Finish update if good stuff
	// ==================================================================
	// ** Converted from a select statement **
	//
	// Make sure printer is turned on
	//
	if (print_to == OUTP_TODEVICE)
	{
		//
		// See if file already exists
		//
	}
	else if ((print_to == OUTP_TOFILE) ||
		(print_to == OUTP_TOWP) ||
		(print_to == OUTP_TODOCUMENT) ||
		(print_to == OUTP_TO2020) ||
		(print_to == OUTP_TOPL))
	{
		//
		// Random access File structured device?
		//
		if (find_fileexists(temp, 0) != 0)
		{
			goto L_6480;
		}
		opt7 = 0;
L_6472:;
		help_34message(scope, temp + " file already exists", "W", scope.prg_program, "", "FILEX");
		// ++
		// Warning:FILEX
		//	^*File Already Exists\*
		//	.b
		//	.lm +5
		//	^*Explanation\*
		//	.b
		//	Selected destination file already exists.
		//	.b
		//	^*User Action\*
		//	.b
		//	Select a new file name or append more text to the existing file
		//	or create a higher version of the file.
		//	.lm -5
		//
		// Index:
		//
		// --
		Junk = "Append Overwrite eXit";
		v = entr_3option(scope, "Command ", Junk, opt7, 0);
		// ** Converted from a select statement **
		if ((scope.scope_exit == SMG$K_TRM_CTRLC) ||
			(scope.scope_exit == SMG$K_TRM_F10) ||
			(scope.scope_exit == SMG$K_TRM_CTRLZ))
		{
			goto exitsub;
		}
		else if ((scope.scope_exit == 0) ||
			(scope.scope_exit == 10) ||
			(scope.scope_exit == 12) ||
			(scope.scope_exit == 13) ||
			(scope.scope_exit == SMG$K_TRM_DO))
		{
		}
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto L_6472;
		}
		// ** Converted from a select statement **
		//
		// Overwrite
		//
		if (v == "O")
		{
			// ++
			// Abstract:OVERWRITE
			//	^*Overwrite File\*
			//	.b
			//	.lm +5
			//	Creates higher version of the file.
			//	.b
			//	^*Note:\* Overwrite doesn't delete or purge files.
			//	.lm -5
			//
			// Index:
			//
			// --
L_6475:;
			unlink(temp.c_str());
			goto L_6475;
			//
			// Append
			//
		}
		else if (v == "A")
		{
			// ++
			// Abstract:APPEND
			//	^*Append to the File\*
			//	.p
			//	Report lines will be added at the end of the existing file.
			// --
			//
			// Exit
			//
		}
		else if (v == "X")
		{
			goto exitsub;
			//
			// Bad input
			//
		}
		else
		{
			entr_3message(scope, "Bad input, try again", 0);
			goto L_6472;
		}
	}
	//
L_6480:;
	// Good value, so keep it
	//
	utl_reportx.printto = print_to;
	utl_reportx.defout = temp;
exitsub:;
L_6490:;
	BReturn;

	//*******************************************************************
L_9000:;
	// List options for printer commands
	//******************************************************************
	try
	{
		for (i9 = 0; i9 <= 32767; i9++)
		{
			printer_printer[i9] = "";
		}
	}
	catch(basic::BasicError &Be)
	{
		goto L_9010;
	}
L_9010:;
	temp_V5 = 0;
	start_loop = printx.groupp[k];
	end_loop = (printx.groupp[k + 1] & 2047) - 1;
	for (j = start_loop; j <= end_loop; j++)
	{
		if (boost::trim_right_copy(printx.item[j]) != "")
		{
			temp_V5 = temp_V5 + 1;
			printer_printer[temp_V5] = boost::trim_right_copy(printx.item[j]);
		}
	}
	if (printer_printer[2] != "")
	{
		x = entr_3choice(scope, "4;43", "13;20", printer_printer, printx.deflt[k], 26, boost::trim_right_copy(printx.descr[k]).substr(0, 18), "", 0);
		if (x > -1)
		{
			printx.deflt[k] = printer_printer[x];
		}
	}
	else
	{
		if (printer_printer[1] != "")
		{
			if (printer_printer[1] == "*")
			{
				entr_3message(scope, boost::trim_right_copy(printx.descr[k]) + " is user definable", 0);
			}
			else
			{
				entr_3message(scope, std::string("Only valid choice is ") + printer_printer[1], 0);
			}
		}
		else
		{
			entr_3message(scope, "No valid choices available", 0);
		}
	}
	BReturn;

	//***************************************************************
L_18200:;
	// OPEN PRINT COMMAND FILES
	//***************************************************************
	outp_initialize(utl_reportx.printtype);
	//
	// Take users defaults if possible
	//
	for (i = 0; i <= 9; i++)
	{
		if (utl_report.itemgroup[i] != "  ")
		{
			j = v_V6 = -1;
			for (k = 0; k <= printx.groups; k++)
			{
				if (utl_report.itemgroup[i] == printx.groupx[k])
				{
					j = k;
				}
			}
			if (j > -1)
			{
				v_V6 = find_3printgroupitem(printx.groupx[j], utl_report.item[i], printx);
				if (v_V6 > 0)
				{
					printx.deflt[j] = utl_report.item[i];
				}
			}
		}
	}
	BReturn;

helperror:;
	//******************************************************************
	// Help Message for an error
	//******************************************************************
	help_34message(scope, std::to_string(0) + " " + basic::ert(0), "E", "", filename, std::to_string(0));
	goto exitprogram;
notlocked:;
	//*******************************************************************
	// The record wasn't locked, so we cannot save
	//*******************************************************************
	help_34message(scope, "Record was not locked", "S", scope.prg_program, "", "NOTLCKD");
	goto L_4010;
	//***************************************************************
L_19000:;
	// E R R O R   T R A P P I N G   C O D E
	//***************************************************************
	//
	// Untrapped error
	//
	filename = "";
	goto helperror;
#endif
}
