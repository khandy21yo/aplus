1	%TITLE "Report Settings File"
	%SBTTL "UTL_MAIN_PRINT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_PRINT(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies thereof may not be provided or otherwise made
	! available to any other person.  No title to and ownership of
	! the software is hereby transferred.
	!
	! The information in this software is subject to change without
	! notice and should not be construed as a commitment by
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:0186
	!
	! Abstract:HELP
	!	.p
	!	The ^*Report Settings\* option
	!	maintains the report settings file.  This file is used
	!	in the system to store the settings for all of the
	!	reports on a user account.
	!	.p
	!	This maintenance program has three major areas.  The
	!	first area is used to define how and where the report
	!	will be printed. The second area is used for
	!	special parameters which will be passed to the report
	!	program, and the third area is used to store the
	!	default values for the printer type.
	!
	! Index:
	!	.x Table>Report Setting
	!	.x Report Setting>Table
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_PRINT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_PRINT
	!	$ DELETE UTL_MAIN_PRINT.OBJ;*
	!
	! Author:
	!
	!	08/03/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	05/30/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	05/24/89 - Kevin Handy
	!		Modified "Getmaster" to allow a starting point for
	!		the required view.
	!
	!	05/31/90 - Kevin Handy
	!		Modified entry's to force some to upper case.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/15/98 - Kevin Handy
	!		Started export/import options
	!
	!	12/22/98 - Kevin Handy
	!		Fixed a bug that could cause the program to lock
	!		up while displaying a record with a bad length on
	!		it.
	!
	!	02/26/99 - Kevin Handy
	!		Open export file in append mode instead of create.
	!
	!	03/10/99 - Kevin Handy
	!		Fixed FIND bug
	!
	!	04/09/99 - Kevin Handy
	!		Define W_PMASTER as a long instead of a float
	!
	!	11/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD UTL_REPORT
	MAP (UTL_MASTER_REPORT)	UTL_REPORT_CDD UTL_MASTER_REPORT
	MAP (UTL_REPORT_OLD) UTL_REPORT_CDD UTL_REPORT_OLD, UTL_REPORT2
	MAP (UTL_MASTER_REPORT_OLD) UTL_REPORT_CDD UTL_MASTER_REPORT_OLD, &
		UTL_MASTER_REPORT2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSTEM.HB"
	MAP (UTL_SYSTEM)	UTL_SYSTEM_CDD	UTL_SYSTEM

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_REPORT) &
		UTL_REPORT.CH%, &
		UTL_REPORT.READONLY%

	COM (CH_UTL_MASTER_REPORT) &
		UTL_MASTER_REPORT.CH%, &
		UTL_MASTER_REPORT.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	DECLARE LONG W_PMASTER

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = &
			" Report definition maintenance"
		SMG_WINDOW::NHELP = "UTL_MAIN_PRINT"
		SMG_WINDOW::HSIZE = 130%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 42%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 3%
		SMG_WINDOW::KNAME(0%) = "Report_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "System"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
		SMG_WINDOW::KNAME(2%) = "suBsystem"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 3%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%

	IF INSTR(1%, " QV", MVALUE) <= 1%
	THEN
		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW)
	END IF

700	!
	! Declare channels
	!
	IF UTL_REPORT.CH% > 0%
	THEN
		!
		! Already open, set flag to read-only if
		! was that way from last time.
		!
		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
			IF UTL_REPORT.READONLY%
		GOTO 790
	END IF

	!
	! Open main file (existing) for modification
	!
750	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.CRE"
	USE
		CONTINUE 760 IF ERR = 10%
		UTL_MAIN_PRINT = ERR
		CONTINUE 770
	END WHEN

	UTL_REPORT.READONLY% = 0%
	GOTO 790

760	!
	! If unable to open for modify, try to open
	! with read access only.
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.OPN"
	USE
		UTL_MAIN_PRINT = ERR
		CONTINUE 770
	END WHEN

	SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
	UTL_REPORT.READONLY% = -1%

	GOTO 790

770	!
	! File not able to open, so reset channel
	!
	CALL ASSG_FREECHANNEL(UTL_REPORT.CH%)

	EXIT FUNCTION

790	SMG_WINDOW::CHAN  = UTL_REPORT.CH%
	WHEN ERROR IN
		RESET #UTL_REPORT.CH%
		GET #UTL_REPORT.CH%, REGARDLESS
	USE
		CONTINUE 32767
	END WHEN

	!
	! Modifiy the command line menu
	!
	CASE OPT_OPTLIST

		MVALUE = MVALUE + " Putmaster Getmaster expOrt iMport"

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1,  2, "(01) Number", &
			2,  2, "(02) System", &
			3,  2, "(03) Subsystem", &
			4,  2, "(04) Descr", &
			5,  2, "(05) Device", &
			6,  2, "(06) Program", &
			1, 40, "(07) CanSpool", &
			2, 40, "(08) CanDisplay", &
			3, 40, "(09) CanDevice", &
			4, 40, "(10) CanFile", &
			5, 40, "(11) CanDetach", &
			6, 40, "(12) Rep. Date", &
			1, 60, "(13) Base Run", &
			2, 60, "(14) Run Freq", &
			3, 60, "(15) Last Date", &
			4, 60, "(16) Last Time", &
			1, 86, "(17) Spooler Name", &
			2, 86, "(18) Spooler Form", &
			3, 86, "(19) Report Width(# of Chars)", &
			4, 86, "(20) Printer Type", &
			5, 86, "(21) Output", &
			6, 86, "(22) Chain To", &
			9,  2, "(23)", &
			10,  2, "(24)", &
			11,  2, "(25)", &
			12,  2, "(26)", &
			13,  2, "(27)", &
			14,  2, "(28)", &
			15,  2, "(29)", &
			16,  2, "(30)", &
			17,  2, "(31)", &
			18,  2, "(32)", &
			9,100, "(33)", &
			10,100, "(34)", &
			11,100, "(35)", &
			12,100, "(36)", &
			13,100, "(37)", &
			14,100, "(38)", &
			15,100, "(39)", &
			16,100, "(40)", &
			17,100, "(41)", &
			18,100, "(42)", &
			8, 12, "Description           Type Length ", &
			8, 46, "Required  Valid String         ", &
			8, 77, "Current value                Group  Value", &
			0,  0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%
		WHILE (XPOS% <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS%, YPOS%, XSTR$
		NEXT

		SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, 7%, &
			1%, 7%, 130%)
		SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, 7%, &
			99%, 18%, 99%)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

  Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!
	!	^*(01) Number\*
	!	.P
	!	The ^*Number\* field is used to identify the report settings
	!	record.  The field will accept six (6) characters of alphanumeric
	!	data and is user defined.  It is the primary key to the
	!	settings record.
	!
	! Index:
	!--
			UTL_REPORT::REPNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;18", TEMP$, &
				UTL_REPORT::REPNUM, MFLAG OR 16%, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) System\*
	!	.P
	!	The ^*System\* name field stores the system name
	!	this report represents. The field will accept six (6) characters
	!	of alphanumeric data and is user defined. It is a secondary
	!	key in the setting record.
	!
	! Index:
	!
	!--
			UTL_REPORT::SYSTEM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;18", TEMP$, &
				UTL_REPORT::SYSTEM, MFLAG OR 16%, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_SYSTEM.ID, "V0  ") = 1%)
				THEN
					UTL_REPORT::SYSTEM = &
						UTL_SYSTEM::SYSTEM
				END IF
				GOTO Reenter
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Subsystem\*
	!	.P
	!	The ^*Subsystem\* field stores the subsystem name.
	!	This field is used to list all reports
	!	for a subsystem.  The field will accept six (6) characters of
	!	alphanumeric data and is a secondary key.
	!
	! Index:
	!
	!--
			UTL_REPORT::SUBSYS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;18", TEMP$, &
				UTL_REPORT::SUBSYS, MFLAG OR 16%, "'E", &
				MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Description\*
	!	.P
	!	The ^*Description\* field briefly describes the report.
	!	The field will accept thirty two (32) characters of alphanumeric
	!	data and is user defined.
	!	.P
	!	Example: The chart of accounts listing could be described as 'Chart
	!	List'.
	!
	! Index:
	!
	!--

			UTL_REPORT::REPDES = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;18", TEMP$, &
				UTL_REPORT::REPDES, MFLAG, &
				"'LLLLLLLLLLLLLLLLLLL", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Device\*
	!	.P
	!	The ^*Device\* field defines the device where the
	!	report program can be found. The field will accept thirty
	!	two (32) characters and must be a valid device/account name.
	!	The device can be either a physical name or a logical name.
	!	.P
	!	For example: DUA0:[CMC.UTL] or CMC:.
	!
	! Index:
	!
	!--

			UTL_REPORT::PRODEV = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;18", TEMP$, &
				UTL_REPORT::PRODEV, MFLAG OR 16%, &
				"'LLLLLLLLLLLLLLLLLLL", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Program\*
	!	.P
	!	The ^*Program\* field identifies the report program.
	!	.P
	!	The field will accept forty (40) characters and must be a valid
	!	program name.  The extension on the program name is not
	!	required.
	!	.P
	!	For example:  The chart list report program could
	!	be 'GL__RPRT__CHRT02'.
	!
	! Index:
	!	.x Program
	!
	!--
			UTL_REPORT::PRONAM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;18", TEMP$, &
				UTL_REPORT::PRONAM, MFLAG OR 16%, &
				"'LLLLLLLLLLLLLLLLLLL", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!
	!	^*(07) CanSpool\*
	!	.P
	!	The ^*CanSpool\* field notifies the system whether or not the
	!	report can be spooled.  Valid answers are either ^*yes\* or ^*no\*.
	!	No other entry will be accepted.
	!
	! Index:
	!--
			UTL_REPORT::CANSPOOL = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;56", TEMP$, &
				UTL_REPORT::CANSPOOL, MFLAG, "'E", &
				MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!
	!	^*(08) CanDisplay\*
	!	.P
	!	The ^*CanDisplay\* field notifies the system whether or not this
	!	report can be displayed on the screen.  Valid answers are ^*yes\*
	!	or ^*no\*.
	!	.P
	!	No other response is valid.
	!
	! Index:
	!--
			UTL_REPORT::CANDISP = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;56", TEMP$, &
				UTL_REPORT::CANDISP, MFLAG, "'E", &
				MVALUE)

			CASE 9%

	!++
	! Abstract:FLD009
	!
	!	^*(09) CanDevice\*
	!	.P
	!	The ^*CanDevice\* field is used to determine whether or not
	!	this report can be set to an output device.  Valid answers are
	!	^*yes\* or ^*no\*.  No other response will be accepted.
	!
	! Index:
	!--
			UTL_REPORT::CANDEV = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;56", TEMP$, &
				UTL_REPORT::CANDEV, MFLAG, "'E", &
				MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) CanFile\*
	!	.P
	!	The ^*CanFile\* field determines whether or not the
	!	report can be sent to an ASCII text file.  Valid responses are
	!	^*yes\* or ^*no\*.  No other response will be accepted.
	!
	! Index:
	!
	!--
			UTL_REPORT::CANFILE = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;56", TEMP$, &
				UTL_REPORT::CANFILE, MFLAG, "'E", &
				MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	^*(11) CanDetach\*
	!	.P
	!	The ^*CanDetach\* field determines whether or not
	!	the report can be run background.
	!	.P
	!	Valid responses are ^*yes\* or ^*no\*.  No other response
	!	will be accepted.
	!
	! Index:
	!	.x Detach
	!
	!--
			UTL_REPORT::CANDET = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;56", TEMP$, &
				UTL_REPORT::CANDET, MFLAG, "'E", &
				MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	^*(12) Report Date\*
	!	.P
	!	The ^*Report Date\* field is used to print in the header of
	!	each page user report date or user subtitle.
	!	.P
	!	Valid responses are ^*yes\* or ^*no\*.  No other response
	!	will be accepted.
	!
	! Index:
	!	.x Report Date
	!
	!--
			UTL_REPORT::REPYN = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;56", TEMP$, &
				UTL_REPORT::REPYN, MFLAG, "'E", &
				MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!
	!	^*(13) Base Run\*
	!	.P
	!	The ^*Base Run\* field is used in conjunction with the Run
	!	Frequency field (field 14) to establish unattended report
	!	processing.
	!	.P
	!	This field is a date and the format is ^*MMDDYY\* or
	!	^*MMDDYYYY\*.  This option is scheduled for a later release.
	!
	! Index:
	!--
			UTL_REPORT::BASERUNDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;75", TEMP$, &
				UTL_REPORT::BASERUNDATE, MFLAG, &
				"'E", MVALUE)

			CASE 14%

	!++
	! Abstract:FLD014
	!
	!	^*(14) Run Freq\*
	!	.p
	!	The ^*Run Frequency\* field is used in conjunction with the
	!	base run date field (field 13) to set up unattended processing
	!	of reports. This  option will be installed in a later release.
	!
	! Index:
	!--
			UTL_REPORT::RUNFREQ = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;75", TEMP$, &
				UTL_REPORT::RUNFREQ, MFLAG, "'E", &
				MVALUE)

		CASE 15%

	!++
	!	Abstract:FLD015
	!
	!	^*(15) Last Date\*
	!	.P
	!	The ^*Last Date\* run field stores the last date that a report
	!	was run.  This field is a date and must have the following
	!	format ^*MMDDYY\* or ^*MMDDYYYY\*.
	!
	! Index:
	!--
			UTL_REPORT::LASTRUNDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;75", TEMP$, &
				UTL_REPORT::LASTRUNDATE, MFLAG, &
				"'E", MVALUE)

			CASE 16%

	!++
	! Abstract:FLD016
	!
	!	^*(16) Last Time\*
	!	.P
	!	The ^*Last Time\* run field stores the last time this report
	!	ran.  This field is a time entry and must be entered in the
	!	following format ^*HHMMSS\* or ^*HHMM\*.
	!
	! Index:
	!--
			UTL_REPORT::LASTRUNTIME = ENTR_3TIME(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;75", TEMP$, &
				UTL_REPORT::LASTRUNTIME, MFLAG, &
				"'E", MVALUE)

		CASE 17%

	!++
	! Abstract:FLD017
	!	^*(17) Spooler Name\*
	!	.P
	!	The ^*Spooler Name\* field stores the name of the spooler
	!	the report was sent to.  This is a default value, so the next
	!	time a report is processed, this spooler name will appear in the
	!	spooler field.  This field will accept thirty-two (32) characters,
	!	and must be a valid spooler name.
	!
	! Index:
	!
	!--
			UTL_REPORT::SPOOL = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;104", TEMP$, &
				UTL_REPORT::SPOOL, MFLAG OR 16%, &
				"'LLLLLLLLLLLLLLLLLLLLLLLLL", MVALUE)

		CASE 18%

	!++
	! Abstract:FLD018
	!	^*(18) Spooler Form\*
	!	.p
	!	The ^*Spooler form\* field stores the form that the
	!	Spooler selects.
	!
	! Index:
	!	.x Spooler>Form
	!	.x Form>Spooler
	!
	!--
			UTL_REPORT::SPOOLFORM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;104", TEMP$, &
				UTL_REPORT::SPOOLFORM, MFLAG OR 16%, &
				"'E", MVALUE)

			CASE 19%

	!++
	! Abstract:FLD019
	!	^*(19) Report Width\*
	!	.p
	!	The ^*Report Width\* field indicates the _# of
	!	Characters which are to appear on each line.  The field is
	!	usually 80 or 132 characters.
	!
	! Index:
	!	.x Report Width
	!
	!--
			UTL_REPORT::REPWID = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;121", TEMP$, &
				UTL_REPORT::REPWID * 1.0, MFLAG, &
				"###", MVALUE)

			CASE 20%

	!++
	! Abstract:FLD020
	!
	!	^*(20) Printer Type\*
	!	.P
	!	The ^*Printer Type\* defines the type of printer to be
	!	utilized This field is actually a field that has
	!	the definitions of a printer stored in it.  This field can
	!	accommodate six (6) characters.
	!
	! Index:
	!--
			UTL_REPORT::PRINTTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;104", TEMP$, &
				UTL_REPORT::PRINTTYPE, MFLAG OR 16%, "'E", &
				MVALUE)

			CASE 21%

	!++
	! Abstract:FLD021
	!
	!	^*(21) Output\*
	!	.P
	!	The ^*Output\* field stores the last device or file name that
	!	this report was output to.  This field will accept twenty (20)
	!	characters, and must be a valid device or file name.
	!
	! Index:
	!--
			UTL_REPORT::DEFOUT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;104", TEMP$, &
				UTL_REPORT::DEFOUT, MFLAG OR 16%, "'E", &
				MVALUE)

		CASE 22%

	!++
	! Abstract:FLD022
	!
	!	^*(22) Chain To\*
	!	.P
	!	This field specifies a program that is to be chained to
	!	after the report has finished.
	!
	! Index:
	!--
			UTL_REPORT::CHAINTO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;104", TEMP$, &
				UTL_REPORT::CHAINTO, MFLAG OR 16%, "'E", &
				MVALUE)

		CASE 23% TO 32%

	!++
	! Abstract:FLD023
	!	^*(23-32) Special Report Parameters\*
	!	.P
	!	The ^*Special Report Parameters\* are used to pass user data to
	!	the report program. For example, these fields could be used
	!	for a starting and ending chart number in the chart list report.
	!	There are 10 special report parameter fields and within each
	!	of the special report parameters are six elements.
	!	Those elements are as follows:
	!	.BLANK 1
	!	.LM +5
	!	.LIST 0,"*"
	!	.LE
	!	Description - 20 characters
	!	.LE
	!	Type - 1 character
	!	.LE
	!	Length - A number between 1 and 20
	!	.LE
	!	Required - Y/N
	!	.LE
	!	Valid string - 20 characters
	!	.LE
	!	Current value - 20 characters
	!	.ELS
	!	.LEFT MARGIN -5
	!	.P
	!	The description field is used to prompt the user in the
	!	report setting screen.
	!	.P
	!	The type is used to identify the type of data which will be
	!	passed to the setting screen. Valid types are:
	!	.BLANK 1
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	$:String value
	!	.LE
	!	S:Upper case string.
	!	.le
	!	F:Floating point number
	!	.le
	!	I:Integer
	!	.le
	!	D:Date
	!	.le
	!	Y:Yes/No
	!	.le
	!	P:Accounting Period
	!	.els
	!	.lm -10
	!	.p
	!	The "length" defines how long the data can be that will be
	!	requested in the setting screen.
	!	.P
	!	The "required" defines whether or not the user must enter
	!	this field in the setting screen each time the report
	!	is processed.
	!	.P
	!	The "valid string" is used to determine valid characters that
	!	can be entered in the setting screen.
	!	.P
	!	The "current value" is the value that was entered the last time
	!	a setting was entered into the setting screen.
	!
	! Index:
	!
	!--
			ARY% = MLOOP - 23%
			ARY$ = NUM1$(ARY% + 9%) + ";"
			SCOPE::SCOPE_EXIT = 0%

			UTL_REPORT::DESCR(ARY%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, ARY$ + "12", &
				TEMP$, UTL_REPORT::DESCR(ARY%), &
				MFLAG, "'E", MVALUE)
			GOTO 20290 IF (SCOPE::SCOPE_EXIT = 3%) OR &
				(SCOPE::SCOPE_EXIT > 255%)

			UTL_REPORT::OPTTYPE(ARY%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, ARY$ + "36", &
				TEMP$, UTL_REPORT::OPTTYPE(ARY%), &
				MFLAG, "'E", MVALUE)
			GOTO 20290 IF (SCOPE::SCOPE_EXIT = 3%) OR &
				(SCOPE::SCOPE_EXIT > 255%)

 Loop_OptLen:		UTL_REPORT::OPTLEN(ARY%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, ARY$ + "41", &
				TEMP$, UTL_REPORT::OPTLEN(ARY%) * 1.0, &
				MFLAG, "##", MVALUE)
			GOTO 20290 IF (SCOPE::SCOPE_EXIT = 3%) OR &
				(SCOPE::SCOPE_EXIT > 255%)
			IF UTL_REPORT::OPTTYPE(ARY%) = "D" AND &
				(UTL_REPORT::OPTLEN(ARY%) <> 6% AND &
				UTL_REPORT::OPTLEN(ARY%) <> 8%) AND &
				(MFLAG AND 1%) = 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"A Date must be 6 or 8 in length UTL_REPORT", 1%)
				GOTO Loop_OptLen
			END IF
			CALL ENTR_3MESSAGE(SCOPE,  "", 1%)

			UTL_REPORT::REQUIRE(ARY%) = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, ARY$ + "48", &
				TEMP$, UTL_REPORT::REQUIRE(ARY%), &
				MFLAG, "'E", MVALUE)
			GOTO 20290 IF (SCOPE::SCOPE_EXIT = 3%) OR &
				(SCOPE::SCOPE_EXIT > 255%)

			UTL_REPORT::VALID(ARY%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, ARY$ + "56", &
				TEMP$, UTL_REPORT::VALID(ARY%), &
				MFLAG, "'E", MVALUE)
			GOTO 20290 IF (SCOPE::SCOPE_EXIT = 3%) OR &
				(SCOPE::SCOPE_EXIT > 255%)

			UTL_REPORT::OPTDEF(ARY%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, ARY$ + "77", &
				TEMP$, UTL_REPORT::OPTDEF(ARY%), &
				MFLAG, "'E", MVALUE)

		CASE 33% TO 42%

	!++
	! Abstract:FLD033
	!
	!	.P
	!	The ^*Print Default Setting\* stores the default value from the
	!	last time a report was run.  There are two elements to each setting,
	!	Group and Value.  The group is a two (2) character field and
	!	the value is the value of the group identifier.
	!
	! Index:
	!	.x Default Settings
	!--
			ARY% = MLOOP - 33%
			ARY$ = NUM1$(ARY% + 9%) + ";"

			UTL_REPORT::ITEMGROUP(ARY%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, ARY$ + "107", &
				TEMP$, UTL_REPORT::ITEMGROUP(ARY%), &
				MFLAG, "'E", MVALUE)
			GOTO 20290 IF (SCOPE::SCOPE_EXIT = 3%) OR &
				(SCOPE::SCOPE_EXIT > 255%)

			UTL_REPORT::ITEM(ARY%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, ARY$ + "113", &
				TEMP$, UTL_REPORT::ITEM(ARY%), &
				MFLAG, "'E", MVALUE)

		END SELECT

20290	SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
	UTL_MAIN_PRINT = 0%

	!
	! Set UTL_REPORT_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_REPORT_OLD = UTL_REPORT

	!
	! Restore UTL_REPORT_OLD value
	!
	CASE OPT_RESETOLD
		UTL_REPORT = UTL_REPORT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_REPORT2 = UTL_REPORT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_REPORT = UTL_REPORT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Report System Subsys " + &
				"Description                    " + &
				"Device                           " + &
				"Program"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009,016,023,054,087"

		!
		! Convert current record into text
		!
		CASE 3%
			IF SMG_WINDOW::CHAN <> UTL_MASTER_REPORT.CH%
			THEN
				MVALUE = &
					UTL_REPORT::REPNUM + " " + &
					UTL_REPORT::SYSTEM + " " + &
					UTL_REPORT::SUBSYS + " " + &
					UTL_REPORT::REPDES + " " + &
					UTL_REPORT::PRODEV + " " + &
					UTL_REPORT::PRONAM
			ELSE
				MVALUE = &
					UTL_MASTER_REPORT::REPNUM + " " + &
					UTL_MASTER_REPORT::SYSTEM + " " + &
					UTL_MASTER_REPORT::SUBSYS + " " + &
					UTL_MASTER_REPORT::REPDES + " " + &
					UTL_MASTER_REPORT::PRODEV + " " + &
					UTL_MASTER_REPORT::PRONAM
			END IF

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_REPORT.CH%, &
				KEY #0% GE UTL_REPORT::REPNUM + "", &
				REGARDLESS

		CASE 1%
			FIND #UTL_REPORT.CH%, KEY #1% &
				GE UTL_REPORT::SYSTEM + &
				UTL_REPORT::REPNUM, &
				REGARDLESS

		CASE 2%
			FIND #UTL_REPORT.CH%, KEY #2% &
				GE UTL_REPORT::SUBSYS + &
				UTL_REPORT::REPNUM, &
				REGARDLESS

		END SELECT

	!
	! More menu options
	!
	CASE OPT_MOREMENU

		SELECT MVALUE
		CASE "Putmaster"

			!
			! Get info required for main file
			!
			GOTO 20602 IF UTL_MASTER_REPORT.CH% > 0%

			!
			! Open main file (existing) for modification
			!
20600			%INCLUDE "SOURCE:[UTL.OPEN]UTL_MASTER_REPORT.CRE"

20602			UTL_MASTER_REPORT = UTL_REPORT

			WHEN ERROR IN
				PUT #UTL_MASTER_REPORT.CH%
			USE
				SELECT ERR
				CASE 134%, 153%	! Dup key deteced, Record already exists
					CONTINUE 20625
				END SELECT
				EXIT HANDLER
			END WHEN

			CALL ENTR_3MESSAGE(SCOPE,  "Put in Master Completed!!!", 1%)

		CASE "Getmaster"

20610			TEMP1$ = TRM$(SCOPE::PRG_ITEM)
			TEMP.CH% = UTL_REPORT.CH%

			GOTO 20622 IF UTL_MASTER_REPORT.CH% > 0%

			!
			! Open main file (existing) for modification
			!
20620			%INCLUDE "SOURCE:[UTL.OPEN]UTL_MASTER_REPORT.CRE"

20622			SMG_WINDOW::CHAN, UTL_REPORT.CH% = UTL_MASTER_REPORT.CH%

			V% = MAIN_WINDOW(UTL_MAIN_PRINT.ID, "VX")

			SMG_WINDOW::CHAN, UTL_REPORT.CH% = TEMP.CH%

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_SELECT
			THEN
				UTL_REPORT = UTL_MASTER_REPORT
				PUT #UTL_REPORT.CH%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Get from Master - Completed", 1%)
			END IF

			SCOPE::SCOPE_EXIT = 0%
			UTL_MAIN_PRINT = 0%
			SCOPE::PRG_ITEM = TEMP1$

		case "expOrt"
			GOSUB DoExport
			UTL_MAIN_PRINT = 0%

		case "iMport"
			GOSUB DoImport
			UTL_MAIN_PRINT = 16%

		END SELECT
		GOTO ExitFunction

20625		WHEN ERROR IN
			GET #UTL_MASTER_REPORT.CH%, &
				KEY #0% EQ UTL_REPORT::REPNUM + ""
		USE
			IF ERR = 155%	! Record not found
			THEN
				CALL ENTR_3MESSAGE(SCOPE, "Record not found!!!", 0%)
				CONTINUE ExitFunction
			END IF

			EXIT HANDLER
		END WHEN

		!
		! Create display
		!
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(7%, &
			52%, W_PMASTER,, SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(W_PMASTER, &
			"        CMC:UTL_REPORT RECORD TO BE UPDATED" + &
			CR + LF + " Number"	+ CR + LF + &
			" System"	+ CR + LF + &
			" Subsystem"	+ CR + LF + &
			" Descr"		+ CR + LF + &
			" Device"	+ CR + LF + &
			" Program", 1%, 1%)
		SMG_STATUS% = SMG$PUT_CHARS(W_PMASTER, &
			UTL_MASTER_REPORT::REPNUM, 2%, 12%,, &
			SMG$M_BOLD)
		SMG_STATUS% = SMG$PUT_CHARS(W_PMASTER, &
			UTL_MASTER_REPORT::SYSTEM, 3%, 12%,, &
			SMG$M_BOLD)
		SMG_STATUS% = SMG$PUT_CHARS(W_PMASTER, &
			UTL_MASTER_REPORT::SUBSYS, 4%, 12%,, &
			SMG$M_BOLD)
		SMG_STATUS% = SMG$PUT_CHARS(W_PMASTER, &
			UTL_MASTER_REPORT::REPDES, 5%, 12%,, &
			SMG$M_BOLD)
		SMG_STATUS% = SMG$PUT_CHARS(W_PMASTER, &
			UTL_MASTER_REPORT::PRODEV, 6%, 12%,, &
			SMG$M_BOLD)
		SMG_STATUS% = SMG$PUT_CHARS(W_PMASTER, &
			UTL_MASTER_REPORT::PRONAM, 7%, 12%,, &
			SMG$M_BOLD)

		!
		! Paste on display
		!
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(W_PMASTER, &
			SCOPE::SMG_PBID, 1%, 41%)

20630		INP$ = ENTR_3YESNO(SCOPE,  SMG_WINDOW::WNUMBER, "", &
			"Key already exists in master - update it (Y/N) ?", &
			"N", 4%, "!", "")

		SELECT SCOPE::SCOPE_EXIT
		!
		! Exit keys
		!
		CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			CALL ENTR_3MESSAGE(SCOPE, "Master Add aborted", 1%)

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
			! Good key

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 20630
		END SELECT

		SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(W_PMASTER, &
			SCOPE::SMG_PBID)

		IF INP$ = "Y"
		THEN
			UTL_MASTER_REPORT = UTL_REPORT
			UPDATE #UTL_MASTER_REPORT.CH%
			CALL ENTR_3MESSAGE(SCOPE,  "Master - Updated!!!", 1%)
		END IF

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

22000	!*******************************************************************
	! Do the work of exporting
	!

 DoExport:
	!
	! Ask for output file name
	!
	OUTNAME$ = SPACE$(40%)
	OUTNAME$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
		"", "File to write to", &
		OUTNAME$, 4%, "'E", "")

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Control/C
	!
	CASE 3%
		GOTO ExitFunction

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitFunction

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, &
		SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO DoExport

	END SELECT

	GOTO ExitFunction IF OUTNAME$ = ""

	!
	! Create the output file
	!
	CALL ASSG_CHANNEL(EXPORT.CH%, STAT%)
	OPEN OUTNAME$ AS FILE EXPORT.CH%, &
		ACCESS APPEND

	!
	! Do the export
	!
	CALL UTL_FUNC_EXPORTREPORT(EXPORT.CH%, UTL_REPORT)

	!
	! Clean up
	!
	CLOSE #EXPORT.CH%
	CALL ASSG_FREECHANNEL(EXPORT.CH%)

	RETURN

	%PAGE

 DoImport:
	!*******************************************************************
	! Import data
	!
	!
	! Ask for output file name
	!
	OUTNAME$ = SPACE$(40%)
	OUTNAME$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
		"", "File to Read From", &
		OUTNAME$, 4%, "'E", "")

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Control/C
	!
	CASE 3%
		GOTO ExitFunction

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitFunction

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, &
		SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO DoExport

	END SELECT

	GOTO ExitFunction IF OUTNAME$ = ""

	!
	! Create the output file
	!
	CALL ASSG_CHANNEL(EXPORT.CH%, STAT%)
	OPEN OUTNAME$ FOR INPUT AS FILE EXPORT.CH%

	!
	! Do the export
	!
	CALL UTL_FUNC_IMPORTREPORT(EXPORT.CH%, UTL_REPORT.CH%)

	!
	! Clean up
	!
	CLOSE #EXPORT.CH%
	CALL ASSG_FREECHANNEL(EXPORT.CH%)

	RETURN

29000	!
	! Trap Errors
	!
	RESUME ExitFunction

32767	END FUNCTION
