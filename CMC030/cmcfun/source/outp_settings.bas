1	%TITLE "Report Settings"
	%SBTTL "OUTP_SETTINGS"
	%IDENT "V3.6a Calico"

	SUB OUTP_SETTINGS(UTL_REPORT_CDD UTL_REPORT, &
		UTL_REPORTX_CDD UTL_REPORTX, WORD UPDATE.CH, &
		STRING LEFT_SIDE_CMD, STRING RIGHT_SIDE_CMD)

	!
	! COPYRIGHT (C) 1986 BY
	! Computer Management Center
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Report Settings\* enables the user to setup input for reports, posting
	!	processes, and forms.
	!	.lm -5
	!
	! Index:
	!	.x Report Screen
	!	.x Report Settings
	!	.x Settings>Report
	!
	! Option:
	!	OUTP_SETTINGS$FLD*
	!
	! Parameters:
	!
	!	LEFT_SIDE_CMD
	!		The passed left side commands.
	!
	!	Left side commands can be:
	!	.table
	!		DD - Document Destination
	!		SF - Spooler Form Name
	!		SP - Start Page
	!		EP - End Page
	!		CP - Copies
	!		AS - Autoscroll
	!		RD - Report Date
	!	.endtable
	!
	!	RIGHT_SIDE_CMD
	!		The passed right side commands.
	!
	!	Right side commands can be:
	!		PT - Printer Type
	!
	!	PRINT.TO% is used to decide where the output is going.
	!
	!	.table
	!		1%  = Display
	!
	!		2%  = Spool
	!
	!		3%  = Output to a Device
	!
	!		4%  = Output to a File
	!
	!		5%  = Output to Local Printer
	!
	!		7%  = Output to Word Processor
	!
	!		9%  = Output to Documentation Library
	!
	!		10% = Output to S2020 Spreadsheet
	!
	!		11% = Output to PlanPerfect Spreadsheet
	!	.endtable
	!
	!	UTL_REPORTX
	!		The definition of the file to be changed.
	!
	!	UPDATE.CH
	!		The channel the file will be updated on.
	!
	!	UTL_REPORT
	!		The revised report settings.
	!
	!	This subroutine changes the report settings to fit the user's
	!	choice.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_SETTINGS/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_SETTINGS
	!	$ DELETE OUTP_SETTINGS.OBJ;*
	!
	! Author:
	!
	!	05/05/87 - Kevin Handy
	!
	! Modification history:
	!
	!	08/07/87 - Kevin Handy
	!		Fix date entry.  Modified to use SMG_* versions
	!		of the entry functions.
	!
	!	08/24/87 - Kevin Handy
	!		Modified to pull printer type (PT) information
	!		out of a library file.
	!
	!	10/19/87 - Kevin Handy
	!		Added output to clipboard, word processor, and
	!		compact disk.
	!
	!	11/12/87 - Kevin Handy
	!		Added documentation stuff.
	!
	!	06/03/88 - Kevin Handy
	!		Modified to work through DECNET.
	!
	!	07/29/88 - Kevin Handy
	!		Make less sensitive to external influences
	!		by forcing it to create the UTL_REPORTX::WINDOW
	!		each time we enter here.  Still assumes that
	!		calling program will delete it when done with it.
	!
	!	11/13/89 - Kevin Handy
	!		Modified to output to PlanPerfect Spreadsheet.
	!
	!	02/23/90 - Frank F. Starman
	!		Change key for help message.
	!
	!	07/20/90 - Frank F. Starman
	!		For documentation force printer type and only the
	!		first page.
	!
	!	09/28/90 - Kevin Handy
	!		Disabled setting the "LAST RUN DATE" and "LAST RUN TIME"
	!		fields.  Nobody ever used them.
	!		This saves an update to the file (reduce disk access).
	!
	!	05/29/91 - Kevin Handy
	!		Created new data type "S" which enters an upper
	!		case string (instead of "$" which is any case).
	!
	!	08/20/91 - Kevin Handy
	!		Modified "Overwrite" so that it will delete multiple
	!		versions of a file, instead of Franks "call the error
	!		message hundreds of times" preference.  It only served
	!		to confuse everyone.
	!
	!	12/23/91 - Kevin Handy
	!		Removed DOC_DEST stuff (which nobody seemed to use)
	!		to greatly reduce size of executables calling
	!		OUTP_SETTINGS.
	!
	!	04/08/92 - Frank Starman
	!		Don't paint screen or stop if it is FORM_DIRECT.
	!
	!	04/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/14/92 - Kevin Handy
	!		Attempt to fix problem of someone (who doesn't like
	!		to put edit histories in any programs) changed all
	!		'#'s in program to tabs. (Who knows why)
	!
	!	09/18/92 - Kevin Handy
	!		More tab problems.
	!
	!	10/01/92 - Kevin Handy
	!		Found more #'s changed to tabs.
	!
	!	10/24/92 - Frank Starman
	!		Added option Wildcard.
	!
	!	03/16/93 - Kevin Handy
	!		Modifications to handle times when record was not
	!		locked out by the main program (due to another
	!		user locking it out).
	!
	!	03/17/93 - Kevin Handy
	!		Assume report width of 132 if none given.  Makes
	!		the displayed values more reasonable.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/22/93 - Kevin Handy
	!		Disable deviceID checking, because it really
	!		isn't working correctly right now.
	!
	!	06/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/03/94 - Kevin Handy
	!		Added data type 'd', which fills in with the
	!		current date.
	!
	!	03/13/95 - Kevin Handy
	!		(V3.6)
	!		Removed commented out code.
	!
	!	03/13/95 - Kevin Handy
	!		Added new input fields: (AF) After Time,
	!		(BG) Background, (OF) Offset.
	!
	!	03/13/95 - Kevin Handy
	!		Modified PaintReport routine to re-use EnterTwo
	!		instead of hard coding the data positions.
	!
	!	03/13/95 - Kevin Handy
	!		Modified to put RD in the option string only if
	!		REPYN is set to Yes.
	!
	!	04/13/95 - Kevin Handy
	!		Update to V3.6 coding standards.
	!		Change last parameter to ENTER_3CHOICE from "" to 0%.
	!
	!	04/14/95 - Kevin Handy
	!		Change parameters on entr_3time again.
	!
	!	12/15/95 - Kevin Handy
	!		Remove commented out code.
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	03/05/97 - Kevin Handy
	!		Force upper case input in several fields
	!		(destination, printer type, spool form)
	!		Reformat source closer to 80 columns.
	!
	!	09/16/97 - Kevin Handy
	!		Lose code for clipboard, which isn't usable.
	!		Much better to use an X terminal.
	!		Lose code for File Cabinet, which didn't do
	!		anything usefull.
	!
	!	11/11/97 - Kevin Handy
	!		Use constants instead of hard coded numbers
	!		in comparison to PRINTTO
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$, DC$ routines
	!
	!	04/28/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$DVIDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$DCDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"
	MAP (PRINTX) PRINTX_CDD PRINTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION FIND_3PRINTGROUPITEM
	EXTERNAL STRING	FUNCTION LIBR_SELECT
	EXTERNAL LONG FUNCTION EDT$EDIT

	%PAGE

	!
	! Declare variables
	!
	DECLARE LONG DEVCHAR, XPOS, YPOS, SYS_STATUS
	DECLARE LONG OUTPUT.CH

	!
	! Dimension statements
	!
	DIM PRINT_LINE%(PRINT.MAXGRP)
	DIM PRINTER_PRINTER$(200%)
	DIM XTYPE$(20%), OTYPE%(30%), OTYPE$(30%)
	DIM RDATA1$(100%, 13%), RDATA2$(100%)

	%PAGE

	ON ERROR GOTO 19000

	SMG_STATUS% = LIB$GET_LUN(OUTPUT.CH)

	REPORT_KEEP_ITEM$ = SCOPE::PRG_ITEM
	REPORT_KEEP_IDENT$ = SCOPE::PRG_IDENT
	REPORT_KEEP_PRG$ = SCOPE::PRG_PROGRAM

	SCOPE::PRG_PROGRAM = "OUTP_SETTINGS"

	!
	! Skip paint if FORM_DIRECT
	!
	IF UTL_REPORTX::SPOOL = "DIRECT"
	THEN
		SKIP% = -1%
	ELSE
		SKIP% = 0%

		!
		! Create new window
		!
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, &
			UTL_REPORTX::WINDOW, SMG$M_BORDER)

		SMG_STATUS% = SMG$LABEL_BORDER(UTL_REPORTX::WINDOW, &
			TRM$(UTL_REPORT::REPNUM) + &
			" - " + TRM$(UTL_REPORT::REPDES))

	END IF

	!
	! Force values for auto-fill fields
	!
	FOR I% = 0% TO 9%

		SELECT UTL_REPORT::OPTTYPE(I%)

		CASE "d"
			UTL_REPORTX::OPTDEF(I%) = PRNT_DATE(DATE_TODAY, 8%)

		END SELECT

	NEXT I%

	!
	! Determine options for report settings
	!
	IF TRM$(LEFT_SIDE_CMD) = ""
	THEN
		LEFT_SIDE_CMD = "DD SF SP EP CP AS AF BG OF "
		LEFT_SIDE_CMD = LEFT_SIDE_CMD + "RD " &
			IF UTL_REPORTX::REPYN <> "N"
	END IF

	RIGHT_SIDE_CMD = "PT " &
		IF TRM$(RIGHT_SIDE_CMD) = ""

	RESTORE

	DATA	1, 1, "(DD) Destination", &
		2, 1, "(SF) Spooler Form", &
		3, 6, "Paper Width        Inches", &
		4, 6, "Paper Length       Inches", &
		5, 1, "(SP) Start Page", &
		6, 1, "(EP) End Page", &
		7, 1, "(CP) Copies", &
		8, 1, "(AS) Autoscroll", &
		9, 1, "(AF) After Time", &
		10, 1, "(BG) Background", &
		11, 1, "(OF) Offset", &
		12, 1, "(RD) Report Date", &
		1, 41, "(PT) Printer Type", &
		0, 0, ""

	READ XPOS, YPOS, XSTR$

	WHILE XPOS

		IF INSTR(1%, LEFT_SIDE_CMD + &
			RIGHT_SIDE_CMD, MID(XSTR$, 2%, 2%)) &
			OR LEFT$(XSTR$, 1%) + MID(XSTR$, 4%, 1%) <> "()"
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(UTL_REPORTX::WINDOW, &
				XSTR$, XPOS, YPOS)
		END IF

		READ XPOS, YPOS, XSTR$

	NEXT


32	!
	! Various output types that can be displayed (0	5)
	!
	DATA	"", &
		"Display", &
		"Spooler", &
		"", &
		"", &
		"Printer Port", &
		"", &
		"Word Processing", &
		"", &
		"Documentation", &
		"2020", &
		"Plan Perfect"

	READ XTYPE$(I%) FOR I% = 0% TO 11%

	!
	! Various output types that can be entered
	!
34	DATA	1,	"DP", &
		1,	"DISPLAY", &
		2,	"SPOOL", &
		2,	"SPOOLER", &
		2,	"SP", &
		5,	"PP", &
		5,	"PRINTER PORT", &
		7,	"WP", &
		7,	"WORD PROCESSING", &
		9,	"DOCUMENT", &
		9,	"DOCUMENTATION", &
		10,	"2020", &
		10,	"S2020", &
		11,	"PL", &
		0,	""

	OCOUNT% = 0%
	READ OTYPE%, OTYPE$

	WHILE OTYPE%
		OCOUNT% = OCOUNT% + 1%
		OTYPE%(OCOUNT%) = OTYPE%
		OTYPE$(OCOUNT%) = OTYPE$
		READ OTYPE%, OTYPE$
	NEXT

	!
	! Force information about output device
	!
	OUTPUT$ = TRM$(UTL_REPORTX::DEFOUT) + " " + &
		XTYPE$(UTL_REPORTX::PRINTTO)
	GOSUB 6400

	!
	! Do not ask for settings if FORM_DIRECT
	!
	IF SKIP%
	THEN
		GOSUB 18200
		GOTO 4150
	ELSE
		GOSUB PaintAll

		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(UTL_REPORTX::WINDOW, &
			SCOPE::SMG_PBID, 2%, 2%)
	END IF

4000	!***************************************************************
	! Repaint screen...
	!***************************************************************

4005	!
	! Display current settings
	!
	GOSUB PaintAll

4010	!
	! Handle users options
	!

	SCOPE::PRG_ITEM = "HELP"
	SCOPE::PRG_IDENT = "H"
	SCOPE::PRG_PROGRAM = TRM$(UTL_REPORT::PRONAM)

	REQITM$ = ""

	OPTION$ = "Change Blank Store Go Wildcard Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTION$, OPT3%, 0%)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		UTL_REPORTX::PRINTTO = 0%
		GOTO 4190

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 4010

	END SELECT

 CheckOption:
	SELECT OPT$
	!
	! Change
	!
	CASE "C"

4040		IF REQITM$<>""
		THEN
			COMMA% = INSTR(1%, REQITM$ + ",", ",")
			ITM$ = LEFT(REQITM$, COMMA% - 1%)
			REQITM$ = RIGHT(REQITM$, COMMA% + 1%)
			GOTO Changee
		END IF

		ITM$ = EDIT$(ENTR_3STRING(SCOPE, UTL_REPORTX::WINDOW, &
			"", "Change", "  ", 4%, "", ""), -1%)
		ITM$ = "0" + ITM$ IF LEN(ITM$) < 2% AND ITM$ <> ""

		SELECT SCOPE::SCOPE_EXIT

		CASE 3%, 290%
			GOTO 4010

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 4010

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 4040

		END SELECT

		GOTO 4010 IF ITM$ = ""

 Changee:
		GOSUB EnterOne

		WORK_CMD$ = LEFT_SIDE_CMD

		WORK_CMD$ = WORK_CMD$ + FORMAT$(I% + 1%, "<0>#") + " " &
			IF UTL_REPORT::OPTLEN(I%) AND &
			EDIT$(UTL_REPORT::DESCR(I%), -1%) <> "" &
			FOR I% = 0% TO 4%
		WORK_CMD$ = WORK_CMD$ + RIGHT_SIDE_CMD
		WORK_CMD$ = WORK_CMD$ + PRINTX::GROUPX(PRINT_LINE%(I%)) + " " &
			FOR I% = 1% TO Q.I%
		WORK_CMD$ = WORK_CMD$ + FORMAT$(I% + 1%, "<0>#") + " " &
			IF UTL_REPORT::OPTLEN(I%) AND &
			EDIT$(UTL_REPORT::DESCR(I%), -1%) <> "" &
			FOR I% = 5% TO 9%

		LOOP% = INSTR(1%, WORK_CMD$, ITM$)
		GOTO 4040 IF LOOP% = 0%

		SELECT SCOPE::SCOPE_EXIT
		CASE 274%	! Uparrow
			ITM$ = MID(WORK_CMD$, LOOP% - 3%, 2%) IF LOOP% > 3%
			GOTO Changee

		CASE 275%	! Downarrow
			ITM$ = MID(WORK_CMD$, LOOP% + 3%, 2%) &
				IF LOOP% < LEN(WORK_CMD$) - 3%
			GOTO Changee

		END SELECT

		GOTO 4040

	!
	! Blank
	!
	CASE "B"
4050		ITM$ = EDIT$(ENTR_3STRING(SCOPE, UTL_REPORTX::WINDOW, &
			"", "Blank", "  ", 4%, "", ""), -1%)
		ITM$ = "0" + ITM$ IF LEN(ITM$) < 2% AND ITM$ <> ""

		SELECT SCOPE::SCOPE_EXIT

		CASE 3%, 290%
			GOTO 4010

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 4010

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 4050

		END SELECT

		GOTO 4010 IF ITM$ = ""

		LOOP% = INSTR(1%, " 01 02 03 04 05 06 07 08 09 10 ", ITM$)

		GOTO 4050 IF LOOP% = 0%

		GOSUB EnterOne

		GOTO 4050

	!
	! Help
	!
	CASE "H"
		CALL HELP_34MESSAGE(SCOPE, "", "H", SCOPE::PRG_PROGRAM, &
			"", "HELP")
		GOTO 4005

	!
	! Store Settings
	!
	CASE "S"
		!
		! Load in the changed data
		!
4060		FOR I% = 0% TO 9%
			UTL_REPORT::OPTDEF(I%) = UTL_REPORTX::OPTDEF(I%)
			UTL_REPORT::ITEMGROUP(I%) = PRINTX::GROUPX(I%)
			UTL_REPORT::ITEM(I%) = PRINTX::DEFLT(I%)
		NEXT I%
		UTL_REPORT::DEFOUT = TRM$(UTL_REPORTX::DEFOUT) + &
			" " + NUM1$(UTL_REPORTX::PRINTTO)
		UTL_REPORT::PRINTTYPE = UTL_REPORTX::PRINTTYPE
		UTL_REPORT::SPOOLFORM = UTL_REPORTX::SPOOLFORM

		WHEN ERROR IN
			UPDATE #UPDATE.CH

			GET #UPDATE.CH, KEY #0% EQ UTL_REPORT::REPNUM
		USE
			CONTINUE NotLocked IF ERR = 131%
			FILENAME$ = "UTL_REPORT"
			CONTINUE HelpError
		END WHEN

		CALL HELP_34MESSAGE(SCOPE, "settings have been stored", &
			"S", SCOPE::PRG_PROGRAM, "", "SETTUPD")
	!++
	! Success:SETTUPD
	!	^*The Settings Updated\*
	!	.b
	!	.lm +5
	!	All fields on the report setting screen has been
	!	stored (except (CP),(EP),(SP)).
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Go
	!
	CASE "G"
4070		GOTO 4150

	CASE "W"
4080		WILD.FILE$ = EDIT$(ENTR_3STRING(SCOPE, &
			UTL_REPORTX::WINDOW, "", "Wildcard Name", &
			SPACE$(20%), 4%, "", ""), -1%)

		SELECT SCOPE::SCOPE_EXIT

		CASE 3%, 290%
			GOTO 4010

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 4010

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
			GOTO 4010 IF WILD.FILE$ = ""

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 4080

		END SELECT

		ST% = EDT$EDIT(WILD.FILE$ + ".WLD",,,,,,,)

		ST% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%) ! Off

		!
		! Refresh screen
		!
		ST% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		WHEN ERROR IN
			KILL WILD.FILE$ + ";-1"
		USE
			CONTINUE 4010
		END WHEN

	!
	! eXit
	!
	CASE "X"
		SCOPE::SCOPE_EXIT = SMG$K_TRM_F10
		UTL_REPORTX::PRINTTO = 0%
		GOTO 4190

	END SELECT

	GOTO 4010

4150	!***************************************************************
	! TEMPORARY PRINT COMMAND FILE
	!***************************************************************

	!
	! Check for required values
	!
	TEMP$ = ""
	TEMP$ = TEMP$ + ", " + FORMAT$(I% + 1%, "<0>#") &
		IF EDIT$(UTL_REPORTX::OPTDEF(I%), -1%) = "" AND &
			UTL_REPORT::REQUIRE(I%) = "Y" FOR I% = 0% TO 9%
	IF TEMP$ <> ""
	THEN
		CALL HELP_34MESSAGE(SCOPE, "required to setup the field(s) " + &
			RIGHT(TEMP$, 3%), "W", SCOPE::PRG_PROGRAM, "", &
			"REQFIELD")
	!++
	! Warning:REQFIELD
	!	^*Required to Setup a field\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	It is required to assign a value for a field before
	!	running the report.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Enter a valid value to the field and run the process again.
	!	.lm -5
	!
	! Index:
	!
	!--

		!
		! Force input to required field
		!
		OPT$ = "C"
		REQITM$ = EDIT$(RIGHT(TEMP$, 3%), -1%)
		GOTO CheckOption
	END IF

	!
	! Set nodetach flag if necessary
	!
	UTL_REPORTX::DETACH = -1% IF (UTL_REPORT::CANDET = "N") OR &
		UTL_REPORTX::PRINTTO = OUTP_TODISPLAY OR &
		UTL_REPORTX::PRINTTO = OUTP_TOLOCAL

 ExitProgram:
4190	!
	! Exit from function (Leaves window there)
	!
	SMG_STATUS% = LIB$FREE_LUN(OUTPUT.CH)

	!
	! Recover the help key
	!
	SCOPE::PRG_PROGRAM = REPORT_KEEP_PRG$
	SCOPE::PRG_IDENT = REPORT_KEEP_IDENT$
	SCOPE::PRG_ITEM = REPORT_KEEP_ITEM$

	EXIT SUB

	%PAGE

6000	!***************************************************************
	! DATA ENTRY SECTION
	!***************************************************************

 EnterOne:

	IF OPT$ = "C"
	THEN
		CBFLAG% = 0%
	ELSE
		CBFLAG% = 33%
	END IF

	SCOPE::PRG_PROGRAM = "OUTP_SETTINGS"
	SCOPE::PRG_ITEM    = "FLD" + ITM$

	!STORE_PRG_ITEM$ = SCOPE::PRG_ITEM

 EnterTwo:
	SELECT ITM$

	!
	! Enter new output device
	!
	CASE "DD"

	!++
	! Abstract:FLDDD
	!	^*(DD) Destination\*
	!	.b
	!	.lm +5
	!	This field is used to specify the device which is assigned
	!	to print the report.
	!	.b
	!	Possible values are:
	!	.b
	!	.lm +5
	!	^*DISPLAY\*	Display the report on the screen.
	!	.b
	!	^*DP\*	Mnemonic for screen display.
	!	.b
	!	^*PRINTER	PORT\*	Send output to printer port.
	!	.b
	!	^*PP\*	Mnemonic for printer port.
	!	.b
	!	^*SPOOLER\*	Send output to spooler.
	!	.b
	!	^*SP\*	Mnemonic for spooler
	!	.b
	!	^*Filename\*	Send output to a file.
	!	.b
	!	^*Device:\*	Send output to a device (Printer,
	!	etc.)
	!	.b
	!	^*Word processing\*	Send output to a word processing
	!	file
	!	.b
	!	^*WP\*	Mnemonic for word processing
	!	.lm -5
	!
	! Index:
	!	.x End Page
	!	.x Report Settings>End Page
	!
	!--

		OUTPUT$ = TRM$(UTL_REPORTX::DEFOUT) + " " + &
			XTYPE$(UTL_REPORTX::PRINTTO)

		OUTPUT$ = LEFT(OUTPUT$ + SPACE$(64%), 64%)

6010		OUTPUT$ = ENTR_3STRING(SCOPE, &
			UTL_REPORTX::WINDOW, "1;19", "Destination", &
			OUTPUT$, CBFLAG% OR 16%, "'LLLLLLLLLLLLLLLLLLL", "")

		SELECT SCOPE::SCOPE_EXIT
		CASE 3%, 290%
			GOTO 6092

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 6092

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, 274%, 275%

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO EnterOne

		END SELECT

		GOTO 6092 IF OUTPUT$ = ""

		GOSUB 6400

		GOSUB PaintTo

	!
	! Enter Spooler Form Name
	!
	CASE "SF"

	!++
	! Abstract:FLDSF
	!	^*(SF) Spooler Form\*
	!	.b
	!	.lm +5
	!	The ^*Spooler Form\* field will allow selection of forms
	!	that have previously been defined through VMS/DCL. This
	!	field is only useful if a legitimate spooler has been
	!	selected in the (DD) field (see DD help).
	!	.b
	!	Currently, there are two items of concern when using
	!	various forms: form name and form stock. Form name
	!	is the name given to the form and form stock describes
	!	the paper required for that form. Form stock defaults
	!	to the form name if it is not specified.
	!	.b
	!	If the form name is the same as the form name currently
	!	associated (mounted on) with the requested queue then the
	!	job will be submitted and printed; otherwise if the form
	!	name is different and the form stock is different then
	!	the job will be pending in the queue until the form names
	!	match or until the form stocks match. The System Manager
	!	or a privileged user may change the form associated with
	!	the queue. If the form stock matches the form stock of
	!	the queue then the job will be submitted and printed even
	!	if the form names do not match.
	!	.b
	!	If your account has the privilege OPER or greater then
	!	the requested form will be mounted on the requested queue
	!	and the job will be submitted and printed provided the queue
	!	is a legitimate queue. This will be done automatically.
	!	.b
	!	^*NOTE:\* It is important to realize that if this occurs
	!	the queue will now have that new form associated with it
	!	until the System Manager or privileged user changes it.
	!	This means that everything sent to that queue (spooler) will
	!	not print unless the form name and/or form stock match as
	!	described above.
	!	.b
	!	Pressing ^*List Choices\* will provide a list of
	!	the available predefined forms.
	!	.lm -5
	!
	! Index:
	!	.x Start Page
	!	.x Report Settings>Start Page
	!
	!--

		UTL_REPORTX::SPOOLFORM = ENTR_3STRING(SCOPE, &
			UTL_REPORTX::WINDOW, "2;19", &
			"Spooler Form", UTL_REPORTX::SPOOLFORM, &
			CBFLAG% OR 16%, "'E", "")

		IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
		THEN
			CALL READ_QUEFORM("SYS$PRINT", RDATA1$(,), STAT%)

			RDATA2$(IZ%) = LEFT(RDATA1$(IZ%, 1%) + &
				SPACE$(31%), 31%) + " " + &
				RDATA1$(IZ%, 2%) + " " + &
				LEFT(RDATA1$(IZ%, 3%) + SPACE$(31%), 31%) &
					FOR IZ% = 1% TO VAL%(RDATA1$(0%, 0%))

			X% = ENTR_3CHOICE(SCOPE, "", "", RDATA2$(), "", 138%, &
				"  Form Name                        #  " + &
				"Form Stock", "034,038", 0%)

			SCOPE::SCOPE_EXIT = 0%

			IF X% > 0%
			THEN
				UTL_REPORTX::SPOOLFORM = &
					EDIT$(LEFT(RDATA2$(X%), 31%), 128%)
				GOTO EnterTwo
			END IF
		END IF

	!
	! Enter the starting page
	!
	CASE "SP"

	!++
	! Abstract:FLDSP
	!	^*(SP) Start Page\*
	!	.b
	!	.lm +5
	!	^*(SP) Start Page\* allows the user to specify the first
	!	page of the report to be printed. The entire report is created,
	!	but will not print until the selected page number is reached.
	!	A value of ^*zero (0)\* or ^*one (1)\* will cause the report to
	!	start printing at the first page.
	!	.b
	!	^*Note:\*
	!	This setting is not functional when the
	!	report is displayed on the screen, since
	!	there are no page numbers on a displayed
	!	report.
	!	.lm -5
	!
	! Index:
	!	.x Start Page
	!	.x Report Settings>Start Page
	!
	!--

		UTL_REPORTX::STARTP = ENTR_3NUMBER(SCOPE, UTL_REPORTX::WINDOW, "5;19", &
			"Start Page", UTL_REPORTX::STARTP * 1.0, CBFLAG%, &
			"#####", "")

	!
	! Enter the ending page
	!
	CASE "EP"

	!++
	! Abstract:FLDEP
	!	^*(EP) End Page\*
	!	.B
	!	.LM +5
	!	^*(EP) End Page\* allows the user to specify the last page
	!	number which is to be printed on the report. A value of ^*zero
	!	(0)\* causes the report to print to and including the last page.
	!	A value of ^*Ten (10)\* for example, would cause the report to
	!	discontinue after printing page l0.
	!	.B
	!	^*Note:\*  This setting is not functional when the report
	!	is displayed on the screen, since there are no
	!	page numbers on a displayed report.
	!	.lm -5
	!
	! Index:
	!	.x End Page
	!	.x Report Settings>End Page
	!
	!--

		UTL_REPORTX::ENDP = ENTR_3NUMBER(SCOPE, UTL_REPORTX::WINDOW, &
			"6;19", "End Page", UTL_REPORTX::ENDP * 1.0, CBFLAG%, &
			"#####", "")

	!
	! Enter the number of copies
	!
	CASE "CP"

	!++
	! Abstract:FLDCP
	!	^*(CP) Copies\*
	!	.b
	!	.lm +5
	!	^*(CP) Copies\*, allows the user to specify the number of
	!	copies to be printed.
	!	.b
	!	^*Note:\* This setting functions only when a spooler has been
	!	selected. The document destination (^*DD\*) must be set
	!	to ^*SP\* (or ^*SPOOLER\*) before the spooler name otherwise
	!	it will not recognize the spooler.
	!	.lm -5
	!
	! Index:
	!	.x Copies
	!	.x Report Setting>Copies
	!
	!--

		UTL_REPORTX::COPIES = ENTR_3NUMBER(SCOPE, &
			UTL_REPORTX::WINDOW, "7;19", &
			"Number of Copies", &
			UTL_REPORTX::COPIES * 1.0, CBFLAG%, "#####", "")

		CALL ENTR_3MESSAGE(SCOPE, &
			"Have you considered a copy machine??", 1%) &
			IF UTL_REPORTX::COPIES > 19%

	!
	! Autoscroll
	!
	CASE "AS"

	!++
	! Abstract:FLDAS
	!	^*(AS) Auto Scroll\*
	!	.b
	!	.lm +5
	!	The ^*(AS) Auto Scroll\* setting is functional only when a
	!	report is displayed on the screen as opposed to printing the
	!	report. A ^*Yes\* setting will cause a displayed report to
	!	scroll continuously on the screen.
	!	.b
	!	The scrolling can be halted by pressing the ^*Hold Screen\*
	!	key, otherwise the scrolling will be continuous until the end
	!	of the report is reached.
	!	.b
	!	The scrolling can be terminated by pressing the ^*Exit\*
	!	key.
	!	.lm -5
	!
	! Index:
	!	.x Auto Scroll
	!	.x Report Settings>Auto Scroll
	!
	!--

		IF UTL_REPORTX::AUTOSCROLL
		THEN
			TEMP$ = "Y"
		ELSE
			TEMP$ = "N"
		END IF
		UTL_REPORTX::AUTOSCROLL = ENTR_3YESNO(SCOPE, &
			UTL_REPORTX::WINDOW, "8;19", "Autoscroll", &
			TEMP$, CBFLAG%, "'E", "") = "Y"

	!
	! Enter Run After Time
	!
	CASE "AF"

	!++
	! Abstract:FLDAF
	!	^*(AF) After\*
	!	.b
	!	.lm +5
	!	The ^*After\* field allows the user to specify the
	!	the time at which the report will run.
	!	.b
	!	NOTE: This field is only used when running the report
	!	in background mode (See field BG).
	!	.lm -5
	!
	! Index:
	!	.x After
	!	.x Report Settings>After
	!
	!--

		UTL_REPORTX::AFTERTIME = ENTR_3TIME(SCOPE, &
			UTL_REPORTX::WINDOW, &
			"9;19", "Run AFter Time", &
			UTL_REPORTX::AFTERTIME, CBFLAG%, "", "")

	!
	! Enter Background/forground mode
	!
	CASE "BG"

	!++
	! Abstract:FLDBG
	!	^*(BG) Background\*
	!	.b
	!	.lm +5
	!	The ^*Background\* field allows the user to specify
	!	if they want to run the report in background mode,
	!	which causes the report generation to be sent to the
	!	system que, freeing up his terminal.
	!	.b
	!	NOTE: The background mode will not work if the
	!	report is being displayed or printed through the
	!	printer port.
	!	.lm -5
	!
	! Index:
	!	.x Background
	!	.x Report Settings>Background
	!
	!--

		UTL_REPORTX::BACKGROUND = &
			ENTR_3YESNO(SCOPE, UTL_REPORTX::WINDOW, &
			"10;19", "Background", &
			UTL_REPORTX::BACKGROUND, CBFLAG%, "'E", "")

	!
	! Enter offset
	!
	CASE "OF"

	!++
	! Abstract:FLDOF
	!	^*(OF) Offset\*
	!	.b
	!	.lm +5
	!	The ^*Offset\* field allows the user to specify
	!	additional spaces added to the right margin.
	!	.lm -5
	!
	! Index:
	!	.x Offset
	!	.x Report Settings>Offset
	!
	!--

		UTL_REPORTX::OFFSET = &
			ENTR_3NUMBER(SCOPE, UTL_REPORTX::WINDOW, &
			"11;19", "Offset", &
			UTL_REPORTX::OFFSET * 1.0, CBFLAG%, "####", "")

	!
	! Enter report date
	!
	CASE "RD"

	!++
	! Abstract:FLDRD
	!	^*(RD) Report Date\*
	!	.b
	!	.lm +5
	!	The ^*Report Date\* field allows the user to specify the
	!	date which will be printed on the report.  The system will
	!	automatically display the current date.  If a change in the
	!	date is needed, use the ^*Change\* function.  Any date format
	!	is acceptable.
	!	.lm -5
	!
	! Index:
	!	.x Report Date
	!	.x Report Settings>Report Date
	!
	!--

		UTL_REPORTX::REPDATE = LEFT(UTL_REPORTX::REPDATE + &
			SPACE$(20%), 20%)
		UTL_REPORTX::REPDATE = ENTR_3STRING(SCOPE, &
			UTL_REPORTX::WINDOW, "12;19", "Report Date", &
			UTL_REPORTX::REPDATE, CBFLAG%, "'E", "")

	!
	! Printer type
	!
	CASE "PT"

	!++
	! Abstract:FLDPT
	!	^*(PT) Printer Type\*
	!	.b
	!	.lm +5
	!	^*(PT) Printer type\* defines the type of printer used to print
	!	the report. If the printer type is changed the system will search
	!	for the new printer type and read in the set command file.
	!	.b
	!	Currently defined printers include:
	!	.table 3,25
	!	.te
	!	^*DEFALT Default printer type
	!	.te
	!	FX286 Epson FX-286
	!	.te
	!	LA120 LA120 Decwriter IV\*
	!	.end table
	!
	! Index:
	!	.x Printer Type
	!	.x Printer Type
	!	.x General Ledger Report>Printer Type
	!	.x Report Settings>Printer Type
	!
	!--

		TEMP_PRINTTYPE$ = UTL_REPORTX::PRINTTYPE

6020		UTL_REPORTX::PRINTTYPE = ENTR_3STRING(SCOPE, &
			UTL_REPORTX::WINDOW, &
			"1;67", "Printer Type", &
			UTL_REPORTX::PRINTTYPE, CBFLAG% OR 16%, "'E", "")

		SELECT SCOPE::SCOPE_EXIT

		!
		! Exit
		!
		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 6092

		!
		! List choices
		!
		CASE SMG$K_TRM_F14

			TEMP$ = LIBR_SELECT("CMC:PRINT_TYPE", &
				"Printer Type", "LIBR_SELECT", &
				"Maintain Create Help eXit")
			!
			! Valid command for LIBR_SELECT ARE:
			!	* Maintain
			!	* Create
			!	* Help
			!	* eXit
			!
			UTL_REPORTX::PRINTTYPE = TEMP$ IF TEMP$ <> ""

			GOSUB 18200

			IF PRINTX::GROUPS = 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					UTL_REPORTX::PRINTTYPE + &
					" has no attributes Please retry.", 0%)
				GOTO 6020
			END IF

			GOSUB 6300

			GOTO 6020

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, &
			SMG$K_TRM_UP, SMG$K_TRM_DOWN

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO EnterOne

		END SELECT

		GOTO 6092 IF TEMP_PRINTTYPE$ = UTL_REPORTX::PRINTTYPE

		GOSUB 18200

		IF PRINTX::GROUPS = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				UTL_REPORTX::PRINTTYPE + &
				" has no attributes Please retry.", 0%)
			GOTO 6020
		END IF

		GOSUB 6300

	!
	! General data entry for Prog. Dep. input
	!
	CASE "01", "02", "03", "04", "05", "06", "07", "08", "09", "10"

		!
		! Set up for help key
		!
		SCOPE::PRG_PROGRAM = TRM$(UTL_REPORT::PRONAM)

		!
		!
		PDN% = VAL%(ITM$) - 1%

		!
		! If blank option and Required field then stop blank?
		!
		IF CBFLAG% = 33%
		THEN
			IF (UTL_REPORT::REQUIRE(PDN%) = "Y") AND (INP$ = "")
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"This field cannot be set to blank!", &
					0%)
				GOTO 6092
			END IF
		END IF

		!
		! Skip out if the length is 0 or if the description is blank
		!
		GOTO 6092 IF UTL_REPORT::OPTLEN(PDN%) = 0% OR &
			EDIT$(UTL_REPORT::DESCR(PDN%), -1%) = "" &

		!
		! Calculate the screen postion and print length
		!
		TEMP% = UTL_REPORT::OPTLEN(PDN%)

		IF PDN% <= 4%
		THEN
			XPOS% = 27%
			YPOS% = PDN% + 14%
			PRINT_LEN% = 12%
			PRINT_LEN% = TEMP% IF TEMP% < PRINT_LEN%
		ELSE
			XPOS% = 66%
			YPOS% = PDN% + 9%
			PRINT_LEN% = 14%
			PRINT_LEN% = TEMP% IF TEMP% < PRINT_LEN%
		END IF

		TEMP$ = NUM1$(YPOS%) + ";" + NUM1$(XPOS%)

6030		!
		! Enter data item
		!
		SELECT UTL_REPORT::OPTTYPE(PDN%)

		CASE "F"	! Floating point
			TEMP1$ = STRING$(PRINT_LEN% - 3%, A"#"B) + ".##"

			M = ENTR_3NUMBER(SCOPE, UTL_REPORTX::WINDOW, TEMP$, &
				TRM$(UTL_REPORT::DESCR(PDN%)), &
				VAL(UTL_REPORTX::OPTDEF(PDN%)) * 1.0, &
				CBFLAG%, TEMP1$, "")
			INP$ = FORMAT$(M, TEMP1$)

			WORK% = INSTR(1%, INP$, "%")
			IF WORK%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Number is to large for format field.", 0%)
				INP$ = RIGHT(INP$, WORK% + 1%)
			END IF

		CASE "D", "d"	! Date

			IF TEMP% = 6%
			THEN
				TT% = 6%
			ELSE
				TT% = 8%
			END IF

			TMEP$ = LEFT(UTL_REPORTX::OPTDEF(PDN%), TT% + 2%)
			TMEP$ = MID(TMEP$, 7%, TT% - 4%) + &
				LEFT(TMEP$, 2%) + MID(TMEP$, 4%, 2%)

			INP$ = ENTR_3DATE(SCOPE,  UTL_REPORTX::WINDOW, TEMP$, &
				TRM$(UTL_REPORT::DESCR(PDN%)), &
				TMEP$, CBFLAG%, NUM1$(TT%), "")

			INP$ = MID(INP$, TT% - 3%, 2%) + "/" + &
				MID(INP$, TT% - 1%, 2%) + "/" + &
				LEFT(INP$, TT% - 4%)

		CASE "I"	! Integer
			TEMP1$ = STRING$(PRINT_LEN%, A"#"B)

			M% = ENTR_3NUMBER(SCOPE, UTL_REPORTX::WINDOW, TEMP$, &
				TRM$(UTL_REPORT::DESCR(PDN%)), &
				VAL(UTL_REPORTX::OPTDEF(PDN%)) * 1.0, &
				CBFLAG%, TEMP1$, "")
			INP$ = FORMAT$(M%, TEMP1$)

		CASE "P" ! Period

			INP$ = ENTR_PERIOD(UTL_REPORTX::WINDOW, TEMP$, &
				TRM$(UTL_REPORT::DESCR(PDN%)), &
				LEFT(UTL_REPORTX::OPTDEF(PDN%), &
				TEMP%), CBFLAG%, "", "")

		CASE "Y"	! Yes/No
			TEMP1$ = "'" + STRING$(PRINT_LEN% - 1%, A"L"B)

			INP$ = ENTR_3YESNO(SCOPE, UTL_REPORTX::WINDOW, TEMP$, &
				TRM$(UTL_REPORT::DESCR(PDN%)), &
				LEFT(UTL_REPORTX::OPTDEF(PDN%), &
				TEMP%), CBFLAG%, TEMP1$, "")

		CASE "S"	! Upper case string
			TEMP1$ = "'" + STRING$(PRINT_LEN% - 1%, A"L"B)

			INP$ = ENTR_3STRING(SCOPE, UTL_REPORTX::WINDOW, TEMP$, &
				TRM$(UTL_REPORT::DESCR(PDN%)), &
				LEFT(UTL_REPORTX::OPTDEF(PDN%), &
				TEMP%), CBFLAG% OR 16%, TEMP1$, "")

		CASE ELSE	! String
			TEMP1$ = "'" + STRING$(PRINT_LEN% - 1%, A"L"B)

			INP$ = ENTR_3STRING(SCOPE, UTL_REPORTX::WINDOW, TEMP$, &
				TRM$(UTL_REPORT::DESCR(PDN%)), &
				LEFT(UTL_REPORTX::OPTDEF(PDN%), &
				TEMP%), CBFLAG%, TEMP1$, "")

		END SELECT

		SELECT SCOPE::SCOPE_EXIT
		CASE 3%, 290%
			GOTO 6092

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 6092

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, 274%, 275%

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO EnterOne
		END SELECT

		!
		! Required field?
		!
		IF (UTL_REPORT::REQUIRE(PDN%) = "Y") AND (INP$ = "")
		THEN
			CALL HELP_34MESSAGE(SCOPE, "value is required", &
			"W", "OUTP_SETTINGS", "", "VALUEREQ")
	!++
	! Warning:VALUEREQ
	!	^*Explanation\*
	!	.p
	!
	!--

			GOTO 6030
		END IF

		!
		! Test input
		!
		IF NOT (UTL_REPORT::VALID(PDN%) = "") AND &
			EDIT$(INP$, -1%) <> ""
		THEN
			IF COMP_STRING(INP$, UTL_REPORT::VALID(PDN%)) = 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, "Valid input is " + &
					UTL_REPORT::VALID(PDN%), 0%)
				SCOPE::SCOPE_EXIT = 0%
				GOTO 6030
			END IF
		END IF

		UTL_REPORTX::OPTDEF(PDN%) = INP$

		!
		! Print item
		!
		SMG_STATUS% = SMG$PUT_CHARS(UTL_REPORTX::WINDOW, &
			LEFT(UTL_REPORTX::OPTDEF(PDN%), PRINT_LEN%), &
			YPOS%, XPOS%,, SMG$M_BOLD)

	!
	! It might be one of the printer functions
	!
	CASE ELSE
		GOTO 6080 IF (ITM$ = PRINTX::GROUPX(PRINT_LINE%(I%))) &
			FOR I% = 1% TO Q.I%
		GOTO 6092

6080		!
		! We got a match
		!

		K% = PRINT_LINE%(I%)
		TEMP$ = ENTR_3STRING(SCOPE, UTL_REPORTX::WINDOW, &
			NUM1$(I% + 1%) + ";67", &
			TRM$(PRINTX::DESCR(K%)), PRINTX::DEFLT(K%), &
			0%, "'E", "")

		SCOPE::PRG_ITEM = "FLD" + PRINTX::GROUPX(K%)

	!++
	! Abstract:FLDLP
	!	^*(LP) Lines per Page\*
	!	.b
	!	.lm +5
	!	^*Lines per page\* defines the number of vertical lines contained on a page.
	!	This number is user defined.
	!	.lm -5
	!
	! Index:
	!	.x Lines per Page
	!
	!--

	!++
	! Abstract:FLDHP
	!	^*(HP) Horizontal Pitch\*
	!	.b
	!	.lm +5
	!	^*Horizontal pitch\* is defined as the number of characters per horizontal inch.
	!	Valid horizontal pitch settings vary, depending upon the printer type ^*(PT)\*
	!	setting. The pitch may be changed by entering a valid number associated with
	!	the printer type. A list of valid settings will be displayed by pressing the
	!	^*List Choices\* key, while the cursor is located at the ^*(HP)\* field. A
	!	choice may be made by positioning the arrow to the left of the desired choice
	!	and pressing the ^*Select\* key.
	!	.lm -5
	!
	! Index:
	!	.x Horizontal Pitch
	!
	!--

	!++
	! Abstract:FLDVP
	!	^*(VP) Vertical Pitch\*
	!	.b
	!	.lm +5
	!	^*Vertical pitch\* is defined as the number of lines per vertical inch. Valid
	!	vertical pitch settings vary, depending on the printer type ^*(PT)\*
	!	settings. The pitch may be changed by entering a valid number associated with
	!	the printer type. A list of valid setting will be displayed by pressing the
	!	^*List Choices\* key, while the cursor is located at the ^*(VP)\* field.
	!	A choice may be made by positioning the arrow to the left of the desired
	!	choice and pressing the ^*Select\* key.
	!	.lm -5
	!
	! Index:
	!	.x Vertical Pitch
	!
	!--

	!++
	! Abstract:FLDFT
	!	^*(FT) Font Selection\*
	!	.b
	!	.lm +5
	!	^*Font Selection\* is the type of printing available. Valid Font settings
	!	vary with the printer type ^*(PT)\* being used.  A valid setting may be
	!	entered or a list may be displayed by using the ^*<List Choices>\* key when
	!	positioned in the ^*(FT)\* field.  A selection may be issued from the list
	!	by positioning the arrow to the left of the selection and pressing the
	!	^*<Select>\* key.
	!
	! Index:
	!	.x Font Selection
	!
	!--

	!++
	! Abstract:FLDQU
	!	^*(QU) Quality\*
	!	.b
	!	.lm +5
	!	^*Quality\* is the type of printing used by the printer. Quality may vary
	!	with the printer type ^*(PT)\* setting. A valid quality may be entered or
	!	a list of choices will be given when the cursor is placed in the ^*(QU)\*
	!	field. A choice may be made by positioning the arrow to the left of the
	!	desired choice and pressing the ^*Select\* key.
	!	.lm -5
	!
	! Index:
	!	.x Quality
	!
	!--

	!++
	! Abstract:FLDCS
	!	^*(CS) Character Set\*
	!	.b
	!	.lm +5
	!	^*Character Set\* determines whether the printing will be normal or
	!	initialized.
	!	The valid code may be entered or the ^*List Choices\* key may be used.
	!	To access through the ^*List Choices\* key, place the cursor in the ^*
	!	Character Set\* field and press the ^*List Choices\* key. Move the arrow
	!	to the desired choice and press the ^*Select\* key
	!	.lm -5
	!
	! Index:
	!	.x Character Set
	!	.x Set>Character
	!	.x Report Settings>Character Set
	!
	!--

	!++
	! Abstract:FLDFO
	!	^*(FO) Font\*
	!	.b
	!	.lM +5
	!	^*Font\* refers to the type of character style available. The options may
	!	vary with the ^*Printer Type (PT)\*. A valid choice may be entered or the
	!	^*<List Choices>\* key used. When using ^*<List Choices>\*, place
	!	the cursor in the ^*Font\* field and press the ^*<List Choices>\* key.  Move
	!	the arrow to the desired selection and press the ^*<Select>\* key.
	!
	! Index:
	!	.x Font>Report Settings
	!	.x Report Settings>Font
	!
	!--

	!++
	! Abstract:FLDLQ
	!	^*(LQ) Letter Quality Mode\*
	!	.b
	!	.lm +5
	!	^*Letter Quality Mode\* refers to the quality of printing and depends
	!	on the ^*Printer Type (PT)\* selected. A valid code may be entered or the
	!	^*<List Choices>\* key may be used. To use the ^*<List Choices>\* key, place
	!	the cursor in the ^*Letter Quality Mode\* field and press the ^*<List
	!	Choices>\*
	!	key. Move the arrow to the desired selection and press the ^*<Select>\* key.
	!	.lm -5
	!
	! Index:
	!	.x Letter Quality Mode>Report Settings
	!	.x Report Settings>Letter Qualtiy Mode
	!	.x Mode>Letter Quality
	!
	!--

	!++
	! Abstract:FLDPF
	!	^*(PF) Page Format\*
	!	.b
	!	.lm +5
	!	^*Page Format\* allows for the option of wide or normal print.  A valid
	!	code may be entered or the ^*<List Choices>\* key used.  In using the^*<List
	!	Choices>\* key, place the cursor in the ^*Page Format\* field and press the
	!	^*<List Choices>\* key. Move the arrow to the desired choice and press the
	!	^*<Select>\* key.
	!	.lm -5
	!
	! Index:
	!	.x Page Format>Report Settings
	!	.x Report Settings>Page Format
	!	.x Format>Page
	!
	!--

	!++
	! Abstract:FLDPZ
	!	^*(PZ) Point Size\*
	!	.b
	!	.lm +5
	!	^*Point Size\* determines how big the characters will be. The ^*Point Size\*
	!	options change with the ^*Printer Type (PT)\*. A valid code may be entered
	!	or the selection may be made by using the ^*<List Choices>\* menu. To use
	!	the ^*<List Choices>\* menu, place the cursor in the ^*Point Size (PZ)\*
	!	field and press the ^*<List Choices>\* key. Move the arrow to the desired
	!	choice and press the ^*select\* key.
	!	.lm -5
	!
	! Index:
	!	.x Point Size>Report Settings
	!	.x Report Settings>Point Size
	!	.x Size>Point
	!
	!--

	!++
	! Abstract:FLDST
	!	^*(ST) Character Style\*
	!	.b
	!	.lm +5
	!	^*Character Style\* refers to the added effects on the characters. A valid
	!	code may be entered or the ^*List Choices\* menu used. To use the ^*List
	!	Choices\* menu, place the cursor in the ^*Character Style\* file and press
	!	the ^*List Choices\* key. Move the arrow to the desired choice and press
	!	the ^*Select\* key.
	!	.lm -5
	!
	! Index:
	!	.x Character Style>Report Settings
	!	.x Report Settings>Character Style
	!	.x Style>Character
	!
	!--

	!++
	! Abstract:FLDTF
	!	^*(TF) Typeface\*
	!	.b
	!	.lm +5
	!	^*Typeface\* is defined as the shape of the set of characters. The ^*Typeface\*
	!	settings vary with the ^*Printer Type (PT)\* being used. A valid code
	!	may be entered or the ^*List Choices\* menu may be accessed. To access the
	!	menu, place the cursor in the ^*Typeface (TF)\* field and press the ^*List
	!	Choices\* key.  Move the arrow to the desired selection and press the
	!	^*Select\* key.
	!	.lm -5
	!
	! Index:
	!	.x Typeface>Report Settings
	!	.x Report Settings>Typeface
	!
	!--
		SELECT SCOPE::SCOPE_EXIT

		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 6092

		CASE SMG$K_TRM_F14
			GOSUB 9000
			GOTO 6080

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, &
			SMG$K_TRM_UP, SMG$K_TRM_DOWN

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO EnterOne

		END SELECT

		!
		! Check for good input
		!
		IF FIND_3PRINTGROUPITEM(PRINTX::GROUPX(K%), TEMP$, PRINTX) <= 0%
		THEN
			GOSUB 9000

			!
			! Bad input| Bad input| (Swat| Swat|)
			!
			GOTO 6080
		ELSE

			!
			! Got a good value
			!
			PRINTX::DEFLT(K%) = TEMP$

			!
			! Update the Form values
			!
			GOSUB PaintForm

		END IF

6090	END SELECT

	SELECT SCOPE::SCOPE_EXIT

	CASE 0%, 3%, 10%, 12%, 13%, SMG$K_TRM_DO, &
		SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
		SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO EnterOne

	END SELECT

6092	RETURN

	%PAGE

6100	!******************************************************************
	! Paint background
	!******************************************************************
 PaintAll:

	GOSUB 18200	! Search out printer type

	GOSUB PaintReport
	GOSUB PaintPrint
	GOSUB PaintTo

	RETURN

	%PAGE

6150	!***************************************************************
	! Paint entire screen background
	!***************************************************************

 PaintReport:
	SCOPE::SCOPE_EXIT = 0%
	CBFLAG% = 1%

	FOR LOOP% = 1% TO LEN(LEFT_SIDE_CMD) STEP 3%
		ITM$ = MID(LEFT_SIDE_CMD, LOOP%, 2%)
		GOSUB EnterTwo
	NEXT LOOP%

	FOR LOOP% = 1% TO LEN(RIGHT_SIDE_CMD) STEP 3%
		ITM$ = MID(RIGHT_SIDE_CMD, LOOP%, 2%)
		GOSUB EnterTwo
	NEXT LOOP%

	GOSUB PaintForm

	FOR I% = 0% TO 4%
		IF UTL_REPORT::OPTLEN(I%) AND &
			EDIT$(UTL_REPORT::DESCR(I%), -1%) <> ""
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(UTL_REPORTX::WINDOW, &
				FORMAT$(I% + 1%, "(<0>#) ") + &
				LEFT(UTL_REPORT::DESCR(I%), 20%) + " ", &
				I% + 14%, 1%)
			SMG_STATUS% = SMG$PUT_CHARS(UTL_REPORTX::WINDOW, &
				LEFT(UTL_REPORTX::OPTDEF(I%), 12%),,,, &
				SMG$M_BOLD)

		END IF
	NEXT I%

	FOR I% = 5% TO 9%
		IF UTL_REPORT::OPTLEN(I%) AND &
			EDIT$(UTL_REPORT::DESCR(I%), -1%) <> ""
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(UTL_REPORTX::WINDOW, &
				FORMAT$(I% + 1%, "(<0>#) ") + &
				LEFT(UTL_REPORT::DESCR(I%), 20%) + " ", &
				I% + 9%, 40%)
			SMG_STATUS% = SMG$PUT_CHARS(UTL_REPORTX::WINDOW, &
				LEFT(UTL_REPORTX::OPTDEF(I%), 14%),,,, &
				SMG$M_BOLD)

		END IF
	NEXT I%

	RETURN

	%PAGE

6200	!***************************************************************
	! Paint where report is going to
	!***************************************************************
 PaintTo:

	OUTPUT$ = TRM$(UTL_REPORTX::DEFOUT) + " " + &
		XTYPE$(UTL_REPORTX::PRINTTO)

	OUTPUT$ = LEFT(OUTPUT$ + SPACE$(20%), 20%)

	SMG_STATUS% = SMG$PUT_CHARS(UTL_REPORTX::WINDOW, &
		OUTPUT$, 1%, 19%, , SMG$M_BOLD)

	RETURN

	%PAGE

6300	!***************************************************************
	! Paint background data for reports
	!***************************************************************

 PaintPrint:
	Q.I% = 0%

	IF INSTR(1%, LEFT_SIDE_CMD + RIGHT_SIDE_CMD, "PT")
	THEN
		SMG_STATUS% = SMG$PUT_CHARS(UTL_REPORTX::WINDOW, &
			UTL_REPORTX::PRINTTYPE, 1%, 67%, , SMG$M_BOLD)

		FOR I% = 1% TO PRINTX::GROUPS

			!
			! Don't print if not allowed
			!
			IF (PRINTX::GROUPP(I%) AND 2048%) = 0%
			THEN
				Q.I% = Q.I% + 1%
				PRINT_LINE%(Q.I%) = I%
				SMG_STATUS% = SMG$PUT_CHARS( &
					UTL_REPORTX::WINDOW, &
					"(" + PRINTX::GROUPX(I%) + ") " + &
					PRINTX::DESCR(I%) + " ", &
					Q.I% + 1%, 41%)
				SMG_STATUS% = SMG$PUT_CHARS( &
					UTL_REPORTX::WINDOW, &
					PRINTX::DEFLT(I%), , , , SMG$M_BOLD)
			END IF

		NEXT I%

	END IF

	SMG_STATUS% = SMG$ERASE_LINE(UTL_REPORTX::WINDOW, I% + 1%, 41%) &
		FOR I% = Q.I% + 1% TO 11%

	RETURN

	%PAGE

 PaintForm:
6350	!***************************************************************
	! Print the from width and length after calculating
	!***************************************************************

	FOR I% = 1% TO PRINTX::GROUPS
		FORM.HP = VAL(PRINTX::DEFLT(I%)) IF PRINTX::GROUPX(I%) = "HP"
		FORM.VP = VAL(PRINTX::DEFLT(I%)) IF PRINTX::GROUPX(I%) = "VP"
		FORM.LP = VAL(PRINTX::DEFLT(I%)) IF PRINTX::GROUPX(I%) = "LP"
	NEXT I%

	UTL_REPORT::REPWID = 132% IF UTL_REPORT::REPWID = 0%

	IF FORM.HP > 0.
	THEN
		FORM.W = UTL_REPORT::REPWID / FORM.HP
	ELSE
		FORM.W = 0.
	END IF
	SMG_STATUS% = SMG$PUT_CHARS(UTL_REPORTX::WINDOW, &
		FORMAT$(FORM.W, "##.#"), 3%, 19%)

	IF FORM.VP > 0.
	THEN
		FORM.L = FORM.LP / FORM.VP
	ELSE
		FORM.L = 0.
	END IF
	SMG_STATUS% = SMG$PUT_CHARS(UTL_REPORTX::WINDOW, &
		FORMAT$(FORM.L, "##.#"), 4%, 19%)

	RETURN

6400	!***************************************************************
	! Figure out what an DD type is (name in OUTPUT$)
	!***************************************************************

	OUTPUT$ = EDIT$(OUTPUT$, 4% + 8% + 16% + 32% + 128% + 256%)

	TEMP$ = "TT:"

	!==================================================================
	! Search the users input for PRINTER PORT, PP, DISPLAY, ...
	!==================================================================

	FOR I% = 1% TO OCOUNT%
		IF RIGHT(" " + OUTPUT$, LEN(OUTPUT$) - LEN(OTYPE$(I%)) + 1%) = &
			" " + OTYPE$(I%)
		THEN
			TEMP1$ = LEFT(OUTPUT$, &
				LEN(OUTPUT$) - LEN(OTYPE$(I%)) - 1%)
			PRINT.TO% = OTYPE%(I%)
			GOTO 6405
		END IF

	NEXT I%

	!
	! Default case - output to a device
	!
	PRINT.TO% = OUTP_TODEVICE
	TEMP1$ = OUTPUT$

6405	!
	! Spool
	!
	TEMP1$ = EDIT$(TEMP1$, 4% + 8% + 32% + 128% + 256%)

	SELECT PRINT.TO%

	CASE OUTP_TOSPOOL
		TEMP$ = "SYS$PRINT"
		TEMP$ = TEMP1$ IF TEMP1$ <> ""
		UTL_REPORTX::PRINTTO = OUTP_TOSPOOL
		UTL_REPORTX::DEFOUT = TEMP$
		UTL_REPORTX::SPOOL = TEMP$
		GOTO ExitSub

	CASE OUTP_TODOCUMENT
		!
		! Send only the first page to the documentation
		! and force LA120 Printer
		!
		UTL_REPORTX::STARTP,UTL_REPORTX::ENDP = 1%
		UTL_REPORTX::PRINTTYPE = "LA120"

		TEMP1$ = "TEMP_DOCUMENT.TMP" IF TEMP1$ = ""

	CASE OUTP_TO2020
		TEMP1$ = "TEMP.S2020" IF TEMP1$ = ""

	CASE OUTP_TOPL
		TEMP1$ = "TEMP.PL" IF TEMP1$ = ""

	END SELECT

	TEMP$ = TEMP1$ IF TEMP1$ <> ""

6410	!==================================================================
	! First try at scanning name
	!==================================================================
	!
	! FTYPE%
	!	3 - Printer/terminal
	!	4 - File
	!	5 - Users Terminal (printer port?)
	!
	SYS_STATUS = LIB$GETDVI(DVI$_DEVCLASS,, TEMP$, DEVCHAR,,)

	!
	! Certain errors may mean that it is a file
	!
	IF (SYS_STATUS = 324%) OR (SYS_STATUS = 2312%)
	THEN
		FTYPE% = 4%
		GOTO 6420
	END IF

	!
	! Error 2288 occurs when attempting to go over DECNET.
	! Attempt to guess where it is really going, and then
	! hope for the best.  Haven't yet figured out how to test
	! devices across networks.
	!
	IF (SYS_STATUS = 2288%)
	THEN
		!
		! If name ends with a colon, assume it is a printer
		! othwewise assume it is a file.
		!
		IF RIGHT(TEMP$, LEN(TEMP$)) = ":"
		THEN
			FTYPE% = 3%
		ELSE
			FTYPE% = 4%
		END IF

		GOTO 6420
	END IF

	!
	! Invalid type?
	!
	IF (SYS_STATUS AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Invalid output device: " + TEMP$ + &
			" (" + NUM1$(SYS_STATUS) + ")", 0%)
		GOTO ExitSub
	END IF

	SELECT DEVCHAR

	CASE DC$_DISK
		FTYPE% = 4%

	CASE DC$_LP, DC$_MAILBOX, DC$_TERM
		FTYPE% = 3%

	CASE ELSE
		CALL ENTR_3MESSAGE(SCOPE, "Invalid output device: " + TEMP$ + &
			" (" + NUM1$(SYS_STATUS) + ")", 0%)
		GOTO ExitSub
	END SELECT

6420	!
	! Handle output to users keyboard
	!
	IF FTYPE% = 3%
	THEN
		!
		! Is OD equal to the terminal this process is running
		! on if so then output to terminal or local printer?
		!
		IF READ_SYSLOG(TEMP$) = READ_SYSLOG("TT:")
		THEN
			FTYPE% =  5%
		END IF

	END IF

	!
	! Is it going to a printable device?
	!
	IF FTYPE% = 4%
	THEN
		!
		! Force extension of .PRT
		!
		TEMP1$ = READ_SYSLOG(TEMP$)
		IF INSTR(1%, TEMP1$, ".") = 0%
		THEN
			IF RIGHT(TEMP1$, LEN(TEMP1$)) <> ":"
			THEN
				TEMP$ = TEMP1$ + ".PRT"
				TEMP$ = TEMP1$ + ".S2020" &
					IF PRINT.TO% = OUTP_TO2020
				TEMP$ = TEMP1$ + ".PL" &
					IF PRINT.TO% = OUTP_TOPL
			END IF
		END IF
	END IF


	!==================================================================
	! Mix PRINT.TO and FTYPE to come up with the users most probible
	! desired output type.
	!==================================================================

	SELECT PRINT.TO%

	!
	! Display
	!
	CASE OUTP_TODISPLAY

		SELECT FTYPE%

		! Display on users terminal

		CASE 5%
			GOTO 6450

		END SELECT

	!
	! Normal output
	!
	CASE OUTP_TODEVICE
		PRINT.TO% = FTYPE%
		GOTO 6450

	!
	! Printer port
	!
	CASE OUTP_TOLOCAL
		GOTO 6450

	!
	! Word Processor (Must go to a file)
	!
	CASE OUTP_TOWP
		GOTO 6450 IF FTYPE% = 4%

	!
	! Documentation (Must go to a file)
	!
	CASE OUTP_TODOCUMENT
		GOTO 6450 IF FTYPE% = 4%

	!
	! SuperComp 2020 (Must go to a file)
	!
	CASE OUTP_TO2020, OUTP_TOPL
		GOTO 6450 IF FTYPE% = 4%

	END SELECT

	CALL ENTR_3MESSAGE(SCOPE, "Unable to parse output specification '" + &
		OUTPUT$ + "'", 4%)
	GOTO ExitSub

6450	!==================================================================
	! Finish update if good stuff
	!==================================================================

	SELECT PRINT.TO%
	!
	! Make sure printer is turned on
	!
	CASE OUTP_TODEVICE
6460
	!
	! See if file already exists
	!
	CASE OUTP_TOFILE, OUTP_TOWP, OUTP_TODOCUMENT, OUTP_TO2020, OUTP_TOPL

6470		!
		! Random access File structured device?
		!
		WHEN ERROR IN
			OPEN TEMP$ FOR INPUT AS FILE OUTPUT.CH
			CLOSE OUTPUT.CH
		USE
			CONTINUE 6480 IF ERR = 5%
			CALL ENTR_3MESSAGE(SCOPE, &
				"Bad output file: " + ERT$(ERR), 0%)
			CONTINUE 6490
		END WHEN

		OPT7% = 0%

6472		CALL HELP_34MESSAGE(SCOPE, TEMP$ + " file already exists", &
			"W", SCOPE::PRG_PROGRAM, "", "FILEX")
	!++
	! Warning:FILEX
	!	^*File Already Exists\*
	!	.b
	!	.lm +5
	!	^*Explanation\*
	!	.b
	!	Selected destination file already exists.
	!	.b
	!	^*User Action\*
	!	.b
	!	Select a new file name or append more text to the existing file
	!	or create a higher version of the file.
	!	.lm -5
	!
	! Index:
	!
	!--

		V$ = ENTR_3OPTION(SCOPE, "Command ", &
			"Append Overwrite eXit", OPT7%, 0%)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_CTRLC, SMG$K_TRM_F10, &
			SMG$K_TRM_CTRLZ

			GOTO ExitSub

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 6472
		END SELECT

		SELECT V$

		!
		! Overwrite
		!
		CASE "O"
	!++
	! Abstract:OVERWRITE
	!	^*Overwrite File\*
	!	.b
	!	.lm +5
	!	Creates higher version of the file.
	!	.b
	!	^*Note:\* Overwrite doesn't delete or purge files.
	!	.lm -5
	!
	! Index:
	!
	!--
6475			WHEN ERROR IN
				KILL TEMP$
				GOTO 6475
			USE
				CONTINUE 6470 IF ERR = 5%
				CONTINUE HelpError
			END WHEN

		!
		! Append
		!
		CASE "A"

	!++
	! Abstract:APPEND
	!	^*Append to the File\*
	!	.p
	!	Report lines will be added at the end of the existing file.
	!--
		!
		! Exit
		!
		CASE "X"
			GOTO ExitSub

		!
		! Bad input
		!
		CASE ELSE
			CALL ENTR_3MESSAGE(SCOPE, "Bad input, try again", 0%)
			GOTO 6472

		END SELECT

	END SELECT

6480	!
	! Good value, so keep it
	!
	UTL_REPORTX::PRINTTO = PRINT.TO%
	UTL_REPORTX::DEFOUT = TEMP$

 ExitSub:
6490	RETURN

	%PAGE

9000	!*******************************************************************
	! List options for printer commands
	!******************************************************************

	WHEN ERROR IN
		PRINTER_PRINTER$(I9%) = "" FOR I9% = 0% TO 32767%
	USE
		CONTINUE 9010
	END WHEN

9010	TEMP% = 0%
	START_LOOP% = PRINTX::GROUPP(K%)
	END_LOOP% = (PRINTX::GROUPP(K% + 1%) AND 2047%) - 1%

	FOR J% = START_LOOP% TO END_LOOP%
		IF TRM$(PRINTX::ITEM(J%)) <> ""
		THEN
			TEMP% = TEMP% + 1%
			PRINTER_PRINTER$(TEMP%) = TRM$(PRINTX::ITEM(J%))
		END IF
	NEXT J%

	IF PRINTER_PRINTER$(2%) <> ""
	THEN
		X% = ENTR_3CHOICE(SCOPE, "4;43", "13;20", &
			PRINTER_PRINTER$(), PRINTX::DEFLT(K%), &
			26%, LEFT(TRM$(PRINTX::DESCR(K%)), 18%), "", 0%)

		PRINTX::DEFLT(K%) = PRINTER_PRINTER$(X%) IF X% > -1%
	ELSE
		IF PRINTER_PRINTER$(1%) <> ""
		THEN
			IF PRINTER_PRINTER$(1%) = "*"
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					TRM$(PRINTX::DESCR(K%)) + &
					" is user definable", 0%)
			ELSE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Only valid choice is " + &
					PRINTER_PRINTER$(1%), 0%)
			END IF
		ELSE
			CALL ENTR_3MESSAGE(SCOPE, &
				"No valid choices available", 0%)
		END IF
	END IF

	RETURN

	%PAGE

18200	!***************************************************************
	! OPEN PRINT COMMAND FILES
	!***************************************************************

	CALL OUTP_INITIALIZE(UTL_REPORTX::PRINTTYPE)

18210	!
	! Take users defaults if possible
	!
	FOR I% = 0% TO 9%
		IF UTL_REPORT::ITEMGROUP(I%) <> "  "
		THEN
			J%, V% = -1%
			J% = K% IF UTL_REPORT::ITEMGROUP(I%) = &
				PRINTX::GROUPX(K%) &
				FOR K% = 0% TO PRINTX::GROUPS

			IF J% > -1%
			THEN
				V% = FIND_3PRINTGROUPITEM(PRINTX::GROUPX(J%), &
					UTL_REPORT::ITEM(I%), PRINTX)

				PRINTX::DEFLT(J%) = UTL_REPORT::ITEM(I%) &
					IF V% > 0%
			END IF
		END IF
	NEXT I%

18290	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

 NotLocked:
	!*******************************************************************
	! The record wasn't locked, so we cannot save
	!*******************************************************************

	CALL HELP_34MESSAGE(SCOPE, "Record was not locked", &
		"S", SCOPE::PRG_PROGRAM, "", "NOTLCKD")

	GOTO 4010

19000	!***************************************************************
	! E R R O R   T R A P P I N G   C O D E
	!***************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

19999	END SUB
