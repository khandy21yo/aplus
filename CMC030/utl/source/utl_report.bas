1	%TITLE "Print Reports"
	%SBTTL "UTL_REPORT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	!
	! Abstract:HELP
	!	.p
	!	Provides a common method of
	!	processing reports.  This is done by standardizing the user
	!	interfaces to all reports.
	!	.P
	!	The ^*Report\* program is divided into various menus.  It
	!	starts in the select reports menu unless the chain entry
	!	is specified from the calling program.  If the chain entry
	!	is specified, the report program will start in the
	!	settings screen.
	!	.P
	!	Other commands in ^*Report\* allow the user to add new reports
	!	from the system report file, which is found on CMC:, set up
	!	batch process files, which will process several reports in
	!	order, erase a report, and series, which allows the user to
	!	select several reports to be processed on a one time basis.
	!
	! Index:
	!	.x Report>Function
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_REPORT/LINE
	!	$ LINK/EXECUTABLE=UTL_EXE: UTL_REPORT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_REPORT.OBJ;*
	!
	! Author:
	!
	!	01/01/87 - Kevin Handy
	!
	! Modification history:
	!
	!	09/14/87 - Kevin Handy
	!		Modified to allow an entry to the main screen
	!		from MENU, by passing "**" through core common.
	!
	!	11/08/88 - Kevin Handy
	!		Fixed bug with background file - added job
	!		number in where it belongs.
	!
	!	05/20/91 - Kevin Handy
	!		Modified to define "DEFAULT" and "SYS$LOGIN" when
	!		creating a report to run in background.
	!
	!	07/16/91 - Kevin Handy
	!		Unwound external definitions.
	!
	!	03/13/92 - Kevin Handy
	!		Removed duplicate error trap (check)
	!
	!	05/15/92 - Frank F. Starman
	!		Use READ_35SET function.
	!
	!	061292 - Kevin Handy
	!		Cleaned up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/09/97 - Kevin Handy
	!		Use OUTP_3WRITESTRUCTURE.
	!
	!	05/19/97 - Kevin Handy
	!		Reformat source code
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/13/99 - Kevin Handy
	!		Fix calls to BEGIN/END_DISPLAY_UPDATE
	!		Use BASIC$STARLET for LIB$
	!
	!	06/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN.
	!		I think this thing needs a lot more REGARDLESS
	!
	!	09/19/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	06/26/2001 - Kevin Handy
	!		Change MAX_MAIN from 200 to 1600 so program would
	!		work again.
	!		Lose "Series" options which didn't work any longer.
	!--
	%PAGE

	!++
	! Abstract:COMMAND
	!	^*REPORT\*
	!	.p
	!	Provides a common method of
	!	processing reports.  This is done by standardizing the user
	!	interfaces to all reports.
	!	.P
	!	The ^*Report\* program is divided into various menus.  It
	!	starts in the select reports menu unless the chain entry
	!	is specified from the calling program.  If the chain entry
	!	is specified, the report program will start in the
	!	settings screen.
	!	.P
	!	Other commands in ^*Report\* allow the user to add new reports
	!	from the system report file, which is found on CMC:, set up
	!	batch process files, which will process several reports in
	!	order, erase a report, and series, which allows the user to
	!	select several reports to be processed on a one time basis.
	!	.p
	!	^*Format: REPORT\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level: /REPORT
	!	.end literal
	!
	! Index:
	!	.x Report>Function
	!
	! Option:
	!	UTL_MAST_DOC_DEST$HELP
	!	UTL_RPRT_REPORT$HELP
	!	UTL_PRINT$HELP
	!
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"

	MAP (SCOPE) SCOPE_STRUCT SCOPE
	MAP (PRINTX) PRINTX_CDD PRINTX

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE  SMG_SCROLL_CDD MAIN_SCROLL
	DECLARE  SMG_SCROLL_CDD SYSTEM_SCROLL
 !	DECLARE  SMG_SCROLL_CDD SERIES_SCROLL
	DECLARE  SMG_SCROLL_CDD BATCH1_SCROLL
	DECLARE  SMG_SCROLL_CDD BATCH2_SCROLL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT) UTL_REPORT_CDD UTL_REPORT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.HB"
	DECLARE UTL_SET_CDD UTL_SET_READ

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION READ_35SET
	EXTERNAL STRING FUNCTION READ_DEFAULT
	EXTERNAL INTEGER FUNCTION DSPL_SCROLL

	EXTERNAL LONG FUNCTION SYS$FILESCAN

	!
	! Dimension statements
	!
	DECLARE LONG SYS_STATUS
	DECLARE LONG CONSTANT PRC$M_DETACH = 512

	DECLARE LONG SMG_MAIN_WINDOW
	DECLARE INTEGER CONSTANT MAX_MAIN = 1600

	DIM MAIN_STRING$(MAX_MAIN)

	DECLARE LONG SMG_SYSTEM_WINDOW

	DECLARE INTEGER CONSTANT MAX_SYSTEM = 200

	DIM SYSTEM_STRING$(MAX_SYSTEM)

 !	DECLARE LONG SMG_SERIES_WINDOW

	DECLARE LONG CONTEXT, SMG_BATCH1, SMG_BATCH2
	DECLARE LONG TEMP_LONG, TEMP1_LONG
	MAP (IOBUF1) NAME.BUFFER$ = 50%
	MAP (IOBUF) LONG IO_BUF(6%)
	MAP (IOBUF) WORD IO_BUF_W(12%)

	DIM BATCH_REP$(100%), BATCH_BATCH$(100%)

	%PAGE

	XLOOP% = 0%

	JJ$ = EDIT$(READ_SYSJOB, -1%)

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Assign channels
	!
	CALL ASSG_CHANNEL(BATCH.CH%, STAT%)
	CALL ASSG_CHANNEL(PRNT.CH%, STAT%)
	CALL ASSG_CHANNEL(DET.CH%, STAT%)
	CALL ASSG_CHANNEL(WORK.CH%, STAT%)

	!
	! Get program name for help
	!
	SCOPE::PRG_IDENT = "PROG"
	SCOPE::PRG_PROGRAM = READ_SYSPN
	V% = READ_35SET("COMPNY", "NAME", UTL_SET_READ)
	SCOPE::PRG_COMPANY = UTL_SET_READ::SDATA

	CUR_SYS$ = ""

	!
	! Pull in core common
	!
	TEMP$ = SYS('7'C)
	IF ASCII(TEMP$) = 14%
	THEN
		TEMP1% = INSTR(1%, TEMP$, '15'C)
		IF TEMP1%
		THEN
			CUR_SYS$ = SEG$(TEMP$, 2%, TEMP1% - 1%)
			REPORT$ = EDIT$(RIGHT(TEMP$, TEMP1% + 1%), -1%)

			!
			! Special case to get to main screen from menu
			!
			IF REPORT$ = "**"
			THEN
				CUR_SYS$ = ""
				REPORT$ = ""
			END IF
		END IF
	END IF

300	!
	! Open REPORT file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.MOD"
	USE
		CONTINUE 305
	END WHEN

	GOTO 600

305	CALL ENTR_3MESSAGE(SCOPE, "Creating new report file", 1%)
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.CRE"

600	!
	! Open the print work file
	!
	OPTION$ = ""

620	XLOOP% = XLOOP% + 1%
	TEMPFILE$ = "PRNT" + JJ$ + "_" + NUM1$(XLOOP%) + ".TMP"

	WHEN ERROR IN
		OPEN TEMPFILE$ FOR INPUT AS FILE PRNT.CH%
	USE
		CONTINUE 630 IF ERR = 5%
		CONTINUE 620
	END WHEN

	CLOSE PRNT.CH%
	GOTO 620

630	CLOSE PRNT.CH%
	WHEN ERROR IN
		OPEN TEMPFILE$ FOR OUTPUT AS FILE PRNT.CH%
	USE
		CONTINUE 620 IF ERR = 154%
		FILENAME$ = TEMPFILE$
		CONTINUE HelpError
	END WHEN

	SYS_STATUS = LIB$SET_SYMBOL("CMC$REPORT", TEMPFILE$,)

	IF (SYS_STATUS AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to declare symbol for work file! " + &
			NUM1$(SYS_STATUS), 0%)
		GOTO 32767
	END IF

	%PAGE

700	!******************************************************************
	! If program is called with a report number given, then go into
	! the report settings menu.
	!******************************************************************

	IF REPORT$ = ""
	THEN
		!
		! Generic call.  Let them select a report.
		!
		JUST_REPORT% = 0%
		GOTO 1000
	END IF

	JUST_REPORT% = -1%

	WHEN ERROR IN
		GET #UTL_REPORT.CH%, KEY #0% EQ REPORT$
	USE
		IF ERR = 154%
		THEN
			FILENAME$ = SCOPE::PRG_PROGRAM
			CONTINUE HelpError
		END IF

		CONTINUE 710
	END WHEN

	GOTO 790

710	!
	! Open system report file if not open
	!
	IF UTL_SYSREP.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSREP.OPN"
		USE
			CALL ENTR_3MESSAGE(SCOPE, &
				"Unable to find report '" + REPORT$ + "'", 0%)
			CONTINUE ExitProgram
		END WHEN
	END IF

	WHEN ERROR IN
		GET #UTL_SYSREP.CH%, KEY #0% EQ REPORT$, REGARDLESS
		PUT #UTL_REPORT.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find report '" + REPORT$ + "'", 0%)
		CONTINUE ExitProgram
	END WHEN

	GOTO 700

790	PRGNAM$ = TRM$(UTL_REPORT::PRODEV) + &
		TRM$(UTL_REPORT::PRONAM)

	!
	! Initilize defaults from report file
	!
	CALL OUTP_INITSTRUCTURE(UTL_REPORT, UTL_REPORTX)

	!
	! Ask user to change settings
	!
	TEST$ = ""
	TEST$ = "RD " IF UTL_REPORTX::REPYN <> "N"
	CALL OUTP_SETTINGS(UTL_REPORT, UTL_REPORTX, UTL_REPORT.CH%, &
		"DD SF SP EP CP AS " + TEST$, "PT ")

	!
	! Un-normal abort, exit, etc.
	!
	IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
	THEN
		GOTO 4200
	END IF

	!
	! Set detach flag
	!
	NODETACH% = UTL_REPORTX::DETACH

	!
	! Write the data out to the ascii file
	!
	CALL OUTP_3WRITESTRUCTURE(UTL_REPORTX, PRNT.CH%, PRINTX)

	GOTO 4100

	%PAGE

1000	!*****************************************************************
	! Handle Main Menu
	!*****************************************************************

	!
	! Create new window
	!
	PREV_WINDOW = SMG_MAIN_WINDOW

	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_MAIN_WINDOW, &
		SMG$M_BORDER &
	)

	!
	! Get initial reports
	!
	GOSUB LoadMain
	V% = DSPL_SCROLL(MAIN_SCROLL, MAIN_STRING$(), 0%, "PAINT")

	!
	! Paste first listing on
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_MAIN_WINDOW, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	OPTFLAG% = 0%

1100	!***************************************************************
	! Main options screen
	!***************************************************************

	SCOPE::PRG_ITEM = ""
	OPT$ = "Report Erase_report Batch List_system_reports " + &
		"Documentation_of_reports Help eXit"

	OPTION$ = ENTR_3OPTION(SCOPE, "COMMAND", OPT$, OPT%, OPTFLAG%)

	!
	! Handle special keys
	!
	SELECT SCOPE::SCOPE_EXIT
	CASE 3%
		GOTO 1100

	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
		SMG$K_TRM_F18, SMG$K_TRM_F19

		V% = DSPL_SCROLL(MAIN_SCROLL, MAIN_STRING$(), &
			SCOPE::SCOPE_EXIT, "PAINT")
		GOTO 1100

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO 4200

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1100
	END SELECT

	OPTFLAG% = 0%

	!
	! Handle options
	!
	SELECT OPTION$

	!
	! Report
	!
	CASE "R"
1148		INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", "Enter report number", &
			LEFT(MAIN_STRING$(MAIN_SCROLL::CUR_LINE), 6%), &
			0%, "", ""), 32%)

		SELECT SCOPE::SCOPE_EXIT

		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		!
		! Handle arrow keys
		!
		CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
			SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
			SMG$K_TRM_F18, SMG$K_TRM_F19

			V% = DSPL_SCROLL(MAIN_SCROLL, MAIN_STRING$(), &
				SCOPE::SCOPE_EXIT, "PAINT")
			GOTO 1148

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 1148
		END SELECT

		GOTO 1100 IF INP$ = ""

1150		WHEN ERROR IN
			GET #UTL_REPORT.CH%, KEY #0% EQ INP$
		USE
			IF ERR = 154%
			THEN
				FILENAME$ = SCOPE::PRG_PROGRAM
				CONTINUE HelpError
			END IF

			CALL ENTR_3MESSAGE(SCOPE, "Report not found!", 0%)
			CONTINUE 1148
		END WHEN

		IF (CUR_SYS$ <> "") AND (CUR_SYS$ <> UTL_REPORT::SUBSYS)
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Report Not in this system", 0%)
			GOTO 1100
		END IF

		PRGNAM$ = TRM$(UTL_REPORT::PRODEV) + &
			TRM$(UTL_REPORT::PRONAM)

		!
		! Initilize defaults from report file
		!
		CALL OUTP_INITSTRUCTURE(UTL_REPORT, UTL_REPORTX)

		!
		! Ask user to change settings
		!
		TEST$ = ""
		TEST$ = "RD " IF UTL_REPORTX::REPYN <> "N"
		CALL OUTP_SETTINGS(UTL_REPORT, UTL_REPORTX, UTL_REPORT.CH%, &
			"DD SF SP EP CP AS " + TEST$, "PT ")

		!
		! Un-normal abort, exit, etc.
		!
		IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
		THEN
			SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY( &
				UTL_REPORTX::WINDOW)
			UTL_REPORTX::WINDOW = 0%
			GOTO 1100
		END IF

		!
		! Set detach flag
		!
		NODETACH% = UTL_REPORTX::DETACH

		!
		! Write the data out to the ascii file
		!
		CALL OUTP_3WRITESTRUCTURE(UTL_REPORTX, PRNT.CH%, PRINTX)

		GOTO 4100

	!
	! Erase report
	!
	CASE "E"
1158		INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", "Report number to erase", &
			LEFT(MAIN_STRING$(MAIN_SCROLL::CUR_LINE), 6%), &
			0%, "", ""), 32%)

		SELECT SCOPE::SCOPE_EXIT

		CASE 3%, 290%
			GOTO 1100

		!
		! Handle arrow keys
		!
		CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
			SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
			SMG$K_TRM_F18, SMG$K_TRM_F19

			V% = DSPL_SCROLL(MAIN_SCROLL, MAIN_STRING$(), &
				SCOPE::SCOPE_EXIT, "PAINT")
			GOTO 1158

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)

		END SELECT

		GOTO 1100 IF INP$ = ""

1160		WHEN ERROR IN
			GET #UTL_REPORT.CH%, KEY #0% EQ INP$
			DELETE #UTL_REPORT.CH%
		USE
			IF ERR = 154%
			THEN
				FILENAME$ = SCOPE::PRG_PROGRAM
				CONTINUE HelpError
			END IF

			CALL ENTR_3MESSAGE(SCOPE, "Report not found!", 0%)
			CONTINUE 1158
		END WHEN

		GOSUB LoadMain
		V% = DSPL_SCROLL(MAIN_SCROLL, MAIN_STRING$(), 0%, "PAINT")

		GOTO 1100

	!
	! Series reports
	!
 !	CASE "S"
 !		GOTO 1300

	!
	! Exit
	!
	CASE "X"
		GOTO 4200

	!
	! List system reports
	!
	CASE "L"
		GOTO 2000

	!
	! Batch reports
	!
	CASE "B"
		GOTO 3000

	!
	! Documentation of reports
	!
	CASE "D"
1170		INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", "Enter report number", &
			LEFT(MAIN_STRING$(MAIN_SCROLL::CUR_LINE), 6%), &
			0%, "", ""), 32%)

		SELECT SCOPE::SCOPE_EXIT

		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		!
		! Handle arrow keys
		!
		CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
			SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
			SMG$K_TRM_F18, SMG$K_TRM_F19

			V% = DSPL_SCROLL(MAIN_SCROLL, MAIN_STRING$(), &
				SCOPE::SCOPE_EXIT, "PAINT")
			GOTO 1170

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 1170
		END SELECT

		GOTO 1100 IF INP$ = ""

1180		WHEN ERROR IN
			GET #UTL_REPORT.CH%, KEY #0% EQ INP$
		USE
			IF ERR = 154%
			THEN
				FILENAME$ = SCOPE::PRG_PROGRAM
				CONTINUE HelpError
			END IF

			CALL ENTR_3MESSAGE(SCOPE, "Report not found!", 0%)
			CONTINUE 1170
		END WHEN

		KEEP_PRG$ = SCOPE::PRG_PROGRAM

		PRG_PROGNAM$ = "UTL_REPXXX_" + UTL_REPORT::REPNUM
		SCOPE::PRG_ITEM = "HELP"

		CALL HELP_3MESSAGE(SCOPE, "", "REPO", PRG_PROGNAM$, "HELP")

		SCOPE::PRG_PROGRAM = KEEP_PRG$

		GOTO 1170

	!
	! Help option
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, &
			"", "PROG", "UTL_REPORT", "HELP")
		GOTO 1100
	END SELECT

	GOTO 1100

	%PAGE

 !1300	******************************************************************
	! Series option.  Lets user run several reports in a row.
	!******************************************************************

 !	KEEP_PRG$ = SCOPE::PRG_PROGRAM
 !	SCOPE::PRG_PROGRAM = "UTL_REPORT_SERIES"
 !	SCOPE::PRG_ITEM = ""

	!
	! Create new window
	!
 !	PREV_WINDOW = SMG_SERIES_WINDOW
 !
 !	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
 !	( &
 !		18%, &
 !		8%, &
 !		SMG_SERIES_WINDOW, &
 !		SMG$M_BORDER &
 !	)
 !
 !	SMG_STATUS% = SMG$LABEL_BORDER &
 !	( &
 !		SMG_SERIES_WINDOW, &
 !		"Series" &
 !	)
 !
 !	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
 !	( &
 !		SMG_SERIES_WINDOW, &
 !		SCOPE::SMG_PBID, &
 !		2%, &
 !		72% &
 !	)
 !
	!
	! Set up structure
	!
 !	SERIES_SCROLL::WINDOW		= SMG_SERIES_WINDOW
 !	SERIES_SCROLL::TOP_ARRAY	= 1%
 !	SERIES_SCROLL::BOT_ARRAY	= 1%
 !	SERIES_SCROLL::SCROLL_TOP	= 1%
 !	SERIES_SCROLL::SCROLL_BOT	= 18%
 !	SERIES_SCROLL::BEG_ELEMENT	= 1%
 !	SERIES_SCROLL::END_ELEMENT	= 1%
 !	SERIES_SCROLL::TOP_LINE		= 1%
 !	SERIES_SCROLL::CUR_LINE		= 1%
 !	SERIES_SCROLL::CUR_W_ROW	= 1%
 !	SERIES_SCROLL::CUR_W_COL	= 1%
 !	SERIES_SCROLL::FIND_LINE	= 1%
 !	SERIES_SCROLL::SMG_FLAG		= 0%
 !	SERIES_SCROLL::PROMPT		= ""
 !	SERIES_SCROLL::VIDEO_COMP	= 0%
 !	SERIES_SCROLL::CHARSET		= 0%
 !	SERIES_SCROLL::DRAW_COLS	= ""
 !
 !	BATCH_REP$(1%) = ""
 !
 !	BATCH.LOOP.END% = 0%
 !
 !	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_SERIES_WINDOW)
 !	V% = DSPL_SCROLL(SERIES_SCROLL, BATCH_REP$(), 0%, "PAINT")
 !	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_SERIES_WINDOW)
 !
 !1310	!
	! Enter report numbers
	!
 !	REPORT$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, "", &
 !		"Report number", &
 !		LEFT(MAIN_STRING$(MAIN_SCROLL::CUR_LINE), 6%), &
 !		0%, "", ""), 32%)
 !
 !	SELECT SCOPE::SCOPE_EXIT
 !
 !	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
 !		GOTO 1340
 !
 !	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
 !		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
 !		SMG$K_TRM_F18, SMG$K_TRM_F19
 !
 !		V% = DSPL_SCROLL(MAIN_SCROLL, MAIN_STRING$(), &
 !			SCOPE::SCOPE_EXIT, "PAINT")
 !		GOTO 1310
 !
 !	CASE SMG$K_TRM_REMOVE
 !		IF BATCH.LOOP.END% >= 1%
 !		THEN
 !			BATCH.LOOP.END% = BATCH.LOOP.END% - 1%
 !			SERIES_SCROLL::BOT_ARRAY = BATCH.LOOP.END%
 !			SERIES_SCROLL::BOT_ARRAY = 1% &
 !				IF BATCH.LOOP.END% = 0%
 !			SERIES_SCROLL::FIND_LINE = SERIES_SCROLL::BOT_ARRAY
 !
 !			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE( &
 !				SMG_SERIES_WINDOW)
 !			V% = DSPL_SCROLL(SERIES_SCROLL, BATCH_REP$(), 0%, &
 !				"FIND")
 !			V% = DSPL_SCROLL(SERIES_SCROLL, BATCH_REP$(), 0%, &
 !				"PAINT")
 !			SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_SERIES_WINDOW)
 !		END IF
 !		GOTO 1310
 !
 !	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
 !
 !	CASE ELSE
 !		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
 !		GOTO 1310
 !	END SELECT
 !
 !	GOTO 1340 IF REPORT$ = ""
 !	BATCH.LOOP.END% = BATCH.LOOP.END% + 1%
 !	BATCH_REP$(BATCH.LOOP.END%) = REPORT$
 !
 !	SERIES_SCROLL::BOT_ARRAY, SERIES_SCROLL::END_ELEMENT = BATCH.LOOP.END%
 !	SERIES_SCROLL::FIND_LINE = BATCH.LOOP.END%
 !
 !	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_SERIES_WINDOW)
 !	V% = DSPL_SCROLL(SERIES_SCROLL, BATCH_REP$(), 0%, "FIND")
 !	V% = DSPL_SCROLL(SERIES_SCROLL, BATCH_REP$(), 0%, "PAINT")
 !	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_SERIES_WINDOW)
 !
 !	CALL ENTR_3MESSAGE(SCOPE, &
 !		NUM1$(BATCH.LOOP.END%) + " reports selected", 1%)
 !
 !	WHEN ERROR IN
 !		GET #UTL_REPORT.CH%, KEY #0% EQ REPORT$
 !	USE
 !		IF ERR = 154%
 !		THEN
 !			FILENAME$ = SCOPE::PRG_PROGRAM
 !			CONTINUE HelpError
 !		END IF
 !
 !		CALL ENTR_3MESSAGE(SCOPE, "Unable to find report", 0%)
 !		CONTINUE 1310
 !	END WHEN
 !
 !1320	!
 !
 !1330	GOTO 1310 IF BATCH.LOOP.END% < 36%
 !
 !1340	YESNO$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, "", &
 !		"Continue ", "N", 0%, "", "")
 !
 !	SELECT SCOPE::SCOPE_EXIT
 !
 !	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
 !		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SERIES_WINDOW)
 !		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
 !
 !		SCOPE::PRG_PROGRAM = KEEP_PRG$
 !		SMG_SERIES_WINDOW = PREV_WINDOW
 !
 !		GOTO 1100
 !
 !	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
 !
 !	CASE ELSE
 !		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
 !		GOTO 1340
 !
 !	END SELECT
 !
 !	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SERIES_WINDOW)
 !	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
 !
 !	SCOPE::PRG_PROGRAM = KEEP_PRG$
 !	SMG_SERIES_WINDOW = PREV_WINDOW
 !
 !	IF YESNO$ <> "Y"
 !	THEN
 !
 !		GOTO 1100
 !	END IF
 !
 !	GOTO 4000
 !
	%PAGE

2000	!******************************************************************
	! List system reports
	!******************************************************************

	OPT1% = 0%

	KEEP_PRG$ = SCOPE::PRG_PROGRAM
	SCOPE::PRG_PROGRAM = "UTL_REPORT_SYSTEM"

	!
	! Create new window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		38%, &
		SMG_SYSTEM_WINDOW, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_SYSTEM_WINDOW, &
		"System Report File" &
	)

	!
	! Open system report file if not open
	!
	IF UTL_SYSREP.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSREP.OPN"
		USE
			CALL ENTR_3MESSAGE(SCOPE, &
				"No system reports available!" + ERT$(ERR), 0%)
			CONTINUE 2900
		END WHEN
	END IF

	!
	! Paint Background
	!
	GOSUB LoadSystem
	V% = DSPL_SCROLL(SYSTEM_SCROLL, SYSTEM_STRING$(), 0%, "PAINT")

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SYSTEM_WINDOW, &
		SCOPE::SMG_PBID, &
		2%, &
		42% &
	)


	%PAGE

2050	!******************************************************************
	! System reports option menu
	!******************************************************************

	SCOPE::PRG_ITEM = ""
	OPT$ = "Copy Documentation_of_reports Help eXit"
	OPTION$ = ENTR_3OPTION(SCOPE, "COMMAND", OPT$, OPT1%, FLAGW%)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%
		GOTO 2900

	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
		SMG$K_TRM_F18, SMG$K_TRM_F19

		V% = DSPL_SCROLL(SYSTEM_SCROLL, SYSTEM_STRING$(), &
			SCOPE::SCOPE_EXIT, "PAINT")
		GOTO 2050

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO 2900

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 2050

	END SELECT

	SELECT OPTION$

	!
	! Copy system
	!
	CASE "C"
2098		INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", "Copy which report", &
			LEFT(SYSTEM_STRING$(SYSTEM_SCROLL::CUR_LINE), 6%), &
			0%, "", ""), 32%)

		!
		! Handle special functions
		!
		SELECT SCOPE::SCOPE_EXIT

		CASE 3%, 290%
			GOTO 2050

		CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
			SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
			SMG$K_TRM_F18, SMG$K_TRM_F19

			V% = DSPL_SCROLL(SYSTEM_SCROLL, SYSTEM_STRING$(), &
				SCOPE::SCOPE_EXIT, "PAINT")
			GOTO 2098

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 2098
		END SELECT

		GOTO 2050 IF INP$ = ""

2100		GET #UTL_SYSREP.CH%, KEY #0% EQ INP$, REGARDLESS

2102		INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, "", &
			"Copy to report", &
			UTL_REPORT::REPNUM, 0%, "", ""), 32%)

		!
		! Handle special keys
		!
		SELECT SCOPE::SCOPE_EXIT

		CASE 3%, 290%
			GOTO 2050

		END SELECT

2110		UTL_REPORT::REPNUM = INP$
		WHEN ERROR IN
			PUT #UTL_REPORT.CH%
		USE
			CALL ENTR_3MESSAGE(SCOPE, "Duplicate Report # ", 0%)
			CONTINUE 2050
		END WHEN

		SCOPE::PRG_PROGRAM = KEEP_PRG$

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_MAIN_WINDOW)
		GOSUB LoadMain
		V% = DSPL_SCROLL(MAIN_SCROLL, MAIN_STRING$(), 0%, &
			"PAINT")
		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_MAIN_WINDOW)

		GOTO 2050

	!
	! Documentation of reports
	!
	CASE "D"
2170		INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, "", &
			"Enter report number", &
			LEFT(SYSTEM_STRING$(SYSTEM_SCROLL::CUR_LINE), 6%), &
			0%, "", ""), 32%)

		SELECT SCOPE::SCOPE_EXIT

		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 2050

		!
		! Handle arrow keys
		!
		CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
			SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
			SMG$K_TRM_F18, SMG$K_TRM_F19

			V% = DSPL_SCROLL(SYSTEM_SCROLL, SYSTEM_STRING$(), &
				SCOPE::SCOPE_EXIT, "PAINT")
			GOTO 2170

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 2170
		END SELECT

		GOTO 2050 IF INP$ = ""

2180		WHEN ERROR IN
			GET #UTL_SYSREP.CH%, KEY #0% EQ INP$, REGARDLESS
		USE
			CALL ENTR_3MESSAGE(SCOPE, "Report not found!", 0%)
			CONTINUE 2170
		END WHEN

		KEEP_PRG$ = SCOPE::PRG_PROGRAM
		PRG_PROGNAM$ = TRM$(UTL_REPORT::PRONAM)
		SCOPE::PRG_ITEM = ""

		CALL HELP_3MESSAGE(SCOPE, "", "REPO", PRG_PROGNAM$, "HELP")

		SCOPE::PRG_PROGRAM = KEEP_PRG$

		GOTO 2170

	!
	! Help
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", &
			KEEP_PRG$, "List_system_reports")
		GOTO 2050

	!
	! Exit
	!
	CASE "X"
		GOTO 2900

	END SELECT

	GOTO 2050

	%PAGE

2900	!
	! Return back
	!
	SCOPE::PRG_PROGRAM = KEEP_PRG$
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SYSTEM_WINDOW)

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_MAIN_WINDOW)
	GOSUB LoadMain
	V% = DSPL_SCROLL(MAIN_SCROLL, MAIN_STRING$(), 0%, "PAINT")
	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_MAIN_WINDOW)

	GOTO 1100

	%PAGE

3000	!******************************************************************
	! Batch option.  Lets user create a list of reports to run in a
	! row, and store that list in a file.
	!******************************************************************

	KEEP_PRG$ = SCOPE::PRG_PROGRAM
	SCOPE::PRG_PROGRAM = "UTL_REPORT_BATCH"

	!
	! Create new window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		8%, &
		SMG_BATCH1, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_BATCH1, &
		"Batch" &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_BATCH1, &
		SCOPE::SMG_PBID, &
		2%, &
		63% &
	)

	!
	! Create new window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		8%, &
		SMG_BATCH2, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_BATCH2, &
		"Reports" &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_BATCH2, &
		SCOPE::SMG_PBID, &
		2%, &
		72% &
	)


3010	!***************************************************************
	! LIST BATCH FILES
	!***************************************************************

	!
	! Set up for call
	!

	OPT2% = 0%

	GOSUB LoadBatch
	V% = DSPL_SCROLL(BATCH1_SCROLL, BATCH_BATCH$(), 0%, "PAINT")

	%PAGE

3050	!******************************************************************
	! Batch options
	!******************************************************************

	SCOPE::PRG_ITEM = ""
	OPT$ = "Create_batch Erase_batch Select_batch Help eXit"
	INP$ = ENTR_3OPTION(SCOPE, "COMMAND", OPT$, OPT2%, FLAGW%)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
		SMG$K_TRM_F18, SMG$K_TRM_F19

		V% = DSPL_SCROLL(BATCH1_SCROLL, BATCH_BATCH$(), &
			SCOPE::SCOPE_EXIT, "PAINT")
		GOTO 3050

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_BATCH1)
		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_BATCH2)

		SCOPE::PRG_PROGRAM = KEEP_PRG$

		GOTO 1100

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 3050

	END SELECT

	SELECT INP$

	!
	! Create batch
	!
	CASE "C"
		GOTO 3100

	!
	! Erase batch
	!
	CASE "E"
3060		INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", "Erase batch name", &
			LEFT(BATCH_BATCH$(BATCH1_SCROLL::CUR_LINE), 6%), &
			0%, "", ""), 32%)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
			SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
			SMG$K_TRM_F18, SMG$K_TRM_F19

			V% = DSPL_SCROLL(BATCH1_SCROLL, BATCH_BATCH$(), &
				SCOPE::SCOPE_EXIT, "PAINT")
			GOTO 3060

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 3050

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 3050

		END SELECT

		GOTO 3050 IF INP$ = ""

		YESNO$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, "", &
			"Confirm", "N", 0%, "", "")
		GOTO 3050 IF YESNO$ <> "Y"

 !		WHEN ERROR IN
 !			KILL INP$ + ".BTH"
 !		USE
 !			CALL ENTR_3MESSAGE(SCOPE, "No such batch to erase", 0%)
 !			CONTINUE 3050
 !		END WHEN

		SMG_STATUS% = LIB$DELETE_FILE(INP$ + ".BTH;*")

		GOTO 3010

	!
	! Select batch
	!
	CASE "S"
		GOTO 3300

	!
	! Exit
	!
	CASE "X"
		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_BATCH1)
		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_BATCH2)

		SCOPE::PRG_PROGRAM = KEEP_PRG$

		GOTO 1100

	!
	! Help
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, &
			"", "PROG", KEEP_PRG$, "BATCH")
		GOTO 3050

	END SELECT

	GOTO 3050

	%PAGE

3100	!**************************************************************
	! Create a new batch file
	!**************************************************************

	!
	! Set up structure
	!
	BATCH2_SCROLL::WINDOW		= SMG_BATCH2
	BATCH2_SCROLL::TOP_ARRAY	= 1%
	BATCH2_SCROLL::BOT_ARRAY	= 1%
	BATCH2_SCROLL::SCROLL_TOP	= 1%
	BATCH2_SCROLL::SCROLL_BOT	= 18%
	BATCH2_SCROLL::BEG_ELEMENT	= 1%
	BATCH2_SCROLL::END_ELEMENT	= 1%
	BATCH2_SCROLL::TOP_LINE		= 1%
	BATCH2_SCROLL::CUR_LINE		= 1%
	BATCH2_SCROLL::CUR_W_ROW	= 1%
	BATCH2_SCROLL::CUR_W_COL	= 1%
	BATCH2_SCROLL::FIND_LINE	= 1%
	BATCH2_SCROLL::SMG_FLAG		= 0%
	BATCH2_SCROLL::PROMPT		= ""
	BATCH2_SCROLL::VIDEO_COMP	= 0%
	BATCH2_SCROLL::CHARSET		= 0%
	BATCH2_SCROLL::DRAW_COLS	= ""

	BATCH_REP$(1%) = ""

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(BATCH2_SCROLL::WINDOW)
	V% = DSPL_SCROLL(BATCH2_SCROLL, BATCH_REP$(), 0%, "FIND")
	V% = DSPL_SCROLL(BATCH2_SCROLL, BATCH_REP$(), 0%, "PAINT")
	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(BATCH2_SCROLL::WINDOW)

	BATCH2_SCROLL::BOT_ARRAY, BATCH2_SCROLL::END_ELEMENT = 0%

	INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"", "Enter new batch name", "      ", 0%, "", ""), 32%)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
		SMG$K_TRM_F18, SMG$K_TRM_F19

		V% = DSPL_SCROLL(BATCH1_SCROLL, BATCH_BATCH$(), &
			SCOPE::SCOPE_EXIT, "PAINT")
		GOTO 3100

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO 3050

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 3050

	END SELECT

	GOTO 3050 IF INP$ = ""

	WHEN ERROR IN
		OPEN INP$ + ".BTH" FOR INPUT AS FILE BATCH.CH%, &
			ACCESS READ, ALLOW MODIFY
	USE
		CONTINUE 3110
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "That file already exists", 1%)
	YESNO$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, "", &
		"Re-create", "N", 0%, "", "")
	GOTO 3100 IF YESNO$ <> "Y"

3110	OPEN INP$ + ".BTH" FOR OUTPUT AS FILE BATCH.CH%

	IF BATCH_BATCH$(1%) <> ""
	THEN
		BATCH1_SCROLL::BOT_ARRAY, BATCH1_SCROLL::END_ELEMENT = &
			BATCH1_SCROLL::BOT_ARRAY + 1%
		BATCH1_SCROLL::FIND_LINE = BATCH1_SCROLL::BOT_ARRAY
	END IF

	BATCH_BATCH$(BATCH1_SCROLL::BOT_ARRAY) = INP$

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(BATCH1_SCROLL::WINDOW)
	V% = DSPL_SCROLL(BATCH1_SCROLL, BATCH_BATCH$(), 0%, "FIND")
	V% = DSPL_SCROLL(BATCH1_SCROLL, BATCH_BATCH$(), 0%, "PAINT")
	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(BATCH1_SCROLL::WINDOW)

	BATCH_COUNT% = 0%

3120	INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, "", &
		"Enter report number for batch", &
		LEFT(MAIN_STRING$(MAIN_SCROLL::CUR_LINE), 6%), &
		0%, "", ""), 32%)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO 3130

	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
		SMG$K_TRM_F18, SMG$K_TRM_F19

		V% = DSPL_SCROLL(MAIN_SCROLL, MAIN_STRING$(), &
			SCOPE::SCOPE_EXIT, "PAINT")
		GOTO 3120

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 3120

	END SELECT

	GOTO 3130 IF INP$ = ""

	WHEN ERROR IN
		GET #UTL_REPORT.CH%, KEY #0% EQ INP$
	USE
		IF ERR = 154%
		THEN
			FILENAME$ = SCOPE::PRG_PROGRAM
			CONTINUE HelpError
		END IF

		CALL ENTR_3MESSAGE(SCOPE, "Unable to find report!", 0%)
		CONTINUE 3120
	END WHEN

	PRINT #BATCH.CH%, INP$

	BATCH_COUNT% = BATCH_COUNT% + 1%
	BATCH_REP$(BATCH_COUNT%) = INP$

	BATCH2_SCROLL::BOT_ARRAY, BATCH2_SCROLL::END_ELEMENT = BATCH_COUNT%
	BATCH2_SCROLL::FIND_LINE = BATCH_COUNT%

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(BATCH2_SCROLL::WINDOW)
	V% = DSPL_SCROLL(BATCH2_SCROLL, BATCH_REP$(), 0%, "FIND")
	V% = DSPL_SCROLL(BATCH2_SCROLL, BATCH_REP$(), 0%, "PAINT")
	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(BATCH2_SCROLL::WINDOW)

3125	GOTO 3120

3130	CLOSE BATCH.CH%
	GOTO 3100

	%PAGE

3300	!***************************************************************
	! Select a batch report
	!***************************************************************

	INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, "", "Select batch", &
		BATCH_BATCH$(BATCH1_SCROLL::CUR_LINE), &
		0%, "", ""), 32%)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
		SMG$K_TRM_F18, SMG$K_TRM_F19

		V% = DSPL_SCROLL(BATCH1_SCROLL, BATCH_BATCH$(), &
			SCOPE::SCOPE_EXIT, "PAINT")
		GOTO 3300

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO 3050

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 3300

	END SELECT

	GOTO 3050 IF INP$ = ""

	WHEN ERROR IN
		OPEN INP$ + ".BTH" FOR INPUT AS FILE BATCH.CH%, &
			ACCESS READ, ALLOW MODIFY
	USE
		CONTINUE 3300
	END WHEN

	BATCH.LOOP.END% = 0%

3310	WHEN ERROR IN
		INPUT LINE #BATCH.CH%, REPORT$
	USE
		CONTINUE 3370 IF ERR = 11%
		FILENAME$ = "BATCH"
		CONTINUE HelpError
	END WHEN

	REPORT$ = LEFT(EDIT$(REPORT$, 4%) + "      ", 6%)

	GOTO 3310 IF REPORT$ = ""

3315	WHEN ERROR IN
		GET #UTL_REPORT.CH%, KEY #0% EQ REPORT$
	USE
		IF ERR = 154%
		THEN
			FILENAME$ = SCOPE::PRG_PROGRAM
			CONTINUE HelpError
		END IF

		CALL ENTR_3MESSAGE(SCOPE, &
			"Undefined report: '" + REPORT$ + "'", 0%)
		CONTINUE 3310
	END WHEN

	BATCH.LOOP.END% = BATCH.LOOP.END% + 1%
	BATCH_REP$(BATCH.LOOP.END%) = LEFT(EDIT$(REPORT$, -1%) + "      ", 6%)

	GOTO 3310

3370	CLOSE BATCH.CH%

	!
	! Set up structure
	!
	BATCH2_SCROLL::WINDOW		= SMG_BATCH2
	BATCH2_SCROLL::TOP_ARRAY	= 1%
	BATCH2_SCROLL::BOT_ARRAY	= BATCH.LOOP.END%
	BATCH2_SCROLL::SCROLL_TOP	= 1%
	BATCH2_SCROLL::SCROLL_BOT	= 18%
	BATCH2_SCROLL::BEG_ELEMENT	= 1%
	BATCH2_SCROLL::END_ELEMENT	= BATCH.LOOP.END%
	BATCH2_SCROLL::TOP_LINE		= 1%
	BATCH2_SCROLL::CUR_LINE		= 1%
	BATCH2_SCROLL::FIND_LINE	= 1%
	BATCH2_SCROLL::SMG_FLAG		= 0%
	BATCH2_SCROLL::PROMPT		= ""
	BATCH2_SCROLL::VIDEO_COMP	= 0%
	BATCH2_SCROLL::CHARSET		= 0%
	BATCH2_SCROLL::DRAW_COLS	= ""

	V% = DSPL_SCROLL(BATCH2_SCROLL, BATCH_REP$(), 0%, "PAINT")

	CALL ENTR_3MESSAGE(SCOPE, &
		"Batching " + NUM1$(BATCH.LOOP.END%) + " items", 1%)

3380	YESNO$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, "", &
		"Continue ", "N", 0%, "", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO 3050

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 3380

	END SELECT

	GOTO 3050 IF YESNO$ <> "Y"

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_BATCH1)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_BATCH2)

	SCOPE::PRG_PROGRAM = KEEP_PRG$

	%PAGE

4000	!*********************************************************************
	! Set up for batch or series
	!*********************************************************************

	NODETACH% = 0%

	FOR BATCH.LOOP% = 1% TO BATCH.LOOP.END%

		GET #UTL_REPORT.CH%, KEY #0% EQ &
			BATCH_REP$(BATCH.LOOP%)

		PRGNAM$ = TRM$(UTL_REPORT::PRODEV) + &
				TRM$(UTL_REPORT::PRONAM) &
					IF BATCH.LOOP% = 1%

		!
		! Initilize defaults from report file
		!
		CALL OUTP_INITSTRUCTURE(UTL_REPORT, UTL_REPORTX)

		!
		! Ask user to change settings
		!
		TEST$ = ""
		TEST$ = "RD " IF UTL_REPORTX::REPYN <> "N"
		CALL OUTP_SETTINGS(UTL_REPORT, UTL_REPORTX, UTL_REPORT.CH%, &
			"DD SF SP EP CP AS " + TEST$, "PT ")

		!
		! Un-normal abort, exit, etc.
		!
		IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
		THEN
			SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY( &
				UTL_REPORTX::WINDOW)
			UTL_REPORTX::WINDOW = 0%
			GOTO 1100
		END IF

		!
		! Set detach flag
		!
		NODETACH% = NODETACH% OR UTL_REPORTX::DETACH

		!
		! Write the data out to the ascii file
		!
		CALL OUTP_3WRITESTRUCTURE(UTL_REPORTX, PRNT.CH%, PRINTX)

	NEXT BATCH.LOOP%

	%PAGE

4100	!***************************************************************
	! Chain to print program
	!***************************************************************

	CLOSE PRNT.CH%

4120	!
	! Test to see if detaching is allowed
	!
	GOTO 4150 IF NODETACH%

	!
	! Should it detach?
	!
	INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"", "Run Background or Normally (B/N*)", "N", &
		4%, "", ""), -1%)

	IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
	THEN
		OPTION$ = ""
		CALL ENTR_3MESSAGE(SCOPE, "Aborting", 0%)
		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(UTL_REPORTX::WINDOW)
		GOTO 4200 IF JUST_REPORT%
		GOTO 1100
	END IF

	GOTO 4150 IF EDIT$(INP$, -1%) <> "B"

	!
	! Attempt to detach
	!
	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

	DLOOP% = -1%

4130	!
	! Create the command file
	!
	DLOOP% = DLOOP% + 1%
	DET.TEMPFILE$ = "DET_" + READ_SYSJOB + "_" + NUM1$(DLOOP%) + ".TMP"

	WHEN ERROR IN
		OPEN DET.TEMPFILE$ FOR INPUT AS FILE DET.CH%
		CLOSE #DET.CH%
	USE
		CONTINUE 4135 IF ERR = 5%
		CONTINUE 4130
	END WHEN

	GOTO 4130

4135	OPEN DET.TEMPFILE$ AS FILE DET.CH%

4140	PRINT #DET.CH%, "$ SET DEFAULT "; READ_DEFAULT
	PRINT #DET.CH%, "$ DEF SYS$LOGIN "; READ_SYSLOG("SYS$LOGIN")
	PRINT #DET.CH%, "$ CMC$REPORT :== " + TEMPFILE$
	PRINT #DET.CH%, "$ RUN "  + PRGNAM$

	CLOSE DET.CH%

	CALL SUBR_SUBMIT(DET.TEMPFILE$, THESTAT%)

	!
	! Exit program
	!
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_MAIN_WINDOW)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(UTL_REPORTX::WINDOW)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

4150	!
	! Not detaching section
	!
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_MAIN_WINDOW)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(UTL_REPORTX::WINDOW)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

4170	CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + PRGNAM$, TEMPFILE$)

	%PAGE

 ExitProgram:
4200	!***************************************************************
	! Exit program
	!***************************************************************

	CLOSE PRNT.CH%

4210 !	WHEN ERROR IN
 !		KILL TEMPFILE$ WHILE (-1%)
 !	USE
 !		CONTINUE 4220
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(TEMPFILE$ + ";*")

4220	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_MAIN_WINDOW)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 LoadMain:
12000	!*******************************************************************
	! Load main list
	!*******************************************************************

	BATCH.LOOP.END% = 0%

	MAIN_STRING$(1%) = ""

12007	WHEN ERROR IN
		IF (CUR_SYS$ = "")
		THEN
			RESET #UTL_REPORT.CH%
		ELSE
			FIND #UTL_REPORT.CH%, KEY #2% GE CUR_SYS$
		END IF
	USE
		IF ERR = 154%
		THEN
			FILENAME$ = SCOPE::PRG_PROGRAM
			CONTINUE HelpError
		END IF

		CONTINUE 12090
	END WHEN

12010	WHEN ERROR IN
		GET #UTL_REPORT.CH%, REGARDLESS
	USE
		CONTINUE 12090
	END WHEN

	IF (CUR_SYS$ = "") OR (CUR_SYS$ = UTL_REPORT::SUBSYS)
	THEN
		!
		! Store report number for arrow keys
		!
		BATCH.LOOP.END% = BATCH.LOOP.END% + 1%
		MAIN_STRING$(BATCH.LOOP.END%) = UTL_REPORT::REPNUM + "  " + &
			UTL_REPORT::REPDES

		GOTO 12010
	END IF

12090	!
	! Set up structure
	!
	MAIN_SCROLL::WINDOW		= SMG_MAIN_WINDOW
	MAIN_SCROLL::TOP_ARRAY		= 1%
	MAIN_SCROLL::BOT_ARRAY		= BATCH.LOOP.END%
	MAIN_SCROLL::BOT_ARRAY		= 1% IF BATCH.LOOP.END% = 0%
	MAIN_SCROLL::SCROLL_TOP		= 1%
	MAIN_SCROLL::SCROLL_BOT		= 18%
	MAIN_SCROLL::BEG_ELEMENT	= 1%
	MAIN_SCROLL::END_ELEMENT	= MAIN_SCROLL::BOT_ARRAY
	MAIN_SCROLL::TOP_LINE		= 1%
	MAIN_SCROLL::CUR_LINE		= 1%
	MAIN_SCROLL::CUR_W_ROW		= 1%
	MAIN_SCROLL::CUR_W_COL		= 1%
	MAIN_SCROLL::FIND_LINE		= 1%
	MAIN_SCROLL::SMG_FLAG		= 0%
	MAIN_SCROLL::PROMPT		= "->"
	MAIN_SCROLL::VIDEO_COMP		= 0%
	MAIN_SCROLL::CHARSET		= 0%
	MAIN_SCROLL::DRAW_COLS		= ""

	RETURN

	%PAGE

 LoadSystem:
13000	!********************************************************************
	! Paint reports available
	!********************************************************************

	WHEN ERROR IN
		RESET #UTL_SYSREP.CH%
	USE
		CONTINUE 13050
	END WHEN

	BATCH.LOOP.END% = 0%

13010	WHEN ERROR IN
		GET #UTL_SYSREP.CH%, REGARDLESS
	USE
		CONTINUE 13050
	END WHEN

	!
	! Store report number for arrow keys
	!
	BATCH.LOOP.END% = BATCH.LOOP.END% + 1%
	SYSTEM_STRING$(BATCH.LOOP.END%) = UTL_REPORT::REPNUM + " " + &
		UTL_REPORT::REPDES

	GOTO 13010

13050	!
	! Set up structure
	!
	SYSTEM_SCROLL::WINDOW		= SMG_SYSTEM_WINDOW
	SYSTEM_SCROLL::TOP_ARRAY	= 1%
	SYSTEM_SCROLL::BOT_ARRAY	= BATCH.LOOP.END%
	SYSTEM_SCROLL::BOT_ARRAY	= 1% IF BATCH.LOOP.END% = 0%
	SYSTEM_SCROLL::SCROLL_TOP	= 1%
	SYSTEM_SCROLL::SCROLL_BOT	= 18%
	SYSTEM_SCROLL::BEG_ELEMENT	= 1%
	SYSTEM_SCROLL::END_ELEMENT	= SYSTEM_SCROLL::BOT_ARRAY
	SYSTEM_SCROLL::TOP_LINE		= 1%
	SYSTEM_SCROLL::CUR_LINE		= 1%
	SYSTEM_SCROLL::CUR_W_ROW	= 1%
	SYSTEM_SCROLL::CUR_W_COL	= 1%
	SYSTEM_SCROLL::FIND_LINE	= 1%
	SYSTEM_SCROLL::SMG_FLAG		= 0%
	SYSTEM_SCROLL::PROMPT		= "->"
	SYSTEM_SCROLL::VIDEO_COMP	= 0%
	SYSTEM_SCROLL::CHARSET		= 0%
	SYSTEM_SCROLL::DRAW_COLS	= ""

	RETURN

	%PAGE

 LoadBatch:
14000	!*********************************************************************
	! Work over the WILD file
	!*********************************************************************

	BATCH_BATCH$(1%) = ""

	FILE.NAME$ = "*.BTH"

14003	BATCH_COUNT% = 0%
	CONTEXT = 0%

14005	!
	! Look up one file
	!
	SYS_STATUS = LIB$FIND_FILE(FILE.NAME$ BY DESC, &
		WILD.FILE$ BY DESC, &
		CONTEXT BY REF, &
		, &
		, &
		COND% BY REF, &
		0% BY REF)

	GOTO 14050 IF (SYS_STATUS AND 1%) = 0%

	!
	! Rip off file name
	!
	NAME.BUFFER$ = WILD.FILE$
	SYS_STATUS = SYS$FILESCAN(NAME.BUFFER$ BY DESC, &
		IO_BUF() BY REF, SYS1_STATUS)
	TEMP_LONG = IO_BUF(1%)
	TEMP1_LONG = LOC(NAME.BUFFER$)
	TEMP_LONG = TEMP_LONG - TEMP1_LONG + 1%
	WILD.FILE$ = MID(NAME.BUFFER$, TEMP_LONG, IO_BUF_W(0%))

	!
	! Add file to list
	!
	BATCH_COUNT% = BATCH_COUNT% + 1%
	WHEN ERROR IN
		BATCH_BATCH$(BATCH_COUNT%) = WILD.FILE$
	USE
		CONTINUE 14050	! Too many files
	END WHEN

	!
	! Set up for next file
	!
	GOTO 14005

14050	SYS_STATUS% = LIB$FIND_FILE_END(CONTEXT BY REF)

	!
	! Set up structure
	!
	BATCH1_SCROLL::WINDOW		= SMG_BATCH1
	BATCH1_SCROLL::TOP_ARRAY	= 1%
	BATCH1_SCROLL::BOT_ARRAY	= BATCH_COUNT%
	BATCH1_SCROLL::BOT_ARRAY	= 1% IF BATCH_COUNT% = 0%
	BATCH1_SCROLL::SCROLL_TOP	= 1%
	BATCH1_SCROLL::SCROLL_BOT	= 18%
	BATCH1_SCROLL::TOP_LINE		= 1%
	BATCH1_SCROLL::BEG_ELEMENT	= 1%
	BATCH1_SCROLL::END_ELEMENT	= BATCH1_SCROLL::BOT_ARRAY
	BATCH1_SCROLL::CUR_LINE		= 1%
	BATCH1_SCROLL::CUR_W_ROW	= 1%
	BATCH1_SCROLL::CUR_W_COL	= 1%
	BATCH1_SCROLL::FIND_LINE	= 1%
	BATCH1_SCROLL::SMG_FLAG		= 0%
	BATCH1_SCROLL::PROMPT		= "->"
	BATCH1_SCROLL::VIDEO_COMP	= 0%
	BATCH1_SCROLL::CHARSET		= 0%
	BATCH1_SCROLL::DRAW_COLS	= ""

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************

	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!***************************************************************
	! E R R O R   T R A P P I N G   C O D E
	!***************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = SCOPE::PRG_PROGRAM
	RESUME HelpError

32767	END
