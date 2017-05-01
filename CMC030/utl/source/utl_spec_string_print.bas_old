1	%TITLE "Print String Reports"
	%SBTTL "UTL_SPEC_STRING_PRINT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print String Reports\* option in the Utility Report String menu provides
	!	the means to select and print a specific string of reports.
	!	.lm -5
	!
	! Index:
	!	.x Reports>Print>String>
	!	.x Print>String>Reports
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_SPEC_STRING_PRINT/LINE
	!	$ LINK/EXECUTABLE=UTL_EXE: UTL_SPEC_STRING_PRINT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_SPEC_STRING_PRINT.OBJ;*
	!
	! Author:
	!
	!	01/14/89 - Kevin Handy
	!
	! Modification history:
	!
	!	02/10/89 - Kevin Handy
	!		Modified for changes in ENTR_NOLSTRING
	!
	!	06/07/89 - Kevin Handy
	!		Started modifications to handle output file
	!		in the string_print file.
	!
	!	09/23/89 - Kevin Handy
	!		modified to use ENTR_ENTER instead of
	!		ENTR_NOLSTRING.
	!
	!	05/23/91 - Kevin Handy
	!		Modified to define "DEFAULT" and "SYS$LOGIN" when
	!		creating a report to run in background.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last param to entr_3choice
	!
	!	04/26/95 - Kevin Handy
	!		Lost deletion of SMG_MAIN_WINDOW display, which
	!		was never created.
	!
	!	08/14/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/09/97 - Kevin Handy
	!		Use OUTP_3WRITESTRUCTURE.
	!
	!	09/16/97 - Kevin Handy
	!		Lose code for clipboard, which wasn't usable.
	!		Lose code for File Cabinet, which didn't work.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/13/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$
	!
	!	09/19/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	!++
	! Abstract:COMMAND
	!	^*STRING/PRINT\*
	!	.b
	!	.lm +5
	!	Provides the means to select and print a specific string
	!	of reports.
	!	.b
	!	^*Format: STRING/PRINT\*
	!	.b
	!	^*Example:\*
	!	.b
	!	Menu Command Level: STRING/PRINT
	!	.lm -5
	!
	! Index:
	!	.x STRING/PRINT
	!	.x Reports>Print>String>
	!	.x Print>String>Reports
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include statements
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$DVIDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STRING_PRINT.HB"
	MAP (UTL_STRING_PRINT)	UTL_STRING_PRINT_CDD	UTL_STRING_PRINT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD	UTL_REPORT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE			SMG_SCROLL_CDD	SMG_SCROLL

	COM (PRINTX) PRINTX_CDD PRINTX

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	FIND_3PRINTGROUPITEM
	EXTERNAL LONG			OUTP_XUNSOL
						! (It's really an AST routine)

	EXTERNAL LONG	FUNCTION	DSPL_SCROLL
	EXTERNAL STRING	FUNCTION	OUTP_CREATESTR
	EXTERNAL STRING	FUNCTION	READ_DEVID
	EXTERNAL STRING FUNCTION	READ_DEFAULT

	!
	! Dimension statements
	!
	DIM REP$(2000%), SELECTED%(2000%)
	DIM RFA POINTER.RFA(2000%)

	DIM XTYPE$(20%), OTYPE%(30%), OTYPE$(30%)

	DECLARE LONG XTEMP, YTEMP

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize process
	!*******************************************************************

	JOBNUM$ = READ_SYSJOB

	CALL READ_INITIALIZE

32	!
	! Various output types that can be displayed (0 - 5)
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
		"Dif"

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
		11,	"DIF", &
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
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%,		! 20 Rows &
		78%,		! 80 Columns &
		SMG_SCREEN_DATA%,	! Identifier &
		SMG$M_BORDER	! Put a border around it &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		"String Print Selection")

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%,	! Data pasteboard &
		SCOPE::SMG_PBID,	! Pasetboard &
		2%,		! Row to start in &
		2%,		! Column to start in &
				! Don't need top-disp &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$SET_CURSOR_ABS(SMG_SCREEN_DATA%, 1%, 1%)

	!
	! Define scrolling region
	!
	SMG_SCROLL::WINDOW	= SMG_SCREEN_DATA%
	SMG_SCROLL::TOP_ARRAY	= 1%
	SMG_SCROLL::BOT_ARRAY	= 1%
	SMG_SCROLL::SCROLL_TOP	= 1%
	SMG_SCROLL::SCROLL_BOT	= 18%
	SMG_SCROLL::BEG_ELEMENT	= 1%
	SMG_SCROLL::END_ELEMENT	= 1%
	SMG_SCROLL::TOP_LINE	= 1%
	SMG_SCROLL::CUR_LINE	= 1%
	SMG_SCROLL::CUR_W_ROW	= 1%
	SMG_SCROLL::CUR_W_COL	= 1%
	SMG_SCROLL::FIND_LINE	= 1%
	SMG_SCROLL::SMG_FLAG	= 0%
	SMG_SCROLL::PROMPT	= "->"
	SMG_SCROLL::VIDEO_SET	= SMG$M_REVERSE
	SMG_SCROLL::VIDEO_COMP	= 0%
	SMG_SCROLL::CHARSET	= 0%
	SMG_SCROLL::DRAW_COLS	= ""

	!
	! Get the program name
	!
	SCOPE::PRG_PROGRAM = READ_SYSPN
	SCOPE::PRG_IDENT = "PROG"

220	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_STRING_PRINT.OPN"
	USE
		FILENAME$ = "UTL_STRING_PRINT"
		CONTINUE HelpError
	END WHEN

230	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.MOD"

	CALL ASSG_CHANNEL(PRNT.CH%, STAT%)
	CALL ASSG_CHANNEL(PRNT1.CH%, STAT%)
	CALL ASSG_CHANNEL(DET.CH%, STAT%)

	%PAGE

300	!*******************************************************************
	! Ask for which system they want to play in
	!*******************************************************************

	!
	! Generate a list of systems
	!
	THIS_SYSTEM$ = "1234567890"
	TOTAL_SYSTEM% = 0%

	RESET #UTL_STRING_PRINT.CH%

310	WHEN ERROR IN
		GET #UTL_STRING_PRINT.CH%, REGARDLESS
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 350 IF ERR = 11%
		FILENAME$ = "UTL_STRING_PRINT"
		CONTINUE HelpError
	END WHEN

	IF (UTL_STRING_PRINT::SYSTEM <> THIS_SYSTEM$)
	THEN
		TOTAL_SYSTEM% = TOTAL_SYSTEM% + 1%

		REP$(TOTAL_SYSTEM%) = UTL_STRING_PRINT::SYSTEM
		THIS_SYSTEM$ = UTL_STRING_PRINT::SYSTEM
	END IF

	GOTO 310

350	!
	! Don't bother asking if there isn't any choice.
	!
	IF TOTAL_SYSTEM% = 1%
	THEN
		THIS_SYSTEM$ = REP$(1%)
		GOTO 400
	END IF

	REP$(0%) = NUM1$(TOTAL_SYSTEM%)
	REP$(TOTAL_SYSTEM% + 1%) = ""

	!
	! Query the user
	!
	SCOPE::PRG_ITEM = "SYSTEM"
	X% = ENTR_3CHOICE(SCOPE, "", "", REP$(), "", &
		0%, "Systems Available", "", 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit key ?
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram
	END SELECT

	IF X% > 0%
	THEN
		THIS_SYSTEM$ = REP$(X%)
		GOTO 400
	ELSE
		GOTO ExitProgram
	END IF

400	!*******************************************************************
	! Ask for which system they want to play in
	!*******************************************************************

	!
	! Generate a list of systems
	!
	THIS_GROUPING$ = "1234567890"
	TOTAL_GROUPING% = 0%

	FIND #UTL_STRING_PRINT.CH%, KEY #0% GE THIS_SYSTEM$

410	WHEN ERROR IN
		GET #UTL_STRING_PRINT.CH%, REGARDLESS
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "UTL_STRING_PRINT"
		CONTINUE HelpError
	END WHEN

	IF (UTL_STRING_PRINT::SYSTEM = THIS_SYSTEM$)
	THEN
		IF (UTL_STRING_PRINT::GROUPING <> THIS_GROUPING$)
		THEN
			TOTAL_GROUPING% = TOTAL_GROUPING% + 1%

			REP$(TOTAL_GROUPING%) = UTL_STRING_PRINT::GROUPING
			THIS_GROUPING$ = UTL_STRING_PRINT::GROUPING
		END IF

		GOTO 410
	END IF

450	!
	! Don't bother asking if there isn't any choice.
	!
	IF TOTAL_GROUPING% = 1%
	THEN
		THIS_GROUPING$ = REP$(1%)
		GOTO 900
	END IF

	REP$(0%) = NUM1$(TOTAL_GROUPING%)
	REP$(TOTAL_GROUPING% + 1%) = ""

	!
	! Query the user
	!
	SCOPE::PRG_ITEM = "GROUP"
	X% = ENTR_3CHOICE(SCOPE, "", "", REP$(), "", &
		0%, "Groups Available", "", 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit key ?
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram
	END SELECT

	IF X% > 0%
	THEN
		THIS_GROUPING$ = REP$(X%)
		GOTO 900
	ELSE
		GOTO ExitProgram
	END IF


900	!******************************************************************
	! Individual reports
	!******************************************************************

910	TOTAL_SEQUENCE% = 0%

	WHEN ERROR IN
		FIND #UTL_STRING_PRINT.CH%, &
			KEY #0% GE THIS_SYSTEM$ + THIS_GROUPING$
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 1000
	END WHEN

920	WHEN ERROR IN
		GET #UTL_STRING_PRINT.CH%, REGARDLESS
	USE
		CONTINUE 1000
	END WHEN

	IF (THIS_SYSTEM$ = UTL_STRING_PRINT::SYSTEM) AND &
		(THIS_GROUPING$ = UTL_STRING_PRINT::GROUPING)
	THEN
		TOTAL_SEQUENCE% = TOTAL_SEQUENCE% + 1%
		SMG_SCROLL::BOT_ARRAY, SMG_SCROLL::END_ELEMENT = TOTAL_SEQUENCE%

		POINTER.RFA(TOTAL_SEQUENCE%) = GETRFA(UTL_STRING_PRINT.CH%)
		REP$(TOTAL_SEQUENCE%) = "  (" + UTL_STRING_PRINT::REPSEQ + &
			") " + TRM$(UTL_STRING_PRINT::TITLES)
		SELECTED%(TOTAL_SEQUENCE%) = 0%

		GOTO 920
	END IF

	%PAGE

1000	!******************************************************************
	! Ask user for the reports wanted
	!******************************************************************

	SCREEN.TOP% = 1%
	SMG_SCROLL::CUR_LINE = 1%

1010	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	V% = DSPL_SCROLL(SMG_SCROLL, REP$(), 0%, "PAINT")

1030	SCOPE::PRG_ITEM = ""
	OPTLIST$ = "Select Clear Print Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT

	!
	! ^C
	!
	CASE SMG$K_TRM_CTRLC
		GOTO 1030

	!
	! Next Screen, Downarrow, etc.
	!
	CASE SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_DOWN, SMG$K_TRM_UP, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_F18, SMG$K_TRM_F19

		V% = DSPL_SCROLL(SMG_SCROLL, REP$(), &
			SCOPE::SCOPE_EXIT, "PAINT")
		GOTO 1030

	!
	! Exit
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE,SCOPE::SCOPE_EXIT)
		GOTO 1030

	END SELECT

	SELECT OPT$
	CASE "X"
		GOTO ExitProgram

	CASE "P"
		GOTO PrintThem

	CASE "S"	! Select_on
 SelectLineStart:
		TEMP$ = "Position arrow on line to start selection " + &
			"- Then press <DO> "

		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

		INP$ = "  "
		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, &
			LEN(TEMP$)+1%, INP$, -1%, 4096%)

		CASE SMG$K_TRM_CTRLC		! ^C
			GOTO 1000

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1030

		CASE SMG$K_TRM_DOWN, SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_F19, &
			SMG$K_TRM_UP, SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_F18

			TEMP = DSPL_SCROLL(SMG_SCROLL, REP$(), &
				SCOPE::SCOPE_EXIT, "")

			GOTO SelectLineStart

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
			69%, 70%, 87%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE,SCOPE::SCOPE_EXIT)
			GOTO SelectLineStart

		END SELECT

		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
			SPACE$(80%), 1%, 1%)

		TEMP = DSPL_SCROLL(SMG_SCROLL, REP$(), &
			SMG$K_TRM_SELECT, "")

		SELECT_START% = SMG_SCROLL::CUR_LINE
		SMG_SCROLL::FIND_LINE = SELECT_START%
		CALL ENTR_3MESSAGE(SCOPE, &
			"Lines highlighted will be selected ",1%)

 SelectLineEnd:
		TEMP$ = "Use arrow keys to select contents " + &
			"- Press <DO> to complete selection process"

		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

		INP$ = " "
		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
			1%, LEN(TEMP$)+1%, INP$, -1%, 4096%)

		CASE SMG$K_TRM_CTRLC		! ^C
			TEMP = DSPL_SCROLL(SMG_SCROLL, &
				REP$(), &
				SMG$K_TRM_REMOVE, &
				"PAINT")

			GOTO 1030

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			TEMP = DSPL_SCROLL(SMG_SCROLL, &
				REP$(), &
				SMG$K_TRM_REMOVE, &
				"PAINT")
			GOTO 1030

		CASE SMG$K_TRM_DOWN, SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_F19, &
			SMG$K_TRM_UP, SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_F18

			TEMP = DSPL_SCROLL(SMG_SCROLL, REP$(), &
				SCOPE::SCOPE_EXIT, "")

			GOTO SelectLineEnd

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
			69%, 70%, 87%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE,SCOPE::SCOPE_EXIT)
			GOTO SelectLineEnd

		END SELECT

		IF SELECT_START% = SMG_SCROLL::CUR_LINE
		THEN
			TEMP = DSPL_SCROLL(SMG_SCROLL, &
				REP$(), &
				SMG$K_TRM_REMOVE, &
				"PAINT")

			GOTO 1030
		END IF

		SELECT_END% = SMG_SCROLL::CUR_LINE - 1%

		IF SELECT_START% > SELECT_END%
		THEN
			SELECT_END% = SELECT_START% - 1%
			SELECT_START% = SMG_SCROLL::CUR_LINE
		END IF

		FOR TEST_LOOP% = SELECT_START% TO SELECT_END%
			REP$(TEST_LOOP%) = "*" + RIGHT(REP$(TEST_LOOP%), 2%)
			SELECTED%(TEST_LOOP%) = -1%
		NEXT TEST_LOOP%

		TEMP = DSPL_SCROLL(SMG_SCROLL, &
			REP$(), &
			SMG$K_TRM_REMOVE, &
			"PAINT")

		CALL ENTR_3MESSAGE(SCOPE, "",1%)

	CASE "C"
		!************************************************************
		! CLEAR all selected reports
		!************************************************************

		FOR LOOP% = 1% TO TOTAL_SEQUENCE%
			REP$(LOOP%) = " " + RIGHT(REP$(LOOP%), 2%)
			SELECTED%(LOOP%) = 0%
		NEXT LOOP%

		V% = DSPL_SCROLL(SMG_SCROLL, REP$(), 0%, "PAINT")

	!
	! Help
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", &
			SCOPE::PRG_PROGRAM, "HELP")

	END SELECT

	GOTO 1030

	%PAGE

 PrintThem:
2000	!******************************************************
	! Write selected report list
	!******************************************************

	PRINT_TEST% = 0%
	TOTAL_QUERY% = 0%

	FOR TEST.LOOP% = 1% TO TOTAL_SEQUENCE%

2050		!
		! Handle all selected reports
		!
		GOTO 2090 UNLESS SELECTED%(TEST.LOOP%)

		PRINT_TEST% = -1%

		WHEN ERROR IN
			GET #UTL_STRING_PRINT.CH%, &
				RFA POINTER.RFA(TEST.LOOP%), REGARDLESS
		USE
			FILENAME$ = "UTL_REPORT"
			CONTINUE HelpError
		END WHEN

		!
		! Check list of items to enter, and add item to
		! list if it has not yet been used
		!
		FOR LOOP1% = 0% TO 9%

			IF UTL_STRING_PRINT::FLAGS(LOOP1%) = "Q"
			THEN
				GOTO 2070 &
					IF UTL_STRING_PRINT::CODES(LOOP1%) = &
					QUERY_CODES$(LOOP2%) &
					FOR LOOP2% = 1% TO TOTAL_QUERY%

				GET #UTL_REPORT.CH%, &
					KEY #0% EQ UTL_STRING_PRINT::REPNUM, &
					REGARDLESS &
					IF (UTL_REPORT::REPNUM <> UTL_STRING_PRINT::REPNUM)

				TOTAL_QUERY% = TOTAL_QUERY% + 1%

				QUERY_REPNUM$(TOTAL_QUERY%) = &
					UTL_STRING_PRINT::REPNUM
				QUERY_CODES$(TOTAL_QUERY%) = &
					UTL_STRING_PRINT::CODES(LOOP1%)
				QUERY_DESCRS$(TOTAL_QUERY%) = &
					UTL_STRING_PRINT::DESCRS(LOOP1%)
				QUERY_OPTTYPE$(TOTAL_QUERY%) = &
					UTL_REPORT::OPTTYPE(LOOP1%)
				QUERY_OPTLEN%(TOTAL_QUERY%) = &
					UTL_REPORT::OPTLEN(LOOP1%)
				QUERY_VALID$(TOTAL_QUERY%) = &
					UTL_REPORT::VALID(LOOP1%)
				QUERY_VALUE$(TOTAL_QUERY%) = &
					UTL_REPORT::OPTDEF(LOOP1%)
				QUERY_PRONAM$(TOTAL_QUERY%) = &
					UTL_REPORT::PRONAM
				QUERY_ITEM%(TOTAL_QUERY%) = &
					LOOP1% + 1%

			END IF

2070		NEXT LOOP1%

2090	!
	NEXT TEST.LOOP%

2200	!*******************************************************************
	! Ask for all of the fields that we require
	!*******************************************************************

	!
	! Skip this screen if not necessary
	!
	GOTO 3000 IF TOTAL_QUERY% = 0%


	!
	! Display the information
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)

	FOR LOOP% = 1% TO TOTAL_QUERY%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"(" + FORMAT$(LOOP%, "<0>#") + ")", &
			LOOP%, 1%,, SMG$M_NORMAL)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			LEFT( QUERY_DESCRS$(LOOP%), 15%), &
			LOOP%, 6%,, SMG$M_NORMAL)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			LEFT( QUERY_VALUE$(LOOP%), QUERY_OPTLEN%(LOOP%)), &
			LOOP%, 27%,, SMG$M_BOLD)

	NEXT LOOP%

2300	SCOPE::PRG_ITEM = ""
	OPTLIST$ = "Change Print Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT

	!
	! ^C
	!
	CASE SMG$K_TRM_CTRLC
		GOTO 2300

	!
	! Exit
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE,SCOPE::SCOPE_EXIT)
		GOTO 2300

	END SELECT

	SELECT OPT$
	!
	! eXit
	!
	CASE "X"
		GOTO ExitProgram

	!
	! Help
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", &
			SCOPE::PRG_PROGRAM, "HELP")
		GOTO 2300

	!
	! Print
	!
	CASE "P"
		GOTO 3000

	CASE "C"
2340		THIS_ITEM% = ENTR_3NUMBER(SCOPE, UTL_REPORTX::WINDOW, "", &
			"Change", &
			0.0, 4%, "##", "")

		SELECT SCOPE::SCOPE_EXIT

		CASE 3%, 290%
			GOTO 2300

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 2300

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE,SCOPE::SCOPE_EXIT)
			GOTO 2340

		END SELECT

		GOTO 2300 IF THIS_ITEM% = 0%

		GOTO 2340 IF (THIS_ITEM% < 1%) OR (THIS_ITEM% > TOTAL_QUERY%)
 Changee:
		FLAG% = 0%
		GOSUB 15000		! entry

		SELECT SCOPE::SCOPE_EXIT
		CASE 274%	! Uparrow
			THIS_ITEM% = THIS_ITEM% - 1% IF THIS_ITEM% > 1%
			GOTO Changee

		CASE 275%	! Downarrow
			THIS_ITEM% = THIS_ITEM% + 1% &
				IF THIS_ITEM% < TOTAL_QUERY%
			GOTO Changee

		END SELECT

		GOTO 2340

	END SELECT

	GOTO 2300

3000	!*******************************************************************
	! Go through a print settings screen
	!*******************************************************************

	WHEN ERROR IN
		GET #UTL_REPORT.CH%, KEY #0% EQ "UTSTRP"
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 3010
	END WHEN

	GOTO 3020

3010	IF SYSREP_OPEN_FLAG% = 0%
	THEN
		!
		! Open system file if report not found
		!
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSREP.OPN"

	END IF

	SYSREP_OPEN_FLAG% = -1%

	!
	! Get report from system report file
	!
	GET #UTL_SYSREP.CH%, KEY #0% EQ "UTSTRP", REGARDLESS
	UNLOCK #UTL_SYSREP.CH%

3012	!
	! Put record into report file
	!
	PUT #UTL_REPORT.CH%

	GOTO 3000

3020	XLOOP% = XLOOP% + 1%
	TEMPFILE$ = "PRNT" + JOBNUM$ + "_" + NUM1$(XLOOP%) + ".TMP"

	WHEN ERROR IN
		OPEN TEMPFILE$ FOR INPUT AS FILE PRNT.CH%
	USE
		CONTINUE 3030 IF ERR = 5%
		FILENAME$ = TEMPFILE$
		CONTINUE HelpError
	END WHEN

	CLOSE PRNT.CH%
	GOTO 3020

3030	CLOSE PRNT.CH%
	OPEN TEMPFILE$ FOR OUTPUT AS FILE PRNT.CH%

	SYS_STATUS% = LIB$SET_SYMBOL("CMC$REPORT", TEMPFILE$,)

	IF (SYS_STATUS% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to declare symbol for work file. " + &
			NUM1$(SYS_STATUS%), 0%)
		GOTO ExitProgram
	END IF

	SMG_SCREEN_DATA% = 0%

	!
	! Initilize defaults from report file
	!
	CALL OUTP_INITSTRUCTURE(UTL_REPORT, UTL_REPORTX)

	TEMP_IDENT$ = SCOPE::PRG_IDENT
	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM
	TEMP_ITEM$ = SCOPE::PRG_ITEM

	SCOPE::PRG_IDENT	= "PROG"
	SCOPE::PRG_PROGRAM	= "UTL_REPORT"

	!
	! Ask user to change settings
	!
	CALL OUTP_SETTINGS(UTL_REPORT, UTL_REPORTX, UTL_REPORT.CH%, &
		"DD SF SP EP CP AS RD ", "PT ")

	SCOPE::PRG_IDENT	= TEMP_IDENT$
	SCOPE::PRG_PROGRAM	= TEMP_PROGRAM$
	SCOPE::PRG_ITEM	= TEMP_ITEM$

	UNLOCK #UTL_REPORT.CH%

	!
	! Set up the detachable flag
	!
	NODETACH% = UTL_REPORTX::DETACH

	!
	! Un-normal abort, exit, etc.
	!
	IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLZ) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
	THEN
		GOSUB KillTempFile
		GOTO ExitProgram
	END IF

	!*******************************************************************
	! Create the work file
	!*******************************************************************

	PRINT_TEST% = 0%

	FOR TEST.LOOP% = 1% TO TOTAL_SEQUENCE%

4050		!
		! Handle all selected reports
		!
		GOTO 4090 UNLESS SELECTED%(TEST.LOOP%)

		GET #UTL_STRING_PRINT.CH%, &
			RFA POINTER.RFA(TEST.LOOP%), REGARDLESS

		GET #UTL_REPORT.CH%, &
			KEY #0% EQ UTL_STRING_PRINT::REPNUM, REGARDLESS

		IF PRINT_TEST% = 0%
		THEN
			PRGNAM$ = TRM$(UTL_REPORT::PRODEV) + &
				TRM$(UTL_REPORT::PRONAM)

			PRINT_TEST% = -1%
		END IF

		IF NOT(UTL_STRING_PRINT::OUTDEV = "")
		THEN
			OUTPUT$ = UTL_STRING_PRINT::OUTDEV
			GOSUB 6400
		END IF

		!
		! Set detach flag
		!
		IF (UTL_REPORT::CANDET = "N")
		THEN
			NODETACH% = -1%
		END IF

		!
		! Check list of items to enter, and add item to
		! list if it has not yet been used
		!
		FOR LOOP1% = 0% TO 9%

			SELECT UTL_STRING_PRINT::FLAGS(LOOP1%)

			CASE "Q"
				FOR LOOP2% = 1% TO TOTAL_QUERY%
					IF UTL_STRING_PRINT::FLAGS(LOOP1%) = &
						QUERY_CODES$(LOOP2%)
					THEN
						UTL_REPORTX::OPTDEF(LOOP1%) = &
							QUERY_VALUE$(LOOP2%)
						GOTO 4070
					END IF
				NEXT LOOP2%

			CASE "S"
				UTL_REPORTX::OPTDEF(LOOP1%) = &
					UTL_STRING_PRINT::DESCRS(LOOP1%)

			CASE ELSE ! (I), etc.
				UTL_REPORTX::OPTDEF(LOOP1%) = &
					UTL_REPORT::OPTDEF(LOOP1%)

			END SELECT

4070		NEXT LOOP1%

		UTL_REPORTX::REPNUM	= UTL_REPORT::REPNUM
		UTL_REPORTX::PRODEV	= UTL_REPORT::PRODEV
		UTL_REPORTX::PRONAM	= UTL_REPORT::PRONAM

		!
		! Write the data out to the ascii file
		!
		CALL OUTP_3WRITESTRUCTURE(UTL_REPORTX, PRNT.CH%, PRINTX)


4090	NEXT TEST.LOOP%

	%PAGE

5000	!***************************************************************
	! Chain to print program
	!***************************************************************

	CLOSE PRNT.CH%

5040	!
	! Test to see if detaching is allowed
	!
	GOTO 5050 IF NODETACH%

	!
	! Should it detach?
	!
	INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"", "Run Background or Normally (B/N*)", "N", &
		4%, "", ""), -1%)

	IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLZ) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Aborting", 0%)

		GOSUB KillTempFile
		GOTO ExitProgram
	END IF

	GOTO 5050 IF EDIT$(INP$, -1%) <> "B"

	!
	! Attempt to detach
	!
	CALL ENTR_3MESSAGE(SCOPE, "", 1%+16%)

	DLOOP% = -1%

5045	!
	! Create the command file
	!
	DLOOP% = DLOOP% + 1%
	DET.TEMPFILE$ = "DET_" + JOBNUM$ + "_" + NUM1$(DLOOP%) + ".TMP"

	WHEN ERROR IN
		OPEN DET.TEMPFILE$ FOR INPUT AS FILE DET.CH%
	USE
		CONTINUE 5047
	END WHEN

	CLOSE #DET.CH%
	GOTO 5045

5047	OPEN DET.TEMPFILE$ AS FILE DET.CH%

5048	PRINT #DET.CH%, "$ SET DEFAULT "; READ_DEFAULT
	PRINT #DET.CH%, "$ DEF SYS$LOGIN "; READ_SYSLOG("SYS$LOGIN")
	PRINT #DET.CH%, "$ CMC$REPORT :== " + TEMPFILE$
	PRINT #DET.CH%, "$ RUN "  + PRGNAM$

	CLOSE DET.CH%

	CALL SUBR_SUBMIT(DET.TEMPFILE$, THESTAT%)

	!
	! Exit program
	!
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	GOTO ExitProgram

5050	!
	! Not detaching section
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

5060	CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + PRGNAM$, TEMPFILE$)

	%PAGE

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
	PRINT.TO% = 3%
	TEMP1$ = OUTPUT$

6405	!
	! Spool
	!
	TEMP1$ = EDIT$(TEMP1$, 4% + 8% + 32% + 128% + 256%)

	SELECT PRINT.TO%

	CASE 2%
		TEMP$ = "SYS$PRINT"
		TEMP$ = TEMP1$ IF TEMP1$ <> ""
		UTL_REPORTX::PRINTTO	= 2%
		UTL_REPORTX::DEFOUT	= TEMP$
		UTL_REPORTX::SPOOL	= TEMP$
		GOTO ExitSub

	CASE 9%
		TEMP1$ = "TEMP_DOCUMENT.TMP" IF TEMP1$ = ""

	CASE 10%
		TEMP1$ = "TEMP.S2020" IF TEMP1$ = ""

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
	SYS_STATUS% = LIB$GETDVI(DVI$_DEVCLASS,, TEMP$, DEVCHAR%,,)

	!
	! Certain errors may mean that it is a file
	!
	IF (SYS_STATUS% = 324%) OR (SYS_STATUS% = 2312%)
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
	IF (SYS_STATUS% = 2288%)
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
	IF (SYS_STATUS% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Invalid output device: " + TEMP$ + &
			" (" + NUM1$(SYS_STATUS%) + ")", 0%)
		GOTO ExitSub
	END IF

	SELECT DEVCHAR%

	CASE DC$_DISK
		FTYPE% = 4%

	CASE DC$_LP, DC$_MAILBOX, DC$_TERM
		FTYPE% = 3%

	CASE ELSE
		CALL ENTR_3MESSAGE(SCOPE, "Invalid output device: " + TEMP$ + &
			" (" + NUM1$(SYS_STATUS%) + ")", 0%)
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
				TEMP$ = TEMP1$ + ".S2020" IF PRINT.TO% = 10%
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
	CASE 1%

		SELECT FTYPE%

		! Display on users terminal
		CASE 5%
			GOTO 6450

		END SELECT

	!
	! Normal output
	!
	CASE 3%
		PRINT.TO% = FTYPE%
		GOTO 6450

	!
	! Printer port
	!
	CASE 5%
		GOTO 6450

	!
	! Word Processor (Must go to a file)
	!
	CASE 7%
		GOTO 6450 IF FTYPE% = 4%

	!
	! Documentation (Must go to a file)
	!
	CASE 9%
		GOTO 6450 IF FTYPE% = 4%

	!
	! SuperComp 2020 (Must go to a file)
	!
	CASE 10%
		GOTO 6450 IF FTYPE% = 4%

	END SELECT

	CALL ENTR_3MESSAGE(SCOPE, &
		"Unable to parse output specification '" + OUTPUT$ + "'", 4%)
	GOTO ExitSub

6450	!==================================================================
	! Finish update if good stuff
	!==================================================================

	SELECT PRINT.TO%
	!
	! Make sure printer is turned on
	!
	CASE 3%
6460		PFLAG$ = ""

		!
		! If there is a device request sequence, use it to
		! see if the device is ready.
		!
		PFLAG$ = READ_DEVID(TEMP$, &
			OUTP_CREATESTR(PRINT.SEQ$( &
				FIND_3PRINTGROUPITEM("RQ", "*", PRINTX)), "")) &
				IF FIND_3PRINTGROUPITEM("RQ", "*", PRINTX) >= 0%

		JTEMP$ = LEFT(PFLAG$, 1%)
		JTEMP$ = LEFT(PFLAG$, INSTR(1%, PFLAG$, " ")-1%) &
			IF JTEMP$ <> '27'C
		SELECT JTEMP$
		CASE "INUSE"
			CALL ENTR_3MESSAGE(SCOPE, "Device is in use - Try "+ &
				"again!!!", 0%)
			UTL_REPORTX::DEFOUT	= "TT:"
			UTL_REPORTX::PRINTTO	= 5%
			OUTPUT$ = TRM$(UTL_REPORTX::DEFOUT) + &
				" "+XTYPE$(UTL_REPORTX::PRINTTO)
			GOTO 6400

		CASE '27'C

		CASE ELSE
			CALL ENTR_3MESSAGE(SCOPE, "Please make sure that "+ &
				"the printer is ready", 0%)
		END SELECT

	!
	! See if file already exists
	!
	CASE 4%, 6%, 7%, 9%, 10%, 11%

6470		!
		! Random access File structured device?
		!
		WHEN ERROR IN
			OPEN TEMP$ FOR INPUT AS FILE PRNT1.CH%
		USE
			CONTINUE 6480
		END WHEN

		CLOSE PRNT1.CH%
		OPT7% = 0%

6472		CALL ENTR_3MESSAGE(SCOPE, &
			"File " + TEMP$ + " already exists!", 1%)
		V$ = ENTR_3OPTION(SCOPE, "Command ", &
			"Append Overwrite eXit", OPT7%, 0%)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_CTRLC, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOTO ExitSub

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE,SCOPE::SCOPE_EXIT)
			GOTO 6472
		END SELECT

6474		SELECT V$

		!
		! Overwrite
		!
		CASE "O"
 !			KILL TEMP$
			SMG_STATUS% = LIB$DELETE_FILE(TEMP$ + ";*")
			GOTO 6470

		!
		! Append
		!
		CASE "A"

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


 ExitProgram:
10000	!**************************************************************
	! KILL TEMP FILES
	!**************************************************************

	CLOSE FINSTA.WRK%

 !	KILL FINSTA_WORK_FILE$ WHILE (-1)

	SMG_STATUS% = LIB$DELETE_FILE(FINSTA_WORK_FILE$ + ";*")

10010	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
	SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

15000	!*******************************************************************
	! Enter/change one field item (THIS_ITEM%)
	!*******************************************************************

	!
	! Set up for help key
	!
	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM
	TEMP_IDENT$ = SCOPE::PRG_IDENT
	TEMP_ITEM$ = SCOPE::PRG_ITEM

	SCOPE::PRG_PROGRAM = TRM$(QUERY_PRONAM$(THIS_ITEM%))
	SCOPE::PRG_IDENT = "REPO"
	SCOPE::PRG_ITEM = "FLD" + FORMAT$(QUERY_ITEM%(THIS_ITEM%), "<0>#")

	!
	! Calculate the screen postion and print length
	!
	TEMP%, PRINT_LEN% = QUERY_OPTLEN%(THIS_ITEM%)

	YPOS% = THIS_ITEM%
	TEMP$ = NUM1$(YPOS%) + ";27"

15030	!
	! Enter data item
	!
	SELECT QUERY_OPTTYPE$(THIS_ITEM%)

	CASE "F"	! Floating point
		TEMP1$ = STRING$(PRINT_LEN% - 3%, A"#"B) + ".##"

		M = ENTR_3NUMBER(SCOPE, SMG_SCREEN_DATA%, TEMP$, &
			TRM$(QUERY_DESCRS$(THIS_ITEM%)), &
			VAL(QUERY_VALUE$(THIS_ITEM%)) * 1.0, &
			CBFLAG%, TEMP1$, "")
		INP$ = FORMAT$(M, TEMP1$)

		WORK% = INSTR(1%, INP$, "%")
		IF WORK%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Number is to large for" + &
				" format field.",0%)
			INP$ = RIGHT(INP$, WORK% + 1%)
		END IF

	CASE "D"	! Date

		IF TEMP% = 6%
		THEN
			TT% = 6%
		ELSE
			TT% = 8%
		END IF

		TMEP$= LEFT( QUERY_VALUE$(THIS_ITEM%), TT% + 2% )
		TMEP$= MID(TMEP$, 7%, TT% - 4%) + &
			LEFT(TMEP$, 2%) + MID(TMEP$, 4%, 2%)

		INP$ = ENTR_3DATE(SCOPE,  SMG_SCREEN_DATA%, TEMP$, &
			TRM$(QUERY_DESCRS$(THIS_ITEM%)), &
			TMEP$, CBFLAG%, NUM1$(TT%), "" )

		INP$ = MID(INP$, TT% - 3%, 2%) + "/" + &
			MID(INP$, TT% - 1%, 2%) + "/" + &
			LEFT(INP$, TT% - 4%)

	CASE "I"	! Integer
		TEMP1$ = STRING$(PRINT_LEN%, A"#"B)

		M% = ENTR_3NUMBER(SCOPE, SMG_SCREEN_DATA%, TEMP$, &
			TRM$(QUERY_DESCRS$(THIS_ITEM%)), &
			VAL(QUERY_VALUE$(THIS_ITEM%)) * 1.0, &
			CBFLAG%, TEMP1$, "")
		INP$ = FORMAT$(M%, TEMP1$)

	CASE "P" ! Period

		INP$ = ENTR_PERIOD(SMG_SCREEN_DATA%, TEMP$, &
			TRM$(QUERY_DESCRS$(THIS_ITEM%)), &
			LEFT(QUERY_VALUE$(THIS_ITEM%), &
			TEMP%), CBFLAG%, "", "")

	CASE "Y"	! Yes/No
		TEMP1$ = "'"+STRING$(PRINT_LEN% - 1%, A"L"B)

		INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, TEMP$, &
			TRM$(QUERY_DESCRS$(THIS_ITEM%)), &
			LEFT(QUERY_VALUE$(THIS_ITEM%), &
			TEMP%), CBFLAG%, TEMP1$, "")

	CASE ELSE	! String
		TEMP1$ = "'"+STRING$(PRINT_LEN% - 1%, A"L"B)

		INP$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, TEMP$, &
			TRM$(QUERY_DESCRS$(THIS_ITEM%)), &
			LEFT(QUERY_VALUE$(THIS_ITEM%), &
			TEMP%), CBFLAG%, TEMP1$, "")

	END SELECT

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, 290%
		GOTO 15092

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO 15092

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, 274%, 275%

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE,SCOPE::SCOPE_EXIT)
		GOTO 15030
	END SELECT

	!
	! Required field?
	!
	IF (INP$ = "")
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "This field must be entered!", 0%)
		GOTO 15030
	END IF

	!
	! Test input
	!
	IF NOT (QUERY_VALID$(THIS_ITEM%) = "") AND &
		EDIT$( INP$, -1%) <> ""
	THEN
		IF COMP_STRING(INP$, QUERY_VALID$(THIS_ITEM%)) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Valid input is " + &
				QUERY_VALID$(THIS_ITEM%), 0%)
			SCOPE::SCOPE_EXIT = 0%
			GOTO 15030
		END IF
	END IF

	QUERY_VALUE$(THIS_ITEM%) = INP$

	!
	! Print item
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		LEFT( QUERY_VALUE$(THIS_ITEM%), PRINT_LEN% ), &
		YPOS%, 27%,, SMG$M_BOLD)

15092	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_ITEM = TEMP_ITEM$

	RETURN

	%PAGE

18900	!******************************************************************
	! Delete temp file to prepare for exit
	!******************************************************************
 KillTempFile:
	CLOSE PRNT.CH%

 !	KILL TEMPFILE$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(TEMPFILE$ + ";*")

18910	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

	%Page

19000	!**************************************************************
	! ERROR TRAPPING
	!**************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
