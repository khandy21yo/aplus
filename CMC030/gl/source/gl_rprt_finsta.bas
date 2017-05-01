1	%TITLE "FINSTA - Select _& Print Financial Stmts"
	%SBTTL "GL_RPRT_FINSTA"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:0000
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This option selects and prints a
	!	specified financial statement or statements.
	!	.b
	!	"Working" financial statements may be printed from the
	!	current General Ledger period file not yet closed. The
	!	option to print "non-working" or "final" financial
	!	statements results in printing financial statements for
	!	the most immediate closed General Ledger period.
	!	.b
	!	Note:  The FINSTA option will not function until the
	!	LAYOUT and CMDFIL files have been completed.
	!	.lm -5
	!
	! Index:
	!	.x Select>Financial Statement
	!	.x Print>Financial Statement
	!	.x Financial Statement>Print
	!	.x Financial Statement>Select
	!
	! Option:
	!
	!	GL_RPRT_FINSTA$WORKING
	!	GL_RPRT_FINSTA$PRINT
	!	GL_RPRT_FINSTA$SELECT
	!	GL_RPRT_FINSTA$CLEAR
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_FINSTA/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_FINSTA, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_FINSTA.OBJ;*
	!
	! Author:
	!
	!	12/15/86 - Kevin Handy
	!
	! Modification history:
	!
	!	10/21/87 - Robert Peterson
	!		Add interrupt menu during create of work file
	!
	!	10/10/88 - Kevin Handy
	!		Added TITLEA to TITLED to report file.
	!
	!	02/10/89 - Kevin Handy
	!		Modified for changes in ENTR_NOLSTRING
	!
	!	09/22/89 - Kevin Handy
	!		Modified to use ENTR_ENTER instead of
	!		ENTR_NOLSTRING.
	!
	!	09/21/90 - Kevin Handy
	!		Cleaned up error trapping.
	!
	!	09/21/90 - Kevin Handy
	!		Added more error trapping to reduce number
	!		of crashes.
	!
	!	09/25/90 - Kevin Handy
	!		Cleaning up error trapping caused bug where 520
	!		was trapped twice, and was seeing the wrong one.
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error handler,
	!		changed to = "GL_" + YYYY_PP$.
	!
	!	05/24/91 - Kevin Handy
	!		Changed "CLOSE GL_PEIROD" to "CLOSE GL_PERIOD".
	!
	!	04/19/93 - Kevin Handy
	!		Changed GL_CLOSE in error messages to
	!		GL_RPRT_FINSTA.
	!
	!	04/22/93 - Kevin Handy
	!		Added "SelectAll" command to menu to allow
	!		selecting all financial statements.
	!
	!	08/09/93 - Kevin Handy
	!		Modified select so it selects as in new version
	!		of DSPL_SCROLL.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source code closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	06/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/15/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include statements
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_FINSTA.HB"
	MAP (GL_FINSTA)		GL_FINSTA_CDD	GL_FINSTA

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE SMG_SCROLL_CDD SMG_SCROLL

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL INTEGER FUNCTION DSPL_SCROLL
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Dimension statements
	!
	DIM REP$(2000%), SELECTED%(2000%)
	DIM RFA POINTER_RFA(2000%)

	DECLARE LONG XTEMP, YTEMP

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize process
	!*******************************************************************

	CALL READ_INITIALIZE

	CALL ASSG_CHANNEL(FINSTA.WRK%, STAT%)

	!
	! Look up device
	!
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)
	CALL READ_DEVICE("GL_FINCMD", GL_FINCMD.DEV$, STAT%)

	FINSTA_WORK_FILE$ = UTL_WORK.DEV$ + "TEMP_" + READ_SYSJOB + ".TMP"

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
		"Financial Statement Selection")

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
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.MOD"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

230	!
	! Line number here for repaint
	!
	RESTORE

	DATA	3, 12, "     Account #", &
		4, 12, "     Description", &
		5, 12, "     Type", &
		6, 12, "-----------Financial Codes-----------", &
		7, 12, "(01) Bal/inc Code", &
		8, 12, "(02) Cash Flow", &
		9, 12, "(03) Work Capital", &
		0,  0, ""

	READ XTEMP, YTEMP, A$

	WHILE XTEMP
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			A$, XTEMP, YTEMP)

		READ XTEMP, YTEMP, A$
	NEXT

235	!******************************************************************
	! Scan through chart of accounts for undefined accounts
	!******************************************************************

	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #3% EQ "          "
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 400
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_CHART::ACCT, 3%, 35%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_CHART::DESCR, 4%, 35%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_CHART::ACCTYPE, &
		5%, 35%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_CHART::FINTYPE, &
		7%, 35%,,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_CHART::FLOW, &
		8%, 35%,,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_CHART::WORK, 9%, 35%, &
		,, SMG$M_BOLD)

240	SCOPE::PRG_ITEM = "FLD000ASG"

	!++
	!
	! Abstract:FLD000ASG
	!	^*Assign\*
	!	.b
	!	.lm +5
	!	This process will check the Chart of Accounts to
	!	verify that financial codes have been assigned to all
	!	accounts. If financial codes have not been assigned,
	!	the process will allow the user to assign those codes.
	!	.lm -5
	!
	! Index:
	!
	!--

	INP$ = ENTR_3STRING(SCOPE, SCOPE::SMG_OPTION, "", &
		"Assign", "  ", 4%, "", "")

	SELECT SCOPE::SCOPE_EXIT

	!
	! ^C
	!
	CASE SMG$K_TRM_CTRLC
		GOTO 240

	!
	! Exit
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
		SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 240

	END SELECT

	GOTO 235 IF INP$ = ""

	WHEN ERROR IN
		LOOP% = VAL%(INP$)
	USE
		CONTINUE 240 IF ERR=52%
		FILENAME$ = ""
		CONTINUE HelpError
	END WHEN

	GOTO 240 IF LOOP% > 3%

 Sneakin:
250	SCOPE::SCOPE_EXIT = 0%

	SCOPE::PRG_ITEM = "FLD" + FORMAT$(LOOP%, "<0>##")

	SELECT LOOP%

	CASE 1%

	!++
	! Abstract:FLD001
	!	.x Select>Bal/Inc Code
	!	^*(01) Bal/Inc Code\*
	!	.b
	!	.lm +5
	!	This field is used to define how each account will be
	!	presented in the financial statements.
	!	.b
	!	This is a secondary key, duplicates are allowed,
	!	and it can be left blank.
	!	.b
	!
	! Index:
	!	.x Balance Sheet/Income Statement Code
	!
	!--
		GL_CHART::FINTYPE = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"7;35", "Change type", &
			GL_CHART::FINTYPE, 0%, "'E", "")

	CASE 2%

	!++
	! Abstract:FLD002
	!	.x Select>Cash Flow Code
	!	^*(02) Cash Flow\*
	!	.b
	!	.lm +5
	!	This field is used to define how each account will
	!	be presented in the ^*Cash flow\* statement.
	!	.b
	!	It is a secondary key, duplicates
	!	are allowed, or it can be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Cash Flow Code>Select
	!
	!--
		GL_CHART::FLOW = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"8;35", "Change flow", &
			GL_CHART::FLOW, 0%, "'E", "")

	CASE 3%

	!++
	! Abstract:FLD003
	!	.x Select>Working Capital Code
	!	^*(03) Work Capital\*
	!	.b
	!	.lm +5
	!	This field is used to define how each account will
	!	be presented in the statement of working capital.
	!	.b
	!	It is a secondary key, duplicates are allowed,
	!	or it can be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Working Capital Code>Select
	!
	!--
		GL_CHART::WORK = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"9;35", "Change work", &
			GL_CHART::WORK, 0%, "'E", "")

	END SELECT

	SELECT SCOPE::SCOPE_EXIT

	!
	! ^C
	!
	CASE SMG$K_TRM_CTRLC
		GOTO 240

	!
	! Exit
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
		SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		LOOP% = LOOP% - 1%
		LOOP% = 1% IF LOOP% < 1%
		GOTO Sneakin

	!
	! Down Arrow
	!
	CASE SMG$K_TRM_DOWN
		LOOP% = LOOP% + 1%
		LOOP% = 3% IF LOOP% > 3%
		GOTO Sneakin

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Sneakin
	END SELECT

255	WHEN ERROR IN
		UPDATE #GL_CHART.CH%
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

260	GOTO 240

	%PAGE

400	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	IF GL_PERIOD::CLOSEFLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "GL Close in process", &
			"ERR", "GL_CLOSE", "ERROR_CLOSE")
		GOTO ExitProgram
	END IF

	IF GL_PERIOD::CLOSEFLAG = "2"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "GL Reset in process", &
			"ERR", "GL_RESET", "ERROR_RESET")
		GOTO ExitProgram
	END IF

	FINAL_YYYY_PP$ = GL_PERIOD::YEAR + "_" + &
		FORMAT$(GL_PERIOD::LASTPERCLO, "<0>#")

	CHART_LOOP% = 0%
	CUR_PERIOD% = GL_PERIOD::LASTPERCLO + 1%
	YEAR$ = GL_PERIOD::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")

420	!*****************************************************************
	! Is this a working financial statement
	!*****************************************************************
	WORKING_FINANCIAL$ = "N"
	SCOPE::PRG_ITEM = "WORKING"

	!++
	!
	! Abstract:WORKING
	!	.x General Ledger>Financial Statements
	!	^*Working Financial Statements\*
	!	.b
	!	.lm +5
	!	It is not necessary to close the General Ledger in order to
	!	print financial statements for the current period. A ^*Yes\*
	!	response to the confirmation prompt will cause the system to
	!	read the transactions in the current General Ledger file and
	!	print the selected financial statements for the current period.
	!	.b
	!	A ^*No\* response at the confirmation prompt will cause the
	!	system to produce selected financial statements for the last
	!	period for which the General Ledger has been closed.
	!	.b
	!	Confirm the printing of working financial statement(s) by
	!	typing ^*<Y>\* at the confirmation prompt.
	!	.b
	!	Confirm the printing of non-working or "final" financial
	!	statement(s) by typing ^*<N>\* at the confirmation prompt.
	!	.b
	!	If working financial statements are to be printed, the system
	!	will read the current transactions in the file and calculate the
	!	amounts to be printed in the statements. At the same time, the
	!	system will check for Balance Sheet/Income Statement codes in
	!	the General Ledger Masterfile. If any blank code fields are
	!	encountered the system requires the user to enter the "Bal/inc"
	!	code.
	!	.b
	!	^*Note:  There is no code validation check. An
	!	inappropriate code will be accepted, but
	!	could cause inaccuracies in the financial
	!	statements.\*
	!	.b
	!	If final financial statements are to be printed, the step
	!	described in the above paragraph is omitted.
	!	.lm -5
	!
	! Index:
	!	.x Financial Statements>Working
	!
	!--

	CALL ENTR_3MESSAGE(SCOPE, "Working period is " + &
		YYYY_PP$ + " " + TRM$(GL_PERIOD::PERIOD(CUR_PERIOD%)) + &
		" Final period is " + &
		FINAL_YYYY_PP$ + " " + TRM$(GL_PERIOD::PERIOD( &
		GL_PERIOD::LASTPERCLO)), 1%)

	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Is this a working financial?  Confirm - then press <Do> ", &
		"N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 420

	END SELECT

	GOTO 800 IF EDIT$(INP$, -1%) <> "Y"
	WORKING_FINANCIAL$ = "Y"

430	!
	! Open period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	%PAGE

500	!******************************************************************
	! Set running total in Chart and calculate summary total if fist
	! period
	!******************************************************************

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Account #", 17%, 32%)

	CALL ENTR_3MESSAGE(SCOPE, &
		"Setting current balance in financial for " + &
		YYYY_PP$ + " " + TRM$(GL_PERIOD::PERIOD(CUR_PERIOD%)), 1%)

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

	WHEN ERROR IN
		RESET #GL_CHART.CH%, KEY #0%
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

505	WHEN ERROR IN
		RESET #GL_YYYY_PP.CH%, KEY #0%

		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		GL_EMPTY_FLAG% = -1%
		CONTINUE 510
	END WHEN

510	!
	! Handle any special junk in RRR_FLAG%
	!
	SELECT RRR_FLAG%

	!
	! Repaint screen
	!
	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Help
	!
	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	!
	! Exit
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	RRR_FLAG% = 0%

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #GL_CHART.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 600 IF ERR = 11%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	IF GL_YYYY_PP::ACCT < GL_CHART::ACCT AND GL_EMPTY_FLAG% = 0%
	THEN
		CALL HELP_3MESSAGE(SCOPE, "Undefined Account", &
			"ERR", "GL_RPRT_FINSTA", "ERROR_UNDFACCT")
		GOTO ExitProgram
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_CHART::ACCT, 17%, 42%)

	BEG_BAL = GL_CHART::DOLLAR(0%)
	BEG_UNT = GL_CHART::UNIT(0%)
	BEG_HRS = GL_CHART::HOUR(0%)

	!
	! Handle the new year
	!
	IF CUR_PERIOD% = 1%
	THEN
		BEG_BAL, BEG_UNT, BEG_HRS = 0.0 IF INSTR(1%, "RE", &
			LEFT(GL_CHART::ACCTYPE, 1%))
		IF GL_CHART::ACCTYPE = "S" AND SUMMARY_FLAG% = 0%
		THEN
			BEG_BAL = BEG_BAL + GL_PERIOD::SUMMARYTOTAL
			SUMMARY_FLAG% = -1%
		END IF
	END IF

520	!******************************************************************
	! Total up all of the GL-YYYY-PP records for a given account
	!******************************************************************

	CHG_DOL, CHG_UNT, CHG_HRS = 0.0

	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, KEY #0% GE GL_CHART::ACCT, REGARDLESS
	USE
		CONTINUE 540
	END WHEN

530	!
	! Handle any special junk in RRR_FLAG%
	!
	SELECT RRR_FLAG%

	!
	! Repaint screen
	!
	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Help
	!
	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	!
	! Exit
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	RRR_FLAG% = 0%

	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		GL_EMPTY_FLAG% = -1%
		CONTINUE 540
	END WHEN

	IF GL_YYYY_PP::ACCT = GL_CHART::ACCT
	THEN
		CHG_DOL = CHG_DOL + GL_YYYY_PP::AMOUNT
		CHG_UNT = CHG_UNT + GL_YYYY_PP::UNITS
		CHG_HRS = CHG_HRS + GL_YYYY_PP::HOURS

		GOTO 530
	END IF

540	GL_CHART::CURDOL	= BEG_BAL + CHG_DOL
	GL_CHART::CURUNIT	= BEG_UNT + CHG_UNT
	GL_CHART::CURHOUR	= BEG_HRS + CHG_HRS

	!
	! Update chart of accounts
	!
	UPDATE #GL_CHART.CH%

	GOTO 510

600	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Handle end of working financial set
	!
	IF GL_YYYY_PP::ACCT > GL_CHART::ACCT
	THEN
		CALL HELP_3MESSAGE(SCOPE, "Undefined Account", &
			"ERR", "GL_RPRT_FINSTA", "ERROR_UNDFACCT")
		GOTO ExitProgram
	END IF

800	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	!*****************************************************************
	! Open work file
	!*****************************************************************

	OPEN FINSTA_WORK_FILE$ FOR OUTPUT AS FILE FINSTA.WRK%

900	!******************************************************************
	! Open financial file
	!******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_FINSTA.OPN"
	USE
		FILENAME$ = "GL_FINSTA"
		CONTINUE HelpError
	END WHEN

910	WHEN ERROR IN
		RESET #GL_FINSTA.CH%
	USE
		CONTINUE 1000
	END WHEN

920	WHEN ERROR IN
		GET #GL_FINSTA.CH%, REGARDLESS
	USE
		CONTINUE 1000
	END WHEN

	FINSTA.LOOP% = FINSTA.LOOP% + 1%
	SMG_SCROLL::BOT_ARRAY, SMG_SCROLL::END_ELEMENT = FINSTA.LOOP%

	POINTER_RFA(FINSTA.LOOP%) = GETRFA(GL_FINSTA.CH%)
	REP$(FINSTA.LOOP%) = "  (" + GL_FINSTA::PROMPT + ") " + &
		TRM$(GL_FINSTA::DESCR)
	SELECTED%(FINSTA.LOOP%) = 0%

	GOTO 920

	%PAGE

1000	!******************************************************************
	! PROGRAM RESTART POINT
	!******************************************************************

	SMG_SCROLL::CUR_LINE = 1%

1010	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	V% = DSPL_SCROLL(SMG_SCROLL, REP$(), 0%, "PAINT")

1030	SCOPE::PRG_ITEM = ""
	OPTLIST$ = "Select selectAll Clear Print Help eXit"
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
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1030

	END SELECT

	SELECT OPT$
	CASE "X"
		GOTO ExitProgram

	CASE "P"
		!******************************************************
		! Write selected report list
		!******************************************************

	!++
	!
	! Abstract:PRINT
	!	^*Print Statements\*
	!	.b
	!	.lm +5
	!	When the statements to be printed have been selected, type
	!	^*<P>\*, then press ^*<Do>\*, or use the ^*right arrow key\*
	!	or the ^*left arrow key\* until ^*Print\* is highlighted,
	!	then press ^*<Do>\*. The report settings screen will appear.
	!	.b
	!	If no statement has been selected, the messages, "^*Aborting.
	!	No statements selected. Press <RESUME> to continue\*," will
	!	appear at the bottom of the screen.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Print Financial Statements
	!	.x Print>Financial Statements
	!	.x Financial Statements>Print
	!
	!--

		IF WORKING_FINANCIAL$ <> ""
		THEN
			PRINT #FINSTA.WRK%, WORKING_FINANCIAL$ + &
				" !working financial report"
			WORKING_FINANCIAL$ = ""
		END IF

		PRINT_TEST% = 0%

		FOR TEST_LOOP% = 1% TO FINSTA.LOOP%

1050			IF SELECTED%(TEST_LOOP%)
			THEN
				WHEN ERROR IN
					GET #GL_FINSTA.CH%, &
						RFA POINTER_RFA(TEST_LOOP%), &
						REGARDLESS
				USE
					FILENAME$ = "GL_FINSTA"
					CONTINUE HelpError
				END WHEN

				!
				! Check for proper extension
				!
				IF INSTR(1%, GL_FINSTA::CMDFIL, ".") = 0%
				THEN
					GL_FINSTA::CMDFIL = &
						EDIT$(GL_FINSTA::CMDFIL, -1%) + ".FS"
				END IF

				!
				! Check to see if financial cmd file exists
				!
				IF FIND_FILEEXISTS(GL_FINCMD.DEV$ + GL_FINSTA::CMDFIL, FLAG%) = 0%
				THEN
					CALL ENTR_3MESSAGE(SCOPE, "Command file is missing for " + &
						GL_FINSTA::PROMPT + " Skipping. . .", 0%)
				ELSE
					PRINT_TEST% = -1%
					PRINT #FINSTA.WRK%, "<>"
					PRINT #FINSTA.WRK%, "<CMD>"; TRM$(GL_FINSTA::CMDFIL)
					PRINT #FINSTA.WRK%, "<TITLE>";TRM$(GL_FINSTA::REPTITLE)
					PRINT #FINSTA.WRK%, "<TITLEA>";TRM$(GL_FINSTA::REPTITLEA)
					PRINT #FINSTA.WRK%, "<TITLEB>";TRM$(GL_FINSTA::REPTITLEB)
					PRINT #FINSTA.WRK%, "<TITLEC>";TRM$(GL_FINSTA::REPTITLEC)
					PRINT #FINSTA.WRK%, "<TITLED>";TRM$(GL_FINSTA::REPTITLED)
					PRINT #FINSTA.WRK%, "<TYPE>";TRM$(GL_FINSTA::FINTYPE)

					FOR LOOP% = 1% TO 8%
						IF GL_FINSTA::FINCMD(LOOP%) <> ""
						THEN
							PRINT #FINSTA.WRK%, "<INP>"; &
								TRM$(GL_FINSTA::FINCMD(LOOP%))
						END IF
					NEXT LOOP%

					PRINT #FINSTA.WRK%, "<>"
				END IF
			END IF
		NEXT TEST_LOOP%

		IF PRINT_TEST%
		THEN
			V$ = SYS('8'C + '14'C + "   " + '15'C + "FINSTA")

			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
			SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

			CALL SUBR_3EXITPROGRAM(SCOPE, &
				"RUN CMC:UTL_REPORT", "FINSTA")
		ELSE
			CALL ENTR_3MESSAGE(SCOPE, &
				"Aborting.  No statements selected", 0%)
			GOTO 1030
		END IF

	CASE "S"	! Select_on
	!++
	!
	! Abstract:SELECT
	!	^*Select Financial Statements\*
	!	.b
	!	.lm +5
	!	The ^*Select\* command is used to select the statements to be
	!	printed. Before turning on the Select command, position the
	!	pointer on the first statement to be selected. Press ^*<S>\* to
	!	select the Select command. Press ^*<Do>\*. The Select command is
	!	now turned on. With the ^*down arrow\* key, move the pointer one
	!	line below the last statement to be selected. Press ^*<Do>\*.
	!	.b
	!	Statements which have been selected will be flagged with
	!	an ^*_*\*.
	!	.b
	!	To select additional statements, repeat the above sequence
	!	of commands.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Select and Print Financial Statements
	!	.x Select and Print Financial Statements
	!	.x Print Financial Statements
	!
	!--

 SelectLineStart:
		TEMP$ = "Position arrow on line to start selection " + &
			"- Then press <DO> "

		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

		INP$ = "  "

		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
			1%, LEN(TEMP$) + 1%, INP$, -1%, 4096%)

		CASE SMG$K_TRM_CTRLC		! ^C
			GOTO 1000

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1030

		CASE SMG$K_TRM_DOWN, SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_F19, &
			SMG$K_TRM_UP, SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_F18

			TEMP% = DSPL_SCROLL(SMG_SCROLL, REP$(), &
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
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO SelectLineStart

		END SELECT

		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
			SPACE$(80%), 1%, 1%)

		TEMP% = DSPL_SCROLL(SMG_SCROLL, REP$(), &
			SMG$K_TRM_SELECT, "")

		SELECT_START% = SMG_SCROLL::CUR_LINE
		SMG_SCROLL::FIND_LINE = SELECT_START%
		CALL ENTR_3MESSAGE(SCOPE, &
			"Lines highlighted will be selected ", 1%)

 SelectLineEnd:
		TEMP$ = "Use arrow keys to select contents " + &
			"- Press <DO> to complete selection process"

		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

		INP$ = "  "

		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
			1%, LEN(TEMP$) + 1%, INP$, -1%, 4096%)

		CASE SMG$K_TRM_CTRLC		! ^C
			TEMP% = DSPL_SCROLL(SMG_SCROLL, &
				REP$(), &
				SMG$K_TRM_REMOVE, &
				"PAINT")

			GOTO 1030

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			TEMP% = DSPL_SCROLL(SMG_SCROLL, &
				REP$(), &
				SMG$K_TRM_REMOVE, &
				"PAINT")
			GOTO 1030

		CASE SMG$K_TRM_DOWN, SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_F19, &
			SMG$K_TRM_UP, SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_F18

			TEMP% = DSPL_SCROLL(SMG_SCROLL, REP$(), &
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
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO SelectLineEnd

		END SELECT

		SELECT_END% = SMG_SCROLL::CUR_LINE

		IF SELECT_START% > SELECT_END%
		THEN
			TEMP% = SELECT_END%
			SELECT_END% = SELECT_START%
			SELECT_START% = TEMP%
		END IF

		FOR TEST_LOOP% = SELECT_START% TO SELECT_END%
			REP$(TEST_LOOP%) = "*" + RIGHT(REP$(TEST_LOOP%), 2%)
			SELECTED%(TEST_LOOP%) = -1%
		NEXT TEST_LOOP%

		TEMP% = DSPL_SCROLL(SMG_SCROLL, &
			REP$(), &
			SMG$K_TRM_REMOVE, &
			"PAINT")

		CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	CASE "A"	! Select_All
	!++
	!
	! Abstract:SELECTALL
	!	^*Select All Financial Statements\*
	!	.b
	!	.lm +5
	!	The ^*Select All\* command is used to select all
	!	the statements to be
	!	printed.
	!	.b
	!	All statements which will be flagged with
	!	an *_*.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Select and Print Financial Statements
	!	.x Select and Print Financial Statements
	!	.x Print Financial Statements
	!
	!--

		FOR TEST_LOOP% = 1% TO FINSTA.LOOP%
			REP$(TEST_LOOP%) = "*" + RIGHT(REP$(TEST_LOOP%), 2%)
			SELECTED%(TEST_LOOP%) = -1%
		NEXT TEST_LOOP%

		TEMP% = DSPL_SCROLL(SMG_SCROLL, &
			REP$(), &
			0%, &
			"PAINT")

	CASE "C"
		!*************************************************************
		! CLEAR all selected reports
		!*************************************************************
	!++
	!
	! Abstract:CLEAR
	!	^*Clear Selected Statements\*
	!	.b
	!	.lm +5
	!	This command voids the selection of
	!	financial statements to be printed. Type ^*<C>\*, then
	!	press ^*<Do>\*.
	!	.b
	!	The selected flag (represented by the
	!	^*_*\* to the left of each selected financial statement)
	!	will be removed.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Clear Selected Financial Statements
	!	.x Clear Selected Financial Statements
	!	.x Financial Statements>Clear Selected
	!
	!--

		FOR LOOP% = 1% TO FINSTA.LOOP%
			REP$(LOOP%) = " " + RIGHT(REP$(LOOP%), 2%)
			SELECTED%(LOOP%) = 0%
		NEXT LOOP%

		V% = DSPL_SCROLL(SMG_SCROLL, REP$(), 0%, "PAINT")

	!
	! Help
	!
	CASE "H"
		CALL HELP_34MESSAGE(SCOPE, "", "H", &
			SCOPE::PRG_PROGRAM, "", "HELP")

	END SELECT

	GOTO 1030

	%PAGE

 ExitProgram:
10000	!**************************************************************
	! KILL TEMP FILES
	!**************************************************************

	CLOSE FINSTA.WRK%

 !	WHEN ERROR IN
 !		KILL FINSTA_WORK_FILE$ WHILE (-1)
 !	USE
 !		CONTINUE 10010
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(FINSTA_WORK_FILE$ + ";*")

10010	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
	SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!**************************************************************
	! ERROR TRAPPING
	!**************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
