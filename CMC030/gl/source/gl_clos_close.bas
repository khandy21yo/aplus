1	%TITLE "CLOSE - Close the General Ledger"
	%SBTTL "GL_CLOS_CLOSE"
	%IDENT "V3.6a Calico"

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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Close the General Ledger\* routine must be executed
	!	after the completion of each accounting period. The purpose
	!	of the Close process is to advance the system to the next accounting
	!	period.
	!	.b
	!	The system will not allow any transactions to be posted
	!	to a closed General Ledger, nor can any General Ledger or
	!	Trial Balance reports be printed for a closed period. (See
	!	^*Reset the General Ledger\* routine for information on re-opening
	!	a closed General Ledger.)
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Close
	!	.x Close>General Ledger
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_CLOS_CLOSE/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_CLOS_CLOSE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_CLOS_CLOSE.OBJ;*
	!
	! Author:
	!
	!	12/23/86 - Kevin Handy
	!
	! Modification history:
	!
	!	10/31/87 - Robert Peterson
	!		Modify to allow interrupt during close process
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	07/08/93 - Kevin Handy
	!		Modified so that it used GL_PERIOD::SUMMARYACCT
	!		instead of looking for first "S" record.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/15/95 - Kevin Handy
	!		Modified to calculate the summary total when
	!		it is needed, and don't rely on the total stored
	!		in GL_PERIOD::SUMMARYTOTAL.
	!
	!	05/18/95 - Kevin Handy
	!		Clean up (check)
	!
	!	12/15/95 - Kevin Handy
	!		Change RIGHT(NUM1$()) for FORMAT$().
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	02/06/97 - Kevin Handy
	!		Added REGARDLESS to get on GL_YYYY_PP.CH.
	!		Reformat closer to 80 columns.
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
	!	01/17/2000 - Kevin Handy
	!		Open chart as "MOD" instead of "UPD" so that
	!		it is possible to close on busy systems.
	!
	!	06/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	%PAGE

	ON ERROR GOTO 19000

	CALL READ_INITIALIZE

	RRR_FLAG% = 0%

310	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.UPD"
		GET #GL_PERIOD.CH%, RECORD 1%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	IF GL_PERIOD::CLOSEFLAG = "2"
	THEN

		CALL HELP_34MESSAGE(SCOPE, "GL Reset in process", &
			"E", SCOPE::PRG_PROGRAM, "", "GLFLAG2")

		GOTO ExitProgram
	END IF

	CUR_PERIOD% = GL_PERIOD::LASTPERCLO + 1%
	YEAR$ = GL_PERIOD::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")

320	!
	! Open chart of accounts
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.MOD"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

330	!
	! Open period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.UPD"
	USE
		FILENAME$ = "GL_" +YYYY_PP$
		CONTINUE HelpError
	END WHEN

340	!
	! Get set file information
	!
	SCOPE::PRG_IDENT = "PROG"
	SCOPE::PRG_PROGRAM = "GL_CLOS_CLOSE"

500	!
	! Paint the background, and confirm close
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_SCREEN_DATA%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_SCREEN_DATA%, &
		"General Ledger Close for " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "CLOSING " + YYYY_PP$ + &
		" " + GL_PERIOD::PERIOD(CUR_PERIOD%), 4%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Account ", 6%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	SCOPE::PRG_ITEM = "CONFIRM"
	!++
	! Abstract:CONFIRM
	!	^*General Ledger Close\*
	!	.b
	!	.lm +5
	!	The ^*General Ledger Close\* requests a confirmation asking if the period
	!	displayed is the correct period to be closed. If the Ledger is closed
	!	no transactions may be posted to it nor General Ledger or Trial Balance
	!	reports printed.
	!	.lm -5
	!
	! Index:
	!
	!--

	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Confirm closing - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	!
	! Set help to program help
	!
	SCOPE::PRG_ITEM = "HELP"

	%PAGE

1000	!******************************************************************
	! Close Chart of accounts
	!******************************************************************

	IF CUR_PERIOD% = 1%
	THEN
		GOSUB CalcSummary
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "Closing", 1%)

	WHEN ERROR IN
		RESET #GL_CHART.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

1010	!
	! Find First record in the general ledger
	!
	GL_EMPTY_FLAG% = 0%

	WHEN ERROR IN
		RESET #GL_YYYY_PP.CH%
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		GL_EMPTY_FLAG% = -1%
		CONTINUE 1015
	END WHEN

1015	!
	! Set close flag in period file
	!
	GL_PERIOD::CLOSEFLAG = "1"

	WHEN ERROR IN
		UPDATE #GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

1020	!
	! Main loop starts here
	!

	!
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
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)
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

		CONTINUE 1400 IF ERR = 11%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

1030	IF GL_YYYY_PP::ACCT < GL_CHART::ACCT AND GL_EMPTY_FLAG% = 0%
	THEN
		CALL HELP_3MESSAGE(SCOPE, "Undefined Account", "ERR", "GL_CLOSE", &
			"ERROR_UNDFACCT")
		GOTO ExitProgram
	END IF

	BEG_BAL = GL_CHART::DOLLAR(0%)
	BEG_UNT = GL_CHART::UNIT(0%)
	BEG_HRS = GL_CHART::HOUR(0%)

	IF GL_CHART::CPERIOD = CUR_PERIOD%
	THEN
		BEG_BAL = GL_CHART::DOLLAR(1%)
		BEG_UNT = GL_CHART::UNIT(1%)
		BEG_HRS = GL_CHART::HOUR(1%)
	END IF


	!
	! Handle the new year
	!
	IF CUR_PERIOD% = 1%
	THEN
		BEG_BAL, BEG_UNT, BEG_HRS = 0.0 &
			IF INSTR(1%, "RE", LEFT(GL_CHART::ACCTYPE, 1%))

		!
		! Handle "S" account (first one only)
		!
		IF GL_CHART::ACCT = GL_PERIOD::SUMMARYACCT AND &
			SUMMARY_FLAG% = 0%
		THEN
			BEG_BAL = FUNC_ROUND(BEG_BAL + XSUMMARY_TOTAL, 2%)
			SUMMARY_FLAG% = -1%
		END IF
	END IF

	GOSUB 1600

	!
	! Update chart record
	!
	END_BAL = FUNC_ROUND(BEG_BAL + CHG_BAL, 2%)
	END_UNT = FUNC_ROUND(BEG_UNT + CHG_UNT, 2%)
	END_HRS = FUNC_ROUND(BEG_HRS + CHG_HRS, 2%)

	BEG_TOTAL = FUNC_ROUND(BEG_TOTAL + BEG_BAL, 2%)
	CHG_TOTAL = FUNC_ROUND(CHG_TOTAL + CHG_BAL, 2%)

	IF GL_CHART::CPERIOD <> CUR_PERIOD%
	THEN
		FOR I% = 20% TO 1% STEP -1%
			GL_CHART::DOLLAR(I%)	= GL_CHART::DOLLAR(I% - 1%)
			GL_CHART::UNIT(I%)	= GL_CHART::UNIT(I% - 1%)
			GL_CHART::HOUR(I%)	= GL_CHART::HOUR(I% - 1%)
		NEXT I%
	END IF

	GL_CHART::DOLLAR(0%) = END_BAL
	GL_CHART::UNIT(0%)   = END_UNT
	GL_CHART::HOUR(0%)   = END_HRS

	GL_CHART::CPERIOD = CUR_PERIOD%

	GOSUB 1700

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_YYYY_PP::ACCT, &
		6%, 14%)

	!
	! Handle last period in year
	!
	IF CUR_PERIOD% = GL_PERIOD::FPFY
	THEN
		SUMMARY_TOTAL = FUNC_ROUND(SUMMARY_TOTAL + END_BAL, 2%) &
			IF INSTR(1%, "RE", LEFT(GL_CHART::ACCTYPE, 1%))
		SUMMARY_ACCT$ = GL_CHART::ACCT IF GL_CHART::ACCTYPE = "S"
	END IF

	GOTO 1020

	%PAGE

1400	!
	! Handle end of report
	!
	IF GL_YYYY_PP::ACCT > GL_CHART::ACCT
	THEN
		CALL HELP_3MESSAGE(SCOPE, "Undefined Account", "ERR", "GL_CLOSE", &
			"ERROR_UNDFACCT")
		GOTO ExitProgram
	END IF

1410	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"The prior month is out of balance   " + &
		FORMAT$(BEG_TOTAL, "###,###,###.##"), 14%, 10%) &
		IF FUNC_ROUND(BEG_TOTAL, 2%) <> 0.0

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"The current month is out of balance " + &
		FORMAT$(CHG_TOTAL, "###,###,###.##"), 16%, 10%) &
		IF FUNC_ROUND(CHG_TOTAL, 2%) <> 0.0

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Ledger has been closed.", 18%, 10%)

1420	!
	! Update period file
	!
	WHEN ERROR IN
		GET #GL_PERIOD.CH%, RECORD 1%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	GL_PERIOD::LASTPERCLO = CUR_PERIOD%
	GL_PERIOD::YEAR = YEAR$

	GL_PERIOD::BTHNUM = ""

	GL_PERIOD::CLOSEFLAG = "0"

	IF CUR_PERIOD% = GL_PERIOD::FPFY
	THEN
		GL_PERIOD::SUMMARYTOTAL = SUMMARY_TOTAL
		GL_PERIOD::SUMMARYACCT	= SUMMARY_ACCT$
	END IF

	WHEN ERROR IN
		UPDATE #GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	CLOSE GL_PERIOD.CH%, GL_CHART.CH%, GL_YYYY_PP.CH%

1500	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Closing completed", 0%)

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

1600	!******************************************************************
	! Total up all of the GL-PP-YYYY records for a given account
	!******************************************************************

	CHG_BAL, CHG_UNT, CHG_HRS = 0.0

	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, KEY #0% GE GL_CHART::ACCT, REGARDLESS
	USE
		GL_EMPTY_FLAG% = -1% IF ERR = 11%
		CONTINUE 1690
	END WHEN

	!
	! Loop through period file for this account
	!
1610	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		GL_EMPTY_FLAG% = -1% IF ERR = 11%
		CONTINUE 1690
	END WHEN

	GOTO 1690 IF GL_YYYY_PP::ACCT <> GL_CHART::ACCT

	!
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
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)
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

	END SELECT

	RRR_FLAG% = 0%

	!
	! Total change for the period
	!
	CHG_BAL = FUNC_ROUND(CHG_BAL + GL_YYYY_PP::AMOUNT, 2%)
	CHG_UNT = FUNC_ROUND(CHG_UNT + GL_YYYY_PP::UNITS, 2%)
	CHG_HRS = FUNC_ROUND(CHG_HRS + GL_YYYY_PP::HOURS, 2%)

	GOTO 1610

1690	RETURN

1700	!******************************************************************
	! Update chart of accounts
	!******************************************************************

	WHEN ERROR IN
		UPDATE #GL_CHART.CH%
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	RETURN

	%PAGE

 CalcSummary:
18000	!*******************************************************************
	! Calculate summary total
	!*******************************************************************

	XSUMMARY_TOTAL = 0.0

	WHEN ERROR IN
		RESET #GL_CHART.CH%
	USE
		CONTINUE 18090
	END WHEN

18010	WHEN ERROR IN
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CONTINUE 18090
	END WHEN

	XSUMMARY_TOTAL = FUNC_ROUND(XSUMMARY_TOTAL + GL_CHART::DOLLAR(0%), 2%) &
		IF INSTR(1%, "RE", LEFT(GL_CHART::ACCTYPE, 1%))

	GOTO 18010

18090	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
