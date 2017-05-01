1	%TITLE "Test Ledger for Invalid Account"
	%SBTTL "GL_SPEC_TESTACCT"
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
	!	The ^*Test Ledger for Invalid Account(s)\* option in the Special Menu is
	!	intended to be used only under extremely unusual circumstances, since the
	!	system will allow posting to identified accounts only.  If a posting process
	!	encounters an invalid General Ledger account, the posting process is aborted.
	!	.b
	!	This process searches for any undefined account number in a General Ledger
	!	transaction file. If a record with an undefined number is found, the record
	!	will be displayed and the user will be prompted to define the account.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Test for Invalid Account
	!	.x Test for Invalid Account>General Ledger
	!	.x Invalid Account
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_TESTACCT/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_SPEC_TESTACCT,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_TESTACCT.OBJ;*
	!
	! Author:
	!
	!	12/23/86 - Kevin Handy
	!
	! Modification history:
	!
	!	10/21/87 - Robert Peterson
	!		Add interrupt menu during create of work file
	!
	!	05/14/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error handler,
	!		changed to = "GL_" + YYYY_PP$.
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Changed GL_MAINT_CHART.ID to GL_MAIN_CHART.ID
	!		Fix parameters to ENTR_3STRING.
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
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
	!	09/28/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

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
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_CHART) &
		GL_CHART.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	%PAGE

	ON ERROR GOTO 19000

	RRR_FLAG% = 0%

	!*******************************************************************
	! Initilize process
	!*******************************************************************

	CALL READ_INITIALIZE

300	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	CHART_LOOP% = 0%
	CUR_PERIOD% = GL_PERIOD::LASTPERCLO + 1%
	YEAR$ = GL_PERIOD::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")

310	!
	! Open chart of accounts
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

320	!
	! Open period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.MOD"
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

500	!
	! Paint the background
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
		"Check for invalid accounts " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Account #", 1%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Source", 2%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Reference", 3%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Tran Date", 4%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Description", 5%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Amount", 6%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Xref", 7%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Post Time", 8%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Post Date", 9%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Chk/Dep", 10%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Voucher #", 11%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Sub account", 12%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Operation", 13%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Units", 14%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Hours", 15%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Batch #", 16%, 2%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

	!++
	! Abstract:CONFIRM
	!	^*Confirm testing General Ledger Period\*
	!	.b
	!	.lm +5
	!	The ^*Confirm testing General Ledger Period\* allows for confirmation of
	!	the period which will be searched for invalid accounts.
	!	.lm -5
	!
	! Index:
	!	.x Confirm
	!
	!--
	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm testing GL period " + &
		YYYY_PP$ + " - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	%PAGE

2000	!******************************************************************
	! Test for undefined accounts
	!******************************************************************

	WHEN ERROR IN
		RESET #GL_YYYY_PP.CH%
	USE
		CONTINUE ExitProgram
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Testing for undefined account numbers", 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Account #", 17%, 32%)

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

2020	!
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

	!
	! Exit
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	RRR_FLAG% = 0%

	!
	! Get gl record
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE 5000 IF ERR = 11%
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::ACCT, 17%, 42%)

2030	WHEN ERROR IN
		FIND #GL_CHART.CH%, KEY #0% EQ GL_YYYY_PP::ACCT, REGARDLESS
	USE
		CONTINUE 2040
	END WHEN

	GOTO 2050

2040	GOSUB 18000	! Undefined account #

2050	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, KEY #0% GT GL_YYYY_PP::ACCT, REGARDLESS
	USE
		CONTINUE 5000
	END WHEN

	GOTO 2020

	%PAGE

5000	!********************************************************************
	! End process
	!********************************************************************
	CLOSE GL_PERIOD.CH%, GL_CHART.CH%, GL_YYYY_PP.CH%

	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Process completed", 0%)

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

18000	!*****************************************************************
	! Display undefined account
	!*****************************************************************
	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_YYYY_PP::ACCT, 1%, 21%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::SOURCE, 2%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::REFNO, 3%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PRNT_DATE(GL_YYYY_PP::TRANDAT, 8%), 4%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::DESCR, 5%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(GL_YYYY_PP::AMOUNT, "###,###,###.##"), 6%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::XREFNO, 7%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PRNT_TIME(GL_YYYY_PP::POSTIM, 4%), 8%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PRNT_DATE(GL_YYYY_PP::POSDAT, 8%), 9%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::CKNO, 10%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::TRANKEY, 11%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::SUBACC, 12%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::OPERATION, 13%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(GL_YYYY_PP::UNITS, "###,###,###.##"), 14%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(GL_YYYY_PP::HOURS, "###,###,###.##"), 15%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::BTHNUM, 16%, 21%)

18010	SCOPE::PRG_ITEM = "FLD001"
	GL_YYYY_PP::ACCT = ENTR_3STRING(SCOPE, &
		SMG_SCREEN_DATA%, "1;21", TEMP$, &
		GL_YYYY_PP::ACCT, 0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F14
		IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "V0  ") = 1%)
		THEN
			GL_YYYY_PP::ACCT = GL_CHART::ACCT
		END IF
		GOTO 18010

	CASE 3%

	!++
	! Abstract:FLD003
	!
	!--

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + GL_YYYY_PP::ACCT) <> 1%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Undefined account. Please re-enter", 0%)
		GOTO 18010
	END IF

18020	DELETE #GL_YYYY_PP.CH%

18030	!
	! Add the new changed record to the file
	!
	PUT #GL_YYYY_PP.CH%

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	CALL ENTR_3MESSAGE(SCOPE, "Testing for undefined account numbers", 1%)

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	RETURN

	%Page

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

19999	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CHART.ID
		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
