1	%TITLE "Resync Running Totals"
	%SBTTL "GL_SPEC_RESYNC"
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
	!	The ^*Resync Running Totals\* process will read all open General
	!	Ledger transaction files and update the real account running totals in the
	!	Chart of Accounts history file. Nominal accounts are not updated.
	!	.b
	!	The regular posting routines normally update the running
	!	totals. The purpose of this routine is to reset or resynchronize
	!	the running totals only in the event the running totals, due
	!	to some abnormal circumstances such as a system crash, become
	!	distorted or out of synchronization.
	!	.b
	!	One purpose of the running total account is to display current
	!	balances on real accounts when a posting transmittal is printed.
	!	.lm -5
	!
	! Index:
	!	.x Resynchronize Running Balance>General Ledger
	!	.x General Ledger>Resynchronize Running Balance
	!	.x Running Balance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_RESYNC/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_SPEC_RESYNC, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_RESYNC.OBJ;*
	!
	! Author:
	!
	!	04/19/87 - Kevin Handy
	!
	! Modification history:
	!
	!	10/21/87 - Robert Peterson
	!		Add interrupt menu during create of work file
	!
	!	05/13/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error handler,
	!		changed to = "GL_" + YYYY_PP$.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD) GL_PERIOD_CDD GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART) GL_CHART_CDD GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP) GL_YYYY_PP_CDD GL_YYYY_PP

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	DIM GL_YYYY_PP.CH%(20%), GLPERIOD$(20%)
	DIM GL_YYYY_PP_FILE$(100)

	%PAGE

	RRR_FLAG% = 0%

	ON ERROR GOTO 19000

	GL_YYYY_PP.CH%(I%) = 5% + I% FOR I% = 1% TO 20%

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	CALL READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

200	!******************************************************************
	! Get period
	!******************************************************************

	CALL FIND_FILE(GL_YYYY_PP.DEV$ + "GL_*.LED", GL_YYYY_PP_FILE$(), &
		16%, "", "")

	GL_YYYY_PP_FILE% = VAL%(GL_YYYY_PP_FILE$(0%))

	IF GL_YYYY_PP_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "No GL files exist", 0%)
		GOTO ExitProgram
	END IF

	GL_YYYY_PP_FILE$(LOOP%) = &
		MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 7%) &
		FOR LOOP% = 1% TO GL_YYYY_PP_FILE%

300	!
	! Get the current period
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	CLOSE GL_PERIOD.CH%

	CUR_PERIOD% = GL_PERIOD::LASTPERCLO + 1%
	YEAR$ = GL_PERIOD::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	TEST$ = FORMAT$(VAL%(YEAR$), "<0>###") + "_" + &
		FORMAT$(CUR_PERIOD%, "<0>#")

	START_LOOP% = 0%

	FOR LOOP% = 1% TO GL_YYYY_PP_FILE%
		IF TEST$ = GL_YYYY_PP_FILE$(LOOP%)
		THEN
			START_LOOP% = LOOP%
		END IF
	NEXT LOOP%

	IF START_LOOP% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "No GL files to be updated", 0%)
		GOTO ExitProgram
	END IF

	GLPERIOD$(LOOP% - START_LOOP% + 1%) = GL_YYYY_PP_FILE$(LOOP%) &
		FOR LOOP% = START_LOOP% TO GL_YYYY_PP_FILE%

	END_LOOP% = GL_YYYY_PP_FILE% - START_LOOP% + 1%

310	!
	! Open chart file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.MOD"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

320	!
	! Open the current gl period file
	!
	NEW_YEAR% = 0%

	FOR LOOP% = 1% TO END_LOOP%
		YYYY_PP$ = GLPERIOD$(LOOP%)
		IF RIGHT(YYYY_PP$, 6%) = "01"
		THEN
			NEW_YEAR% = -1%
		END IF

		GL_YYYY_PP.CH% = GL_YYYY_PP.CH%(LOOP%)

		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
		USE
			FILENAME$ = "GL_" + YYYY_PP$
			CONTINUE HelpError
		END WHEN

	NEXT LOOP%

	%PAGE

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
		"Aligning running balances " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm Alignment - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	%PAGE

1000	!******************************************************************
	! Set running total in Chart and calculate summary total if fist
	! period
	!******************************************************************

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Account #", 17%, 32%)

	CALL ENTR_3MESSAGE(SCOPE, "Setting running balance in chart", 1%)

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

	WHEN ERROR IN
		RESET #GL_CHART.CH%
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

1010	!
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
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	RRR_FLAG% = 0%

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #GL_CHART.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 2000
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_CHART::ACCT, 17%, 42%)

	GOTO 1010 IF INSTR(1%, "RE", GL_CHART::ACCTYPE)

	BEG_BAL = GL_CHART::DOLLAR(0%)
	BEG_UNT = GL_CHART::UNIT(0%)
	BEG_HRS = GL_CHART::HOUR(0%)

	IF GL_CHART::ACCTYPE = "S" AND SUMMARY_FLAG% = 0% AND NEW_YEAR%
	THEN
		BEG_BAL = BEG_BAL + GL_PERIOD::SUMMARYTOTAL
		SUMMARY_FLAG% = -1%
	END IF

1020	!******************************************************************
	! Total up all of the GL-YYYY-PP records for a given account
	!******************************************************************

	CHG_DOL, CHG_UNT, CHG_HRS = 0.0

	FOR LOOP% = 1% TO END_LOOP%

		WHEN ERROR IN
			FIND #GL_YYYY_PP.CH%(LOOP%), KEY #0% GE GL_CHART::ACCT
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1040
		END WHEN

1030		!
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
			SMG_STATUS% = &
				SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

			CALL MENU_3INTERRUPT(SCOPE)

			SMG_STATUS% = &
				SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
				LOC(OUTP_XUNSOL) BY VALUE, &
				LOC(SCOPE::SMG_KBID) BY VALUE)

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO ExitProgram

		END SELECT

		RRR_FLAG% = 0%

		!
		! Get next gl record
		!
		WHEN ERROR IN
			GET #GL_YYYY_PP.CH%(LOOP%), REGARDLESS
		USE
			CONTINUE 1040
		END WHEN

		IF GL_YYYY_PP::ACCT = GL_CHART::ACCT
		THEN

			CHG_DOL = CHG_DOL + GL_YYYY_PP::AMOUNT
			CHG_UNT = CHG_UNT + GL_YYYY_PP::UNITS
			CHG_HRS = CHG_HRS + GL_YYYY_PP::HOURS

			GOTO 1030
		END IF

1040	NEXT LOOP%

	GL_CHART::RUNDOL	= BEG_BAL + CHG_DOL
	GL_CHART::RUNUNIT	= BEG_UNT + CHG_UNT
	GL_CHART::RUNHOUR	= BEG_HRS + CHG_HRS

	!
	! Update chart of accounts
	!
	WHEN ERROR IN
		UPDATE #GL_CHART.CH%
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	GOTO 1010

2000	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Process complete", 0%)

	!******************************************************************
	! End of the program
	!******************************************************************
 ExitProgram:

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
