1	%TITLE "RESET - Reset the General Ledger"
	%SBTTL "GL_CLOS_RESET"
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
	!	The ^*General Ledger Reset\* allows for opening of a General Ledger period
	!	file that was previously closed. Any number, up to nineteen, of previous
	!	ledgers to be reset may
	!	be specified. Changes may then be made and new reports
	!	printed. To return to the current General Ledger, close the General Ledgers
	!	that have been reset.
	!	.lm -5
	!
	! Index:
	!	.x Reset<General Ledger
	!	.x General Ledger>Reset
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_CLOS_RESET/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_CLOS_RESET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_CLOS_RESET.OBJ;*
	!
	! Author:
	!
	!	12/24/86 - Kevin Handy
	!
	! Modification history:
	!
	!	10/21/87 - Robert Peterson
	!		Added menu for interrupts during the reset process.
	!
	!	06/05/89 - Aaron Redd
	!		Modified so that more than one month can be
	!		reset at one time.
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	08/16/89 - Kevin Handy
	!		Attempt to fix problem where GL_CHART is
	!		getting off by one period.
	!
	!	08/17/89 - Kevin Handy
	!		Fixed problem where reset would not reset
	!		properly when the year turned back.
	!
	!	01/19/89 - Frank F. Starman
	!		Check existance of each resetting period.
	!
	!	03/26/93 - Kevin Handy
	!		Fix bug in resetting over year.  Was mangling
	!		the summary amount.  Now will only reset it in
	!		the first period (January) even if you go earlier
	!		than that, since it is used only in January (I hope)
	!
	!	08/30/93 - Kevin Handy
	!		Defined FUNC_ROUND as an external function to lose
	!		a "Subscript" error at 1300.
	!
	!	09/15/94 - Kevin Handy
	!		Modified to look at FPFY in determining the
	!		previous period names.
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Coding standards.
	!		Change call to ENTR_3NUMBER to pass real instead
	!		in integer.
	!
	!	06/28/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/07/98 - Kevin Handy
	!		Change '%INCLUDE %FROM %CDD' to '%INCLUDE'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	12/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
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
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	MAP	(DP_OUTP_XUNSOL)		RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL		! (It's really an AST routine)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	CALL READ_INITIALIZE

	!******************************************************************
	! Draw initial screen, and determine how many months' worth of
	! resetting the user had in mind.
	!******************************************************************

	!
	! Open General Ledger Period Definition file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.MOD"
		GET #GL_PERIOD.CH%, RECORD 1%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	!
	! Is there a GL Close in progress?
	!
	IF GL_PERIOD::CLOSEFLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "GL Close in process", "ERR", &
			"GL_CLOSE", "ERROR_CLOSE")
		GOTO ExitProgram
	END IF

	!
	! Open General Ledger Chart of Accounts file
	!
400	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.UPD"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Determine the date of the last period closed
	!
	CUR_PERIOD% = GL_PERIOD::LASTPERCLO
	YEAR$ = GL_PERIOD::YEAR

	YYYY_PP1$ = YEAR$ + "_" + FORMAT$(GL_PERIOD::LASTPERCLO, "<0>#")

	!
	! Paint the background
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, &
		SMG_SCREEN_DATA%, SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		" General Ledger reset for " + &
		TRM$(SCOPE::PRG_COMPANY) + " ", SMG$K_TOP)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, YYYY_PP1$ + &
		" was the last period closed.", 6%, 20%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 2%, 2%)

	!
	! How many months did the user wish to reset?
	!
	RESET% = 0%

450	RESET% = ENTR_3NUMBER(SCOPE, SMG_SCREEN_DATA%, "9;20", &
		"Reset the GL by how many periods ", RESET% * 1.0, &
		64%, "##", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 450

	END SELECT

	GOTO 450 IF (RESET% > 11%) OR (RESET% < 1%)

	!
	! Open specified period files to make sure they exists
	!
	FOR LOOP% = 1% TO RESET%

		CUR_PERIOD% = GL_PERIOD::LASTPERCLO - LOOP% + 1%
		YEAR$ = GL_PERIOD::YEAR

		WHILE (CUR_PERIOD% < 1%)
			CUR_PERIOD% = CUR_PERIOD% + GL_PERIOD::FPFY
			YEAR$ = FORMAT$(VAL%(YEAR$) - 1%, "<0>###")
		NEXT

		!
		! Determine when we're resetting to
		!
		YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")

460		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
		USE
			CONTINUE 465 IF ERR = 5%
			FILENAME$ = "GL_" + YYYY_PP$
			CONTINUE HelpError
		END WHEN

		CLOSE GL_YYYY_PP.CH%

	NEXT LOOP%
	GOTO 470

465	RESET% = LOOP% - 1%
	YYYY_PP_MIST$ = YYYY_PP$

470	!
	! Now determine how far back we go
	!
	CUR_PERIOD% = GL_PERIOD::LASTPERCLO - RESET%
	YEAR$ = GL_PERIOD::YEAR

	!
	! Resetting back and skipping a year
	! (i.e. 1989_xx reset to 1987_xx)
	!
	WHILE (CUR_PERIOD% < 1%)
		CUR_PERIOD% = CUR_PERIOD% + GL_PERIOD::FPFY
		YEAR$ = FORMAT$(VAL%(YEAR$) - 1%, "<0>###")
	NEXT

	!
	! Determine when we're resetting to
	!
	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")

	%PAGE

500	!******************************************************************
	! Program restart point
	!******************************************************************

	!
	! Redraw screen background
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"WARNING! GL " + YYYY_PP_MIST$ + &
		" period file doesn't exist.", &
		5%, 15%) &
		IF YYYY_PP_MIST$ <> ""

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"The General Ledger will be reset to " + YYYY_PP$ + ".", &
		6%, 15%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		YYYY_PP$ + " will then be the last month closed.", 7%, 15%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Account ", 9%, 15%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 2%, 2%)

	!
	! Confirm reset with user
	!
	SCOPE::PRG_ITEM = "CONFIRM"

	GOTO ExitProgram IF ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Confirm resetting - then press <Do> ", "N", 0%, &
		"", "") <> "Y"

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	%PAGE

1000	!******************************************************************
	! Begin resetting GL Chart
	!******************************************************************

	!
	! Set close flag in period file
	!
	GL_PERIOD::CLOSEFLAG = "2"
	WHEN ERROR IN
		UPDATE #GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	!
	! Start the reset
	!
1200	CALL ENTR_3MESSAGE(SCOPE, "Starting to reset the general ledger", 1%)

	WHEN ERROR IN
		RESET #GL_CHART.CH%
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		OUTP_XUNSOL, SCOPE::SMG_KBID)

	RRR_FLAG% = 0%

	SUMMARY_TOTAL = 0.0
	SUMMARY_FLAG% = 0%

	%PAGE

	!******************************************************************
	! Main reset loop starts here
	!******************************************************************

	!
	! Handle any special junk in RRR_FLAG%
	!
1300	SELECT RRR_FLAG%

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
	! Interrupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			OUTP_XUNSOL, SCOPE::SMG_KBID)

	END SELECT

	RRR_FLAG% = 0%

	!
	! Get next chart record
	!
	WHEN ERROR IN
		GET #GL_CHART.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 1400 IF ERR = 11%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Is this one closed properly?
	!
	GOTO 1300 IF GL_PERIOD::LASTPERCLO <> GL_CHART::CPERIOD

	!
	! Roll back RESET% number of months
	!
	WHILE GL_CHART::CPERIOD <> CUR_PERIOD%

		FOR I% = 0% TO 19%
			GL_CHART::DOLLAR(I%) = GL_CHART::DOLLAR(I% + 1%)
			GL_CHART::UNIT(I%) = GL_CHART::UNIT(I% + 1%)
			GL_CHART::HOUR(I%) = GL_CHART::HOUR(I% + 1%)
		NEXT I%

		GL_CHART::DOLLAR(20%) = 0.0
		GL_CHART::UNIT(20%) = 0.0
		GL_CHART::HOUR(20%) = 0.0

		GL_CHART::CPERIOD = GL_CHART::CPERIOD - 1%
		GL_CHART::CPERIOD = GL_PERIOD::FPFY IF GL_CHART::CPERIOD <= 0%

		IF GL_CHART::CPERIOD = 1%
		THEN
			IF GL_CHART::ACCTYPE = "R" OR GL_CHART::ACCTYPE = "E"
			THEN
				SUMMARY_TOTAL = FUNC_ROUND(SUMMARY_TOTAL + &
					GL_CHART::DOLLAR(1%), 2%)
				SUMMARY_FLAG% = -1%
			END IF
		END IF
	NEXT

	WHEN ERROR IN
		UPDATE #GL_CHART.CH%
	USE
		CONTINUE 1400 IF ERR = 11%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_CHART::ACCT, &
		9%, 25%)

	!
	! Go back to the top of the loop
	!
	GOTO 1300

	%PAGE

	!******************************************************************
	! Finish up reset
	!******************************************************************

1400	!
	! Fix period file to reflect changes
	!
	WHEN ERROR IN
		GET #GL_PERIOD.CH%, RECORD 1%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	GL_PERIOD::LASTPERCLO	= CUR_PERIOD%
	GL_PERIOD::YEAR		= YEAR$

	GL_PERIOD::BTHNUM	= SPACE$(8%)
	GL_PERIOD::CLOSEFLAG	= "0"

	IF SUMMARY_FLAG%
	THEN
		GL_PERIOD::SUMMARYTOTAL = SUMMARY_TOTAL
	END IF

	WHEN ERROR IN
		UPDATE #GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Reset completed", 0%)

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************
	CLOSE GL_PERIOD.CH%, GL_CHART.CH%

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

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

	!
	! Untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32000	!******************************************************************
	! End of GL_CLOS_RESET
	!******************************************************************
	END
