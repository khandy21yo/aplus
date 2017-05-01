1	%TITLE "Purge AR Closed and Customer Files"
	%SBTTL "AR_SPEC_PURGE"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	!	This program will remove records from the
	!	closed file (AR__CLOSED) based on the retention cycle.
	!	Then it will test the purge flag in the customer
	!	master file (AR__35CUSTOM).
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_SPEC_PURGE/LINE
	!	$ LINK/EXECUTABLE=AR_EXE: AR_SPEC_PURGE,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_SPEC_PURGE.OBJ;*
	!
	! Author:
	!
	!	06/05/89 - Aaron Redd
	!
	! Modification history:
	!
	!	06/06/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	10/26/92 - Kevin Handy
	!		Removed rest of customer file purge section.
	!
	!	06/15/94 - Kevin Handy
	!		Modified to open AR_TEMP_CLOSE on AR_CLOSED.DEV$
	!		instead of UTL_WORK.DEV$ so that it ends up on
	!		the correct drive.
	!
	!	06/15/94 - Kevin Handy
	!		Removed excess number of tabs on some lines.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/27/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	06/27/95 - Kevin Handy
	!		Reinstate code to delete customers flagged
	!		as "P" (Purgable)
	!
	!	12/15/95 - Kevin Handy
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	04/05/96 - Kevin Handy
	!		Increased RECORD_RFA size from 100 to 1000.
	!
	!	04/23/96 - Kevin Handy
	!		Modified to handle Balance foreward on a per line
	!		basis, instead of a per invoice basis.
	!		Rewrote handling section so didn't need a
	!		flag to say that we already pushed out an
	!		invoice.
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Add SCOPE:: to SMG_PBID and SMG_KBID references
	!
	!	04/20/99 - Kevin Handy
	!		Fixes for unsolicited input
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!
	!	03/07/2005 - Kevin Handy
	!		Change RECORD_RFA max from 1000 to 48000. (marco)
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP	(AR_OPEN)	AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP	(AR_CLOSED)	AR_CLOSED_CDD	AR_CLOSED

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	MAP	(DP_OUTP_XUNSOL)		RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG			OUTP_XUNSOL
						! (It's really an AST routine)

	!
	! Declare constants
	!
	DECLARE LONG	CONSTANT	RECORD_ARRAY = 48000%

	!
	! Declare variables
	!
	DECLARE RFA RECORD_RFA(RECORD_ARRAY), RECORD_RFA

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Assign a channel for the TEMP file
	!
	CALL ASSG_CHANNEL(AR_TEMP_CLOSE.CH%, STAT%)

	!
	! Open Accounts Receivable Open Item file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Open Accounts Receivable Closed file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.UPD"
	USE
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Receivable Customer file
	!
330	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.MOD"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Figure out what in the world needs done (a whole lot)
	!
340	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	!
	! Open AR Control file
	!
350	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.MOD"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF AR_CONTROL::CLOSEFLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AR Close in process", "ERR", "AR_CLOSED", &
			"ERROR_CLOSE")
		GOTO ExitProgram
	END IF

	IF AR_CONTROL::CLOSEFLAG = "2"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AR Reset in process", "ERR", "AR_RESET", &
			"ERROR_RESET")
		GOTO ExitProgram
	END IF

	CUR_PERIOD% = AR_CONTROL::LASTPERCLOSE
	RETENTION% = AR_CONTROL::RETAIN

	YEAR_OFF% = RETENTION% / GL_PERIOD::FPFY

	PERIOD_OFF% = RETENTION% - YEAR_OFF% * GL_PERIOD::FPFY

	PERIOD_OFF% = CUR_PERIOD% - PERIOD_OFF%

	IF PERIOD_OFF% < 1%
	THEN
		YEAR_OFF% = YEAR_OFF% + 1%
		PERIOD_OFF% = GL_PERIOD::FPFY + PERIOD_OFF%
	END IF

	YEAR$ = FORMAT$(VAL%(AR_CONTROL::YEAR) - YEAR_OFF%, "<0>###")

	YYYY_PP$ = YEAR$ + "_" + FORMAT$(PERIOD_OFF%, "<0>#")

360	!======================================================================
	! AR_CLOSED file (create, open read/write)
	!======================================================================

	CALL WRIT_CURPROTECTION(AR_CLOSED.PRO$, ST%)

	WHEN ERROR IN
		OPEN AR_CLOSED.DEV$ + "AR_TEMP_CLOSE.LED" FOR OUTPUT &
			AS FILE AR_TEMP_CLOSE.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP AR_CLOSED, &
			PRIMARY KEY (AR_CLOSED::CUSNUM, AR_CLOSED::INVNUM, &
				AR_CLOSED::TRATYP)  DUPLICATES, &
			ALTERNATE KEY (AR_CLOSED::SALNUM, AR_CLOSED::CUSNUM, &
				AR_CLOSED::INVNUM, AR_CLOSED::TRATYP) &
			DUPLICATES CHANGES, &
			ACCESS MODIFY, ALLOW MODIFY
	USE
		FILENAME$ = "AR_TEMP_CLOSE"
		CONTINUE HelpError
	END WHEN

	CALL WRIT_CURPROTECTION(OLD_PROT$, ST%)

500	!
	! Paint the background
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, &
		SMG_SCREEN_DATA%, SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		" Purge Accounts Receivable " + &
		TRM$(SCOPE::PRG_COMPANY) + " ", SMG$K_TOP)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"All transaction dated before " + &
		YYYY_PP$ + " will be purged", 4%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Customer # ", 6%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 2%, 2%)

	%PAGE

	!
	! Confirm whether or not to purge
	!
	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm purge process  - then press <Do> ", &
		"N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "") IF INP$ <> "Y"

	!
	! Set help message
	!
	SCOPE::PRG_ITEM = "HELP"

	%PAGE

1000	!******************************************************************
	! Purge AR closed file
	!******************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Purging closed file", 1%)

	WHEN ERROR IN
		RESET #AR_CLOSED.CH%
	USE
		CONTINUE 2000
	END WHEN

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

	!
	! Set close flag in control file
	!
1015	GET #AR_CONTROL.CH%, RECORD 1%

	AR_CONTROL::CLOSEFLAG = "3"

	UPDATE #AR_CONTROL.CH%

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
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	!
	! Exit Not allowed
	!
	END SELECT

	RRR_FLAG% = 0%

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_CLOSED.CH%
		RECORD_RFA = GETRFA(AR_CLOSED.CH%)
	USE
		CONTINUE 2000 IF ERR = 11%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

1030	IF AR_CLOSED::CUSNUM <> AR_35CUSTOM::CUSNUM
	THEN
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			AR_CLOSED::CUSNUM, &
			6%, 23%)

		WHEN ERROR IN
			GET #AR_35CUSTOM.CH%, &
				KEY #0% EQ AR_CLOSED::CUSNUM, &
				REGARDLESS
		USE
			AR_35CUSTOM::CUSNUM = AR_CLOSED::CUSNUM
			AR_35CUSTOM::METHOD = "O"
			CONTINUE 1040
		END WHEN
	END IF

1040	IF TEST_CUSNUM$ <> AR_CLOSED::CUSNUM
	THEN
		GOSUB PutRecord

		GET #AR_CLOSED.CH%, RFA RECORD_RFA
		RECORD_LOOP% = 0%

		INVNUM$ = "ZZZZZZZZZZZZZZZ"
		CURRENT_FLAG%, FIRST_PASS% = 0%

		TEST_CUSNUM$ = AR_CLOSED::CUSNUM
	ELSE
		IF (AR_CLOSED::INVNUM <> INVNUM$) OR &
			(AR_35CUSTOM::METHOD = "B")
		THEN
			GOSUB PutRecord

			GET #AR_CLOSED.CH%, RFA RECORD_RFA
			RECORD_LOOP% = 0%

			CURRENT_FLAG% = 0%
		END IF
	END IF

	IF YYYY_PP$ <= LEFT(AR_CLOSED::UPDATED, 4%) + &
		"_" + MID(AR_CLOSED::UPDATED, 5%, 2%)
	THEN
		CURRENT_FLAG% = -1%
	END IF

	INVNUM$ = AR_CLOSED::INVNUM

	RECORD_LOOP% = RECORD_LOOP% + 1%
	RECORD_RFA(RECORD_LOOP%) = GETRFA(AR_CLOSED.CH%)

	GOTO 1020

	%PAGE

2000	!
	! Put last record out to the close file
	!
	GOSUB PutRecord IF RECORD_LOOP% <> 0%

	!******************************************************************
	! Purge AR customer file
	!******************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Purging customer file file", 1%)

	WHEN ERROR IN
		RESET #AR_35CUSTOM.CH%
	USE
		CONTINUE 3000
	END WHEN

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
	! Interrupt
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
		GET #AR_35CUSTOM.CH%
	USE
		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	GOTO 2020 IF AR_35CUSTOM::SSTATUS <> "P"

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AR_35CUSTOM::CUSNUM, &
		6%, 23%)

2030	WHEN ERROR IN
		FIND #AR_OPEN.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 2040 IF ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 2060

2040	WHEN ERROR IN
		FIND #AR_TEMP_CLOSE.CH%, &
			KEY #0% EQ AR_35CUSTOM::CUSNUM, &
			REGARDLESS
	USE
		CONTINUE 2050 IF ERR = 155%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	GOTO 2060

2050	WHEN ERROR IN
		DELETE #AR_35CUSTOM.CH%
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

2060	GOTO 2020

3000	!
	! Kill AR_CLOSED file and rename temp file to close
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Renaming close temp file to close file. . .", 1%)

	CLOSE AR_CLOSED.CH%, AR_TEMP_CLOSE.CH%

 !	KILL AR_CLOSED.DEV$ + "AR_CLOSED.LED" FOR LOOP% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(AR_CLOSED.DEV$ + "AR_CLOSED.LED;*")

3010	NAME AR_CLOSED.DEV$ + "AR_TEMP_CLOSE.LED" AS &
		AR_CLOSED.DEV$ + "AR_CLOSED.LED"

3090	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Purge completed", 0%)

4000	!
	! Update Control file
	!
	WHEN ERROR IN
		GET #AR_CONTROL.CH%, RECORD 1%

		AR_CONTROL::CLOSEFLAG = "0"

		UPDATE #AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	CLOSE AR_CLOSED.CH%, AR_CONTROL.CH%, AR_TEMP_CLOSE.CH%

5000	!
	! Kill temp files
	!
 !	KILL AR_CLOSED$ + "AR_TEMP_CLOSE.LED" FOR LOOP% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(AR_CLOSED$ + "AR_TEMP_CLOSE.LED;*")

5010	!
	! Exit program
	!
 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 PutRecord:
18000	!*******************************************************************
	! Put records in temporary file
	!*******************************************************************
	GOTO PutReturn IF (RECORD_LOOP% = 0%) OR (CURRENT_FLAG% = 0%)

	FOR LOOP% = 1% TO RECORD_LOOP%

		WHEN ERROR IN
			GET #AR_CLOSED.CH%, RFA RECORD_RFA(LOOP%)
		USE
			FILENAME$ = "AR_CLOSED"
			CONTINUE HelpError
		END WHEN

18010		WHEN ERROR IN
			PUT #AR_TEMP_CLOSE.CH%
		USE
			FILENAME$ = "AR_TEMP_CLOSE"
			CONTINUE HelpError
		END WHEN

	NEXT LOOP%

 PutReturn:
	RETURN

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of AR_SPEC_PURGE
	!******************************************************************
	END
