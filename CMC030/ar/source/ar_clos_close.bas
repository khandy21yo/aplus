1	%TITLE "Accounts Receivable Closing Program"
	%SBTTL "AR_CLOS_CLOSE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	The ^*Close Accounts Receivable Ledger\* function is used to close the
	!	AR Ledger for the month. The process will update the balance
	!	forward customers and transfer invoices with zero balances
	!	to the AR History file for open item customers.
	!	.b
	!	^*Note:\* Before closing the Accounts Receivable Ledger,
	!	the service charges should be calculated and the
	!	AR Statements, final AR Register, and final
	!	AR Aging report should be printed.
	!	.lm -5
	!
	! Index:
	!	.x Close>Ledger
	!	.x Ledger>Close
	!
	! Option:
	!
	!	AR_CLOS_CLOSE$CONFIRM
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_CLOS_CLOSE/LINE
	!	$ LINK/EXECUTABLE=AR_EXE: AR_CLOS_CLOSE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_CLOS_CLOSE.OBJ;*
	!
	! Author:
	!
	!	03/23/88 - Kevin Handy
	!
	! Modification history:
	!
	!	10/21/87 - Robert Peterson
	!		Added interrupt menu during create of work file
	!
	!	08/26/88 - Kevin Handy
	!		Many, many fixes in an attempt to make it work
	!		correctly.
	!
	!	09/27/88 - Kevin Handy
	!		Increased RFA array size, fixed finishing up of
	!		AR control file.
	!
	!	10/25/88 - Kevin Handy
	!		Modified so that will not close items that were
	!		updated after the current period (UPDATED field).
	!
	!	12/02/88 - Kevin Handy
	!		Modified calculation of the closing date, so that
	!		it will get the year right.
	!
	!	07/20/90 - Kevin Handy
	!		Modified to open closed file in update mode to try
	!		to gain a little bit more speed.
	!
	!	06/06/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	10/29/91 - Dan Perkins
	!		Added common statements for Aging Function,
	!		AR_FUNC_AGE.
	!
	!	02/08/92 - Kevin Handy
	!		Re-did common statements so they matched those
	!		in AR_FUNC_AGE.
	!
	!	03/12/92 - Kevin Handy
	!		Removed duplicate error trapping (check)
	!
	!	10/14/92 - Frank F. Starman
	!		Change file open to PST.
	!		Modified calculation of the closing date, so that
	!		it will get the date right.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/05/93 - Kevin Handy
	!		Added AR_OPEN::DUEDATE and AR_OPEN::DISCOUNTDATE.
	!
	!	11/24/93 - Kevin Handy
	!		Modified to use COPY_COPYRECORDS function instead
	!		of manually copying over the closed file.  This
	!		may gain us lots of speed (I hope).
	!
	!	11/24/93 - Kevin Handy
	!		Restructured the error trap code.
	!
	!	01/04/93 - Kevin Handy
	!		Code is faster after 11/24/93 change. Now closes
	!		in about 1/3 of the original time.
	!
	!	01/04/93 - Kevin Handy
	!		Increased dimension for RFA array from 2000 to 6000.
	!
	!	01/10/94 - Kevin Handy
	!		Removed error trap for 2230 which no longer exists.
	!
	!	01/21/94 - Kevin Handy
	!		Reformatted to 80 columns.
	!
	!	01/26/94 - Kevin Handy
	!		Modified to handle closing balance forward customers
	!		correctly.
	!
	!	04/04/95 - Kevin Handy
	!		(V3.6)
	!		Fixed for KingB. Was getting stuck on 4000003.
	!		Test case when at end of customer file, but not
	!		at end of open file, did not work right,
	!
	!	05/03/95 - Kevin Handy
	!		More fixed for end of file conditions.
	!
	!	12/04/95 - Kevin Handy
	!		Changed PTDSALES to LAST_PAID.
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	06/19/96 - Kevin Handy
	!		Reformat source code.
	!
	!	02/13/1997 - Kevin Handy
	!		Modifications to make sure that "_NEW" files
	!		are killed if the user exits without closing.
	!
	!	02/18/97 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/21/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/20/99 - Kevin Handy
	!		Fix calls to unsolicited input
	!
	!	10/16/99 - Kevin Handy
	!		Use COPY_COPYFILE instead of OPY_COPYRECORDS
	!		because it is faster and will work on Alpha.
	!
	!	08/23/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Map statements
	!
	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.HB"
	MAP (AR_OPEN_DIST)	AR_OPEN_DIST_CDD	AR_OPEN_DIST

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD	AR_CLOSED

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)		AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD	AR_CUSBAL
	DIM AR_CUSBAL_CDD ARRAY_CUSBAL(100%)

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	COM (CH_AR_CUSBAL) AR_CUSBAL.CH%
	COM (CH_AR_OPEN) AR_OPEN.CH%
	COM (CH_AR_CONTROL) AR_CONTROL.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION AR_FUNC_AGE
	EXTERNAL LONG		OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Declare variables
	!
	DECLARE INTEGER CONSTANT RECORD_ARRAY = 6000
	DECLARE RFA RECORD_RFA(RECORD_ARRAY), RECORD_RFA

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	!
	! Open AP open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.PST"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

310	!
	! Open AR open distribution file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.PST"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "AR_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

320	!
	! Open AR close file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.PST"
	USE
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

330	!
	! Open customer balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.PST"
	USE
		FILENAME$ = "AR_CUSBAL"
		CONTINUE HelpError
	END WHEN

340	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%
		CLOSE GL_PERIOD.CH%
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

350	!
	! Open Customer file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

360	!
	! Open AR Control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.MOD"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF AR_CONTROL::CLOSEFLAG = "2"
	THEN
		CALL HELP_34MESSAGE(SCOPE, "reset in process", "E", &
			SCOPE::PRG_PROGRAM, "", "AR_RESET")
		GOTO ExitProgram
	END IF


	IF AR_CONTROL::CLOSEFLAG = "3"
	THEN
		CALL HELP_34MESSAGE(SCOPE, "purge in process", "E", &
			SCOPE::PRG_PROGRAM, "", "AR_PURGE")
		GOTO ExitProgram
	END IF

	CUR_PERIOD% = AR_CONTROL::LASTPERCLOSE + 1%
	YEAR$ = AR_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	!
	! Note: YYYY_PP has underscore stored in it, YYYYPP doesn't.
	!
	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")
	YYYYPP$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")
	UPDATE_GL$ = YYYYPP$

	!
	! Calculate last day of the period.
	!
	AGE.DATE$ = LEFT(DATE_INVMCODE( &
		DATE_MONCODE(YYYYPP$) - GL_PERIOD::NEWYEAR + 1% &
		), 4%) + GL_PERIOD::ENDDATE(CUR_PERIOD%)

370	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.NEW"
	USE
		FILENAME$ = "AR_TEMP_OPEN"
		CONTINUE HelpError
	END WHEN

380 !	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.NEW"

390	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.NEW"
	USE
		FILENAME$ = "AR_TEMP_CUSBAL"
		CONTINUE HelpError
	END WHEN

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
		"Accounts Receivable Close for " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "CLOSING " + YYYY_PP$ + &
		" " + GL_PERIOD::PERIOD(CUR_PERIOD%), 2%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Age Date         " + PRNT_DATE(AGE.DATE$, 8%), 4%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Customer # ", 6%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "End Balance", 8%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Close Balance", &
		10%, 5%)

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
	!	^*Confirm Closing\*
	!	.b
	!	.lm +5
	!	The ^*Confirm Closing\* field prompts for user confirmation for closing
	!	the Accounts Receivable.  A ^*Y\* answer confirms the closing and an ^*N\*
	!	answer will cancel the closing procedure.
	!	.lm -5
	!
	! Index:
	!	.x Confirm Closing>Accounts Receivable Closing
	!	.x Accounts Receivable Closing>Confirm Closing
	!
	!--
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Confirm closing - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO AbortExit
 !		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	%PAGE

1000	!******************************************************************
	!
	!******************************************************************
	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

1100	!******************************************************************
	! Close Accounts receivable
	!******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Copying old closed data", 1%)

	TEST_CUSNUM$ = STRING$(10%, 0%)

1115	!
	! Set close flag in control file
	!
	WHEN ERROR IN
		GET #AR_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	AR_CONTROL::CLOSEFLAG = "1"

	WHEN ERROR IN
		UPDATE #AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

1117	!
	! Copy the current closed file over to the nre closed file
	!
	CLOSE AR_CLOSED.CH%
	AR_CLOSED_NEW.NAME$ = AR_CLOSED.NAME$ + "_NEW"
 !	STATUS% = COPY_COPYRECORDS(AR_CLOSED.NAME$, AR_CLOSED_NEW.NAME$)
	STATUS% = COPY_COPYFILE(AR_CLOSED.NAME$, AR_CLOSED_NEW.NAME$)
	IF (STATUS% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to create copy of closed file! " + &
			NUM1$(STATUS%), 0%)
 !		WHEN ERROR IN
 !			KILL AR_CLOSED_NEW.NAME$
 !		USE
 !			CONTINUE AbortExit
 !		END WHEN

		SMG_STATUS% = LIB$DELETE_FILE(AR_CLOSED_NEW.NAME$ + ";*")

		GOTO AbortExit
	END IF

1118	!*******************************************************************
	! Close old open file into new open file and new closed file
	!*******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.NEU"
	USE
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Closing open records", 1%)

1120	!
	! Choose the customer from either the customer file, or from
	! the register file (whichever is smaller at the moment)
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, KEY #0% GT TEST_CUSNUM$ &
			UNLESS AR_35CUSTOM.END%
	USE
		AR_35CUSTOM.END% = -1%
		CONTINUE 1120
	END WHEN

1125	WHEN ERROR IN
		GET #AR_OPEN.CH%, KEY #0% GT TEST_CUSNUM$ &
			UNLESS AR_OPEN.END%
	USE
		AR_OPEN.END% = -1%
		CONTINUE 1125
	END WHEN

	!
	! Quit if we have read both files
	!
	GOTO 2200 IF (AR_OPEN.END% <> 0%) AND (AR_35CUSTOM.END% <> 0%)

	IF ((AR_35CUSTOM::CUSNUM <= AR_OPEN::CUSNUM) AND &
		(AR_35CUSTOM.END% = 0%)) OR &
		(AR_OPEN.END%)
	THEN
		!
		! Normal case
		!
		TEST_CUSNUM$ = AR_35CUSTOM::CUSNUM + ""
		TEST_METHOD$ = AR_35CUSTOM::METHOD

		!
		! Age the customer.
		!
		GOSUB AgeCustomer

	ELSE
		!
		! Bad case - orphan record
		!
		TEST_CUSNUM$ = AR_OPEN::CUSNUM + ""
		TEST_METHOD$ = "O"

		CALL ENTR_3MESSAGE(SCOPE, "Orphan records: " + TEST_CUSNUM$, 1%)
	END IF

	GOSUB PlaceRecords

	!
	! Display information on the screen
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AR_OPEN::CUSNUM, &
		6%, 23%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(END_BAL_DUE, "##,###,###.##"), &
		8%, 23%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(CLOSE_BAL_DUE, "##,###,###.##"), &
		10%, 23%)

	GOTO 1120

	%PAGE

2200	!*******************************************************************
	! Complete Initail process
	!*******************************************************************

	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM = "BALANC"
	!++
	! Abstract:BALANC
	!
	!
	! Index:
	!
	!
	!--
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Are balances correct?  confirm - then press <Do> ", &
		"N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO AbortExit
	END IF

	%PAGE

2210	!*******************************************************************
	! Purge distribution file
	!*******************************************************************

	RESET #AR_OPEN_DIST.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Purging distribution records. . .", 1%)

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

2220	!
	! Main loop starts here
	!

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_OPEN_DIST.CH%
	USE
		CONTINUE 2300 IF ERR = 11%
		FILENAME$ = "AR_TEMP_CLOSE"
		CONTINUE HelpError
	END WHEN

2240	IF AR_OPEN::INVNUM <> AR_OPEN_DIST::INVNUM
	THEN
		AR_OPEN::CUSNUM = AR_OPEN_DIST::CUSNUM
		AR_OPEN::INVNUM = AR_OPEN_DIST::INVNUM

		!
		! Assume we will delete it
		!
		DELETE_FLAG% = -1%

		WHEN ERROR IN
			FIND #AR_OPEN.CH_NEW%, KEY #0% EQ AR_OPEN_DIST::CUSNUM + &
				AR_OPEN_DIST::INVNUM, REGARDLESS
		USE
			CONTINUE 2250
		END WHEN

		!
		! We won't after all since there are open invoices for
		! this.
		!
		DELETE_FLAG% = 0%
	END IF

2250	!
	! Check for an interrupt
	!
	GOSUB Interrupt

	!
	! Get distribution record
	!
	IF (DELETE_FLAG% <> 0%)
	THEN
		WHEN ERROR IN
			DELETE #AR_OPEN_DIST.CH%
		USE
			FILENAME$ = "AR_OPEN_DIST"
			CONTINUE HelpError
		END WHEN
	END IF

2290	GOTO 2220

2300	!*******************************************************************
	! Replace registers with new versions
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, &
		"Renaming open temp file to open file. . .", 1%)

	CLOSE AR_OPEN.CH%, AR_OPEN.CH_NEW%
	CLOSE AR_CUSBAL.CH%, AR_CUSBAL.CH_NEW%
	CLOSE AR_CLOSED.CH%, AR_CLOSED.CH_NEW%

 !	WHEN ERROR IN
 !		KILL AR_OPEN.NAME$ FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 2310
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AR_OPEN.NAME$ + ";*")

2310	NAME AR_OPEN.NAME_NEW$ AS AR_OPEN.NAME$

2320 !	WHEN ERROR IN
 !		KILL AR_CUSBAL.NAME$ FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 2330
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AR_CUSBAL.NAME$ + ";*")

2330	NAME AR_CUSBAL.NAME_NEW$ AS AR_CUSBAL.NAME$

2340 !	WHEN ERROR IN
 !		KILL AR_CLOSED.NAME$ FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 2350
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AR_CLOSED.NAME$ + ";*")

2350	NAME AR_CLOSED.NAME_NEW$ AS AR_CLOSED.NAME$

2390	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)


	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Closing completed", 0%)

2400	!
	! Update Control file
	!
	WHEN ERROR IN
		GET #AR_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	AR_CONTROL::CLOSEFLAG = "0"
	AR_CONTROL::LASTPERCLOSE = CUR_PERIOD%
	AR_CONTROL::YEAR = YEAR$

	WHEN ERROR IN
		UPDATE #AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram

	!*******************************************************************
	! Abort the close
	!*******************************************************************
 AbortExit:
	GET #AR_CONTROL.CH%, RECORD 1%

	AR_CONTROL::CLOSEFLAG = "0"

	UPDATE #AR_CONTROL.CH%

	CLOSE AR_OPEN.CH%, AR_CLOSED.CH%, AR_CONTROL.CH%, &
		AR_CLOSED.CH_NEW%, AR_OPEN.CH_NEW%

2500	!
	! Kill temp files
	!
 !	WHEN ERROR IN
 !		KILL AR_OPEN.NAME_NEW$ FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 2510
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AR_OPEN.NAME_NEW$ + ";*")

2510 !	WHEN ERROR IN
 !		KILL AR_CLOSED.NAME_NEW$ FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 2520
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AR_CLOSED.NAME_NEW$ + ";*")

2520 !	WHEN ERROR IN
 !		KILL AR_CUSBAL.NAME_NEW$ FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 2530
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AR_CUSBAL.NAME_NEW$ + ";*")

2530	!
	! Exit program
	!
 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 AgeCustomer:
4000	!*******************************************************************
	! Age one customer
	!*******************************************************************

	!
	! Age the customer
	!
	IF AR_FUNC_AGE(AR_35CUSTOM::CUSNUM, AR_35CUSTOM::METHOD, &
		AGE.DATE$, UPDATE_GL$, NUM_ACCT%, ARRAY_CUSBAL())
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to age " + AR_35CUSTOM::CUSNUM, 0%)
		RETURN
	END IF

4010	!
	! If this is an open item customer, keep only those records
	! that have a credit limit attached to them.
	!
	IF AR_35CUSTOM::METHOD <> "B"
	THEN
		FOR LOOP_AGE% = 1% TO NUM_ACCT%

			IF ARRAY_CUSBAL(LOOP_AGE%)::CREDIT <> 0.0
			THEN
				AR_CUSBAL = ARRAY_CUSBAL(LOOP_AGE%)
				WHEN ERROR IN
					PUT #AR_CUSBAL.CH_NEW%
				USE
					CONTINUE 4090
				END WHEN
			END IF

		NEXT LOOP_AGE%

		!
		! Skip work for balance foreward when done with this
		!
		GOTO 4090
	END IF

4050	!
	! Handle balance foreward customers.  Don't write them out
	! unless there is a non-zero amount somewhere.
	!
	FOR LOOP_AGE% = 1% TO NUM_ACCT%

		AR_CUSBAL = ARRAY_CUSBAL(LOOP_AGE%)

		!
		! Check for a non-zero amount somewhere
		!
		GOTO 4060 IF AR_CUSBAL::CREDIT <> 0.0
		GOTO 4060 IF AR_CUSBAL::AGING(LOOP_AGE1%) <> 0.0 &
			FOR LOOP_AGE1% = 0% TO 4%
		GOTO 4060 IF (AR_CUSBAL::CHARGE <> 0.0) OR &
			(AR_CUSBAL::YTDSERVICE <> 0.0) OR &
			(AR_CUSBAL::LAST_PAID <> 0.0) OR &
			(AR_CUSBAL::YTDSALES <> 0.0)
		GOTO 4070

4060		!
		! Write out the record
		!
		WHEN ERROR IN
			PUT #AR_CUSBAL.CH_NEW%
		USE
			FILENAME$ = "AR_TEMP_CUSBAL"
			CONTINUE HelpError
		END WHEN

4070	NEXT LOOP_AGE%

4090	RETURN

	%PAGE

 PlaceRecords:
18000	!*******************************************************************
	! Main loop starts here
	!*******************************************************************

	CURRENT_FLAG% = 0%
	RECORD_LOOP% = 0%
	INVNUM$ = ""
	BAL_DUE = 0.0

	WHEN ERROR IN
		FIND #AR_OPEN.CH%, KEY #0% GE TEST_CUSNUM$
	USE
		CONTINUE 18090 IF ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

18020	!
	! Check for an interrupt
	!
	GOSUB Interrupt

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_OPEN.CH%
	USE
		CONTINUE 18090 IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

18040	!
	! Done with this customer?
	!
	IF TEST_CUSNUM$ <> AR_OPEN::CUSNUM
	THEN
		GOTO 18090
	END IF

	!
	! Handle balance forward differently than open item
	!
	IF TEST_METHOD$ = "B"
	THEN
		IF AR_OPEN::UPDATED <= UPDATE_GL$
		THEN
			AR_CLOSED::CUSNUM	= AR_OPEN::CUSNUM
			AR_CLOSED::INVNUM	= AR_OPEN::INVNUM
			AR_CLOSED::TRATYP	= AR_OPEN::TRATYP
			AR_CLOSED::TRADAT	= AR_OPEN::TRADAT
			AR_CLOSED::SALAMT	= AR_OPEN::SALAMT
			AR_CLOSED::DISAMT	= AR_OPEN::DISAMT
			AR_CLOSED::OTHCHG	= AR_OPEN::OTHCHG
			AR_CLOSED::RECNUM	= AR_OPEN::RECNUM
			AR_CLOSED::CHKNUM	= AR_OPEN::CHKNUM
			AR_CLOSED::ARACCT	= AR_OPEN::ARACCT
			AR_CLOSED::SUBACC	= AR_OPEN::SUBACC
			AR_CLOSED::SALNUM	= AR_OPEN::SALNUM
			AR_CLOSED::DESCR	= AR_OPEN::DESCR
			AR_CLOSED::BATCH	= AR_OPEN::BATCH
			AR_CLOSED::UPDATED	= AR_OPEN::UPDATED
			AR_CLOSED::CLOSEDATE	= YYYYPP$
			AR_CLOSED::DUEDATE	= AR_OPEN::DUEDATE
			AR_CLOSED::DISCOUNTDATE	= AR_OPEN::DISCOUNTDATE

			PUT #AR_CLOSED.CH_NEW%
		ELSE
			PUT #AR_OPEN.CH_NEW%
		END IF

		GOTO 18020
	END IF

18050	!
	! Must be working on an open item person if we reach here.
	!

	!
	! Done with this invoice?
	!
	IF AR_OPEN::INVNUM <> INVNUM$
	THEN
		RECORD_RFA = GETRFA(AR_OPEN.CH%)

		GOSUB PutRecord

		GET #AR_OPEN.CH%, RFA RECORD_RFA

		RECORD_LOOP% = 0%
		BAL_DUE = 0.0
		CURRENT_FLAG% = 0%

		INVNUM$ = AR_OPEN::INVNUM + ""
	END IF

	IF AR_OPEN::TRADAT > AGE.DATE$
	THEN
		CURRENT_FLAG% = -1%
	END IF

	IF LEFT(AR_OPEN::UPDATED, 6%) > UPDATE_GL$
	THEN
		CURRENT_FLAG% = -1%
	END IF

	BAL_DUE = FUNC_ROUND(BAL_DUE + AR_OPEN::SALAMT, 2%) &
		IF AR_OPEN::TRATYP <> "02"

	END_BAL_DUE = FUNC_ROUND(END_BAL_DUE + AR_OPEN::SALAMT, 2%) &
		IF AR_OPEN::TRATYP <> "02"

	CLOSE_BAL_DUE = FUNC_ROUND(CLOSE_BAL_DUE + AR_OPEN::SALAMT, 2%) &
		IF AR_OPEN::TRATYP <> "02" &
			IF UPDATE_GL$ >= LEFT(AR_OPEN::UPDATED, 6%)

	RECORD_LOOP% = RECORD_LOOP% + 1%
	RECORD_RFA(RECORD_LOOP%) = GETRFA(AR_OPEN.CH%)

	GOTO 18020

18090	!
	! Make sure customer has been written out completely
	!
	GOSUB PutRecord

	RETURN

	%Page

 PutRecord:
18100	!*******************************************************************
	! Put records in temporary file
	!*******************************************************************

	!
	! Skip if no records in list
	!
	GOTO 18190 IF RECORD_LOOP% = 0%

	!
	! Transfer to temp_closed if the balance is zero (and there
	! is nothing in the future column), or if
	! this is a balance foreward customer.
	!
	GOTO 18120 IF (TEST_METHOD$ = "B")
	GOTO 18120 IF (FUNC_ROUND(BAL_DUE, 2%) = 0.0) AND (CURRENT_FLAG% = 0%)

18110	FOR LOOP% = 1% TO RECORD_LOOP%

		WHEN ERROR IN
			GET #AR_OPEN.CH%, RFA RECORD_RFA(LOOP%)
		USE
			FILENAME$ = "AR_OPEN"
			CONTINUE HelpError
		END WHEN

18115		PUT #AR_OPEN.CH_NEW%

	NEXT LOOP%

	GOTO 18190

18120	!
	! Skip storing history if the retention period is 0
	!
	FOR LOOP% = 1% TO RECORD_LOOP%

		WHEN ERROR IN
			GET #AR_OPEN.CH%, RFA RECORD_RFA(LOOP%)
		USE
			FILENAME$ = "AR_OPEN"
			CONTINUE HelpError
		END WHEN

		IF AR_CONTROL::RETAIN <= 0%
		THEN
			GOTO 18130
		END IF

		AR_CLOSED::CUSNUM	= AR_OPEN::CUSNUM
		AR_CLOSED::INVNUM	= AR_OPEN::INVNUM
		AR_CLOSED::TRATYP	= AR_OPEN::TRATYP
		AR_CLOSED::TRADAT	= AR_OPEN::TRADAT
		AR_CLOSED::SALAMT	= AR_OPEN::SALAMT
		AR_CLOSED::DISAMT	= AR_OPEN::DISAMT
		AR_CLOSED::OTHCHG	= AR_OPEN::OTHCHG
		AR_CLOSED::RECNUM	= AR_OPEN::RECNUM
		AR_CLOSED::CHKNUM	= AR_OPEN::CHKNUM
		AR_CLOSED::ARACCT	= AR_OPEN::ARACCT
		AR_CLOSED::SUBACC	= AR_OPEN::SUBACC
		AR_CLOSED::SALNUM	= AR_OPEN::SALNUM
		AR_CLOSED::DESCR	= AR_OPEN::DESCR
		AR_CLOSED::BATCH	= AR_OPEN::BATCH
		AR_CLOSED::UPDATED	= AR_OPEN::UPDATED
		AR_CLOSED::CLOSEDATE	= YYYYPP$
		AR_CLOSED::DUEDATE	= AR_OPEN::DUEDATE
		AR_CLOSED::DISCOUNTDATE	= AR_OPEN::DISCOUNTDATE

18125		PUT #AR_CLOSED.CH_NEW%

18130	NEXT LOOP%

18190	RETURN

 Interrupt:
	!********************************************************************
	! Check for an interrupt
	!********************************************************************

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

32767	END
