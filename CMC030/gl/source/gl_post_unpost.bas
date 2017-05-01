1	%TITLE "Unpost a Batch from GL"
	%SBTTL "GL_POST_UNPOST"
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
	! ID:GLUNPS
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Unpost a Batch from G[eneral] L[edger]\* option in the Special Menu provides
	!	the means to unpost or delete a specified system assigned batch from a
	!	designated period in the General Ledger.
	!	.b
	!	This option will search for the batch in the General Ledger for the period
	!	specified. If the period has been closed, the execution of this process will
	!	end.  If the period has not been closed, all records in the Ledger which have
	!	the specific batch number will be deleted.
	!	.b
	!	^*CAUTION:\*  Do not utilize this option unless the intent is to absolutely
	!	delete the records from the file, with restoration unintended.  If records are
	!	to be moved from one General Ledger period to another, see the Move Batch
	!	Between Periods option.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_POST_UNPOST/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_POST_UNPOST, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_POST_UNPOST.OBJ;*
	!
	! Author:
	!
	!	07/19/89 - Aaron Redd
	!
	! Modification history:
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error handler,
	!		changed to = "GL_" + YYYY_PP$.
	!
	!	03/12/92 - Kevin Handy
	!		Unwrapped error trapping
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/29/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico standards.
	!		Added call to OUTP_FINISH, which was missing.
	!
	!	06/21/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	06/21/95 - Kevin Handy
	!		Modifications to update the chart of accounts
	!		running totals.
	!
	!	12/15/95 - Kevin Handy
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include special CMC information
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"

	!
	! Memory MAPs
	!
	MAP	(GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD
	MAP	(GL_YYYY_PP)		GL_YYYY_PP_CDD	GL_YYYY_PP
	MAP	(GL_YYYY_PP_SEQ)	GL_YYYY_PP_CDD	GL_YYYY_PP_SEQ
	MAP	(GL_CHART)		GL_CHART_CDD	GL_CHART

	RECORD GL_SUMMARY_CDD

		STRING ACCOUNT = 18%
		REAL AMOUNT
		REAL UNITS
		REAL HOURS

	END RECORD

	DECLARE INTEGER CONSTANT MAX_SUMMARY = 600%

	DIM GL_SUMMARY_CDD GL_SUMMARY(MAX_SUMMARY)

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED

	!
	! Common memory areas
	!
	COM (GL_POST_UNPOST.CH) &
		GL_YYYY_PP.CH%, &
		GL_YYYY_PP.SEQ%

	!
	! Declare internal variables
	!
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE	REAL			RUNTOT
	DECLARE	STRING			GLPERIOD
	DECLARE	STRING			PROCESS_BATCH
	DECLARE	STRING			TITLE(10%)

	%PAGE

	!**************************************************************
	! Get some stuff done before we start
	!**************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Set flags
	!
	SCOPE::PRG_PROGRAM = "GL_POST_UNPOST"
	SCOPE::PRG_IDENT = "H"
	INTR_STATUS = CMC$_NORMAL
	LIN% = 999%
	GL_SUMMARY% = 0%

	%PAGE

	!**************************************************************
	! Process `from user' input
	!**************************************************************

	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	!
	! Get the user input into a usable form
	!

	GLPERIOD = TRM$(UTL_REPORTX::OPTDEF(0%))

	!++
	! Abstract:FLD01
	!	^*(01) GL Period\*
	!	.b
	!	.lm +5
	!	^*GL Period\* refers to the time period in which the General Ledger is used.
	!	In this instance for use, it must correspond with the ^*Batch\*.
	!	.b
	!	The format for entry YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

	BATCH_NO$ = TRM$(UTL_REPORTX::OPTDEF(1%))
	YYYY_PP$ = LEFT(GLPERIOD, 4%) + "_" + TRM$(RIGHT(GLPERIOD, 5%))

	!++
	! Abstract:FLD02
	!	^*(02) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* refers to the process batch number
	!	which was assigned by the system in the original posting
	!	process.
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Title
	!
	TITLE(1%) = "PROCESS  TO  UNPOST  BATCH  " + BATCH_NO$ + &
		"  FROM  " + YYYY_PP$ + "  LEDGER"
	TITLE(2%) = "General Ledger System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	%PAGE

	!*******************************************************************
	! See if we are restarting an interrupted process
	!*******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_RESTART, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP$ + ".LED", &
		BATCH_NO$, GLPERIOD, "")

	GOTO AbortProcess IF (EXIT_STATUS <> CMC$_NORMAL) AND &
		(EXIT_STATUS <> CMC$_WARNING)

	%PAGE

	!*******************************************************************
	! Open the GL Period Definition file and check the GL Period
	!*******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_ASSIGN, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP$ + ".LED", &
		BATCH_NO$, GLPERIOD, "")

	GOTO AbortProcess IF (EXIT_STATUS AND 1%) = 0%

	!
	! Open GL_PERIOD file
	!
100	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)

	CLOSE_YYYY_PP$ = GL_PERIOD::YEAR + "_" + &
		FORMAT$(GL_PERIOD::LASTPERCLO, "<0>#")

	!
	! Check format, year, and period
	!
	IF (LEN(YYYY_PP$) <> 7%)
	THEN
		TEXT$ = SPACE$(18%) + "GL period " + GLPERIOD + &
			" must have the following format:  YYYYPP"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

	IF (RIGHT(YYYY_PP$, 6%) > FORMAT$(GL_PERIOD::FPFY, "<0>#")) OR &
		(RIGHT(YYYY_PP$, 6%) < "01")
	THEN
		TEXT$ = SPACE$(18%) + "GL period '" + YYYY_PP$ + "' is invalid"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

	IF LEN(XLATE(LEFT(YYYY_PP$, 4%), STRING$(48%, 0%) + "0123456789")) <> 4%
	THEN
		TEXT$ = SPACE$(18%) + "The year " + LEFT(YYYY_PP$, 4%) + &
			" must have the following format:  YYYY"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

	IF YYYY_PP$ <= CLOSE_YYYY_PP$
	THEN
		TEXT$ = SPACE$(18%) + "GL period " + YYYY_PP$ + " has been closed"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

110	!
	! Open chart of accounts
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.MOD"

	%PAGE

	!******************************************************************
	! Open the Period Ledger, and the temporary Ledger file
	!******************************************************************
	!
	! Set record counter as a flag for finding period ledger
	!
	RECORDS% = -2%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP$ + ".LED", &
		BATCH_NO$, GLPERIOD, "")

	GOTO AbortProcess IF (EXIT_STATUS AND 1%) = 0%

	!
	! Open GL ledger file
	!
200	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.UPD"
	USE
		!
		! Cannot lock file
		!
		IF ERR = 138%
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE AbortProcess IF (ERR = 5%)
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP$ + ".LED", &
		BATCH_NO$, GLPERIOD, "")

	GOTO AbortProcess IF (EXIT_STATUS AND 1%) = 0%

300	CALL ASSG_CHANNEL(GL_YYYY_PP.SEQ%, STAT%)
	WHEN ERROR IN
		OPEN "GL_YYYY_PP.SEQ" AS FILE GL_YYYY_PP.SEQ%, &
			ORGANIZATION SEQUENTIAL FIXED, &
			MAP GL_YYYY_PP_SEQ, &
			TEMPORARY, &
			ALLOW NONE, &
			ACCESS MODIFY
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	%PAGE

	!******************************************************************
	! Remove the Batch number from the Ledger file
	!******************************************************************
	!
	! Reset record counter for finding the batch number
	!
	RECORDS% = -1%

	!
	! FIND the first record with this batch number
	!
400	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, KEY #4% EQ BATCH_NO$
	USE
		!
		! Locked Block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE AbortProcess IF (ERR = 155%)
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
	! Reset record counter to zero
	!
	RECORDS% = 0%

	!
	! Check unsolicited input
	!
500	IF RRR_FLAG%
	THEN
		IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
		THEN
			INTR_STATUS = CMC$_UNTERROR
			GOTO InterruptProcess
		END IF
	END IF

	!
	! GET the (next) Ledger record
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%
	USE
		!
		! Locked Block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 600 IF (ERR = 11%)
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
	! Make sure it has the same Batch number
	!
	GOTO 600 IF (GL_YYYY_PP::BTHNUM <> BATCH_NO$)

	!
	! PUT the record into the temp file, and delete it from the Ledger
	!
	GL_YYYY_PP_SEQ = GL_YYYY_PP
	WHEN ERROR IN
		PUT #GL_YYYY_PP.SEQ%
		DELETE #GL_YYYY_PP.CH%
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
	! Try to summarise information
	!
	FOR I% = 1% TO GL_SUMMARY%

		GOTO 520 IF GL_SUMMARY(I%)::ACCOUNT = GL_YYYY_PP::ACCT

	NEXT I%

	!
	! Too many to add another?
	!
	GOTO 530 IF GL_SUMMARY% = MAX_SUMMARY

	GL_SUMMARY%, I% = GL_SUMMARY% + 1%
	GL_SUMMARY(GL_SUMMARY%)::ACCOUNT = GL_YYYY_PP::ACCT
	GL_SUMMARY(GL_SUMMARY%)::AMOUNT = 0.0
	GL_SUMMARY(GL_SUMMARY%)::HOURS = 0.0
	GL_SUMMARY(GL_SUMMARY%)::UNITS = 0.0

520	!
	! Add amounts to summary
	!
	GL_SUMMARY(I%)::AMOUNT = &
		FUNC_ROUND(GL_SUMMARY(I%)::AMOUNT + &
		GL_YYYY_PP::AMOUNT, 2%)
	GL_SUMMARY(I%)::HOURS = &
		GL_SUMMARY(I%)::HOURS + &
		GL_YYYY_PP::HOURS
	GL_SUMMARY(I%)::UNITS = &
		GL_SUMMARY(I%)::UNITS + &
		GL_YYYY_PP::UNITS

530	!
	! Add to the record counter, and go up to GET the next record
	!
	RECORDS% = RECORDS% + 1%
	GOTO 500

	!
	! One way or another, we're done with the Ledger
	!
600	CLOSE #GL_YYYY_PP.CH%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP$ + ".LED", &
		BATCH_NO$, GLPERIOD, "")

	GOTO AbortProcess IF (EXIT_STATUS AND 1%) = 0%

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + " Deleted Records"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	!
	! Remove amounts from GL running totals using the summary that
	! was created during the deletion of the records.
	!
	TEXT$ = "GL_CHART"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	FOR I% = 1% TO GL_SUMMARY%

620		WHEN ERROR IN
			GET #GL_CHART.CH%, &
				KEY #0% EQ GL_SUMMARY(I%)::ACCOUNT
		USE
			!
			! Locked Block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 640
		END WHEN

		GL_CHART::RUNDOL = FUNC_ROUND(GL_CHART::RUNDOL - &
			GL_SUMMARY(I%)::AMOUNT, 2%)
		GL_CHART::RUNUNIT = GL_CHART::RUNUNIT - &
			GL_SUMMARY(I%)::UNITS
		GL_CHART::RUNHOUR = GL_CHART::RUNHOUR - &
			GL_SUMMARY(I%)::HOURS

630		UPDATE #GL_CHART.CH%

640	NEXT I%

	TEXT$ = SPACE$(9%) + FORMAT$(GL_SUMMARY%, "########") + " Updated Records"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP$ + ".LED", &
		BATCH_NO$, GLPERIOD, "")

	GOTO AbortProcess IF (EXIT_STATUS AND 1%) = 0%

	%PAGE

 PrintReport:
	!******************************************************************
	! Print out a report of the records (from the temp file)
	!******************************************************************
	!
	! Set the Header
	!
	TITLE(4%) = "Account            SubAccount RefNumber        " + &
		"Xref       Check  TransDate Source TranKey Description" + &
		"                    EntryAmount"
	TITLE(5%) = "."
	INTR_STATUS = CMC$_NORMAL

	!
	! Go back to the beginning of the temp file
	!
700	WHEN ERROR IN
		RESET #GL_YYYY_PP.SEQ%
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
	! Check unsolicited input
	!
800	IF RRR_FLAG%
	THEN
		IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
		THEN
			INTR_STATUS = CMC$_UNTERROR
			GOTO InterruptProcess
		END IF
	END IF

	!
	! Get the first (next) record
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.SEQ%
	USE
		CONTINUE 900 IF (ERR = 11%)
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
	! Print out the GL_YYYY_PP_SEQ record
	!
	TEXT$ = GL_YYYY_PP_SEQ::ACCT + " " + &
		GL_YYYY_PP_SEQ::SUBACC + " " + &
		GL_YYYY_PP_SEQ::REFNO + " " + &
		GL_YYYY_PP_SEQ::XREFNO + " " + &
		GL_YYYY_PP_SEQ::CKNO + " " + &
		PRNT_DATE(GL_YYYY_PP_SEQ::TRANDAT, OPT_MMDDYY) + "  " + &
		GL_YYYY_PP_SEQ::SOURCE + "   " + &
		GL_YYYY_PP_SEQ::TRANKEY + "  " + &
		LEFT(GL_YYYY_PP_SEQ::DESCR, 27%) + &
		FORMAT$(GL_YYYY_PP_SEQ::AMOUNT, "  ##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, LIN%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Add to running total
	!
	RUNTOT = RUNTOT + GL_YYYY_PP_SEQ::AMOUNT

	!
	! Go to get the next record
	!
	LIN% = 0%
	GOTO 800

	!
	! Print out a blank line
	!
900	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print out the Running Total
	!
	TEXT$ = SPACE$(103%) + "Running Total:" + FORMAT$(RUNTOT, "  ##,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
	GOTO ExitProgram

	%PAGE

 AbortProcess:
	!
	! Print an error line if the period file did not exist
	!
	IF RECORDS% = -2%
	THEN
		TEXT$ = STRING$(18%, A"."B) + "Ledger " + YYYY_PP$ + &
			" does not exist"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
	END IF

	!
	! Print an error line if batch number was not found
	!
	IF RECORDS% = -1%
	THEN
		TEXT$ = STRING$(18%, A"."B) + "Batch number not in file"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
	END IF

	EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP$ + ".LED", &
		BATCH_NO$, GLPERIOD, "")

 InterruptProcess:
	IF INTR_STATUS <> CMC$_NORMAL
	THEN
		!
		! If some records were deleted, then print out number
		! (then go to print out the report, and finally go back to MENU)
		!
		IF (RECORDS% > 0%)
		THEN
			TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
				" Deleted Records"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		END IF

		EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, PROCESS_BATCH, &
			TITLE(), UTL_REPORTX, &
			"GL_" + YYYY_PP$ + ".LED", &
			BATCH_NO$, GLPERIOD, "")

		GOTO PrintReport IF (LIN% = 999%)
	END IF

	%PAGE

 ExitProgram:
	!
	! Close the temporary sequential file (thereby killing it)
	!
	CLOSE #GL_YYYY_PP.SEQ%

	!
	! Close down report
	!
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO AbortProcess

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of program GL_POST_UNPOST
	!******************************************************************
	END
