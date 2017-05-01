1	%TITLE "Reverse a Batch in the GL Ledger"
	%SBTTL "GL_SPEC_REVBATCH"
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
	! ID:GLREVB
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program, after given a GL Period and
	!	a Post batch number, will search for the ledger from the
	!	specified period, determine whether or not it has been
	!	closed (the program will end if the period is closed),
	!	and reverse the amounts in all records in
	!	that period ledger which have the specified batch number.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_REVBATCH/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_SPEC_REVBATCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_REVBATCH.OBJ;*
	!
	! Author:
	!
	!	07/24/89 - Aaron Redd
	!
	! Modification history:
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error handler,
	!		changed to = "GL_" + YYYY_PP$.
	!
	!	03/12/92 - Kevin Handy
	!		Unrolled error trapping (check)
	!
	!	03/30/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/29/95 - Kevin Handy
	!		(V3.6)
	!		Added call to OUTP_FINISH, which was missing.
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/29/2000 - Kevin Handy
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

	!
	! Memory MAPs
	!
	MAP	(DP_OUTP_XUNSOL)			RRR_FLAG%
	MAP	(GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD
	MAP	(GL_YYYY_PP)		GL_YYYY_PP_CDD	GL_YYYY_PP
	MAP	(GL_YYYY_PP_SEQ)	GL_YYYY_PP_CDD	GL_YYYY_PP_SEQ

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED
	EXTERNAL LONG			OUTP_XUNSOL
						! (It's really an AST routine)

	!
	! Common memory areas
	!
	COM (GL_SPEC_REVBATCH.CH) &
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

	INTR_STATUS = CMC$_NORMAL
	LIN% = 999%

	%PAGE

	!**************************************************************
	! Process `from user' input
	!**************************************************************

	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GLPERIOD = TRM$(UTL_REPORTX::OPTDEF(0%))

	!++
	! Abstract:FLD01
	!
	! Datatype:TEXT
	! Size:6
	! Required:Y
	!--
	BATCH_NO$ = TRM$(UTL_REPORTX::OPTDEF(1%))

	!++
	! Abstract:FLD02
	!
	! Datatype:TEXT
	! Size:6
	! Required:Y
	!--

	YYYY_PP$ = LEFT(GLPERIOD, 4%) + "_" + TRM$(RIGHT(GLPERIOD, 5%))

	TITLE(1%) = "PROCESS  TO  REVERSE  BATCH  " + BATCH_NO$ + &
		"  IN  " + YYYY_PP$ + "  LEDGER"
	TITLE(2%) = "General Ledger System"
	TITLE(3%) = ""

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
		TEXT$ = SPACE$(18%) + "GL period " + YYYY_PP$ + &
			" has been closed"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

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
		IF ERR = 138% OR ERR = 154%	! Locked Block
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
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
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
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
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
	! Reverse the Amount, Hours, and Units
	!
	GL_YYYY_PP::AMOUNT = -GL_YYYY_PP::AMOUNT
	GL_YYYY_PP::HOURS = -GL_YYYY_PP::HOURS
	GL_YYYY_PP::UNITS = -GL_YYYY_PP::UNITS

	!
	! PUT the record into the temp file, and delete it from the Ledger
	!
	WHEN ERROR IN
		GL_YYYY_PP_SEQ = GL_YYYY_PP
		PUT #GL_YYYY_PP.SEQ%
		UPDATE #GL_YYYY_PP.CH%
	USE
		CONTINUE 600 IF (ERR = 11%)
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
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

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + " Updated Records"
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
	TITLE(4%) = STRING$(96%, A"="B) + &
		"Amounts are shown as AFTER reversing"
	TITLE(5%) = "Account            SubAccount RefNumber        " + &
		"Xref       Check  TransDate Source TranKey Description" + &
		"                    EntryAmount"
	TITLE(6%) = "."
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
				" Updated Records"
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
	! End report
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

	%PAGE

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

	%PAGE

32767	!******************************************************************
	! End of program GL_SPEC_REVBATCH
	!******************************************************************
	END
