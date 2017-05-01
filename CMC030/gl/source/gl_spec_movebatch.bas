1	%TITLE "Move a Batch Between Periods"
	%SBTTL "GL_SPEC_MOVEBATCH"
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
	! ID:GLMVBT
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Move a Batch Between Periods\* option in the Special Menu
	!	moves all records with a designated system assigned batch number from
	!	one General Ledger period file to another.
	!	.b
	!	Note:
	!	.br
	!	.lm +5
	!	1) The execution of this option will not affect any system other than the
	!	General Ledger.
	!	.br
	!	2) The General Ledger will need to be resynchronized after this option is
	!	executed.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_MOVEBATCH/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_SPEC_MOVEBATCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_MOVEBATCH.OBJ;*
	!
	! Author:
	!
	!	07/24/89 - Aaron Redd
	!
	! Modification history:
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
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	04/01/2003 - Kevin Handy
	!		Open using "MOD" instead of "UPD", so that it has
	!		less strict access requirements, since postings use
	!		the less strich ones anyway.
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
	COM	(GL_SPEC_MOVEBATCH.CH) &
		GL_YYYY_PP_FROM.CH%, &
		GL_YYYY_PP.SEQ%, &
		GL_YYYY_PP.CH%, &
		GL_PERIOD.CH%

	!
	! Declare internal variables
	!
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE	REAL			RUNTOT
	DECLARE	STRING			FROMPERIOD
	DECLARE	STRING			TOPERIOD
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

	!**************************************************************
	! Process `from user' input
	!**************************************************************

	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	FROMPERIOD = TRM$(UTL_REPORTX::OPTDEF(0%))

	!++
	! Abstract:FLD01
	!	^*(01) From Period\*
	!	.b
	!	.lm +5
	!	The ^*From Period\* field enters the period from which
	!	the records will be taken for replacement in a different ledger.
	!	.lm -5
	!
	! Index:
	!	.x From Period
	!	.x Period>From
	!
	!--
	TOPERIOD = TRM$(UTL_REPORTX::OPTDEF(1%))

	!++
	! Abstract:FLD02
	!	^*(02) To Period\*
	!	.b
	!	.lm +5
	!	The ^*To Period\* field enters the period to which the
	!	records will be transferred.
	!	.lm -5
	!
	! Index:
	!	.x To Period
	!
	!--
	BATCH_NO$ = TRM$(UTL_REPORTX::OPTDEF(2%))

	!++
	! Abstract:FLD03
	!	^*(03) Batch\*
	!	.b
	!	.lm +5
	!	The ^*Batch\* field specifies the batch number of the records
	!	which will be transferred.
	!	.lm -5
	!
	! Index:
	!	.x Batch
	!
	!--

	YYYY_PP1$ = LEFT(FROMPERIOD, 4%) + "_" + TRM$(RIGHT(FROMPERIOD, 5%))
	YYYY_PP2$ = LEFT(TOPERIOD, 4%) + "_" + TRM$(RIGHT(TOPERIOD, 5%))

	TITLE(1%) = "PROCESS  TO  MOVE  BATCH  " + BATCH_NO$ + &
		"  FROM  " + YYYY_PP1$ + "  TO  " + YYYY_PP2$
	TITLE(2%) = "General Ledger System"
	TITLE(3%) = ""

	TITLE(4%) = "."

	%PAGE

	!*******************************************************************
	! See if we are restarting an interrupted process
	!*******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_RESTART, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP1$ + ".LED", &
		BATCH_NO$, FROMPERIOD, TOPERIOD)

	GOTO AbortProcess &
		IF (EXIT_STATUS <> CMC$_NORMAL) AND &
		(EXIT_STATUS <> CMC$_WARNING)

	!*******************************************************************
	! Open the GL Period Definition file and check the GL Period
	!*******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_ASSIGN, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP1$ + ".LED", &
		BATCH_NO$, FROMPERIOD, TOPERIOD)

	GOTO AbortProcess &
		IF (EXIT_STATUS AND 1%) = 0%

	!
	! Open GL_PERIOD file
	!
100	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
		CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	CLOSE_YYYY_PP$ = GL_PERIOD::YEAR + "_" + &
		FORMAT$(GL_PERIOD::LASTPERCLO, "<0>#")

	!
	! Check format, year, and period of the FROM GL Period
	!
	IF (LEN(YYYY_PP1$) <> 7%)
	THEN
		TEXT$ = SPACE$(18%) + "GL period " + FROMPERIOD + &
			" must have the following format:  YYYYPP"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

	IF (RIGHT(YYYY_PP1$, 6%) > FORMAT$(GL_PERIOD::FPFY, "<0>#")) OR &
		(RIGHT(YYYY_PP1$, 6%) < "01")
	THEN
		TEXT$ = SPACE$(18%) + "GL period '" + &
			FROMPERIOD + "' is invalid"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

	IF LEN(XLATE(LEFT(YYYY_PP1$, 4%), &
		STRING$(48%, 0%) + "0123456789")) <> 4%
	THEN
		TEXT$ = SPACE$(18%) + "The year " + LEFT(YYYY_PP1$, 4%) + &
			" must have the following format:  YYYY"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

	!
	! Has the FROM period been closed?
	!
	IF YYYY_PP1$ <= CLOSE_YYYY_PP$
	THEN
		TEXT$ = SPACE$(18%) + "GL period " + YYYY_PP1$ + &
			" has been closed"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

	!
	! Check format, year, and period of the TO GL Period
	!
	IF (LEN(YYYY_PP2$) <> 7%)
	THEN
		TEXT$ = SPACE$(18%) + "GL period " + TOPERIOD + &
			" must have the following format:  YYYYPP"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

	IF (RIGHT(YYYY_PP2$, 6%) > FORMAT$(GL_PERIOD::FPFY, "<0>#")) OR &
		(RIGHT(YYYY_PP2$, 6%) < "01")
	THEN
		TEXT$ = SPACE$(18%) + "GL period '" + TOPERIOD + "' is invalid"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

	IF LEN(XLATE(LEFT(YYYY_PP2$, 4%), &
		STRING$(48%, 0%) + "0123456789")) <> 4%
	THEN
		TEXT$ = SPACE$(18%) + "The year " + LEFT(YYYY_PP2$, 4%) + &
			" must have the following format:  YYYY"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

	!
	! Has the TO period been closed?
	!
	IF YYYY_PP2$ <= CLOSE_YYYY_PP$
	THEN
		TEXT$ = SPACE$(18%) + "GL period " + YYYY_PP2$ + &
			" has been closed"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

	!
	! To go on would really be stupid if the "from" and "to" were the same
	!
	IF YYYY_PP2$ = YYYY_PP1$
	THEN
		TEXT$ = SPACE$(18%) + "The 'from' period and 'to' period" + &
			" are the same:  " + YYYY_PP1$
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_UNTERROR
		GOTO AbortProcess
	END IF

	%PAGE

	!******************************************************************
	! Open the Period Ledger, and the temporary Ledger file
	!******************************************************************
	!
	! Set record counter as a flag for finding the "from" period ledger
	!
	RECORDS% = -2%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP1$ + ".LED", &
		BATCH_NO$, FROMPERIOD, TOPERIOD)

	GOTO AbortProcess IF (EXIT_STATUS AND 1%) = 0%

	!
	! Set the YYYY_PP$ variable, and open the "from" GL ledger file
	!
200	YYYY_PP$ = YYYY_PP1$
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.MOD"
	USE
		IF ERR = 138%
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE AbortProcess IF (ERR = 5%)
		FILENAME$ = "GL_YYYY_PP - " + YYYY_PP1$
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP1$ + ".LED", &
		BATCH_NO$, FROMPERIOD, TOPERIOD)

	GOTO AbortProcess IF (EXIT_STATUS AND 1%) = 0%

	!
	! Store the channel number for the "from" ledger
	!
	GL_YYYY_PP_FROM.CH% = GL_YYYY_PP.CH%
	GL_YYYY_PP.CH% = 0%

	!
	! Reset the YYYY_PP$ variable, and open the "to" GL ledger file
	!
300	YYYY_PP$ = YYYY_PP2$
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.CRE"
 !		CLOSE #GL_YYYY_PP.CH%
 !		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.MOD"
	USE
		IF ERR = 138%
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "GL_YYYY_PP - " + YYYY_PP2$
		CONTINUE HelpError
	END WHEN

	!
	! Open the temp file
	!
350	CALL ASSG_CHANNEL(GL_YYYY_PP.SEQ%, STAT%)
	WHEN ERROR IN
		OPEN "GL_YYYY_PP.SEQ" AS FILE GL_YYYY_PP.SEQ%, &
			ORGANIZATION SEQUENTIAL FIXED, &
			MAP GL_YYYY_PP, &
			TEMPORARY, &
			ALLOW NONE, &
			ACCESS MODIFY
	USE
		FILENAME$ = "GL_YYYY_PP"
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
		FIND #GL_YYYY_PP_FROM.CH%, KEY #4% EQ BATCH_NO$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE AbortProcess IF (ERR = 155%)
		FILENAME$ = "GL_YYYY_PP - " + YYYY_PP1$
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
		GET #GL_YYYY_PP_FROM.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 600 IF (ERR = 11%)
		FILENAME$ = "GL_YYYY_PP - " + YYYY_PP1$
		CONTINUE HelpError
	END WHEN

	!
	! Make sure it has the same Batch number
	!
	GOTO 600 IF (GL_YYYY_PP::BTHNUM <> BATCH_NO$)

	!
	! PUT the record into both the "to" ledger and the temp file,
	!	and delete it from the "from" ledger
	!
	WHEN ERROR IN
		PUT #GL_YYYY_PP.CH%
		PUT #GL_YYYY_PP.SEQ%
		DELETE #GL_YYYY_PP_FROM.CH%
	USE
		CONTINUE 600 IF (ERR = 11%)
		FILENAME$ = "GL_YYYY_PP - " + YYYY_PP1$
		CONTINUE HelpError
	END WHEN

	!
	! Add to the record counter, and go up to GET the next record
	!
	RECORDS% = RECORDS% + 1%
	GOTO 500

	!
	! One way or another, we're done with the Ledgers
	!
600	CLOSE #GL_YYYY_PP.CH%
	CLOSE #GL_YYYY_PP_FROM.CH%

	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP2$ + ".LED", &
		BATCH_NO$, FROMPERIOD, TOPERIOD)

	GOTO AbortProcess &
		IF (EXIT_STATUS AND 1%) = 0%

	TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + " Deleted Records"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, PROCESS_BATCH, TITLE(), &
		UTL_REPORTX, "GL_" + YYYY_PP1$ + ".LED", &
		BATCH_NO$, FROMPERIOD, TOPERIOD)

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
		FILENAME$ = "GL_YYYY_PP"
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
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

	!
	! Print out the GL_YYYY_PP_SEQ record
	!
	TEXT$ = GL_YYYY_PP::ACCT + " " + &
		GL_YYYY_PP::SUBACC + " " + &
		GL_YYYY_PP::REFNO + " " + &
		GL_YYYY_PP::XREFNO + " " + &
		GL_YYYY_PP::CKNO + " " + &
		PRNT_DATE(GL_YYYY_PP::TRANDAT, OPT_MMDDYY) + "  " + &
		GL_YYYY_PP::SOURCE + "   " + &
		GL_YYYY_PP::TRANKEY + "  " + &
		LEFT(GL_YYYY_PP::DESCR, 27%) + &
		FORMAT$(GL_YYYY_PP::AMOUNT, "  ##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, LIN%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Add to running total
	!
	RUNTOT = RUNTOT + GL_YYYY_PP::AMOUNT

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
	TEXT$ = SPACE$(103%) + "Running Total:" + &
		FORMAT$(RUNTOT, "  ##,###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
	GOTO ExitProgram

	%PAGE

 AbortProcess:
	!
	! Print an error line if the period file did not exist
	!
	IF RECORDS% = -2%
	THEN
		TEXT$ = STRING$(18%, A"."B) + "'From' ledger:  " + &
			YYYY_PP1$ + " does not exist"
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
		UTL_REPORTX, "GL_" + YYYY_PP1$ + ".LED", &
		BATCH_NO$, FROMPERIOD, TOPERIOD)

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
			"GL_" + YYYY_PP1$ + ".LED", &
			BATCH_NO$, FROMPERIOD, TOPERIOD)

		GOTO PrintReport IF (LIN% = 999%)
	END IF

	%PAGE

 ExitProgram:
	!
	! Close the temporary sequential file (thereby killing it)
	!
	CLOSE #GL_YYYY_PP.SEQ%

	!
	! Finish up report
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
	! End of program GL_SPEC_MOVEBATCH
	!******************************************************************
	END
