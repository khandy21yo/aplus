1	%TITLE "Posting to GL System"
	%SBTTL "GL_TRAN_POSTGL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_TRAN_POSTGL(LONG OPT, &
		LONG SUBOPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD UTL_REPORTX, &
		GL_YYYY_PP_CDD GL_YYYY_PP_POST, &
		GL_CHART_CDD GL_CHART_POST, &
		STRING GLPERIOD)

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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	^*POST\*
	!	.b
	!	This function is used to post to the GL ledger file,
	!	and the chart of account file.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Inputs:
	!
	!	BATCH_NUMBER
	!		Batch number assigned for this posting.
	!
	! Outputs:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_TRAN_POSTGL
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_TRAN_POSTGL
	!	$ DELETE GL_TRAN_POSTGL.OBJ;*
	!
	! Author:
	!
	!	01/23/89 - Frank F. Starman
	!
	! Modification History:
	!
	!	07/10/89 - Aaron Redd
	!		Altered the function so that it puts the
	!		GL Chart information (Credits, Debits, Balance, etc.)
	!		in a file (GL_CHART.IDX) instead of in an array.
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error
	!		handler, changed to = "GL_" + YYYY_PP$.
	!
	!	05/01/91 - J. Shad Rydalch
	!		Changed so that SB_TRAN_POST is dealt with
	!		when ever someone calls this function.
	!
	!	08/06/91 - Craig Tanner
	!		Wrote help messages for when posting aborts for some
	!		reason.
	!
	!	10/08/91 - Frank F. Starman
	!		Check for keyboard input after marking file.
	!
	!	02/25/92 - Kevin Handy
	!		Added messages to printout so the user doesn't
	!		have to guess what Frank's "Aborted" message
	!		is really telling them.
	!
	!	05/07/92 - Frank F. Starman
	!		Trap error 155 at line 470.
	!
	!	06/01/92 - Frank F. Starman
	!		Summarize posting based on SUMMARY flag.
	!		Remove OPT_DETAIL from posting section.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/06/92 - Frank F. Starman
	!		Display units and hours without decimal point.
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (check)
	!
	!	09/11/92 - Dan Perkins
	!		Fixed "GOTO ExitFunction" statement error on line 300
	!		which ended function before checking SB_TRAN_POST at
	!		line 320.
	!		Cleaned program code.
	!
	!	01/12/92 - Frank F. Starman
	!		Set START_DATE to 0000 if is greater then END_DATE
	!
	!	02/01/93 - Frank F. Starman
	!		Do not work with SUMMARY flag.
	!
	!	03/19/93 - Kevin Handy
	!		Modified so that units and hours in summary are only
	!		for this posting, not some strange undetermined
	!		number that doesn't seem to mean anything.
	!
	!	03/30/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/14/95 - Kevin Handy
	!		Remove tons of commented out code.
	!		Format source code closer to 80 columns.
	!		Use FORMAT$(<0>##) instead of RIGHT(NUM1$()) many times
	!
	!	07/02/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!		Lose unecessary function definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/28/99 - Kevin Handy
	!		Clean up source code
	!
	!	09/08/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Clean up error trapping
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART
	MAP (GL_CHART_IDX)	GL_CHART_CDD		GL_CHART_IDX

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD		GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP
	MAP (GL_YYYY_PP_SEQ)	GL_YYYY_PP_CDD		GL_YYYY_PP_SEQ

	!
	! Memory MAPs
	!
	MAP (DP_OUTP_XUNSOL)	RRR_FLAG%

	!
	! Common memory areas
	!
	COM (GL_TRAN_POSTGL_TEST.CH) &
		GL_YYYY_PP.CH%, &
		GL_YYYY_PP.SEQ%, &
		GL_CHART.CH%, &
		GL_CHART.IDX%

	COM (GL_TRAN_POSTGL_TEST.COM) &
		STRING	CUR_PERIOD = 6%, &
		STRING	START_DATE = 4%, &
		STRING	END_DATE = 4%, &
		REAL	DEBIT_TOTAL, &
		REAL	CREDIT_TOTAL, &
		LONG	OUT_DATE, &
		LONG	OUT_BALANCE

	!
	! Declare internal variables
	!
	DECLARE	LONG	EXIT_STATUS

	DECLARE LONG	CONSTANT	CREDIT = 1%
	DECLARE LONG	CONSTANT	DEBIT = 2%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED
	EXTERNAL LONG	FUNCTION	SB_TRAN_POST

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Assume success
	!
	EXIT_STATUS = CMC$_NORMAL

	SELECT OPT

	!
	! Remove batch number
	!
	CASE OPT_RESTART

		YYYY_PP$ = LEFT(GLPERIOD, 4%) + "_" + TRM$(RIGHT(GLPERIOD, 5%))

		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"GL_" + YYYY_PP$ + ".LED", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		!
		! Open GL ledger file
		!
100		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.CRE"
		USE
			!
			! File locked
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "GL_" + YYYY_PP$
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		RECORDS% = 0%

105		WHEN ERROR IN
			FIND #GL_YYYY_PP.CH%, KEY #4% EQ BATCH_NUMBER
		USE
			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 120 IF ERR = 155%
			FILENAME$ = "GL_" + YYYY_PP$
			CONTINUE HelpError
		END WHEN

110		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitKeyFunction
			END IF
		END IF

		WHEN ERROR IN
			GET #GL_YYYY_PP.CH%
		USE
			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 120 IF ERR = 11%
			FILENAME$ = "GL_" + YYYY_PP$
			CONTINUE HelpError
		END WHEN

		IF (GL_YYYY_PP::BTHNUM = BATCH_NUMBER)
		THEN
			WHEN ERROR IN
				DELETE #GL_YYYY_PP.CH%
			USE
				FILENAME$ = "GL_" + YYYY_PP$
				CONTINUE HelpError
			END WHEN

			RECORDS% = RECORDS% + 1%
			GOTO 110
		END IF

120		CLOSE #GL_YYYY_PP.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Deleted Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	CASE OPT_CHECK

200		!
		! Open GL Period file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
			GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
			CLOSE GL_PERIOD.CH%
		USE
			!
			! File locked
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "GL_PERIOD"
			CONTINUE HelpError
		END WHEN

		CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)

		YYYY_PP$ = LEFT(GLPERIOD, 4%) + "_" + TRM$(RIGHT(GLPERIOD, 5%))

		IF (RIGHT(YYYY_PP$, 6%) > &
			FORMAT$(GL_PERIOD::FPFY, "<0>#")) OR &
			(RIGHT(YYYY_PP$, 6%) < "01")
		THEN
			TEXT$ = "%Bad GL Period"

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

			CALL HELP_PRINTMESS(SCOPE, &
				"the GL period must be within the range  of '01' to '" + &
				FORMAT$(GL_PERIOD::FPFY, "<0>#") + "'", &
				"E", "GL_TRAN_POSTGL", "", "ILLPER", &
				UTL_REPORTX, TITLE(), 0%)
	!++
	! Error:ILLPER
	!	^*Invalid Period\*
	!	.p
	!	^*Explaination:\*
	!	.p
	!	The entered period must be within the range of valid accounting
	!	periods.
	!	.p
	!	^*User Action:\*
	!	.p
	!	Renter a valid period where asked for in the-report settings
	!	screen.
	!
	! Index:
	!	.x Invalid Period
	!--
			EXIT_STATUS = CMC$_UNTERROR

			GOTO ExitFunction
		END IF

		IF LEN(XLATE(LEFT(YYYY_PP$, 4%), &
			STRING$(48%, 0%) + "0123456789")) <> 4%
		THEN
			TEXT$ = "%Bad GL Year"

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

			CALL HELP_PRINTMESS(SCOPE, &
				"the GL year " + LEFT(YYYY_PP$, 4%) + &
				" must have the following format:  YYYY", &
				"E", "GL_TRAN_POSTGL", "", "ILLFORM", &
				UTL_REPORTX, TITLE(), 0%)
	!++
	! Error:ILLFORM
	!	^*Invalid Period Format\*
	!	.p
	!	^*Explaination:\*
	!	.p
	!	The format for the GL period in the-report settings screen was
	!	entered wrong.
	!	.p
	!	*User Action:\*
	!	.p
	!	Renter the GL period in YYYYPP format.
	!
	! Index:
	!	.x Invalid Period Format
	!--
			EXIT_STATUS = CMC$_UNTERROR

			GOTO ExitFunction
		END IF

		IF YYYY_PP$ <= &
			(GL_PERIOD::YEAR + "_" + &
			FORMAT$(GL_PERIOD::LASTPERCLO, "<0>#"))
		THEN
			TEXT$ = "%GL Period Closed"

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

			CALL HELP_PRINTMESS(SCOPE, &
				"GL period " + YYYY_PP$ + " has been closed", &
				"E", "GL_TRAN_POSTGL", "", "GLCLOS", &
				UTL_REPORTX, TITLE(), 0%)
	!++
	! Error:GLCLOS
	!	^*GL Period is closed\*
	!	.p
	!	^*Explaination:\*
	!	.p
	!	The entered GL period has been closed.
	!	.p
	!	*User Action:\*
	!	.p
	!	Either reopen the GL period or enter the currently opened GL
	!	period.
	! Index:
	!	.x GL Period Closed
	!--

			EXIT_STATUS = CMC$_UNTERROR

			GOTO ExitFunction
		END IF

		CUR_PERIOD% = GL_PERIOD::LASTPERCLO + 1%
		YEAR$ = GL_PERIOD::YEAR

		IF CUR_PERIOD% > GL_PERIOD::FPFY
		THEN
			CUR_PERIOD% = 1%
			YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
		END IF

		CUR_PERIOD = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

		TEMP% = VAL%(MID(YYYY_PP$, 6%, 2%))
		END_DATE = GL_PERIOD::ENDDATE(TEMP%)
		TEMP% = TEMP% - 1%
		TEMP% = GL_PERIOD::FPFY &
			IF TEMP% < 1%
		START_DATE = GL_PERIOD::ENDDATE(TEMP%)
		START_DATE = "0000" &
			IF START_DATE > END_DATE

210		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"

			GET #GL_YYYY_PP.CH%, KEY #4% EQ BATCH_NUMBER
		USE
			!
			! File locked
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 220 IF (ERR = 5%) OR (ERR = 155%)
			FILENAME$ = "GL_" + YYYY_PP$
			CONTINUE HelpError
		END WHEN

		TEXT$ = "%Batch Already exists in GL_YYYY_PP"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		EXIT_STATUS = CMC$_WARNING

		GOTO ExitFunction

220		CALL ASSG_CHANNEL(GL_YYYY_PP.SEQ%, STAT%)

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

230		CALL ASSG_CHANNEL(GL_CHART.IDX%, STAT%)

		OPEN "GL_CHART.IDX" AS FILE GL_CHART.IDX%, &
			ORGANIZATION INDEXED FIXED, &
			MAP GL_CHART_IDX, &
			PRIMARY KEY (GL_CHART_IDX::ACCT), &
			BUFFER 32%, &
			TEMPORARY, &
			ALLOW NONE, &
			ACCESS MODIFY

		!
		! Assume dates will be OK
		!
		OUT_DATE = CMC$_NORMAL

		DEBIT_TOTAL, CREDIT_TOTAL = 0.0

		!
		! Deal with sub account now
		!
		EXIT_STATUS = SB_TRAN_POST(OPT_CHECK, SUBOPT, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, GLPERIOD)

	!
	! Create posting array
	!
	CASE OPT_ADDREC

		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR

				GOTO ExitKeyFunction
			END IF
		END IF

		!
		! Check date range
		!
		IF (SUBOPT AND SUBOPT_CHECK)
		THEN
			TSTDAT$ = RIGHT(GL_YYYY_PP_POST::TRANDAT, 5%)

			EXIT_STATUS, OUT_DATE = CMC$_DATEOUT &
				IF ((TSTDAT$ < START_DATE) OR &
				(TSTDAT$ > END_DATE)) AND &
				(START_DATE <> "")
		END IF

		!
		! Create transaction array
		!
		UPDATE.FLAG% = 0%

245		GL_YYYY_PP_SEQ = GL_YYYY_PP_POST

		PUT #GL_YYYY_PP.SEQ%

		!
		! Figure out debit/credit amounts
		!
		IF GL_YYYY_PP_POST::AMOUNT > 0.0
		THEN
			DEBIT_AMOUNT = GL_YYYY_PP_POST::AMOUNT
			CREDIT_AMOUNT= 0.0
		ELSE
			DEBIT_AMOUNT = 0.0
			CREDIT_AMOUNT= GL_YYYY_PP_POST::AMOUNT
		END IF

		DEBIT_TOTAL = FUNC_ROUND(DEBIT_TOTAL + DEBIT_AMOUNT, 2%)
		CREDIT_TOTAL = FUNC_ROUND(CREDIT_TOTAL + CREDIT_AMOUNT, 2%)

		!
		! Search trial balance list for currently existing account
		!
300		WHEN ERROR IN
			GET #GL_CHART.IDX%, KEY #0% EQ GL_YYYY_PP_POST::ACCT
		USE
			CONTINUE 310 IF ERR = 155%
			FILENAME$ = "GL_CHART"
			CONTINUE HelpError
		END WHEN

		GL_CHART_IDX::UNIT(0%) = GL_CHART_IDX::UNIT(0%) + &
			GL_YYYY_PP_POST::UNITS

		GL_CHART_IDX::HOUR(0%) = GL_CHART_IDX::HOUR(0%) + &
			GL_YYYY_PP_POST::HOURS

		GL_CHART_IDX::DOLLAR(0%) = GL_CHART_IDX::DOLLAR(0%) + &
			GL_YYYY_PP_POST::AMOUNT

		GL_CHART_IDX::DOLLAR(DEBIT) = GL_CHART_IDX::DOLLAR(DEBIT) + &
			DEBIT_AMOUNT

		GL_CHART_IDX::DOLLAR(CREDIT) = GL_CHART_IDX::DOLLAR(CREDIT) + &
			CREDIT_AMOUNT

		UPDATE #GL_CHART.IDX%

		GOTO 320

		!
		! Item not found, create it
		! Improvisation:  I am using DOLLAR(1%) to contain the Credits
		!		for an account, while DOLLAR(2%) will contain
		!		the Debits.
		!
310		GL_CHART_IDX::ACCT		= GL_YYYY_PP_POST::ACCT
		GL_CHART_IDX::DESCR		= GL_CHART_POST::DESCR
		GL_CHART_IDX::ACCTYPE		= GL_CHART_POST::ACCTYPE
		GL_CHART_IDX::DOLLAR(0%)	= GL_YYYY_PP_POST::AMOUNT
		GL_CHART_IDX::DOLLAR(CREDIT)	= CREDIT_AMOUNT
		GL_CHART_IDX::DOLLAR(DEBIT)	= DEBIT_AMOUNT
		GL_CHART_IDX::UNIT(0%)		= GL_YYYY_PP_POST::UNITS
		GL_CHART_IDX::HOUR(0%)		= GL_YYYY_PP_POST::HOURS
		GL_CHART_IDX::RUNUNIT		= GL_CHART_POST::RUNUNIT
		GL_CHART_IDX::RUNHOUR		= GL_CHART_POST::RUNHOUR
		GL_CHART_IDX::RUNDOL		= GL_CHART_POST::RUNDOL

		PUT #GL_CHART.IDX%

		!
		! Deal with sub account now.
		!
		! Only post to sub account if necessary
		!
320		GOTO ExitFunction IF (GL_YYYY_PP_POST::SUBACC = "")

		EXIT_STATUS = SB_TRAN_POST(OPT_ADDREC, SUBOPT, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP_POST, GLPERIOD)

	CASE OPT_CONFIRM

		!
		! Confirm posting
		!
		IF OUT_DATE <> CMC$_NORMAL
		THEN
			CALL HELP_PRINTMESS(SCOPE, &
				"date out of period range in posting to GL", &
				"E", "GL_TRAN_POSTGL", "", "OUTDATE", &
				UTL_REPORTX, TITLE(), 0%)
	!++
	! Error:OUTDATE
	!	^*Date out of Period\*
	!	.p
	!	^*Explaination:\*
	!	.p
	!	A transaction date in a journal is
	!	not in the GL period to be posted to.
	!	.p
	!	*User Action:\*
	!	.p
	!	Either specify that dates will not be checked, or
	!	change the dates in the journals so that they will be within range.
	! Index:
	!	.x Date out of Period
	!
	!--
			EXIT_STATUS = CMC$_UNTERROR

		END IF


		IF DEBIT_TOTAL + CREDIT_TOTAL <> 0.0
		THEN
			CALL HELP_PRINTMESS(SCOPE, &
				"credit and debit is not in balance", &
				"E", "GL_TRAN_POSTGL", "", "OUTBAL", &
				UTL_REPORTX, TITLE(), 0%)

	!++
	! Error:OUTBAL
	!	^*Journal out of Balance\*
	!	.p
	!	^*Explaination:\*
	!	.p
	!	The journal header is not in balance with the line items.
	!	.p
	!	*User Action:\*
	!	.p
	!	Either transactions must be added to balance the header, or
	!	change the header to balance the journal.
	! Index:
	!	.x Journal out of Balance
	!--

			EXIT_STATUS, OUT_BALANCE = CMC$_UNTERROR

		END IF

	CASE OPT_SUMMARY

		SELECT (SUBOPT AND SUBOPT_FINAL)

		CASE SUBOPT_FINAL
			GOSUB Summary

		CASE ELSE
			GOSUB Summary &
				IF OUT_BALANCE = CMC$_UNTERROR

		END SELECT

	!
	! Post to the GL ledger file and the chart of account file
	!
	CASE OPT_POSTFILE

		YYYY_PP$ = LEFT(GLPERIOD, 4%) + "_" + TRM$(RIGHT(GLPERIOD, 5%))

400		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"GL_" + GLPERIOD + ".LED", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR

				GOTO ExitKeyFunction
			END IF
		END IF

		!
		! Open GL transaction file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.CRE"
		USE
			!
			! File locked
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "GL_" + YYYY_PP$
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		RECORDS% = 0%

410		WHEN ERROR IN
			RESET #GL_YYYY_PP.SEQ%
		USE
			CONTINUE 445
		END WHEN

420		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR

				GOTO ExitKeyFunction
			END IF
		END IF

		WHEN ERROR IN
			GET #GL_YYYY_PP.SEQ%
		USE
			CONTINUE 445 IF ERR = 11%
			FILENAME$ = "GL_YYYY_PP"
			CONTINUE HelpError
		END WHEN

		GL_YYYY_PP = GL_YYYY_PP_SEQ

		IF (SUBOPT AND SUBOPT_REVERSE)
		THEN
			GL_YYYY_PP::AMOUNT = -GL_YYYY_PP::AMOUNT
			GL_YYYY_PP::UNITS = -GL_YYYY_PP::UNITS
			GL_YYYY_PP::HOURS = -GL_YYYY_PP::HOURS
		END IF

		PUT #GL_YYYY_PP.CH%

		RECORDS% = RECORDS% + 1%

		GOTO 420

445		CLOSE GL_YYYY_PP.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Posted Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

450		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, &
			"GL_CHART.MAS", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

455		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.CRE"
		USE
			!
			! File locked
			!
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "GL_CHART"
			CONTINUE HelpError
		END WHEN

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		GOTO ExitFunction IF (1% AND EXIT_STATUS) = 0%

		!
		! Reset the temporary GL Chart file
		!
460		WHEN ERROR IN
			RESET #GL_CHART.IDX%
		USE
			FILENAME$ = "GL_CHART"
			CONTINUE HelpError
		END WHEN

		RECORDS% = 0%

		!
		! Get the (next) GL Chart record
		!
470		WHEN ERROR IN
			GET #GL_CHART.IDX%
		USE
			CONTINUE 485 IF ERR = 11%
			FILENAME$ = "GL_CHART"
			CONTINUE HelpError
		END WHEN

		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR

				GOTO ExitKeyFunction
			END IF
		END IF

		IF (SUBOPT AND SUBOPT_REVERSE)
		THEN
			GL_CHART_IDX::DOLLAR(0%) = -GL_CHART_IDX::DOLLAR(0%)
			GL_CHART_IDX::UNIT(0%)   = -GL_CHART_IDX::UNIT(0%)
			GL_CHART_IDX::HOUR(0%)   = -GL_CHART_IDX::HOUR(0%)
		END IF

		WHEN ERROR IN
			GET #GL_CHART.CH%, KEY #0% EQ GL_CHART_IDX::ACCT
		USE
			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

 !			CONTINUE 470 IF ERR = 155%
			FILENAME$ = "GL_CHART"
			CONTINUE HelpError
		END WHEN

		IF CUR_PERIOD = GLPERIOD
		THEN
			GL_CHART::CURDOL = GL_CHART::CURDOL + &
				GL_CHART_IDX::DOLLAR(0%)
			GL_CHART::CURUNIT = GL_CHART::CURUNIT + &
				GL_CHART_IDX::UNIT(0%)
			GL_CHART::CURHOUR = GL_CHART::CURHOUR + &
				GL_CHART_IDX::HOUR(0%)
		END IF

		GL_CHART::RUNDOL = GL_CHART::RUNDOL + GL_CHART_IDX::DOLLAR(0%)
		GL_CHART::RUNUNIT = GL_CHART::RUNUNIT + GL_CHART_IDX::UNIT(0%)
		GL_CHART::RUNHOUR = GL_CHART::RUNHOUR + GL_CHART_IDX::HOUR(0%)

		GL_CHART::BATCH = BATCH_NUMBER

		!
		! Update the GL_CHART file
		!
		UPDATE #GL_CHART.CH%

		RECORDS% = RECORDS% + 1%

		GOTO 470

485		CLOSE #GL_CHART.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", "")

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + &
			" Updated Records"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		!
		! Post to Sub Account Balances file
		!
		EXIT_STATUS =  SB_TRAN_POST(OPT_POSTFILE, SUBOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, GL_YYYY_PP_POST, &
			GLPERIOD)

	CASE ELSE
		!
		! Undefined option
		!
		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:
	GL_TRAN_POSTGL = EXIT_STATUS
	EXIT FUNCTION

 ExitKeyFunction:
	TEXT$ = "%Abort Key Typed"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	GOTO ExitFunction

	%PAGE

 Summary:
	!******************************************************************
	! Print summary
	!******************************************************************
	I% = 1%

	WHILE (TITLE(I%) <> "")
		I% = I% + 1%
	NEXT

	J% = I% + 1%

	WHILE (TITLE(J%) <> "")
		TITLE(J%) = ""
		J% = J% + 1%
	NEXT

	YYYY_PP$ = LEFT(GLPERIOD, 4%) + "_" + TRM$(RIGHT(GLPERIOD, 5%))

	TITLE(I% + 1%) = "Batch number: " + BATCH_NUMBER + "        To: " + &
		"GL_" + YYYY_PP$ + ".LED"

	TITLE(I% + 1%) = STRING$(132% - LEN(TITLE(I% + 1%)), A"="B) + &
		TITLE(I% + 1%)

	TITLE(I% + 2%) = "Account            Description" + SPACE$(29%) + &
		"Debit         Credit        Balance      Units      Hours"

	TITLE(I% + 3%) = "."

	LIN% = 999%

	!
	! Print transmittal
	!
1000	WHEN ERROR IN
		RESET #GL_CHART.IDX%
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Get the (next) GL Chart record from the POST process
	!
1010	WHEN ERROR IN
		GET #GL_CHART.IDX%
	USE
		CONTINUE 1020 IF ERR = 11%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Check unsolicited input
	!
	IF RRR_FLAG%
	THEN
		IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
		THEN
			EXIT_STATUS = CMC$_UNTERROR

			GOTO RetSummary
		END IF
	END IF

	TEXT$ = GL_CHART_IDX::ACCT + " " + &
		LEFT(GL_CHART_IDX::DESCR, 30%) + " "

	IF (SUBOPT AND SUBOPT_REVERSE)
	THEN
		TEXT$ = TEXT$ + &
			FORMAT$(-GL_CHART_IDX::DOLLAR(CREDIT), &
			"<%>##,###,###.## ") + &
			FORMAT$(GL_CHART_IDX::DOLLAR(DEBIT), &
			"<%>##,###,###.## ")

		GL_CHART_IDX::DOLLAR(0%) = 0.0
	ELSE
		TEXT$ = TEXT$ + &
			FORMAT$(GL_CHART_IDX::DOLLAR(DEBIT), &
			"<%>##,###,###.## ") + &
			FORMAT$(-GL_CHART_IDX::DOLLAR(CREDIT), &
			"<%>##,###,###.## ")
	END IF

	IF INSTR(1%, "RE", EDIT$(GL_CHART_IDX::ACCTYPE, -1%)) = 0%
	THEN
		TEXT$ = TEXT$ + &
			FORMAT$(GL_CHART_IDX::RUNDOL + &
			GL_CHART_IDX::DOLLAR(0%), "<%>##,###,###.## ")
	ELSE
		TEXT$ = TEXT$ + FORMAT$(0.0, "<%>##,###,###.## ")
	END IF

	TEXT$ = TEXT$ + &
		FORMAT$(GL_CHART_IDX::UNIT(0%), "<%>#,###,### ") + &
		FORMAT$(GL_CHART_IDX::HOUR(0%), "<%>#,###,###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, LIN%)

	LIN% = 0%

	GOTO 1010

	!
	! Print out totals
	!
1020	TEXT$ = "Total" + SPACE$(45%) + &
		FORMAT$(DEBIT_TOTAL, "###,###,###.## ") + &
		FORMAT$(-CREDIT_TOTAL, "###,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, -1%)

 RetSummary:
	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), &
		UTL_REPORTX, TITLE(), 0%)

	EXIT_STATUS = CMC$_UNTERROR

	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
