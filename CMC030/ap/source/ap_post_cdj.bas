1	%TITLE "Post Cash Disbursements Journal"
	%SBTTL "AP_POST_CDJ"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
	! Computer Management Center
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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:APCDPO
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Post Cash Disbursements Journal\* option
	!	posts the subject journal to both the General Ledger and the
	!	Accounts Payable Subsidiary Ledger.
	!	.note RETENTION
	!	The Cash Disbursements Posting Transmittal report
	!	should be permanently filed along with the related
	!	Cash Disbursements Journal report.
	!	.end note
	!	^*
	!	.note
	!	The Cash Disbursements Journal report ^&must\& be printed
	!	prior to executing the Cash Disbursements Post option,
	!	since the journal file is deleted at the completion of
	!	the posting routine.
	!	.end note
	!	\*
	!
	! Index:
	!	.x Accounts Payable>Cash Disbursements>Post CD Journal
	!	.x Cash Disbursements>Post CD Journal
	!	.x Post>Cash Disbursements Journal
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_POST_CDJ/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_POST_CDJ, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_POST_CDJ.OBJ;*
	!
	! Author:
	!
	!	06/23/89 - Aaron Redd
	!
	! Modification history:
	!
	!	09/14/89 - Frank Starman
	!		Check undefined chart of account number only if
	!		it is necessary.
	!
	!	10/23/89 - Frank Starman
	!		Post cash summary after each check.
	!
	!	08/06/91 - Craig Tanner
	!		Declare CHECK_PERIOD so post will check dates.
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	02/02/95 - Kevin Handy
	!		Lose excessive tabs.
	!		Add RESET for AP_CDJ, (line 490) with error trap.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/04/96 - Kevin Handy
	!		Reformat source code.
	!		Add batch number to AP_CDJ.
	!
	!	06/07/96 - Kevin Handy
	!		Added batch number to kill command.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/25/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/13/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	01/20/2004 - Kevin Handy
	!		Add CHECK_LIST entries even if they come to zero,
	!		so we have a history of the check information.
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include constants and and some functions
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%PAGE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.HB"
	MAP (AP_CDJ)		AP_CDJ_CDD	AP_CDJ

	MAP (AP_OPEN_DIST)	AP_OPEN_DIST_CDD AP_OPEN_DIST

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	RECORD ACCT_SUMMARY_CDD
		STRING	ACCT = 18%
		GFLOAT	AMOUNT
		GFLOAT	DISAMT
	END RECORD

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	AP_EXAM_VENDOR
	EXTERNAL LONG	FUNCTION	AP_TRAN_POSTAP
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES

	!
	! Dimension statements
	!
	DIM ACCT_SUMMARY_CDD	ACCOUNT_LIST(100%), CHECK_LIST(100%)

	!
	! Declare internal variables
	!
	DECLARE	AP_OPEN_CDD		AP_OPEN
	DECLARE	AP_VENDOR_CDD		AP_VENDOR_EXAM
	DECLARE	GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX

	DECLARE LONG			CHECK_PERIOD
	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE	LONG			PRNT_SUMMARY

	DECLARE	STRING			BATCH_NUMBER
	DECLARE	STRING			GLPERIOD
	DECLARE	STRING			AP.INTER.PERIOD
	DECLARE	STRING			GL.INTER.PERIOD
	DECLARE	STRING			POSTDATE
	DECLARE	STRING			POSTTIME
	DECLARE	STRING			TEXT
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
	! Title
	!
	TITLE(1%) = "CASH  DISBURSEMENTS  JOURNAL  POSTING  PROTOCOL"
	TITLE(2%) = "Accounts Payable System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	%PAGE

	!**************************************************************
	! Process `from user' input
	!**************************************************************

	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GLPERIOD = TRM$(UTL_REPORTX::OPTDEF(0%))
	!++
	! Abstract:FLD01
	!	^*(01) Year Period\*
	!	.b
	!	.lm +5
	!	The ^*Year Period\* field enters the year
	!	and the accounting period into which the corresponding
	!	records are to be posted in the General Ledger.
	!	.lm -5
	!
	! Index:
	!	.x YearPeriod>Cash Distribution Posting
	!	.x Cash Distribution Posting>YearPeriod
	!
	!--

	CDJ_BATCH$ = TRM$(UTL_REPORTX::OPTDEF(1%))
	!++
	! Abstract:FLD02
	!	^*(02) Batch Number\*
	!	.b
	!	This field specifies which journal to post.
	!
	! Index:
	!	.x Batch Number>Cash Distribution Posting
	!	.x Cash Distribution Posting>Batch Number
	!
	!--

	CHECK_DATE$ = TRM$(UTL_REPORTX::OPTDEF(2%))
	!++
	! Abstract:FLD03
	!	^*(03) Check Dates\*
	!	.b
	!	.lm +5
	!	The ^*Check Dates\* checks the transaction date
	!	before it is posted. If ^*Y\* is entered, the date of the transaction will
	!	be checked to see if it falls within the specified posting period. If ^*N\*
	!	is entered, the transaction date will not be checked and all transactions will
	!	be posted.
	!	.lm -5
	!
	! Index:
	!	.x Check Dates>Cash Distribution Posting
	!	.x Cash Distribution Posting>Check Dates
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK &
		IF (CHECK_DATE$ = "Y")

	!
	! Set up the value for the "user defined" batch number
	!
	BATCH_NO$ = ""

310	!
	! Open AP CD journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.UPD"
	USE
		IF ERR = 138%
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	%PAGE

320	!******************************************************************
	!	1) See if the posting process has been interrupted
	!	2) If not interrupted, go on
	!	3) If interrupted, delete the superceded ledger records
	!		and then continue
	!******************************************************************

	!******************************************************************
	! Check if posting process has been interrupted
	!******************************************************************

	!
	! Open up batch control file and check if interrupted
	!
	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AP_CDJ", BATCH_NO$, &
		GL.INTER.PERIOD, AP.INTER.PERIOD)

	SELECT INTR_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Process was interrupted
	!
	CASE CMC$_WARNING

		IF TRM$(GL.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				GL_TRAN_POSTGL(OPT_RESTART, SUBOPT_NOOPT, &
					BATCH_NUMBER, TITLE(), &
					UTL_REPORTX, "", "", &
					GL.INTER.PERIOD) <> CMC$_NORMAL
		END IF

		IF TRM$(AP.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				AP_TRAN_POSTAP(OPT_RESTART, SUBOPT_NOOPT, &
					BATCH_NUMBER, TITLE(), &
					UTL_REPORTX, "", "", &
					AP.INTER.PERIOD) <> CMC$_NORMAL
		END IF

	!
	! Abort post process
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	%PAGE

	!******************************************************************
	!	1) Assign a batch number
	!	2) Make sure no legitimate records in the ledger already
	!		have this batch number; if any records do have
	!		this newly assigned number, go back to (1) and
	!		get a new one.
	!******************************************************************

 AssignBatch:
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted &
		IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AP_CDJ", BATCH_NO$, GLPERIOD, "") <> CMC$_NORMAL

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", GLPERIOD)

	SELECT EXIT_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something's definitely wrong here
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	EXIT_STATUS = AP_TRAN_POSTAP(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", GLPERIOD)

	SELECT EXIT_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something's definitely wrong here
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	%PAGE

	!******************************************************************
	!	1) List the debits/credits transmittal for the user while
	!		also putting the journal data in temporary files
	!	2) If confirmation, then go on
	!******************************************************************

	!******************************************************************
	! Create transmittal
	!******************************************************************
	GOTO Aborted &
		IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AP_CDJ", BATCH_NO$, "", "") <> CMC$_NORMAL

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	!
	! Blank flags
	!
	VENDNUM$, APACCT$, CASHACCT$, DISCACCT$, TRANDAT$ = " "

490	WHEN ERROR IN
		RESET #AP_CDJ.CH%
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Read in one record from the AP CD Journal file
	!
500	WHEN ERROR IN
		GET #AP_CDJ.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ListChecks IF ERR = 11%
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec &
		IF TRM$(AP_CDJ::CKNUM) = ""

	IF CK_TEST$ <> "" AND CK_TEST$ <> AP_CDJ::CKNUM
	THEN
		!
		! Generate a GL for check payment record to pass
		! to the post function
		!
		GL_YYYY_PP::SOURCE	= "CD"
		GL_YYYY_PP::REFNO	= CK_TEST$
		GL_YYYY_PP::TRANDAT	= TEST_CKDAT$
		GL_YYYY_PP::DESCR	= TEST_VENNAM$
		GL_YYYY_PP::XREFNO	= TEST_VENNUM$
		GL_YYYY_PP::POSTIM	= POSTTIME
		GL_YYYY_PP::POSDAT	= POSTDATE
		GL_YYYY_PP::CKNO	= CK_TEST$
		GL_YYYY_PP::TRANKEY	= ""
		GL_YYYY_PP::SUBACC	= ""
		GL_YYYY_PP::OPERATION	= ""
		GL_YYYY_PP::UNITS	= 0.0
		GL_YYYY_PP::HOURS	= 0.0
		GL_YYYY_PP::UPDSTA	= ""
		GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

		FOR LOOP% = 1% TO CHECK_LIST%

 !			IF CHECK_LIST(LOOP%)::AMOUNT <> 0.0
 !			THEN
				GL_YYYY_PP::ACCT = CHECK_LIST(LOOP%)::ACCT
				GL_YYYY_PP::AMOUNT = -CHECK_LIST(LOOP%)::AMOUNT

				ST% = GL_EXAM_CHART(AP_CDJ::CASH_ACCT, &
					GL_CHART_EXAM)

				!
				! Put the record into the temporary file
				!
				GOTO Aborted IF GL_TRAN_POSTGL(OPT_ADDREC, &
					SUBOPT_NOOPT, "", TITLE(), &
					UTL_REPORTX, GL_YYYY_PP, &
					GL_CHART_EXAM, GLPERIOD) &
					<> CMC$_NORMAL

 !			END IF

		NEXT LOOP%
		CHECK_LIST% = 0%
	END IF

	!
	! Check the vendor number - is it defined?
	!
	EXIT_STATUS = AP_EXAM_VENDOR(AP_CDJ::VENNUM, AP_VENDOR_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		VENDNUM$ = "*"

	!
	! SNAFU:  Situation Normal - All Fouled Up
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Keep track of information on this CDJ check number
	!
	CK_TEST$ = AP_CDJ::CKNUM
	TEST_CKDAT$ = AP_CDJ::CKDAT
	TEST_VENNAM$ = AP_VENDOR_EXAM::VENNAM
	TEST_VENNUM$ = AP_CDJ::VENNUM

	!
	! Do special things if user is masking
	!
	IF INSTR(1%, AP_CDJ::CASH_ACCT + AP_CDJ::AP_ACCT + &
		AP_CDJ::DISCLOST_ACCT, "?")
	THEN
		GOSUB LoadOpenDist

	ELSE
		ACCOUNT_LIST% = 1%
		ACCOUNT_LIST(1%)::ACCT = ""
		ACCOUNT_LIST(1%)::AMOUNT = AP_CDJ::CKAMT
		ACCOUNT_LIST(1%)::DISAMT = AP_CDJ::DISAMT

	END IF

	!
	! Summarize check information
	!
	IF INSTR(1%, AP_CDJ::CASH_ACCT, "?")
	THEN
		FOR LOOP% = 1% TO ACCOUNT_LIST%

			AMOUNT = ACCOUNT_LIST(LOOP%)::AMOUNT
			AMOUNT = AMOUNT + ACCOUNT_LIST(LOOP%)::DISAMT &
				IF (AP_CDJ::DISC_LOST_AMT <> 0.0)

			CALL GL_ASSG_ACCMASK(AP_CDJ::CASH_ACCT, &
				ACCOUNT_LIST(LOOP%)::ACCT, &
				ACCOUNT$)

			GOSUB SummarizeCash &
				IF AMOUNT <> 0.0

		NEXT LOOP%

	ELSE
		AMOUNT = AP_CDJ::CKAMT + AP_CDJ::DISC_LOST_AMT
		ACCOUNT$ = AP_CDJ::CASH_ACCT

		GOSUB SummarizeCash &
			IF AMOUNT <> 0.0
	END IF

	GOTO PostToGL &
		IF (AP_CDJ::DISC_LOST_AMT = 0.0)

	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::SOURCE	= "CD"
	GL_YYYY_PP::REFNO	= AP_CDJ::INVNUM
	GL_YYYY_PP::TRANDAT	= AP_CDJ::CKDAT
	GL_YYYY_PP::DESCR	= AP_VENDOR_EXAM::VENNAM
	GL_YYYY_PP::XREFNO	= AP_CDJ::VENNUM
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::CKNO	= AP_CDJ::CKNUM
	GL_YYYY_PP::TRANKEY	= AP_CDJ::TRANKEY
	GL_YYYY_PP::SUBACC	= ""
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	IF INSTR(1%, AP_CDJ::DISCLOST_ACCT, "?")
	THEN
		FOR LOOP% = 1% TO ACCOUNT_LIST%

			IF ACCOUNT_LIST(LOOP%)::DISAMT <> 0.0
			THEN
				CALL GL_ASSG_ACCMASK(AP_CDJ::DISCLOST_ACCT, &
					ACCOUNT_LIST(LOOP%)::ACCT, &
					GL_YYYY_PP::ACCT)

				GL_YYYY_PP::AMOUNT = ACCOUNT_LIST(LOOP%)::DISAMT

				!
				! See if Disclost Account number is defined
				!
				EXIT_STATUS = GL_EXAM_CHART(GL_YYYY_PP::ACCT, GL_CHART_EXAM)

				SELECT EXIT_STATUS
				!
				! Success; go on
				!
				CASE CMC$_NORMAL

				!
				! Undefined; set a flag and go on
				!
				CASE CMC$_UNDEFINED
					DISCACCT$ = "*"

				!
				! SNAFU:  (Situation Normal - it's All Fouled Up)
				!
				CASE ELSE
					GOTO Aborted

				END SELECT

				!
				! Put the record into the temporary file
				!
				EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
					CHECK_PERIOD, "", TITLE(), &
					UTL_REPORTX, GL_YYYY_PP, &
					GL_CHART_EXAM, GLPERIOD)

					!
					! Make sure the date is in the correct period
					!
					SELECT EXIT_STATUS
					!
					! Date correct; go on
					!
					CASE CMC$_NORMAL

					!
					! Date out of range
					!
					CASE CMC$_DATEOUT
						TRANDAT$ = "*"

					!
					! Weird happenings at the 7-11
					!
					CASE ELSE
						GOTO Aborted

					END SELECT

			END IF

		NEXT LOOP%

	ELSE
		GL_YYYY_PP::ACCT	= AP_CDJ::DISCLOST_ACCT
		GL_YYYY_PP::AMOUNT	= AP_CDJ::DISC_LOST_AMT

		!
		! See if Disclost Account number is defined
		!
		EXIT_STATUS = GL_EXAM_CHART(AP_CDJ::DISCLOST_ACCT, &
			GL_CHART_EXAM)

		SELECT EXIT_STATUS
		!
		! Success; go on
		!
		CASE CMC$_NORMAL

		!
		! Undefined; set a flag and go on
		!
		CASE CMC$_UNDEFINED
			DISCACCT$ = "*"

		!
		! SNAFU:  (Situation Normal - it's All Fouled Up)
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		!
		! Put the record into the temporary file
		!
		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, "", &
			TITLE(), UTL_REPORTX, &
			GL_YYYY_PP, GL_CHART_EXAM, &
			GLPERIOD)

		!
		! Make sure the date is in the correct period
		!
		SELECT EXIT_STATUS
		!
		! Date correct; go on
		!
		CASE CMC$_NORMAL

		!
		! Date out of range
		!
		CASE CMC$_DATEOUT
			TRANDAT$ = "*"

		!
		! Weird happenings at the 7-11
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

	END IF

 PostToGL:
	GOTO PostToAP &
		IF (AP_CDJ::CKAMT = 0.0)

	!
	! Generate a GL for check payment record to pass through &
	! to the post function
	!
	GL_YYYY_PP::SOURCE	= "CD"
	GL_YYYY_PP::REFNO	= AP_CDJ::INVNUM
	GL_YYYY_PP::TRANDAT	= AP_CDJ::CKDAT
	GL_YYYY_PP::DESCR	= AP_VENDOR_EXAM::VENNAM
	GL_YYYY_PP::XREFNO	= AP_CDJ::VENNUM
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::CKNO	= AP_CDJ::CKNUM
	GL_YYYY_PP::TRANKEY	= AP_CDJ::TRANKEY
	GL_YYYY_PP::SUBACC	= ""
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	IF INSTR(1%, AP_CDJ::AP_ACCT, "?")
	THEN
		FOR LOOP% = 1% TO ACCOUNT_LIST%

			IF ACCOUNT_LIST(LOOP%)::AMOUNT <> 0.0
			THEN
				CALL GL_ASSG_ACCMASK(AP_CDJ::AP_ACCT, &
					ACCOUNT_LIST(LOOP%)::ACCT, &
					GL_YYYY_PP::ACCT)


				!
				! See if AP Account number used is defined
				!
				EXIT_STATUS = GL_EXAM_CHART(GL_YYYY_PP::ACCT, GL_CHART_EXAM)

				SELECT EXIT_STATUS
				!
				! Success; go on
				!
				CASE CMC$_NORMAL

				!
				! Undefined; set a flag and go on
				!
				CASE CMC$_UNDEFINED
					APACCT$ = "*"

				!
				! SNAFU:  (Situation Normal - it's All Fouled Up)
				!
				CASE ELSE
					GOTO Aborted

				END SELECT

				GL_YYYY_PP::AMOUNT = ACCOUNT_LIST(LOOP%)::AMOUNT

				!
				! Put the record into the temporary file
				!
				EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
					CHECK_PERIOD, "", TITLE(), &
					UTL_REPORTX, GL_YYYY_PP, &
					GL_CHART_EXAM, GLPERIOD)

				!
				! Make sure the date is in the correct period
				!
				SELECT EXIT_STATUS
				!
				! Date correct; go on
				!
				CASE CMC$_NORMAL

				!
				! Date out of range
				!
				CASE CMC$_DATEOUT
					TRANDAT$ = "*"

				!
				! Weird happenings at the 7-11
				!
				CASE ELSE
					GOTO Aborted

				END SELECT

			END IF

		NEXT LOOP%

	ELSE
		GL_YYYY_PP::ACCT	= AP_CDJ::AP_ACCT
		GL_YYYY_PP::AMOUNT	= AP_CDJ::CKAMT

		!
		! See if AP Account number used is defined
		!
		EXIT_STATUS = GL_EXAM_CHART(AP_CDJ::AP_ACCT, GL_CHART_EXAM)

		SELECT EXIT_STATUS
		!
		! Success; go on
		!
		CASE CMC$_NORMAL

		!
		! Undefined; set a flag and go on
		!
		CASE CMC$_UNDEFINED
			APACCT$ = "*"

		!
		! SNAFU:  (Situation Normal - it's All Fouled Up)
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		!
		! Put the record into the temporary file
		!
		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, "", &
			TITLE(), UTL_REPORTX, &
			GL_YYYY_PP, GL_CHART_EXAM, &
			GLPERIOD)

		!
		! Make sure the date is in the correct period
		!
		SELECT EXIT_STATUS
		!
		! Date correct; go on
		!
		CASE CMC$_NORMAL

		!
		! Date out of range
		!
		CASE CMC$_DATEOUT
			TRANDAT$ = "*"

		!
		! Weird happenings at the 7-11
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

	END IF

 PostToAP:
	!
	! Generate a AP record to pass through to the post function
	!
	AP_OPEN::VENNUM		= AP_CDJ::VENNUM
	AP_OPEN::TRANKEY	= AP_CDJ::TRANKEY
	AP_OPEN::TRANKEY_DATE	= ""
	AP_OPEN::INVNUM		= AP_CDJ::INVNUM
	AP_OPEN::INVDAT		= AP_CDJ::INVDAT
	AP_OPEN::INVAMT		= 0.0
	AP_OPEN::CODE_1099	= ""
	AP_OPEN::AMT_1099	= 0.0
	AP_OPEN::USE_JOB_NUM	= ""
	AP_OPEN::USE_AMT	= 0.0
	AP_OPEN::DISCDAT	= ""
	AP_OPEN::DISAMT		= -AP_CDJ::DISC_LOST_AMT
	AP_OPEN::DUEDAT		= ""
	AP_OPEN::PONUM		= ""
	AP_OPEN::AP_ACCT	= AP_CDJ::AP_ACCT
	AP_OPEN::CASH_ACCT	= AP_CDJ::CASH_ACCT
	AP_OPEN::CKNUM		= AP_CDJ::CKNUM
	AP_OPEN::CKDAT		= AP_CDJ::CKDAT
	AP_OPEN::CKDESC		= AP_CDJ::CKDESC
	AP_OPEN::CKAMT		= AP_CDJ::CKAMT + AP_CDJ::DISC_LOST_AMT
	AP_OPEN::UPDATED	= GLPERIOD + "00"
	AP_OPEN::CLOSEDATE	= ""
	AP_OPEN::SELECTED	= ""
	AP_OPEN::BATCH		= BATCH_NUMBER

	!
	! Put the AP Open record into the temporary file
	!
	GOTO Aborted &
		IF AP_TRAN_POSTAP(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AP_OPEN, AP_OPEN_DIST, "") <> CMC$_NORMAL

	!
	! Was anything undefined?
	!
	IF INSTR(1%, VENDNUM$ + APACCT$ + CASHACCT$ + DISCACCT$ + TRANDAT$, "*")
	THEN
		!
		! Put the invalid stuff in TEXT to send to OUTP_UNDEFCODES
		!
		TEXT = VENDNUM$ + AP_CDJ::VENNUM + " " + &
			APACCT$ + AP_CDJ::AP_ACCT + " " + &
			CASHACCT$ + AP_CDJ::CASH_ACCT + " " + &
			DISCACCT$ + AP_CDJ::DISCLOST_ACCT + " " + &
			TRANDAT$ + PRNT_DATE(AP_CDJ::CKDAT, 8%)

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, &
				TEXT) <> CMC$_NORMAL

		!
		! Blank flags
		!
		VENDNUM$, APACCT$, CASHACCT$, DISCACCT$, TRANDAT$ = " "

	END IF

	GOTO GetNextRec

 ListChecks:
	!
	! Generate a GL for check payment record to pass to the post function
	!
	GL_YYYY_PP::SOURCE	= "CD"
	GL_YYYY_PP::REFNO	= CK_TEST$
	GL_YYYY_PP::TRANDAT	= TEST_CKDAT$
	GL_YYYY_PP::DESCR	= TEST_VENNAM$
	GL_YYYY_PP::XREFNO	= TEST_VENNUM$
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::CKNO	= CK_TEST$
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= ""
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	FOR LOOP% = 1% TO CHECK_LIST%

 !		IF CHECK_LIST(LOOP%)::AMOUNT <> 0.0
 !		THEN
			GL_YYYY_PP::ACCT	= CHECK_LIST(LOOP%)::ACCT
			GL_YYYY_PP::AMOUNT	= -CHECK_LIST(LOOP%)::AMOUNT

			ST% = GL_EXAM_CHART(AP_CDJ::CASH_ACCT, GL_CHART_EXAM)

			!
			! Put the record into the temporary file
			!
			GOTO Aborted &
				IF GL_TRAN_POSTGL(OPT_ADDREC, &
				SUBOPT_NOOPT, "", TITLE(), &
				UTL_REPORTX, GL_YYYY_PP, &
				GL_CHART_EXAM, GLPERIOD) &
				<> CMC$_NORMAL

 !		END IF

	NEXT LOOP%

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", GLPERIOD)

	EXIT_STATUS = EXIT_STATUS AND OUTP_UNDEFCODES(OPT_CONFIRM, TITLE(), &
		UTL_REPORTX, "")

	GOTO Aborted IF EXIT_STATUS <> CMC$_NORMAL

	%PAGE

	!******************************************************************
	!	1) Since everything's correct and we have the user's
	!		permission, transfer all data from the temporary
	!		files into the ledger
	!******************************************************************
	!
	! Begin posting
	!
	GOTO Interrupt &
		IF GL_TRAN_POSTGL(OPT_POSTFILE, SUBOPT_DETAIL, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", GLPERIOD) <> CMC$_NORMAL

	!
	! Post the AP CD Journal items
	!
	GOTO Interrupt &
		IF AP_TRAN_POSTAP(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", GLPERIOD) <> CMC$_NORMAL

	%PAGE

	!
	! Remove file
	!
5000	CLOSE AP_CDJ.CH%

5100	SMG_STATUS% = LIB$DELETE_FILE(AP_CDJ.NAME$ + ";*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", BATCH_NO$, "", "")

	PRNT_SUMMARY = SUBOPT_FINAL

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************
	!
	! Print credit and debit transmittals
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_SUMMARY, PRNT_SUMMARY, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	!
	! Print undefined codes (if any)
	!
	TEXT = "VendorNum   AP Account          Cash Account        " + &
		"Disclost Account    TransDate"

	EXIT_STATUS = OUTP_UNDEFCODES(OPT_SUMMARY, TITLE(), UTL_REPORTX, TEXT)

	!
	! Finish up the transmittal
	!
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 SummarizeCash:
	!******************************************************************
	! Summarise a check by its account(s)
	!******************************************************************

	TEMP% = 0%
	TEMP% = LOOP1% &
		IF CHECK_LIST(LOOP1%)::ACCT = ACCOUNT$ &
		FOR LOOP1% = 1% TO CHECK_LIST%

	IF TEMP% = 0%
	THEN
		TEMP%, CHECK_LIST% = CHECK_LIST% + 1%
		CHECK_LIST(TEMP%)::ACCT = ACCOUNT$
		CHECK_LIST(TEMP%)::AMOUNT = 0.0

		!
		! See if Cash Account number is defined
		!
		EXIT_STATUS = GL_EXAM_CHART(ACCOUNT$, GL_CHART_EXAM)

		SELECT EXIT_STATUS
		!
		! Success; go on
		!
		CASE CMC$_NORMAL

		!
		! Undefined; set a flag and go on
		!
		CASE CMC$_UNDEFINED
			CASHACCT$ = "*"

		!
		! SNAFU:  (Situation Normal - it's All Fouled Up)
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

	END IF

	CHECK_LIST(TEMP%)::AMOUNT = CHECK_LIST(TEMP%)::AMOUNT + AMOUNT

	RETURN

	%PAGE

 LoadOpenDist:
	!******************************************************************
	! Open AP_OPEN_DIST and read information into an array
	!******************************************************************
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.OPN"

	ACCOUNT_LIST% = 0%
	INV_TOTAL = 0.0
	DIS_TOTAL = 0.0

6000	WHEN ERROR IN
		FIND #AP_OPEN_DIST.CH%, KEY #0% GE AP_CDJ::TRANKEY, REGARDLESS
	USE
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

6010	WHEN ERROR IN
		GET #AP_OPEN_DIST.CH%, REGARDLESS
	USE
		CONTINUE 6020 IF (ERR = 11%)
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

	IF AP_OPEN_DIST::TRANKEY = AP_CDJ::TRANKEY
	THEN
		TEMP% = 0%
		TEMP% = LOOP% &
			IF ACCOUNT_LIST(LOOP%)::ACCT = AP_OPEN_DIST::ACCT &
				FOR LOOP% = 1% TO ACCOUNT_LIST%

		IF TEMP% = 0%
		THEN
			ACCOUNT_LIST%, TEMP% = ACCOUNT_LIST% + 1%

			ACCOUNT_LIST(ACCOUNT_LIST%)::ACCT = AP_OPEN_DIST::ACCT
			ACCOUNT_LIST(ACCOUNT_LIST%)::AMOUNT = 0.0
			ACCOUNT_LIST(ACCOUNT_LIST%)::DISAMT = 0.0

		END IF

		ACCOUNT_LIST(TEMP%)::AMOUNT = &
			ACCOUNT_LIST(TEMP%)::AMOUNT + &
			AP_OPEN_DIST::AMOUNT
		ACCOUNT_LIST(TEMP%)::DISAMT = &
			ACCOUNT_LIST(TEMP%)::DISAMT + &
			AP_OPEN_DIST::DISCAMT

		INV_TOTAL = FUNC_ROUND(INV_TOTAL + AP_OPEN_DIST::AMOUNT, 2%)
		DIS_TOTAL = FUNC_ROUND(DIS_TOTAL + AP_OPEN_DIST::DISCAMT, 2%)

		GOTO 6010
	END IF

6020	CLOSE AP_OPEN_DIST.CH%

	IF (DIS_TOTAL <> AP_CDJ::DISAMT) AND (AP_CDJ::DISAMT <> 0.0)
	THEN
		TEXT$ = "Vendor: " + AP_CDJ::VENNUM + "  Tran: " + &
			AP_CDJ::TRANKEY + " discount does not match invoice!"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted
	END IF

	IF (INV_TOTAL <> (AP_CDJ::CKAMT + AP_CDJ::DISAMT))
	THEN
		TEXT$ = "Vendor: " + AP_CDJ::VENNUM + "  Tran: " + &
			AP_CDJ::TRANKEY + " net amount does not match invoice!"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted
	END IF

	RETURN

	%PAGE

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************
	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", &
			BATCH_NO$, "", "")
		GOTO ExitProgram
	END IF


 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", BATCH_NO$, "", "")
	GOTO ExitProgram

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram


19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32000	!******************************************************************
	! End of posting program
	!******************************************************************
	END
