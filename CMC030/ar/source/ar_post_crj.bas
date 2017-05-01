1	%TITLE "Accounts Receivable CR Journal Post Program"
	%SBTTL "AR_POST_CRJ"
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
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Post Cash Receipts Journal\* is provided to post entries
	!	which have been made in the Cash Receipts Journal to the General
	!	Ledger System and the Accounts Receivable Register.
	!	.b
	!	If the entries being posted are out of balance, the message,
	!	^*"Batch is OUT OF BALANCE - POSTING IS ABORTED. Hit any key
	!	to continue"\*, will appear on the screen. Any imbalance must
	!	be corrected before the posting can be continued. The corrections
	!	will be made in the Cash Receipts Journal Maintenance routine.
	!	.b
	!	After the posting is complete, the system will return to the
	!	Cash Receipts Journal menu screen.
	!	.lm -5
	!
	! Index:
	!	.x Post>Cash Receipts Journal
	!	.x Cash Receipts Journal>Post
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_POST_CRJ/LINE
	!	$ LINK/EXECUTABLE=AR_EXE: AR_POST_CRJ, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_POST_CRJ.OBJ;*
	!
	! Author:
	!
	!	06/27/89 - Aaron Redd
	!
	! Modification history:
	!
	!	10/03/89 - Kevin Handy
	!		Modified to try to remove possiblilty of NULL
	!		descriptions.
	!
	!	12/10/90 - Frank F. Starman
	!		Use description from journal header (if there is any)
	!		in posting to GL ledger description field.
	!
	!	05/03/91 - J. Shad Rydalch
	!		Removed all calls to SB_TRAN_POST since those were
	!		moved into GL_TRAN_POSTGL.
	!
	!	05/17/91 - Frank F. Starman
	!		Use always description from journal header if type is
	!		not 09.
	!
	!	08/06/91 - Craig Tanner
	!		Declare CHECK_PERIOD so that post will check dates.
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	02/05/93 - Kevin Handy
	!		Added AR_OPEN::DUEDATE and AR_OPEN::DISCOUNTDATE.
	!
	!	04/28/93 - Kevin Handy
	!		Cleaned out excessive indentation so I could see
	!		right side of program.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/20/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/21/96 - Kevin Handy
	!		Modifications to post line type "3" as
	!		header type "11"
	!
	!	07/30/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
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

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJH.HB"
	MAP (AR_CRJH)		AR_CRJH_CDD	AR_CRJH

	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.HB"
	MAP (AR_CRJL)		AR_CRJL_CDD	AR_CRJL

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION	AR_TRAN_POSTAR
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES
	EXTERNAL LONG	FUNCTION	SB_EXAM_SUBACCOUNT

	!
	! Declare internal variables
	!
	DECLARE AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM
	DECLARE AR_OPEN_CDD		AR_OPEN
	DECLARE	GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE	LONG			PRNT_SUMMARY
	DECLARE LONG			CHECK_PERIOD

	DECLARE	STRING			BATCH_NUMBER
	DECLARE	STRING			GLPERIOD
	DECLARE	STRING			AR.INTER.PERIOD
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
	TITLE(1%) = "CASH  RECEIPTS  JOURNAL  POSTING  PROTOCOL"
	TITLE(2%) = "Accounts Receivable System"
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
	!	^*(01) Post Period\*
	!	.b
	!	.lm +5
	!	The ^*Post Period\* field enters the
	!	accounting period to which the Cash Receipts records are to be
	!	posted.
	!	.B
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

	BATCH_NO$ = TRM$(UTL_REPORTX::OPTDEF(1%))
	!++
	! Abstract:FLD02
	!	^*(02) Batch to Post\*
	!	.b
	!	.lm +5
	!	The ^*Batch to Post\* field is to be entered with the batch
	!	_# of the Cash Receipts Journal which is to be posted.
	!	.b
	!	Only one batch at a time may be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	CHECK_DATE$ = TRM$(UTL_REPORTX::OPTDEF(2%))
	!++
	! Abstract:FLD03
	!	^*(03) Check Dates\*
	!	.b
	!	.lm +5
	!	The ^*Check Dates\* field checks all dates
	!	of the entries in the journal to insure they are within a particular
	!	date range (accounting period) for posting.
	!	.b
	!	By entering "^*N\*" for No the dates will not be checked and all
	!	entries will be posted.  A "^*Y\*" for Yes entry causes all dates to
	!	be checked.  If "Y" is entered and dates are found that are not within
	!	the date range for posting, the posting will be aborted.
	!	.lm -5
	!
	! Index:
	!	.x Check Date>Post Cash Receipts Journal
	!	.x Post Cash Receipts Journal>Check Date
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF (CHECK_DATE$ = "Y")

310	!
	! Open journal header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CRJH.UPD"
	USE
		FILENAME$ = "AR_CRJH"
		CONTINUE HelpError
	END WHEN

320	!
	! Open journal line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.UPD"
	USE
		FILENAME$ = "AR_CRJL"
		CONTINUE HelpError
	END WHEN

	%PAGE

	!******************************************************************
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
		UTL_REPORTX, "AR_CRJH", BATCH_NO$, &
		GL.INTER.PERIOD, AR.INTER.PERIOD)

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
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"", "", GL.INTER.PERIOD) <> CMC$_NORMAL
		END IF

		IF TRM$(AR.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				AR_TRAN_POSTAR(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"", "", "", AR.INTER.PERIOD) <> CMC$_NORMAL
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
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AR_CRJH", BATCH_NO$, &
		GLPERIOD, "") <> CMC$_NORMAL

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

	EXIT_STATUS = AR_TRAN_POSTAR(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", "", GLPERIOD)

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
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AR_CRJH", BATCH_NO$, &
		"", "") <> CMC$_NORMAL

	RESET #AR_CRJH.CH%

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	!
	! Blank flags
	!
	CUSTNUM$, HDRACCT$, TRANDAT$ = " "

 ReadHeader:
	!
	! Read in one record from the header file
	!
3000	WHEN ERROR IN
		GET #AR_CRJH.CH%
	USE
		!
		! Locked block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE Confirm IF ERR = 11%
		FILENAME$ = "AR_CRJH"
		CONTINUE HelpError
	END WHEN

	!
	! Set some initial variable values
	!
	PRINTHEAD% = 0%

	!
	! Get the customer description
	!
	EXIT_STATUS = AR_EXAM_CUSTOM(AR_CRJH::CUSNUM, AR_35CUSTOM_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		CUSTNUM$ = "*" IF (AR_CRJH::TRATYP <> "10")

	!
	! SNAFU:  Situation Normal - All Fouled Up
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! See if CRJ Header Account number is defined
	!
	EXIT_STATUS = GL_EXAM_CHART(AR_CRJH::ACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set a flag and go on
	!
	CASE CMC$_UNDEFINED
		HDRACCT$ = "*"

	!
	! SNAFU:  (Situation Normal - it's All Fouled Up)
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

 ProcessLines:
	!
	! Find the first line item for the header
	!
3100	WHEN ERROR IN
		FIND #AR_CRJL.CH%, KEY #0% EQ AR_CRJH::RECNUM
	USE
		!
		! Locked block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader
	END WHEN

	LINACCT$, SUBACCT$ = " "

 LineItem:
	!
	! Get the (next) line item
	!
3200	WHEN ERROR IN
		GET #AR_CRJL.CH%
	USE
		!
		! Locked block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader IF ERR = 11%
		FILENAME$ = "AR_CRJL"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the header if we're done with the line items
	!
	GOTO ProcessHeader IF AR_CRJL::RECNUM <> AR_CRJH::RECNUM

	!
	! Generate a GL record to pass through to the post function
	!
	IF EDIT$(AR_CRJH::DESCR, -1%) = "" AND AR_CRJH::TRATYP = "09"
	THEN
		GL_YYYY_PP::DESCR = AR_35CUSTOM_EXAM::CUSNAM
	ELSE
		GL_YYYY_PP::DESCR = AR_CRJH::DESCR
	END IF
	GL_YYYY_PP::ACCT	= AR_CRJL::ACCT
	GL_YYYY_PP::SOURCE	= "CRJ"
	GL_YYYY_PP::REFNO	= AR_CRJL::INVNUM
	GL_YYYY_PP::TRANDAT	= AR_CRJH::TRADAT
	GL_YYYY_PP::AMOUNT	= AR_CRJL::AMOUNT
	GL_YYYY_PP::XREFNO	= AR_CRJH::CUSNUM
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::CKNO	= AR_CRJH::DEPOSIT
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= AR_CRJL::SALNUM
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	! See if GL Chart number is defined
	!
	EXIT_STATUS = GL_EXAM_CHART(AR_CRJL::ACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set a flag and go on
	!
	CASE CMC$_UNDEFINED
		LINACCT$ = "*"

	!
	! SNAFU:  (Situation Normal - it's All Fouled Up)
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Put the record into the temporary file
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, "", TITLE(), &
		UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, GLPERIOD)

	!
	! Check date; is it in the correct period?
	!
	SELECT EXIT_STATUS
	!
	! Date correct; go on
	!
	CASE CMC$_NORMAL

	CASE CMC$_WARNING
		!
		! See if salesman number is defined
		!
		EXIT_STATUS = SB_EXAM_SUBACCOUNT("S", AR_CRJL::SALNUM, &
			SB_SUBACCOUNT_EXAM)

		SELECT EXIT_STATUS
		!
		! Success; go on
		!
		CASE CMC$_NORMAL

		!
		! Undefined; Set a flag and go on
		!
		CASE CMC$_UNDEFINED
			SUBACCT$ = "*"

		!
		! Somethings not right
		!
		CASE ELSE
			GOTO Aborted
		END SELECT

	!
	! Date out of range
	!
	CASE CMC$_DATEOUT
		TRANDAT$ = "*"

	!
	! Odd happenings
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Types "2" and "10" records are posted to GL, but not to AR.
	!
	GOTO ChkUndef IF (AR_CRJH::TRATYP = "10") OR (AR_CRJL::TRATYP = "2")

	!
	! Generate an AR_OPEN record to pass through to the post function
	!
	AR_OPEN::CUSNUM		= AR_CRJH::CUSNUM
	AR_OPEN::INVNUM		= AR_CRJL::INVNUM
	IF AR_CRJL::TRATYP = "3"
	THEN
		AR_OPEN::TRATYP		= "11"
	ELSE
		AR_OPEN::TRATYP		= AR_CRJH::TRATYP
	END IF
	AR_OPEN::TRADAT		= AR_CRJH::TRADAT
	AR_OPEN::SALAMT		= AR_CRJL::AMOUNT
	AR_OPEN::DISAMT		= 0.0
	AR_OPEN::OTHCHG		= 0.0
	AR_OPEN::RECNUM		= AR_CRJH::RECNUM
	AR_OPEN::CHKNUM		= AR_CRJH::CHECK
	AR_OPEN::ARACCT		= AR_CRJL::ACCT
	AR_OPEN::SUBACC		= ""
	AR_OPEN::SALNUM		= AR_CRJL::SALNUM
	AR_OPEN::DESCR		= EDIT$(AR_CRJH::DESCR, 4%)
	AR_OPEN::BATCH		= BATCH_NUMBER
	AR_OPEN::UPDATED	= GLPERIOD + "00"
	AR_OPEN::CLOSEDATE	= ""
	AR_OPEN::DUEDATE	= ""
	AR_OPEN::DISCOUNTDATE	= ""


	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_OPEN, "", "", GLPERIOD) <> CMC$_NORMAL

 ChkUndef:
	!
	! Was the account number undefined?
	!
	IF LINACCT$ = "*" OR SUBACCT$ = "*"
	THEN
		!
		! Print out the header if it hasn't already been
		!
		IF PRINTHEAD% = 0%
		THEN
			!
			! Put the header line in TEXT
			!
			TEXT = "HEADER " + AR_CRJH::RECNUM + " " + &
				CUSTNUM$ + AR_CRJH::CUSNUM + " " + &
				HDRACCT$ + AR_CRJH::ACCT + " " + &
				TRANDAT$ + PRNT_DATE(AR_CRJH::TRADAT, 8%)

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, &
				TEXT) <> CMC$_NORMAL

			PRINTHEAD% = -1%

		END IF

		!
		! Put the invalid stuff in TEXT to send to OUTP_UNDEFCODES
		!
		TEXT = "  LINE                     " + &
			LINACCT$ + AR_CRJL::ACCT + SPACE$(12%) + &
			SUBACCT$ + AR_CRJL::SALNUM

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, &
				TEXT) <> CMC$_NORMAL

		!
		! Blank the flag
		!
		LINACCT$, SUBACCT$ = " "

	END IF

	GOTO LineItem

 ProcessHeader:
	!
	! Process the header
	!

3300	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::REFNO = AR_CRJH::RECNUM
	IF EDIT$(AR_CRJH::DESCR, -1%) = "" AND AR_CRJH::TRATYP = "09"
	THEN
		GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM
	ELSE
		GL_YYYY_PP::DESCR	= AR_CRJH::DESCR
	END IF
	GL_YYYY_PP::XREFNO	= AR_CRJH::CUSNUM
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= ""
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER
	GL_YYYY_PP::ACCT	= AR_CRJH::ACCT
	GL_YYYY_PP::SOURCE	= "CRJ"
	GL_YYYY_PP::TRANDAT	= AR_CRJH::TRADAT
	GL_YYYY_PP::AMOUNT	= AR_CRJH::AMNT
	GL_YYYY_PP::CKNO	= AR_CRJH::DEPOSIT

	!
	! Define AR Account number
	!
	GOTO Aborted IF &
		GL_EXAM_CHART(AR_CRJH::ACCT, GL_CHART_EXAM) <> CMC$_NORMAL

	!
	! Put the record into the temporary file
	!
	GOTO Aborted IF GL_TRAN_POSTGL(OPT_ADDREC, SUBOPT_NOOPT, "", &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, &
		GL_CHART_EXAM, GLPERIOD) <> CMC$_NORMAL

	!
	! Was anything undefined?
	!
	IF INSTR(1%, CUSTNUM$ + HDRACCT$, "*") AND (PRINTHEAD% = 0%)
	THEN
		!
		! Put the invalid stuff in TEXT to send to OUTP_UNDEFCODES
		!
		TEXT = "HEADER " + AR_CRJH::RECNUM + " " + &
			CUSTNUM$ + AR_CRJH::CUSNUM + " " + &
			HDRACCT$ + AR_CRJH::ACCT + " " + &
			TRANDAT$ + PRNT_DATE(AR_CRJH::TRADAT, 8%)

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, &
				TEXT) <> CMC$_NORMAL

		!
		! Blank flags
		!
		CUSTNUM$, HDRACCT$, TRANDAT$ = " "

	END IF

	GOTO ReadHeader

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
	GOTO Interrupt IF &
		GL_TRAN_POSTGL(OPT_POSTFILE, SUBOPT_DETAIL, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD) <> CMC$_NORMAL

	!
	! Post the AR Cash Receipts Journal
	!
	GOTO Interrupt IF &
		AR_TRAN_POSTAR(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "", GLPERIOD) <> CMC$_NORMAL

	%PAGE

	!
	! Remove files
	!
5000	CLOSE AR_CRJH.CH%
	CLOSE AR_CRJL.CH%

5010	SMG_STATUS% = LIB$DELETE_FILE(AR_CRJH.DEV$ + "AR_CRJH_" + &
		BATCH_NO$ + ".JRL;*")

5020	SMG_STATUS% = LIB$DELETE_FILE(AR_CRJL.DEV$ + "AR_CRJL_" + &
		BATCH_NO$ + ".JRL;*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AR_CRJH", BATCH_NO$, "", "")

	PRNT_SUMMARY = SUBOPT_FINAL

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
	TEXT = "Item   Receipt Customer   AR Account Number   TransDate  " + &
		"Salesman #"

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

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************
	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "AR_CRJH", &
			BATCH_NO$, "", "")
		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AR_CRJH", BATCH_NO$, "", "")
	GOTO ExitProgram

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

32000	END
