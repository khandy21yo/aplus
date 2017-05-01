1	%TITLE "Accounts Receivable Sales Journal Post Program"
	%SBTTL "AR_POST_FJTAB"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2003 BY
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:SJPST
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Post Sales Journal\* option is provided to post entries
	!	which have been made in the Sales Journal to the General
	!	Ledger System and the Accounts Receivable Register.
	!	.b
	!	If the entries being posted are out of balance, the message,
	!	^*"Batch is OUT OF BALANCE - POSTING IS ABORTED. Hit any key
	!	to continue"\*, will appear on the screen. Any imbalance must
	!	be corrected before the posting can be executed. Corrections
	!	are made in the Sales Journal Maintenance routine.
	!	.b
	!	After the posting is completed, the system will return to the
	!	Sales Journal Menu screen.
	!	.lm -5
	!
	! Index:
	!	.x Post>Sales Journal
	!	.x Sales Journal>Post
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_POST_FJTAB/LINE
	!	$ LINK/EXE=AR_EXE: AR_POST_FJTAB, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_POST_FJTAB.OBJ;*
	!
	! Author:
	!
	!	07/17/2003 - Kevin Handy
	!
	! Modification history:
	!
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
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.HB"
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
	DECLARE AR_OPEN_DIST_CDD	AR_OPEN_DIST
	DECLARE AR_SALTAXLED_CDD	AR_SALTAXLED
	DECLARE	GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM
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
	TITLE(1%) = "SALES  JOURNAL  POSTING  PROTOCOL"
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
	!	.x Post Period>Post Sales Journal
	!	^*(01) Post Period\*
	!	.b
	!	.lm +5
	!	The ^*Post Period\* field determines
	!	the year and the accounting period corresponding to which the
	!	Sales Journal records are to be posted in the General Ledger.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Post Sales Journal>Post Period
	!
	!--
	BATCH_NO$ = TRM$(UTL_REPORTX::OPTDEF(1%))
	!++
	! Abstract:FLD02
	!	^*(02) Batch to Post\*
	!	.b
	!	.lm +5
	!	The ^*Batch to Post\* field allows for entry of the batch
	!	_# of the Sales Journal which is to be posted.
	!	.b
	!	Only one batch at a time may be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch to Post>Post Sales Journal
	!	.x Post Sales Journal>Batch to Post
	!
	!--
	CHECK_DATE$ = TRM$(UTL_REPORTX::OPTDEF(2%))
	!++
	! Abstract:FLD03
	!	^*(03) Check Dates\*
	!	.b
	!	.lm +5
	!	The ^*Check Dates\* allows for the option of checking the date of all
	!	transactions before posting.  If ^*Y\* is entered, the dates of all
	!	transactions will
	!	be checked to insure they are within the specified accounting period.  If
	!	Y is entered and dates are found that are not within the date range for
	!	posting, the posting will be aborted.  If
	!	^*N\* is entered, the dates will not be checked
	!	and all transactions will be posted.
	!	.lm -5
	!
	! Index:
	!	.x Check Dates>Post Sales Journal
	!	.x Post Sales Journal>Check Dates
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF (CHECK_DATE$ = "Y")

300	!
	! Open Accounts Receivable SJ header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.UPD"
	USE
		FILENAME$ = "AR_SJH"
		CONTINUE HelpError
	END WHEN

310	!
	! Open AR Sales Journal line items file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.UPD"
	USE
		FILENAME$ = "AR_SJL"
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
		UTL_REPORTX, "AR_SJH", BATCH_NO$, &
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

 AssignBatch:
	!******************************************************************
	!	1) Assign a batch number
	!	2) Make sure no legitimate records in the ledger already
	!		have this batch number; if any records do have
	!		this newly assigned number, go back to (1) and
	!		get a new one.
	!******************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AR_SJH", BATCH_NO$, &
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
		UTL_REPORTX, "AR_SJH", BATCH_NO$, &
		"", "") <> CMC$_NORMAL

	RESET #AR_SJH.CH%

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	!
	! Blank flags
	!
	CUSTNUM$, ARACCT$, TRANDAT$ = " "

 ReadHeader:
	!
	! Read in one record from the header file
	!
3000	WHEN ERROR IN
		GET #AR_SJH.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE Confirm
	END WHEN

	!
	! Set some initial variable values
	!
	TOTAL.DISCOUNT, TOTAL.OTHER = 0.0
	PRINTHEAD% = 0%

	!
	! Get the customer description
	!
	EXIT_STATUS = AR_EXAM_CUSTOM(AR_SJH::CUSNUM, AR_35CUSTOM_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		CUSTNUM$ = "*"

	!
	! SNAFU:  Situation Normal - All Fouled Up
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! See if AR Account number is defined
	!
	EXIT_STATUS = GL_EXAM_CHART(AR_SJH::ARACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set a flag and go on
	!
	CASE CMC$_UNDEFINED
		ARACCT$ = "*"

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
		FIND #AR_SJL.CH%, KEY #0% EQ AR_SJH::INVNUM
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader
	END WHEN

 LineItem:
	!
	! Get the (next) line item
	!
3200	WHEN ERROR IN
		GET #AR_SJL.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader IF ERR = 11%
		FILENAME$ = "AR_SJL"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the header if we're done with the line items
	!
	GOTO ProcessHeader IF AR_SJL::INVNUM <> AR_SJH::INVNUM

	!
	! Summarize amounts
	!
	SELECT AR_SJL::LTYPE

	CASE "D"
		TOTAL.DISCOUNT = TOTAL.DISCOUNT + AR_SJL::AMOUNT

	CASE "O", "F", "T"
		TOTAL.OTHER = TOTAL.OTHER + AR_SJL::AMOUNT

	CASE "H"
		!
		! *** This is the Wizzo Zowie Fako method of making
		! a line item act as a header, so that multiple due
		! dates can be entered.
		! With the "H" type, the description becomes a due
		! date, the account is the AR/ACCOUNT, and the amount
		! is the invoice amount for that date. All other fields
		! are happily ignored.
		!

		!
		! Generate a GL record to pass through to the post function
		!
		GL_YYYY_PP::REFNO	= AR_SJH::INVNUM
		GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM
		GL_YYYY_PP::XREFNO	= AR_SJH::CUSNUM
		GL_YYYY_PP::POSTIM	= POSTTIME
		GL_YYYY_PP::POSDAT	= POSTDATE
		GL_YYYY_PP::TRANKEY	= ""
		GL_YYYY_PP::SUBACC	= AR_SJH::SUBACCT
		GL_YYYY_PP::OPERATION	= ""
		GL_YYYY_PP::UNITS	= 0.0
		GL_YYYY_PP::HOURS	= 0.0
		GL_YYYY_PP::UPDSTA	= ""
		GL_YYYY_PP::BTHNUM	= BATCH_NUMBER
		GL_YYYY_PP::ACCT	= AR_SJL::ACCT
		GL_YYYY_PP::SOURCE	= "SJ"
		GL_YYYY_PP::TRANDAT	= AR_SJH::TRADAT
		GL_YYYY_PP::AMOUNT	= AR_SJL::AMOUNT
		GL_YYYY_PP::CKNO	= AR_SJH::DEPOSIT
		GL_YYYY_PP::TRANKEY	= ""

		!
		! Define AR Account number
		!
		GOTO Aborted IF &
			GL_EXAM_CHART(AR_SJL::ACCT, GL_CHART_EXAM) <> &
			CMC$_NORMAL

		!
		! Put the record into the temporary file
		!
		GOTO Aborted IF GL_TRAN_POSTGL(OPT_ADDREC, SUBOPT_NOOPT, "", &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, GLPERIOD) <> CMC$_NORMAL

		!
		! Generate a AR record to pass through to the post function
		!
		AR_OPEN::CUSNUM		= AR_SJH::CUSNUM
		AR_OPEN::INVNUM		= AR_SJH::INVNUM
		AR_OPEN::TRATYP		= AR_SJH::TRATYP
		AR_OPEN::TRADAT		= AR_SJH::TRADAT
		AR_OPEN::SALAMT		= AR_SJL::AMOUNT
		AR_OPEN::DISAMT		= 0.0
		AR_OPEN::OTHCHG		= 0.0
		AR_OPEN::RECNUM		= AR_SJH::RECNUM
		AR_OPEN::CHKNUM		= AR_SJH::CHECK
		AR_OPEN::ARACCT		= AR_SJH::ARACCT
		AR_OPEN::SUBACC		= ""
		AR_OPEN::SALNUM		= AR_35CUSTOM_EXAM::SALESMAN
		AR_OPEN::DESCR		= AR_SJH::DESCR
		AR_OPEN::BATCH		= BATCH_NUMBER
		AR_OPEN::UPDATED	= GLPERIOD + "00"
		AR_OPEN::CLOSEDATE	= ""
		AR_OPEN::DUEDATE	= AR_SJL::DESCR
		AR_OPEN::DISCOUNTDATE	= AR_SJH::DISCOUNTDATE


		!
		! Call the post function
		!
		GOTO Aborted IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_REGISTER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			AR_OPEN, AR_OPEN_DIST, AR_SALTAXLED, GLPERIOD) <> &
			CMC$_NORMAL

		!
		! Was anything undefined?
		!
		IF INSTR(1%, CUSTNUM$+ARACCT$+TRANDAT$, "*") AND &
			(PRINTHEAD% = 0%)
		THEN
			!
			! Put the invalid stuff in TEXT to send to
			! OUTP_UNDEFCODES
			!
			TEXT = "HEADER " + AR_SJH::INVNUM + " " + &
				CUSTNUM$ + AR_SJH::CUSNUM + " " + &
				ARACCT$ + AR_SJH::ARACCT + " " + &
				TRANDAT$ + PRNT_DATE(AR_SJH::TRADAT, 8%)

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, &
				TEXT) <> CMC$_NORMAL

			!
			! Blank flags
			!
			CUSTNUM$, ARACCT$, TRANDAT$ = " "

		END IF

		GOTO LineItem
	END SELECT

	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::ACCT	= AR_SJL::ACCT
	GL_YYYY_PP::SOURCE	= "SJ"
	GL_YYYY_PP::REFNO	= AR_SJH::INVNUM
	GL_YYYY_PP::TRANDAT	= AR_SJH::TRADAT
	IF (EDIT$(AR_SJL::DESCR, -1%) = "")
	THEN
		GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM
	ELSE
		GL_YYYY_PP::DESCR	= AR_SJL::DESCR
	END IF
	GL_YYYY_PP::AMOUNT	= AR_SJL::AMOUNT
	GL_YYYY_PP::XREFNO	= AR_SJH::CUSNUM
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::CKNO	= AR_SJH::DEPOSIT
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= AR_SJL::SUBACCT
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= AR_SJL::QTY
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	! See if GL Chart number is defined
	!
	EXIT_STATUS = GL_EXAM_CHART(AR_SJL::ACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set a flag and go on
	!
	CASE CMC$_UNDEFINED
		ACCTNUM$ = "*"

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
	! Check the date; is it out of range?
	!
	SELECT EXIT_STATUS

	!
	! Date is correct; go on
	!
	CASE CMC$_NORMAL

	!
	! Warning; check subaccount number
	!
	CASE CMC$_WARNING
		!
		! See if Sub Account number is defined
		!
		EXIT_STATUS = SB_EXAM_SUBACCOUNT("J", &
			AR_SJL::SUBACCT, SB_SUBACCOUNT_EXAM)

		SELECT EXIT_STATUS

		!
		! Success; go on
		!
		CASE CMC$_NORMAL

		!
		! Undefined; set a flag and go on
		!
		CASE CMC$_UNDEFINED
			SUBACCT$ = "*"

		CASE ELSE
			GOTO Aborted

		END SELECT

	!
	! Date is not in the correct period
	!
	CASE CMC$_DATEOUT
		TRANDAT$ = "*"

	!
	! Something else going wrong
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Generate a AR_OPEN_DIST record to pass through to the
	! post function
	!
	AR_OPEN_DIST::INVNUM		= AR_SJL::INVNUM
	AR_OPEN_DIST::CUSNUM		= AR_SJH::CUSNUM
	AR_OPEN_DIST::SLINE		= AR_SJL::SLINE
	AR_OPEN_DIST::ACCT		= AR_SJL::ACCT
	AR_OPEN_DIST::SUBACCT		= AR_SJL::SUBACCT
	AR_OPEN_DIST::AMOUNT		= AR_SJL::AMOUNT
	AR_OPEN_DIST::QTY		= AR_SJL::QTY
	AR_OPEN_DIST::LTYPE		= AR_SJL::LTYPE
	AR_OPEN_DIST::TAXTYP		= AR_SJL::TAXTYP
	AR_OPEN_DIST::DESCR		= AR_SJL::DESCR
	AR_OPEN_DIST::BATCH		= BATCH_NUMBER
	AR_OPEN_DIST::STAFF_NUM		= ""
	AR_OPEN_DIST::POST_DATE		= DATE_TODAY
	AR_OPEN_DIST::POST_TIME		= POSTTIME

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_OPEN, AR_OPEN_DIST, AR_SALTAXLED, GLPERIOD) <> CMC$_NORMAL

	!
	! Post to sales tax file if correct type
	!
	IF AR_SJL::LTYPE = "S"
	THEN
		AR_SALTAXLED::TAXTYP = AR_SJL::TAXTYP
		AR_SALTAXLED::CUSNUM = AR_SJH::CUSNUM
		AR_SALTAXLED::INVNUM = AR_SJL::INVNUM
		AR_SALTAXLED::AMOUNT = -AR_SJL::AMOUNT
		AR_SALTAXLED::BATCH  = BATCH_NUMBER
		AR_SALTAXLED::TRADAT = AR_SJH::TRADAT

		GOTO Aborted IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_LEDGER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			AR_OPEN, AR_OPEN_DIST, AR_SALTAXLED, GLPERIOD) <> &
			CMC$_NORMAL

	END IF

 GoToNext:
	!
	! Was the account number undefined?
	!
	IF (ACCTNUM$ = "*") OR (SUBACCT$ = "*")
	THEN
		!
		! Print out the header if it hasn't already been
		!
		IF PRINTHEAD% = 0%
		THEN
			!
			! Put the header line in TEXT
			!
			TEXT = "HEADER " + AR_SJH::INVNUM + " " + &
				CUSTNUM$ + AR_SJH::CUSNUM + " " + &
				ARACCT$ + AR_SJH::ARACCT + " " + &
				TRANDAT$ + PRNT_DATE(AR_SJH::TRADAT, 8%)

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
		TEXT = "  LINE                              " + &
			ACCTNUM$ + AR_SJL::ACCT + "           " + &
			SUBACCT$ + AR_SJL::SUBACCT

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, &
				TEXT) <> CMC$_NORMAL

		!
		! Blank flags
		!
		ACCTNUM$, SUBACCT$ = " "

	END IF

	GOTO LineItem

 ProcessHeader:
	!
	! Process the header
	!

3300	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::REFNO	= AR_SJH::INVNUM
	IF (EDIT$(AR_SJL::DESCR, -1%) = "")
	THEN
		GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM
	ELSE
		GL_YYYY_PP::DESCR	= AR_SJL::DESCR
	END IF
	GL_YYYY_PP::XREFNO	= AR_SJH::CUSNUM
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= AR_SJH::SUBACCT
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER
	GL_YYYY_PP::ACCT	= AR_SJH::ARACCT
	GL_YYYY_PP::SOURCE	= "SJ"
	GL_YYYY_PP::TRANDAT	= AR_SJH::TRADAT
	GL_YYYY_PP::AMOUNT	= AR_SJH::AMOUNT
	GL_YYYY_PP::CKNO	= AR_SJH::DEPOSIT
	GL_YYYY_PP::TRANKEY	= ""

	!
	! Define AR Account number
	!
	GOTO Aborted IF &
		GL_EXAM_CHART(AR_SJH::ARACCT, GL_CHART_EXAM) <> CMC$_NORMAL

	!
	! Put the record into the temporary file
	!
	GOTO Aborted IF GL_TRAN_POSTGL(OPT_ADDREC, SUBOPT_NOOPT, "", &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, &
		GL_CHART_EXAM, GLPERIOD) <> CMC$_NORMAL

	!
	! Generate a AR record to pass through to the post function
	!
	AR_OPEN::CUSNUM		= AR_SJH::CUSNUM
	AR_OPEN::INVNUM		= AR_SJH::INVNUM
	AR_OPEN::TRATYP		= AR_SJH::TRATYP
	AR_OPEN::TRADAT		= AR_SJH::TRADAT
	AR_OPEN::SALAMT		= AR_SJH::AMOUNT
	AR_OPEN::DISAMT		= -TOTAL.DISCOUNT
	AR_OPEN::OTHCHG		= -TOTAL.OTHER
	AR_OPEN::RECNUM		= AR_SJH::RECNUM
	AR_OPEN::CHKNUM		= AR_SJH::CHECK
	AR_OPEN::ARACCT		= AR_SJH::ARACCT
	AR_OPEN::SUBACC		= ""
	AR_OPEN::SALNUM		= AR_35CUSTOM_EXAM::SALESMAN
	AR_OPEN::DESCR		= AR_SJH::DESCR
	AR_OPEN::BATCH		= BATCH_NUMBER
	AR_OPEN::UPDATED	= GLPERIOD + "00"
	AR_OPEN::CLOSEDATE	= ""
	AR_OPEN::DUEDATE	= AR_SJH::DUEDATE
	AR_OPEN::DISCOUNTDATE	= AR_SJH::DISCOUNTDATE

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_OPEN, AR_OPEN_DIST, AR_SALTAXLED, GLPERIOD) <> CMC$_NORMAL

	!
	! Was anything undefined?
	!
	IF INSTR(1%, CUSTNUM$+ARACCT$+TRANDAT$, "*") AND (PRINTHEAD% = 0%)
	THEN
		!
		! Put the invalid stuff in TEXT to send to OUTP_UNDEFCODES
		!
		TEXT = "HEADER " + AR_SJH::INVNUM + " " + &
			CUSTNUM$ + AR_SJH::CUSNUM + " " + &
			ARACCT$ + AR_SJH::ARACCT + " " + &
			TRANDAT$ + PRNT_DATE(AR_SJH::TRADAT, 8%)

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, &
				TEXT) <> CMC$_NORMAL

		!
		! Blank flags
		!
		CUSTNUM$, ARACCT$, TRANDAT$ = " "

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
	! Post the AR Sales Journal header
	!
	GOTO Interrupt IF &
		AR_TRAN_POSTAR(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "", GLPERIOD) <> CMC$_NORMAL

	!
	! Continue by posting the Sales Journal line items
	!
	GOTO Interrupt IF &
		AR_TRAN_POSTAR(OPT_POSTFILE, SUBOPT_LINEITEM, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "", GLPERIOD) <> CMC$_NORMAL

	!
	! Post any Sales Tax items
	!
	GOTO Interrupt IF &
		AR_TRAN_POSTAR(OPT_POSTFILE, SUBOPT_LEDGER, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "", GLPERIOD) <> CMC$_NORMAL

	%PAGE

	!
	! Remove files
	!
5000	CLOSE AR_SJH.CH%
	CLOSE AR_SJL.CH%

5010 !	WHEN ERROR IN
 !		KILL AR_SJH.DEV$ + "AR_SJH_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5020
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AR_SJH.DEV$ + "AR_SJH_" + &
		BATCH_NO$ + ".JRL;*")

5020 !	WHEN ERROR IN
 !		KILL AR_SJL.DEV$ + "AR_SJL_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE Complete
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AR_SJL.DEV$ + "AR_SJL_" + &
		BATCH_NO$ + ".JRL;*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AR_SJH", BATCH_NO$, "", "")

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
	TEXT = "Item   Invoice CustomerNum AR AccountNum       " + &
		"TransDate SubAcct"

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
			TITLE(), UTL_REPORTX, "AR_SJH", &
			BATCH_NO$, "", "")
		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AR_SJH", BATCH_NO$, "", "")

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

	%PAGE

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

	%PAGE

32000	!******************************************************************
	! End of posting program AR_POST_FJTAB
	!******************************************************************
	END
