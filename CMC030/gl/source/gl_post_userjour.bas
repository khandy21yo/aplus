1	%TITLE "General Ledger User Journal Post Program"
	%SBTTL "GL_POST_USERJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1998 BY
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
	!	.x Post>User Journal
	!	.x User Journal>Post
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_POST_USERJOUR/LINE
	!	$ LINK/EXE=GL_EXE: GL_POST_USERJOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_POST_USERJOUR.OBJ;*
	!
	! Author:
	!
	!	11/24/98 - Kevin Handy
	!
	! Modification history:
	!
	!	12/04/98 - Kevin Handy
	!		Post AP entries as paid, not invoices.
	!
	!	12/08/98 - Kevin Handy
	!		Post customer/vendor number into subaccount (FJ).
	!
	!	12/14/1998 - Kevin Handy
	!		Modifications to get a better transaction key for
	!		the created transactions.
	!
	!	08/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/15/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[GL.OPEN]GL_USERHEAD.HB"
	MAP (GL_USERHEAD)	GL_USERHEAD_CDD	GL_USERHEAD

	%INCLUDE "SOURCE:[GL.OPEN]GL_USERJOUR.HB"
	MAP (GL_USERJOUR)	GL_USERJOUR_CDD	GL_USERJOUR

	%INCLUDE "SOURCE:[GL.OPEN]GL_USERDEF.HB"
	MAP	(GL_USERDEF)	GL_USERDEF_CDD	GL_USERDEF

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD		AP_CONTROL

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	AR_TRAN_POSTAR
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES
	EXTERNAL LONG	FUNCTION	AP_TRAN_POSTAP

	!
	! Declare internal variables
	!
	DECLARE AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM
	DECLARE AR_OPEN_CDD		AR_OPEN
	DECLARE AR_OPEN_DIST_CDD	AR_OPEN_DIST
	DECLARE AR_SALTAXLED_CDD	AR_SALTAXLED
	DECLARE	GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE	AP_OPEN_CDD		AP_OPEN
	DECLARE	AP_OPEN_DIST_CDD	AP_OPEN_DIST
	DECLARE	AP_VENDOR_CDD		AP_VENDOR_EXAM

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
		%INCLUDE "SOURCE:[GL.OPEN]GL_USERHEAD.UPD"
	USE
		FILENAME$ = "GL_USERHEAD"
		CONTINUE HelpError
	END WHEN

310	!
	! Open AR Sales Journal line items file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_USERJOUR.UPD"
	USE
		FILENAME$ = "GL_USERJOUR"
		CONTINUE HelpError
	END WHEN

320	!
	! Open AP control file
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.MOD"

340	!
	! User defined file
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_USERDEF.OPN"

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
		UTL_REPORTX, "GL_USERHEAD", BATCH_NO$, &
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
			GOTO Aborted IF &
				AP_TRAN_POSTAP(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, "", "", &
				AR.INTER.PERIOD) <> CMC$_NORMAL
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
		UTL_REPORTX, "GL_USERHEAD", BATCH_NO$, &
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
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "GL_USERHEAD", BATCH_NO$, &
		"", "") <> CMC$_NORMAL

	RESET #GL_USERHEAD.CH%

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
		GET #GL_USERHEAD.CH%
	USE
		!
		! Locked block
		!
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
	PRINTHEAD% = 0%

 ProcessLines:
	!
	! Find the first line item for the header
	!
3100	WHEN ERROR IN
		FIND #GL_USERJOUR.CH%, KEY #0% EQ GL_USERHEAD::JCODE
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

 LineItem:
	!
	! Get the (next) line item
	!
3200	WHEN ERROR IN
		GET #GL_USERJOUR.CH%
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
		FILENAME$ = "GL_USERJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the header if we're done with the line items
	!
	GOTO ProcessHeader IF GL_USERJOUR::JCODE <> GL_USERHEAD::JCODE

	!
	! Get definition of this line
	!
	GOSUB NeedDefine

	IF GL_USERDEF::SIGNED = "-"
	THEN
		SIGN = -1.0
	ELSE
		SIGN = 1.0
	END IF

	!
	! Summarize amounts
	!
	SELECT GL_USERDEF::ARPFLAG

	CASE "AR"
		!
		! Generate a AR record to pass through to the post function
		!
		AR_OPEN::CUSNUM		= GL_USERJOUR::XREF
		AR_OPEN::INVNUM		= GL_USERJOUR::INVNUM
		AR_OPEN::TRATYP		= "01"
		AR_OPEN::TRADAT		= GL_USERHEAD::JDATE
		AR_OPEN::SALAMT		= GL_USERJOUR::DOLLARS
		AR_OPEN::DISAMT		= 0.0
		AR_OPEN::OTHCHG		= 0.0
		AR_OPEN::RECNUM		= GL_USERHEAD::DEPOSIT
		AR_OPEN::CHKNUM		= ""
		AR_OPEN::ARACCT		= GL_USERJOUR::ACCOUNT
		AR_OPEN::SUBACC		= GL_USERHEAD::JCODE
		AR_OPEN::SALNUM		= AR_35CUSTOM_EXAM::SALESMAN
		AR_OPEN::DESCR		= GL_USERJOUR::DESCRIPTION
		AR_OPEN::BATCH		= BATCH_NUMBER
		AR_OPEN::UPDATED	= GLPERIOD + "00"
		AR_OPEN::CLOSEDATE	= ""
		AR_OPEN::DUEDATE	= GL_USERHEAD::JDATE
		AR_OPEN::DISCOUNTDATE	= GL_USERHEAD::JDATE


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
		IF INSTR(1%, CUSTNUM$ + ARACCT$ + TRANDAT$, "*") AND &
			(PRINTHEAD% = 0%)
		THEN
			!
			! Put the invalid stuff in TEXT to send to
			! OUTP_UNDEFCODES
			!
			TEXT = "HEADER " + GL_USERHEAD::JCODE + " " + &
				CUSTNUM$ + GL_USERJOUR::XREF + " " + &
				TRANDAT$ + PRNT_DATE(GL_USERHEAD::JDATE, 8%) + " " + &
				GL_USERJOUR::INVNUM + " " + &
				GL_USERJOUR::DESCRIPTION

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

		!
		! Generate a AR_OPEN_DIST record to pass through to the
		! post function
		!
		AR_OPEN_DIST::INVNUM		= GL_USERJOUR::INVNUM
		AR_OPEN_DIST::CUSNUM		= GL_USERJOUR::XREF
		AR_OPEN_DIST::SLINE		= GL_USERJOUR::JLINE
		AR_OPEN_DIST::ACCT		= GL_USERJOUR::ACCOUNT
		AR_OPEN_DIST::SUBACCT		= GL_USERJOUR::JCODE
		AR_OPEN_DIST::AMOUNT		= GL_USERJOUR::DOLLARS
		AR_OPEN_DIST::QTY		= GL_USERJOUR::UNITS
		AR_OPEN_DIST::LTYPE		= ""
		AR_OPEN_DIST::TAXTYP		= ""
		AR_OPEN_DIST::DESCR		= GL_USERJOUR::DESCRIPTION
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

	CASE "AP"
		!
		! Generate a AP_OPEN_DIST record to pass through to the
		! post function
		!
		AP_OPEN_DIST::TRANKEY		= GL_USERJOUR::INVNUM
		AP_OPEN::TRANKEY		= GL_USERJOUR::INVNUM
		GOSUB GetTrans

		AP_OPEN_DIST::SLINE		= "001"
		AP_OPEN_DIST::PONUM		= ""
		AP_OPEN_DIST::PO_LINE		= ""
		AP_OPEN_DIST::ACCT		= GL_USERJOUR::ACCOUNT
		AP_OPEN_DIST::SUBACC		= GL_USERJOUR::JCODE
		AP_OPEN_DIST::OPERATION		= ""
		AP_OPEN_DIST::UNITS		= GL_USERJOUR::UNITS
		AP_OPEN_DIST::AMOUNT		= GL_USERJOUR::DOLLARS
		AP_OPEN_DIST::DISCAMT		= 0.0
		AP_OPEN_DIST::USE_TAX_FLAG	= ""
		AP_OPEN_DIST::BTHNUM		= BATCH_NUMBER

		!
		! Put the record into the temporary file
		!
		GOTO Aborted &
			IF AP_TRAN_POSTAP(OPT_ADDREC, SUBOPT_LINEITEM, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, AP_OPEN, AP_OPEN_DIST, "") <> CMC$_NORMAL


		!
		! Generate a AP record to pass through to the post function
		!
		AP_OPEN::VENNUM		= GL_USERJOUR::XREF
		AP_OPEN::TRANKEY_DATE	= GL_USERHEAD::JDATE
		AP_OPEN::INVNUM		= GL_USERJOUR::INVNUM
		AP_OPEN::INVDAT		= GL_USERHEAD::JDATE
		AP_OPEN::INVAMT		= GL_USERJOUR::DOLLARS
		AP_OPEN::CODE_1099	= ""
		AP_OPEN::AMT_1099	= 0.0
		AP_OPEN::USE_JOB_NUM	= ""
		AP_OPEN::USE_AMT	= 0.0
		AP_OPEN::DISCDAT	= GL_USERHEAD::JDATE
		AP_OPEN::DISAMT		= 0.0
		AP_OPEN::DUEDAT		= GL_USERHEAD::JDATE
		AP_OPEN::PONUM		= ""
		AP_OPEN::AP_ACCT	= GL_USERJOUR::ACCOUNT
		AP_OPEN::CASH_ACCT	= ""
		AP_OPEN::CKNUM		= "CHECK"
		AP_OPEN::CKDAT		= GL_USERHEAD::JDATE
		AP_OPEN::CKDESC		= GL_USERJOUR::DESCRIPTION
		AP_OPEN::CKAMT		= GL_USERJOUR::DOLLARS
		AP_OPEN::UPDATED	= GLPERIOD + "00"
		AP_OPEN::CLOSEDATE	= ""
		AP_OPEN::SELECTED	= ""
		AP_OPEN::BATCH		= BATCH_NUMBER

		!
		! Put the AP Open record into the temporary file
		!
		GOTO Aborted IF AP_TRAN_POSTAP(OPT_ADDREC, SUBOPT_REGISTER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			AP_OPEN, AP_OPEN_DIST, "") <> CMC$_NORMAL

	END SELECT

	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::ACCT	= GL_USERJOUR::ACCOUNT
	GL_YYYY_PP::SOURCE	= "GU"
	GL_YYYY_PP::REFNO	= GL_USERJOUR::INVNUM
	GL_YYYY_PP::TRANDAT	= GL_USERHEAD::JDATE
	GL_YYYY_PP::DESCR	= GL_USERJOUR::DESCRIPTION
	GL_YYYY_PP::AMOUNT	= GL_USERJOUR::DOLLARS * SIGN
	GL_YYYY_PP::XREFNO	= GL_USERJOUR::XREF
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::CKNO	= GL_USERHEAD::DEPOSIT
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= GL_USERJOUR::JCODE
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= GL_USERJOUR::UNITS * SIGN
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	! See if GL Chart number is defined
	!
	EXIT_STATUS = GL_EXAM_CHART(GL_USERJOUR::ACCOUNT, GL_CHART_EXAM)

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
	CASE CMC$_NORMAL, CMC$_WARNING

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
			TEXT = "HEADER " + GL_USERHEAD::JCODE + " " + &
				CUSTNUM$ + GL_USERJOUR::XREF + " " + &
				ARACCT$ + GL_USERJOUR::ACCOUNT + " " + &
				TRANDAT$ + PRNT_DATE(GL_USERHEAD::JDATE, 8%) + " " + &
				GL_USERJOUR::INVNUM + " " + &
				GL_USERJOUR::DESCRIPTION

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
			ACCTNUM$ + GL_USERJOUR::ACCOUNT + "           " + &
			SUBACCT$

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

3300	GOTO ReadHeader

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

	!
	! Post the Header items
	!
	GOTO Interrupt IF &
		AP_TRAN_POSTAP(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD) <> CMC$_NORMAL

	!
	! Continue by posting the line items
	!
	GOTO Interrupt IF &
		AP_TRAN_POSTAP(OPT_POSTFILE, SUBOPT_LINEITEM, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD) <> CMC$_NORMAL

	%PAGE

	!
	! Remove files
	!
5000	CLOSE GL_USERHEAD.CH%
	CLOSE GL_USERJOUR.CH%

5010 !	WHEN ERROR IN
 !		KILL GL_USERHEAD.DEV$ + "GL_USERHEAD_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5020
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(GL_USERHEAD.DEV$ + "GL_USERHEAD_" + &
		BATCH_NO$ + ".JRL;*")

5020 !	WHEN ERROR IN
 !		KILL GL_USERJOUR.DEV$ + "GL_USERJOUR_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE Complete
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(GL_USERJOUR.DEV$ + "GL_USERJOUR_" + &
		BATCH_NO$ + ".JRL;*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "GL_USERHEAD", BATCH_NO$, "", "")

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
			TITLE(), UTL_REPORTX, "GL_USERHEAD", &
			BATCH_NO$, "", "")
		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "GL_USERHEAD", BATCH_NO$, "", "")

	GOTO ExitProgram

	%PAGE

 NeedDefine:
18100	!*******************************************************************
	! Dig through the userdef file for the proper definition for
	! this line
	!*******************************************************************

	IF GL_USERDEF::JCODE = GL_USERJOUR::JCODE AND &
		GL_USERDEF::JLINE = GL_USERJOUR::JLINE
	THEN
		RETURN
	END IF

	WHEN ERROR IN
		GET #GL_USERDEF.CH%, &
			KEY #0% EQ GL_USERJOUR::JCODE + GL_USERJOUR::JLINE, &
			REGARDLESS
	USE
		CONTINUE 18190
	END WHEN

18190	RETURN

	%PAGE


 GetTrans:
18200	!*******************************************************************
	! This subroutine will assign an Transaction number from the control
	! file AP_CONTROL.  It will make sure that the number it is trying
	! to assign does not already exist.
	!*******************************************************************

18220	!
	! Read in the control record
	!
	GET #AP_CONTROL.CH%, RECORD 1%

18260	!
	! We have a key to try now
	!
	TEMP = VAL(AP_CONTROL::LAST_TRANKEY) + 1.

	IF TEMP > 1000000.
	THEN
		AP_CONTROL::LAST_TRANKEY = "000000"
	ELSE
		AP_CONTROL::LAST_TRANKEY = FORMAT$(TEMP, "<0>#####")
	END IF

	AP_OPEN::TRANKEY = AP_CONTROL::LAST_TRANKEY
	AP_OPEN_DIST::TRANKEY = AP_CONTROL::LAST_TRANKEY

	UPDATE #AP_CONTROL.CH%

	RETURN

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
	! End of posting program GL_POST_USERJOUR
	!******************************************************************
	END
