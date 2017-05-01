1	%TITLE "Post Purchases Journal RECURRING"
	%SBTTL "AP_POST_PJ_RECUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	! ID:APPJPO
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Post Purchases Journal\* option posts
	!	the Purchases Journal transactions into the General Ledger,
	!	the Accounts Payable Subsidiary Ledger,
	!	the Job Costing System,
	!	and the Purchase Order Register.
	!	.note RETENTION
	!	The Purchases Journal Posting Transmittal report
	!	should be permanently filed along with the related
	!	Purchases Journal report.
	!	.end note
	!	^*
	!	.note
	!	Both the Print Purchases Journal and Print Invoice
	!	Attachments routines must be executed prior to posting
	!	a Purchases Journal batch, since the batch file is
	!	deleted upon the completion of the posting routine.\*
	!	.end note
	!
	! Index:
	!	.x Purchase Journal>Post
	!	.x Post>Purchase Journal
	!	.Y PO
	!	.Y PURCHASEORDERSYSTEM
	!	.Y PURCHASEORDER
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_POST_PJ_RECUR/LINE
	!	$ LINK/EXE=AP_EXE: AP_POST_PJ_RECUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_POST_PJ_RECUR.OBJ;*
	!
	! Author:
	!
	!	09/267/2002 - Kevin Handy
	!		Based on AP_POST_PJ
	!
	! Modification history:
	!
	!	09/30/2002 - Kevin Handy
	!		Calculate discount date and due date
	!
	!	05/23/2006 - Kevin Handy
	!		Change to dup duedates from journal file instead
	!		of calculating them.
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
	%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.HB"
	MAP	(AP_PJH)	AP_PJH_CDD	AP_PJH

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.HB"
	MAP	(AP_PJL)	AP_PJL_CDD	AP_PJL

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD		AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD		AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP (AP_CLOSE)		AP_CLOSE_CDD		AP_CLOSE


	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	AP_EXAM_VENDOR
	EXTERNAL LONG	FUNCTION	AP_TRAN_POSTAP
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES
	EXTERNAL LONG	FUNCTION	SB_EXAM_SUBACCOUNT
	EXTERNAL LONG	FUNCTION	PO_TRAN_POSTPO
	EXTERNAL LONG	FUNCTION	AP_FUNC_CALCVARIANCE

	!
	! Declare internal variables
	!
	DECLARE	AP_OPEN_DIST_CDD	AP_OPEN_DIST
	DECLARE	AP_VENDOR_CDD		AP_VENDOR_EXAM
	DECLARE	GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE PO_REG_LINE_CDD		PO_REG_LINE
	DECLARE PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE

	!
	! Record for remembering account/amount distribution for one
	! pj header.
	!
	RECORD ACCT_SUMMARY_CDD
		STRING	ACCT = 18%
		GFLOAT	AMOUNT
	END RECORD

	COM (CH_AP_CONTROL) &
		AP_CONTROL.CH%


	!
	! Dimension arrays
	!
	DIM ACCT_SUMMARY_CDD ACCOUNT_LIST(100%), CASH_LIST(100%)

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE	LONG			PRNT_SUMMARY
	DECLARE LONG			CHECK_PERIOD

	DECLARE	STRING			BATCH_NUMBER
	DECLARE	STRING			GLPERIOD
	DECLARE	STRING			AP.INTER.PERIOD
	DECLARE	STRING			GL.INTER.PERIOD
	DECLARE	STRING			POSTDATE
	DECLARE	STRING			POSTTIME
	DECLARE	STRING			TEXT
	DECLARE	STRING			TITLE(10%)

	%PAGE

	!*******************************************************************
	! Function to decide which description(s) to place in the GL
	! description field
	!
	!	V - Vendor name
	!	P - PJ description
	!	VP - Vendor name if there, else PJ description
	!	PV - PJ description if there, else vendor name
	!
	! Defaults to VP.
	!*******************************************************************

	DEF FNDESCR$(DESC_VALUE$, VEN_NAME$, PJ_DESC$)

		SELECT DESC_VALUE$

		CASE "V"
			FNDESCR$ = VEN_NAME$

		CASE "P"
			FNDESCR$ = PJ_DESC$

		CASE "PV"
			IF PJ_DESC$ = ""
			THEN
				FNDESCR$ = VEN_NAME$
			ELSE
				FNDESCR$ = PJ_DESC$ + " " + VEN_NAME$
			END IF

		CASE ELSE
			IF VEN_NAME$ = ""
			THEN
				FNDESCR$ = PJ_DESC$
			ELSE
				FNDESCR$ = VEN_NAME$ + " " + PJ_DESC$
			END IF

		END SELECT
	FNEND

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
	TITLE(1%) = "PURCHASE  JOURNAL  POSTING  PROTOCOL"
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
	!	^*(01) YearPeriod (YYYYPP)\*
	!	.b
	!	.lm +5
	!	The ^*YearPeriod\* field
	!	posts a Purchases Journal
	!	into a specific accounting period.
	!	.b
	!	Enter the period into which the journal file is to be
	!	posted in YYYYPP format.
	!	.lm -5
	!
	! Index:
	!	.x YearPeriod>Post Purchase Journal
	!	.x Post Purchase Journal>YearPeriod
	!
	!--
	BATCH_NO$ = TRM$(UTL_REPORTX::OPTDEF(1%))

	!++
	! Abstract:FLD02
	!	^*(02) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field
	!	specifies which Purchases
	!	Journal batch is to be posted to the General Ledger and the
	!	Accounts Payable Subsidiary Ledger.  The Batch Number field must
	!	contain a valid value, a batch number which contains data.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Post Purchase Journal
	!	.x Post Purchase Journal>Batch Number
	!
	!--
	CHECK_DATE$ = TRM$(UTL_REPORTX::OPTDEF(2%))

	!++
	! Abstract:FLD03
	!
	!
	! Index:
	!
	!
	!--

	DESC_VALUE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)
	!++
	! Abstract:FLD06
	!	^*(06) Description (VP)\*
	!	.b
	!	.lm +5
	!	The ^*Description (VP)\* field determines which
	!	description is to be put into the General Ledger Description field.
	!	This is the description that shows up on the General Ledger
	!	and the Check Reconciliation.
	!	.b
	!	There are only five valid values for this field:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	*V - Put the Vendor Name in the General Ledger.
	!	.le
	!	*P - Put the Purchase Journal Description in the General Ledger.
	!	.le
	!	^*VP\* - If there is a vendor name,
	!	put it in the General Ledger,
	!	otherwise use the Purchase Journal description.
	!	.le
	!	^*PV\* - If there is a Purchase Journal description,
	!	put it in the General Ledger,
	!	otherwise use the Vendor Name.
	!	.le
	!	^*<Blank>\* - Same as ^*VP\*.
	!	.els
	!
	! Index:
	!	.x PJ Post>Description
	!	.x Description>PJ Post
	!
	!--

	TRAN_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%))
	!++
	! Abstract:FLD07
	!	^*(07) Transaction Date\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!	.x PJ Post>Transaction Date
	!	.x Transaction Date>PJ Post
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF (CHECK_DATE$ = "Y")

310	!
	! Open journal header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.UPD"
	USE
		IF ERR = 138%
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "AP_PJH"
		CONTINUE HelpError
	END WHEN

320	!
	! Open journal line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.UPD"
	USE
		IF ERR = 138%
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "AP_PJL"
		CONTINUE HelpError
	END WHEN

330	!
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

340	!
	! Closed file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.OPN"
	USE
		FILENAME$ = "AP_CLOSE"
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
		UTL_REPORTX, "AP_PJH", BATCH_NO$, &
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
				PO_TRAN_POSTPO(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"", "") <> CMC$_NORMAL
			GOTO Aborted IF &
				GL_TRAN_POSTGL(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, "", "", &
				GL.INTER.PERIOD) <> CMC$_NORMAL
		END IF

		IF TRM$(AP.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				AP_TRAN_POSTAP(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, "", "", &
				AP.INTER.PERIOD) <> CMC$_NORMAL
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
		UTL_REPORTX, "AP_PJH", BATCH_NO$, &
		GLPERIOD, "") <> CMC$_NORMAL

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", GLPERIOD)

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

	EXIT_STATUS = PO_TRAN_POSTPO(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "")

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
		UTL_REPORTX, "AP_PJH", BATCH_NO$, "", "") <> CMC$_NORMAL

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	!
	! Blank flags
	!
	VENDNUM$, CASHACCT$, APACCT$, TRANDAT$, PONUM$ = " "

 ReadHeader:
	!
	! Read in one record from the header file
	!
500	WHEN ERROR IN
		GET #AP_PJH.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE Confirm
	END WHEN

	!
	! Check the vendor number - is it defined?
	!
	EXIT_STATUS = AP_EXAM_VENDOR(AP_PJH::VENNUM, AP_VENDOR_EXAM)

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
		TEXT$ = "Transaction " + TRM$(AP_PJH::TRANKEY) + &
			" - " + TRM$(AP_PJH::VENNUM) + &
			"Invalid Vendor Number!"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted

	END SELECT

	!
	! See if AP Account number is defined
	!
	EXIT_STATUS = GL_EXAM_CHART(AP_PJH::AP_ACCT, GL_CHART_EXAM)

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
		TEXT$ = "Transaction " + TRM$(AP_PJH::TRANKEY) + &
			" - " + TRM$(AP_PJH::VENNUM) + " " + &
			TRM$(AP_PJH::AP_ACCT) + &
			" Invalid GL Account!"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted

	END SELECT

	!
	! See if Cash account number is defined
	!
	EXIT_STATUS = GL_EXAM_CHART(AP_PJH::CASH_ACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set a flag and go on
	!
	CASE CMC$_UNDEFINED
		CASHACCT$ = "*" IF (AP_PJH::CKNUM <> "")

	!
	! SNAFU:  (Situation Normal - it's All Fouled Up)
	!
	CASE ELSE
		TEXT$ = "Transaction " + TRM$(AP_PJH::TRANKEY) + &
			" - " + TRM$(AP_PJH::VENNUM) + " " + &
			TRM$(AP_PJH::CASH_ACCT) + &
			" Invalid GL Account!"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted

	END SELECT

	!
	! Set initial values for some variables
	!
	ACCOUNT_LIST% = 0%
	CASH_LIST% = 0%
	USE_AMT = 0.0
	PRINTHEAD% = 0%
	ACCTNUM$ = " "
	VARACCTNUM$ = " "
	SUBACCT$ = " "

 ProcessLines:
	!
	! Process all line items for this header
	!
550	WHEN ERROR IN
		FIND #AP_PJL.CH%, KEY #0% EQ AP_PJH::TRANKEY
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
600	WHEN ERROR IN
		GET #AP_PJL.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader IF ERR = 11%
		FILENAME$ = "AP_PJL"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the header if we're done with the line items
	!
	GOTO ProcessHeader IF AP_PJL::TRANKEY <> AP_PJH::TRANKEY

	!
	! Calculate the net money amount
	!
	NET = AP_PJL::AMOUNT - AP_PJL::DISCAMT

	!
	! Handle summarizing by account before playing around
	! with price variences
	!
	CALL GL_ASSG_ACCMASK(AP_PJH::AP_ACCT, AP_PJL::ACCT, TEST_ACCT$)

	!
	! Add (summarize) for header amount
	!
	FOR SEEK_LOOP% = 1% TO ACCOUNT_LIST%
		IF (ACCOUNT_LIST(SEEK_LOOP%)::ACCT = TEST_ACCT$)
		THEN
			ACCOUNT_LIST(SEEK_LOOP%)::AMOUNT = &
				ACCOUNT_LIST(SEEK_LOOP%)::AMOUNT + NET
			GOTO ExitSeek
		END IF
	NEXT SEEK_LOOP%

	SEEK_LOOP%, ACCOUNT_LIST% = ACCOUNT_LIST% + 1%
	ACCOUNT_LIST(ACCOUNT_LIST%)::ACCT = TEST_ACCT$
	ACCOUNT_LIST(ACCOUNT_LIST%)::AMOUNT = NET

 ExitSeek:

	IF (AP_PJH::CKAMT <> 0.0)
	THEN
		!
		! Also summarize cash if necessary
		!
		CALL GL_ASSG_ACCMASK(AP_PJH::CASH_ACCT, &
			AP_PJL::ACCT, TEST_ACCT$)

		CASH_LIST(SEEK_LOOP%)::AMOUNT = &
			CASH_LIST(SEEK_LOOP%)::AMOUNT + NET &
			IF (CASH_LIST(SEEK_LOOP%)::ACCT = TEST_ACCT$) &
			FOR SEEK_LOOP% = 1% TO CASH_LIST%

		IF CASH_LIST(CASH_LIST%)::ACCT <> TEST_ACCT$
		THEN
			SEEK_LOOP%, CASH_LIST% = CASH_LIST% + 1%
			CASH_LIST(CASH_LIST%)::ACCT = TEST_ACCT$
			CASH_LIST(CASH_LIST%)::AMOUNT = 0.0
		END IF

	END IF

	!
	! Calculate any variance problems
	!
	EXIT_STATUS = AP_FUNC_CALCVARIANCE(AP_PJL::PONUM, AP_PJL::PO_LINE, &
		AP_PJL::ACCT, AP_PJL::UNITS, AP_PJL::AMOUNT, &
		PO_ACCOUNT$, PO_VARIANCE, PO_AMOUNT, AP_PJH::TRANKEY_DATE)

	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::ACCT	= AP_PJL::ACCT
	GL_YYYY_PP::SOURCE	= "PJ"
	GL_YYYY_PP::REFNO	= AP_PJH::INVNUM
	GL_YYYY_PP::TRANDAT	= TRAN_DATE$
	GL_YYYY_PP::DESCR	= FNDESCR$(DESC_VALUE$, &
		AP_VENDOR_EXAM::VENNAM, AP_PJH::DESCR)
	GL_YYYY_PP::AMOUNT	= NET - PO_VARIANCE
	GL_YYYY_PP::XREFNO	= AP_PJH::VENNUM
	GL_YYYY_PP::POSTIM	= TIME_NOW
	GL_YYYY_PP::POSDAT	= DATE_TODAY
	GL_YYYY_PP::CKNO	= AP_PJH::CKNUM
	GL_YYYY_PP::TRANKEY	= AP_PJH::TRANKEY
	GL_YYYY_PP::SUBACC	= AP_PJL::SUBACC
	GL_YYYY_PP::OPERATION	= AP_PJL::OPERATION
	GL_YYYY_PP::UNITS	= AP_PJL::UNITS
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	! See if GL Chart number is defined
	!
	EXIT_STATUS = GL_EXAM_CHART(AP_PJL::ACCT, GL_CHART_EXAM)

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
		TEXT$ = "Transaction " + TRM$(AP_PJH::TRANKEY) + &
			" - " + TRM$(AP_PJH::VENNUM) + " " + &
			TRM$(AP_PJL::ACCT) + &
			" Invalid GL Account!"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted

	END SELECT

	!
	! Put the record into the temporary file
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
		SUBOPT_NOOPT + CHECK_PERIOD, &
		"", TITLE(), UTL_REPORTX, GL_YYYY_PP, &
		GL_CHART_EXAM, GLPERIOD)

	!
	! Check date; is it in the correct period?
	!
	SELECT EXIT_STATUS

	!
	! Date out of range
	!
	CASE CMC$_DATEOUT
		TRANDAT$ = "*"

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Check subaccount
	!
	CASE CMC$_WARNING
		!
		! See if Sub Account number is defined
		!
		EXIT_STATUS = SB_EXAM_SUBACCOUNT("J", AP_PJL::SUBACC, &
			SB_SUBACCOUNT_EXAM)

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

		!
		! SNAFU:  (Situation Normal - it's All Fouled Up)
		!
		CASE ELSE
			TEXT$ = "Transaction " + TRM$(AP_PJH::TRANKEY) + &
				" - " + TRM$(AP_PJH::VENNUM) + " " + &
				TRM$(AP_PJL::SUBACC) + &
				" Invalid SubAccount!"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			GOTO Aborted

		END SELECT

	CASE ELSE
		GOTO Aborted
	END SELECT


	IF PO_VARIANCE <> 0.0
	THEN
		!
		! Generate a GL record to pass through to the post function
		!
		GL_YYYY_PP::ACCT	= PO_ACCOUNT$
		GL_YYYY_PP::SOURCE	= "PJ"
		GL_YYYY_PP::REFNO	= AP_PJH::INVNUM
		GL_YYYY_PP::TRANDAT	= TRAN_DATE$
		GL_YYYY_PP::DESCR	= FNDESCR$(DESC_VALUE$, &
			AP_VENDOR_EXAM::VENNAM, AP_PJH::DESCR)
		GL_YYYY_PP::AMOUNT	= PO_VARIANCE
		GL_YYYY_PP::XREFNO	= AP_PJH::VENNUM
		GL_YYYY_PP::POSTIM	= TIME_NOW
		GL_YYYY_PP::POSDAT	= DATE_TODAY
		GL_YYYY_PP::CKNO	= AP_PJH::CKNUM
		GL_YYYY_PP::TRANKEY	= AP_PJH::TRANKEY
		GL_YYYY_PP::SUBACC	= AP_PJL::SUBACC
		GL_YYYY_PP::OPERATION	= AP_PJL::OPERATION
		GL_YYYY_PP::UNITS	= AP_PJL::UNITS
		GL_YYYY_PP::HOURS	= 0.0
		GL_YYYY_PP::UPDSTA	= ""
		GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

		!
		! See if GL Chart number is defined
		!
		EXIT_STATUS = GL_EXAM_CHART(PO_ACCOUNT$, GL_CHART_EXAM)

		SELECT EXIT_STATUS

		!
		! Success; go on
		!
		CASE CMC$_NORMAL

		!
		! Undefined; set a flag and go on
		!
		CASE CMC$_UNDEFINED
			VARACCTNUM$ = "*"

		!
		! SNAFU:  (Situation Normal - it's All Fouled Up)
		!
		CASE ELSE
			TEXT$ = "Transaction " + TRM$(AP_PJH::TRANKEY) + &
				" - " + TRM$(AP_PJH::VENNUM) + " " + &
				TRM$(PO_ACCOUNT$) + &
				" Invalid GL Account!"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			GOTO Aborted

		END SELECT

		!
		! Put the record into the temporary file
		!
		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
			SUBOPT_NOOPT + CHECK_PERIOD, &
			"", TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, GLPERIOD)

		!
		! Check date; is it in the correct period?
		!
		SELECT EXIT_STATUS

		!
		! Date out of range
		!
		CASE CMC$_DATEOUT
			TRANDAT$ = "*"

		!
		! Success; go on
		!
		CASE CMC$_NORMAL

		!
		! Check subaccount
		!
		CASE CMC$_WARNING
			!
			! See if Sub Account number is defined
			!
			EXIT_STATUS = SB_EXAM_SUBACCOUNT("J", &
				AP_PJL::SUBACC, SB_SUBACCOUNT_EXAM)

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

			!
			! SNAFU:  (Situation Normal - it's All Fouled Up)
			!
			CASE ELSE
				TEXT$ = "Transaction " + &
					TRM$(AP_PJH::TRANKEY) + &
					" - " + TRM$(AP_PJH::VENNUM) + " " + &
					TRM$(AP_PJL::SUBACC) + &
					" Invalid SubAccount!"
				CALL OUTP_LINE("", UTL_REPORTX, TITLE(), &
					TEXT$, 0%)
				GOTO Aborted

			END SELECT

		CASE ELSE
			GOTO Aborted
		END SELECT

	END IF
	!
	! Generate a AP_OPEN_DIST record to pass through to the
	! post function
	!
	IF PONUM$ = ""
	THEN
		PONUM$ = AP_PJL::PONUM
	END IF

	AP_OPEN_DIST::TRANKEY		= AP_PJL::TRANKEY
	AP_OPEN_DIST::SLINE		= AP_PJL::SLINE
	AP_OPEN_DIST::PONUM		= AP_PJL::PONUM
	AP_OPEN_DIST::PO_LINE		= AP_PJL::PO_LINE
	AP_OPEN_DIST::ACCT		= AP_PJL::ACCT
	AP_OPEN_DIST::SUBACC		= AP_PJL::SUBACC
	AP_OPEN_DIST::OPERATION		= AP_PJL::OPERATION
	AP_OPEN_DIST::UNITS		= AP_PJL::UNITS
	AP_OPEN_DIST::AMOUNT		= AP_PJL::AMOUNT
	AP_OPEN_DIST::DISCAMT		= AP_PJL::DISCAMT
	AP_OPEN_DIST::USE_TAX_FLAG	= AP_PJL::USE_TAX_FLAG
	AP_OPEN_DIST::BTHNUM		= BATCH_NUMBER

	!
	! Put the record into the temporary file
	!
	GOTO Aborted &
		IF AP_TRAN_POSTAP(OPT_ADDREC, SUBOPT_LINEITEM, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, AP_OPEN, AP_OPEN_DIST, "") <> CMC$_NORMAL

	!
	! Add to the USE_AMT total
	!
	USE_AMT = USE_AMT + NET IF (AP_PJL::USE_TAX_FLAG = "Y")

	IF NOT(AP_PJL::PONUM = "")
	THEN
		PO_REG_SUB_LINE::PO		= AP_PJL::PONUM
		PO_REG_SUB_LINE::PO_LINE	= AP_PJL::PO_LINE
		PO_REG_SUB_LINE::PO_ACTION	= "09"
		PO_REG_SUB_LINE::ACTION_DATE	= TRAN_DATE$
		PO_REG_SUB_LINE::QTY		= AP_PJL::UNITS
		PO_REG_SUB_LINE::ACCOUNT	= AP_PJL::ACCT
		PO_REG_SUB_LINE::SUBACCT	= AP_PJL::SUBACC

		IF AP_PJL::UNITS <> 0.0
		THEN
			PO_REG_SUB_LINE::PRICE	= AP_PJL::AMOUNT / AP_PJL::UNITS
		ELSE
			PO_REG_SUB_LINE::PRICE	= AP_PJL::AMOUNT
		END IF

		PO_REG_SUB_LINE::BATCH		= BATCH_NUMBER
		PO_REG_SUB_LINE::POSTDATE	= POSTDATE
		PO_REG_SUB_LINE::POSTTIME	= POSTTIME

		!
		! Call the post function
		!
		GOTO Aborted IF PO_TRAN_POSTPO(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			PO_REG_LINE, PO_REG_SUB_LINE) <> CMC$_NORMAL
	END IF


 GoToNext:
	!
	! Was the account number undefined?
	!
	IF (ACCTNUM$ = "*") OR (SUBACCT$ = "*") OR (VARACCTNUM$ = "*")
	THEN
		!
		! Print out the header if it hasn't already been
		!
		IF PRINTHEAD% = 0%
		THEN
			!
			! Put the header line in TEXT
			!
			TEXT = "HEADER " + AP_PJH::TRANKEY + " " + &
				VENDNUM$ + AP_PJH::VENNUM + " " + &
				APACCT$ + AP_PJH::AP_ACCT + " " + &
				CASHACCT$ + AP_PJH::CASH_ACCT + " " + &
				TRANDAT$ + PRNT_DATE(TRAN_DATE$, 0%)

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
		TEXT = "  LINE                    " + &
			ACCTNUM$ + AP_PJL::ACCT + " " + &
			VARACCTNUM$ + PO_ACCOUNT$ + &
			"           " + &
			SUBACCT$ + AP_PJL::SUBACC

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, &
			TEXT) <> CMC$_NORMAL

		!
		! Blank flags
		!
		ACCTNUM$, VARACCNUM$, SUBACCT$ = " "

	END IF

650	GOTO LineItem

 ProcessHeader:
	!
	! Process the header
	!

	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::REFNO	= AP_PJH::INVNUM
	GL_YYYY_PP::DESCR	= FNDESCR$(DESC_VALUE$, &
		AP_VENDOR_EXAM::VENNAM, AP_PJH::DESCR)
	GL_YYYY_PP::XREFNO	= AP_PJH::VENNUM
	GL_YYYY_PP::POSTIM	= TIME_NOW
	GL_YYYY_PP::POSDAT	= DATE_TODAY
	GL_YYYY_PP::TRANKEY	= AP_PJH::TRANKEY
	GL_YYYY_PP::SUBACC	= ""
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	IF (INSTR(1%, AP_PJH::AP_ACCT, "?") <> 0%) OR &
		(INSTR(1%, AP_PJH::CASH_ACCT, "?") <> 0%)
	THEN
		!
		! Masking
		!
		TEST_AMT = FUNC_ROUND(AP_PJH::INVAMT - AP_PJH::DISCAMT, 2%)

		!
		! Make sure we are updating the proper amount
		!
		SEEK_TEST = 0.0
		SEEK_TEST = SEEK_TEST + ACCOUNT_LIST(SEEK_LOOP%)::AMOUNT &
			FOR SEEK_LOOP% = 1% TO ACCOUNT_LIST%
		IF (TEST_AMT <> FUNC_ROUND(SEEK_TEST, 2%))
		THEN
			TEXT$ = "Transaction " + TRM$(AP_PJH::TRANKEY) + &
				" - " + TRM$(AP_PJH::VENNUM) + &
				"Doesn't Balance!"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			GOTO Aborted
		END IF

		!
		! We either dump it all to cash, or to ap when
		! a mask is in effect.
		!
		IF CASH_LIST% = 0%
		THEN
			FOR SEEK_LOOP% = 1% TO ACCOUNT_LIST%

				IF ACCOUNT_LIST(SEEK_LOOP%)::AMOUNT <> 0.0
				THEN
					!
					! Amount that has not been paid off
					!
					GL_YYYY_PP::ACCT	= ACCOUNT_LIST(SEEK_LOOP%)::ACCT
					GL_YYYY_PP::SOURCE	= "PJ"
					GL_YYYY_PP::TRANDAT	= TRAN_DATE$
					GL_YYYY_PP::AMOUNT	= -ACCOUNT_LIST(SEEK_LOOP%)::AMOUNT
					GL_YYYY_PP::CKNO	= AP_PJH::CKNUM
					GL_YYYY_PP::TRANKEY	= AP_PJH::TRANKEY

					EXIT_STATUS = GL_EXAM_CHART(GL_YYYY_PP::ACCT, GL_CHART_EXAM)
					IF (EXIT_STATUS <> CMC$_NORMAL) AND &
						(EXIT_STATUS <> CMC$_WARNING)
					THEN
						TEXT$ = "Transaction " + TRM$(AP_PJH::TRANKEY) + &
							" - " + TRM$(AP_PJH::VENNUM) + " " + &
							TRM$(GL_YYYY_PP::ACCT) + &
							" Invalid GL Account!"
						CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
						GOTO Aborted
					END IF

					!
					! Put the record into the temporary file
					!
					EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
						CHECK_PERIOD, "", TITLE(), &
						UTL_REPORTX, &
						GL_YYYY_PP, &
						GL_CHART_EXAM, GLPERIOD)

				END IF

			NEXT SEEK_LOOP%
		ELSE
			!
			! Generate a GL for check payment
			! record to pass through
			! to the post function
			!
			!
			! Make sure we are updating the proper amount
			!
			SEEK_TEST = 0.0
			SEEK_TEST = SEEK_TEST + CASH_LIST(SEEK_LOOP%)::AMOUNT &
				FOR SEEK_LOOP% = 1% TO CASH_LIST%

			IF (AP_PJH::CKAMT <> FUNC_ROUND(SEEK_TEST, 2%))
			THEN
				TEXT$ = "Cash amount for " + &
					TRM$(AP_PJH::TRANKEY) + " - " + &
					TRM$(AP_PJH::VENNUM) + &
					"Doesn't Balance!"
				CALL OUTP_LINE("", UTL_REPORTX, &
					TITLE(), TEXT$, 0%)
				GOTO Aborted
			END IF

			FOR SEEK_LOOP% = 1% TO CASH_LIST%

				IF CASH_LIST(SEEK_LOOP%)::AMOUNT <> 0.0
				THEN
					GL_YYYY_PP::ACCT	= CASH_LIST(SEEK_LOOP%)::ACCT
					GL_YYYY_PP::SOURCE	= "PJ"
					GL_YYYY_PP::TRANDAT	= TRAN_DATE$
					GL_YYYY_PP::AMOUNT	= -CASH_LIST(SEEK_LOOP%)::AMOUNT
					GL_YYYY_PP::CKNO	= AP_PJH::CKNUM

					EXIT_STATUS = GL_EXAM_CHART(GL_YYYY_PP::ACCT, GL_CHART_EXAM)
					IF (EXIT_STATUS <> CMC$_NORMAL) AND &
						(EXIT_STATUS <> CMC$_WARNING)
					THEN
						TEXT$ = "Transaction " + TRM$(AP_PJH::TRANKEY) + &
							" - " + TRM$(AP_PJH::VENNUM) + " " + &
							TRM$(GL_YYYY_PP::ACCT) + &
							" Invalid GL Account!"
						CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
						GOTO Aborted
					END IF

					!
					! Put the record into the temporary file
					!
					EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
						CHECK_PERIOD, "", &
						TITLE(), UTL_REPORTX, &
						GL_YYYY_PP, &
						GL_CHART_EXAM, GLPERIOD)

				END IF

			NEXT SEEK_LOOP%

		END IF

	ELSE
		!
		! No masking. Update both the AP and the CASH
		! as desired.
		!
		TEST_AMT = FUNC_ROUND(-((AP_PJH::INVAMT - AP_PJH::DISCAMT) &
			- AP_PJH::CKAMT), 2%)

		IF TEST_AMT <> 0.0
		THEN
			!
			! Amount that has not been paid off
			!
			GL_YYYY_PP::ACCT	= AP_PJH::AP_ACCT
			GL_YYYY_PP::SOURCE	= "PJ"
			GL_YYYY_PP::TRANDAT	= TRAN_DATE$
			GL_YYYY_PP::AMOUNT	= TEST_AMT
			GL_YYYY_PP::CKNO	= AP_PJH::CKNUM
			GL_YYYY_PP::TRANKEY	= AP_PJH::TRANKEY

			EXIT_STATUS = GL_EXAM_CHART(GL_YYYY_PP::ACCT, &
				GL_CHART_EXAM)
			IF (EXIT_STATUS <> CMC$_NORMAL) AND &
				(EXIT_STATUS <> CMC$_WARNING)
			THEN
				TEXT$ = "Transaction " + TRM$(AP_PJH::TRANKEY) + &
					" - " + TRM$(AP_PJH::VENNUM) + " " + &
					TRM$(GL_YYYY_PP::ACCT) + &
					" Invalid GL Account!"
				CALL OUTP_LINE("", UTL_REPORTX, &
					TITLE(), TEXT$, 0%)
				GOTO Aborted
			END IF

			!
			! Put the record into the temporary file
			!
			EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
				CHECK_PERIOD, "", &
				TITLE(), UTL_REPORTX, &
				GL_YYYY_PP, &
				GL_CHART_EXAM, GLPERIOD)

		END IF


		IF (AP_PJH::CKAMT <> 0.0) OR (EDIT$(AP_PJH::CKNUM, -1%) <> "")
		THEN
			GL_YYYY_PP::ACCT	= AP_PJH::CASH_ACCT
			GL_YYYY_PP::SOURCE	= "PJ"
			GL_YYYY_PP::TRANDAT	= TRAN_DATE$
			GL_YYYY_PP::AMOUNT	= -AP_PJH::CKAMT
			GL_YYYY_PP::CKNO	= AP_PJH::CKNUM

			EXIT_STATUS = GL_EXAM_CHART(GL_YYYY_PP::ACCT, &
				GL_CHART_EXAM)
			IF (EXIT_STATUS <> CMC$_NORMAL) AND &
				(EXIT_STATUS <> CMC$_WARNING)
			THEN
				TEXT$ = "Transaction " + TRM$(AP_PJH::TRANKEY) + &
					" - " + TRM$(AP_PJH::VENNUM) + " " + &
					TRM$(GL_YYYY_PP::ACCT) + &
					" Invalid GL Account!"
				CALL OUTP_LINE("", UTL_REPORTX, &
					TITLE(), TEXT$, 0%)
				GOTO Aborted
			END IF

			!
			! Put the record into the temporary file
			!
			EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
				CHECK_PERIOD, "", &
				TITLE(), UTL_REPORTX, &
				GL_YYYY_PP, &
				GL_CHART_EXAM, GLPERIOD)

		END IF

	END IF

670	!
	! Generate a AP record to pass through to the post function
	!
	AP_OPEN::VENNUM		= AP_PJH::VENNUM
	AP_OPEN::TRANKEY	= AP_PJH::TRANKEY
	AP_OPEN::TRANKEY_DATE	= AP_PJH::TRANKEY_DATE
	AP_OPEN::INVNUM		= AP_PJH::INVNUM
	AP_OPEN::INVDAT		= TRAN_DATE$
	AP_OPEN::INVAMT		= AP_PJH::INVAMT
	AP_OPEN::CODE_1099	= AP_PJH::CODE_1099
	AP_OPEN::AMT_1099	= AP_PJH::AMT_1099
	AP_OPEN::USE_JOB_NUM	= AP_PJH::USE_JOB_NUM
	AP_OPEN::USE_AMT	= USE_AMT

	!
	! Calculate discount date
	!
 !	IF AP_VENDOR_EXAM::DISDATE <> ""
 !	THEN
		!
		! Discount date on specific day of the month
		!
 !		V_DAY% = FNVAL%(AP_VENDOR_EXAM::DISDATE)
 !		NEW_DAY%, I_DAY% = FNVAL%(MID$(AP_OPEN::INVDAT, 7%, 2%))
 ! 		NEW_MON% = FNVAL%(MID$(AP_OPEN::INVDAT, 5%, 2%))
 !		NEW_YEAR$ = LEFT(AP_OPEN::INVDAT, 4%)
 !
 !		SELECT I_DAY%
 !		CASE < V_DAY%
 !			NEW_DAY% = V_DAY%
 !		CASE ELSE
 !			NEW_MON% = NEW_MON% + 1%
 !			IF NEW_MON% > 12%
 !			THEN
 !				NEW_MON% = 1%
 !				NEW_YEAR$ = FORMAT$( &
 !					FNVAL%(NEW_YEAR$) + 1%, &
 !					"<0>###")
 !			END IF
 !			NEW_DAY% = V_DAY%
 !		END SELECT
 !
 !		AP_OPEN::DISCDAT = NEW_YEAR$ + &
 !			FORMAT$(NEW_MON%, "<0>#") + &
 !			FORMAT$(NEW_DAY%, "<0>#")
 !
		!
		! Make sure the date exists this month
		!
 !		IF (AP_OPEN::DISCDAT <> DATE_INVDCODE(DATE_DAYCODE( &
 !			AP_OPEN::DISCDAT)))
 !		THEN
 !			AP_OPEN::DISCDAT = DATE_INVDCODE(DATE_DAYCODE( &
 !				AP_OPEN::DISCDAT) - 1%)
 !		END IF
 !
 !	ELSE
		!
		! Discount date so many days after invoice date
		!
 !		IF AP_VENDOR_EXAM::DISDAYS > 0%
 !		THEN
 !			AP_OPEN::DISCDAT = DATE_INVDCODE(DATE_DAYCODE( &
 !				AP_OPEN::INVDAT) + &
 !				AP_VENDOR_EXAM::DISDAYS)
 !		ELSE
 !			AP_OPEN::DISCDAT = AP_OPEN::INVDAT
 !		END IF
 !	END IF

	AP_OPEN::DISCDAT = AP_PJH::DISCDAT
	AP_OPEN::DISAMT = AP_PJH::DISCAMT

680	!
	! Calculate due date
	!
	AP_OPEN::DUEDAT = AP_PJH::DUEDAT

 !	AP_OPEN::DUEDAT = AP_OPEN::INVDAT
 !
 !	IF AP_VENDOR_EXAM::DUEDAYS <> 0%
 !	THEN
		!
		! Paid so many days after the invoice date.
		!
 !		AP_OPEN::DUEDAT = DATE_INVDCODE(DATE_DAYCODE( &
 !			AP_OPEN::INVDAT) + &
 !			AP_VENDOR_EXAM::DUEDAYS)
 !
 !	ELSE
 !		GOTO CheckDate &
 !			IF FNVAL%(AP_VENDOR_EXAM::DUEDATE) = 0%
		!
		! Paid on a specific day of the month
		!
 !		NEW_MON% = &
 !			FNVAL%(MID$(AP_OPEN::INVDAT, 5%, 2%)) + 1%
 !		NEW_YEAR% = FNVAL%(MID$(AP_OPEN::INVDAT, 1%, 4%))
 !		IF NEW_MON% > 12%
 !		THEN
 !			NEW_MON% = 1%
 !			NEW_YEAR% = NEW_YEAR% + 1%
 !		END IF
 !
 !		AP_OPEN::DUEDAT = &
 !			FORMAT$(NEW_YEAR%, "<0>###") + &
 !			FORMAT$(NEW_MON%, "<0>#") + &
 !			FORMAT$(FNVAL%(AP_VENDOR_EXAM::DUEDATE), &
 !			"<0>#")
 !
 !	END IF

 CheckDate:
690	!
	! Make sure the date exists this month
	!
	IF (AP_OPEN::DUEDAT <> DATE_INVDCODE(DATE_DAYCODE( &
		AP_OPEN::DUEDAT)))
	THEN
		AP_OPEN::DUEDAT = LEFT(AP_OPEN::DUEDAT, 6%) + &
			FORMAT$(FNVAL%(MID(AP_OPEN::DUEDAT, 7%, 2%)) - &
			1%, "<0>#")
		GOTO CheckDate
	END IF

	!
	! Now, back to filling out the rest of AP_OPEN
	!
	AP_OPEN::PONUM		= PONUM$
	AP_OPEN::AP_ACCT	= AP_PJH::AP_ACCT
	AP_OPEN::CASH_ACCT	= AP_PJH::CASH_ACCT
	AP_OPEN::CKNUM		= AP_PJH::CKNUM
	AP_OPEN::CKDAT		= AP_PJH::CKDAT
	AP_OPEN::CKDESC		= AP_PJH::DESCR
	AP_OPEN::CKAMT		= AP_PJH::CKAMT
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

	!
	! Was anything undefined?
	!
700	IF INSTR(1%, VENDNUM$ + CASHACCT$ + APACCT$ + TRANDAT$, "*") AND &
		(PRINTHEAD% = 0%)
	THEN
		!
		! Put the invalid stuff in TEXT to send to OUTP_UNDEFCODES
		!
		TEXT = "HEADER " + AP_PJH::TRANKEY + " " + &
			VENDNUM$ + AP_PJH::VENNUM + " " + &
			APACCT$ + AP_PJH::AP_ACCT + " " + &
			CASHACCT$ + AP_PJH::CASH_ACCT + " " + &
			TRANDAT$ + PRNT_DATE(TRAN_DATE$, 0%)

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT) <> CMC$_NORMAL

		!
		! Blank flags
		!
		VENDNUM$, APACCT$, CASHACCT$, TRANDAT$ = " "

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

	!
	! Post the PO Journal header
	!
	GOTO Interrupt IF &
		PO_TRAN_POSTPO(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	!
	! Continue by posting the Sales Journal line items
	!
	GOTO Interrupt IF &
		PO_TRAN_POSTPO(OPT_POSTFILE, SUBOPT_LINEITEM, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	%PAGE

	!
	! Remove files
	!
5000	CLOSE AP_PJH.CH%
	CLOSE AP_PJL.CH%

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AP_PJH", BATCH_NO$, "", "")

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
	TEXT = "Item   Tran#   Vendor      AP Account          " + &
		"Cash Account        InvDate   SubAccount"

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
			TITLE(), UTL_REPORTX, "AP_PJH", &
			BATCH_NO$, "", "")
		GOTO ExitProgram
	END IF

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AP_PJH", BATCH_NO$, "", "")

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

	%PAGE

 GetTrans:
28000	!*******************************************************************
	! This subroutine will assign an Transaction number from the control
	! file AP_CONTROL.  It will make sure that the number it is trying
	! to assign does not already exist.
	!*******************************************************************

28020	!
	! Read in the control record
	!
	WHEN ERROR IN
		GET #AP_CONTROL.CH%, RECORD 1%
	USE
		!
		! Invoice Record not found in ap open file
		!
		CONTINUE ExitProgram
	END WHEN

	AP_PJH::AP_ACCT = AP_CONTROL::AP_ACCT

28060	!
	! We have a key to try now
	!
	TEMP = VAL(AP_CONTROL::LAST_TRANKEY) + 1.0

	IF TEMP > 1000000.0
	THEN
		AP_CONTROL::LAST_TRANKEY = "000000"
	ELSE
		AP_CONTROL::LAST_TRANKEY = FORMAT$(TEMP, "<0>#####")
	END IF

	AP_PJH::TRANKEY = AP_CONTROL::LAST_TRANKEY

	UPDATE #AP_CONTROL.CH%

	RETURN

	%PAGE

 DupOpenFile:
28100	!**********************************************************************
	! Search for duplicate invoice in AP Open file
	!**********************************************************************
	WHEN ERROR IN
		FIND #AP_OPEN.CH%, &
			KEY #1% EQ AP_PJH::VENNUM + AP_PJH::INVNUM, &
			REGARDLESS
	USE
		CONTINUE 28190 IF ERR = 155% OR ERR = 9%
		EXIT HANDLER
	END WHEN

	DUP_INV% = -1%

28190	RETURN

 DupCloseFile:
28200	!**********************************************************************
	! Search for duplicate invoice in AP Close file
	!**********************************************************************
	WHEN ERROR IN
		FIND #AP_CLOSE.CH%, &
			KEY #1% EQ AP_PJH::VENNUM + AP_PJH::INVNUM, &
			REGARDLESS
	USE
		CONTINUE 28290 IF ERR = 155% OR ERR = 9%
		EXIT HANDLER
	END WHEN

	DUP_INV% = -1%

28290	RETURN

28900	!*******************************************************************
	! FNVAL%() function - FNVAL with error trapping
	!*******************************************************************

	DEF FNVAL%(X$)

		WHEN ERROR IN
			RESULT% = VAL%(X$)
		USE
			RESULT% = 0%
		END WHEN

		FNVAL% = RESULT%
	FNEND

32000	!******************************************************************
	! End of posting program AP_POST_PJ_RECUR
	!******************************************************************
	END
