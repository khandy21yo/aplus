1	%TITLE "Inventory Journal Post"
	%SBTTL "IC_POST_JOURNAL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	! ID:IC028
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Transaction Journal\* post
	!	option posts entries,
	!	which have been made in the journal,
	!	to the General Ledger system.
	!	.b
	!	If the entries being posted are out of balance,
	!	the message, ^*"Batch
	!	is OUT OF BALANCE - POSTING IS ABORTED.
	!	Hit any key to continue",\*
	!	will appear on the screen.
	!	Any imbalance must be corrected before the
	!	posting can be executed.
	!	.b
	!	After the posting is completed, the system will return to the
	!	Transaction Journal menu.
	!	.lm -5
	!
	! Index:
	!	.x Post>Transaction Journal
	!	.x Transaction Journal>Post
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_POST_JOURNAL
	!	$ LINK/EXE=IC_EXE: IC_POST_JOURNAL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_POST_JOURNAL.OBJ;*
	!
	! Author:
	!
	!	01/31/1989 - Frank Starman
	!
	! Modification history:
	!
	!	05/30/1989 - B. Craig Larsen
	!		Modified to post to the Sub Account system.
	!
	!	12/18/1990 - Frank Starman
	!		Decrease number of arguments in IC_TRAN_POST function.
	!
	!	11/18/1991 - Kevin Handy
	!		Fixed stupid bug that caused it to loop back up the
	!		kill files (Complete), when it got an error in the
	!		kill files routine, causing failures.
	!
	!	02/04/1992 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	03/03/92 - Kevin Handy
	!		Changed "CMC$WARNING" to "CMC$_WARNING".
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/18/2000 - Kevin Handy
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
	! Include map statements
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURNAL.HB"
	MAP	(IC_JOURNAL)		IC_JOURNAL_CDD		IC_JOURNAL

	!
	! Include CDD
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"

	!
	! Declare internal variables
	!
	DECLARE GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	IC_TRANSACTION_CDD	IC_TRANSACTION
	DECLARE PD_ACCOUNT_CDD		PD_ACCOUNT_EXAM
	DECLARE PD_PRODUCT_CDD		PD_PRODUCT_EXAM
	DECLARE UTL_LOCATION_CDD	UTL_LOCATION_EXAM
	DECLARE UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE UTL_TRANSTYPE_CDD	UTL_TRANSTYPE_EXAM

	DECLARE LONG	EXIT_STATUS
	DECLARE LONG	CHECK_PERIOD
	DECLARE LONG	INTR_STATUS
	DECLARE LONG	PRNT_SUMMARY

	DECLARE REAL	COST_QTY_A

	DECLARE STRING	TITLE(10%)
	DECLARE STRING	BATCH_NUMBER
	DECLARE STRING	ICPERIOD
	DECLARE STRING	IC.INTER.PERIOD
	DECLARE STRING	CHECK_DATE
	DECLARE STRING	POSTDATE
	DECLARE STRING	POSTTIME

	!
	! External functions
	!
	EXTERNAL	LONG	FUNCTION IC_TRAN_POST
	EXTERNAL	LONG	FUNCTION GL_TRAN_POSTGL

	EXTERNAL	LONG	FUNCTION GL_EXAM_CHART
	EXTERNAL	LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL	LONG	FUNCTION PD_READ_ACCOUNT
	EXTERNAL	LONG	FUNCTION SB_EXAM_SUBACCOUNT
	EXTERNAL	LONG	FUNCTION UTL_EXAM_LOCATION
	EXTERNAL	LONG	FUNCTION UTL_EXAM_TRANSTYPE
	EXTERNAL	LONG	FUNCTION ASSG_POSTBATCH
	EXTERNAL	LONG	FUNCTION OUTP_UNDEFCODES

	%PAGE

	ON ERROR GOTO 19000

 Init:	!==============================================================
	! OPEN THE PRINT CONTROL FILE
	!==============================================================

	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	BATCH_NO$ = TRM$(UTL_REPORTX::OPTDEF(0%))
	!++
	! Abstract:FLD01
	!	^*(01) Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters the number
	!	of the batch which is to be posted.
	!	.b
	!	Only one batch at a time may be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Post
	!	.x Post>Batch Number
	!
	!--

	ICPERIOD = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)
	!++
	! Abstract:FLD02
	!	^*(02) Posting Period\*
	!	.b
	!	.lm +5
	!	The ^*Posting Period\* field enters the
	!	Inventory Control accounting period into which this batch
	!	will be posted.
	!	.b
	!	The format for entry is YYYYPP.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Inventory Period >Post
	!	.x Post>Inventory Period
	!
	!--

	CHECK_DATE = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)
	!++
	! Abstract:FLD04
	!	^*(04) Check Date\*
	!	.b
	!	.lm +5
	!	The ^*Check Date\* field
	!	posts all transactions or
	!	checks the date of the transaction
	!	to see it falls between specified dates
	!	before posting. A ^*Y\* entry causes
	!	the dates to be checked, while
	!	an ^*N\* entry causes all transactions to be posted.
	!	.lm -5
	!
	! Index:
	!	.x Check Dates
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF CHECK_DATE = "Y"

	!
	! Title
	!
	TITLE(1%) = "INVENTORY  JOURNAL  POSTING  PROTOCOL"
	TITLE(2%) = "Inventory Control System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

300	!
	! Open the IC journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURNAL.UPD"
	USE
		FILENAME$ = "IC_JOURNAL"
		CONTINUE HelpError
	END WHEN

	%PAGE

	!******************************************************************
	! Check if process has been interrupted
	!******************************************************************
	!
	! Open up batch control file and check if interrupted
	!
	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "IC_JOURNAL", BATCH_NO$, &
		IC.INTER.PERIOD, "")

	SELECT INTR_STATUS

	!
	! Success
	!
	CASE CMC$_NORMAL

	!
	! Process was interrupted
	!
	CASE CMC$_WARNING

		IF IC.INTER.PERIOD <> ""
		THEN
			GOTO Aborted &
				IF IC_TRAN_POST(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", &
				IC.INTER.PERIOD) <> CMC$_NORMAL

			GOTO Aborted &
				IF GL_TRAN_POSTGL(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", &
				IC.INTER.PERIOD) <> CMC$_NORMAL
		END IF

	!
	! Something else wrong
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

 AssignBatch:
	!******************************************************************
	! Assign batch number and open control files
	!******************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "IC_JOURNAL", BATCH_NO$, &
		ICPERIOD, "") <> CMC$_NORMAL

	EXIT_STATUS = IC_TRAN_POST(OPT_CHECK, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", ICPERIOD)

	SELECT EXIT_STATUS
	!
	! Success
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something else wrong
	!
	CASE ELSE
		GOTO Aborted
	END SELECT

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CHECK, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", ICPERIOD)

	SELECT EXIT_STATUS
	!
	! Success
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something else wrong
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

 CreateTrans:
	!******************************************************************
	! Create transmittal
	!******************************************************************
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", "", "") <> CMC$_NORMAL

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	!
	! Blank flags
	!
	PRODUCT$, LOCATION$, TRANSTYPEA$, TRANSTYPEB$, TRANDATE$, &
		INVACCT$, EXPACCT$ = " "

	RESET #IC_JOURNAL.CH%

 NextRec:
	!
	! Get next journal record
	!
1000	WHEN ERROR IN
		GET #IC_JOURNAL.CH%
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
		FILENAME$ = "IC_JOURNAL"
		CONTINUE HelpError
	END WHEN

	!
	! Test product number
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(IC_JOURNAL::PRODUCT, PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS
	!
	! Code found; go on
	!
	CASE CMC$_NORMAL

	!
	! Product number undefined; set flag and go on
	!
	CASE ELSE
		PRODUCT$ = "*"

	END SELECT

	!
	! Test Location number
	!
	EXIT_STATUS = UTL_EXAM_LOCATION(IC_JOURNAL::LOCATION, UTL_LOCATION_EXAM)

	SELECT EXIT_STATUS
	!
	! Code found; go on
	!
	CASE CMC$_NORMAL

	!
	! Location number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		LOCATION$ = "*"

	!
	! Something's going wrong here
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Test transaction type B (If we need to)
	!
	GOTO SkipTypeB IF (TRM$(IC_JOURNAL::TYPE_B) = "")

	EXIT_STATUS = UTL_EXAM_TRANSTYPE(IC_JOURNAL::TYPE_B, UTL_TRANSTYPE_EXAM)

	SELECT EXIT_STATUS
	!
	! Code found; set "B" coefficient and go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and coefficient and go on
	!
	CASE CMC$_UNDEFINED
		TRANSTYPEB$ = "*"

	!
	! Other things going wrong
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

 SkipTypeB:
	!
	! Test transaction type A
	!
	EXIT_STATUS = UTL_EXAM_TRANSTYPE(IC_JOURNAL::TYPE_A, UTL_TRANSTYPE_EXAM)

	SELECT EXIT_STATUS
	!
	! Code found; set "A" coefficient and go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TRANSTYPEA$ = "*"

	!
	! Something else going on here
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	COST_QTY_A = FUNC_ROUND(IC_JOURNAL::COST * IC_JOURNAL::QUANTITY_A, 2%)
	!
	! Generate an IC transaction ledger record
	!
	IC_TRANSACTION::PRODUCT		= IC_JOURNAL::PRODUCT
	IC_TRANSACTION::LOCATION	= IC_JOURNAL::LOCATION
	IC_TRANSACTION::TRANS_DATE	= IC_JOURNAL::TRANS_DATE
	IC_TRANSACTION::PRIMARY_REF	= IC_JOURNAL::PRIMARY_REF
	IC_TRANSACTION::CROSS_REF	= IC_JOURNAL::CROSS_REF
	IC_TRANSACTION::SUBACCOUNT	= IC_JOURNAL::SUBACCOUNT
	IC_TRANSACTION::LOT		= ""
	IC_TRANSACTION::STATIONMAN	= IC_JOURNAL::STATIONMAN
	IC_TRANSACTION::TYPE_A		= IC_JOURNAL::TYPE_A
	IC_TRANSACTION::QUANTITY_A	= IC_JOURNAL::QUANTITY_A
	IC_TRANSACTION::TYPE_B		= IC_JOURNAL::TYPE_B
	IC_TRANSACTION::QUANTITY_B	= IC_JOURNAL::QUANTITY_B
	IC_TRANSACTION::COST		= ABS(COST_QTY_A)
	IC_TRANSACTION::PRICE		= 0.0
	IC_TRANSACTION::TRANSACCT	= IC_JOURNAL::EXPACCT
	IC_TRANSACTION::POSTDATE	= POSTDATE
	IC_TRANSACTION::POSTTIME	= POSTTIME
	IC_TRANSACTION::BATCH		= BATCH_NUMBER

	!
	! Post to inventory transaction file
	!
	EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, IC_TRANSACTION, &
		ICPERIOD)

	!
	! Was the date out of the given range?
	!
	SELECT EXIT_STATUS
	!
	! Date good; go on
	!
	CASE CMC$_NORMAL

	!
	! Date out of range; set flag and go on
	!
	CASE CMC$_DATEOUT
		TRANDATE$ = "*"

	!
	! Other things going on
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Check if to location is there
	!
	GOTO PostToGL IF TRM$(IC_JOURNAL::TOLOCATION) = ""

	!
	! Test to Location number
	!
	EXIT_STATUS = UTL_EXAM_LOCATION(IC_JOURNAL::TOLOCATION, &
		UTL_LOCATION_EXAM)

	SELECT EXIT_STATUS
	!
	! Code found; go on
	!
	CASE CMC$_NORMAL

	!
	! Location number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TOLOCATION$ = "*"

	!
	! Something's going wrong here
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Generate an IC transaction ledger record
	!
	IC_TRANSACTION::LOCATION	= IC_JOURNAL::TOLOCATION
	IC_TRANSACTION::QUANTITY_A	= -IC_JOURNAL::QUANTITY_A

	!
	! Post to inventory transaction file
	!
	EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, IC_TRANSACTION, &
		ICPERIOD)

 PostToGL:
	!
	! Check if we need to post to GL and SA
	!
	GOTO GoToNext IF (PD_PRODUCT_EXAM::METHOD <> "STD") OR &
		(UTL_TRANSTYPE_EXAM::CLASS <> "01") OR &
		(COST_QTY_A = 0.0)

	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::SOURCE	= "IJ"
	GL_YYYY_PP::REFNO	= IC_JOURNAL::PRIMARY_REF
	GL_YYYY_PP::TRANDAT	= IC_JOURNAL::TRANS_DATE
	GL_YYYY_PP::DESCR	= IC_JOURNAL::PRODUCT + IC_JOURNAL::LOCATION
	GL_YYYY_PP::XREFNO	= IC_JOURNAL::CROSS_REF
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= IC_JOURNAL::SUBACCOUNT
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	GOTO InvAcct IF TRM$(IC_JOURNAL::EXPACCT) = ""

	!
	! Is Expense account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(IC_JOURNAL::EXPACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS
	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		EXPACCT$ = "*"

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Finish the Expense record
	!
	GL_YYYY_PP::ACCT	= IC_JOURNAL::EXPACCT
	GL_YYYY_PP::AMOUNT	= (-1.0) * COST_QTY_A
	GL_YYYY_PP::UNITS	= (-1.0) * IC_JOURNAL::QUANTITY_A
	GL_YYYY_PP::DESCR	= GL_CHART_EXAM::DESCR

	!
	! Post the Expense side
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, &
		GL_CHART_EXAM, ICPERIOD)

	!
	! Was the date out of the given range?
	!
	SELECT EXIT_STATUS
	!
	! Date OK; set flag and go on
	!
	CASE CMC$_NORMAL

	!
	! Date out; set flag (if not already set) and go on
	!
	CASE CMC$_DATEOUT
		TRANDATE$ = "*"

	!
	! Warning; check subaccount number
	!
	CASE CMC$_WARNING
		!
		! See if Sub Account number is defined
		!
		EXIT_STATUS = SB_EXAM_SUBACCOUNT("J", &
			IC_JOURNAL::SUBACCOUNT, SB_SUBACCOUNT_EXAM)

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

		END SELECT

	!
	! Strange Days
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

 InvAcct:
	!
	! Get the inventory account number
	!
	EXIT_STATUS = PD_READ_ACCOUNT(IC_JOURNAL::LOCATION, &
		PD_PRODUCT_EXAM::PROD_TYPE, &
		PD_ACCOUNT_EXAM)

	SELECT EXIT_STATUS
	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		INVACCT$ = "*"

	!
	! Weird happenings
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Is the inventory account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_EXAM::INVACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS
	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		INVACCT$ = "*"

	!
	! Weird happenings
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Finish the Inventory record
	!
	GL_YYYY_PP::ACCT	= PD_ACCOUNT_EXAM::INVACCT
	GL_YYYY_PP::AMOUNT	= COST_QTY_A
	GL_YYYY_PP::UNITS	= IC_JOURNAL::QUANTITY_A
	GL_YYYY_PP::DESCR	= GL_CHART_EXAM::DESCR

	!
	! Post the Inventory record
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, &
		GL_CHART_EXAM, ICPERIOD)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Warning; check subaccount number
	!
	CASE CMC$_WARNING
		!
		! See if Sub Account number is defined
		!
		EXIT_STATUS = SB_EXAM_SUBACCOUNT("J", IC_JOURNAL::SUBACCOUNT, &
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
		! Strange Days
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

	!
	! Strange Days
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Check if to location is there
	!
	GOTO GoToNext IF TRM$(IC_JOURNAL::TOLOCATION) = "" OR &
		IC_JOURNAL::EXPACCT <> ""

	!
	! Get the inventory account number
	!
	EXIT_STATUS = PD_READ_ACCOUNT(IC_JOURNAL::TOLOCATION, &
		PD_PRODUCT_EXAM::PROD_TYPE, &
		PD_ACCOUNT_EXAM)

	SELECT EXIT_STATUS
	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TOINVACCT$ = "*"

	!
	! Weird happenings
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Is the inventory account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_EXAM::INVACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS
	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TOINVACCT$ = "*"

	!
	! Weird happenings
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Finish the Inventory record
	!
	GL_YYYY_PP::ACCT	= PD_ACCOUNT_EXAM::INVACCT
	GL_YYYY_PP::AMOUNT	= -COST_QTY_A
	GL_YYYY_PP::UNITS	= -IC_JOURNAL::QUANTITY_A
	GL_YYYY_PP::DESCR	= GL_CHART_EXAM::DESCR

	!
	! Post the Inventory record
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, &
		GL_CHART_EXAM, ICPERIOD)

 GoToNext:
	!
	! Check for undefined codes
	!
	IF INSTR(1%, PRODUCT$ + LOCATION$ + TOLOCATION$ + TRANSTYPEA$ + &
		SUBACCT$ + TRANSTYPEB$ + TRANDATE$ + INVACCT$ + TOINVACCT$ + &
		EXPACCT$, "*")
	THEN
		TEXT$ = PRODUCT$ + IC_JOURNAL::PRODUCT + " " + &
			LOCATION$ + IC_JOURNAL::LOCATION + " " + &
			TOLOCATION$ + IC_JOURNAL::TOLOCATION + " " + &
			TRANDATE$ + &
			PRNT_DATE(IC_JOURNAL::TRANS_DATE, 8%) + " " + &
			TRANSTYPEA$ + IC_JOURNAL::TYPE_A + " " + &
			FORMAT$(IC_JOURNAL::QUANTITY_A, "######.### ") + &
			TRANSTYPEB$ + IC_JOURNAL::TYPE_B + " " + &
			FORMAT$(IC_JOURNAL::QUANTITY_B, "######.### ") + &
			EXPACCT$ + IC_JOURNAL::EXPACCT + " " + &
			INVACCT$ + GL_YYYY_PP::ACCT + " " + &
			SUBACCT$ + IC_JOURNAL::SUBACCOUNT

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC,TITLE(), UTL_REPORTX, &
			TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		PRODUCT$, LOCATION$, TOLOCATION$, TRANSTYPEA$, &
			TRANSTYPEB$, TRANDATE$, INVACCT$, TOINVACCT$, &
			EXPACCT$ = " "

	END IF

	GOTO NextRec

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", ICPERIOD)

	EXIT_STATUS = EXIT_STATUS AND IC_TRAN_POST(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", ICPERIOD)

	EXIT_STATUS = EXIT_STATUS AND OUTP_UNDEFCODES(OPT_CONFIRM, TITLE(), &
		UTL_REPORTX, "")

	GOTO Aborted IF EXIT_STATUS <> CMC$_NORMAL

	!******************************************************************
	! Posting
	!******************************************************************
	!
	! Post the IC Journal to the Transaction ledger
	!
	GOTO Interrupt IF IC_TRAN_POST(OPT_POSTFILE, SUBOPT_LEDGER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", ICPERIOD) <> CMC$_NORMAL

	!
	! Post to GL
	!
	GOTO Interrupt IF GL_TRAN_POSTGL(OPT_POSTFILE, SUBOPT_DETAIL, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", ICPERIOD) <> CMC$_NORMAL

	!
	! Remove file
	!
1500	CLOSE IC_JOURNAL.CH%

	CALL READ_DEVICE("IC_JOURNAL", IC_JOURNAL.DEV$, STAT%)

1510 !	WHEN ERROR IN
 !		KILL IC_JOURNAL.DEV$ + "IC_JOURNAL_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE Complete
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(IC_JOURNAL.DEV$ + &
		"IC_JOURNAL_" + BATCH_NO$ + ".JRL;*")

 Complete:
	!******************************************************************
	! Complete process and delete journal
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", "", "")

	PRNT_SUMMARY = SUBOPT_FINAL

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************
	!
	! Skip printing the transmittal if we aren't posting to GL
	!
	!
	! Print credit and debit transmittal
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_SUMMARY, PRNT_SUMMARY, &
		BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", ICPERIOD)

	!
	! Print undefined code if any
	!
	TEXT$ = " Product#        Loc  Loc TransDate   A       Qty" + &
		"  B       Qty  ExpAccountNumber    " + &
		"InvAccountNumber1   InvAccountNumber2   SubAccount"

	EXIT_STATUS = OUTP_UNDEFCODES(OPT_SUMMARY, TITLE(), UTL_REPORTX, TEXT$)

	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF (TRM$(UTL_REPORTX::NEXTRUN) = "")
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
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, TITLE(), &
			UTL_REPORTX, "", "", "", "")
		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", "", "")

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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
	!+-+-+
	!++
	! Abstract:FLD03
	!	^*(03) Period to General Ledger\*
	!	.b
	!	.lm +5
	!	The ^*Period to General Ledger\* field enters the
	!	General Ledger accounting period into which this batch
	!	will be posted.
	!	.b
	!	The format for entry is YYYYPP.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Period to General Ledger>Post
	!	.x Post>Period to General Ledger
	!
	!--
