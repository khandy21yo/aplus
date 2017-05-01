1	%TITLE "Post Issue Journal"
	%SBTTL "WP_POST_ISSUE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:WP0110
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Post Issue Journal\* option posts information from
	!	an Issue Journal to the Work in Process Requisitions Register.
	!	.b
	!	The execution of this process credits items issued from inventory and debits
	!	work in process.
	!	.lm -5
	!
	! Index:
	!	.x Issue Journal>Post
	!	.x Post>Issue Journal
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_POST_ISSUE/LINE
	!	$ LINK/EXE=WP_EXE: WP_POST_ISSUE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_POST_ISSUE.OBJ;*
	!
	! Author:
	!
	!	07/31/91 - Val James Allen
	!
	! Modification history:
	!
	!	08/06/91 - Craig Tanner
	!		Posting was not checking dates when specified to,
	!		so correted the error.
	!
	!	05/06/92 - Dan Perkins
	!		Moved Opens before ASSG_POSTBATCH so program
	!		would work properly.
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	09/03/92 - Dan Perkins
	!		Changed GL accounts to debit the WIP account and
	!		credit the inventory account.
	!
	!	09/22/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/04/92 - Dan Perkins
	!		Commented out 12 and 13 type records.
	!
	!	12/02/92 - Frank F. Starman
	!		Fixed several bugs.
	!
	!	12/10/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/04/93 - Frank F. Starman
	!		Use FUNC_ROUND before posting to GL.
	!
	!	03/19/93 - Frank F. Starman
	!		Notify UTL_BATCH file about GLPERIOD.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/18/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Lose unecessary function definitions
	!
	!	05/19/98 - Kevin Handy
	!		Add batch number to errors with file names.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/20/2000 - Kevin Handy
	!		Apply PRODUCT_FACTOR to posings to inventory tran.
	!		Use WHEN ERROR IN
	!
	!	07/31/2000 - Kevin Handy
	!		Lose a mess of commented out code.
	!
	!	09/19/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSJOUR.HB"
	MAP (WP_ISSJOUR)		WP_ISSJOUR_CDD		WP_ISSJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSLINE.HB"
	MAP (WP_ISSLINE)		WP_ISSLINE_CDD		WP_ISSLINE

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	DECLARE		JC_JOB_CDD	JC_JOB_EXAM

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)		WP_CONTROL_CDD		WP_CONTROL

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"


	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION	UTL_EXAM_LOCATION
	EXTERNAL LONG	FUNCTION	IC_TRAN_POST
	EXTERNAL LONG	FUNCTION	WP_TRAN_POSTREQ
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES
	EXTERNAL LONG   FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG   FUNCTION	GL_EXAM_CHART
	EXTERNAL LONG   FUNCTION	WP_READ_REQREGISTER
	EXTERNAL LONG	FUNCTION	PD_READ_ACCOUNT
	EXTERNAL LONG	FUNCTION	SB_EXAM_SUBACCOUNT

	!
	! Declare internal variables
	!
	DECLARE GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE PD_PRODUCT_CDD		PD_PRODUCT_EXAM
	DECLARE UTL_LOCATION_CDD	UTL_LOCATION_EXAM
	DECLARE	IC_TRANSACTION_CDD	IC_TRANSACTION
	DECLARE WP_REQREGISTER_CDD	WP_REQREGISTER, WP_REQREGISTER_READ
	DECLARE PD_ACCOUNT_CDD		PD_ACCOUNT_READ

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE	LONG			CHECK_PERIOD
	DECLARE LONG			PRNT_SUMMARY

	DECLARE STRING			GLPERIOD
	DECLARE STRING			GL.INTER.PERIOD
	DECLARE	STRING			ICPERIOD
	DECLARE	STRING			IC.INTER.PERIOD
	DECLARE	STRING			BATCH_NUMBER
	DECLARE STRING			CHECK_DATE
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
	TITLE(1%) = "ISSUE JOURNAL  POSTING  PROTOCOL"
	TITLE(2%) = "Work In Process System"
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

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^* (01) Batch Number\*
	!	.lm +5
	!	.b
	!	The ^*Batch Number\* field
	!	enters a two (2) digit batch number to be posted.
	!	.b
	!	An entry is required in this field.
	!	.b
	!	Only one batch at time may be posted.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Post>Issue Journal
	!	.x Post>Issue Journal>Batch Number
	!	.x Issue Journal>Post>Batch Number
	!
	! Required:
	!--

	ICPERIOD = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) Inventory Period\*
	!	.LM +5
	!	.B
	!	The ^*Inventory Period\* field
	!	enters the Inventory Control accounting period into which
	!	a batch will be posted.
	!	.B
	!	The format for this field is YYYYPP.
	!	.B
	!	This field requires an entry.
	!	.lm -5
	!
	! Index:
	!	.X Post>Inventory Period
	!	.X Inventory Period>Post
	!	.X Post>Period
	!	.X Period>Post
	!	.x Issue Journal>Post>Inventory Period
	!
	!--

	GLPERIOD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) General Ledger Period\*
	!	.lm +5
	!	.b
	!	The ^*General Ledger Period\* field
	!	enters the General Ledger accounting period into
	!	which a batch will be posted.
	!	.b
	!	The format for entry is YYYYPP.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger Period>Post>Issue Journal
	!	.x Post>Issue Journal>General Ledger Period
	!	.x Issue Journal>Post>General Ledger Period
	!
	!--

	CHECK_DATE = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Check Date\*
	!	.lm +5
	!	.b
	!	The ^*Check Date\* field
	!	posts all transactions with or without checking dates.
	!	A ^*Y\* value causes the dates in the journal records to be compared with the
	!	Inventory and General Ledger periods into which the journal is to be posted.
	!	If any journal record dates are outside the range of the period into which they
	!	are to be posted, the system will warn the user and ask if the execution of the
	!	posting is to be continued or aborted.  An ^*N\* value will cause the system to
	!	not compare journal record dates with the periods into which they are to be
	!	posted.
	!	.lm -5
	!
	! Index:
	!	.x Check Dates>Post>Issue Journal
	!	.x Issue Journal>Post>Check Dates
	!	.x Post>Issue Journal>Check Dates
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF CHECK_DATE = "Y"

300	!
	! Open WIP Order Issue header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_ISSJOUR.UPD"
	USE
		FILENAME$ = "WP_ISSJOUR_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

310	!
	! Open WIP  Issue Journal line items file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_ISSLINE.UPD"
	USE
		FILENAME$ = "WP_ISSLINE_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

320	!
	! Open control file, get control record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.OPN"

		GET #WP_CONTROL.CH%, RECORD 1%, REGARDLESS

		CLOSE WP_CONTROL.CH%
	USE
		FILENAME$ = "WP_CONTROL"
		CONTINUE HelpError
	END WHEN

	CALL ASSG_FREECHANNEL(WP_CONTROL.CH%)

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
		UTL_REPORTX, "WP_ISSJOUR", BATCH_NO$, &
		IC.INTER.PERIOD, GL.INTER.PERIOD)

	SELECT INTR_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Process was interrupted
	!
	CASE CMC$_WARNING

		IF TRM$(IC.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				WP_TRAN_POSTREQ(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "") <> CMC$_NORMAL
		END IF

		IF GL.INTER.PERIOD <> ""
		THEN
			GOTO Aborted IF GL_TRAN_POSTGL(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", &
				GL.INTER.PERIOD) <> CMC$_NORMAL
		END IF

		GOTO Aborted IF IC_TRAN_POST(OPT_RESTART, SUBOPT_NOOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, "", &
			IC.INTER.PERIOD) <> CMC$_NORMAL

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
		UTL_REPORTX, "WP_ISSJOUR", BATCH_NO$, &
		ICPERIOD, GLPERIOD) <> CMC$_NORMAL

	EXIT_STATUS = WP_TRAN_POSTREQ(OPT_CHECK, CHECK_PERIOD, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "")

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

	EXIT_STATUS = IC_TRAN_POST(OPT_CHECK, CHECK_PERIOD, BATCH_NUMBER, &
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

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CHECK, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD)

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
		UTL_REPORTX, "WP_ISSJOUR", BATCH_NO$, "", "") <> CMC$_NORMAL

	RESET #WP_ISSJOUR.CH%

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	PRODUCT$, INVACCT$, EXPACCT$, TRANDATE$, LOCATION$ = ""

 ReadHeader:
	!
	! Read in one record from the header file
	!
3000	WHEN ERROR IN
		GET #WP_ISSJOUR.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE Confirm IF ERR = 155% OR ERR = 11%
		FILENAME$ = "WP_ISSJOUR_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = SB_EXAM_SUBACCOUNT("J",WP_ISSJOUR::JOB,JC_JOB_EXAM)

	!
	! Check location
	!
	EXIT_STATUS = UTL_EXAM_LOCATION(JC_JOB_EXAM::LOCATION, &
		UTL_LOCATION_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		LOCATION$ = "*"

	!
	! Untrapped error
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Check for undefined codes
	!
	IF INSTR(1%, LOCATION$, "*")
	THEN
		TEXT$ = WP_ISSLINE::JOB + " " + &
			WP_ISSLINE::LLINE + " " + &
			WP_ISSLINE::REQNUM + " " + &
			WP_ISSLINE::REQLINE + " " + &
			LOCATION$ + JC_JOB_EXAM::LOCATION

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		LOCATION$ = " "

	END IF

	!
	! Find the first line item for the header
	!
3100	WHEN ERROR IN
		FIND #WP_ISSLINE.CH%, KEY #0% EQ WP_ISSJOUR::REQNUM + &
			WP_ISSJOUR::JOB + WP_ISSJOUR::LLINE, REGARDLESS
	USE
		CONTINUE ProcessHeader IF ERR = 155%
		FILENAME$ = "WP_ISSUELINE_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

 LineItem:
	!
	! Get the (next) line item
	!
3200	WHEN ERROR IN
		GET #WP_ISSLINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader IF ERR = 11%
		FILENAME$ = "WP_ISSLINE_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the  header if we're done with the line items
	!
	GOTO ProcessHeader &
		IF WP_ISSLINE::REQNUM <> WP_ISSJOUR::REQNUM OR &
		WP_ISSLINE::JOB <> WP_ISSJOUR::JOB OR &
		WP_ISSLINE::LLINE <> WP_ISSJOUR::LLINE

	V% = WP_READ_REQREGISTER(WP_ISSLINE::JOB, WP_ISSLINE::LLINE, &
		WP_ISSLINE::REQNUM + WP_ISSLINE::REQLINE, &
		"EQ", WP_REQREGISTER_READ, QTY())

	GOTO 3220 IF WP_ISSLINE::QTYISSUE = 0.0

	WP_REQREGISTER::JOB      = WP_ISSLINE::JOB
	WP_REQREGISTER::LLINE    = WP_ISSLINE::LLINE
	WP_REQREGISTER::REQNUM   = WP_ISSLINE::REQNUM
	WP_REQREGISTER::REQLIN   = WP_ISSLINE::REQLINE
	WP_REQREGISTER::RECTYP   = "02"
	WP_REQREGISTER::PRODUCT  = WP_ISSLINE::PRODUCT
	WP_REQREGISTER::LOCATION = JC_JOB_EXAM::LOCATION
	WP_REQREGISTER::QTY      = WP_ISSLINE::QTYISSUE
	WP_REQREGISTER::AMT      = &
		FUNC_ROUND(WP_ISSLINE::COST * WP_ISSLINE::QTYISSUE, 2%)
	WP_REQREGISTER::TRANDATE = WP_ISSLINE::ISSDATE
	WP_REQREGISTER::OPERATOR = WP_ISSJOUR::OPERATOR
	WP_REQREGISTER::PERIOD   = ICPERIOD
	WP_REQREGISTER::POSTDATE = POSTDATE
	WP_REQREGISTER::POSTTIME = POSTTIME
	WP_REQREGISTER::BATCH    = BATCH_NUMBER

	!
	! Call the post function for orders
	!
	GOTO Aborted IF WP_TRAN_POSTREQ(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		WP_REQREGISTER) <> CMC$_NORMAL

3220	GOTO 3230 IF WP_ISSLINE::QTYCANCEL = 0.0

	WP_REQREGISTER::JOB      = WP_ISSLINE::JOB
	WP_REQREGISTER::LLINE    = WP_ISSLINE::LLINE
	WP_REQREGISTER::REQNUM   = WP_ISSLINE::REQNUM
	WP_REQREGISTER::REQLIN   = WP_ISSLINE::REQLINE
	WP_REQREGISTER::RECTYP   = "03"
	WP_REQREGISTER::PRODUCT  = WP_ISSLINE::PRODUCT
	WP_REQREGISTER::LOCATION = JC_JOB_EXAM::LOCATION
	WP_REQREGISTER::QTY      = WP_ISSLINE::QTYCANCEL
	WP_REQREGISTER::AMT      = &
		FUNC_ROUND(WP_ISSLINE::COST * WP_ISSLINE::QTYCANCEL, 2%)
	WP_REQREGISTER::TRANDATE = WP_ISSLINE::ISSDATE
	WP_REQREGISTER::OPERATOR = WP_ISSJOUR::OPERATOR
	WP_REQREGISTER::PERIOD   = ICPERIOD
	WP_REQREGISTER::POSTDATE = POSTDATE
	WP_REQREGISTER::POSTTIME = POSTTIME
	WP_REQREGISTER::BATCH    = BATCH_NUMBER

	!
	! Call the post function for orders
	!
	GOTO Aborted IF WP_TRAN_POSTREQ(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		WP_REQREGISTER) <> CMC$_NORMAL

3230	!
	! Check Product
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(WP_ISSLINE::PRODUCT, PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		PRODUCT$ = "*"

	!
	! Untrapped error
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
		IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

	PLUGQTY = WP_ISSLINE::QTYISSUE + WP_ISSLINE::QTYCANCEL
	PLUGQTY = QTY(0%) IF ABS(PLUGQTY) > ABS(QTY(0%))

	IC_TRANSACTION::PRODUCT		= WP_ISSLINE::PRODUCT
	IC_TRANSACTION::LOCATION	= JC_JOB_EXAM::LOCATION
	IC_TRANSACTION::TRANS_DATE	= WP_ISSLINE::ISSDATE
	IC_TRANSACTION::PRIMARY_REF	= RIGHT(WP_ISSLINE::LLINE, 3%) + &
			WP_ISSLINE::REQNUM + WP_ISSLINE::REQLINE
	IC_TRANSACTION::CROSS_REF	= ""
	IC_TRANSACTION::SUBACCOUNT	= WP_ISSLINE::JOB
	IC_TRANSACTION::LOT		= ""
	IC_TRANSACTION::STATIONMAN	= WP_ISSJOUR::OPERATOR
	IC_TRANSACTION::TYPE_A		= "IS"
	IC_TRANSACTION::QUANTITY_A	= FUNC_ROUND( &
		-WP_ISSLINE::QTYISSUE / PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%)
	IC_TRANSACTION::TYPE_B		= "RQ"
	IC_TRANSACTION::QUANTITY_B	= FUNC_ROUND( &
		PLUGQTY / PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%)
	IC_TRANSACTION::COST		= &
		FUNC_ROUND(WP_ISSLINE::COST * WP_ISSLINE::QTYISSUE, 2%)
	IC_TRANSACTION::PRICE		= 0.0
	IC_TRANSACTION::TRANSACCT	= ""
	IC_TRANSACTION::POSTDATE	= POSTDATE
	IC_TRANSACTION::POSTTIME	= POSTTIME
	IC_TRANSACTION::BATCH		= BATCH_NUMBER

	!
	! Post order to inventory transaction file
	!
	EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)
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
	! Strange Days
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Check for undefined codes
	!
	IF INSTR(1%, TRANDATE$, "*")
	THEN
		TEXT$ = WP_ISSLINE::JOB + " " + &
			WP_ISSLINE::LLINE + " " + &
			WP_ISSLINE::REQNUM + " " + &
			WP_ISSLINE::REQLINE + " " + &
			TRANDATE$ + WP_ISSLINE::ISSDATE + &
			" IC DATE OUT OF RANGE"

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		TRANDATE$ = " "

	END IF

	!
	! Check for undefined codes
	!
	IF INSTR(1%, PRODUCT$, "*")
	THEN
		TEXT$ = WP_ISSLINE::JOB + " " + &
			WP_ISSLINE::LLINE + " " + &
			WP_ISSLINE::REQNUM + " " + &
			WP_ISSLINE::REQLINE + " " + &
			PRODUCT$ + WP_ISSLINE::PRODUCT

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		PRODUCT$ = " "

	END IF

	GOTO LineItem IF WP_ISSLINE::QTYISSUE = 0.0

 PostGLStuffline:
	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::SOURCE	= "IS"
	GL_YYYY_PP::REFNO	= JC_JOB_EXAM::REFNO
	GL_YYYY_PP::TRANDAT	= WP_ISSLINE::ISSDATE
	GL_YYYY_PP::DESCR	= WP_ISSLINE::PRODUCT + JC_JOB_EXAM::LOCATION
	GL_YYYY_PP::XREFNO	= ""
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= WP_ISSLINE::JOB
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	EXIT_STATUS = PD_READ_ACCOUNT(JC_JOB_EXAM::LOCATION, &
		PD_PRODUCT_EXAM::PROD_TYPE,PD_ACCOUNT_READ)

	!
	! Is WIP account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_READ::WIPACCT, GL_CHART_EXAM)

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
	! Check for undefined codes
	!
	IF INSTR(1%, EXPACCT$, "*")
	THEN
		TEXT$ = WP_ISSLINE::JOB + " " + &
			WP_ISSLINE::LLINE + " " + &
			WP_ISSLINE::REQNUM + " " + &
			WP_ISSLINE::REQLINE + " " + &
			EXPACCT$ + PD_ACCOUNT_READ::WIPACCT + " WIP ACCT"

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		EXPACCT$ = " "

	END IF

	!
	! Finish the record
	!
	GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::WIPACCT
	GL_YYYY_PP::AMOUNT	= &
		FUNC_ROUND(WP_ISSLINE::COST * WP_ISSLINE::QTYISSUE, 2%)
	GL_YYYY_PP::UNITS	= WP_ISSLINE::QTYISSUE
	GL_YYYY_PP::DESCR	= GL_CHART_EXAM::DESCR

	!
	! Post the Expense side
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, GLPERIOD)

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
	! Strange Days
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Check for undefined codes
	!
	IF INSTR(1%, TRANDATE$, "*")
	THEN
		TEXT$ = WP_ISSLINE::JOB + " " + &
			WP_ISSLINE::LLINE + " " + &
			WP_ISSLINE::REQNUM + " " + &
			WP_ISSLINE::REQLINE + " " + &
			TRANDATE$ + WP_ISSLINE::ISSDATE + &
			" GL DATE OUT OF RANGE"

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		TRANDATE$ = " "

	END IF

	!
	! Is inventory account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_READ::INVACCT, GL_CHART_EXAM)

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
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::INVACCT
	GL_YYYY_PP::AMOUNT	= &
		-FUNC_ROUND(WP_ISSLINE::COST * WP_ISSLINE::QTYISSUE, 2%)
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= GL_CHART_EXAM::DESCR

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, GLPERIOD)

	!
	! Check for undefined codes
	!
	IF INSTR(1%, INVACCT$, "*")
	THEN
		TEXT$ = WP_ISSLINE::JOB + " " + &
			WP_ISSLINE::LLINE + " " + &
			WP_ISSLINE::REQNUM + " " + &
			WP_ISSLINE::REQLINE + " " + &
			INVACCT$ + PD_ACCOUNT_READ::INVACCT + " INV ACCT"

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		INVACCT$ = " "

	END IF

	GOTO LineItem

 ProcessHeader:
	GOTO ReadHeader

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	EXIT_STATUS = EXIT_STATUS AND IC_TRAN_POST(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", ICPERIOD)

	EXIT_STATUS = EXIT_STATUS AND OUTP_UNDEFCODES(OPT_CONFIRM, TITLE(), &
		UTL_REPORTX, "")

	GOTO Aborted IF EXIT_STATUS <> CMC$_NORMAL

	!******************************************************************
	!	1) Since everything's correct and we have the user's
	!		permission, transfer all data from the temporary
	!		files into the ledger
	!******************************************************************
	!
	! Begin posting
	!
	! Post to the register lines
	!
	GOTO Interrupt IF WP_TRAN_POSTREQ(OPT_POSTFILE, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "") <> CMC$_NORMAL

	!
	! Post to the inventory ledger
	!
	GOTO Interrupt IF IC_TRAN_POST(OPT_POSTFILE, SUBOPT_LEDGER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", ICPERIOD) <> CMC$_NORMAL

	!
	! Post to GL
	!
	GOTO Interrupt IF GL_TRAN_POSTGL(OPT_POSTFILE, SUBOPT_DETAIL, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", GLPERIOD) <> CMC$_NORMAL

	%PAGE

	!
	! Remove files
	!
5000	CLOSE WP_ISSJOUR.CH%
	CLOSE WP_ISSLINE.CH%

5010 !	WHEN ERROR IN
 !		KILL WP_ISSJOUR.DEV$ + "WP_ISSJOUR_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5020
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(WP_ISSJOUR.DEV$ + &
		"WP_ISSJOUR_" + BATCH_NO$ + ".JRL;*")

5020 !	WHEN ERROR IN
 !		KILL WP_ISSLINE.DEV$ + "WP_ISSLINE_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE Complete
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(WP_ISSLINE.DEV$ + &
		"WP_ISSLINE_" + BATCH_NO$ + ".JRL;*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "WP_ISSJOUR", BATCH_NO$, "", "")

	PRNT_SUMMARY = SUBOPT_FINAL

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************

	!
	! Print credit and debit transmittal
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_SUMMARY, PRNT_SUMMARY, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	!
	! Print undefined codes (if any)
	!
	TEXT = ""

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
			TITLE(), UTL_REPORTX, "WP_ISSJOUR", BATCH_NO$, "", "")

		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "WP_ISSJOUR", BATCH_NO$, "", "")

	GOTO ExitProgram

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), &
		UTL_REPORTX, TITLE(), 0%)

	GOTO Aborted

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
	! End of posting program WP_POST_ISSUE
	!******************************************************************
	END
