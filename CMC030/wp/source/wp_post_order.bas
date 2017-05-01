1	%TITLE "Post Job Journal"
	%SBTTL "WP_POST_ORDER"
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
	! ID:WP0010
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The execution of the ^*Post Job Journal\* option transfers
	!	information from Job Journal to the Work in Process Register.
	!	.lm -5
	!
	! Index:
	!	.x Post>Job Journal
	!	.x Job Journal>Post
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_POST_ORDER/LINE
	!	$ LINK/EXE=WP_EXE: WP_POST_ORDER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_POST_ORDER.OBJ;*
	!
	! Author:
	!
	!	05/29/91 - Val James Allen
	!
	! Modification history:
	!
	!	05/06/92 - Dan Perkins
	!		Moved Opens before ASSG_POSTBATCH so program
	!		would work properly.
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	12/08/92 - Frank F. Starman
	!		Fixed error trapping on line 330.
	!
	!	06/08/93 - Frank F. Starman
	!		Added WP_REQLINE
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	07/28/95 - Kevin Handy
	!		Format closer to 80 columns.
	!
	!	06/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.HB"
	MAP (WP_JOB)		WP_JOB_CDD		WP_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.HB"
	MAP (WP_ORDERLINE)	WP_ORDERLINE_CDD	WP_ORDERLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)		WP_REGLINE_CDD	WP_REGLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.HB"
	MAP (WP_REQLINE)	WP_REQLINE_CDD		WP_REQLINE

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION UTL_EXAM_LOCATION
	EXTERNAL LONG	FUNCTION IC_TRAN_POST
	EXTERNAL LONG	FUNCTION WP_TRAN_POST
	EXTERNAL LONG	FUNCTION WP_TRAN_POSTREQ
	EXTERNAL LONG	FUNCTION ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION OUTP_UNDEFCODES
	EXTERNAL REAL	FUNCTION PC_READ_COST

	!
	! Declare internal variables
	!
	DECLARE JC_JOB_CDD		JC_JOB
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE PD_PRODUCT_CDD		PD_PRODUCT_EXAM
	DECLARE UTL_LOCATION_CDD	UTL_LOCATION_EXAM
	DECLARE	IC_TRANSACTION_CDD	IC_TRANSACTION

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE LONG			CHECK_PERIOD

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
	TITLE(1%) = "WORK  ORDER  JOURNAL  POSTING  PROTOCOL"
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
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field
	!	enters the batch number to be posted.
	!	.b
	!	An entry is required in this field.
	!	.b
	!	Only one batch at a time may be posted.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Post Journal
	!	.x Post Journal>Batch Number
	!
	! Required:
	!--

	ICPERIOD = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) Inventory Period\*
	!	.b
	!	.lm +5
	!	The ^*Inventory Period\* field
	!	enters the Inventory Control accounting period into which
	!	a batch will be posted.
	!	.b
	!	The format for entry is YYYYPP.
	!	.b
	!	This field requires a valid entry.
	!	.lm -5
	!
	! Index:
	!	.X Post>Inventory Period
	!	.X Inventory Period>Post
	!	.X Post>Period
	!	.X Period>Post
	!
	!--

	CHECK_DATE = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Check Date\*
	!	.b
	!	.lm +5
	!	The ^*Check Date\* field
	!	posts transactions with or without checking dates. A ^*Y\* value
	!	causes the dates in the journal records to be compared with the Inventory
	!	Ledger period into which the journal is to be posted.  If any journal record
	!	dates are outside the range of the period in which they are to be posted, the
	!	system will warn the user and ask if the execution of the posting is to be
	!	continued or aborted.  An ^*N\* value will cause the system to not compare
	!	journal record dates with the period into which they are to be posted.
	!
	! Index:
	!	.x Check Dates
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF CHECK_DATE = "Y"

300	!
	! Open WIP Order header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.UPD"
	USE
		FILENAME$ = "WP_JOB"
		CONTINUE HelpError
	END WHEN

310	!
	! Open WIP Journal line items file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.UPD"
	USE
		FILENAME$ = "WP_ORDERLINE"
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

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

340	!
	! Open WIP Journal line items file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.UPD"
	USE
		CONTINUE 345 IF ERR = 5%
		FILENAME$ = "WP_REQLINE"
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
345	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "WP_JOB", BATCH_NO$, IC.INTER.PERIOD, "")

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
				WP_TRAN_POST(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"", "") <> CMC$_NORMAL

			GOTO Aborted IF WP_TRAN_POSTREQ(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "") <> CMC$_NORMAL

			GOTO Aborted IF IC_TRAN_POST(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", &
				IC.INTER.PERIOD) <> CMC$_NORMAL
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
		UTL_REPORTX, "WP_JOB", BATCH_NO$, &
		ICPERIOD, "") <> CMC$_NORMAL

	EXIT_STATUS = WP_TRAN_POST(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "")

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

	EXIT_STATUS = WP_TRAN_POSTREQ(OPT_CHECK, SUBOPT_NOOPT, &
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
		UTL_REPORTX, "WP_JOB", BATCH_NO$, "", "") <> CMC$_NORMAL

	RESET #WP_JOB.CH%

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	PRODUCT$, LOCATION$ = ""

 ReadHeader:
	!
	! Read in one record from the header file
	!
3000	WHEN ERROR IN
		GET #WP_JOB.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE Confirm IF ERR = 155% OR ERR = 11%
		FILENAME$ = "WP_JOB"
		CONTINUE HelpError
	END WHEN

	!
	! Set some initial variable values
	!
	LINECNT$ = "0000"
	APPEND$ = "N"

	!
	! Check location
	!
	EXIT_STATUS = UTL_EXAM_LOCATION(WP_JOB::LOCATION, UTL_LOCATION_EXAM)

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
	! Find out if this is and append to register or completely new
	!
3010	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, KEY #0% EQ WP_JOB::JOB, REGARDLESS
	USE
		CONTINUE ProcessLines IF ERR = 155% OR ERR = 9%
		FILENAME$ =  "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	APPEND$ = "Y"

3020	WHEN ERROR IN
		GET #WP_REGLINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessLines IF ERR = 11%
		FILENAME$ =  "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO ProcessLines IF WP_REGLINE::JOB <> WP_JOB::JOB

	LINECNT$ = WP_REGLINE::LLINE

	GOTO 3020

 ProcessLines:
	!
	! Find the first line item for the header
	!
3100	WHEN ERROR IN
		FIND #WP_ORDERLINE.CH%, KEY #0% EQ WP_JOB::JOB
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader IF ERR = 155%
		FILENAME$ = "WP_ORDERLINE"
		CONTINUE HelpError
	END WHEN

 LineItem:
	!
	! Get the (next) line item
	!
3200	WHEN ERROR IN
		GET #WP_ORDERLINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader IF ERR = 11%
		FILENAME$ = "WP_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the header if we're done with the line items
	!
	GOTO ProcessHeader IF WP_ORDERLINE::JOB <> WP_JOB::JOB

	V% = FUNC_INCREMENT(LINECNT$)

	WP_REGLINE::JOB		= WP_ORDERLINE::JOB
	WP_REGLINE::LLINE	= LINECNT$
	WP_REGLINE::REC_TYPE	= "01"
	WP_REGLINE::ITEMCODE	= WP_ORDERLINE::ITEMCODE
	WP_REGLINE::QTY		= WP_ORDERLINE::QTY
	WP_REGLINE::COST	= WP_ORDERLINE::COST
	WP_REGLINE::POST_DATE	= POSTDATE
	WP_REGLINE::POST_TIME	= POSTTIME
	WP_REGLINE::BATCH	= BATCH_NUMBER
	WP_REGLINE::TTYPE	= WP_ORDERLINE::TTYPE
	WP_REGLINE::DESCR	= WP_ORDERLINE::DESCR
	WP_REGLINE::START_DATE  = WP_ORDERLINE::START_DATE
	WP_REGLINE::COMP_DATE   = WP_ORDERLINE::COMP_DATE

	!
	! Call the post function for orders
	!
	GOTO Aborted IF WP_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		JC_JOB, WP_REGLINE) <> CMC$_NORMAL

	!
	! Bypass post to inventory if not a Material line
	!
	GOTO LineItem IF WP_REGLINE::TTYPE <> "M"

	!
	! Check Product
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(WP_ORDERLINE::ITEMCODE, PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED, CMC$_WARNING
		PRODUCT$ = "*"

	!
	! Untrapped error
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	IC_TRANSACTION::PRODUCT		= WP_ORDERLINE::ITEMCODE
	IC_TRANSACTION::LOCATION	= WP_JOB::LOCATION
	IC_TRANSACTION::TRANS_DATE	= WP_JOB::BDATE
	IC_TRANSACTION::PRIMARY_REF	= ""
	IC_TRANSACTION::CROSS_REF	= ""
	IC_TRANSACTION::SUBACCOUNT	= WP_ORDERLINE::JOB
	IC_TRANSACTION::LOT		= RIGHT(WP_REGLINE::LLINE, 3%)
	IC_TRANSACTION::STATIONMAN	= WP_JOB::OPERATOR
	IC_TRANSACTION::TYPE_A		= "WO"
	IC_TRANSACTION::QUANTITY_A	= WP_ORDERLINE::QTY
	IC_TRANSACTION::TYPE_B		= ""
	IC_TRANSACTION::QUANTITY_B	= 0.0
	IC_TRANSACTION::COST		= &
		FUNC_ROUND(WP_ORDERLINE::COST * WP_ORDERLINE::QTY, 2%)
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
	! Check for undefined codes
	!
	IF INSTR(1%, PRODUCT$, "*")
	THEN
		TEXT$ = WP_JOB::JOB + " " + &
			LOCATION$ + WP_JOB::LOCATION + " " + &
			PRODUCT$ + WP_ORDERLINE::ITEMCODE

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

	LASTREQLINE$ = "0000"

	!
	! Find the first line item for the header
	!
3300	WHEN ERROR IN
		FIND #WP_REQLINE.CH%, &
			KEY #0% EQ WP_JOB::JOB + WP_ORDERLINE::LLINE
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE LineItem IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

 RQLineItem:
	!
	! Get the (next) line item
	!
3400	WHEN ERROR IN
		GET #WP_REQLINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE LineItem IF ERR = 11%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up  if we're done with the line items
	!
	GOTO LineItem IF WP_REQLINE::JOB + WP_REQLINE::LLINE <> &
		WP_JOB::JOB + WP_ORDERLINE::LLINE

	!
	! Check Product
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(WP_REQLINE::PRODUCT, PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED, CMC$_WARNING
		PRODUCT$ = "*"

	!
	! Untrapped error
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	COST = PC_READ_COST(WP_REQLINE::PRODUCT, WP_JOB::LOCATION, &
		WP_JOB::BDATE, "")

	PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
		IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

	IF WP_REQLINE::REQNUM = "" AND (TRM$(WP_REQLINE::REQLINE) = "" OR &
		WP_REQLINE::REQLINE = "0000")
	THEN
		V% = FUNC_INCREMENT(LASTREQLINE$)
	ELSE
		LASTREQLINE$ = WP_REQLINE::REQLINE
	END IF

	WP_REQREGISTER::JOB = WP_REGLINE::JOB
	WP_REQREGISTER::LLINE = WP_REGLINE::LLINE
	WP_REQREGISTER::REQNUM = WP_REQLINE::REQNUM
	WP_REQREGISTER::REQLIN = LASTREQLINE$
	WP_REQREGISTER::RECTYP   = "01"
	WP_REQREGISTER::PRODUCT  = WP_REQLINE::PRODUCT
	WP_REQREGISTER::LOCATION = WP_JOB::LOCATION
	WP_REQREGISTER::QTY      = WP_REQLINE::QTY
	WP_REQREGISTER::AMT      = COST
	WP_REQREGISTER::TRANDATE = WP_JOB::BDATE
	WP_REQREGISTER::OPERATOR = WP_JOB::OPERATOR
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

	IC_TRANSACTION::PRODUCT		= WP_REQLINE::PRODUCT
	IC_TRANSACTION::LOCATION	= WP_JOB::LOCATION
	IC_TRANSACTION::TRANS_DATE	= WP_JOB::BDATE

	IC_TRANSACTION::PRIMARY_REF	= RIGHT(WP_REQLINE::LLINE, 3%) + &
		WP_REQLINE::REQNUM + WP_REQLINE::REQLINE

	IC_TRANSACTION::CROSS_REF	= ""
	IC_TRANSACTION::SUBACCOUNT	= WP_REQLINE::JOB
	IC_TRANSACTION::LOT		= ""
	IC_TRANSACTION::STATIONMAN	= WP_JOB::OPERATOR
	IC_TRANSACTION::TYPE_A		= "RQ"

	IC_TRANSACTION::QUANTITY_A	= FUNC_ROUND( &
		-WP_REQLINE::QTY / PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%)

	IC_TRANSACTION::TYPE_B		= ""
	IC_TRANSACTION::QUANTITY_B	= 0.0

	IC_TRANSACTION::COST		= ABS(FUNC_ROUND(COST * &
		WP_REQLINE::QTY / PD_PRODUCT_EXAM::PRODUCT_FACTOR, 2%))

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
	! Check for undefined codes
	!
	IF INSTR(1%, PRODUCT$ + LOCATION$, "*")
	THEN
		TEXT$ = WP_REQLINE::JOB              + " " + &
			WP_REQLINE::LLINE            + " " + &
			LOCATION$ + WP_JOB::LOCATION + " " + &
			PRODUCT$ + WP_REQLINE::PRODUCT

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

	GOTO RQLineItem

 ProcessHeader:
	!
	! Process the header
	!
	!
	! Generate a JC_JOB record to pass through to the
	! post function
	!
	GOTO BypassHeader IF APPEND$ = "Y"

	JC_JOB::JOB		= WP_JOB::JOB
	JC_JOB::SUBJECT		= "J"
	JC_JOB::DESCR		= WP_JOB::DESCR
	JC_JOB::TTYPE		= WP_JOB::TTYPE
	JC_JOB::CLASS		= WP_JOB::CLASS
	JC_JOB::BDATE		= WP_JOB::BDATE
	JC_JOB::SSTATUS		= "A"
	JC_JOB::EDATE		= ""
	JC_JOB::LOCATION	= WP_JOB::LOCATION
	JC_JOB::OPERATOR	= WP_JOB::OPERATOR
	JC_JOB::REFNO		= WP_JOB::REFNO
	JC_JOB::BATCH		= BATCH_NUMBER
	JC_JOB::POST_TIME	= POSTTIME
	JC_JOB::POST_DATE	= POSTDATE

	!
	! Call the post function
	!
	GOTO Aborted IF WP_TRAN_POST(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		JC_JOB, WP_REGLINE) <> CMC$_NORMAL

	!
	! Check for undefined codes
	!
	IF INSTR(1%, LOCATION$, "*")
	THEN
		TEXT$ = WP_JOB::JOB + " " + &
			LOCATION$ + WP_JOB::LOCATION + "..."

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

 BypassHeader:
	GOTO ReadHeader

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************
	CLOSE WP_REGLINE.CH%

	EXIT_STATUS = IC_TRAN_POST(OPT_CONFIRM, SUBOPT_NOOPT, &
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
	! Post to the register header
	!
	GOTO Interrupt IF &
		WP_TRAN_POST(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	!
	! Post to the register lines
	!
	GOTO Interrupt IF &
		WP_TRAN_POST(OPT_POSTFILE, SUBOPT_LINEITEM, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	!
	! Post to the inventory ledger
	!
	GOTO Interrupt IF IC_TRAN_POST(OPT_POSTFILE, SUBOPT_LEDGER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", ICPERIOD) <> CMC$_NORMAL

	! Post to the register
	!
	GOTO Interrupt IF &
		WP_TRAN_POSTREQ(OPT_POSTFILE, SUBOPT_LINEITEM, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX,  "") <> CMC$_NORMAL

	%PAGE

	!
	! Remove files
	!
5000	CLOSE WP_JOB.CH%
	CLOSE WP_ORDERLINE.CH%
	CLOSE WP_REQLINE.CH%

5010 !	WHEN ERROR IN
 !		KILL WP_JOB.DEV$ + "WP_JOB_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5020
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(WP_JOB.DEV$ + &
		"WP_JOB_" + BATCH_NO$ + ".JRL;*")

5020 !	WHEN ERROR IN
 !		KILL WP_ORDERLINE.DEV$ + "WP_ORDERLINE_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5030
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(WP_REQLINE.DEV$ + &
		"WP_REQLINE_" + BATCH_NO$ + ".JRL;*")

5030 !	WHEN ERROR IN
 !		KILL WP_REQLINE.DEV$ + "WP_REQLINE_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE Complete
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(WP_REQLINE.DEV$ + &
		"WP_REQLINE_" + BATCH_NO$ + ".JRL;*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "WP_JOB", BATCH_NO$, "", "")

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************
	!
	! Print undefined codes (if any)
	!
	TEXT = "Job#         Loc   Product#"

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
	CALL HELP_PRINTMESS(SCOPE, "aborted at: " + &
		WP_JOB::JOB + WP_REGLINE::LLINE, &
		"E", "0", FILENAME$, NUM1$(EXIT_STATUS), &
		UTL_REPORTX, TITLE(), 0%)

	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "WP_JOB", BATCH_NO$, "", "")

		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "WP_JOB", BATCH_NO$, "", "")

	GOTO ExitProgram

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), UTL_REPORTX, TITLE(), 0%)

	!UTL_REPORTX::STAT = -1%
	GOTO Aborted

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of posting program WP_POST_ORDER
	!******************************************************************
	END
