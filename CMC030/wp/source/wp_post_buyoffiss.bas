1	%TITLE "Post Buy off/Issue Journal"
	%SBTTL "WP_POST_BUYOFFISS"
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
	! ID:WP0020
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Post Buy off/Issue Journal\* process
	!	transfers quantity data at standard cost
	!	from the buy off journal to a specified
	!	Finished Goods inventory, depending upon
	!	whether a work order is a finished inventory
	!	item or a finished equipment ledger
	!	item.  The appropriate accounts are posted
	!	to the General Ledger system as well.
	!	.lm -5
	!
	! Index:
	!	.x Post>Buy off
	!	.x Post>Issue
	!	.x Buy off>Post
	!	.x Issue>Post
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_POST_BUYOFFISS/LINE
	!	$ LINK/EXE=WP_EXE: WP_POST_BUYOFFISS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_POST_BUYOFFISS.OBJ;*
	!
	! Author:
	!
	!	05/29/91 - Val James Allen
	!
	! Modification history:
	!
	!	05/06/92 - Dan Perkins
	!		Put Opens before ASSG_POSTBATCH so the program
	!		will work properly.
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/18/92 - Dan Perkins
	!		Changed code "ROUND_FUNC" to FUNC_ROUND.
	!
	!	09/22/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/12/92 - Frank F. Starman
	!		Fixed RESUME on line 3100.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/01/92 - Dan Perkins
	!		Set WP_REGLINE::TTYPE to whatever is read from
	!		the file with WP_READ_REGLINE.
	!
	!	12/10/92 - Frank F. Starman
	!		Set Subaccount as Job rather than FG ID if
	!		posting to BUYOFF account.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/22/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Lose unecessary externals.
	!
	!	12/22/95 - Kevin Handy
	!		Modified to buyoff to a seperate location,
	!		using new WP_BUYOFF::TOLOCATION field.
	!
	!	12/26/95 - Kevin Handy
	!		Always use JC_JOB_EXAM, instead of JC_JOB which
	!		is never set up.
	!		Always use JC_JOB_EXAM::LOCATION instead of
	!		WP_BUYOFF::LOCATION.
	!
	!	12/27/95 - Kevin Handy
	!		Catch TOLOCATION into one more place.
	!
	!	01/02/96 - Kevin Handy
	!		Another try at making TOLOCATION business work right.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Use PRODUCT_FACTOR for issues.
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
	%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFF.HB"
	MAP (WP_BUYOFF)		WP_BUYOFF_CDD		WP_BUYOFF

	%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFFLINE.HB"
	MAP (WP_BUYOFFLINE)	WP_BUYOFFLINE_CDD	WP_BUYOFFLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSLINE.HB"
	MAP (WP_ISSLINE)	WP_ISSLINE_CDD	WP_ISSLINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE PD_ACCOUNT_CDD	PD_ACCOUNT_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD		WP_REQREGISTER

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
	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION	UTL_EXAM_LOCATION
	EXTERNAL LONG	FUNCTION	IC_TRAN_POST
	EXTERNAL LONG	FUNCTION	WP_TRAN_POST
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES
	EXTERNAL LONG   FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG   FUNCTION	GL_EXAM_CHART
	EXTERNAL LONG   FUNCTION	SB_EXAM_SUBACCOUNT
	EXTERNAL LONG   FUNCTION	WP_READ_REGLINE
	EXTERNAL LONG	FUNCTION	WP_TRAN_POSTREQ
	EXTERNAL LONG	FUNCTION	PD_READ_ACCOUNT

	!
	! Declare internal variables
	!
	DECLARE GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE PD_PRODUCT_CDD		PD_PRODUCT_EXAM
	DECLARE UTL_LOCATION_CDD	UTL_LOCATION_EXAM
	DECLARE	IC_TRANSACTION_CDD	IC_TRANSACTION
	DECLARE JC_JOB_CDD		JC_JOB_EXAM
	DECLARE WP_REGLINE_CDD		WP_REGLINE
	DECLARE WP_REGLINE_CDD		WP_REGLINE_READ

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE LONG			PRNT_SUMMARY
	DECLARE LONG			CHECK_PERIOD

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
	TITLE(1%) = "BUYOFF JOURNAL POSTING PROTOCOL"
	TITLE(2%) = "Work in Process System"
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
	!	The ^*Batch Number\* field enters a
	!	particular batch to be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Print Journal
	!	.x Print Journal>Batch Number
	!
	! Required:
	!--

	ICPERIOD = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) Inventory Period\*
	!	.b
	!	.lm +5
	!	The ^*Inventory Period\* field enters the
	!	Inventory Control accounting period into which this batch
	!	will be posted.
	!	.b
	!	The format for entry is YYYYPP.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.X Post>Inventory Period
	!	.X Inventory Period>Post
	!	.X Post>Period
	!	.X Period>Post
	!
	!--

	GLPERIOD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03)General Ledger Period\*
	!	.b
	!	.lm +5
	!	The ^*General Ledger Period\* field enters the
	!	General Ledger accounting period into which this batch
	!	will be posted.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Period to General Ledger>Post
	!	.x Post>Period to General Ledger
	!
	!--

	CHECK_DATE = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Check Dates\*
	!	.b
	!	.lm +5
	!	The ^*Check Dates\* field posts all transactions or
	!	checks the date of the transaction to see it falls between specified dates
	!	before posting. A ^*Y\* entry causes the dates to be checked, while a ^*N\*
	!	entry causes all transactions to be posted.
	!	.lm -5
	!
	! Index:
	!	.x Check Dates
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF CHECK_DATE = "Y"

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
		UTL_REPORTX, "WP_BUYOFF", BATCH_NO$, &
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
			GOTO Aborted &
				IF IC_TRAN_POST(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", &
				IC.INTER.PERIOD) <> CMC$_NORMAL
		END IF

		IF GL.INTER.PERIOD <> ""
		THEN
			GOTO Aborted &
				IF GL_TRAN_POSTGL(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", &
				GL.INTER.PERIOD) <> CMC$_NORMAL
		END IF

		GOTO Aborted IF WP_TRAN_POST(OPT_RESTART, SUBOPT_NOOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			"", "") <> CMC$_NORMAL

		GOTO Aborted IF WP_TRAN_POSTREQ(OPT_RESTART, SUBOPT_NOOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			"") <> CMC$_NORMAL

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
		UTL_REPORTX, "WP_BUYOFF", BATCH_NO$, &
		ICPERIOD, GLPERIOD) <> CMC$_NORMAL

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

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CHECK, SUBOPT_NOOPT, BATCH_NUMBER, &
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

300	!
	! Open WIP Order Buyoff header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFF.UPD"
	USE
		FILENAME$ = "WP_BUYOFF"
		CONTINUE HelpError
	END WHEN

310	!
	! Open WIP  Buyoff Journal line items file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFFLINE.UPD"
	USE
		FILENAME$ = "WP_BUYOFFLINE"
		CONTINUE HelpError
	END WHEN

320	!
	! Open WIP  Issue Journal line items file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_ISSLINE.UPD"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "WP_ISSLINE"
		CONTINUE HelpError
	END WHEN

330	!
	! Open control file, get control record
	!
	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.OPN"

	GET #WP_CONTROL.CH%, RECORD 1%, REGARDLESS

	CLOSE WP_CONTROL.CH%

	CALL ASSG_FREECHANNEL(WP_CONTROL.CH%)

	!******************************************************************
	!	1) List the debits/credits transmittal for the user while
	!		also putting the journal data in temporary files
	!	2) If confirmation, then go on
	!******************************************************************

	!******************************************************************
	! Create transmittal
	!******************************************************************
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "WP_BUYOFF", BATCH_NO$, "", "") <> CMC$_NORMAL

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	PRODUCT$, INVACCT$, EXPACCT$, TRANDATE$, LOCATION$ = ""

	RESET #WP_BUYOFF.CH%

 ReadHeader:
3000	!
	! Read in one record from the header file
	!
	WHEN ERROR IN
		GET #WP_BUYOFF.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE Confirm IF ERR = 11%
		FILENAME$ = "WP_BUYOFF"
		CONTINUE HelpError
	END WHEN

	V% = SB_EXAM_SUBACCOUNT("J", WP_BUYOFF::JOB, JC_JOB_EXAM)

	!
	! Set some initial variable values
	!
	PRINTHEAD% = 0%
	TOTALPOST = 0.0

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
	! Find the first line item for the header
	!
3100	WHEN ERROR IN
		FIND #WP_BUYOFFLINE.CH%, KEY #0% EQ WP_BUYOFF::JOB
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ReadHeader IF ERR = 155% OR ERR = 11%
		FILENAME$ = "WP_BUYOFFLINE"
		CONTINUE HelpError
	END WHEN

 LineItem:
	!
	! Get the (next) line item
	!
	WHEN ERROR IN
		GET #WP_BUYOFFLINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ReadHeader IF ERR = 155% OR ERR = 11%
		FILENAME$ = "WP_BUYOFFLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the header if we're done with the line items
	!
	GOTO ReadHeader IF WP_BUYOFFLINE::JOB <> WP_BUYOFF::JOB

	V% = WP_READ_REGLINE(WP_BUYOFFLINE::JOB, WP_BUYOFFLINE::LLINE, "EQ", &
		WP_REGLINE_READ, QTY())

	GOTO 3150 IF WP_BUYOFFLINE::COMPQTY = 0.0

	WP_REGLINE::JOB		= WP_BUYOFFLINE::JOB
	WP_REGLINE::LLINE	= WP_BUYOFFLINE::LLINE
	WP_REGLINE::REC_TYPE	= "02"
	WP_REGLINE::TTYPE	= WP_REGLINE_READ::TTYPE
	WP_REGLINE::ITEMCODE	= WP_REGLINE_READ::ITEMCODE
	WP_REGLINE::COST	= WP_BUYOFFLINE::COST
	WP_REGLINE::DESCR	= WP_REGLINE_READ::DESCR
	WP_REGLINE::QTY		= WP_BUYOFFLINE::COMPQTY
	WP_REGLINE::START_DATE	= WP_REGLINE_READ::START_DATE
	WP_REGLINE::COMP_DATE	= WP_BUYOFFLINE::BDATE
	WP_REGLINE::BATCH	= BATCH_NUMBER
	WP_REGLINE::POST_DATE	= POSTDATE
	WP_REGLINE::POST_TIME	= POSTTIME

	!
	! Call the post function for orders
	!
	GOTO Aborted IF WP_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", WP_REGLINE) <> CMC$_NORMAL

	!
	! Bypass post to inventory if not a Material line
	!
	GOTO CancelQty IF WP_REGLINE_READ::TTYPE <> "M"

	!
	! Check Product
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(WP_REGLINE_READ::ITEMCODE, &
		PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE ELSE
		PRODUCT$ = "*"

	END SELECT

	PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
		IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

	!
	! Here we calculate how many items to post to offset those
	! that were originally ordered (May be +/- because of adjustments)
	!
	!	We don't want to take off more than were originally
	!	ordered, because that would leave a balance in inventory.
	!
	!	TO.BUILD = Total that should be built
	!	PREVIOUS.BUILT = Number that have been posted already
	!	FINAL.BUILT = How many total posted after this
	!	REM.QTY = Adjustment to be made
	!
	TO.BUILD = QTY(1%) - QTY(3%)
	PREVIOUS.BUILT = QTY(2%)
	PREVIOUS.BUILT = TO.BUILD IF PREVIOUS.BUILT > TO.BUILD
	FINAL.BUILT = QTY(2%) + WP_BUYOFFLINE::COMPQTY
	FINAL.BUILT = TO.BUILD IF FINAL.BUILT > TO.BUILD
	REM.QTY = FINAL.BUILT - PREVIOUS.BUILT

 !	REM.QTY = WP_BUYOFFLINE::COMPQTY
 !	REM.QTY = QTY(0%) IF QTY(0%) < REM.QTY

	IF (WP_BUYOFF::TOLOCATION = "") OR &
		(WP_BUYOFF::TOLOCATION = JC_JOB_EXAM::LOCATION)
	THEN
		IC_TRANSACTION::PRODUCT		= WP_REGLINE::ITEMCODE
		IC_TRANSACTION::LOCATION	= JC_JOB_EXAM::LOCATION
		IC_TRANSACTION::TRANS_DATE	= WP_BUYOFFLINE::BDATE
		IC_TRANSACTION::PRIMARY_REF	= ""
		IC_TRANSACTION::CROSS_REF	= ""
		IC_TRANSACTION::SUBACCOUNT	= WP_BUYOFFLINE::JOB
		IC_TRANSACTION::LOT		= WP_BUYOFFLINE::LLINE
		IC_TRANSACTION::STATIONMAN	= JC_JOB_EXAM::OPERATOR

		IF WP_BUYOFFLINE::FG_ID_NO <> ""
		THEN
			IC_TRANSACTION::TYPE_A		= ""
			IC_TRANSACTION::QUANTITY_A	= 0.0
		ELSE
			IC_TRANSACTION::TYPE_A		= "MA"
			IC_TRANSACTION::QUANTITY_A	= WP_BUYOFFLINE::COMPQTY
		END IF

		IC_TRANSACTION::TYPE_B		= "WO"
		IC_TRANSACTION::QUANTITY_B	= -REM.QTY
		IC_TRANSACTION::COST		= &
			FUNC_ROUND(WP_BUYOFFLINE::COST * &
			WP_BUYOFFLINE::COMPQTY, 2%)
		IC_TRANSACTION::PRICE		= 0.0
		IC_TRANSACTION::TRANSACCT	= WP_BUYOFFLINE::ACCT
		IC_TRANSACTION::POSTDATE	= POSTDATE
		IC_TRANSACTION::POSTTIME	= POSTTIME
		IC_TRANSACTION::BATCH		= BATCH_NUMBER

		!
		! Post order to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)
	ELSE
		IC_TRANSACTION::PRODUCT		= WP_REGLINE::ITEMCODE
		IC_TRANSACTION::LOCATION	= JC_JOB_EXAM::LOCATION
		IC_TRANSACTION::TRANS_DATE	= WP_BUYOFFLINE::BDATE
		IC_TRANSACTION::PRIMARY_REF	= ""
		IC_TRANSACTION::CROSS_REF	= ""
		IC_TRANSACTION::SUBACCOUNT	= WP_BUYOFFLINE::JOB
		IC_TRANSACTION::LOT		= WP_BUYOFFLINE::LLINE
		IC_TRANSACTION::STATIONMAN	= JC_JOB_EXAM::OPERATOR
		IC_TRANSACTION::TYPE_A		= "WO"
		IC_TRANSACTION::QUANTITY_A	= -REM.QTY
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0
		IC_TRANSACTION::COST		= &
			FUNC_ROUND(WP_BUYOFFLINE::COST * &
			WP_BUYOFFLINE::COMPQTY, 2%)
		IC_TRANSACTION::PRICE		= 0.0
		IC_TRANSACTION::TRANSACCT	= WP_BUYOFFLINE::ACCT
		IC_TRANSACTION::POSTDATE	= POSTDATE
		IC_TRANSACTION::POSTTIME	= POSTTIME
		IC_TRANSACTION::BATCH		= BATCH_NUMBER

		!
		! Post order to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		IF WP_BUYOFFLINE::FG_ID_NO <> ""
		THEN
			IC_TRANSACTION::TYPE_A		= ""
			IC_TRANSACTION::QUANTITY_A	= 0.0
		ELSE
			IC_TRANSACTION::LOCATION	= WP_BUYOFF::TOLOCATION
			IC_TRANSACTION::TYPE_A		= "MA"
			IC_TRANSACTION::QUANTITY_A	= WP_BUYOFFLINE::COMPQTY
			!
			! Post order to inventory transaction file
			!
			EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
				CHECK_PERIOD, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		END IF

	END IF

	!
	! Find the first line item for the header
	!
3150	WHEN ERROR IN
		FIND #WP_ISSLINE.CH%, KEY #0% EQ "          " + &
			WP_BUYOFFLINE::JOB + WP_BUYOFFLINE::LLINE, REGARDLESS
	USE
		CONTINUE CancelQty IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_ISSLINE"
		CONTINUE HelpError
	END WHEN

 IssLineItem:
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

		CONTINUE CancelQty IF ERR = 11%
		FILENAME$ = "WP_ISSLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the  header if we're done with the line items
	!
	GOTO CancelQty IF WP_ISSLINE::REQNUM + WP_ISSLINE::JOB &
		+ WP_ISSLINE::LLINE &
		<> "          " + WP_BUYOFFLINE::JOB + WP_BUYOFFLINE::LLINE

	GOTO 3200 IF WP_ISSLINE::QTYISSUE = 0.0

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
	WP_REQREGISTER::OPERATOR = WP_BUYOFF::OPERATOR
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

	IC_TRANSACTION::PRODUCT		= WP_ISSLINE::PRODUCT
	IC_TRANSACTION::LOCATION	= JC_JOB_EXAM::LOCATION
	IC_TRANSACTION::TRANS_DATE	= WP_ISSLINE::ISSDATE
	IC_TRANSACTION::PRIMARY_REF	= RIGHT(WP_ISSLINE::LLINE, 3%) + &
		WP_ISSLINE::REQNUM + WP_ISSLINE::REQLINE
	IC_TRANSACTION::CROSS_REF	= ""
	IC_TRANSACTION::SUBACCOUNT	= WP_ISSLINE::JOB
	IC_TRANSACTION::LOT		= ""
	IC_TRANSACTION::STATIONMAN	= WP_BUYOFF::OPERATOR
	IC_TRANSACTION::TYPE_A		= "IS"
	IC_TRANSACTION::QUANTITY_A	= FUNC_ROUND( &
		-WP_ISSLINE::QTYISSUE / PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%)
	IC_TRANSACTION::TYPE_B		= ""
	IC_TRANSACTION::QUANTITY_B	= 0.0
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
		TEXT$ = WP_ISSLINE::JOB + " " + &
			WP_ISSLINE::LLINE + " " + &
			WP_ISSLINE::REQNUM + " " + &
			WP_ISSLINE::REQLINE + " " + &
			"*" + PD_ACCOUNT_READ::WIPACCT + " WIP ACCT"

		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT


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
		TEXT$ = WP_ISSLINE::JOB + " " + &
			WP_ISSLINE::LLINE + " " + &
			WP_ISSLINE::REQNUM + " " + &
			WP_ISSLINE::REQLINE + " " + &
			"*" + PD_ACCOUNT_READ::INVACCT + " INV ACCT"

	!
	! Keep undefined codes
	!
	GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
		UTL_REPORTX, TEXT$) <> CMC$_NORMAL

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

	GOTO IssLineItem

 CancelQty:
	GOTO PostInv IF WP_BUYOFFLINE::CANCELQTY = 0.0

	WP_REGLINE::JOB		= WP_BUYOFFLINE::JOB
	WP_REGLINE::LLINE	= WP_BUYOFFLINE::LLINE
	WP_REGLINE::REC_TYPE	= "03"
	WP_REGLINE::TTYPE	= WP_REGLINE_READ::TTYPE
	WP_REGLINE::ITEMCODE	= WP_REGLINE_READ::ITEMCODE
	WP_REGLINE::COST	= 0.0
	WP_REGLINE::DESCR	= WP_REGLINE_READ::DESCR
	WP_REGLINE::QTY		= WP_BUYOFFLINE::CANCELQTY
	WP_REGLINE::START_DATE	= WP_REGLINE_READ::START_DATE
	WP_REGLINE::COMP_DATE	= WP_BUYOFFLINE::BDATE
	WP_REGLINE::BATCH	= BATCH_NUMBER
	WP_REGLINE::POST_DATE	= POSTDATE
	WP_REGLINE::POST_TIME	= POSTTIME

	!
	! Call the post function for orders
	!
	GOTO Aborted IF WP_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", WP_REGLINE) <> CMC$_NORMAL

 PostInv:
	REM.QTY = QTY(0%) - REM.QTY
	REM.QTY = WP_BUYOFFLINE::CANCELQTY &
		IF WP_BUYOFFLINE::CANCELQTY < REM.QTY

	IF REM.QTY <> 0.0 AND WP_BUYOFFLINE::CANCELQTY <> 0.0
	THEN
		IC_TRANSACTION::PRODUCT		= WP_REGLINE::ITEMCODE
		IC_TRANSACTION::LOCATION	= JC_JOB_EXAM::LOCATION
		IC_TRANSACTION::TRANS_DATE	= WP_BUYOFFLINE::BDATE
		IC_TRANSACTION::PRIMARY_REF	= ""
		IC_TRANSACTION::CROSS_REF	= ""
		IC_TRANSACTION::SUBACCOUNT	= WP_BUYOFFLINE::JOB
		IC_TRANSACTION::LOT		= WP_BUYOFFLINE::LLINE
		IC_TRANSACTION::STATIONMAN	= JC_JOB_EXAM::OPERATOR
		IC_TRANSACTION::TYPE_A		= "WO"
		IC_TRANSACTION::QUANTITY_A	= -REM.QTY
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0
		IC_TRANSACTION::COST		= 0.0
		IC_TRANSACTION::PRICE		= 0.0
		IC_TRANSACTION::TRANSACCT	= WP_BUYOFFLINE::ACCT
		IC_TRANSACTION::POSTDATE	= POSTDATE
		IC_TRANSACTION::POSTTIME	= POSTTIME
		IC_TRANSACTION::BATCH		= BATCH_NUMBER

		!
		! Post order to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)
	END IF

	!
	! Check for undefined codes
	!
	IF INSTR(1%, PRODUCT$, "*")
	THEN
		TEXT$ = WP_BUYOFF::JOB + " " + &
			LOCATION$ + JC_JOB_EXAM::LOCATION + " " + &
			PRODUCT$ + WP_REGLINE_READ::ITEMCODE

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

 PostGLStuffline:
	GOTO LineItem IF FUNC_ROUND( &
		WP_BUYOFFLINE::COST * WP_BUYOFFLINE::COMPQTY, 2%) = 0.0

	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::SOURCE	= "WPBO"
	GL_YYYY_PP::REFNO	= JC_JOB_EXAM::REFNO
	GL_YYYY_PP::TRANDAT	= WP_BUYOFFLINE::BDATE
	GL_YYYY_PP::DESCR	= WP_REGLINE_READ::ITEMCODE
	GL_YYYY_PP::XREFNO	= JC_JOB_EXAM::REFNO
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER
	GL_YYYY_PP::SUBACC	= WP_BUYOFFLINE::FG_ID_NO
	GL_YYYY_PP::SUBACC	= WP_BUYOFFLINE::JOB &
					IF WP_BUYOFFLINE::FG_ID_NO = ""
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::OPERATION	= WP_REGLINE_READ::ITEMCODE &
		IF WP_REGLINE_READ::TTYPE = "L"

	!
	! Is Inventory (COS) account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(WP_BUYOFFLINE::ACCT, GL_CHART_EXAM)

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
	! Finish the record
	!
	GL_YYYY_PP::ACCT	= WP_BUYOFFLINE::ACCT
	GL_YYYY_PP::AMOUNT	= FUNC_ROUND( &
		WP_BUYOFFLINE::COST * WP_BUYOFFLINE::COMPQTY, 2%)
	GL_YYYY_PP::UNITS	= WP_BUYOFFLINE::COMPQTY
	GL_YYYY_PP::DESCR	= WP_REGLINE_READ::DESCR

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
	! Is Header account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(WP_BUYOFF::ACCT, GL_CHART_EXAM)

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

	!
	! Finish the record
	!
	GL_YYYY_PP::SUBACC	= WP_BUYOFFLINE::JOB
	GL_YYYY_PP::ACCT	= WP_BUYOFF::ACCT
	GL_YYYY_PP::AMOUNT	= -FUNC_ROUND( &
		WP_BUYOFFLINE::COST * WP_BUYOFFLINE::COMPQTY, 2%)
	GL_YYYY_PP::UNITS	= WP_BUYOFFLINE::COMPQTY
	GL_YYYY_PP::DESCR	= WP_REGLINE_READ::DESCR

	!
	! Post the Credit side
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
	IF INSTR(1%, LOCATION$ + EXPACCT$ + INVACCT$ + TRANDATE$, "*")
	THEN
		TEXT$ = LOCATION$ + JC_JOB_EXAM::LOCATION + " " + &
			EXPACCT$ + WP_BUYOFFLINE::ACCT + " " + &
			INVACCT$ + WP_BUYOFF::ACCT + " " + &
			TRANDATE$ + "DATE OUT OF RANGE"

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC,TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		INVACCT$, EXPACCT$, TRANDATE$, LOCATION$ = " "

	END IF

	GOTO LineItem

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	EXIT_STATUS = EXIT_STATUS AND IC_TRAN_POST(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", ICPERIOD)

	EXIT_STATUS = EXIT_STATUS AND OUTP_UNDEFCODES(OPT_CONFIRM, &
		TITLE(), UTL_REPORTX, "")

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
	GOTO Interrupt IF &
		WP_TRAN_POST(OPT_POSTFILE, SUBOPT_LINEITEM, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	!
	! Post to the register lines
	!
	GOTO Interrupt IF WP_TRAN_POSTREQ(OPT_POSTFILE, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "") <> CMC$_NORMAL

	!
	! Post to the inventory ledger
	!
	GOTO Interrupt IF IC_TRAN_POST(OPT_POSTFILE, SUBOPT_LEDGER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", ICPERIOD) <> CMC$_NORMAL

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
5000	CLOSE WP_BUYOFF.CH%
	CLOSE WP_BUYOFFLINE.CH%
	CLOSE WP_ISSLINE.CH%

5010 !	WHEN ERROR IN
 !		KILL WP_BUYOFF.DEV$ + "WP_BUYOFF_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5020
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(WP_BUYOFF.DEV$ + &
		"WP_BUYOFF_" + BATCH_NO$ + ".JRL;*")

5020 !	WHEN ERROR IN
 !		KILL WP_BUYOFFLINE.DEV$ + "WP_BUYOFFLINE_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5030
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(WP_BUYOFFLINE.DEV$ + &
		"WP_BUYOFFLINE_" + BATCH_NO$ + ".JRL;*")

5030 !	WHEN ERROR IN
 !		KILL WP_ISSLINE.DEV$ + "WP_ISSLINE_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE Complete
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(WP_ISSLINE.DEV$ + &
		"WP_ISSLINE_" + BATCH_NO$ + ".JRL;*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "WP_BUYOFF", BATCH_NO$, "", "")

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
	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "WP_BUYOFF", BATCH_NO$, "", "")

		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "WP_BUYOFF", BATCH_NO$, "", "")

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

32767	END
