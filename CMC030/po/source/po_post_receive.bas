1	%TITLE "Purchase Order Receipts Post Program"
	%SBTTL "PO_POST_RECEIVE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:SJPST
	!
	! Abstract:HELP
	!	.p
	!	The ^*Post PO Receipts\* option posts entries
	!	which have been made in the PO Receiver Journal to the PO
	!	Register and the Inventory System.
	!	.note
	!	Items that were not received against a PO (ie. the PO
	!	number was entered as blank) will not be entered into
	!	the PO Register, as there is no PO Item to post them
	!	against.
	!	.end note
	!	.note
	!	Items that are entered without an inventory part number
	!	will not be posted to the inventory system.
	!	.end note
	!	.p
	!	After the posting is completed, the system will return to the
	!	PO Receiver Menu screen.
	!
	! Index:
	!	.x Post>PO Receiver
	!	.x PO Receiver>Post
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_POST_RECEIVE/LINE
	!	$ LINK/EXE=PO_EXE: PO_POST_RECEIVE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_POST_RECEIVE.OBJ;*
	!
	! Author:
	!
	!	07/20/90 - Kevin Handy
	!
	! Modification history:
	!
	!	08/07/90 - Kevin Handy
	!		Modified for changes Frank made in file layouts.
	!
	!	12/18/90 - Frank F. Starman
	!		Decrease number of agruments in IC_TRAN_POST function.
	!
	!	03/03/92 - Dan Perkins
	!		Modified for changes in file layouts.
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/13/92 - Kevin Handy
	!		Removed duplicate error trap (check)
	!
	!	03/16/92 - Dan Perkins
	!		Added error trapping to line 3200.
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	08/19/92 - Frank F. Starman
	!		Assign cost and price in the inventory trans ledger.
	!
	!	09/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	11/10/93 - Kevin Handy
	!		Fixed the case where a line doesn't yet exist in
	!		the file. Used to use the line it didn't find to
	!		create a record. Now searches for any line in the
	!		po file with that po # to use as a template.
	!
	!	11/04/94 - Kevin Handy
	!		Added parameter to PO_READ_REGLINE.
	!
	!	04/05/95 - Kevin Handy
	!		(V3.6)
	!		Update source code to Version 3.6 Calico standards.
	!		Fix problem with a negative receipt when there
	!		was an over-receipt posted earlier.
	!
	!	06/15/95 - Kevin Handy
	!		Format closer to 80 columns.
	!
	!	06/15/95 - Kevin Handy
	!		Re-wrote the section of code that calculated the
	!		REMAIN_QTY. Lost a strange set of of-then-else
	!		statements and used a delta calculation instead.
	!		MUCH easier to understand, and maybe it will
	!		work more often.
	!
	!	09/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/27/96 - Kevin Handy
	!		Format source much closer to 80 columns.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	04/04/2005 - Kevin Handy
	!		Use receipt date instead of order date to calculate
	!		costs.
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
	%INCLUDE "SOURCE:[PO.OPEN]PO_RECJOUR.HB"
	MAP (PO_RECJOUR)	PO_RECJOUR_CDD			PO_RECJOUR

	%INCLUDE "SOURCE:[PO.OPEN]PO_RECLINE.HB"
	MAP (PO_RECLINE)	PO_RECLINE_CDD			PO_RECLINE

	COM (READ_PO_REG_LINE)	PO_REG_LINE.CH%, &
				PO_REG_SUB_LINE.CH%

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION	PO_TRAN_POSTPO
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	IC_TRAN_POST
	EXTERNAL LONG	FUNCTION	PO_READ_REG_LINE
	EXTERNAL REAL	FUNCTION	PC_READ_COST

	!
	! Declare internal variables
	!
	DECLARE PD_PRODUCT_CDD		PD_PRODUCT_EXAM
	DECLARE PO_REG_LINE_CDD		PO_REG_LINE, PO_REG_LINE_READ
	DECLARE PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE, PO_REG_SUB_LINE_READ
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE	IC_TRANSACTION_CDD	IC_TRANSACTION

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE LONG			CHECK_PERIOD

	DECLARE	STRING			ICPERIOD
	DECLARE	STRING			BATCH_NUMBER
	DECLARE	STRING			PO.INTER.PERIOD
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
	TITLE(1%) = "PURCHASE  ORDER  JOURNAL  POSTING  PROTOCOL"
	TITLE(2%) = "Purchase Order System"
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
	!	.p
	!	The ^*Batch Number\* field enters a
	!	particular batch to be posted.
	!	.p
	!	An entry is required in this field.
	!
	! Index:
	!	.x Batch Number>Post Receiver
	!	.x Post Receiver>Batch Number
	!
	! Required:
	!--

	ICPERIOD = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) Inventory Period\*
	!	.p
	!	The ^*Inventory Period\* field enters the
	!	Inventory Control accounting period into which this batch
	!	will be posted.
	!	.p
	!	The format for entry is YYYYPP.
	!	.p
	!	This field requires an entry.
	!
	! Index:
	!	.X Post>Inventory Period
	!	.X Inventory Period>Post
	!	.X Post>Period
	!	.X Period>Post
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
	! Open RECJOUR
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_RECJOUR.UPD"
	USE
		FILENAME$ = "PO_RECJOUR"
		CONTINUE HelpError
	END WHEN

315	!
	! Open RECLINE
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_RECLINE.UPD"
	USE
		FILENAME$ = "PO_RECLINE"
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
		UTL_REPORTX, "PO_JOUR_RECEIVE", BATCH_NO$, &
		GL.INTER.PERIOD, PO.INTER.PERIOD)

	SELECT INTR_STATUS

	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Process was interrupted
	!
	CASE CMC$_WARNING

		IF TRM$(PO.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				PO_TRAN_POSTPO(OPT_RESTART, SUBOPT_NOOPT, &
					BATCH_NUMBER, TITLE(), UTL_REPORTX, &
					"", "") <> CMC$_NORMAL

			GOTO Aborted &
				IF IC_TRAN_POST(OPT_RESTART, SUBOPT_NOOPT, &
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
		UTL_REPORTX, "PO_JOUR_RECEIVE", BATCH_NO$, &
		ICPERIOD, "") <> CMC$_NORMAL

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
		UTL_REPORTX, "PO_JOUR_RECEIVE", BATCH_NO$, &
		"", "") <> CMC$_NORMAL

	RESET #PO_RECJOUR.CH%

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

 ReadHeader:
	!
	! Read in one record from the header file
	!
3000	WHEN ERROR IN
		GET #PO_RECJOUR.CH%
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
		FILENAME$ = "PO_RECJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Find the first line item
	!
3100	WHEN ERROR IN
		FIND #PO_RECLINE.CH%, KEY #0% EQ PO_RECJOUR::PO
	USE
		!
		! Locked block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ReadHeader IF ERR = 155%
		FILENAME$ = "PO_RECLINE"
		CONTINUE HelpError
	END WHEN

 LineItem:
	!
	! Get the (next) line item
	!
3200	WHEN ERROR IN
		GET #PO_RECLINE.CH%
	USE
		!
		! Locked block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ReadHeader IF ERR = 11%
		FILENAME$ = "PO_RECLINE"
		CONTINUE HelpError
	END WHEN

	!PRODUCT$ = " "
	!
	! Finish up the header if we're done with the line items
	!
	GOTO ReadHeader IF PO_RECLINE::PO <> PO_RECJOUR::PO

	!
	! Get the product description
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(PO_RECLINE::PRODUCT, PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		!PRODUCT$ = "*"

	!
	! Product terminated
	!
	CASE CMC$_TERMINATED

		CALL HELP_PRINTMESS(SCOPE, PO_RECLINE::PRODUCT + &
			" product is inactive", &
			"W", SCOPE::PRG_PROGRAM, "", "INACT", &
			UTL_REPORTX, TITLE(), 0%)

		GOTO Aborted

	!
	! SNAFU:  Situation Normal - All Fouled Up
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	IF PO_READ_REG_LINE(PO_RECLINE::PO, PO_RECLINE::PO_LINE, &
		"EQ", PO_REG_LINE_READ, PO_REG_SUB_LINE_READ, &
		QTY(), "") <> CMC$_NORMAL
	THEN
		!
		! ** Search for another line on this PO to fill in other
		! fields into the PO_REG_LINE_READ.
		!
		GOSUB CreateGeneric


		!
		! Call the post function
		!
		GOTO Aborted IF PO_TRAN_POSTPO(OPT_ADDREC, SUBOPT_REGISTER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			PO_REG_LINE, PO_REG_SUB_LINE) <> CMC$_NORMAL

	END IF

	GOTO CancelQty IF PO_RECLINE::RECQTY = 0.0

	PO_REG_SUB_LINE::PO		= PO_RECJOUR::PO
	PO_REG_SUB_LINE::PO_LINE	= PO_RECLINE::PO_LINE
	PO_REG_SUB_LINE::PO_ACTION	= "02"
	PO_REG_SUB_LINE::ACTION_DATE	= PO_RECJOUR::RECDATE
	PO_REG_SUB_LINE::QTY		= PO_RECLINE::RECQTY
	PO_REG_SUB_LINE::PRICE		= PO_REG_SUB_LINE_READ::PRICE
	PO_REG_SUB_LINE::SUBACCT	= ""
	PO_REG_SUB_LINE::ACCOUNT	= ""
	PO_REG_SUB_LINE::BATCH		= BATCH_NUMBER
	PO_REG_SUB_LINE::POSTDATE	= POSTDATE
	PO_REG_SUB_LINE::POSTTIME	= POSTTIME

	!
	! Call the post function
	!
	GOTO Aborted IF PO_TRAN_POSTPO(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		PO_REG_LINE, PO_REG_SUB_LINE) <> CMC$_NORMAL

	!
	! Post to Inventory system if there is a part number set up
	!
	IF TRM$(PO_RECLINE::PRODUCT) <> ""
	THEN
		!
		! Calculate the delta for the on-order value by
		! calculating what should have been on order before
		! this receipt, and what should have been on order
		! after this receipt, and posting the difference.
		!
		TEST_BALANCE = QTY(1%) - QTY(2%) - QTY(3%)
		IF TEST_BALANCE > 0.0
		THEN
			BEFORE_BALANCE = TEST_BALANCE
		ELSE
			BEFORE_BALANCE = 0.0
		END IF

		IF BEFORE_BALANCE > QTY(1%)
		THEN
			BEFORE_BALANCE = QTY(1%)
		END IF

		AFTER_BALANCE = TEST_BALANCE - PO_RECLINE::RECQTY
		IF AFTER_BALANCE < 0.0
		THEN
			AFTER_BALANCE = 0.0
		END IF

		IF AFTER_BALANCE > QTY(1%)
		THEN
			AFTER_BALANCE = QTY(1%)
		END IF

		REMAIN_QTY = BEFORE_BALANCE - AFTER_BALANCE

		!
		! Now pull up costs & stuff like that
		!
		COST = PC_READ_COST(PO_RECLINE::PRODUCT, &
			PO_REG_LINE_READ::FROMLOCATION, &
			PO_RECJOUR::RECDATE, "")

		COST = ABS(FUNC_ROUND(COST * PO_RECLINE::RECQTY, 2%))

		IC_TRANSACTION::PRODUCT		= PO_RECLINE::PRODUCT
		IC_TRANSACTION::LOCATION	= PO_REG_LINE_READ::FROMLOCATION
		IC_TRANSACTION::TRANS_DATE	= PO_RECJOUR::RECDATE

		IC_TRANSACTION::PRIMARY_REF	= CONV_STRING( &
			PO_RECLINE::PO, CMC$_LEFT) + PO_RECLINE::PO_LINE

		IC_TRANSACTION::CROSS_REF	= PO_REG_LINE_READ::VENDOR
		IC_TRANSACTION::SUBACCOUNT	= ""
		IC_TRANSACTION::LOT		= ""
		IC_TRANSACTION::STATIONMAN	= PO_RECJOUR::OPERATOR
		IC_TRANSACTION::TYPE_A		= "RE"
		IC_TRANSACTION::QUANTITY_A	= PO_RECLINE::RECQTY
		IC_TRANSACTION::TYPE_B		= "PO"
		IC_TRANSACTION::QUANTITY_B	= -REMAIN_QTY
		IC_TRANSACTION::COST		= COST

		IC_TRANSACTION::PRICE		= ABS(FUNC_ROUND( &
			PO_REG_SUB_LINE_READ::PRICE * PO_RECLINE::RECQTY, 2%))

		IC_TRANSACTION::TRANSACCT	= ""
		IC_TRANSACTION::POSTDATE	= POSTDATE
		IC_TRANSACTION::POSTTIME	= POSTTIME
		IC_TRANSACTION::BATCH		= BATCH_NUMBER

		!
		! Post to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, &
			ICPERIOD)
	END IF

 CancelQty:
	GOTO LineItem IF PO_RECLINE::CANQTY = 0.0

	PO_REG_SUB_LINE::PO		= PO_RECJOUR::PO
	PO_REG_SUB_LINE::PO_LINE	= PO_RECLINE::PO_LINE
	PO_REG_SUB_LINE::PO_ACTION	= "03"			! Check this
	PO_REG_SUB_LINE::ACTION_DATE	= PO_RECJOUR::RECDATE
	PO_REG_SUB_LINE::QTY		= PO_RECLINE::CANQTY
	PO_REG_SUB_LINE::PRICE		= 0.0			! Check this
	PO_REG_SUB_LINE::SUBACCT	= PO_RECJOUR::REFNO	! Check this
	PO_REG_SUB_LINE::ACCOUNT	= ""
	PO_REG_SUB_LINE::BATCH		= BATCH_NUMBER
	PO_REG_SUB_LINE::POSTDATE	= POSTDATE
	PO_REG_SUB_LINE::POSTTIME	= POSTTIME

	!
	! Call the post function
	!
	GOTO Aborted IF PO_TRAN_POSTPO(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		PO_REG_LINE, PO_REG_SUB_LINE) <> CMC$_NORMAL

	!
	! Post to Inventory system if there is a part number set up
	!
	IF TRM$(PO_RECLINE::PRODUCT) <> ""
	THEN
		!
		! Calculate remaining on-order qty and post inventory
		! with only the correct PO qty (in case of over shipment)
		!
		IF QTY(0%) - PO_RECLINE::RECQTY < PO_RECLINE::CANQTY
		THEN
			REMAIN_QTY = QTY(0%) - PO_RECLINE::RECQTY
		ELSE
			REMAIN_QTY = PO_RECLINE::CANQTY
		END IF

		IF REMAIN_QTY <> 0.0
		THEN
			IC_TRANSACTION::PRODUCT		= PO_RECLINE::PRODUCT
			IC_TRANSACTION::LOCATION = &
				PO_REG_LINE_READ::FROMLOCATION
			IC_TRANSACTION::TRANS_DATE	= PO_RECJOUR::RECDATE

			IC_TRANSACTION::PRIMARY_REF	= CONV_STRING( &
				PO_RECLINE::PO,CMC$_LEFT) + PO_RECLINE::PO_LINE

			IC_TRANSACTION::CROSS_REF = PO_REG_LINE_READ::VENDOR
			IC_TRANSACTION::SUBACCOUNT	= ""
			IC_TRANSACTION::LOT		= ""
			IC_TRANSACTION::STATIONMAN	= PO_RECJOUR::OPERATOR
			IC_TRANSACTION::TYPE_A		= "PO"
			IC_TRANSACTION::QUANTITY_A	= -REMAIN_QTY
			IC_TRANSACTION::TYPE_B		= ""
			IC_TRANSACTION::QUANTITY_B	= 0.0
			IC_TRANSACTION::COST		= COST

			IC_TRANSACTION::PRICE		= ABS(FUNC_ROUND( &
				PO_REG_SUB_LINE_READ::PRICE * &
				PO_RECLINE::RECQTY, 2%))

			IC_TRANSACTION::TRANSACCT	= ""
			IC_TRANSACTION::POSTDATE	= POSTDATE
			IC_TRANSACTION::POSTTIME	= POSTTIME
			IC_TRANSACTION::BATCH		= BATCH_NUMBER

			!
			! Post to inventory transaction file
			!
			EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
				CHECK_PERIOD, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, IC_TRANSACTION, &
				ICPERIOD)
		END IF

	END IF

	GOTO LineItem

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************

	EXIT_STATUS = EXIT_STATUS AND OUTP_UNDEFCODES(OPT_CONFIRM, TITLE(), &
		UTL_REPORTX, "")

	EXIT_STATUS = EXIT_STATUS AND IC_TRAN_POST(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", ICPERIOD)

	GOTO Aborted IF EXIT_STATUS <> CMC$_NORMAL

	%PAGE

	!******************************************************************
	!	1) Since everything's correct and we have the user's
	!		permission, transfer all data from the temporary
	!		files into the ledger
	!******************************************************************

	!
	! Post the PO Journal header
	!
	GOTO Interrupt IF &
		PO_TRAN_POSTPO(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	GOTO Interrupt IF IC_TRAN_POST(OPT_POSTFILE, SUBOPT_LEDGER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", ICPERIOD) <> CMC$_NORMAL

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
5000	CLOSE PO_RECJOUR.CH%
	CLOSE PO_RECLINE.CH%

5010 !	WHEN ERROR IN
 !		KILL PO_RECJOUR.DEV$ + "PO_RECJOUR_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5020
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(PO_RECJOUR.DEV$ + &
		"PO_RECJOUR_" + BATCH_NO$ + ".JRL;*")

5020 !	WHEN ERROR IN
 !		KILL PO_RECLINE.DEV$ + "PO_RECLINE_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE Complete
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(PO_RECLINE.DEV$ + &
		"PO_RECLINE_" + BATCH_NO$ + ".JRL;*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "PO_JOUR_RECEIVE", BATCH_NO$, "", "")

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************
	!
	! Print credit and debit transmittals
	!

	!
	! Print undefined codes (if any)
	!
	TEXT ="Item   Invoice Vendor  Num AR AccountNum       TransDate SubAcct"

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

8000	!*******************************************************************
	! Search for a PO line to fill in several of the blank fields
	! when we don't find the correct line.
	!*******************************************************************
 CreateGeneric:

	!
	! Try to get a previous line for the generic information
	!
	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, KEY #0% EQ PO_RECJOUR::PO, REGARDLESS
	USE
		CONTINUE 8020
	END WHEN

	GOTO 8050

8020	!
	! Couldn't find anything, so fake the necessary fields with
	! something as best as possible.
	!
	PO_REG_LINE::VENDOR		= ""
	PO_REG_LINE::FROMLOCATION	= ""
	PO_REG_LINE::PO_TYPE		= ""
	PO_REG_LINE::OPEN_CLOSE		= ""

8050	!
	! Generate a PO_REG_LINE record from any other information we
	! have laying around.
	!
	PO_REG_LINE::PO			= PO_RECJOUR::PO
	PO_REG_LINE::PO_LINE		= PO_RECLINE::PO_LINE
	PO_REG_LINE::PRODUCT		= PO_RECLINE::PRODUCT
	PO_REG_LINE::UOM		= PO_RECLINE::UOM
	PO_REG_LINE::DESCRIPTION	= PO_RECLINE::DESCRIPTION
	PO_REG_LINE::ORDDATE		= PO_RECJOUR::RECDATE
	PO_REG_LINE::BATCH		= BATCH_NUMBER
	PO_REG_LINE::PERIOD		= ICPERIOD

	RETURN

	%PAGE

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************
	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "PO_JOUR_RECEIVE", &
			BATCH_NO$, "", "")

		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "PO_JOUR_RECEIVE", BATCH_NO$, "", "")

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
	! End of posting program PO_POST_RECEIVE
	!******************************************************************
	END
