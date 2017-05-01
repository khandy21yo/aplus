1	%TITLE "Post Journal"
	%SBTTL "PO_POST_JOURNAL"
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
	!	The ^*Post Journal\* option posts the entries in a
	!	Purchase Order Journal batch to the Purchase Order Register and the Inventory
	!	System.
	!
	! Index:
	!	.x Post>Purchase Order Journal
	!	.x Purchase Order>Journal>Post
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_POST_JOURNAL/LINE
	!	$ LINK/EXE=PO_EXE: PO_POST_JOURNAL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_POST_JOURNAL.OBJ;*
	!
	! Author:
	!
	!	07/19/90 - Kevin Handy
	!
	! Modification history:
	!
	!	08/07/90 - Kevin Handy
	!		Modified for changes frank made in file layouts.
	!
	!	12/18/90 - Frank F. Starman
	!		Decrease number of arguments in IC_TRAN_POST function.
	!
	!	03/03/92 - Dan Perkins
	!		Modified for changes in file layouts.
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	04/12/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
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
	!	12/03/2001 - Kevin Handy
	!		Post PO date and not Requested Receive date to
	!		PO archive.
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
	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.HB"
	MAP (PO_ORDERJOUR)		PO_ORDERJOUR_CDD	PO_ORDERJOUR

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.HB"
	MAP (PO_ORDERLINE)		PO_ORDERLINE_CDD	PO_ORDERLINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.HB"
	MAP (PO_ORDERSLINE)		PO_ORDERSLINE_CDD	PO_ORDERSLINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.HB"
	MAP (PO_CONTROL)		PO_CONTROL_CDD		PO_CONTROL

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION	AP_EXAM_VENDOR
	EXTERNAL LONG	FUNCTION	UTL_EXAM_LOCATION
	EXTERNAL LONG	FUNCTION	PO_TRAN_POSTPO
	EXTERNAL LONG	FUNCTION	IC_TRAN_POST
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES
	EXTERNAL REAL	FUNCTION	PC_READ_COST


	!
	! Declare internal variables
	!
	DECLARE AP_VENDOR_CDD		AP_VENDOR_EXAM
	DECLARE PD_PRODUCT_CDD		PD_PRODUCT_EXAM
	DECLARE UTL_LOCATION_CDD	UTL_LOCATION_EXAM
	DECLARE PO_REG_LINE_CDD		PO_REG_LINE
	DECLARE PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE
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
	!	.x Batch Number>Post Order
	!	.x Post Order>Batch Number
	!
	! Required:
	!--

	ICPERIOD = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) Inventory Period\*
	!	.p
	!	The ^*Inventory Period\* field enters the Inventory
	!	Control accounting period into which a purchase order journal batch
	!	is to be posted.
	!	.p
	!	The format for entry is YYYYPP.
	!	.p
	!	This field requires an entry.
	!
	! Index:
	!	.X Post>Inventory Period
	!	.X Inventory Period>Post
	!	.x Purchase Order>Journal>Post Period
	!	.X Post>Period>Purchase Order Journal
	!	.X Period>Post
	!
	!--

300	!
	! Open ORDER JOURNAL
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.UPD"
	USE
		FILENAME$ = "PO_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open ORDER LINE
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.UPD"
	USE
		CONTINUE 315 IF ERR = 5%
		FILENAME$ = "PO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

315	!
	! Open ORDER SUBLINE
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.UPD"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "PO_ORDERSLINE"
		CONTINUE HelpError
	END WHEN

320	!
	! Open control file, get control record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.OPN"
		GET #PO_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE PO_CONTROL.CH%
	USE
		FILENAME$ = "PO_CONTROL"
		CONTINUE HelpError
	END WHEN

	CALL ASSG_FREECHANNEL(PO_CONTROL.CH%)

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
		UTL_REPORTX, "PO_ORDERJOUR", BATCH_NO$, &
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
		UTL_REPORTX, "PO_ORDERJOUR", BATCH_NO$, &
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
		UTL_REPORTX, "PO_ORDERJOUR", BATCH_NO$, &
		"", "") <> CMC$_NORMAL

	RESET #PO_ORDERJOUR.CH%

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

 ReadHeader:
	!
	! Read in one record from the header file
	!
3000	WHEN ERROR IN
		GET #PO_ORDERJOUR.CH%
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
		FILENAME$ = "PO_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Blank flags
	!
	VENNUM$, PRODUCT$, LOC$ = " "

	!
	! Get the customer description
	!
	EXIT_STATUS = AP_EXAM_VENDOR(PO_ORDERJOUR::VENDOR, AP_VENDOR_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		VENNUM$ = "*"

	!
	! SNAFU:  Situation Normal - All Fouled Up
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Get the location
	!
	EXIT_STATUS = UTL_EXAM_LOCATION(PO_ORDERJOUR::FROMLOCATION, &
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
		LOC$ = "*"

	!
	! SNAFU:  Situation Normal - All Fouled Up
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Check for undefined tax account stuff
	!
	IF INSTR(1%, VENNUM$ + LOC$, "*")
	THEN
		TEXT$ = PO_ORDERJOUR::PO+ " " + &
			VENNUM$ + PO_ORDERJOUR::VENDOR + " " + &
			LOC$ + PO_ORDERJOUR::FROMLOCATION
		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, &
			TEXT$) <> CMC$_NORMAL

	END IF

	!
	! Find the first line item
	!
3100	WHEN ERROR IN
		FIND #PO_ORDERLINE.CH%, KEY #0% EQ PO_ORDERJOUR::PO
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
		FILENAME$ = "PO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

 LineItem:
	!
	! Get the (next) line item
	!
3200	WHEN ERROR IN
		GET #PO_ORDERLINE.CH%
	USE
		!
		! Locked block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ReadHeader IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the header if we're done with the line items
	!
	GOTO ReadHeader IF PO_ORDERLINE::PO <> PO_ORDERJOUR::PO

	GOTO 3250 IF PO_ORDERLINE::OUR_PRODUCT = ""

	!
	! Get the product description
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(PO_ORDERLINE::OUR_PRODUCT, &
		PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL, CMC$_TERMINATED

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		PRODUCT$ = "*"

	!
	! SNAFU:  Situation Normal - All Fouled Up
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Check for undefined tax account stuff
	!
	IF INSTR(1%, PRODUCT$, "*")
	THEN
		TEXT$ = PO_ORDERJOUR::PO + " " + &
			VENNUM$ + PO_ORDERJOUR::VENDOR + " " + &
			LOC$ + PO_ORDERJOUR::FROMLOCATION + " " + &
			PRODUCT$ + PO_ORDERLINE::OUR_PRODUCT
		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC,TITLE(),UTL_REPORTX, &
			TEXT$) <> CMC$_NORMAL

	END IF

3250	!
	! Generate a PO_REG_LINE record to pass through to the
	! post function
	!
	PO_REG_LINE::PO			= PO_ORDERJOUR::PO
	PO_REG_LINE::PO_LINE		= PO_ORDERLINE::PO_LINE
	PO_REG_LINE::VENDOR		= PO_ORDERJOUR::VENDOR
	PO_REG_LINE::FROMLOCATION	= PO_ORDERJOUR::FROMLOCATION
	PO_REG_LINE::PRODUCT		= PO_ORDERLINE::OUR_PRODUCT
	PO_REG_LINE::UOM		= PO_ORDERLINE::OUR_UOM
	PO_REG_LINE::DESCRIPTION	= PO_ORDERLINE::DESCRIPTION
	PO_REG_LINE::PO_TYPE		= PO_ORDERJOUR::POTYPE
	PO_REG_LINE::OPEN_CLOSE		= "O"
	PO_REG_LINE::ORDDATE		= PO_ORDERJOUR::PODATE
	PO_REG_LINE::BATCH		= BATCH_NUMBER
	PO_REG_LINE::PERIOD		= ICPERIOD

	!
	! Call the post function
	!
	GOTO Aborted IF PO_TRAN_POSTPO(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		PO_REG_LINE, PO_REG_SUB_LINE) <> CMC$_NORMAL

	PRODUCT$ = " "
	!
	! Find the first subline item
	!
3300	WHEN ERROR IN
		FIND #PO_ORDERSLINE.CH%, KEY #0% EQ PO_ORDERLINE::PO + &
			PO_ORDERLINE::PO_LINE
	USE
		!
		! Locked block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE LineItem IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PO_ORDERSLINE"
		CONTINUE HelpError
	END WHEN

 SubLineItem:
	!
	! Get the (next) subline item
	!
3400	WHEN ERROR IN
		GET #PO_ORDERSLINE.CH%
	USE
		!
		! Locked block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE LineItem IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PO_ORDERSLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the lines if we're done with the subline items
	!
	GOTO LineItem IF PO_ORDERSLINE::PO <> PO_ORDERLINE::PO OR &
		PO_ORDERSLINE::PO_LINE <> PO_ORDERLINE::PO_LINE

	PO_REG_SUB_LINE::PO		= PO_ORDERLINE::PO
	PO_REG_SUB_LINE::PO_LINE	= PO_ORDERLINE::PO_LINE
	PO_REG_SUB_LINE::PO_ACTION	= "01"
 !	PO_REG_SUB_LINE::ACTION_DATE	= PO_ORDERSLINE::RECEIVEDATE
	PO_REG_SUB_LINE::ACTION_DATE	= PO_ORDERJOUR::PODATE
	PO_REG_SUB_LINE::QTY		= PO_ORDERSLINE::OUR_QTY
	PO_REG_SUB_LINE::PRICE		= PO_ORDERLINE::VEN_PRICE
	PO_REG_SUB_LINE::SUBACCT	= PO_ORDERSLINE::SUBACCT
	PO_REG_SUB_LINE::ACCOUNT	= PO_ORDERSLINE::GL_ACCOUNT
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
	IF TRM$(PO_ORDERLINE::OUR_PRODUCT) <> ""
	THEN
		COST = PC_READ_COST(PO_ORDERLINE::OUR_PRODUCT, &
			PO_ORDERJOUR::FROMLOCATION, &
			PO_ORDERJOUR::PODATE, "")

		COST = ABS(FUNC_ROUND(COST * PO_ORDERSLINE::OUR_QTY, 2%))

		IC_TRANSACTION::PRODUCT		= PO_ORDERLINE::OUR_PRODUCT
		IC_TRANSACTION::LOCATION	= PO_ORDERJOUR::FROMLOCATION
		IC_TRANSACTION::TRANS_DATE	= PO_ORDERJOUR::PODATE
		IC_TRANSACTION::PRIMARY_REF	= &
			CONV_STRING(PO_ORDERLINE::PO, CMC$_LEFT) + &
			PO_ORDERLINE::PO_LINE
		IC_TRANSACTION::CROSS_REF	= PO_ORDERJOUR::VENDOR
		IC_TRANSACTION::SUBACCOUNT	= PO_ORDERSLINE::SUBACCT
		IC_TRANSACTION::LOT		= ""
		IC_TRANSACTION::STATIONMAN	= PO_ORDERJOUR::OPERATOR
		IC_TRANSACTION::TYPE_A		= "PO"
		IC_TRANSACTION::QUANTITY_A	= PO_ORDERSLINE::OUR_QTY
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0
		IC_TRANSACTION::COST		= COST
		IC_TRANSACTION::PRICE		= ABS(FUNC_ROUND( &
			PO_ORDERLINE::VEN_PRICE * PO_ORDERSLINE::OUR_QTY, 2%))
		IC_TRANSACTION::TRANSACCT	= PO_ORDERSLINE::GL_ACCOUNT
		IC_TRANSACTION::POSTDATE	= POSTDATE
		IC_TRANSACTION::POSTTIME	= POSTTIME
		IC_TRANSACTION::BATCH		= BATCH_NUMBER

		!
		! Post to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)
	END IF

	GOTO SubLineItem

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************

	EXIT_STATUS = EXIT_STATUS AND OUTP_UNDEFCODES(OPT_CONFIRM, TITLE(), &
		UTL_REPORTX, "")

	EXIT_STATUS = EXIT_STATUS AND IC_TRAN_POST(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", ICPERIOD)

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

	!
	! Continue by posting the Sales Journal line items
	!
	GOTO Interrupt IF &
		PO_TRAN_POSTPO(OPT_POSTFILE, SUBOPT_LINEITEM, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	GOTO Interrupt IF IC_TRAN_POST(OPT_POSTFILE, SUBOPT_LEDGER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", ICPERIOD) <> CMC$_NORMAL

	%PAGE

	!
	! Remove files
	!
5000	CLOSE PO_ORDERJOUR.CH%
	CLOSE PO_ORDERLINE.CH%
	CLOSE PO_ORDERSLINE.CH%

5010 !	WHEN ERROR IN
 !		KILL PO_ORDERJOUR.DEV$ + "PO_ORDERJOUR_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5020
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(PO_ORDERJOUR.DEV$ + &
		"PO_ORDERJOUR_" + BATCH_NO$ + ".JRL;*")

5020 !	WHEN ERROR IN
 !		KILL PO_ORDERLINE.DEV$ + "PO_ORDERLINE_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5030
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(PO_ORDERSLINE.DEV$ + &
		"PO_ORDERSLINE_" + BATCH_NO$ + ".JRL;*")

5030 !	WHEN ERROR IN
 !		KILL PO_ORDERSLINE.DEV$ + "PO_ORDERSLINE_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE Complete
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(PO_ORDERSLINE.DEV$ + &
		"PO_ORDERSLINE_" + BATCH_NO$ + ".JRL;*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "PO_ORDERJOUR", BATCH_NO$, "", "")

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
	TEXT = "PO#        VendorNum  Loc  Product"

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
			TITLE(), UTL_REPORTX, "PO_ORDERJOUR", &
			BATCH_NO$, "", "")
		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "PO_ORDERJOUR", BATCH_NO$, "", "")

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
	! End of posting program PO_POST_JOURNAL
	!******************************************************************
	END
