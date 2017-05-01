1	%TITLE "Inventory Control Cycle Count Journal Post"
	%SBTTL "IC_POST_CYCLEJOUR"
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
	! ID:IC005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Post Journal\* option posts entries which have
	!	been made into the General Ledger System.
	!	.b
	!	If the entries being posted are out of balance, the message, ^*"Batch
	!	is OUT OF BALANCE - POSTING IS ABORTED.  Hit any key to continue"\*,
	!	will appear on the screen.  Any imbalance must be corrected before the
	!	posting can be executed.  After the posting is completed, the system
	!	will return to the Cycle Count Journal menu.
	!	.lm -5
	!
	! Index:
	!	.x Post>Cycle Counting Journal
	!	.x Cycle Counting Journal>Post
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_POST_CYCLEJOUR
	!	$ LINK/EXE=IC_EXE: IC_POST_CYCLEJOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_POST_CYCLEJOUR.OBJ;*
	!
	! Author:
	!
	!	02/01/1989 - Frank Starman
	!
	! Modification history:
	!
	!	12/18/1990 - Craig Tanner
	!		Deleted last two arguments on calls to IC_TRAN_POST
	!
	!	11/22/1991 - Unknown
	!		Added lines to kill IC_JOURCOUNT files
	!
	!	02/25/92 - Kevin Handy
	!		Added hundreds of error messages to report instead
	!		of Frank's "Aborted" message, which tells nothing
	!		of what caused the abort and causes me to spend 2
	!		hours trying to figure it out.
	!
	!	03/03/92 - Kevin Handy
	!		Changed "CMC$WARNING" to "CMC$_WARNING".
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	10/01/92 - Dan Perkins
	!		Trap IC_JOURADJUST error on line 310. Set FILENAME$
	!		to IC_JOURADJUST at line 1005 and 1010.
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/06/95 - Kevin Handy
	!		(V3.6)
	!		Update source to V3.6 Calico standards.
	!		Modified to use LIB$DELETE_FILE instead of KILL.
	!
	!	07/27/95 - Kevin Handy
	!		Format closer to 80 columns.
	!
	!	08/17/95 - Kevin Handy
	!		Modified to use IC_CYCLEJOUR.NAME$ in the LIB$DELETE
	!		call instead of rebuilding the file name from
	!		several other variables. An attempt to get it to
	!		reliably kill the journals when done.
	!		Same for IC_JOURADJUST.
	!
	!	08/24/95 - Kevin Handy
	!		Modified description put into the GL from
	!		ProductNumber+Location to ProductDescription+Location.
	!
	!	09/05/95 - Kevin Handy
	!		Added test to check status of deleting the journals
	!		after the LIB$DELETE_FILE call.
	!
	!	11/07/95 - Kevin Handy
	!		Format closer to 80 columns.
	!
	!	11/07/95 - Kevin Handy
	!		Do a lookup for device name for IC_JOURCOUNT file
	!		in delete.
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$LIBRARY for LIB$ routines
	!
	!	08/31/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include constants and and some functions
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Include map statements
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP	(IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.HB"
	MAP	(IC_JOURADJUST)	IC_JOURADJUST_CDD	IC_JOURADJUST

	!
	! Include CDD
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
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
	DECLARE SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM
	DECLARE UTL_LOCATION_CDD	UTL_LOCATION_EXAM
	DECLARE UTL_REPORTX_CDD		UTL_REPORTX

	DECLARE LONG	EXIT_STATUS
	DECLARE LONG	CHECK_PERIOD
	DECLARE LONG	INTR_STATUS
	DECLARE LONG	PRNT_SUMMARY

	DECLARE REAL	STDCOST

	DECLARE STRING	TITLE(10%)
	DECLARE STRING	BATCH_NUMBER
	DECLARE STRING	ICPERIOD
	DECLARE STRING	GLPERIOD
	DECLARE STRING	IC.INTER.PERIOD
	DECLARE STRING	GL.INTER.PERIOD
	DECLARE STRING	CHECK_DATE
	DECLARE STRING	POSTDATE
	DECLARE STRING	POSTTIME

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG	FUNCTION	IC_TRAN_POST

	EXTERNAL LONG	FUNCTION	GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES
	EXTERNAL LONG	FUNCTION	PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION	PD_READ_ACCOUNT
	EXTERNAL LONG	FUNCTION	SB_EXAM_SUBACCOUNT
	EXTERNAL LONG	FUNCTION	UTL_EXAM_LOCATION

	EXTERNAL REAL	FUNCTION	PC_READ_COST
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH

	%PAGE

	!
	! Set up error trapping
	!
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
	!	^*(02) Period to Inventory Control\*
	!	.b
	!	.lm +5
	!	The ^*Period to Inventory Control\* field enters the
	!	Inventory Control accounting period into which this
	!	batch will be posted.
	!	.b
	!	The format for entry is YYYYPP.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Period to Inventory Control>Post
	!	.x Post>Period to Inventory Control
	!
	!--

	GLPERIOD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Period to General Ledger\*
	!	.b
	!	.lm +5
	!	The ^*Period to General Ledger\* field enters the
	!	General Ledger accounting period into which this batch
	!	will be posted.
	!	.b
	!	Leaving this field blank will cause the General Ledger, and
	!	Subaccount Ledgers (WIP, JC) not to post.
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
	!	^*(04) Check Date\*
	!	.b
	!	.lm +5
	!	The ^*Check Date\* field posts all transactions or
	!	checks the date of the transaction to see it falls between specified dates
	!	before posting. A ^*Y\* entry causes the dates to be checked, while
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
	TITLE(1%) = "INVENTORY  CYCLE  JOURNAL  POSTING  PROTOCOL"
	TITLE(2%) = "Inventory Control System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

300	!
	! Open the IC Cycle Journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.UPD"
	USE
		FILENAME$ = "IC_CYCLEJOUR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open the IC Inventory Adjustments Journal
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.UPD"
	USE
		FILENAME$ = "IC_JOURADJUST"
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
		UTL_REPORTX, "IC_CYCLEJOUR", BATCH_NO$, &
		IC.INTER.PERIOD, GL.INTER.PERIOD)

	SELECT INTR_STATUS

	!
	! No interrupted processes; go on
	!
	CASE CMC$_NORMAL

	!
	! Restart interrupted process
	!
	CASE CMC$_WARNING

		IF IC.INTER.PERIOD <> ""
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
		UTL_REPORTX, "IC_CYCLEJOUR", BATCH_NO$, &
		ICPERIOD, GLPERIOD) <> CMC$_NORMAL

	EXIT_STATUS = IC_TRAN_POST(OPT_CHECK, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", ICPERIOD)

	SELECT EXIT_STATUS

	!
	! Batch number OK; go on
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

	!
	! Skip checking GL and SA if we aren't posting to them
	!
	GOTO CreateTrans IF (GLPERIOD = "")

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CHECK, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	SELECT EXIT_STATUS

	!
	! Batch number OK; continue
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Other things going on here
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
	PRODUCT$, LOCATION$, TRANSTYPE$, TRANDATE$, INVACCT$, EXPACCT$ = " "

	RESET #IC_CYCLEJOUR.CH%

	!
	! Get the (next) Cycle journal record
	!
1000	WHEN ERROR IN
		GET #IC_CYCLEJOUR.CH%
	USE
		IF ERR = 154%
		THEN
			!
			! Locked block
			!
			SLEEP 1%
			RETRY
		END IF

		CONTINUE Confirm IF ERR = 11%
		FILENAME$ = "IC_CYCLEJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Find the first Adjustment to the Cycle journal
	!
1005	WHEN ERROR IN
		FIND #IC_JOURADJUST.CH%, &
			KEY #0% EQ IC_CYCLEJOUR::LOCATION, &
			REGARDLESS
	USE
		CONTINUE 1000 IF ERR = 155%
		FILENAME$ = "IC_JOURADJUST"
		CONTINUE HelpError
	END WHEN

 NextRec:
	!
	! Get the (next) Adjustment
	!
1010	WHEN ERROR IN
		GET #IC_JOURADJUST.CH%, REGARDLESS
	USE
		CONTINUE 1000 IF ERR = 11%
		FILENAME$ = "IC_JOURADJUST"
		CONTINUE HelpError
	END WHEN

	!
	! Get the next Journal record if we're done with Adjustments
	!
	GOTO GoToNext IF IC_JOURADJUST::LOCATION <> IC_CYCLEJOUR::LOCATION

	!
	! Test product number
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(IC_JOURADJUST::PRODUCT, PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS

	!
	! Product number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		PRODUCT$ = "*"

	!
	! Something else is wrong here
	!
	CASE ELSE
		TEXT$ = "%Undefined Error in Product Number (" + &
			IC_JOURADJUST::PRODUCT + ")"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		GOTO Aborted

	END SELECT

	!
	! Test Location Number
	!
	EXIT_STATUS = UTL_EXAM_LOCATION(IC_JOURADJUST::LOCATION, &
		UTL_LOCATION_EXAM)

	SELECT EXIT_STATUS

	!
	! Location number defined
	!
	CASE CMC$_NORMAL

	!
	! Location undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		LOCATION$ = "*"

	!
	! Other things are going on here
	!
	CASE ELSE
		TEXT$ = "%Undefined Error in Location (" + &
			IC_JOURADJUST::LOCATION + ")"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		GOTO Aborted

	END SELECT

	STDCOST = PC_READ_COST(IC_JOURADJUST::PRODUCT, &
		IC_JOURADJUST::LOCATION, &
		IC_CYCLEJOUR::COUNTDATE, "")

	QTY_COST = FUNC_ROUND(STDCOST * IC_JOURADJUST::QUANTITY, 2%)

	!
	! Generate an IC Transaction Ledger record
	!
	IC_TRANSACTION::PRODUCT		= IC_JOURADJUST::PRODUCT
	IC_TRANSACTION::LOCATION	= IC_JOURADJUST::LOCATION
	IC_TRANSACTION::TRANS_DATE	= IC_CYCLEJOUR::COUNTDATE
	IC_TRANSACTION::PRIMARY_REF	= IC_CYCLEJOUR::PRIMREF
	IC_TRANSACTION::CROSS_REF	= IC_CYCLEJOUR::CROSSREF
	IC_TRANSACTION::SUBACCOUNT	= IC_CYCLEJOUR::SUBACCT
	IC_TRANSACTION::LOT		= ""
	IC_TRANSACTION::STATIONMAN	= IC_CYCLEJOUR::STATIONMAN
	IC_TRANSACTION::TYPE_A		= IC_CYCLEJOUR::TRANSTYPE
	IC_TRANSACTION::QUANTITY_A	= IC_JOURADJUST::QUANTITY
	IC_TRANSACTION::TYPE_B		= ""
	IC_TRANSACTION::QUANTITY_B	= 0.0
	IC_TRANSACTION::COST		= QTY_COST
	IC_TRANSACTION::PRICE		= 0.0
	IC_TRANSACTION::TRANSACCT	= IC_JOURADJUST::ACCOUNT
	IC_TRANSACTION::POSTDATE	= POSTDATE
	IC_TRANSACTION::POSTTIME	= POSTTIME
	IC_TRANSACTION::BATCH		= BATCH_NUMBER

	!
	! Put the record in the IC transaction ledger
	!
	EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, CHECK_PERIOD, "", TITLE(), &
		UTL_REPORTX, IC_TRANSACTION, "")

	!
	! Was the date out of the specified range?
	!
	SELECT EXIT_STATUS

	!
	! Date good; continue
	!
	CASE CMC$_NORMAL

	!
	! Warning; check subaccount number
	!
	CASE CMC$_WARNING
		!
		! See if Sub Account number is defined
		!
		EXIT_STATUS = SB_EXAM_SUBACCOUNT("J", IC_CYCLEJOUR::SUBACCT, &
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

		CASE ELSE
			TEXT$ = "%Undefined Error in Subaccount (" + &
				IC_CYCLEJOUR::SUBACCT + ")"

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			GOTO Aborted

		END SELECT

	!
	! Bad date; set flag and continue
	!
	CASE CMC$_DATEOUT
		TRANDATE$ = "*"

	!
	! Something else went wrong
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Skip past the GL and SA post if we don't need to do them
	!
	GOTO GoToNext IF (PD_PRODUCT_EXAM::METHOD <> "STD") OR (GLPERIOD = "")

	!
	! Generate a GL_YYYY_PP Record
	!
	GL_YYYY_PP::SOURCE	= "CC"
	GL_YYYY_PP::REFNO	= IC_JOURADJUST::PRODUCT
	GL_YYYY_PP::TRANDAT	= IC_CYCLEJOUR::COUNTDATE
	GL_YYYY_PP::DESCR	= LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 25%) + &
		" " + &
		IC_CYCLEJOUR::LOCATION
	GL_YYYY_PP::XREFNO	= IC_CYCLEJOUR::CROSSREF
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= IC_CYCLEJOUR::SUBACCT
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	! Is the Expense account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(IC_JOURADJUST::ACCOUNT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and continue
	!
	CASE CMC$_UNDEFINED
		EXPACCT$ = "*"

	!
	! Something else has gone wrong
	!
	CASE ELSE
		TEXT$ = "%Undefined Error in Account (" + &
			IC_JOURADJUST::ACCOUNT + ")"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted

	END SELECT

	!
	! Finish the Expense record
	!
	GL_YYYY_PP::ACCT	= IC_JOURADJUST::ACCOUNT
	GL_YYYY_PP::AMOUNT	= (-1.0) * QTY_COST
	GL_YYYY_PP::UNITS	= (-1.0) * IC_JOURADJUST::QUANTITY

	!
	! Put the Expense record in the GL Ledger
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, GLPERIOD)
	!
	! Was the date out of the range?
	!
	SELECT EXIT_STATUS

	CASE CMC$_NORMAL
		!
		! Date OK
		!

	CASE CMC$_DATEOUT
		!
		! Date out of range; set flag (if not already set) and go on
		!
		TRANDATE$ = "*"

	CASE CMC$_WARNING
		!
		! Warning; check subaccount number
		!

		!
		! See if Sub Account number is defined
		!
		EXIT_STATUS = SB_EXAM_SUBACCOUNT("J", &
			IC_CYCLEJOUR::SUBACCT, SB_SUBACCOUNT_EXAM)

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
			TEXT$ = "%Undefined Error in Subaccount (" + &
				IC_CYCLEJOUR::SUBACCT + ")"

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			GOTO Aborted

		END SELECT

	CASE ELSE
		!
		! Something else is wrong
		!
		GOTO Aborted

	END SELECT

	!
	! Get the Inventory account number
	!
	EXIT_STATUS = PD_READ_ACCOUNT(IC_JOURADJUST::LOCATION, &
		PD_PRODUCT_EXAM::PROD_TYPE, &
		PD_ACCOUNT_EXAM)

	!
	! Is the Inventory account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_EXAM::INVACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and continue
	!
	CASE CMC$_UNDEFINED
		INVACCT$ = "*"

	!
	! Something else wrong
	!
	CASE ELSE
		TEXT$ = "%Undefined Error in Account (" + &
			PD_ACCOUNT_EXAM::INVACCT + ")"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted

	END SELECT

	!
	! Finish the Inventory record
	!
	GL_YYYY_PP::ACCT	= PD_ACCOUNT_EXAM::INVACCT
	GL_YYYY_PP::AMOUNT	= QTY_COST
	GL_YYYY_PP::UNITS	= IC_JOURADJUST::QUANTITY

	!
	! Put the Inventory record into the GL Ledger
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, &
		GL_CHART_EXAM, GLPERIOD)

	SELECT EXIT_STATUS

	CASE CMC$_NORMAL, CMC$_WARNING
		!
		! Success; go on
		!

	CASE ELSE
		GOTO Aborted

	END SELECT

 GoToNext:
	!
	! Was anything undefined?
	!
	IF INSTR(1%, PRODUCT$ + LOCATION$ + TRANSTYPE$ + &
		SUBACCT$ + TRANDATE$ + INVACCT$ + EXPACCT$, "*")
	THEN
		TEXT$ = PRODUCT$ + IC_JOURADJUST::PRODUCT + " " + &
			LOCATION$ + IC_JOURADJUST::LOCATION + " " + &
			TRANDATE$ + PRNT_DATE(IC_CYCLEJOUR::COUNTDATE, 8%) + " " + &
			TRANSTYPE$ + IC_CYCLEJOUR::TRANSTYPE + "       " + &
			FORMAT$(IC_JOURADJUST::QUANTITY, "######.### ") + &
			EXPACCT$ + IC_JOURADJUST::ACCOUNT + " " + &
			INVACCT$ + GL_YYYY_PP::ACCT + " " + &
			SUBACCT$ + IC_CYCLEJOUR::SUBACCT

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, &
			TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		SUBACCT$, PRODUCT$, LOCATION$, &
			TRANDATE$, INVACCT$, EXPACCT$ = " "

	END IF

	GOTO NextRec

	%PAGE

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************
	EXIT_STATUS = IC_TRAN_POST(OPT_CONFIRM, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", ICPERIOD)

	IF GLPERIOD <> ""
	THEN
		EXIT_STATUS = EXIT_STATUS AND GL_TRAN_POSTGL(OPT_CONFIRM, &
			SUBOPT_NOOPT, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", GLPERIOD)
	END IF

	EXIT_STATUS = EXIT_STATUS AND OUTP_UNDEFCODES(OPT_CONFIRM, TITLE(), &
		UTL_REPORTX, "")

	GOTO Aborted IF EXIT_STATUS <> CMC$_NORMAL

	%PAGE

	!******************************************************************
	! Posting
	!******************************************************************
	GOTO Interrupt IF IC_TRAN_POST(OPT_POSTFILE, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", ICPERIOD) <> CMC$_NORMAL

	!
	! Skip posting to GL and SA if unnecessary
	!
	IF GLPERIOD <> ""
	THEN
		GOTO Interrupt IF GL_TRAN_POSTGL(OPT_POSTFILE, SUBOPT_DETAIL, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			"", "", GLPERIOD) <> CMC$_NORMAL
	END IF

	%PAGE

	CLOSE IC_CYCLEJOUR.CH%
	CLOSE IC_JOURADJUST.CH%

1500	!
	! Remove IC Cycle Journal file
	!
	V% = LIB$DELETE_FILE(IC_CYCLEJOUR.NAME$ + ";*")

	IF (V% AND 1%) = 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "1500(" + NUM1$(V%) + ")", &
			"E", "LIB$DELETE_FILE", "IC_CYCLEJOUR", NUM1$(V%))
	END IF

1510	!
	! Remove IC Inventory Adjustment Journal
	!
	V% = LIB$DELETE_FILE(IC_JOURADJUST.NAME$ + ";*")

	IF (V% AND 1%) = 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "1510(" + NUM1$(V%) + ")", &
			"E", "LIB$DELETE_FILE", "IC_CYCLEJOUR", NUM1$(V%))
	END IF

1520	!
	! Remove IC Journal Count
	!
	CALL READ_DEVICE('IC_JOURCOUNT', IC_JOURCOUNT.DEV$, STAT%)
	V% = LIB$DELETE_FILE(IC_JOURCOUNT.DEV$ + "IC_JOURCOUNT_" + &
		BATCH_NO$ + ".JRL;*")

	IF (V% AND 1%) = 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "1520(" + NUM1$(V%) + ")", &
			"E", "LIB$DELETE_FILE", "IC_JOURCOUNT", NUM1$(V%))
	END IF

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", "", "")

	PRNT_SUMMARY = SUBOPT_FINAL

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************
	IF GLPERIOD <> ""
	THEN
		!
		! Print credit and debit transmittal
		!
		EXIT_STATUS = GL_TRAN_POSTGL(OPT_SUMMARY, PRNT_SUMMARY, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", GLPERIOD)
	END IF

	!
	! Print undefined code if any
	!
	TEXT$ = " Product#        Loc   TransDate   TransType       Qty" + &
		"  ExpAccountNumber    InvAccountNumber    SubAccount"

	EXIT_STATUS = OUTP_UNDEFCODES(OPT_SUMMARY, TITLE(), UTL_REPORTX, TEXT$)

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
			TITLE(), UTL_REPORTX, &
			"", "", "", "")
		GOTO ExitProgram
	END IF

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "", "")

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

32767	!******************************************************************
	! End of posting program IC_POST_CYCLEJOUR
	!******************************************************************
	END
