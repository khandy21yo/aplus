1	%TITLE "Accounts Payable Journal Line Maintenance"
	%SBTTL "AP_MAIN_PJ_L"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_PJ_L(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
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
	! Abstract:HELP
	!	.P
	!	The ^*Enter Purchases\* option enters invoices
	!	from vendors and any other vendor charges into the Purchases Journal.
	!	.P
	!	Vendor charges which are yet to be paid as well as charges for
	!	which hand checks have already been written are entered in the
	!	Purchases Journal.
	!
	! Index:
	!	.x Purchases Journal>Enter Transactions
	!	.x Purchases Journal>Maintain Transactions
	!	.x Purchases Journal
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_PJ_L/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_PJ_L
	!	$ DELETE AP_MAIN_PJ_L.OBJ;*
	!
	! Author:
	!
	!	08/06/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	05/17/88 - Lance Williams
	!		Modified the header.
	!
	!	12/05/88 - Kevin Handy
	!		Increased size of window, and added description of
	!		account number on second line.
	!
	!	07/02/90 - Kevin Handy
	!		Modified to use formatted PO number.
	!
	!	08/02/90 - Kevin Handy
	!		Modified to look into PO register for
	!		valid PO information
	!
	!	12/11/91 - Kevin Handy
	!		Modified to use discount amount entered in the
	!		header for the total discount amout that needs
	!		to be spread about.
	!
	!	12/11/91 - Kevin Handy
	!		Modified default of discount to default to
	!		remaining discount instead of a calculated
	!		discount.
	!
	!	03/05/92 - Dan Perkins
	!		Changed ENTR_3PO to ENTR_3STRING.
	!		Use PO_READ_REG_LINE function to get data
	!		from PO register.
	!
	!	08/19/92 - Kevin Handy
	!		Modified to interface to PO system.
	!
	!	11/05/92 - Kevin Handy
	!		Added code to test subaccount.
	!
	!	11/06/92 - Dan Perkins
	!		Added code to allow adjusting of numeric field
	!		lengths.
	!
	!	02/12/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	09/20/93 - Kevin Handy
	!		Increased RARRAY from 300 to 600.
	!
	!	11/04/94 - Kevin Handy
	!		Added parameter to PO_READ_REG_LINE.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/13/95 - Kevin Handy
	!		If can't find subaccount as a job cost, look
	!		for it as an equipment ledger.
	!
	!	12/19/95 - Kevin Handy
	!		Fix lookup into equipment ledger so it passes "E"
	!		instead of "EL".
	!
	!	07/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/20/97 - Kevin Handy
	!		use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/05/98 - Kevin Handy
	!		Modifications to the list choices key in the
	!		PO number fields.
	!
	!	10/09/98 - Kevin Handy
	!		Display variance information.
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	05/26/99 - Kevin Handy
	!		Don't calculate discount amount by doubling the
	!		amount from previous lines.
	!
	!	08/15/2000 - Kevin Handy
	!		Pass new date field through variance calculation
	!		(transaction date) (robson)
	!
	!	11/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD and Maps
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.HB"
	MAP (AP_PJL)		AP_PJL_CDD		AP_PJL
	MAP (AP_PJL_OLD)	AP_PJL_CDD		AP_PJL_OLD, AP_PJL2

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.HB"
	MAP (AP_PJH)		AP_PJH_CDD		AP_PJH

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	DECLARE			PO_REG_LINE_CDD		PO_REG_LINE_READ
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	DECLARE			PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	DECLARE SB_SUBACCOUNT_CDD SB_SUBACCOUNT_READ

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AP_PJL) &
		AP_PJL.CH%

	COM (CH_SB_SUBACCOUNT) &
		SB_SUBACCOUNT.CH%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
		STRING	SLINE = 4	! Line number
		REAL	AMOUNT		! Amount for record
		REAL	DISCAMT		! Discount Amount for record
	END RECORD

	COM (TT_AP_PJL) RARRAY_RECORD RARRAY(600%)	! Allocate for 300

	COM (AP_MAIN_PJ_L_FRM) FRM$(9%)


	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG   FUNCTION PO_READ_REG_LINE
	EXTERNAL LONG   FUNCTION PO_MAIN_REGLINE
	EXTERNAL LONG	FUNCTION SB_EXAM_SUBACCOUNT
	EXTERNAL LONG	FUNCTION AP_FUNC_CALCVARIANCE

	%PAGE

	ON ERROR GOTO 29000


	SELECT MOPTION
	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Line Items"
		SMG_WINDOW::NHELP = "AP_MAIN_PJ_L"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 10%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 10%
		SMG_WINDOW::NITEMS= 9%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 9%
		SMG_WINDOW::LINREC = 2%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF AP_PJL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_PJL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AP_MAIN_PJ_L = ERR
			CONTINUE 770
		END WHEN

		AP_PJL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.OPN"
		USE
			AP_MAIN_PJ_L = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AP_PJL.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AP_PJL.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = AP_PJL.CH%

	%PAGE


20100	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  01         02   03           04     " + &
			"05       06 07       08         09      ", &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  PO Number  PO L Acct Num     S Acct " + &
			"Oper.    Ut Units        Amount Disc Amt", &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Generate totals
		!
		AMOUNT = AP_PJH::INVAMT
		DISCAMT = AP_PJH::DISCAMT

		AMOUNT = AMOUNT - RARRAY(I%)::AMOUNT &
			FOR I% = 1% TO SMG_WINDOW::TOTREC

		DISCAMT = FUNC_ROUND(DISCAMT - RARRAY(I%)::DISCAMT, 2%) &
			FOR I% = 1% TO SMG_WINDOW::TOTREC

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "####") + &
			"                            REMAINING" + &
			FORMAT$(AMOUNT, " #########.##") + &
			FORMAT$(DISCAMT, " #####.##"), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 8%
			A% = VAL%(MID("013,018,031,038,047,050,059,070", &
				I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)
		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 Eloop:		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Purchase Order Number\*
	!	.p
	!	The ^*Purchase Order Number\* field enters the number
	!	of the purchase order represented by a line item in a Purchases Journal
	!	record.  By entering the purchase order number and the purchase order
	!	line number in the ^*Line\* field, the system will track that a vendor's
	!	invoice for a specific purchase order line item has been record in the
	!	Purchases Journal
	!
	! Index:
	!	.x Purchase Order Number>Line Items
	!	.x Line Items>Purchase Order Number
	!
	!--

			AP_PJL::PONUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				AP_PJL::PONUM, MFLAG OR 2%, "~R 'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PO_MAIN_REGLINE.ID, &
					"V2" + AP_PJH::VENNUM) = 1%
				THEN
					AP_PJL::PONUM = PO_REG_LINE::PO
					AP_PJL::PO_LINE = PO_REG_LINE::PO_LINE
					JUNK% = AP_MAIN_PJ_L(SMG_WINDOW, &
						OPT_ENTRY, I%, 1%, MVALUE) &
						FOR I% = 1% TO 2%
				END IF
				GOTO ELoop
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Purchase Order Line\*
	!	.p
	!	The ^*Purchase Order Line\* enters the line number on the
	!	purchase order related to a specific line item in a Purchases Journal record.
	!
	! Index:
	!	.x Purchase Order Line>Line Items
	!	.x Line Items>Purchase Order Line
	!
	!--
			AP_PJL::PO_LINE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";14", TEMP$, &
				AP_PJL::PO_LINE, MFLAG, "~L0'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PO_MAIN_REGLINE.ID, &
					"V2" + AP_PJH::VENNUM + &
					AP_PJL::PONUM) = 1%
				THEN
					AP_PJL::PO_LINE = PO_REG_LINE::PO_LINE
				END IF
				GOTO ELoop
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Account Number\*
	!	.p
	!	The ^*Account Number\* field enters the General Ledger
	!	account number to which a specific line item is to be charged.  Charges to
	!	different General Ledger account numbers can be accommodated by adding line
	!	items to that section of a Purchases Journal record.
	!
	! Index:
	!	.x Account Number>Line Items
	!	.x Line Items>Account Number
	!
	!--
			AP_PJL::ACCT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";19", TEMP$, &
				AP_PJL::ACCT, MFLAG, "'LLLLLLLLLLL", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				AP_PJL::ACCT = GL_CHART::ACCT &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				GOTO ELoop
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Sub Account\*
	!	.p
	!	The ^*Sub Account\* field enters a work order or job
	!	number.
	!
	! Index:
	!	.x Sub Account>Line Items
	!	.x Line Items>Sub Account
	!
	!--
			AP_PJL::SUBACC = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";32", TEMP$, &
				AP_PJL::SUBACC, MFLAG, "'LLLLL", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Operation\*
	!	.p
	!	The ^*Operation\* field enters a process or operation
	!	code related to a job, work order, etc.
	!
	! Index:
	!	.x Operation>Line Items
	!	.x Line Items>Operation
	!
	!--
			AP_PJL::OPERATION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";39", TEMP$, &
				AP_PJL::OPERATION, MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Use Tax\*
	!	.p
	!	The ^*Use Tax\* field indicates if use tax is applicable
	!	to a specific line item in a purchases journal record.  A ^*Y\* indicates
	!	that the purchase represented in a line item is subject to use tax, while an
	!	^*N\* indicates that use tax is not applicable.
	!
	! Index:
	!	.x Use Tax>Line Items
	!	.x Line Items>Use Tax
	!
	!--
			AP_PJL::USE_TAX_FLAG = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";48", TEMP$, &
				AP_PJL::USE_TAX_FLAG, MFLAG, "!", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Units\*
	!	.p
	!	The ^*Units\* field enters the number of units purchased
	!	of a product represented in a specific line item.  This is memo information
	!	only unless the purchase order system is utilized, in which case the data
	!	entered in this field tracks the number of units invoiced in relation to the
	!	number of units ordered on a specific purchase order.  If the purchase order
	!	system is not utilized, or if the line item does not relate to a purchase
	!	order, this field may be blank.
	!
	! Index:
	!	.x Units>Line Item
	!	.x Line Item>Units
	!
	!--
			AP_PJL::UNITS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";51", TEMP$, AP_PJL::UNITS, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)


			JUNK% = AP_MAIN_PJ_L(SMG_WINDOW, &
				OPT_ENTRY, I%, 1%, MVALUE) &
				FOR I% = 8% TO 8%
		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Amount\*
	!	.p
	!	The ^*Amount\* field enters the dollar amount related
	!	to a specific line item.
	!
	! Index:
	!	.x Amount>Line Items
	!	.x Line Items>Amount
	!
	!--
			AP_PJL::AMOUNT = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";60", TEMP$, &
				AP_PJL::AMOUNT, MFLAG, "#######.##", MVALUE)

			IF (AP_PJL::PONUM = "")
			THEN
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					"        ", &
					SMG_WINDOW::CURLIN + 1%, 51%, , SMG$M_NORMAL)
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					"          ", &
					SMG_WINDOW::CURLIN + 1%, 60%, , SMG$M_NORMAL)
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					"        ", &
					SMG_WINDOW::CURLIN + 1%, 71%, , SMG$M_NORMAL)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					"Variance", &
					SMG_WINDOW::CURLIN + 1%, 51%, , SMG$M_REVERSE)
				!
				! Calculate any variance problems
				!
				EXIT_STATUS = AP_FUNC_CALCVARIANCE(AP_PJL::PONUM, &
					AP_PJL::PO_LINE, AP_PJL::ACCT, AP_PJL::UNITS, &
					AP_PJL::AMOUNT, &
					PO_ACCOUNT$, PO_VARIANCE, PO_AMOUNT, &
					AP_PJH::TRANKEY_DATE)

				!
				! Display the variance
				!
				PO_VARIANCE = &
					ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
					NUM1$(SMG_WINDOW::CURLIN + 1%) + ";60", &
					TEMP$, PO_VARIANCE, 1%, "#######.##", MVALUE)

				IF AP_PJL::AMOUNT = PO_VARIANCE
				THEN
					PERCENTAGE = 0.0
				ELSE
					PERCENTAGE = 100.0 * PO_VARIANCE / &
						(AP_PJL::AMOUNT - PO_VARIANCE)
				END IF

				PERCENTAGE = 9999.99 IF PERCENTAGE > 9999.99
				PERCENTAGE = -999.99 IF PERCENTAGE < -999.99

				PERCENTAGE = &
					ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
					NUM1$(SMG_WINDOW::CURLIN + 1%) + ";71", TEMP$, &
					PERCENTAGE, 1%, "####.##%", MVALUE)
			END IF

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(12) Discount Amount\*
	!	.P
	!	The discount percentage entered in the Vendor Master File
	!	for the subject vendor will cause the system to calculate the
	!	discount amount and automatically enter that amount in this field.
	!	If the discount percentage in the Vendor Master File is zero, the
	!	calculated result will be zero. The automatically entered amount
	!	may be overridden by typing a different amount and pressing ^*<Enter>\*.
	!
	! Index:
	!	.x Discount Amount>Line Items
	!	.x Line Items>Discount Amount
	!
	!--
			AP_PJL::DISCAMT = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";71", TEMP$, &
				AP_PJL::DISCAMT, MFLAG, "#####.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		AP_MAIN_PJ_L = 0%

		SELECT MLOOP

		CASE 1%
			GOSUB PONumExists

		CASE 2%
			IF AP_PJL::PONUM <> ""
			THEN
				IF PO_READ_REG_LINE( &
					AP_PJL::PONUM, AP_PJL::PO_LINE, &
					"EQ", PO_REG_LINE_READ, &
					PO_REG_SUB_LINE_READ, QTY(), &
					"") = CMC$_NORMAL
				THEN
					AP_PJL::ACCT   = PO_REG_SUB_LINE_READ::ACCOUNT
					AP_PJL::SUBACC = PO_REG_SUB_LINE_READ::SUBACCT
					AP_PJL::UNITS = QTY(2%) - QTY(9%)
					AP_PJL::AMOUNT = FUNC_ROUND(AP_PJL::UNITS * &
						PO_REG_SUB_LINE_READ::PRICE, 2%)
					CALL AP_MAIN_PJ_L(SMG_WINDOW, &
						OPT_ENTRY, I%, 1%, "") &
						FOR I% = 1% TO SMG_WINDOW::NITEMS

					JUNK% = AP_MAIN_PJ_L(SMG_WINDOW, &
						OPT_ENTRY, I%, 1%, MVALUE) &
						FOR I% = 1% TO SMG_WINDOW::NITEMS

				ELSE
					AP_MAIN_PJ_L = 1%
					CALL ENTR_3MESSAGE(SCOPE, &
						"PO line doesn't exist in PO Register", 1%)
				END IF
			END IF

		CASE 3%

			AP_MAIN_PJ_L = FUNC_TESTENTRY(SMG_WINDOW, &
				AP_PJL::ACCT, GL_CHART::DESCR, &
				"GL", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 12%), &
				SMG_WINDOW::CURLIN + 1%, 19%,, SMG$M_BOLD)

		CASE 4%
			!
			! Check subaccount (if they enter)
			!
			IF TRM$(AP_PJL::SUBACC) <> ""
			THEN
				V% = SB_EXAM_SUBACCOUNT("J", &
					AP_PJL::SUBACC, &
					SB_SUBACCOUNT_READ)

				IF (V% <> CMC$_NORMAL) and &
					(SB_SUBACCOUNT.CH% > 0%)
				THEN
					V% = SB_EXAM_SUBACCOUNT("E", &
						AP_PJL::SUBACC, &
						SB_SUBACCOUNT_READ)
				END IF

				IF (SB_SUBACCOUNT.CH% > 0%)
				THEN
					IF V% <> CMC$_NORMAL
					THEN
						CALL ENTR_3MESSAGE(SCOPE, &
							"Subaccount is Undefined! ", 0%)
					ELSE
						IF SB_SUBACCOUNT_READ::SSTATUS = "C"
						THEN
							CALL ENTR_3MESSAGE(SCOPE, &
								"Subaccount is Closed! ", 0%)
						END IF
					END IF
				END IF
			END IF

		END SELECT

20500	CASE OPT_SETOLD
		AP_PJL_OLD = AP_PJL

	CASE OPT_RESETOLD
		AP_PJL = AP_PJL_OLD

	CASE OPT_SETDEFAULT
		AP_PJL2 = AP_PJL

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(7%) = "#####.##"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	CASE OPT_RESETDEFAULT
		AP_PJL = AP_PJL2

		AP_PJL::TRANKEY = AP_PJH::TRANKEY

		IF SMG_WINDOW::TOTREC = 0%
		THEN
			AP_PJL::SLINE = "0001"
		ELSE
			AP_PJL::SLINE = &
			FORMAT$(VAL%(RARRAY(SMG_WINDOW::TOTREC)::SLINE) + 1%, &
			"<0>###")
		END IF

		AMOUNT = AP_PJH::INVAMT
		DISCAMT = AP_PJH::DISCAMT

		AMOUNT = AMOUNT - RARRAY(I%)::AMOUNT &
			FOR I% = 1% TO SMG_WINDOW::TOTREC

		DISCAMT = FUNC_ROUND(DISCAMT - RARRAY(I%)::DISCAMT, 2%) &
			FOR I% = 1% TO SMG_WINDOW::TOTREC

		AP_PJL::AMOUNT = AMOUNT

		AP_PJL::DISCAMT = DISCAMT

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AP_PJL.CH%, &
				KEY #0% GE AP_PJL::TRANKEY + "", &
				REGARDLESS
		END SELECT

27000	CASE OPT_ARRAY

		SELECT MLOOP

		CASE 1%

			SMG_WINDOW::TOTREC = 0%

27110			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE AP_PJH::TRANKEY + "", &
					REGARDLESS
			USE
				CONTINUE ExitFunction
			END WHEN

27120			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE ExitFunction IF ERR = 11%
				EXIT HANDLER
			END WHEN

			IF AP_PJL::TRANKEY = AP_PJH::TRANKEY
			THEN
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				RARRAY(SMG_WINDOW::TOTREC)::SLINE = &
					AP_PJL::SLINE
				RARRAY(SMG_WINDOW::TOTREC)::AMOUNT = &
					AP_PJL::AMOUNT
				RARRAY(SMG_WINDOW::TOTREC)::DISCAMT = &
					AP_PJL::DISCAMT
				GOTO 27120
			END IF

		CASE 2%

			RARRAY(I%) = RARRAY(I% + 1%) &
				FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%

		CASE 3%

			RARRAY(MFLAG)::LINRFA	= GETRFA(SMG_WINDOW::CHAN)
			RARRAY(MFLAG)::SLINE	= AP_PJL::SLINE
			RARRAY(MFLAG)::AMOUNT	= AP_PJL::AMOUNT
			RARRAY(MFLAG)::DISCAMT	= AP_PJL::DISCAMT

		CASE 4%

27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		CASE 5%

			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		CASE 6%

			AP_PJL::TRANKEY = RIGHT(MVALUE, 2%)

		CASE 7%

			GL_CHART::DESCR = "???????????????????" &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + AP_PJL::ACCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 12%), &
				SMG_WINDOW::CURLIN + 1%, 19%,, SMG$M_BOLD)

		END SELECT
	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

28000	!*******************************************************************
	! Check up on PO number in register
	!*******************************************************************
 PONumExists:
	!
	! Blank is good
	!
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)
	GOTO 28090 IF AP_PJL::PONUM = ""

	!
	! Work on it to see if it is in register
	!
	IF PO_READ_REG_LINE(AP_PJL::PONUM, "", "EQ", PO_REG_LINE_READ, &
		PO_REG_SUB_LINE_READ, QTY(), "") <> CMC$_NORMAL
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"PO number doesn't exist in PO Register", 1%)
		GOTO 28090
	END IF

	IF PO_REG_LINE_READ::VENDOR <> AP_VENDOR::VENNUM
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"PO doesn't belong to this Vendor!", 0%)
	END IF

28090	RETURN

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
