1	%TITLE "Inventory Transaction Ledger"
	%SBTTL "IC_MAIN_TRANSACTION"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_MAIN_TRANSACTION(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.p
	!	Maintains the detailed transaction ledger for a specific inventory
	!	period.
	!
	! Index:
	!	.x Period Transaction
	!	.x Archive>Period Transaction
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAIN_TRANSACTION/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN IC_MAIN_TRANSACTION
	!	$ DELETE IC_MAIN_TRANSACTION.OBJ;*
	!
	! Author:
	!
	!	05/20/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	05/31/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	12/20/91 - Dan Perkins
	!		Modified View by Cross_Referecne to display only one
	!		uppercase character.  Also moved cursor to displaying
	!		LOT number to display Cross_Reference.
	!
	!	12/26/91 - Dan Perkins
	!		Added BATCH as a key field in OPT_FIND. Modified
	!		post date, time, and batch fields to fields that
	!		can be maintained.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/02/92 - Dan Perkins
	!		Put sort keys in proper order so OPT_FIND and
	!		OPT_VIEW would work.
	!
	!	04/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/24/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/02/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/01/94 - Kevin Handy
	!		Fixed OPT_TESTENTRY to check transaction types
	!		at positions 10 and 12, instead of 12 and 14.
	!
	!	03/01/94 - Kevin Handy
	!		Modifications to make the batch number possible
	!		to default (don't automatically force to '000000').
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/18/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION
	MAP (IC_TRANSACTION_OLD) IC_TRANSACTION_CDD	IC_TRANSACTION_OLD, &
							IC_TRANSACTION2

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE)	UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_IC_TRANSACTION) &
		IC_TRANSACTION.CH%, &
		IC_TRANSACTION.READONLY%

	COM (PERIOD_IC_TRANSACTION) YYYYPP$ = 6%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT

		!*************************************************************
		! Set up information
		!*************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Inventory Transaction " + YYYYPP$ + " File"
		SMG_WINDOW::NHELP = "IC_MAIN_TRANSACTION"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 18%

		SMG_WINDOW::NKEYS = 5%
		SMG_WINDOW::KNAME(0%) = "Product_num"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "Batch"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 18%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
		SMG_WINDOW::KNAME(2%) = "Subaccount"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 8%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%
		SMG_WINDOW::KNAME(3%) = "Cross_ref"
			SMG_WINDOW::KFIELD(3%, 0%) = 2%
			SMG_WINDOW::KFIELD(3%, 1%) = 5%
			SMG_WINDOW::KFIELD(3%, 2%) = 1%
		SMG_WINDOW::KNAME(4%) = "Location"
			SMG_WINDOW::KFIELD(4%, 0%) = 2%
			SMG_WINDOW::KFIELD(4%, 1%) = 2%
			SMG_WINDOW::KFIELD(4%, 2%) = 1%

		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF IC_TRANSACTION.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF IC_TRANSACTION.READONLY%
			GOTO 790
		END IF


		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			IC_MAIN_TRANSACTION = ERR
			CONTINUE 770
		END WHEN

		IC_TRANSACTION.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
		USE
			IC_MAIN_TRANSACTION = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		IC_TRANSACTION.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(IC_TRANSACTION.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = IC_TRANSACTION.CH%

		WHEN ERROR IN
			RESET #IC_TRANSACTION.CH%
			GET #IC_TRANSACTION.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	3,5, "(01) Product #", &
			4,5, "(02) Location #", &
			5,5, "(03) Date", &
			7,5, "(04) Reference #", &
			8,5, "(05) Cross Ref", &
			9,5, "(06) Lot#", &
			10,5, "(07) Operator", &
			11,5, "(08) Subaccount", &
			12,5, "(09) Trans Account", &
			7,45, "(10) Trans Type A", &
			8,45, "(11) Quantity A", &
			9,45, "(12) Trans Type B", &
			10,45, "(13) Quantity B", &
			11,45, "(14) Cost Amount", &
			12,45, "(15) Price Amount", &
			15,45, "(16) Post Date", &
			16,45, "(17) Post Time", &
			17,45, "(18) Post Batch", &
			0, 0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%
		WHILE (XPOS% <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS%, YPOS%, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Product _#\* field
	!	enters an assigned number which identifies a specific product.
	!	.b
	!	The field will accommodate up to fourteen (14) alphanumeric
	!	characters.
	!	.b
	!	Each product number must be unique. No duplicates are allowed.
	!	.lm -5
	!
	! Index:
	!	.x Product>Number
	!
	!--
			IC_TRANSACTION::PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;25",TEMP$, IC_TRANSACTION::PRODUCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX")=1%
				THEN
					IC_TRANSACTION::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Location _#\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field
	!	enters the location code which is established in the Company
	!	Profile file located in the Utility system.
	!	.b
	!	The field will accommodate up to four (4) alphanumeric
	!	characters.
	!	.b
	!	Valid Location codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Location Number>Period Transaction
	!	.x Period Transaction>Location Number
	!	.x Number>Location
	!
	!--
			IC_TRANSACTION::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;25",	TEMP$, IC_TRANSACTION::LOCATION, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0")=1%
				THEN
					IC_TRANSACTION::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field enters the valid date for
	!	this particular record.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Date>Period Transaction
	!	.x Period Transaction>Date
	!
	!--
			IC_TRANSACTION::TRANS_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;25", TEMP$, &
				IC_TRANSACTION::TRANS_DATE, MFLAG, "'E", &
				MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Reference #\*
	!	.b
	!	.lm +5
	!	The ^*Reference _#\* field contains the number which posting in the
	!	General Ledger will be completed under.  An example of a
	!	Primary Reference number is the invoice number.
	!	.lm -5
	!
	! Index:
	!	.x Reference Number
	!
	!--
			IC_TRANSACTION::PRIMARY_REF = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;25", TEMP$, &
				IC_TRANSACTION::PRIMARY_REF, MFLAG, "'E", &
				MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Cross Reference\*
	!	.b
	!	.lm +5
	!	The ^*Cross Reference\* field refers to the individual who originated the
	!	transaction. Examples of a ^*Cross Reference\* are a customer or vendor
	!	number. This entry connects the transaction to the
	!	person responsible.
	!	.lm -5
	!
	! Index:
	!	.x Cross Reference
	!
	!--
			IC_TRANSACTION::CROSS_REF = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;25", TEMP$, &
				IC_TRANSACTION::CROSS_REF, MFLAG, "'E", &
				MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Lot Number\*
	!	.b
	!	.lm +5
	!	The ^*Lot Number\* field enters the number which refers
	!	to the specific production process. ^*Lot Numbers\* are especially helpful
	!	when tracing items in case of a problem.
	!	.lm -5
	!
	! Index:
	!	.x Lot Number
	!
	!--
			IC_TRANSACTION::LOT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;25", TEMP$, &
				IC_TRANSACTION::LOT, MFLAG, "'E", &
				MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters the code
	!	which will identify the operator responsible for this
	!	particular journal entry.
	!	.lm -5
	!
	! Index:
	!	.x Operator>Period Transaction
	!	.x Period Transaction>Operator
	!
	!--
			IC_TRANSACTION::STATIONMAN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;25", TEMP$, &
				IC_TRANSACTION::STATIONMAN, MFLAG, "'E", &
				MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Subaccount\*
	!	.b
	!	.lm +5
	!	The ^*Subaccount\* field creates a cross matrix for the sub
	!	ledger. Each entry refers to the specific job identification. Examples of
	!	a ^*Subaccount\* are job _# and work order.
	!	.lm -5
	!
	! Index:
	!	.x Subaccount
	!
	!--
			IC_TRANSACTION::SUBACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;25", TEMP$, &
				IC_TRANSACTION::SUBACCOUNT, MFLAG, "'E", &
				MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Transaction Account\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Account\* field enters
	!	the General Ledger Chart of Account number which this
	!	record has reference to.
	!	.b
	!	Valid General Ledger Chart of Account numbers may be viewed by
	!	pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Account
	!	.x Account>Transaction
	!
	!--
			IC_TRANSACTION::TRANSACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;25", TEMP$, &
				IC_TRANSACTION::TRANSACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
				IC_TRANSACTION::TRANSACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Transaction Type A\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Type A\* field enters a valid
	!	transaction type code which has been established in the transaction
	!	type file.
	!	.b
	!	Valid Type codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Type A
	!	.x Type>A>Transaction
	!
	!--
			IC_TRANSACTION::TYPE_A = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;65", TEMP$, &
				IC_TRANSACTION::TYPE_A, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, "V0") = 1%
				THEN
					IC_TRANSACTION::TYPE_A = &
						UTL_TRANSTYPE::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Quantity A\*
	!	.b
	!	.lm +5
	!	The ^*Quantity A\* field refers to the number of items sold, exchanged,
	!	received, etc., in relation to the transaction type.
	!	.lm -5
	!
	! Index:
	!	.x Quantity
	!
	!--
			IC_TRANSACTION::QUANTITY_A = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"08;65", TEMP$, IC_TRANSACTION::QUANTITY_A, &
				MFLAG, "###,###.###", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Transaction Type B\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Type B\* field enters a valid
	!	transaction type code which has been established in the transaction
	!	type file.
	!	.b
	!	Valid Type codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Type B
	!	.x Type>B>Transaction
	!
	!--
			IC_TRANSACTION::TYPE_B = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;65", TEMP$, &
				IC_TRANSACTION::TYPE_B, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, "V0") = 1%
				THEN
					IC_TRANSACTION::TYPE_B = &
						UTL_TRANSTYPE::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Quantity B\*
	!	.b
	!	.lm +5
	!	The ^*Quantity B\* field refers to the number of items sold, exchanged,
	!	received, etc. in relation to the transaction type.
	!	.lm -5
	!
	! Index:
	!	.x Quantity
	!
	!--
			IC_TRANSACTION::QUANTITY_B = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;65", TEMP$, IC_TRANSACTION::QUANTITY_B, &
				MFLAG, "###,###.###", MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	^*(14) Cost Amount\*
	!	.B
	!	.LM +5
	!	The ^*Cost Amount\* field enters the cost of the item
	!	to the company.
	!	.LM -5
	!
	! Index:
	!	.x Cost Amount
	!	.x Amount>Cost
	!
	!--
			IC_TRANSACTION::COST = ABS(ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;65", TEMP$, IC_TRANSACTION::COST, MFLAG, &
				"###,###.##", MVALUE))

		CASE 15%
	!++
	! Abstract:FLD015
	!	^*(15) Price Amount\*
	!	.B
	!	.LM +5
	!	The ^*Price Amount\* field enters the selling amount
	!	that will be charged to each purchaser of the item.
	!	.LM -5
	!
	! Index:
	!	.x Price Amount
	!
	!--
			IC_TRANSACTION::PRICE = ABS(ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;65", TEMP$, IC_TRANSACTION::PRICE, MFLAG, &
				"###,###.##", MVALUE))

		CASE 16%
	!++
	! Abstract:FLD016
	!	^*(16) Post Date\*
	!	.B
	!	.LM +5
	!	The ^*Post Date\* field maintains the
	!	date the transaction was posted.
	!	.LM -5
	!
	! Index:
	!	.x Post Date
	!
	!--
			IC_TRANSACTION::POSTDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;65", TEMP$, &
				IC_TRANSACTION::POSTDATE, MFLAG, "'E", &
				MVALUE)

		CASE 17%
	!++
	! Abstract:FLD017
	!	^*(17) Post Time\*
	!	.B
	!	.LM +5
	!	The ^*Post Time\* field maintains the
	!	time the transaction was posted.
	!	.LM -5
	!
	! Index:
	!	.x Post Time
	!
	!--
			IC_TRANSACTION::POSTTIME = ENTR_3TIME(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"16;65", TEMP$, IC_TRANSACTION::POSTTIME, &
				MFLAG, "###,###.##", MVALUE)

		CASE 18%
	!++
	! Abstract:FLD018
	!	^*(18) Post Batch\*
	!	.B
	!	.LM +5
	!	The ^*Post Batch\* field maintains the
	!	number of the batch to which the transaction was posted.
	!	.LM -5
	!
	! Index:
	!	.x Post Batch
	!
	!--
			IC_TRANSACTION::BATCH = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "17;65", TEMP$, &
				IC_TRANSACTION::BATCH, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		IC_MAIN_TRANSACTION = 0%

		SELECT MLOOP

		CASE 1%
			IF IC_TRANSACTION::PRODUCT = ""
			THEN
				IC_MAIN_TRANSACTION = 1%
			ELSE
				!
				! Is the input defined?
				!
				IC_MAIN_TRANSACTION = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_TRANSACTION::PRODUCT, &
					PD_PRODUCT::DESCRIPTION, &
					"IC", MLOOP, "PROD", &
					"Product", PD_MAIN_PRODUCT.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 37%), &
				3%,41%, , SMG$M_BOLD)

		CASE 2%
			IF IC_TRANSACTION::LOCATION = ""
			THEN
				IC_MAIN_TRANSACTION = 1%
			ELSE
				!
				! Is the input defined?
				!
				IC_MAIN_TRANSACTION = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_TRANSACTION::LOCATION, &
					UTL_LOCATION::LOCNAME, &
					"IC", MLOOP, "PROD", &
					"Location", UTL_MAIN_LOCATION.ID)
			END IF

		CASE 10%
			IF IC_TRANSACTION::TYPE_A = ""
			THEN
				IC_MAIN_TRANSACTION = 1%
			ELSE
				!
				! Is the input defined?
				!
				IC_MAIN_TRANSACTION = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_TRANSACTION::TYPE_A, &
					UTL_TRANSTYPE::DESCRIPTION, &
					"IC", MLOOP, "PROD", &
					"Transaction Type", UTL_MAIN_TRANSTYPE.ID)
			END IF

		CASE 12%
			!
			! Is the input defined?
			!
			IF IC_TRANSACTION::TYPE_B <> ""
			THEN
				IC_MAIN_TRANSACTION = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_TRANSACTION::TYPE_B, &
					UTL_TRANSTYPE::DESCRIPTION, &
					"IC", MLOOP, "PROD", &
					"Transaction Type", UTL_MAIN_TRANSTYPE.ID)
			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			PRODDESC$ = STRING$(40%, 63%)
			PRODDESC$ = PD_PRODUCT::DESCRIPTION &
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
				"Q0" + IC_TRANSACTION::PRODUCT) = 1%
			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PRODDESC$, 37%), &
				3%, 41%, , SMG$M_BOLD)
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			PRNT_DATE(IC_TRANSACTION::POSTDATE, 8%), 15%, 65%, , &
			SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			PRNT_TIME(IC_TRANSACTION::POSTTIME, 1%), 16%, 65%, , &
			SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			IC_TRANSACTION::BATCH, 17%, 65%, , SMG$M_BOLD)

	!
	! Set IC_TRANSACTION_OLD value
	!
20500	CASE OPT_SETOLD
		IC_TRANSACTION_OLD = IC_TRANSACTION

	!
	! Restore IC_TRANSACTION_OLD value
	!
	CASE OPT_RESETOLD
		IC_TRANSACTION = IC_TRANSACTION_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		IC_TRANSACTION2 = IC_TRANSACTION

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		IC_TRANSACTION = IC_TRANSACTION2

		IF IC_TRANSACTION::BATCH = ""
		THEN
			IC_TRANSACTION::POSTDATE = DATE_TODAY
			IC_TRANSACTION::POSTTIME = TIME_NOW
			IC_TRANSACTION::BATCH    = "000000"
		END IF

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Product #      Loc   Date    " + &
				" Reference        CrossRef      Subaccount " + &
				"Batch        Cost       Price"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "017,023,032,049,063,074,081,092"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = IC_TRANSACTION::PRODUCT + " " + &
				IC_TRANSACTION::LOCATION + "  " + &
				PRNT_DATE(IC_TRANSACTION::TRANS_DATE, 6%) + " "    + &
				IC_TRANSACTION::PRIMARY_REF + " " + &
				IC_TRANSACTION::CROSS_REF + "    " + &
				IC_TRANSACTION::SUBACCOUNT + " " + &
				IC_TRANSACTION::BATCH + " " + &
				FORMAT$(IC_TRANSACTION::COST, &
					"#######.##") + "  " + &
				FORMAT$(IC_TRANSACTION::PRICE, &
					"#######.##")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE IC_TRANSACTION::PRODUCT + &
				IC_TRANSACTION::LOCATION + &
				IC_TRANSACTION::TRANS_DATE, &
				REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE IC_TRANSACTION::BATCH + &
				IC_TRANSACTION::PRODUCT, &
				REGARDLESS

		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE IC_TRANSACTION::SUBACCOUNT + &
				IC_TRANSACTION::PRODUCT, &
				REGARDLESS

		CASE 3%
			FIND #SMG_WINDOW::CHAN, &
				KEY #3% GE IC_TRANSACTION::CROSS_REF + &
				IC_TRANSACTION::PRODUCT, &
				REGARDLESS

		CASE 4%
			FIND #SMG_WINDOW::CHAN, &
				KEY #4% GE IC_TRANSACTION::LOCATION + &
				IC_TRANSACTION::PRODUCT, &
				REGARDLESS

		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!*****************************************************************
	! Trap errors
	!*****************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
