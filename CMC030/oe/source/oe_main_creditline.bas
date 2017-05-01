1	%TITLE "Credit Line Journal"
	%SBTTL "OE_MAIN_CREDITLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_CREDITLINE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991 BY
	!
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The Credit Line Journal is used to enter the number of credit
	!	memos issued.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_CREDITLINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN OE_MAIN_CREDITLINE
	!	$ DELETE OE_MAIN_CREDITLINE.OBJ;*
	!
	! Author:
	!
	!	08/30/91 - Dan Perkins
	!
	! Modification history:
	!
	!	10/17/91 - Dan Perkins
	!		Modified to accomodate new record file layout.
	!
	!	04/16/92 - Dan Perkins
	!		Allow formatting of numeric fields using /SET.
	!
	!	05/06/92 - Dan Perkins
	!		Added MISCELLANEOUS charges field.
	!
	!	11/25/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	11/30/92 - Dan Perkins
	!		Fixed problem of not changing lines when header
	!		memo number is changed.
	!
	!	12/01/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/09/93 - Dan Perkins
	!		Added Reason Code field.
	!
	!	04/12/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include Statements
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITLINE.HB"
	MAP (OE_CREDITLINE)	OE_CREDITLINE_CDD	OE_CREDITLINE
	MAP (OE_CREDITLINE_OLD)	OE_CREDITLINE_CDD	OE_CREDITLINE_OLD, &
							OE_CREDITLINE_DEF

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITJOUR.HB"
	MAP (OE_CREDITJOUR)	OE_CREDITJOUR_CDD	OE_CREDITJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.HB"
	MAP (OE_CREASON)	OE_CREASON_CDD		OE_CREASON

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	DECLARE			OE_PROMO_CDD		OE_PROMO_READ

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Common Statements
	!
	COM (CH_OE_CREDITJOUR) &
		BATCH_NO$ = 2%

	COM (CH_OE_CREDITLINE) &
		OE_CREDITLINE.CH%, &
		OE_CREDITLINE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE
	EXTERNAL LONG	FUNCTION OE_READ_PROMO
	EXTERNAL REAL	FUNCTION PC_READ_COST
	EXTERNAL REAL	FUNCTION PC_READ_PRICE
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Credit Memo Lines"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "OE_MAIN_CREDITLINE"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 11%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 8%
		SMG_WINDOW::NITEMS= 9%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::HVIEW = 125%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 8%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Line"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 0%
		SMG_WINDOW::KNAME(1%) = "Product_num"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		COM (OE_MAIN_CREDITLINE_FRM) FRM$(9%)

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF OE_CREDITLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_CREDITLINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			EXIT HANDLER
		END WHEN

		OE_CREDITLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITLINE.OPN"
		USE
			CONTINUE 770 IF ERR = 5%
			EXIT HANDLER
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_CREDITLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_CREDITLINE.CH%)
		GOTO 32767

790		SMG_WINDOW::CHAN  = OE_CREDITLINE.CH%
		WHEN ERROR IN
			RESET #OE_CREDITLINE.CH%
			GET #OE_CREDITLINE.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN


	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!******************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	01,05, "(01) Reason Code", &
			02,05, "(02) Product #", &
			03,05, "(03) Qty Credited", &
			04,05, "(04) Qty into Inv", &
			05,05, "(05) Unit Price", &
			06,05, "(06) Promo Amt Off", &
			07,05, "(07) Misc Charges", &
			08,05, "(08) Discount %", &
			09,05, "(09) Unit Cost", &
			08,45, "Ext. Price", &
			09,45, "Ext. Cost", &
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

 Reentry:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.x Reason Code
	!	^*(01) Reason Code\*
	!	.b
	!	.lm +5
	!	The ^*Reason Code\* field enters
	!	the reason for the Credit Memo.
	!	.b
	!	Valid Reason Codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITLINE::REASON = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;25", TEMP$, OE_CREDITLINE::REASON, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(OE_MAIN_CREASON.ID, "VX") = 1%
				THEN
					OE_CREDITLINE::REASON = &
						OE_CREASON::CREASON
				END IF
				GOTO Reentry

			CASE SMG$K_TRM_F17
				V% = MAIN_WINDOW(OE_MAIN_CREASON.ID, "M")
					OE_CREDITLINE::REASON = &
						OE_CREASON::CREASON
				GOTO Reentry

			END SELECT

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Product Number\*
	!	.b
	!	.lm +5
	!	The ^*Product Number\* field enters the
	!	product number of the items which are being credited.
	!	.b
	!	Valid Product Numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Product Number
	!
	!--
			OE_CREDITLINE::PRODUCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;25", TEMP$, OE_CREDITLINE::PRODUCT, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX" + &
					OE_CREDITLINE::PRODUCT) = 1%
				THEN
					OE_CREDITLINE::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reentry
			END SELECT

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x Quantity Credited
	!	^*(03) Quantity Credited\*
	!	.b
	!	.lm +5
	!	The ^*Quantity Credited\* field enters the number of
	!	units which are to be credited.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITLINE::CREDQTY = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;25", TEMP$, OE_CREDITLINE::CREDQTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Unit Price
	!	^*(05) Unit Price\*
	!	.b
	!	.lm +5
	!	The ^*Unit Price\* field enters the price of
	!	a particular unit which is being credited.
	!	.b
	!	If a product is available in a particular unit of
	!	measure, i.e. "gallon", "piece", "yard", etc., that price is
	!	entered in this field.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITLINE::INVQTY = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;25", TEMP$, OE_CREDITLINE::INVQTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 5%
	!++
	! Abstract:FLD004
	!	.x Unit Price
	!	^*(05) Unit Price\*
	!	.b
	!	.lm +5
	!	The ^*Unit Price\* field enters the price of
	!	a particular unit which is being credited.
	!	.b
	!	If a product is available in a particular unit of
	!	measure, i.e. "gallon", "piece", "yard", etc., that price is
	!	entered in this field.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITLINE::PRICE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;25", TEMP$, OE_CREDITLINE::PRICE, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x Miscellenous Charges
	!	^*(07) Miscellenous Charges\*
	!	.b
	!	.lm +5
	!	The ^*Miscellanous Charges\* field enters
	!	any additional charges to be credited which may have been entered on the
	!	original order.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITLINE::PROMO = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;25", TEMP$, OE_CREDITLINE::PROMO, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 7%
	!++
	! Abstract:FLD006
	!	.x Miscellenous Charges
	!	^*(07) Miscellenous Charges\*
	!	.b
	!	.lm +5
	!	The ^*Miscellanous Charges\* field enters
	!	any additional charges to be credited which may have been entered on the
	!	original order.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITLINE::MISC = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;25", TEMP$, OE_CREDITLINE::MISC, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	.x Discount
	!	^*(08) Discount\*
	!	.b
	!	.lm +5
	!	The Discount field enters the discount
	!	percentage given on the original order.
	!	.b
	!	The field may contain a figure as large as 99.999.
	!	.lm +5
	!
	! Index:
	!
	!--
			OE_CREDITLINE::DISCOUNT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;32", TEMP$, OE_CREDITLINE::DISCOUNT, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	.x Unit Cost
	!	^*(09) Unit Cost\*
	!	.b
	!	.lm +5
	!	The Unit Cost field enters the cost of each unit
	!	which is to be credited. If the product is available in a particular
	!	unit of measure, i.e. "gallon", "piece", etc., that price is entered
	!	here.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITLINE::COST = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;25", TEMP$, OE_CREDITLINE::COST, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		OE_MAIN_CREDITLINE = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Test Reason Code
			!
			OE_MAIN_CREDITLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_CREDITLINE::REASON, &
				OE_CREASON::DESCR, &
				"OE", MLOOP, "PROG", &
				"Reason Code", OE_MAIN_CREASON.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_CREASON::DESCR, 1%, 40%, , SMG$M_BOLD)

		CASE 2%
			!
			! Test OE_CREDITLINE product
			!
			OE_MAIN_CREDITLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_CREDITLINE::PRODUCT, &
				PD_PRODUCT::DESCRIPTION, &
				"OE", MLOOP, "PROG", &
				"Product", PD_MAIN_PRODUCT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 2%, 40%, , SMG$M_BOLD)

			OE_CREDITLINE::PRICE = PC_READ_PRICE( &
				OE_CREDITLINE::PRODUCT, &
				OE_CREDITJOUR::LOCATION, &
				AR_35CUSTOM::TTYPE, OE_CREDITJOUR::ORDDATE, &
				"", "", "")

			OE_CREDITLINE::COST = PC_READ_COST( &
				OE_CREDITLINE::PRODUCT, &
				OE_CREDITJOUR::LOCATION, &
				OE_CREDITJOUR::ORDDATE, "")

			V% = OE_READ_PROMO(OE_CREDITLINE::PRODUCT, &
				OE_CREDITJOUR::ORDDATE, &
				OE_CREDITJOUR::CUSNUM, OE_PROMO_READ, &
				OE_CREDITLINE::PRICE, OE_CREDITLINE::PROMO)

		END SELECT

	CASE OPT_DISPLAY

		DISC = FUNC_ROUND(OE_CREDITLINE::CREDQTY &
			* ((OE_CREDITLINE::PRICE - OE_CREDITLINE::PROMO) &
			* (1 - (OE_CREDITLINE::DISCOUNT / 100)) &
			+ OE_CREDITLINE::MISC), 2%)

		COST = FUNC_ROUND(OE_CREDITLINE::INVQTY &
			* OE_CREDITLINE::COST, 3%)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(DISC, "##,###,###.##"), 8%, 61%, , &
			SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(COST, "##,###,###.###"), 9%, 61%, , &
			SMG$M_BOLD)

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			OE_CREASON::DESCR = &
				STRING$(LEN(OE_CREASON::DESCR), A"?"B) &
				IF MAIN_WINDOW(OE_MAIN_CREASON.ID, "Q0" + &
				OE_CREDITLINE::REASON) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_CREASON::DESCR, 1%, 40%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			PD_PRODUCT::DESCRIPTION = &
				STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "Q0" + &
				OE_CREDITLINE::PRODUCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 2%, 40%, , SMG$M_BOLD)
		END IF

	!
	! Set OE_CREDITLINE_OLD value
	!
20500	CASE OPT_SETOLD
		OE_CREDITLINE_OLD = OE_CREDITLINE

	!
	! Restore OE_CREDITLINE_OLD value
	!
	CASE OPT_RESETOLD
		OE_CREDITLINE = OE_CREDITLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_CREDITLINE_DEF = OE_CREDITLINE

		IF MFLAG = 1%
		THEN
			SELECT MLOOP

			CASE 0%
				FRM$(3%) = "#,###,###.##"
				FRM$(4%) = "#,###,###.##"
				FRM$(5%) = "#,###,###.###"
				FRM$(6%) = "#,###,###.##"
				FRM$(7%) = "#,###,###.##"
				FRM$(8%) = "##.###"
				FRM$(9%) = "#,###,###.###"

			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_CREDITLINE = OE_CREDITLINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		OE_CREDITLINE::MEMONUM  = MVALUE

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE OE_CREDITLINE::MEMONUM + "", &
				REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE OE_CREDITLINE::PRODUCT + &
				OE_CREDITLINE::MEMONUM, REGARDLESS

		END SELECT

	!
	! View header
	!
20600	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  RC ProdNum        " + &
				"     CredQty       QtyInv     UnitPrice"  + &
				"     PromoAmt Discount%      UnitCost"    + &
				"      ExtPrice        ExtCost"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "005,020,033,046,060,073,083,097,111"

		!
		! Convert current record into text
		!
		CASE 3%
			DISC = FUNC_ROUND(OE_CREDITLINE::CREDQTY*(OE_CREDITLINE::PRICE-OE_CREDITLINE::PROMO) &
				* (1-(OE_CREDITLINE::DISCOUNT/100)), 2%)

			COST = FUNC_ROUND(OE_CREDITLINE::INVQTY &
				* OE_CREDITLINE::COST, 3%)

			MVALUE = OE_CREDITLINE::REASON                          + " "    + &
				OE_CREDITLINE::PRODUCT				+ " " + &
				FORMAT$(OE_CREDITLINE::CREDQTY, "#,###,###.##")	+ " "	+ &
				FORMAT$(OE_CREDITLINE::INVQTY, "#,###,###.##")	+ " "	+ &
				FORMAT$(OE_CREDITLINE::PRICE, "#,###,###.###")	+ " "	+ &
				FORMAT$(OE_CREDITLINE::PROMO, "#,###,###.##")	+ "    " + &
				FORMAT$(OE_CREDITLINE::DISCOUNT, "##.###")	+ " "	+ &
				FORMAT$(OE_CREDITLINE::COST, "#,###,###.###")	+ " "	+ &
				FORMAT$(DISC, "##,###,###.##")			+ " "	+ &
				FORMAT$(COST, "##,###,###.###")
		END SELECT

	!
	! Handle array of records
	!
	CASE OPT_SUBWIND

		SELECT MLOOP

		!
		! Find first record (if there is any)
		!
		CASE 1%
			!
			! Check if erasing the whole order's lines and if so
			! then reset the inventory quantities correctly?
			!
			SELECT MFLAG

			CASE -1%

				V% = IC_WRIT_35BALANCE (OE_CREDITLINE::PRODUCT, &
					OE_CREDITJOUR::LOCATION, "RT", &
					-OE_CREDITLINE::INVQTY)

			END SELECT

			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, KEY #0% EQ MVALUE, REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000 IF ERR = 155%
				EXIT HANDLER
			END WHEN

		!
		! Find starting record (if there is any)
		!
		CASE 2%
			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27115			!
			! Search for starting record
			!
			SELECT MFLAG

			CASE 0%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #0% GE MVALUE, REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN

			CASE 1%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #1% GE OE_CREDITLINE::PRODUCT + &
						MVALUE, REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN

			END SELECT

			!
			! Get a record
			!
			SMG_WINDOW::CURREC = 0%

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%

			IF OE_CREDITLINE::MEMONUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			OE_CREDITLINE::MEMONUM = MVALUE

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"
			V% = IC_WRIT_35BALANCE (OE_CREDITLINE::PRODUCT, &
				OE_CREDITJOUR::LOCATION, "RT", &
				OE_CREDITLINE::INVQTY)

		CASE "Change", "Blank", "Initialize"
			V% = IC_WRIT_35BALANCE (OE_CREDITLINE::PRODUCT, &
				OE_CREDITJOUR::LOCATION, "RT", &
				-OE_CREDITLINE_OLD::INVQTY)

			V% = IC_WRIT_35BALANCE (OE_CREDITLINE::PRODUCT, &
				OE_CREDITJOUR::LOCATION, "RT", &
				OE_CREDITLINE::INVQTY)

		CASE "Erase"
			IF MLOOP <> 1%
			THEN
				V% = IC_WRIT_35BALANCE(OE_CREDITLINE::PRODUCT, &
					OE_CREDITJOUR::LOCATION, "RT", &
					-OE_CREDITLINE::CREDQTY)
			END IF

		END SELECT

	END SELECT

28000	GOTO 32767

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Warning:UNDMEMNUM
	!	^*Undefined Memo Number\*
	!	.b
	!	^*Explanation\*
	!	.b
	!	.lm +5
	!	Selected memo number doesn't exist in Credit Journal
	!	file.
	!	.b
	!	.lm -5
	!	^*User Action\*
	!	.b
	!	.lm +5
	!	Select memo number from the Credit Journal file. Pressing
	!	^*List Choices\* will display a list of all valid lines for
	!	the particular order.
	!	.lm -5
	!
	! Index:
	!	.x Undefined Memo Number
	!
	!--
	!+-+-+
	!++
	! Warning:UNDORDLIN
	!	^*Undefined Memo Number\*
	!	.b
	!	^*Explanation\*
	!	.b
	!	.lm +5
	!	Selected memo number doesn't exist in Credit Journal
	!	file.
	!	.b
	!	.lm -5
	!	^*User Action\*
	!	.b
	!	.lm +5
	!	Select memo number from the Credit Journal file. Pressing
	!	^*List Choices\* will display a list of all valid lines for
	!	the particular order.
	!	.lm -5
	!
	! Index:
	!	.x Undefined Memo Number
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD005
	!	.x Promo Amount Off
	!	^*(04) Promo Amount Off\*
	!	.b
	!	.lm +5
	!	The ^*Promo Amount Off\* field enters if there was a
	!	promotional sale on this particular product for which the credit
	!	is being issued.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD007
	!	.x Discount
	!	^*(07) Discount\*
	!	.b
	!	.lm +5
	!	The Discount field enters the discount
	!	percentage given on the original order.
	!	.b
	!	The field may contain a figure as large as 99.999.
	!	.lm +5
	!
	! Index:
	!
	!--
