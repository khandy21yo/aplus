1	%TITLE "Ticket Line Journal"
	%SBTTL "PS_MAIN_TICKETLINE_01"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PS_MAIN_TICKETLINE_01(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1996 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Ticket Line Journal\* is used to enter a line item
	!	for each of the products to be invoiced.
	!	.lm -5
	!
	! Index:
	!	.x Help>Ticket Line Journal
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PS_SOURCE:PS_MAIN_TICKETLINE_01/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PS_MAIN_TICKETLINE_01
	!	$ DELETE PS_MAIN_TICKETLINE_01.OBJ;*
	!
	! Author:
	!
	!	08/15/96 - Kevin Handy
	!		Copied from PS_MAIN_TICKETLINE, and lost the
	!		serial number field (for Robinson's).
	!
	! Modification history:
	!
	!	02/11/97 - Kevin Handy
	!		Fix FRM$() so they aren't one off.
	!
	!	08/28/97 - Kevin Handy
	!		Lose undefined external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	06/09/99 - Kevin Handy
	!		Lose LoadSerialnumber (Dead Code)
	!
	!	11/28/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/19/2006 - Kevin Handy
	!		Base cost on ship date instead of order date.
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

	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE
	MAP (OE_ORDERLINE_OLD)	OE_ORDERLINE_CDD	OE_ORDERLINE_OLD, &
							OE_ORDERLINE_DEF

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	DECLARE			OE_PROMO_CDD		OE_PROMO_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_CUSTDISC.HB"
	DECLARE			OE_CUSTDISC_CDD		OE_CUSTDISC_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	MAP (PD_PRODUCT_ONE)	PD_PRODUCT_CDD		PD_PRODUCT_ONE
	DECLARE		PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP (PC_PRICE)		PC_PRICE_CDD		PC_PRICE

	%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.HB"
	MAP (PS_CONTROL)	PS_CONTROL_CDD		PS_CONTROL

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.HB"
	MAP (PC_PRCTYPE)	PC_PRCTYPE_CDD		PC_PRCTYPE

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE

	!
	! Common Statements
	!
	COM (CH_OE_ORDERJOUR) &
		OE_ORDERJOUR.CH%

	COM (BATCH_NO) &
		BATCH_NO$ = 7%

	COM (CH_PS_TICKETCONTROL) &
		PS_CONTROL.CH%, &
		MISCTYPE$ = 2%, &
		MISCDESCRIPTION$ = 15%, &
		MISC2TYPE$ = 2%, &
		MISC2DESCRIPTION$ = 15%

	COM (CH_OE_ORDERLINE) &
		OE_ORDERLINE.CH%, &
		OE_ORDERLINE.READONLY%

	COM (DSPL_ORDER) &
		AVAILABLE, &
		LONG TEMP.DISPLAY

	COM (CH_PC_PRCTYPE) &
		PC_PRCTYPE.CH%

	COM (CH_SB_BALANCE) &
		SB_BALANCE.CH%, &
		SB_BALANCE.READONLY%

	!
	! External functions
	!
	EXTERNAL REAL   FUNCTION PC_READ_COST
	EXTERNAL REAL   FUNCTION PC_READ_PRICE
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION OE_READ_PROMO
	EXTERNAL LONG   FUNCTION OE_READ_CUSTDISC
	EXTERNAL LONG   FUNCTION OE_READ_REGLINE
	EXTERNAL LONG   FUNCTION PC_DSPL_PRICE
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE
	EXTERNAL LONG   FUNCTION IC_DSPL_35BALANCE
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR  = "Line Items"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP  = "PS_MAIN_TICKETLINE_01"
		SMG_WINDOW::CHAN   = OE_ORDERLINE.CH%
		SMG_WINDOW::HSIZE  = 77%
		SMG_WINDOW::VSIZE  = 14%
		SMG_WINDOW::HPOS   = 3%
		SMG_WINDOW::VPOS   = 6%
		SMG_WINDOW::NITEMS = 14%
		SMG_WINDOW::FLAGS  = 0%
		SMG_WINDOW::HVIEW  = 77%
		SMG_WINDOW::VVIEW  = 13%
		SMG_WINDOW::VHPOS  = 3%
		SMG_WINDOW::VVPOS  = 7%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%)	= "Line"
		SMG_WINDOW::KFIELD(0%, 0%)	= 1%
		SMG_WINDOW::KFIELD(0%, 1%)	= 0%
		SMG_WINDOW::KFIELD(0%, 1%)	= 1% &
			IF OE_ORDERJOUR::REG_FLAG = "Y"

		COM (PS_MAIN_TICKETLINE_01_FRM) FRM$(14%)

		MISCDESCRIPTION$ = "Misc Charges"
		MISC2DESCRIPTION$ = "Misc Charges"

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

		!
		! Declare channels
		!
700		IF OE_ORDERLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_ORDERLINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PS_MAIN_TICKETLINE_01 = ERR
			CONTINUE 770
		END WHEN

		OE_ORDERLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.OPN"
		USE
			PS_MAIN_TICKETLINE_01 = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_ORDERLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_ORDERLINE.CH%)

		EXIT FUNCTION

790		IF PS_CONTROL.CH% <= 0%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.MOD"
				GET #PS_CONTROL.CH%, RECORD 1%, REGARDLESS
			USE
				CONTINUE 32767
			END WHEN
		END IF

		MISCTYPE$ = PS_CONTROL::MISCTYPE
		MISC2TYPE$ = PS_CONTROL::MISC2TYPE

		IF MVALUE = "A" AND OE_ORDERJOUR::REG_FLAG = "Y"
		THEN
			GOSUB LoadLines

			IF X% = 1% OR X% = 2% OR X% = 3%
			THEN
				PS_MAIN_TICKETLINE_01 = 1%
				GOTO 28000
			END IF
		END IF

		SMG_WINDOW::CHAN  = OE_ORDERLINE.CH%
		RESET #OE_ORDERLINE.CH%
		GET #OE_ORDERLINE.CH%, REGARDLESS

795		!
		! Look up miscelanous price type title
		!
		IF (PC_PRCTYPE.CH% = 0%)
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.OPN"
			USE
				CONTINUE 796
			END WHEN

		END IF

		WHEN ERROR IN
			GET #PC_PRCTYPE.CH%, &
				KEY #0% EQ PS_CONTROL::MISCTYPE + "", &
				REGARDLESS
		USE
			CONTINUE 796
		END WHEN

		MISCDESCRIPTION$ = PC_PRCTYPE::DESCRIPTION

796		WHEN ERROR IN
			GET #PC_PRCTYPE.CH%, &
				KEY #0% EQ PS_CONTROL::MISC2TYPE + "", &
				REGARDLESS
		USE
			CONTINUE 798
		END WHEN

		MISC2DESCRIPTION$ = PC_PRCTYPE::DESCRIPTION

798		!

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

		DATA	01,02, "(01) Line #", &
			02,02, "(02) Product", &
			03,02, "(03) Qty Requested", &
			04,02, "(04) Qty Invoiced", &
			05,02, "(05) Qty Back", &
			06,02, "(06) Unit Price", &
			07,02, "(07) Promo Amt Off", &
			08,02, "(08)", &
			09,02, "(09)", &
			10,02, "(10) Discount %", &
			11,02, "(11) Unit Cost", &
			12,02, "(12) Request Date", &
			13,02, "(13) Line Notes", &
			10,40, "Ext. Price", &
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

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			MISCDESCRIPTION$, 8%, 7%) &
			IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			MISC2DESCRIPTION$, 9%, 7%) &
			IF (SMG_WINDOW::HFLAG(9%) AND 2%) = 0%

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
	!	.x Line
	!	^*(01) Line\*
	!	.b
	!	.lm +5
	!	The ^*Line\* field is used to enter the line number
	!	of the product that was ordered.
	!	.b
	!	Four (04) spaces are available for the entry.
	!	.b
	!	Valid line numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> "Y"

			OE_ORDERLINE::LIN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;25", TEMP$, OE_ORDERLINE::LIN, MFLAG, &
				"~L0'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(OE_MAIN_REGLINE.ID, &
					"V0" + OE_ORDERLINE::ORDNUM) = 1%
				THEN
					OE_ORDERLINE::LIN = OE_REGLINE::LIN
				END IF
				GOTO Reentry

			END SELECT

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Product\*
	!	.b
	!	.lm +5
	!	The ^*Product\* field is used to enter the number of the
	!	product to be ordered.
	!	.b
	!	Valid products may be viewed by pressing ^*List Choices\*.  Additional
	!	products may be added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!	.x Product
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND OE_ORDERLINE::LIN <> "NEWL"

			OE_ORDERLINE::PRODUCT = ENTR_3PRODUCT(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;25", TEMP$, OE_ORDERLINE::PRODUCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					OE_ORDERLINE::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reentry
			END IF

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F17)
			THEN
				V% = MAIN_WINDOW(IC_WRIT_PRODUCT.ID, &
					"M0" + OE_ORDERLINE::PRODUCT)

				OE_ORDERLINE::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				GOTO Reentry
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Quantity Requested\*
	!	.b
	!	.lm +5
	!	The ^*Quantity Requested\* field is to be entered with the number of
	!	units which have been ordered.
	!	.b
	!	The field may contain a figure as large as 99,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Quantity Requested
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND OE_ORDERLINE::LIN <> "NEWL"

			OE_ORDERLINE::ORDQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;25", TEMP$, OE_ORDERLINE::ORDQTY, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				V% = IC_DSPL_35BALANCE(OE_ORDERLINE::PRODUCT, &
					OE_ORDERJOUR::LOCATION, &
					AVAILABLE, "14;10", 0%)

				GOTO Reentry
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Quantity Invoiced\*
	!	.b
	!	.lm +5
	!	The ^*Quantity Invoiced\* field is to be entered with the number of
	!	units which have been invoiced.
	!	.b
	!	The field may contain a figure as large as 99,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Quantity Invoiced
	!
	!--
			OE_ORDERLINE::SHPQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;25", TEMP$, OE_ORDERLINE::SHPQTY, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Quantity on Backorder\*
	!	.b
	!	.lm +5
	!	The ^*Quantity on Backorder\* field is to be entered with the number of
	!	units which have been back ordered for this
	!	particular product.
	!	.b
	!	The field may contain a figure as large as 99,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Quantity on Backorder
	!
	!--
			OE_ORDERLINE::BCKQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;25", TEMP$, OE_ORDERLINE::BCKQTY, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Unit Price\*
	!	.b
	!	.lm +5
	!	If a product is available in a particular unit of
	!	measure, i.e. "gallon", "piece", "yard", etc., that price is
	!	entered in this field.
	!	.lm -5
	!
	! Index:
	!	.x Unit Price
	!
	!--
			OE_ORDERLINE::PRICE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;25", TEMP$, OE_ORDERLINE::PRICE, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF PC_DSPL_PRICE (OE_ORDERLINE::PRODUCT, &
					OE_ORDERJOUR::LOCATION, &
					OE_ORDERJOUR::ORDDATE, "", &
					PRICE, "3;45", 0%) = CMC$_NORMAL
				THEN
					OE_ORDERLINE::PRICE = PRICE
				END IF
				GOTO Reentry
			END IF

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F17)
			THEN
				PD_PRODUCT_ONE = PD_PRODUCT

				V% = MAIN_WINDOW(PC_MAIN_PRICE.ID, &
					"M0" + OE_ORDERLINE::PRODUCT)

				OE_ORDERLINE::PRICE = &
						PC_PRICE::PRICECOST
				GOTO Reentry
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	.x Promo Amount
	!	^*(07) Promo Amount off\*
	!	.b
	!	.lm +5
	!	If a product is offered as a promotional item,
	!	the amount of the discount for the promotional pricing is
	!	entered in this field.
	!	.b
	!	The format for entry is %%.%%, i.e., if the percent of discount is
	!	to be 10%, the entry would be made as 10.00.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_ORDERLINE::PROMO = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;25", TEMP$, OE_ORDERLINE::PROMO, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Miscellaneous Charges\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Charges\* is to be entered with any
	!	additional charges per unit.
	!	.lm -5
	!
	! Index:
	!	.x Miscellaneous Charges
	!
	!--
			OE_ORDERLINE::MISCH = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;25", TEMP$, OE_ORDERLINE::MISCH, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Miscellaneous Charges\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Charges\* is to be entered with any
	!	additional charges per unit.
	!	.lm -5
	!
	! Index:
	!	.x Miscellaneous Charges
	!
	!--
			OE_ORDERLINE::MISCH2 = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;25", TEMP$, OE_ORDERLINE::MISCH2, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	.x Discount
	!	^*(10) Discount\*
	!	.b
	!	.lm +5
	!	The ^*Discount\* field is to be entered if there is a discount on
	!	this particular product.
	!	.b
	!	As an example, if the discount is to be 10%, the entry would be
	!	made as 10.00.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_ORDERLINE::DISCOUNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;32", TEMP$, OE_ORDERLINE::DISCOUNT, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Cost/Unit\*
	!	.b
	!	.lm +5
	!	The ^*Cost/Unit\* field is provided to enter the cost of
	!	a particular unit.
	!	.b
	!	If the product is available in a particular unit
	!	of measure, i.e. "gallon", "piece", "yard", that cost
	!	is entered in this field.
	!	.lm -5
	!
	! Index:
	!	.x Cost/Unit
	!
	!--
			OE_ORDERLINE::COST = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;25", TEMP$, OE_ORDERLINE::COST, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)


		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Request Date\*
	!	.b
	!	.lm +5
	!	The ^*Request Date\* field is to indicate the date
	!	the product is requested.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Request Date
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND OE_ORDERLINE::LIN <> "NEWL"

			OE_ORDERLINE::REQDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;25", TEMP$, OE_ORDERLINE::REQDATE, &
				MFLAG, "'E", MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Line Notes\*
	!	.b
	!	.lm +5
	!	The ^*Line Notes\* field is provided to enter up to forty (40)
	!	characters of line notes, which can be printed on the ticket form.
	!	.lm -5
	!
	! Index:
	!	.x Line Notes
	!
	!--

 FirstNote:
			OE_ORDERLINE::NOTES1 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"13;25", TEMP$, OE_ORDERLINE::NOTES1, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO BypassNotes

			END SELECT

 SecondNote:
			OE_ORDERLINE::NOTES2 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"14;25", TEMP$, OE_ORDERLINE::NOTES2, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO FirstNote

			END SELECT

 BypassNotes:

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		PS_MAIN_TICKETLINE_01 = 0%

		SELECT MLOOP

		CASE 1%
			!
			! See if the line is in the REGLINE file
			!
			IF OE_ORDERJOUR::REG_FLAG = "Y" AND &
				OE_ORDERLINE::LIN <> "NEWL"
			THEN
				IF OE_READ_REGLINE(OE_ORDERLINE::ORDNUM, &
					OE_ORDERLINE::LIN, "EQ", &
					OE_REGLINE_READ, QTY()) <> CMC$_NORMAL
				THEN
					PS_MAIN_TICKETLINE_01 = 1%
					GOTO ExitFunction
				END IF

				OE_ORDERLINE::PRODUCT   = OE_REGLINE_READ::PRODUCT
				OE_ORDERLINE::ORDQTY    = QTY(0%)
				OE_ORDERLINE::SHPQTY    = QTY(0%)
				OE_ORDERLINE::BCKQTY    = 0.0
				OE_ORDERLINE::PRICE     = OE_REGLINE_READ::PRICE
				OE_ORDERLINE::DISCOUNT  = OE_REGLINE_READ::DISCOUNT
				OE_ORDERLINE::COST      = OE_REGLINE_READ::COST
				OE_ORDERLINE::REQDATE   = OE_REGLINE_READ::TDATE
				OE_ORDERLINE::PROMO     = OE_REGLINE_READ::PROMO
				OE_ORDERLINE::MISCH     = OE_REGLINE_READ::MISCH
				OE_ORDERLINE::MISCH2    = OE_REGLINE_READ::MISCH2
				OE_ORDERLINE::NOTES1 = OE_REGLINE_READ::NOTES1
				OE_ORDERLINE::NOTES2 = OE_REGLINE_READ::NOTES2
				OE_ORDERLINE::SUBACCT	= OE_REGLINE_READ::SUBACCT

				JUNK% = PS_MAIN_TICKETLINE_01(SMG_WINDOW, &
					OPT_ENTRY, LOOP%, MFLAG OR 1%, MVALUE) &
					IF (SMG_WINDOW::HFLAG(LOOP%) AND 2%) = 0% &
					FOR LOOP% = 1% TO SMG_WINDOW::NITEMS

			END IF

		CASE 2%
			IF OE_ORDERLINE::PRODUCT = ""
			THEN
				PS_MAIN_TICKETLINE_01 = 1%
				GOTO ExitFunction
			END IF

			IF MVALUE = "ADD"
			THEN
				IF TEMP.DISPLAY
				THEN
					SMG_STATUS% = &
						SMG$POP_VIRTUAL_DISPLAY( &
						TEMP.DISPLAY, SCOPE::SMG_PBID)
					TEMP.DISPLAY = 0%
				END IF

				SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(1%, &
					13%, TEMP.DISPLAY)
			END IF


			!Test OE_ORDERLINE product

			IF PD_EXAM_PRODUCT(OE_ORDERLINE::PRODUCT, &
				PD_PRODUCT_EXAM) = CMC$_UNDEFINED
			THEN
				EXIT_STATUS% = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ORDERLINE::PRODUCT, &
					PD_PRODUCT::DESCRIPTION, &
					"OE", MLOOP, "PROD", &
					"Product", PD_MAIN_PRODUCT.ID)

				IF EXIT_STATUS% = 1%
				THEN
					PS_MAIN_TICKETLINE_01 = EXIT_STATUS%
				ELSE
					PS_MAIN_TICKETLINE_01 = &
						MAIN_WINDOW(IC_WRIT_PRODUCT.ID, "A")
				END IF
			END IF


			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT_EXAM::DESCRIPTION, 2%, 40%, , &
				SMG$M_BOLD)

			IF MVALUE = "ADD"
			THEN
				SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY( &
					TEMP.DISPLAY, SCOPE::SMG_PBID, &
					10%, 18%)

				IF PS_CONTROL::DSPLQTY = "Y"
				THEN
					V% = IC_DSPL_35BALANCE( &
						OE_ORDERLINE::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						AVAILABLE, "2;2", 64% + 512%)
				END IF

				IF PS_CONTROL::DSPLPRICE = "Y"
				THEN
					V% = PC_DSPL_PRICE( &
						OE_ORDERLINE::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						OE_ORDERJOUR::ORDDATE, "", &
						PRICE, "7;45", 64% + 512%)
				END IF

				IF AR_35CUSTOM::BACKORDER <> "Y"
				THEN
					SMG_STATUS% = SMG$PUT_CHARS( &
						TEMP.DISPLAY, &
						"NO BACKORDERS", &
						3%, 1%, , SMG$M_REVERSE)
				END IF
			END IF

			IF OE_ORDERJOUR::REG_FLAG <> "Y" OR &
				OE_ORDERLINE::LIN = "NEWL"
			THEN
				IF OE_READ_CUSTDISC(OE_ORDERLINE::PRODUCT, &
					OE_ORDERJOUR::CUSNUM, &
					AR_35CUSTOM::TTYPE, &
					OE_CUSTDISC_READ) = CMC$_NORMAL
				THEN
					OE_ORDERLINE::PRICE = PC_READ_PRICE( &
						OE_ORDERLINE::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						OE_CUSTDISC_READ::PRICETYPE, &
						OE_ORDERJOUR::ORDDATE, "", "", "")
					OE_ORDERLINE::DISCOUNT = &
						OE_CUSTDISC_READ::DISCOUNT

					JUNK% = PS_MAIN_TICKETLINE_01(SMG_WINDOW, &
						OPT_ENTRY, LOOP%, 1%, MVALUE) &
						IF (SMG_WINDOW::HFLAG(LOOP%) AND 2%) = 0% &
						FOR LOOP% = 1% TO SMG_WINDOW::NITEMS

				ELSE
					OE_ORDERLINE::PRICE = PC_READ_PRICE( &
						OE_ORDERLINE::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						AR_35CUSTOM::TTYPE, &
						OE_ORDERJOUR::ORDDATE, "", "", "")
					OE_ORDERLINE::DISCOUNT = &
						OE_ORDERLINE_DEF::DISCOUNT
				END IF

				IF (AR_35CUSTOM::TAXFLAG = " ") OR &
					(INSTR(1%, PS_CONTROL::MISCEXEMPT, &
					AR_35CUSTOM::TAXFLAG) = 0%)
				THEN
					OE_ORDERLINE::MISCH = PC_READ_PRICE( &
						OE_ORDERLINE::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						MISCTYPE$, &
						OE_ORDERJOUR::ORDDATE, &
						"", "", "")
				ELSE
					OE_ORDERLINE::MISCH = 0.0
				END IF

				IF (AR_35CUSTOM::TAXFLAG = " ") OR &
					(INSTR(1%, PS_CONTROL::MISC2EXEMPT, &
					AR_35CUSTOM::TAXFLAG) = 0%)
				THEN
					OE_ORDERLINE::MISCH2 = PC_READ_PRICE( &
						OE_ORDERLINE::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						MISC2TYPE$, &
						OE_ORDERJOUR::ORDDATE, &
						"", "", "")
				ELSE
					OE_ORDERLINE::MISCH2 = 0.0
				END IF
			END IF

			OE_ORDERLINE::COST = PC_READ_COST( &
				OE_ORDERLINE::PRODUCT, OE_ORDERJOUR::LOCATION, &
				OE_ORDERJOUR::SHIPDATE, "")

			V% = OE_READ_PROMO(OE_ORDERLINE::PRODUCT, &
				OE_ORDERJOUR::ORDDATE, &
				OE_ORDERJOUR::CUSNUM, OE_PROMO_READ, &
				OE_ORDERLINE::PRICE, OE_ORDERLINE::PROMO)

			IF OE_PROMO_READ::REFPROMO <> ""
			THEN
				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					"Promo#: " + OE_PROMO_READ::REFPROMO, &
					6%, 50%, , SMG$M_BOLD)
			END IF

			! Unit price
			! Unit Cost
			JUNK% = PS_MAIN_TICKETLINE_01(SMG_WINDOW, &
				OPT_ENTRY, LOOP%, 1%, MVALUE) &
				IF (SMG_WINDOW::HFLAG(LOOP%) AND 2%) = 0% &
				FOR LOOP% = 1% TO SMG_WINDOW::NITEMS

		CASE 3%
			IF MVALUE = "ADD"
			THEN

				IF SMG_WINDOW::HFLAG(4%) = 0%
				THEN
					IF AVAILABLE >= OE_ORDERLINE::ORDQTY
					THEN
						OE_ORDERLINE::SHPQTY = &
							OE_ORDERLINE::ORDQTY
					ELSE
						OE_ORDERLINE::SHPQTY = AVAILABLE
					END IF
				END IF

				IF SMG_WINDOW::HFLAG(5%) = 0%
				THEN
					OE_ORDERLINE::BCKQTY = &
						OE_ORDERLINE::ORDQTY - &
						OE_ORDERLINE::SHPQTY

					OE_ORDERLINE::BCKQTY = 0.0 &
						IF OE_ORDERLINE::BCKQTY < 0.0 &
						OR OE_ORDERLINE::ORDQTY <= 0.0
				END IF

				! Qty Inv
				! Qty Back
				JUNK% = PS_MAIN_TICKETLINE_01(SMG_WINDOW, &
					OPT_ENTRY, LOOP%, 1%, MVALUE) &
					IF (SMG_WINDOW::HFLAG(LOOP%) AND 2%) = 0% &
					FOR LOOP% = 1% TO SMG_WINDOW::NITEMS

			END IF

		CASE 4%
			!
			! Calculate the backorder
			!
			IF MVALUE = "ADD"
			THEN
				OE_ORDERLINE::BCKQTY = &
					OE_ORDERLINE::ORDQTY - &
					OE_ORDERLINE::SHPQTY

				OE_ORDERLINE::BCKQTY = 0.0 &
					IF OE_ORDERLINE::BCKQTY < 0.0 &
					OR OE_ORDERLINE::ORDQTY <= 0.0

				! Qty Back
				JUNK% = PS_MAIN_TICKETLINE_01(SMG_WINDOW, &
					OPT_ENTRY, LOOP%, 1%, MVALUE) &
					IF (SMG_WINDOW::HFLAG(LOOP%) AND 2%) = 0% &
					FOR LOOP% = 1% TO SMG_WINDOW::NITEMS

			END IF

		CASE 5%
			IF MVALUE = "ADD"
			THEN
				CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
					OE_ORDERLINE::LIN, &
					OE_ORDERLINE::ORDQTY, &
					OE_ORDERLINE::SHPQTY, &
					OE_ORDERLINE::BCKQTY, &
					TRANTYPE$(), TRANQTY())

					FOR I% = 1% TO 3%
						SMG_STATUS% = SMG$PUT_CHARS( &
							SMG_WINDOW::WNUMBER, &
							RIGHT(TRANTYPE$(I%), 3%), &
							I% + 2%, 40%, ,SMG$M_BOLD)
					NEXT I%

				IF TRANQTY(0%) = 0.0
				THEN
					PS_MAIN_TICKETLINE_01 = 1%
					GOTO ExitFunction
				END IF

			END IF

		CASE 6%
			IF MVALUE = "ADD"
			THEN
				SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY( &
					TEMP.DISPLAY, SCOPE::SMG_PBID)
				TEMP.DISPLAY = 0%
			END IF

		CASE 10%
			EXTP = FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
				((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
				(1.0 - (OE_ORDERLINE::DISCOUNT / 100)) + &
				OE_ORDERLINE::MISCH + OE_ORDERLINE::MISCH2), 2%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(EXTP, "#,###,###.##"), 10%, 52%, , &
				SMG$M_BOLD)

		CASE 11%
			IF OE_ORDERLINE::COST > OE_ORDERLINE::PRICE
			THEN
				CALL HELP_34MESSAGE(SCOPE, &
					"cost is higher then selling price", &
					"W", "PS_MAIN_TICKETLINE_01", "", "COSTGT")
			END IF

		END SELECT

	CASE OPT_DISPLAY

		!
		! Display the descriptions
		!
		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			V% = PD_EXAM_PRODUCT(OE_ORDERLINE::PRODUCT, &
				PD_PRODUCT_EXAM)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT_EXAM::DESCRIPTION, 2%, 40%, , &
				SMG$M_BOLD)
		END IF

		CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM,OE_ORDERLINE::LIN, &
			OE_ORDERLINE::ORDQTY, &
			OE_ORDERLINE::SHPQTY, OE_ORDERLINE::BCKQTY, &
			TRANTYPE$(), TRANQTY())

		FOR I% = 1% TO 3%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				RIGHT(TRANTYPE$(I%), 3%), I% + 2%, 40%, , &
				SMG$M_BOLD)
		NEXT I%

		EXTP = FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
			((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
			(1-(OE_ORDERLINE::DISCOUNT/100)) + &
			OE_ORDERLINE::MISCH + OE_ORDERLINE::MISCH2), 2%)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(EXTP, "#,###,###.##"), 10%, 52%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			SPACE$(40%), 7%, 50%, , SMG$M_BOLD)

	!
	! Set OE_ORDERLINE_OLD value
	!
20500	CASE OPT_SETOLD
		OE_ORDERLINE_OLD = OE_ORDERLINE

	!
	! Restore OE_ORDERLINE_OLD value
	!
	CASE OPT_RESETOLD
		OE_ORDERLINE = OE_ORDERLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_ORDERLINE_DEF = OE_ORDERLINE
		OE_ORDERLINE_DEF::SUBACCT = ""

		IF MFLAG = 1%
		THEN
			SELECT MLOOP

			CASE 0%
				FRM$(3%)  = "#,###,###.##"
				FRM$(4%)  = "#,###,###.##"
				FRM$(5%)  = "#,###,###.##"
				FRM$(6%)  = "#,###,###.##"
				FRM$(7%)  = "#,###,###.##"
				FRM$(8%)  = "#,###,###.##"
				FRM$(9%)  = "#,###,###.##"
				FRM$(10%)  = "##.##%"
				FRM$(11%) = "#,###,###.##"
			CASE ELSE
				FRM$(MLOOP) = MVALUE

			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_ORDERLINE = OE_ORDERLINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		OE_ORDERLINE::ORDNUM = MVALUE

		IF MFLAG = 1%
		THEN
			OE_ORDERLINE::LIN = "NEWL"

			OE_ORDERLINE::REQDATE = OE_ORDERJOUR::SHIPDATE &
				IF OE_ORDERLINE::REQDATE=""
		END IF

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			IF OE_ORDERJOUR::REG_FLAG = "Y"
			THEN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE OE_ORDERLINE::ORDNUM + &
					OE_ORDERLINE::LIN, REGARDLESS
			ELSE
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE OE_ORDERLINE::ORDNUM + "", &
					REGARDLESS
			END IF

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE OE_ORDERLINE::PRODUCT + &
				OE_ORDERLINE::ORDNUM, REGARDLESS

		END SELECT

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Product        Description     ReqQty " + &
				"QtySld     Price Disc%   ExtPrice"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "017,033,040,047,057,063"

		!
		! Convert current record into text
		!
		CASE 3%
			V% = PD_EXAM_PRODUCT(OE_ORDERLINE::PRODUCT, &
				PD_PRODUCT_EXAM)

			EXTP = FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
				((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
				(1 - (OE_ORDERLINE::DISCOUNT / 100)) + &
				OE_ORDERLINE::MISCH + OE_ORDERLINE::MISCH2), 2%)

			MVALUE = OE_ORDERLINE::PRODUCT + " " + &
				LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 15%) + " " + &
				FORMAT$(OE_ORDERLINE::ORDQTY, "######") + " " + &
				FORMAT$(OE_ORDERLINE::SHPQTY, "######") + " " + &
				FORMAT$(OE_ORDERLINE::PRICE, "######.##") + " " + &
				FORMAT$(OE_ORDERLINE::DISCOUNT, "##.##") + " " + &
				FORMAT$(EXTP, "#######.##")

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
				CONTINUE 28000
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
						KEY #0% GE MVALUE + &
						OE_ORDERLINE::LIN, REGARDLESS
				USE
					CONTINUE 28000
				END WHEN

			CASE 1%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #1% GE OE_ORDERLINE::PRODUCT + &
						MVALUE, REGARDLESS
				USE
					CONTINUE 28000
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
			IF OE_ORDERLINE::ORDNUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			OE_ORDERLINE::ORDNUM = MVALUE

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"

			CALL SUBR_TRANTYPE( &
				OE_ORDERJOUR::CUSNUM, &
				OE_ORDERLINE::LIN, &
				OE_ORDERLINE::ORDQTY, &
				OE_ORDERLINE::SHPQTY, &
				OE_ORDERLINE::BCKQTY, &
				TRANTYPE$(), TRANQTY())

			FOR I% = 1% TO VAL%(TRANTYPE$(0%))

				V% = IC_WRIT_35BALANCE( &
					OE_ORDERLINE::PRODUCT, &
					OE_ORDERJOUR::LOCATION, &
					LEFT(TRANTYPE$(I%), 2%), &
					TRANQTY(I%))

				IF LEFT(TRANTYPE$(I%), 2%) = "TR"
				THEN
					V% = IC_WRIT_35BALANCE( &
						OE_ORDERLINE::PRODUCT, &
						OE_ORDERJOUR::SHIPLIN, &
						LEFT(TRANTYPE$(I%), 2%), &
						-TRANQTY(I%))
				END IF
			NEXT I%


		CASE "Change", "Blank", "Initialize"

			CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
				OE_ORDERLINE_OLD::LIN, &
				OE_ORDERLINE_OLD::ORDQTY, &
				OE_ORDERLINE_OLD::SHPQTY, &
				OE_ORDERLINE_OLD::BCKQTY, &
				TRANTYPE$(), TRANQTY())

			FOR I% = 1% TO VAL%(TRANTYPE$(0%))
				V% = IC_WRIT_35BALANCE( &
					OE_ORDERLINE_OLD::PRODUCT, &
					OE_ORDERJOUR::LOCATION, &
					LEFT(TRANTYPE$(I%), 2%), &
					-TRANQTY(I%))

				IF LEFT(TRANTYPE$(I%), 2%) = "TR"
				THEN
					V% = IC_WRIT_35BALANCE( &
						OE_ORDERLINE_OLD::PRODUCT, &
						OE_ORDERJOUR::SHIPLIN, &
						LEFT(TRANTYPE$(I%), 2%), &
						TRANQTY(I%))
				END IF
			NEXT I%

			CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
				OE_ORDERLINE::LIN, &
				OE_ORDERLINE::ORDQTY, &
				OE_ORDERLINE::SHPQTY, &
				OE_ORDERLINE::BCKQTY, &
				TRANTYPE$(), TRANQTY())

			FOR I% = 1% TO VAL%(TRANTYPE$(0%))
				V% = IC_WRIT_35BALANCE( &
					OE_ORDERLINE::PRODUCT, &
					OE_ORDERJOUR::LOCATION, &
					LEFT(TRANTYPE$(I%), 2%), &
					TRANQTY(I%))
				IF LEFT(TRANTYPE$(I%), 2%) = "TR"
				THEN
					V% = IC_WRIT_35BALANCE( &
						OE_ORDERLINE::PRODUCT, &
						OE_ORDERJOUR::SHIPLIN, &
						LEFT(TRANTYPE$(I%), 2%), &
						-TRANQTY(I%))
				END IF
			NEXT I%

		CASE "Erase"
			IF MLOOP <> 1%
			THEN
				CALL SUBR_TRANTYPE( &
					OE_ORDERJOUR::CUSNUM, &
					OE_ORDERLINE::LIN, &
					OE_ORDERLINE::ORDQTY, &
					OE_ORDERLINE::SHPQTY, &
					OE_ORDERLINE::BCKQTY, &
					TRANTYPE$(), TRANQTY())

				FOR I% = 1% TO VAL%(TRANTYPE$(0%))
					V% = IC_WRIT_35BALANCE( &
						OE_ORDERLINE::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						LEFT(TRANTYPE$(I%), 2%), &
						-TRANQTY(I%))
					IF LEFT(TRANTYPE$(I%), 2%) = "TR"
					THEN
						V% = IC_WRIT_35BALANCE( &
							OE_ORDERLINE::PRODUCT, &
							OE_ORDERJOUR::SHIPLIN, &
							LEFT(TRANTYPE$(I%), 2%), &
							TRANQTY(I%))
					END IF
				NEXT I%

			END IF

		END SELECT

	END SELECT

 ExitFunction:

	SELECT SCOPE::SCOPE_EXIT

	!
	! Control C, Exit
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_F8, SMG$K_TRM_PF3

		IF TEMP.DISPLAY
		THEN
			!
			! Erase virtual display if still exists
			!
			SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY( &
				TEMP.DISPLAY, SCOPE::SMG_PBID)
			TEMP.DISPLAY = 0%
		END IF

	END SELECT

28000	EXIT FUNCTION

	%PAGE

	!=======================================================================
	! Subroutine to automatically load the invoice lines
	!=======================================================================
 LoadLines:
	SOPTION$(1%) = "Auto Invoice Lines"
	SOPTION$(2%) = "Auto Cancel Lines"
	SOPTION$(3%) = "Auto Lost Sale"
	SOPTION$(4%) = "Manually Enter Lines"
	SOPTION$(5%) = ""

 SelectSOption:
	X% = ENTR_3CHOICE(SCOPE, "", "", SOPTION$(), "", &
		0%, "Select Option", "", 0%)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitLoadLines

	END SELECT

	SELECT X%

	CASE 1%, 2%, 3%
		!
		! Auto Load lines
		!
		CALL ENTR_3MESSAGE(SCOPE, "Loading Lines", 1% + 16%)

	CASE 4%
		GOTO ExitLoadLines

	CASE ELSE
		GOTO SelectSOption

	END SELECT

	INVLIN$ = "    "

 ReqLine:
	!
	! Get quantity from REGLINE
	!
	IF OE_READ_REGLINE(OE_ORDERJOUR::ORDNUM, &
		INVLIN$, "GT", OE_REGLINE_READ, QTY()) <> CMC$_NORMAL
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "", 1%)
		GOTO ExitLoadLines
	END IF

	INVLIN$ = OE_REGLINE_READ::LIN

	GOTO ReqLine IF QTY(0%) = 0.0

	!
	! Build most of invoice line here from register line
	!
	OE_ORDERLINE::ORDNUM    = OE_ORDERJOUR::ORDNUM
	OE_ORDERLINE::LIN       = OE_REGLINE_READ::LIN
	OE_ORDERLINE::PRODUCT   = OE_REGLINE_READ::PRODUCT
	OE_ORDERLINE::ORDQTY    = QTY(0%)
	OE_ORDERLINE::PRICE     = OE_REGLINE_READ::PRICE
	OE_ORDERLINE::DISCOUNT  = OE_REGLINE_READ::DISCOUNT
	OE_ORDERLINE::PROMO     = OE_REGLINE_READ::PROMO
	OE_ORDERLINE::MISCH     = OE_REGLINE_READ::MISCH
	OE_ORDERLINE::MISCH2    = OE_REGLINE_READ::MISCH2
	OE_ORDERLINE::REQDATE   = OE_REGLINE_READ::TDATE
	OE_ORDERLINE::NOTES1 = OE_REGLINE_READ::NOTES1
	OE_ORDERLINE::NOTES2 = OE_REGLINE_READ::NOTES2
	OE_ORDERLINE::SUBACCT	= OE_REGLINE_READ::SUBACCT

	OE_ORDERLINE::COST = PC_READ_COST(OE_ORDERLINE::PRODUCT, &
		OE_ORDERJOUR::LOCATION, OE_ORDERJOUR::SHIPDATE, "")

	SELECT X%

	CASE 1%
		OE_ORDERLINE::SHPQTY = QTY(0%)
		OE_ORDERLINE::BCKQTY = 0.0

		V% = IC_WRIT_35BALANCE (OE_REGLINE_READ::PRODUCT, &
			OE_ORDERJOUR::LOCATION, "SO", QTY(0%))

		IF OE_ORDERJOUR::CUSNUM <> ""
		THEN
			V% = IC_WRIT_35BALANCE( &
				OE_REGLINE_READ::PRODUCT, &
				OE_ORDERJOUR::LOCATION, "SA", -QTY(0%))
		ELSE
			V% = IC_WRIT_35BALANCE( &
				OE_REGLINE_READ::PRODUCT, &
				OE_ORDERJOUR::LOCATION, "TR", -QTY(0%))

			V% = IC_WRIT_35BALANCE (OE_ORDERLINE::PRODUCT, &
				OE_ORDERJOUR::SHIPLIN, &
				"TR", QTY(0%))
		END IF


	CASE 2%
		OE_ORDERLINE::SHPQTY = 0.0
		OE_ORDERLINE::BCKQTY = -QTY(0%)

		V% = IC_WRIT_35BALANCE (OE_REGLINE_READ::PRODUCT, &
			OE_ORDERJOUR::LOCATION, "SO", QTY(0%))

	CASE 3%
		OE_ORDERLINE::SHPQTY = 0.0
		OE_ORDERLINE::BCKQTY = 0.0

		V% = IC_WRIT_35BALANCE (OE_REGLINE_READ::PRODUCT, &
			OE_ORDERJOUR::LOCATION, "LS", QTY(0%))

	END SELECT

28100	PUT #OE_ORDERLINE.CH%

	GOTO ReqLine

 ExitLoadLines:
	RETURN

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
