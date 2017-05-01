1	%TITLE "Register Line Journal"
	%SBTTL "OE_MAIN_REGLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_REGLINE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1990 BY
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
	!	The Register Line Journal enters the number of products
	!	to be Registered.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_REGLINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN OE_MAIN_REGLINE
	!	$ DELETE OE_MAIN_REGLINE.OBJ;*
	!
	! Author:
	!
	!	06/18/90 - Lance Williams
	!
	! Modification history:
	!
	!	07/01/91 - Frank F. Starman
	!		Change promo ref num to promo amount off.
	!
	!	08/27/91 - Dan Perkins
	!		Centered View screen display.
	!
	!	10/31/91 - Dan Perkins
	!		Fixed OPT_DISPLAY to display the product
	!		description in filed #3.
	!
	!	12/24/91 - Dan Perkins
	!		Added REFNUM field resulting form change in
	!		record layout.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	03/20/92 - Dan Perkins
	!		Removed COM statments that didn't have much use.
	!
	!	11/25/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.  Added BATCH which is a key to
	!		the view screen.
	!
	!	02/01/93 - Dan Perkins
	!		Added Notes to the file layout.
	!
	!	02/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/26/93 - Dan Perkins
	!		Added Miscellaneous Charges and Period fields.
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/20/94 - Kevin Handy
	!		Added code to handle the new ::MISCH2 field.
	!		Not as fancy as in the PS journal though.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/08/95 - Kevin Handy
	!		Reformatted source closer to 80 columne.
	!		Lose unecessary external definitions.
	!		Layout change for OE_REGLINE::NOTES.
	!		Add OE_REGLINE::SUBACCT field.
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE
	MAP (OE_REGLINE_OLD)	OE_REGLINE_CDD		OE_REGLINE_OLD, &
							OE_REGLINE_DEF

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Common Statements
	!
	COM (CH_OE_REGLINE) &
		OE_REGLINE.CH%, &
		OE_REGLINE.READONLY%

	COM (CH_AR_35CUSTOM) &
		AR_35CUSTOM.CH%

	COM (TT_OE_MAIN_REGLINE) &
		TRANTITLE$ = 20%, &
		TRANTYPE$(7%) = 40%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL REAL	FUNCTION PC_READ_PRICE

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR  = "Order-Invoice Register Lines"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP  = "OE_MAIN_REGLINE"
		SMG_WINDOW::HSIZE  = 74%
		SMG_WINDOW::VSIZE  = 13%
		SMG_WINDOW::HPOS   = 3%
		SMG_WINDOW::VPOS   = 6%
		SMG_WINDOW::NITEMS = 19%
		SMG_WINDOW::FLAGS  = 0%
		SMG_WINDOW::HVIEW = 98%
		SMG_WINDOW::VVIEW = 12%
		SMG_WINDOW::VHPOS = 19%
		SMG_WINDOW::VVPOS = 7%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Line_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Product"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 4%
		SMG_WINDOW::KNAME(2%) = "Batch"
			SMG_WINDOW::KFIELD(2%, 0%) = 1%
			SMG_WINDOW::KFIELD(2%, 1%) = 14%
		SMG_WINDOW::KNAME(3%) = "Reference_no"
			SMG_WINDOW::KFIELD(3%, 0%) = 2%
			SMG_WINDOW::KFIELD(3%, 1%) = 17%
			SMG_WINDOW::KFIELD(3%, 2%) = 2%

		!
		! Transaction type
		!
		TRANTITLE$ = "Type    Description"
		TRANTYPE$(0%) = "3"
		TRANTYPE$(1%) = "01    On Order"
		TRANTYPE$(2%) = "02    Shipping"
		TRANTYPE$(3%) = "03    Cancel"

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF OE_REGLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_REGLINE.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			OE_MAIN_REGLINE = ERR
			CONTINUE 770
		END WHEN

		OE_REGLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
		USE
			OE_MAIN_REGLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_REGLINE.READONLY% = -1%
		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_REGLINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = OE_REGLINE.CH%
		WHEN ERROR IN
			RESET #OE_REGLINE.CH%
			GET #OE_REGLINE.CH%, REGARDLESS
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

		DATA	1, 2, "(01) Line #", &
			2, 2, "(02) Trans. Type", &
			3, 2, "(03) Trans. Date", &
			4, 2, "(04) Product #", &
			5, 2, "(05) Quantity", &
			6, 2, "(06) Unit Price", &
			7, 2, "(07) Promo Amt Off", &
			8, 2, "(08) Misc Charges", &
			9, 2, "(09) Misc Charges", &
			10, 2, "(10) Discount %", &
			11, 2, "(11) Unit Cost", &
			05,40, "(12) Post Date", &
			06,40, "(13) Post Time", &
			07,40, "(14) Batch #", &
			08,40, "(15) Period", &
			09,40, "(16) Ship No", &
			10,40, "(17) Reference No", &
			12, 2, "(18) Notes", &
			11,40, "(19) Serial Number", &
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
	!	.x Line
	!	^*(01) Line\*
	!	.b
	!	.lm +5
	!	The ^*Line\* field enters the line number
	!	of the product that was ordered.
	!	.b
	!	Four spaces are available for the entry.
	!	.b
	!	Valid line numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::LIN = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"01;22", TEMP$, OE_REGLINE::LIN, MFLAG, &
				"~L0'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Transaction Type
	!	^*(02) Transaction Type\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Type\* field enters the code which will
	!	identify this product transaction.
	!	.b
	!	The field will accommodate two characters.
	!	.b
	!	Valid transaction type codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::TRANTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"02;22", TEMP$, OE_REGLINE::TRANTYPE, MFLAG, "'E", MVALUE, &
				TRANTYPE$(), TRANTITLE$, "008"), -1%)

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x Transaction Date
	!	^*(03) Transaction Date\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Date\* field enters the date of the
	!	transaction.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::TDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;22", TEMP$, OE_REGLINE::TDATE, MFLAG, &
				"'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Product Number
	!	^*(04) Product Number\*
	!	.b
	!	.lm +5
	!	The ^*Product Number\* field enters the number of
	!	the product to be shipped on the particular date for this
	!	particular product.
	!	.b
	!	The field will accept up to fifteen characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::PRODUCT= ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;22", TEMP$, OE_REGLINE::PRODUCT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					OE_REGLINE::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reentry
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Quantity Ordered
	!	^*(05) Quantity Ordered\*
	!	.b
	!	.lm +5
	!	The ^*Quantity Ordered\* field enters the
	!	number of items that are to either be ordered, shipped, or
	!	cancelled, depending on the transaction type.
	!	.b
	!	The field will accept up to twelve characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::QTY = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;22", TEMP$, OE_REGLINE::QTY, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x Unit Price
	!	^*(06) Unit Price\*
	!	.b
	!	.lm +5
	!	The ^*Unit Price\* field enters the price of
	!	a particular unit.
	!	.b
	!	The field will accept a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--
			IF (TEMP$ = "Add") AND (OE_REGLINE::PRICE = 0.0)
			THEN
				OE_REGLINE::PRICE = PC_READ_PRICE( &
				OE_REGLINE::PRODUCT, OE_ORDERJOUR::LOCATION, &
				AR_35CUSTOM::TTYPE, &
				OE_ORDERJOUR::ORDDATE, "", "", "")
			END IF

			OE_REGLINE::PRICE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;22", TEMP$, OE_REGLINE::PRICE, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	.x Promo Amount Off
	!	^*(07) Promo Amount Off\*
	!	.b
	!	.lm +5
	!	The ^*Promo Amount Off\* field indicates the dollar amount
	!	taken off the regular price for this promotion.
	!	.b
	!	The field will accept a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::PROMO = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;22", TEMP$, OE_REGLINE::PROMO, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Miscellaneous Charges\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Charges\* is any additional price increase
	!	per one unit.
	!	.lm -5
	!
	! Index:
	!	.x Miscellaneous Charges
	!
	!--
			OE_REGLINE::MISCH = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;22", TEMP$, OE_REGLINE::MISCH, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Miscellaneous Charges 2\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Charges\* is any additional price increase
	!	per one unit.
	!	.lm -5
	!
	! Index:
	!	.x Miscellaneous Charges
	!
	!--
			OE_REGLINE::MISCH2 = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"09;22", TEMP$, OE_REGLINE::MISCH2, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	.x Discount Percentage
	!	^*(09) Discount Percentage\*
	!	.b
	!	.lm +5
	!	The Discount Percentage field indicates the percentage
	!	of discount to be given.
	!	.b
	!	Example:  If the discount to be given is 10%, the entry would
	!	be made as 10.00.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::DISCOUNT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;22", TEMP$, OE_REGLINE::DISCOUNT, MFLAG, &
				"##.###", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	.x Unit Cost
	!	^*(11) Unit Cost\*
	!	.b
	!	.lm +5
	!	The Unit Cost field indicates the cost of a particular
	!	unit.
	!	.b
	!	The field will accept a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::COST = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;22", TEMP$, OE_REGLINE::COST, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	.x Post Date
	!	^*(12) Post Date\*
	!	.b
	!	.lm +5
	!	The Post Date field indicates the date
	!	this particular product was posted.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::POSTDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;59", TEMP$, OE_REGLINE::POSTDATE, MFLAG, &
				"'E", MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	.x Post Time
	!	^*(13) Post Time\*
	!	.b
	!	.lm +5
	!	The Post Time field indicates what time
	!	this particular product was posted.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::POSTTIME = ENTR_3TIME(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;59", TEMP$, OE_REGLINE::POSTTIME, MFLAG, &
				"'E", MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	.x Batch
	!	^*(14) Batch\*
	!	.b
	!	.lm +5
	!	The ^*Batch\* field contains the batch number assigned
	!	when the transaction
	!	was posted.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::BATCH = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;59", TEMP$, OE_REGLINE::BATCH, MFLAG, &
				"'E", MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	.x Period
	!	^*(15) Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field contains the period into which the batch
	!	was posted.  The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::PERIOD = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;59", TEMP$, OE_REGLINE::PERIOD, MFLAG, &
				"'E", MVALUE)

		CASE 16%
	!++
	! Abstract:FLD016
	!	.x Ship No
	!	^*(16) Ship No.\*
	!	.b
	!	.lm +5
	!	The Ship No. field enters a shipment number
	!	for this line.
	!	.b
	!	The field will accommodate two characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::SHIPNO = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"09;59", TEMP$, OE_REGLINE::SHIPNO, MFLAG, &
				"'E", MVALUE)

		CASE 17%
	!++
	! Abstract:FLD017
	!	.x Reference No
	!	^*(17) Reference No.\*
	!	.b
	!	.lm +5
	!	The Reference No. field enters a reference number
	!	for this line. An example of a reference number would be the
	!	invoice number from the shipping journal or a credit memo number
	!	from the credit journal.
	!	.b
	!	The field will accommodate eight characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::REFNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;59", TEMP$, OE_REGLINE::REFNUM, MFLAG, &
				"'E", MVALUE)

		CASE 18%
	!++
	! Abstract:FLD018
	!	.x Notes
	!	^*(18) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* field enters any free formatted notes
	!	relative to the record.
	!	.b
	!	Forty spaces are available for entry.
	!	.lm -5
	!
	! Index:
	!
	!--
 FirstNote:
			OE_REGLINE::NOTES1 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;22", TEMP$, OE_REGLINE::NOTES1, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO BypassNotes

			END SELECT

			GOTO BypassNotes IF OE_REGLINE::NOTES1 = "" &
				AND OE_REGLINE::NOTES2 = ""

 SecondNote:
			OE_REGLINE::NOTES2 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;22", TEMP$, OE_REGLINE::NOTES2, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO FirstNote

			END SELECT

 BypassNotes:

		CASE 19%
	!++
	! Abstract:FLD019
	!	.x Subaccount
	!	^*(18) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* field enters any free formatted notes
	!	relative to the record.
	!	.b
	!	Forty spaces are available for entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGLINE::SUBACCT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;59", TEMP$, OE_REGLINE::SUBACCT, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		OE_MAIN_REGLINE = 0%

		SELECT MLOOP

		CASE 4%
			!
			! Test OE_REGLINE product
			!
			OE_MAIN_REGLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_REGLINE::PRODUCT, &
				PD_PRODUCT::DESCRIPTION, &
				"OE", MLOOP, "PROG", &
				"Product", PD_MAIN_PRODUCT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 4%, 40%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			PD_PRODUCT::DESCRIPTION = &
				STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "Q0" + OE_REGLINE::PRODUCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 4%, 40%, , SMG$M_BOLD)
		END IF

	!
	! Set OE_REGLINE_OLD value
	!
20500	CASE OPT_SETOLD
		OE_REGLINE_OLD = OE_REGLINE

	!
	! Restore OE_REGLINE_OLD value
	!
	CASE OPT_RESETOLD
		OE_REGLINE = OE_REGLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_REGLINE_DEF = OE_REGLINE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_REGLINE = OE_REGLINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		OE_REGLINE::ORDNUM = MVALUE

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE OE_REGLINE::ORDNUM + &
				OE_REGLINE::LIN, REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE OE_REGLINE::PRODUCT + &
				OE_REGLINE::ORDNUM, REGARDLESS

		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE OE_REGLINE::BATCH + &
				OE_REGLINE::ORDNUM, REGARDLESS

		CASE 3%
			FIND #SMG_WINDOW::CHAN, &
				KEY #3% GE OE_REGLINE::REFNUM + &
				OE_REGLINE::TRANTYPE + &
				OE_REGLINE::ORDNUM, REGARDLESS

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
			MVALUE = "  Line Type ProductCode    Quantity "  + &
				"TransDate  UnitPrice  PromoOff  Disc% " + &
				" UnitCost ShipNo Batch"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,012,027,036,047,057,067,074,084,091"

		!
		! Convert current record into text
		!
		CASE 3%
			STUFFS$ = "ORD" IF OE_REGLINE::TRANTYPE = "01"
			STUFFS$ = "SHP" IF OE_REGLINE::TRANTYPE = "02"
			STUFFS$ = "CAN" IF OE_REGLINE::TRANTYPE = "03"

			MVALUE = OE_REGLINE::LIN + " " + &
				STUFFS$ + "  " + &
				OE_REGLINE::PRODUCT + " " + &
				FORMAT$(OE_REGLINE::QTY, "#####.##") + " " + &
				PRNT_DATE(OE_REGLINE::TDATE, 8%) + " " + &
				FORMAT$(OE_REGLINE::PRICE, "######.##") + " " + &
				FORMAT$(OE_REGLINE::PROMO, "######.##") + " " + &
				FORMAT$(OE_REGLINE::DISCOUNT, "##.###") + " " + &
				FORMAT$(OE_REGLINE::COST, "######.##") + " " + &
				OE_REGLINE::SHIPNO + "     " + &
				OE_REGLINE::BATCH

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
						OE_REGLINE::LIN, REGARDLESS
				USE
					CONTINUE 28000
				END WHEN

			CASE 1%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #1% GE OE_REGLINE::PRODUCT + &
						MVALUE, REGARDLESS
				USE
					CONTINUE 28000
				END WHEN

			CASE 2%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #2% GE OE_REGLINE::BATCH + &
						MVALUE, REGARDLESS
				USE
					CONTINUE 28000
				END WHEN

			CASE 3%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #3% GE OE_REGLINE::REFNUM + &
						OE_REGLINE::TRANTYPE + &
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
			IF OE_REGLINE::ORDNUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			OE_REGLINE::ORDNUM = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
