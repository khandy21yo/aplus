1	%TITLE "Customer Price Discount Table Maintenance"
	%SBTTL "OE_MAIN_CUSTDISC"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_CUSTDISC(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1990 BY
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
	!	.b
	!	.lm +5
	!	The ^*Customer Price Discount Table Maintenance\* option
	!	accesses the file where customer product
	!	discounts are stored and may be viewed, modified, or erased.
	!	.lm -5
	!
	! Index:
	!	.x Customer Price Discount Table Maintenance
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_CUSTDISC/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_MAIN_CUSTDISC
	!	$ DELETE OE_MAIN_CUSTDISC.OBJ;*
	!
	! Author:
	!
	!	04/16/93 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/21/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/09/93 - Kevin Handy
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
	!	12/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.HB"
	MAP (PD_PRODTYPE)	PD_PRODTYPE_CDD		PD_PRODTYPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_CATEGORY.HB"
	MAP (PD_CATEGORY)	PD_CATEGORY_CDD		PD_CATEGORY

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.HB"
	MAP (PC_PRCTYPE)	PC_PRCTYPE_CDD	PC_PRCTYPE

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_CUSTDISC.HB"
	MAP (OE_CUSTDISC)	OE_CUSTDISC_CDD	OE_CUSTDISC
	MAP (OE_CUSTDISC_OLD)	OE_CUSTDISC_CDD	OE_CUSTDISC_OLD
	MAP (OE_CUSTDISC_DEF)	OE_CUSTDISC_CDD	OE_CUSTDISC_DEF


	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_OE_CUSTDISC) &
		OE_CUSTDISC.CH%, &
		OE_CUSTDISC.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

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

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Product Price Discount Maintenance"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "OE_MAIN_CUSTDISC"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 7%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 12%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 7%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Product"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 7%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 12%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF OE_CUSTDISC.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_CUSTDISC.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CUSTDISC.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			OE_MAIN_CUSTDISC = ERR
			CONTINUE 770
		END WHEN

		OE_CUSTDISC.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CUSTDISC.OPN"
		USE
			OE_MAIN_CUSTDISC = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_CUSTDISC.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_CUSTDISC.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = OE_CUSTDISC.CH%
		WHEN ERROR IN
			RESET #OE_CUSTDISC.CH%
			GET #OE_CUSTDISC.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
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

		DATA	02,05, "(01) Product", &
			03,05, "(02) Prod Type", &
			04,05, "(03) Prod Catg", &
			05,05, "(04) Price Type", &
			06,05, "(05) Disc %", &
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
	!	.x Product Number>Price Discount Maintenance
	!	^*(01) Product Number\*
	!	.lm +5
	!	.b
	!	The ^*Product Number\* field enters a valid product
	!	code.
	!	.b
	!	Ten spaces are available for the entry.
	!	.b
	!	Valid Product Number codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Price Discount Maintenance>Product Number
	!	.x Product>Price Discount
	!
	!--


			OE_CUSTDISC::PRODUCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"02;28", TEMP$, &
				OE_CUSTDISC::PRODUCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "V0") = 1%
				THEN
					OE_CUSTDISC::PRODUCT = PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Product Type>Price Discount Maintenance
	!	^*(02) Product Type\*
	!	.b
	!	.lm +5
	!	The ^*Product Type\* field enters the Product
	!	type.
	!	.b
	!	Valid Product Type codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Price Discount Maintenance>Customer Type
	!
	!--


			OE_CUSTDISC::PRODTYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;28", TEMP$, OE_CUSTDISC::PRODTYPE, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "V0") = 1%
				THEN
					OE_CUSTDISC::PRODTYPE = PD_PRODTYPE::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Product Category>Price Discount Maintenance
	!	^*(03) Product Category\*
	!	.b
	!	.lm +5
	!	The ^*Product Category\* field enters the applicable
	!	Product Category code.
	!	.b
	!	Valid Product Category codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Price Discount Maintenance>Product Category
	!
	!--


			OE_CUSTDISC::PRODCAT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;28", TEMP$, OE_CUSTDISC::PRODCAT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_CATEGORY.ID, "V0") = 1%
				THEN
					OE_CUSTDISC::PRODCAT = PD_CATEGORY::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	.x Price Type>Price Discount Maintenance
	!	^*(04) Price Type\*
	!	.b
	!	.lm +5
	!	The ^*Price Type\* field enters the price type discount
	!	code as applicable.
	!	.b
	!	Price type codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Price Discount Maintenance>Price Type
	!
	!--


			OE_CUSTDISC::PRICETYPE = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"5;28",TEMP$, OE_CUSTDISC::PRICETYPE, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "V0") = 1%
				THEN
					OE_CUSTDISC::PRICETYPE = PC_PRCTYPE::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	.x Discount Percentage>Price Discount Maintenance
	!	^*(05) Discount Percentage\*
	!	.b
	!	.lm +5
	!	The ^*Discount Percentage\* field enters the discount percentage
	!	to be given for this particular unit.
	!	.b
	!	^*Note:\* If the discount is to be 5%, the entry would be made as 5.00.
	!	.lm -5
	!
	! Index:
	!	.x Price Discount Maintenance>Discount Percentage
	!
	!--


			OE_CUSTDISC::DISCOUNT = ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				"6;27",TEMP$, OE_CUSTDISC::DISCOUNT, MFLAG, &
				"###.##%", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

	CASE OPT_DISPLAY

	!
	! Set OE_CUSTDISC_OLD value
	!
20500	CASE OPT_SETOLD
		OE_CUSTDISC_OLD = OE_CUSTDISC

	!
	! Restore OE_CUSTDISC_OLD value
	!
	CASE OPT_RESETOLD
		OE_CUSTDISC = OE_CUSTDISC_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_CUSTDISC_DEF = OE_CUSTDISC

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_CUSTDISC = OE_CUSTDISC_DEF
		OE_CUSTDISC::CUSNUM = MVALUE

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  ProdNumber           ProdType             " + &
				"ProdCat              PT  Disc%"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "023,044,065,068"

		!
		! Convert current record into text
		!
		CASE 3%

			MVALUE = OE_CUSTDISC::PRODUCT + " " + &
				OE_CUSTDISC::PRODTYPE + " " + &
				OE_CUSTDISC::PRODCAT + " " + &
				OE_CUSTDISC::PRICETYPE + " " + &
				FORMAT$(OE_CUSTDISC::DISCOUNT, "###.##%")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE OE_CUSTDISC::PRODUCT + "", &
					REGARDLESS

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

			IF OE_CUSTDISC::CUSNUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			OE_CUSTDISC::CUSNUM = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
