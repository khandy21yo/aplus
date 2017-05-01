1	%TITLE "Add Product Description, Cost and Price"
	%SBTTL "IC_WRIT_PRODUCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_WRIT_PRODUCT(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	The temporary product numbers are assigned and identified, and new product information
	!	is entered as well as cost ans price.
	!
	! Index:
	!	.x Product Description>Table
	!	.x Tables>Product Description
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_WRIT_PRODUCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_WRIT_PRODUCT
	!	$ DELETE IC_WRIT_PRODUCT.OBJ;*
	!
	! Author:
	!
	!	04/04/92 - Frank Starman
	!
	! Modification history:
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	09/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Include PC_WINDOW.INC file.
	!
	!	06/19/95 - Kevin Handy
	!		Clean out extra externals.
	!
	!	01/26/96 - Kevin Handy
	!		Changed STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!		Reformatted source code.
	!
	!	07/22/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
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
	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	MAP (PD_PRODUCT_OLD)	PD_PRODUCT_CDD		PD_PRODUCT_OLD
	MAP (PD_PRODUCT_DEF)	PD_PRODUCT_CDD		PD_PRODUCT_DEF

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.HB"
	MAP (PD_PRODTYPE)	PD_PRODTYPE_CDD		PD_PRODTYPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_CATEGORY.HB"
	MAP (PD_CATEGORY)	PD_CATEGORY_CDD		PD_CATEGORY

	%INCLUDE "SOURCE:[PD.OPEN]PD_LABEL.HB"
	MAP (PD_LABEL)		PD_LABEL_CDD		PD_LABEL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_MEASURE.HB"
	MAP (UTL_MEASURE)	UTL_MEASURE_CDD		UTL_MEASURE

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP (PC_PRICE)		PC_PRICE_CDD		PC_PRICE
	MAP (PC_PRICE_OLD)	PC_PRICE_CDD		PC_PRICE_OLD
	MAP (PC_PRICE_DEF)	PC_PRICE_CDD		PC_PRICE_DEF

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST)		PC_COST_CDD		PC_COST
	MAP (PC_COST_OLD)	PC_COST_CDD		PC_COST_OLD
	MAP (PC_COST_DEF)	PC_COST_CDD		PC_COST_DEF

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.HB"
	MAP (PC_PRCTYPE)	PC_PRCTYPE_CDD		PC_PRCTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PD_PRODUCT) &
		PD_PRODUCT.CH%, &
		PD_PRODUCT.READONLY%

	COM (CH_PC_PRICE) &
		PC_PRICE.CH%

	COM (CH_PC_COST) &
		PC_COST.CH%

	COM (TT_TABLE_LIST) &
		STITLE$ = 30%, &
		SSTAT$(3%) = 30%, &
		MTHDTITLE$ = 30%, &
		MTHDTYPE$(4%) = 30%

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
		SMG_WINDOW::DESCR = "New Inventory Product"
		SMG_WINDOW::NHELP = "IC_WRIT_PRODUCT"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::HSIZE = 40%
		SMG_WINDOW::VSIZE = 15%
		SMG_WINDOW::HPOS  = 40%
		SMG_WINDOW::VPOS  = 3%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 19%

		COM (IC_WRIT_PRODUCT_FRM) FRM$(19%)

		SMG_WINDOW::LWIDTH  = 40%
		SMG_WINDOW::LHEIGHT = 10%
		SMG_WINDOW::LHPOS   = 40%
		SMG_WINDOW::LVPOS   = 6%
		SMG_WINDOW::LLAST   = 1%
		SMG_WINDOW::LTITLE(0%) = "First Page"
		SMG_WINDOW::LPAGE(0%) = 13%
		SMG_WINDOW::LTITLE(1%) = "Last Page"
		SMG_WINDOW::LPAGE(1%) = 19%

		MTHDTITLE$ = "Meth    Description"
		MTHDTYPE$(0%) = "4"
		MTHDTYPE$(1%) = "STD     Standard Cost"
		MTHDTYPE$(2%) = "LIFO    Last-In, First-Out"
		MTHDTYPE$(3%) = "FIFO    First-In, First-Out"
		MTHDTYPE$(4%) = "WAVE    Weighted Average"

		STITLE$ = "Status   Description"
		SSTAT$(0%) = "3"
		SSTAT$(1%) = "A      Active"
		SSTAT$(2%) = "I      Inactive"
		SSTAT$(3%) = "O      Obsolete"

		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF PD_PRODUCT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PD_PRODUCT.READONLY%
			GOTO 790
		END IF


750		!
		! Open main file (existing) for modification
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			IC_WRIT_PRODUCT = ERR
			CONTINUE 770
		END WHEN

		PD_PRODUCT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
		USE
			IC_WRIT_PRODUCT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PD_PRODUCT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PD_PRODUCT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PD_PRODUCT.CH%

	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!

20100	CASE OPT_BACKGROUND

		SELECT MLOOP
		!
		! Main screen
		!
		CASE 0%

			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


			DATA	02,2, "(01) Description", &
				03,2, "(02) Product Type", &
				04,2, "(03) Category", &
				05,2, "(04) UOM", &
				06,2, "(05) Label", &
				07,2, "(06) Costing Method", &
				08,2, "(07) Onset Date", &
				09,2, "(08) Current Status", &
				10,2, "(09) Status Date", &
				11,2, "(10) Secondary Code", &
				12,2, "(11) Unit Weight", &
				13,2, "(12) Mfg UOM", &
				14,2, "(13) UOM Factor", &
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
		! 2nd page
		!
		CASE 1%
			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::LWINDOW)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::LWINDOW)

			DATA	02,  2, "(14) Location", &
				03,  2, "(15) Date", &
				04,  2, "(16) Cost", &
				05,  2, "(17) Price Type", &
				06,  2, "(18) Price", &
				0,  0, ""

			RESTORE
			XPOS% = -1%
			READ XPOS%, YPOS%, XSTR$ UNTIL XPOS% = 0%
			READ XPOS%, YPOS%, XSTR$

			I% = SMG_WINDOW::LPAGE(0%)

			WHILE (XPOS% <> 0%)
				I% = I% + 1%
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					XSTR$, XPOS%, YPOS%) &
					IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%

				READ XPOS%, YPOS%, XSTR$
			NEXT

			SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::LWINDOW)

		END SELECT
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
	!	^*(01) Description\*
	!	.p
	!	The ^*Description\* field
	!	enters a description for a specific product.
	!	.p
	!	The field will accommodate up to forty (40) alphanumeric
	!	characters.
	!
	! Index:
	!	.x Product>Description
	!	.x Tables>Product Description
	!
	!--
			PD_PRODUCT::DESCRIPTION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"02;27", TEMP$, &
				PD_PRODUCT::DESCRIPTION, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Product Type\*
	!	.p
	!	The ^*Product Type\* field
	!	enters a product type code as established in the Product
	!	Type table.
	!	.p
	!	The purpose of assigning product types is to group together
	!	like or similar products which may, as a group, be assigned a unique
	!	General Ledger account number.
	!	.p
	!	The field will accommodate two (2) alphanumeric characters.
	!	.p
	!	Pressing ^*<List Choices>\* while the cursor is located at
	!	this field will cause valid Product Types to be displayed.
	!
	! Index:
	!	.x Product>Type
	!
	!--
			PD_PRODUCT::PROD_TYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;27", TEMP$, &
				PD_PRODUCT::PROD_TYPE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "VX") = 1%)
				THEN
					PD_PRODUCT::PROD_TYPE = &
						PD_PRODTYPE::CODE
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "M")
				PD_PRODUCT::PROD_TYPE = PD_PRODTYPE::CODE
				GOTO ReEnter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Category\*
	!	.p
	!	The ^*Category\* field
	!	enters a category code as established in the Product Category
	!	table.
	!	.p
	!	The purpose of Category codes is to categorize products within
	!	a product type.
	!	.p
	!	The field will accommodate up to four (4) alphanumeric characters.
	!	.p
	!	Pressing ^*<List Choices>\* while the cursor is located at this
	!	field will cause valid Product Categories to be displayed.
	!
	! Index:
	!	.x Product>Category
	!
	!--
			PD_PRODUCT::CATEGORY = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;27", TEMP$, &
				PD_PRODUCT::CATEGORY, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PD_MAIN_CATEGORY.ID, "VX") = 1%)
				THEN
					PD_PRODUCT::CATEGORY = &
						PD_CATEGORY::CODE
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(PD_MAIN_CATEGORY.ID, "M")
				PD_PRODUCT::CATEGORY = PD_CATEGORY::CODE
				GOTO ReEnter
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Unit Of Measure\*
	!	.p
	!	The ^*Unit Of Measure\* field
	!	enters a code which identifies an applicable unit of measure which
	!	has been established in the Unit of Measure Table. By this UOM
	!	is inventory stored or sold.
	!	.p
	!	The field will accommodate two (2) alphanumeric characters.
	!	.p
	!	Pressing ^*<List Choices>\* while the cursor is located at this
	!	field will cause valid Units of Measure to be displayed.
	!
	! Index:
	!	.x Unit of Measure
	!
	!--
			PD_PRODUCT::UOM= ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;27", TEMP$, &
				PD_PRODUCT::UOM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "V0  ") = 1%)
				THEN
					PD_PRODUCT::UOM = &
						UTL_MEASURE::CODE
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "M")
				PD_PRODUCT::UOM = UTL_MEASURE::CODE
				GOTO ReEnter
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Label\*
	!	.p
	!	The ^*Label\* field
	!	enters an applicable label code as established in the Label
	!	Description table.
	!	.p
	!	If the same general product is available with different labels,
	!	a unique product number must be established for each label.
	!	.p
	!	This field will accommodate up to four (4) alphanumeric characters.
	!	.p
	!	Pressing ^*<List Choices>\* while the cursor is at this field
	!	will cause valid Label Codes to be displayed.
	!
	! Index:
	!	.x Label
	!
	!--
			PD_PRODUCT::LABEL = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;27", TEMP$, &
				PD_PRODUCT::LABEL, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PD_MAIN_LABEL.ID, "V0  ") = 1%)
				THEN
					PD_PRODUCT::LABEL = &
						PD_LABEL::CODE
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(PD_MAIN_LABEL.ID, "M")
				PD_PRODUCT::LABEL = PD_LABEL::CODE
				GOTO ReEnter
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Costing Method\*
	!	.p
	!	The ^*Costing Method\* field enters the
	!	method of costing for a particular item.
	!	.p
	!	Valid settings are:
	!	.lm +10
	!	.b
	!	.list 0,"*"
	!	.le
	!	STD #- Standard Cost
	!	.le
	!	LIFO - Last In, First Out
	!	.le
	!	FIFO - First In, First Out
	!	.le
	!	WAVE - Weighted Average
	!	.els
	!	.lm -10
	!	.p
	!	Pressing ^*<List Choices>\* while the cursor is located at this
	!	field will cause a list of valid Costing Method codes to be
	!	displayed.
	!
	! Index:
	!	.x Costing Method
	!
	!--
			PD_PRODUCT::METHOD = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;27", TEMP$, PD_PRODUCT::METHOD, &
				MFLAG, "'E", MVALUE, MTHDTYPE$(), &
				MTHDTITLE$, "007"), -1%)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Onset Date\*
	!	.p
	!	The ^*Onset Date\* field contains the date of the first time the record
	!	was added to the files-the first time the file was created.
	!
	! Index:
	!	.x Onset Date>Product Maintenance
	!	.x Product Maintenance>Onset Date
	!
	!--
			PD_PRODUCT::BDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;27", TEMP$, &
				PD_PRODUCT::BDATE, MFLAG, "8", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Current Status \*
	!	.p
	!	The ^*current status\* field enters the
	!	activity status of the product.
	!	.p
	!	Valid settings are:
	!	.lm +10
	!	.b
	!	.list 0,"*"
	!	.le
	!	A - Active
	!	.le
	!	I - Inactive
	!	.le
	!	O - Obsolete
	!	.els
	!	.lm -10
	!	.p
	!	Pressing ^*<List Choices>\* while the cursor is located at this
	!	field will display a list of valid codes.
	!
	! Index:
	!	.x Current Status
	!	.x Activity Status
	!
	!--
			PD_PRODUCT::SSTATUS = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;27", TEMP$, &
				PD_PRODUCT::SSTATUS, MFLAG, "'E", MVALUE, &
				SSTAT$(), STITLE$, "008"), -1%)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Status Date\*
	!	.p
	!	The ^*Status Date\* field contains the date the decision was made to
	!	no longer  use the product. This decision may be temporary.
	!
	! Index:
	!	.x Status Date>Product Maintenance
	!	.x Product Maintenance>Status Date
	!
	!--
			PD_PRODUCT::EDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;27", TEMP$, &
				PD_PRODUCT::EDATE, MFLAG, "8", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Secondary Code\*
	!	.p
	!	The ^*Secondary Code\* field assigns a secondary
	!	product number, such as product bar code number, product cash
	!	register number, etc.
	!	.p
	!	The field will accommodate up to fourteen (10) alphanumeric
	!	characters.
	!	.p
	!	Each secondary product number (which is not empty)
	!	must be unique. No duplicates are allowed.
	!
	! Index:
	!	.x Product>Secondary Code
	!	.x Bar Code
	!	.x Product>Secondary Code
	!
	!--
			PD_PRODUCT::SECONDARY_CODE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;27", TEMP$, &
				PD_PRODUCT::SECONDARY_CODE, MFLAG, "'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Unit Weight\*
	!	.p
	!	The ^*Unit Weight\* field enters the
	!	weight for a specific inventory product.  Any
	!	measure of weight may be used, but the measure of weight used must
	!	be consistent throughout the Product Description file.
	!
	! Index:
	!	.x Unit Weight
	!	.x Weight of the Product
	!
	!--

			PD_PRODUCT::WEIGHT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;27", TEMP$, PD_PRODUCT::WEIGHT, MFLAG, "###,###.####", &
				MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) BOM Unit Of Measure\*
	!	.p
	!	The ^*BOM Unit Of Measure\* field
	!	enters a code which identifies an unit of measure which
	!	will be used in the Bill of Materials Tables.
	!	.p
	!	The field will accommodate two (2) alphanumeric characters.
	!	.p
	!	Pressing ^*<List Choices>\* while the cursor is located at this
	!	field will cause valid Units of Measure to be displayed.
	!
	! Index:
	!	.x BOM Unit of Measure
	!
	!--
			PD_PRODUCT::BOMUOM= ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;27", TEMP$, &
				PD_PRODUCT::BOMUOM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "V0  ") = 1%)
				THEN
					PD_PRODUCT::BOMUOM = &
						UTL_MEASURE::CODE
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "M")
				PD_PRODUCT::BOMUOM = UTL_MEASURE::CODE
				GOTO ReEnter
			END IF

		CASE 13%

	!++
	! Abstract:FLD013
	!	^*(13) UOM Factor\*
	!	.p
	!	The ^*UOM Factor\* field enters
	!	a quantity of BOM units of measure (field 15) contained in a
	!	particular product unit, based on the unit of measure
	!	from the field 5.
	!
	! Index:
	!	.x UOM Factor
	!
	!--

			PD_PRODUCT::PRODUCT_FACTOR = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;27", TEMP$, PD_PRODUCT::PRODUCT_FACTOR, MFLAG, "###,###.#", &
				MVALUE)

		CASE 14%

	!++
	! Abstract:FLD014
	!	^*(14) Location Number\*
	!	.p
	!	The ^*Location _#\* field enters a location code
	!	which is established in the Company Profile file located in the
	!	Utility System.
	!	.p
	!	This field will accommodate up to four (4) alphanumeric
	!	characters.
	!	.p
	!	Pressing ^*<List Choices>\* while the cursor is located at this
	!	field will cause a list of valid Location codes to be displayed.
	!
	! Index:
	!	.x Location Number>Cost Maintenance
	!	.x Cost Maintenance>Location Number
	!
	!--

			PC_COST::LOCATION = ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"02;23", TEMP$, &
				PC_COST::LOCATION, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					PC_COST::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 15%

	!++
	! Abstract:FLD015
	!	^*(15) Date\*
	!	.p
	!	The ^*Date\* field enters the effective date for
	!	a particular product cost.
	!	.p
	!	The format for entry is MMDDYYYY or MMDDYY.
	!
	! Index:
	!	.x Date>Cost Maintenance
	!	.x Cost Maintenance>Date
	!
	!--

			PC_COST::EFFDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::LWINDOW, &
				"03;23", TEMP$, &
				PC_COST::EFFDATE, MFLAG, "'E", MVALUE)

		CASE 16%

	!++
	! Abstract:FLD016
	!	^*(16) Cost/Unit\*
	!	.p
	!	The ^*Cost/Unit\* field enters the cost of
	!	a particular unit.
	!	.p
	!	If a unique product is available in a particular unit
	!	of measure, i.e. "gallon", "piece", "yard", that cost
	!	is entered in this field.
	!
	! Index:
	!	.x Cost/Unit>Cost Maintenance
	!	.x Cost Maintenance>Cost/Unit
	!
	!--

			PC_COST::COST = ENTR_3NUMBER(SCOPE,  SMG_WINDOW::LWINDOW, &
				"04;23",TEMP$, PC_COST::COST, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 17%

	!++
	! Abstract:FLD017
	!	^*(17) Price Type\*
	!	.p
	!	The ^*Price Type\* field enters a valid price
	!	type code as established in the Price Type description file.
	!	.p
	!	This field will accommodate two (2) alphanumeric characters.
	!	.p
	!	Pressing ^*<List Choices>\* at this field will provide a list
	!	of valid Price Type codes.
	!
	! Index:
	!	.x Price Type>Price Maintenance
	!	.x Price Maintenance>Price Type
	!	.x Type>Price
	!
	!--

			PC_PRICE::PCTYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"05;23", TEMP$, PC_PRICE::PCTYPE, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "V0") = 1%
				THEN
					PC_PRICE::PCTYPE = PC_PRCTYPE::CODE
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "M")
				PC_PRICE::PCTYPE =  PC_PRCTYPE::CODE
				GOTO ReEnter
			END IF

		CASE 18%

	!++
	! Abstract:FLD018
	!	^*(18) Price/Unit\*
	!	.p
	!	The ^*Price/Unit\* field enters the price of
	!	a particular unit.
	!	.p
	!	If a unique product is available in a particular unit of
	!	measure, i.e. "gallon", "piece", "yard", etc., that price is
	!	entered in this field.
	!
	! Index:
	!	.x Price/Unit>Price Maintenance
	!	.x Price Maintenance>Price/Unit
	!
	!--

			PC_PRICE::PRICECOST = ENTR_3NUMBER(SCOPE,  SMG_WINDOW::LWINDOW, &
				"06;23",TEMP$, PC_PRICE::PRICECOST, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		IC_WRIT_PRODUCT = 0%

		SELECT MLOOP

		CASE 2%

			FLAG% = MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "Q0" + &
				PD_PRODUCT::PROD_TYPE)

			PD_PRODTYPE::DESCRIPTION = STRING$(LEN( &
				PD_PRODTYPE::DESCRIPTION), A"?"B) &
				IF FLAG% <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODTYPE::DESCRIPTION, 3%, 50%, , SMG$M_BOLD)


		CASE 3%
			FLAG% = MAIN_WINDOW(PD_MAIN_CATEGORY.ID, "Q0" + &
				PD_PRODUCT::CATEGORY)

			PD_CATEGORY::DESCRIPTION = STRING$(LEN( &
				PD_CATEGORY::DESCRIPTION), A"?"B) &
				IF FLAG% <> 1%

			PD_CATEGORY::DESCRIPTION = "" &
				IF PD_PRODUCT::CATEGORY = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_CATEGORY::DESCRIPTION, 4%, 50%, , SMG$M_BOLD)


		CASE 4%
			!
			! Is the input defined?
			!
			IF MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "Q0" + &
				PD_PRODUCT::UOM) <> 1%
			THEN
				UTL_MEASURE::DESCRIPTION = STRING$(LEN( &
					UTL_MEASURE::DESCRIPTION), A"?"B)
				!
				! See if they are allowing undefined inputs
				!
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_MEASURE::DESCRIPTION, 5%, 50%, , SMG$M_BOLD)

		CASE 5%
			FLAG% = MAIN_WINDOW(PD_MAIN_LABEL.ID, "Q0" + &
				PD_PRODUCT::LABEL)

			PD_LABEL::DESCRIPTION = STRING$(LEN( &
				PD_LABEL::DESCRIPTION), A"?"B) &
				IF FLAG% <> 1%

			PD_LABEL::DESCRIPTION = "" &
				IF PD_PRODUCT::LABEL = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_LABEL::DESCRIPTION, 6%, 50%, , SMG$M_BOLD)

		CASE 12%

			!
			! Is the input defined?
			!
			IF MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "Q0" + &
				PD_PRODUCT::BOMUOM) <> 1%
			THEN
				UTL_MEASURE::DESCRIPTION = STRING$(LEN( &
					UTL_MEASURE::DESCRIPTION), A"?"B)

			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_MEASURE::DESCRIPTION, 13%, 50%, , SMG$M_BOLD)

		END SELECT

	!
	! Set PD_PRODUCT_OLD value
	!
20500	CASE OPT_SETOLD
		PD_PRODUCT_OLD = PD_PRODUCT
		PC_PRICE_OLD = PC_PRICE
		PC_COST_OLD = PC_COST

	!
	! Restore PD_PRODUCT_OLD value
	!
	CASE OPT_RESETOLD
		PD_PRODUCT = PD_PRODUCT_OLD
		PC_PRICE = PC_PRICE_OLD
		PC_COST = PC_COST_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PD_PRODUCT_DEF = PD_PRODUCT
		PC_COST_DEF = PC_COST
		PC_PRICE_DEF = PC_PRICE

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(16%) = "#,###,###.######"
				FRM$(18%) = "#,###,###.###"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF
	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PD_PRODUCT = PD_PRODUCT_DEF
		PC_COST = PC_COST_DEF
		PC_PRICE = PC_PRICE_DEF

		PD_PRODUCT::PRODUCT_NUM = MVALUE

		PD_PRODUCT::BDATE = DATE_TODAY &
			IF PD_PRODUCT::BDATE = ""

		PC_COST::EFFDATE = DATE_TODAY &
			IF PC_COST::EFFDATE =  ""

	CASE OPT_AFTEROPT
		!
		! Open cost for modification
		!
		IF PC_COST.CH% <= 0%
		THEN
			%INCLUDE "SOURCE:[PC.OPEN]PC_COST.CRE"
		END IF

		PC_COST::PRODUCT = PD_PRODUCT::PRODUCT_NUM

		PUT #PC_COST.CH%

		!
		! Open cost for modification
		!
		IF PC_PRICE.CH% <= 0%
		THEN
			%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.CRE"
		END IF

		PC_PRICE::PRODUCT_NUM = PD_PRODUCT::PRODUCT_NUM
		PC_PRICE::LOCATION = PC_COST::LOCATION
		PC_PRICE::XTIME  = "000000"
		PC_PRICE::XDATE = PC_COST::EFFDATE

		PUT #PC_PRICE.CH%

		!
		! do not add the next record
		!
		IC_WRIT_PRODUCT = 1%

	END SELECT

27000	EXIT FUNCTION

29000	!****************************************************************
	! Trap errors
	!****************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	! More menu option hidden in mast
	!++
	! Abstract:CYCLE
	!	^*Cycle\*
	!	.p
	!	The ^*Cycle\* option accesses the information
	!	needed for cycle counting.
	!
	! Index:
	!	.x Cycle
	!
	!--
	!+-+-+
	! More menu option hidden in mast
	!++
	! Abstract:PRICE
	!	^*Price\*
	!	.p
	!	The ^*Price\* option
	!	maintains the selling price. There may be several selling prices for a single
	!	item.
	!
	! Index:
	!	.x Price
	!
	!--
	!+-+-+
	! More menu option hidden in mast
	!++
	! Abstract:COST
	!	^*Cost\*
	!	.b
	!	.lm +5
	!	The ^*Cost\* option stores information
	!	concerning the standard costing method if used. It also contains the company's
	!	cost for the item.
	!	.lm -5
	!
	! Index:
	!	.x Cost
	!
	!--
	!+-+-+
	! More menu option hidden in mast
	!++
	! Abstract:DIRECTIONS
	!	^*Directions\*
	!	.p
	!	The ^*Directions\* option includes step by step
	!	instructions for the recipe and ingredients.
	!
	! Index:
	!	.x Directions
	!
	!--
	!+-+-+
	! More menu option hidden in mast
	!++
	! Abstract:INGREDIENTS
	!	^*Ingredients\*
	!	.p
	!	The ^*Ingredients\* option
	!	allows for entry of specific ingredients information.
	!
	! Index:
	!	.x Ingredients
	!
	!--
	!+-+-+
	! More menu option hidden in mast
	!++
	! Abstract:COMPONENTS
	!	^*Components\*
	!	.p
	!	The ^*Components\* option
	!	maintains information concerning the material components.
	!
	! Index:
	!	.x Components
	!
	!--
	!+-+-+
	! More menu option hidden in mast
	!++
	! Abstract:INSTRUCTIONS
	!	^*Instructions\*
	!	.p
	!	The ^*Instructions\* option enters
	!	specific instructions for the system.
	!
	! Index:
	!	.x Instructions
	!
	!--
	!+-+-+
	! More menu option hidden in mast
	!++
	! Abstract:TRANSFLAG
	!	^*Transaction Flag\*
	!	.p
	!	The ^*Transaction Flag\* option
	!	enters the transaction types and descriptions which will be used as flags.
	!
	! Index:
	!	.x Transaction Flag
	!	.x Flag>Transaction
	!
	!--
