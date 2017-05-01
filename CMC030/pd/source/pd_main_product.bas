1	%TITLE "Product Description"
	%SBTTL "PD_MAIN_PRODUCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PD_MAIN_PRODUCT(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	.b
	!	.lm +5
	!	The new product numbers are assigned and identified, and new product information
	!	is entered and maintained for existing products through the ^*Product
	!	Description\* file.
	!	.lm -5
	!
	! Index:
	!	.x Product Description>Table
	!	.x Tables>Product Description
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_MAIN_PRODUCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PD_MAIN_PRODUCT
	!	$ DELETE PD_MAIN_PRODUCT.OBJ;*
	!
	! Author:
	!
	!	07/21/87 - Frank Starman
	!
	! Modification history:
	!
	!	07/21/87 - Frank F. Starman
	!		Changed layout of the Product file (added UOM)
	!
	!	05/23/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	06/22/89 - J. Shad Rydalch
	!		Changed layout of the Product file (added BDATE,
	!		SSTATUS, EDATE, SECONDARY_CODE)
	!
	!	08/19/91 - Dan Perkins
	!		Added F17 key feature.
	!		Print Unit of Measure Description
	!
	!	12/20/91 - Dan Perkins
	!		Changed VO on fields 3 and 4 to VX to allow
	!		lookup at a given point instead of beginning
	!		of file.
	!
	!	12/26/91 - Frank F. Starman
	!		Change layout. Added WEIGHT,BOMUOM,PRODUCT_FACTOR.
	!
	!	12/26/91 - Dan Perkins
	!		Added fields from PD_PACK in order to combine
	!		these two files into one.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	02/26/92 - Kevin Handy
	!		Removed references to file PD_PACK that Frank
	!		deleted so programs couldn't compile.
	!
	!	03/06/92 - Kevin Handy
	!		Fixed field 12 so that it had it's own number (12)
	!		to access it by, instead of being part of (11).
	!
	!	03/09/92 - Dan Perkins
	!		Added code in OPT_DISPLAY to display BOMUOM info in
	!		field 13.
	!
	!	03/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/09/93 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/25/95 - Kevin Handy
	!		Modified to put current date in EDATE, like it did
	!		for BDATE.
	!
	!	01/27/96 - Kevin Handy
	!		Reformat source code.
	!		Changed STRING$(...,ASCII(" ")) to "" in several places.
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/25/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	06/01/2000 - Kevin Handy
	!		More WHEN ERROR IN
	!		Clean up (Check2)
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
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

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PD_PRODUCT) &
		PD_PRODUCT.CH%, &
		PD_PRODUCT.READONLY%

	COM (TT_TABLE_LIST) &
		STITLE$ = 30%, &
		SSTAT$(3%) = 30%, &
		MTHDTITLE$ = 30%, &
		MTHDTYPE$(4%) = 30%

	!
	! External functions
	!
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
		! Define window
		!
		SMG_WINDOW::DESCR = "Inventory Product Description"
		SMG_WINDOW::NHELP = "PD_MAIN_PRODUCT"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 14%

		SMG_WINDOW::NKEYS = 5%
		SMG_WINDOW::KNAME(0%) = "Product_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
		SMG_WINDOW::KNAME(2%) = "Category"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 4%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%
		SMG_WINDOW::KNAME(3%) = "Description"
			SMG_WINDOW::KFIELD(3%, 0%) = 1%
			SMG_WINDOW::KFIELD(3%, 1%) = 2%
		SMG_WINDOW::KNAME(4%) = "Secondary_code"
			SMG_WINDOW::KFIELD(4%, 0%) = 2%
			SMG_WINDOW::KFIELD(4%, 1%) = 11%
			SMG_WINDOW::KFIELD(4%, 2%) = 1%

		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%

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

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

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
			PD_MAIN_PRODUCT = ERR
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
			PD_MAIN_PRODUCT = ERR
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
		WHEN ERROR IN
			RESET #PD_PRODUCT.CH%
			GET #PD_PRODUCT.CH%, REGARDLESS
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


		DATA	02,15, "(01) Product #", &
			03,15, "(02) Description", &
			04,15, "(03) Product Type", &
			05,15, "(04) Category", &
			06,15, "(05) UOM", &
			07,15, "(06) Label", &
			08,15, "(07) Costing Method", &
			09,15, "(08) Onset Date", &
			10,15, "(09) Current Status", &
			11,15, "(10) Status Date", &
			12,15, "(11) Secondary Code", &
			13,15, "(12) Unit Weight", &
			14,15, "(13) Mfg UOM", &
			15,15, "(14) UOM Factor", &
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
	!	.x Product>Number
	!	^*(01) Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Product _#\* field
	!	enters an assigned number which identifies a specific product.
	!	.b
	!	Each product number must be unique.  No duplicates are allowed.
	!	.B
	!	The field will accept up to fourteen characters.  An entry is
	!	required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
			PD_PRODUCT::PRODUCT_NUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;40", TEMP$, &
				PD_PRODUCT::PRODUCT_NUM, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Product>Description
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field
	!	enters a description for a specific product entered in
	!	field (01).
	!	.b
	!	Up to forty characters may be entered for the description.
	!	.lm -5
	!
	! Index:
	!	.x Tables>Product Description
	!
	!--
			PD_PRODUCT::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;40", TEMP$, &
				PD_PRODUCT::DESCRIPTION, MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x Product>Type
	!	^*(03) Product Type\*
	!	.b
	!	.lm +5
	!	The ^*Product Type\* field
	!	enters a product type code which has been established
	!	in the Inventory Control System.
	!	.b
	!	The purpose of assigning product types is to group together
	!	like or similar products which may, as a group, be assigned a unique
	!	General Ledger account number.
	!	.b
	!	Valid Product Type codes may be viewed by pressing ^*List Choices\* or
	!	additional Product Types added by pressing ^*F17\*.
	!	.b
	!	The field will accommodate two characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PD_PRODUCT::PROD_TYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;40", TEMP$, &
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

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Product>Category
	!	^*(04) Category\*
	!	.b
	!	.lm +5
	!	The ^*Category\* field
	!	enters a category code as established in the Product Category
	!	table.
	!	.b
	!	The purpose of Category codes is to categorize products within
	!	a product type.
	!	.b
	!	Valid Category codes may be viewed by pressing ^*List Choices\*.
	!	Additional Category codes may be added by pressing ^*F17\*.
	!	.b
	!	The field will accept up to four characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PD_PRODUCT::CATEGORY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;40", TEMP$, &
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

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Unit of Measure
	!	^*(05) Unit Of Measure\*
	!	.b
	!	.lm +5
	!	The ^*Unit Of Measure\* field
	!	enters a code which identifies an applicable unit of measure which
	!	has been established in the Unit of Measure Table.
	!	.b
	!	Valid Unit of Measure codes may be viewed by pressing ^*List Choices\*.
	!	Additional Unit of Measure codes may be added by pressing ^*F17\*.
	!	.b
	!	The field will accept two characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PD_PRODUCT::UOM= ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;40", TEMP$, &
				PD_PRODUCT::UOM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_MEASURE.ID, &
					"V0  ") = 1%)
				THEN
					PD_PRODUCT::UOM = UTL_MEASURE::CODE
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "M")
				PD_PRODUCT::UOM = UTL_MEASURE::CODE
				GOTO ReEnter
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x Label
	!	^*(06) Label\*
	!	.b
	!	.lm +5
	!	The ^*Label\* field
	!	enters an applicable label code as established in the Label
	!	Description table.
	!	.b
	!	If the same general product is available with different labels,
	!	a unique product number must be established for each label.
	!	No duplicates are allowed.
	!	.b
	!	Valid label codes may be viewed by pressing ^*List Choices\*.
	!	Additional label codes may be added by pressing ^*F17\*.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			PD_PRODUCT::LABEL = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;40", TEMP$, &
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

		CASE 7%
	!++
	! Abstract:FLD007
	!	.x Costing Method
	!	^*(07) Costing Method\*
	!	.b
	!	.lm +5
	!	The ^*Costing Method\* field enters the
	!	method of costing for a particular item.
	!	.b
	!	Valid settings are:
	!	.table 3,10
	!	.te
	!	^*STD\*  - Standard Cost
	!	.te
	!	^*LIFO\* - Last In, First Out
	!	.te
	!	^*FIFO\* - First In, First Out
	!	.te
	!	^*WAVE\* - Weighted Average
	!	.end table
	!	Valid Costing Methods may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			PD_PRODUCT::METHOD = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"08;40", TEMP$, PD_PRODUCT::METHOD, &
				MFLAG, "'E", MVALUE, MTHDTYPE$(), &
				MTHDTITLE$, "007"), -1%)

		CASE 8%
	!++
	! Abstract:FLD008
	!	.x Onset Date>Product Maintenance
	!	^*(08) Onset Date\*
	!	.b
	!	.lm +5
	!	The ^*Onset Date\* field contains the date the record
	!	was added to the files, the first time the file was created.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Product Maintenance>Onset Date
	!
	!--
			PD_PRODUCT::BDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;40", TEMP$, &
				PD_PRODUCT::BDATE, MFLAG, "8", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	.x Current Status
	!	^*(09) Current Status\*
	!	.b
	!	.lm +5
	!	The ^*current status\* field enters the
	!	activity status of the product.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*A\* - Active
	!	.te
	!	^*I\* - Inactive
	!	.te
	!	^*O\* - Obsolete
	!	.end table
	!	Valid codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Activity Status
	!
	!--
			PD_PRODUCT::SSTATUS = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;40", TEMP$, &
				PD_PRODUCT::SSTATUS, MFLAG, "'E", MVALUE, &
				SSTAT$(), STITLE$, "008"), -1%)

		CASE 10%
	!++
	! Abstract:FLD010
	!	.x Status Date>Product Maintenance
	!	^*(10) Status Date\*
	!	.b
	!	.lm +5
	!	The ^*Status Date\* field contains the date the decision for
	!	the entry in field (09) Current Status was made.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Product Maintenance>Status Date
	!
	!--
			PD_PRODUCT::EDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;40", TEMP$, &
				PD_PRODUCT::EDATE, MFLAG, "8", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	.x Product>Secondary Code
	!	^*(11) Secondary Code\*
	!	.b
	!	.lm +5
	!	The ^*Secondary Code\* field assigns a secondary
	!	product number, such as product bar code number, product cash
	!	register number, etc.
	!	.b
	!	Each secondary product number
	!	must be unique. No duplicates are allowed.
	!	.b
	!	The field will accept up to ten characters.
	!	.lm -5
	!
	! Index:
	!	.x Bar Code
	!	.x Product>Secondary Code
	!
	!--
			PD_PRODUCT::SECONDARY_CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;40", TEMP$, &
				PD_PRODUCT::SECONDARY_CODE, MFLAG, "'E", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Unit Weight\*
	!	.b
	!	.lm +5
	!	The ^*Unit Weight\* field enters the
	!	weight for a specific inventory product.
	!	.b
	!	^*Note:\* Any measure of weight may be used, but the measure
	!	of weight used must be consistent throughout the Product Description file.
	!	.lm -5
	!
	! Index:
	!	.x Unit Weight
	!	.x Weight of the Product
	!
	!--

			PD_PRODUCT::WEIGHT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"13;40", TEMP$, PD_PRODUCT::WEIGHT, &
				MFLAG, "###,###.####", MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	.x Mfg Unit of Measure
	!	^*(13) Mfg Unit Of Measure\*
	!	.b
	!	.lm +5
	!	The ^*Mfg Unit Of Measure\* field
	!	enters a code which identifies a unit of measure which
	!	will be used in the Mfg Tables.
	!	.b
	!	Valid Unit Of Measure (UOM) codes may be viewed by pressing ^*List Choices\*,
	!	or additional codes added by pressing ^*F17\*.
	!	.b
	!	The field will accept up to two characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PD_PRODUCT::BOMUOM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;40", TEMP$, &
				PD_PRODUCT::BOMUOM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_MEASURE.ID, &
					"V0  ") = 1%)
				THEN
					PD_PRODUCT::BOMUOM = UTL_MEASURE::CODE
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "M")
				PD_PRODUCT::BOMUOM = UTL_MEASURE::CODE
				GOTO ReEnter
			END IF

		CASE 14%

	!++
	! Abstract:FLD014
	!	^*(14) UOM Factor\*
	!	.b
	!	.lm +5
	!	The ^*UOM Factor\* field enters
	!	the quantity of BOM units of measure (field 15) contained in a
	!	particular product unit, based on the unit of measure
	!	(field 05).
	!	.lm -5
	!
	! Index:
	!	.x UOM Factor
	!
	!--

			PD_PRODUCT::PRODUCT_FACTOR = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"15;40", TEMP$, PD_PRODUCT::PRODUCT_FACTOR, &
				MFLAG, "###,###.#", MVALUE)
		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		PD_MAIN_PRODUCT = 0%

		SELECT MLOOP

		CASE 1%
			IF PD_PRODUCT::PRODUCT_NUM = ""
			THEN
				PD_MAIN_PRODUCT = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					PD_MAIN_PRODUCT = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		CASE 3%
			PD_MAIN_PRODUCT = FUNC_TESTENTRY(SMG_WINDOW, &
				PD_PRODUCT::PROD_TYPE, &
				PD_PRODTYPE::DESCRIPTION, &
				"PD", MLOOP, "PROD", &
				"Product Type", PD_MAIN_PRODTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODTYPE::DESCRIPTION, 4%, 50%, , SMG$M_BOLD)

		CASE 4%
			IF PD_PRODUCT::CATEGORY = ""
			THEN
				PD_CATEGORY::DESCRIPTION = ""
			ELSE
				PD_MAIN_PRODUCT = FUNC_TESTENTRY(SMG_WINDOW, &
					PD_PRODUCT::CATEGORY, &
					PD_CATEGORY::DESCRIPTION, &
					"PD", MLOOP, "PROD", &
					"Product Category", PD_MAIN_CATEGORY.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_CATEGORY::DESCRIPTION, 5%, 50%, , SMG$M_BOLD)

		CASE 5%
			PD_MAIN_PRODUCT = FUNC_TESTENTRY(SMG_WINDOW, &
				PD_PRODUCT::UOM, UTL_MEASURE::DESCRIPTION, &
				"PD", MLOOP, "PROD", &
				"Product UOM", UTL_MAIN_MEASURE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_MEASURE::DESCRIPTION, 6%, 50%, , SMG$M_BOLD)

		CASE 6%
			IF PD_PRODUCT::LABEL = ""
			THEN
				PD_LABEL::DESCRIPTION = ""
			ELSE
				PD_MAIN_PRODUCT = FUNC_TESTENTRY(SMG_WINDOW, &
					PD_PRODUCT::LABEL, &
					PD_LABEL::DESCRIPTION, &
					"PD", MLOOP, "PROD", &
					"Product Label", PD_MAIN_LABEL.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_LABEL::DESCRIPTION, 7%, 50%, , SMG$M_BOLD)

		CASE 13%
			PD_MAIN_PRODUCT = FUNC_TESTENTRY(SMG_WINDOW, &
				PD_PRODUCT::BOMUOM, UTL_MEASURE::DESCRIPTION, &
				"PD", MLOOP, "PROD", &
				"Pack UOM", UTL_MAIN_MEASURE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_MEASURE::DESCRIPTION, 14%, 50%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			!
			! Display desciption for product type
			!
			PD_PRODTYPE::DESCRIPTION = STRING$(LEN( &
				PD_PRODTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "Q0" + &
				PD_PRODUCT::PROD_TYPE) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODTYPE::DESCRIPTION, 4%, 50%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			!
			! Display description for category
			!
			PD_CATEGORY::DESCRIPTION = STRING$(LEN( &
				PD_CATEGORY::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_CATEGORY.ID, "Q0" + &
				PD_PRODUCT::CATEGORY) <> 1%

			PD_CATEGORY::DESCRIPTION = "" &
				IF PD_PRODUCT::CATEGORY = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_CATEGORY::DESCRIPTION, 5%, 50%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			!
			! Display Unit of Measure
			!
			UTL_MEASURE::DESCRIPTION = STRING$(LEN( &
				UTL_MEASURE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "Q0" + &
				PD_PRODUCT::UOM) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_MEASURE::DESCRIPTION, 6%, 50%, , SMG$M_BOLD)

		END IF

		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN
			!
			! Display label description
			!
			PD_LABEL::DESCRIPTION = STRING$(LEN( &
				PD_LABEL::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_LABEL.ID, "Q0" + &
				PD_PRODUCT::LABEL) <> 1%

			PD_LABEL::DESCRIPTION = "" &
				IF PD_PRODUCT::LABEL = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_LABEL::DESCRIPTION, 7%, 50%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(13%) AND 2%) = 0%
		THEN
			!
			! Display BOM Unit of Measure
			!
			UTL_MEASURE::DESCRIPTION = STRING$(LEN( &
				UTL_MEASURE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "Q0" + &
				PD_PRODUCT::BOMUOM) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_MEASURE::DESCRIPTION, 14%, 50%, , SMG$M_BOLD)

		END IF

	!
	! Set PD_PRODUCT_OLD value
	!
20500	CASE OPT_SETOLD
		PD_PRODUCT_OLD = PD_PRODUCT

	!
	! Restore PD_PRODUCT_OLD value
	!
	CASE OPT_RESETOLD
		PD_PRODUCT = PD_PRODUCT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PD_PRODUCT_DEF = PD_PRODUCT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PD_PRODUCT = PD_PRODUCT_DEF
		PD_PRODUCT::BDATE = DATE_TODAY &
			IF PD_PRODUCT::BDATE = ""
		PD_PRODUCT::EDATE = DATE_TODAY &
			IF PD_PRODUCT::EDATE = ""

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = "  Product #      " + &
				"Description                              Ty Catg " + &
				"UOM Lbl  CMth OnsetDate  AS TerminDate SecCode"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "017,058,061,066,070,075,080,091,094,105"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PD_PRODUCT::PRODUCT_NUM + " "  + &
				PD_PRODUCT::DESCRIPTION + " "  + &
				PD_PRODUCT::PROD_TYPE + " "  + &
				PD_PRODUCT::CATEGORY + " "  + &
				PD_PRODUCT::UOM + "  " + &
				PD_PRODUCT::LABEL + " "  + &
				PD_PRODUCT::METHOD + " "  + &
				PRNT_DATE(PD_PRODUCT::BDATE, 8%) + " "  + &
				PD_PRODUCT::SSTATUS + "  " + &
				PRNT_DATE(PD_PRODUCT::EDATE, 8%) + " "  + &
				PD_PRODUCT::SECONDARY_CODE

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PD_PRODUCT::PRODUCT_NUM + "", &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE PD_PRODUCT::PROD_TYPE + &
				PD_PRODUCT::PRODUCT_NUM, &
				REGARDLESS
		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE PD_PRODUCT::CATEGORY + &
				PD_PRODUCT::PRODUCT_NUM, &
				REGARDLESS
		CASE 3%
			FIND #SMG_WINDOW::CHAN, &
				KEY #3% GE PD_PRODUCT::DESCRIPTION + "", &
				REGARDLESS
		CASE 4%
			FIND #SMG_WINDOW::CHAN, &
				KEY #4% GE PD_PRODUCT::SECONDARY_CODE + "", &
				REGARDLESS
		END SELECT

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
	!	.b
	!	.lm +5
	!	The ^*Cycle\* option accesses to the information
	!	needed for cycle counting.
	!	.lm -5
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
	!	.b
	!	.lm +5
	!	The ^*Price\* option
	!	maintains the selling price. There may be several selling prices for a single
	!	item.
	!	.lm -5
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
	!	.b
	!	.lm +5
	!	The ^*Directions\* option includes step by step
	!	instructions for the recipe and ingredients.
	!	.lm -5
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
	!	.b
	!	.lm +5
	!	The ^*Ingredients\* option
	!	entrers specific ingredients information.
	!	.lm -5
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
	!	.b
	!	.lm +5
	!	The ^*Components\* option
	!	maintains information concerning the material components.
	!	.lm -5
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
	!	.b
	!	.lm +5
	!	The ^*Instructions\* option in the COMMAND menu enters
	!	user specific instructions for the system.
	!	.lm -5
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
	!	.b
	!	.lm +5
	!	The ^*Transaction Flag\* option
	!	enters the transaction types and descriptions which will be used as flags.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Flag
	!	.x Flag>Transaction
	!
	!--
	!+-+-+
	!++
	! Abstract:SUB
	!	^*SUB - Substitute part numbers\*
	!	.b
	!	Defines substitute part numbers for a known part number.
	!	This allows entering such things as a UPC number in a part
	!	number field, and having it replace it with your actual
	!	part number.
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:UPD
	!	^*Upd - Update Costs\*
	!	.b
	!	Update the cost of this product based upon it's components.
	!	It only looks down one level.
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:CALCY_LAB
	!	^*Calcy_labor - Calculate Labor\*
	!	.b
	!	Recalculates the labor on the current product based
	!	on the labor in it's children and the cost specified
	!	for the current level.
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:COPY_MAT
	!	^*Copy_Mat - Copy Material\*
	!	.b
	!	Copies the material from another component to the current one.
	!	Used to generate bill of material for a new product that
	!	is similiar to an existing one.
	!
	! Index:
	!
	!--
