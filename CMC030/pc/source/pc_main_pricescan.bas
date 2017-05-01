1	%TITLE "Maintain Product Price File"
	%SBTTL "PC_MAIN_PRICESCAN"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PC_MAIN_PRICESCAN(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.b
	!	.lm +5
	!	The ^*Product Price Scan\* screen accesses
	!	routines where records for new or additional standard prices are
	!	entered and maintained.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_MAIN_PRICESCAN/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PC_MAIN_PRICESCAN
	!	$ DELETE PC_MAIN_PRICESCAN.OBJ;*
	!
	! Author:
	!
	!	05/18/88 - Frank Starman
	!
	! Modification history:
	!
	!	02/26/92 - Kevin Handy
	!		Removed references to file PD_PACK that Frank
	!		deleted so programs couldn't compile.
	!
	!	04/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!		Remove commented out code.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
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

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP (PC_PRICE)		PC_PRICE_CDD	PC_PRICE
	MAP (PC_PRICE_OLD)	PC_PRICE_CDD	PC_PRICE_OLD, PC_PRICE2

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.HB"
	MAP (PC_PRCTYPE)	PC_PRCTYPE_CDD	PC_PRCTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PC_PRICE) &
		PC_PRICE.CH%, &
		PC_PRICE.READONLY%

	!
	! External functions
	!
	EXTERNAL	LONG    FUNCTION MAIN_WINDOW, FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Product Price"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "PC_MAIN_PRICESCAN"
		SMG_WINDOW::HSIZE = 68%
		SMG_WINDOW::VSIZE = 14%
		SMG_WINDOW::HPOS  = 8%
		SMG_WINDOW::VPOS  = 4%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 7%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Product_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "price_Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 3%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
			SMG_WINDOW::KFIELD(1%, 3%) = 2%

		SMG_WINDOW::HVIEW = 68%
		SMG_WINDOW::VVIEW = 14%
		SMG_WINDOW::VHPOS = 8%
		SMG_WINDOW::VVPOS = 4%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PC_PRICE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PC_PRICE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PC_MAIN_PRICESCAN = ERR
			CONTINUE 770
		END WHEN

		PC_PRICE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.OPN"
		USE
			PC_MAIN_PRICESCAN = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PC_PRICE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PC_PRICE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PC_PRICE.CH%
		WHEN ERROR IN
			RESET #PC_PRICE.CH%
			GET #PC_PRICE.CH%, REGARDLESS
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


		DATA	03,05, "(01) Product #", &
			04,05, "(02) Location #", &
			05,05, "(03) Price Type", &
			06,05, "(04) Date", &
			07,05, "(05) Time", &
			08,05, "(06) Price/Pack", &
			09,05, "(07) Price/Unit", &
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
	!	enters an assigned number which identifies a specific products
	!	pricing.
	!	.b
	!	The field will accept up to fourteen characters.
	!	.lm -5
	!
	! Index:
	!
	!--

			PC_PRICE::PRODUCT_NUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;25", TEMP$, &
				PC_PRICE::PRODUCT_NUM, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					PC_PRICE::PRODUCT_NUM = &
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
	!	The ^*Location _#\* field enters a location
	!	code which is established in the Company Profile file,
	!	located in the Utility system.
	!	.b
	!	Valid Location codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--

			PC_PRICE::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;25", TEMP$, &
				PC_PRICE::LOCATION, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					PC_PRICE::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Price Type\*
	!	.b
	!	.lm +5
	!	The ^*Price Type\* field enters a valid price
	!	type code as established in the Price Type description file.
	!	.b
	!	Valid Price Types may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--

			PC_PRICE::PCTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;25", TEMP$, &
				PC_PRICE::PCTYPE, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "V0 ") = 1%
				THEN
					PC_PRICE::PCTYPE = PC_PRCTYPE::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field enters the effective date
	!	of this price, at this particular location.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--

			PC_PRICE::XDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;25", TEMP$, &
				PC_PRICE::XDATE, MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Time\*
	!	.b
	!	.lm +5
	!	The ^*Time\* field enters the time of day
	!	for which this particular price is to become effective.
	!	.b
	!	The entry is to be made in military time, i.e. 8:00 a.m.
	!	would be entered as 8:00,  2:30 p.m. would be entered as
	!	14:30.
	!	.b
	!	The format for entry is HHMMSS.
	!	.lm -5
	!
	! Index:
	!
	!--

			PC_PRICE::XTIME = ENTR_3TIME(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;25", TEMP$, &
				PC_PRICE::XTIME, MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Price/Pack\*
	!	.b
	!	.lm +5
	!	The ^*Price/Pack\* field enters the price
	!	of a particular product.
	!	.b
	!	If a unique product is available in different packs, i.e.
	!	"case", "each", etc., that price is entered in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

			AMOUNT_CASE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;25",TEMP$, &
				AMOUNT_CASE, MFLAG, "#,###,###.###", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Price/Unit\*
	!	.b
	!	.lm +5
	!	The ^*Price/Unit\* field enters the price of
	!	a particular unit.
	!	.b
	!	If a unique product is available in a particular unit of
	!	measure, i.e. "gallon", "piece", "yard", etc., that price is
	!	entered in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

			PC_PRICE::PRICECOST = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"09;25",TEMP$, PC_PRICE::PRICECOST, MFLAG, &
				"#,###,###.######", MVALUE)

		END SELECT


		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PC_MAIN_PRICESCAN = 0%

		SELECT MLOOP

		CASE 1%
			PD_PRODUCT::UOM = &
				STRING$(LEN(PD_PRODUCT::UOM), A"?"B)

			!
			! Is the product number defined?
			!
			PC_MAIN_PRICESCAN = FUNC_TESTENTRY(SMG_WINDOW, &
				PC_PRICE::PRODUCT_NUM, &
				PD_PRODUCT::DESCRIPTION, &
				"PC", MLOOP, "PRODUC", &
				"Product number", &
				PD_MAIN_PRODUCT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 20%), &
				3%, 43%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::UOM, 9%, 43%, , SMG$M_BOLD)

		CASE 2%
			!
			! Is the location defined?
			!
			PC_MAIN_PRICESCAN = FUNC_TESTENTRY(SMG_WINDOW, &
				PC_PRICE::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"PC", MLOOP, "STORE", &
				"Location", &
				UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(UTL_LOCATION::LOCNAME, 20%), &
				4%, 43%, , SMG$M_BOLD)

		CASE 3%
			!
			! Is the input defined?
			!
			PC_MAIN_PRICESCAN = FUNC_TESTENTRY(SMG_WINDOW, &
				PC_PRICE::PCTYPE, PC_PRCTYPE::DESCRIPTION, &
				"PC", MLOOP, "PRCTYPE", "Type", &
				PC_MAIN_PRCTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PC_PRCTYPE::DESCRIPTION, &
				5%, 43%, , SMG$M_BOLD)

		END SELECT


	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
				"Q0" + PC_PRICE::PRODUCT_NUM) <> 1%
			THEN
				PD_PRODUCT::DESCRIPTION = &
					STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)
				PD_PRODUCT::UOM = &
					STRING$(LEN(PD_PRODUCT::UOM), A"?"B)
				PD_PRODUCT::PACK = &
					STRING$(LEN(PD_PRODUCT::PACK), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 20%), &
				3%, 43%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = &
				STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"Q0" + PC_PRICE::LOCATION) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(UTL_LOCATION::LOCNAME, 20%), &
				4%, 43%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			PC_PRCTYPE::DESCRIPTION = &
				STRING$(LEN(PC_PRCTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, &
				"Q0" + PC_PRICE::PCTYPE) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PC_PRCTYPE::DESCRIPTION, 5%, 43%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(7%) AND 2%) = 0%
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::UOM, 9%, 43%, , SMG$M_BOLD)
		END IF
	!
	! Set PC_PRICE_OLD value
	!
20500	CASE OPT_SETOLD
		PC_PRICE_OLD = PC_PRICE

	!
	! Restore PC_PRICE_OLD value
	!
	CASE OPT_RESETOLD
		PC_PRICE = PC_PRICE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PC_PRICE2 = PC_PRICE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PC_PRICE = PC_PRICE2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Product#       Location " + &
				"PriceType Date     Time                 Price"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "017,026,036,045,054"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PC_PRICE::PRODUCT_NUM + " " + &
				PC_PRICE::LOCATION + "     " + &
				PC_PRICE::PCTYPE + "        " + &
				PRNT_DATE(PC_PRICE::XDATE, 0%) + " " + &
				PRNT_TIME(PC_PRICE::XTIME, 8%) + "  " + &
				FORMAT$(PC_PRICE::PRICECOST, "#,###,###.######")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PC_PRICE::PRODUCT_NUM + &
				PC_PRICE::LOCATION, &
				REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE PC_PRICE::PCTYPE + &
				PC_PRICE::PRODUCT_NUM + &
				PC_PRICE::LOCATION, &
				REGARDLESS
		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
