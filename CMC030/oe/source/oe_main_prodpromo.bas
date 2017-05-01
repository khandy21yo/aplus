1	%TITLE "Product Promotion Price Maintenance"
	%SBTTL "OE_MAIN_PRODPROMO"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_PRODPROMO(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, &
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
	!	The ^*Product Promotional Price Maintenance\* file screen
	!	accesses routines where records for new or additional promotional prices
	!	are entered and maintained.
	!	.lm -5
	!
	! Index:
	!	.x Promotional Price Maintenance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_PRODPROMO/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_MAIN_PRODPROMO
	!	$ DELETE OE_MAIN_PRODPROMO.OBJ;*
	!
	! Author:
	!
	!	12/03/90 - Val James Allen
	!
	! Modification history:
	!
	!	04/10/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	11/25/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	12/01/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSTYPE.HB"
	MAP (AR_CUSTYPE)	AR_CUSTYPE_CDD		AR_CUSTYPE

	%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.HB"
	MAP (OE_CATEGORY)	OE_CATEGORY_CDD		OE_CATEGORY

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_PRODPROMO.HB"
	MAP (OE_PRODPROMO)	OE_PRODPROMO_CDD	OE_PRODPROMO
	MAP (OE_PRODPROMO_OLD)	OE_PRODPROMO_CDD	OE_PRODPROMO_OLD
	MAP (OE_PRODPROMO_DEF)	OE_PRODPROMO_CDD	OE_PRODPROMO_DEF

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	MAP (OE_PROMO)		OE_PROMO_CDD		OE_PROMO

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_OE_PRODPROMO) &
		OE_PRODPROMO.CH%, &
		OE_PRODPROMO.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG	FUNCTION AR_MAIN_CUSTYPE
	EXTERNAL LONG	FUNCTION OE_MAIN_CATEGORY
	EXTERNAL LONG	FUNCTION SA_MAIN_SALESMAN
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

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

		!************************************************************
		! Set up information
		!************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Product Promotional Price Maintenance"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "OE_MAIN_PRODPROMO"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 11%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 8%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 7%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Promo"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 8%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF OE_PRODPROMO.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_PRODPROMO.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_PRODPROMO.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			OE_MAIN_PRODPROMO = ERR
			CONTINUE 770
		END WHEN

		OE_PRODPROMO.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_PRODPROMO.OPN"
		USE
			OE_MAIN_PRODPROMO = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_PRODPROMO.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_PRODPROMO.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = OE_PRODPROMO.CH%
		WHEN ERROR IN
			RESET #OE_PRODPROMO.CH%
			GET #OE_PRODPROMO.CH%, REGARDLESS
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

		DATA	03,05, "(01) Promo #", &
			04,05, "(02) Customer #", &
			05,05, "(03) Cust Type", &
			06,05, "(04) Cust Catg", &
			07,05, "(05) Salesman", &
			08,05, "(06) Price Off", &
			09,05, "(07) Promo %", &
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
	!	.x Promotion Number>Price Maintenance
	!	^*(01) Promo _#\*
	!	.b
	!	.lm +5
	!	The ^*Promo _#\* field enters a promotion
	!	code which is assigned to each promotional process.
	!	.b
	!	Sixteen spaces are available for the entry.
	!	.b
	!	Valid Promotion _#'s may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Price Maintenance>Promotion Number
	!
	!--

			OE_PRODPROMO::REFPROMO = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;28", TEMP$, OE_PRODPROMO::REFPROMO, &
				MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(OE_MAIN_PROMO.ID, "V0") = 1%
				THEN
					OE_PRODPROMO::REFPROMO = &
						OE_PROMO::REFPROMO
				END IF
				GOTO Reenter
			END IF


		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Customer Number>Price Promotion Maintenance
	!	^*(02) Customer Number\*
	!	.lm +5
	!	.b
	!	The ^*Customer Number\* field enters a valid customer
	!	code as established in the Customer master file.
	!	.b
	!	Ten spaces are available for the entry.
	!	.b
	!	Valid Customer Number codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Price Promotion Maintenance>Customer Number
	!	.x Customer>Price Promotion
	!
	!--


			OE_PRODPROMO::CUSTOMER = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;28", TEMP$, &
				OE_PRODPROMO::CUSTOMER, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "V0") = 1%
				THEN
					OE_PRODPROMO::CUSTOMER = &
						AR_35CUSTOM::CUSNUM
				END IF
				GOTO Reenter
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Customer Type>Price Promotion Maintenance
	!	^*(03) Customer Type\*
	!	.b
	!	.lm +5
	!	The ^*Customer Type\* field enters the customer
	!	type as applied to this promotion.
	!	.b
	!	Valid customer type codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Price Promotion Maintenance>Customer Type
	!
	!--


			OE_PRODPROMO::CUSTYPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;28", TEMP$, &
				OE_PRODPROMO::CUSTYPE, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AR_MAIN_CUSTYPE.ID, "V0") = 1%
				THEN
					OE_PRODPROMO::CUSTYPE = &
						AR_CUSTYPE::CUSTYPE
				END IF
				GOTO Reenter
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	.x Cust Category>Price Promotion Maintenance
	!	^*(04) Customer Category\*
	!	.b
	!	.lm +5
	!	The ^*Customer Category\* field enters the applicable
	!	customer category code that applies to this promotion.
	!	.b
	!	Valid customer category codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Price Promotion Maintenance>Cust Category
	!
	!--


			OE_PRODPROMO::CUSTCAT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;28", TEMP$, &
				OE_PRODPROMO::CUSTCAT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(OE_MAIN_CATEGORY.ID, "V0") = 1%
				THEN
					OE_PRODPROMO::CUSTCAT = &
						OE_CATEGORY::ORDCAT
				END IF
				GOTO Reenter
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	.x Saleman>Price Promotion Maintenance
	!	^*(05) Salesman\*
	!	.b
	!	.lm +5
	!	The ^*Salesman\* field enters the applicable
	!	salesman number for this particular promotion.
	!	.b
	!	Valid salesman codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Price Promotion Maintenance>Salesman
	!
	!--


			OE_PRODPROMO::SALESMAN = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;28",TEMP$, &
				OE_PRODPROMO::SALESMAN, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "V0") = 1%
				THEN
					OE_PRODPROMO::SALESMAN = &
						SA_SALESMAN::SALESMAN
				END IF
				GOTO Reenter
			END IF

		CASE 6%

	!++
	! Abstract:FLD006
	!	.x Price/Unit>Price Promotion Maintenance
	!	^*(06) Price Off\*
	!	.b
	!	.lm +5
	!	The ^*Price Off\* field enters the dollar
	!	amount of discount to be given during a promotion for this particular
	!	product.
	!	.b
	!	This field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Price Promotion Maintenance>Price/Unit
	!
	!--


			OE_PRODPROMO::PROMODOLL = &
				ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				"08;28",TEMP$, OE_PRODPROMO::PROMODOLL, &
				MFLAG, "#,###,###.######", MVALUE)

			IF OE_PRODPROMO::PROMODOLL <> 0.0
			THEN
				OE_PRODPROMO::PROMOPERC = 0%
			END IF
		CASE 7%

	!++
	! Abstract:FLD007
	!	.x Promo Percentage>Price Promotion Maintenance
	!	^*(07) Promo Percentage\*
	!	.b
	!	.lm +5
	!	The ^*Promo Percentage\* field enters the discount percentage
	!	to be taken off of the standard price of a particular unit for this
	!	promotion only.
	!	.b
	!	^*Note:\* If the promo % off is to be 5%, the entry would be made as 5.00.
	!	.b
	!	^*Note:\* This field cannot be used in conjunction with the discount
	!	dollar amount in field (06).  If field (06) has a value, this field should
	!	be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Price Promotion Maintenance>Promo Percentage
	!
	!--


			OE_PRODPROMO::PROMOPERC = &
				ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				"09;28",TEMP$, OE_PRODPROMO::PROMOPERC, MFLAG, &
				"##.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		OE_MAIN_PRODPROMO = 0%

		SELECT MLOOP

		CASE 1%
			OE_MAIN_PRODPROMO = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_PRODPROMO::REFPROMO, &
				OE_PROMO::DESCRIPTION, &
				"OE", MLOOP, "PROG", &
				"Promotion Number", &
				OE_MAIN_PROMO.ID)

		CASE 7%
			IF OE_PRODPROMO::PROMODOLL <> 0.0 AND &
				OE_PRODPROMO::PROMOPERC <> 0.0
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Dollars or percent off only - not both", 1%)
				OE_PRODPROMO::PROMOPERC = 0%
				OE_MAIN_PRODPROMO = 1%
			END IF


		END SELECT

	CASE OPT_DISPLAY

	!
	! Set OE_PRODPROMO_OLD value
	!
20500	CASE OPT_SETOLD
		OE_PRODPROMO_OLD = OE_PRODPROMO

	!
	! Restore OE_PRODPROMO_OLD value
	!
	CASE OPT_RESETOLD
		OE_PRODPROMO = OE_PRODPROMO_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_PRODPROMO_DEF = OE_PRODPROMO

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_PRODPROMO = OE_PRODPROMO_DEF
		OE_PRODPROMO::PRODUCT = MVALUE

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Promotion Number CustNumber Type Cate " + &
				"Salesman   DollarAmount Promo %"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "019,030,035,040,051,064"

		!
		! Convert current record into text
		!
		CASE 3%

			MVALUE = OE_PRODPROMO::REFPROMO + " " + &
				OE_PRODPROMO::CUSTOMER  + " " + &
				OE_PRODPROMO::CUSTYPE   + "   " + &
				OE_PRODPROMO::CUSTCAT   + " " + &
				OE_PRODPROMO::SALESMAN  + " " + &
				FORMAT$(OE_PRODPROMO::PROMODOLL, "#######.####") + "   " + &
				FORMAT$(OE_PRODPROMO::PROMOPERC, "##.##")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE OE_PRODPROMO::PRODUCT + &
				OE_PRODPROMO::REFPROMO, REGARDLESS

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
				FIND #SMG_WINDOW::CHAN, KEY #0% EQ MVALUE, &
					REGARDLESS

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
						KEY #0% GE MVALUE + &
						OE_PRODPROMO::REFPROMO, REGARDLESS
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

			IF OE_PRODPROMO::PRODUCT = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			OE_PRODPROMO::PRODUCT = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
