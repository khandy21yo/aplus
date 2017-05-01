1	%TITLE "Product Cost Layer Maintenance"
	%SBTTL "IC_MAIN_LAYER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_MAIN_LAYER(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	.lm -5
	!
	! Index:
	!	.x Product Layers
	!	.x Archive>Product Layers
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAIN_LAYER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_MAIN_LAYER
	!	$ DELETE IC_MAIN_LAYER.OBJ;*
	!
	! Author:
	!
	!	06/23/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/25/92 - Kevin Handy
	!		Removed references to function PD_MAST_PACK that Frank
	!		deleted so programs couldn't compile.
	!
	!	04/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/24/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/27/97 - Kevin Handy
	!		use integer for #key
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

	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_LAYER.HB"
	MAP (IC_LAYER)		IC_LAYER_CDD		IC_LAYER
	MAP (IC_LAYER_OLD)	IC_LAYER_CDD		IC_LAYER_OLD, IC_LAYER2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_IC_LAYER) &
		IC_LAYER.CH%, &
		IC_LAYER.READONLY%

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
		! Define window
		!
		SMG_WINDOW::DESCR = "Product Cost Layer"
		SMG_WINDOW::NHELP = "IC_MAIN_LAYER"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 9%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Product_num"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		IC_LAYER::BATCH = ""

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF IC_LAYER.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF IC_LAYER.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_LAYER.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			IC_MAIN_LAYER = ERR
			CONTINUE 770
		END WHEN

		IC_LAYER.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_LAYER.OPN"
		USE
			IC_MAIN_LAYER = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		IC_LAYER.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(IC_LAYER.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = IC_LAYER.CH%
		WHEN ERROR IN
			RESET #IC_LAYER.CH%
			GET #IC_LAYER.CH%, REGARDLESS
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


		DATA	04,05, "(01) Product #", &
			05,05, "(02) Location #", &
			06,05, "(03) Date", &
			07,05, "(04) Cost/Pack", &
			08,05, "(05) Cost/Unit", &
			09,05, "(06) Quantity In", &
			10,05, "(07) Check Number", &
			11,05, "(08) Vendor Name", &
			12,05, "(09) Invoice Number", &
			14,05, "     Post Date", &
			15,05, "     Post Time", &
			16,05, "     Post Batch", &
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
	!	The ^*Product _#\* field enters the
	!	assigned number which identifies this specific product.
	!	.b
	!	Valid Product _#'s may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--

			IC_LAYER::PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"04;25", TEMP$, &
				IC_LAYER::PRODUCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
				IC_LAYER::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter
			END IF

			PD_PRODUCT::DESCRIPTION = STRING$(35%, A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
					"Q0" + IC_LAYER::PRODUCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 35%), &
				4%, 43%, , SMG$M_BOLD)

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
	!	The field will accommodate up to four (4) alphanumeric
	!	characters.
	!	.b
	!	Valid Location codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--

			IC_LAYER::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;25", TEMP$, &
				IC_LAYER::LOCATION, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
				IC_LAYER::LOCATION = &
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
	!	The ^*Date\* field enters the effective date for
	!	a particular product cost.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--

			IC_LAYER::TRANSDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;25", TEMP$, &
				IC_LAYER::TRANSDATE, MFLAG, "'E", &
				MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Cost/Pack\*
	!	.b
	!	.lm +5
	!	The ^*Cost/Pack\* field enters the cost
	!	of a particular product.
	!	.b
	!	If a unique product is available in different packs, i.e.
	!	"case", "each", etc., that cost is entered in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

			PRODUCT_FACTOR = 1.0

			AMOUNT_CASE = IC_LAYER::COST * PRODUCT_FACTOR

			AMOUNT_CASE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;25",TEMP$, &
				AMOUNT_CASE, MFLAG, &
				"#,###,###.##", MVALUE)

			IC_LAYER::COST = AMOUNT_CASE / PRODUCT_FACTOR &
				IF PRODUCT_FACTOR <> 0.0

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Cost/Unit\*
	!	.b
	!	.lm +5
	!	The ^*Cost/Unit\* field enters the cost of
	!	a particular unit.
	!	.b
	!	If a unique product is available in a particular unit
	!	of measure, i.e. "gallon", "piece", "yard", that cost
	!	is entered in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

			IC_LAYER::COST = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;25",TEMP$, IC_LAYER::COST, MFLAG, &
				"#,###,###.######", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Quantity In\*
	!	.b
	!	.lm +5
	!	The ^*Quantity In\* field enters the quantity of items
	!	contained in the specified ledger.
	!	.lm -5
	!
	! Index:
	!
	!--

			IC_LAYER::QUANTITY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;25",TEMP$, IC_LAYER::QUANTITY, MFLAG, &
				"#,###,###.###", MVALUE)

	!++
	! Abstract:FLD007
	!	^*(07) Check Number\*
	!	.b
	!	.lm +5
	!	The ^*Check Number\* field enters the number of the check
	!	used to pay for the specified product layer.
	!	.lm -5
	!
	! Index:
	!	.x Check Number
	!	.x Number>Check
	!
	!--
		CASE 7%

			IC_LAYER::CHECK = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;25", TEMP$, &
				IC_LAYER::CHECK, MFLAG, "'E", &
				MVALUE)


	!++
	! Abstract:FLD008
	!	^*(08) Vendor Name\*
	!	.b
	!	.lm +5
	!	The ^*Vendor Name\* field enters the name of the vendor
	!	from which the product layer was purchased.
	!	.lm -5
	!
	! Index:
	!	.x Vendor Name
	!
	!--
		CASE 8%
			IC_LAYER::VENDORALF = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;25", TEMP$, &
				IC_LAYER::VENDORALF, MFLAG, "'E", &
				MVALUE)


	!++
	! Abstract:FLD009
	!	^*(09) Invoice Number\*
	!	.b
	!	.lm +5
	!	The ^*Invoice Number\* field enters the number of the
	!	invoice accompanying the product layer.
	!	.lm -5
	!
	! Index:
	!	.x Invoice Number
	!	.x Number>Invoice
	!
	!--
		CASE 9%

			IC_LAYER::INVOICE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;25", TEMP$, &
				IC_LAYER::INVOICE, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

			IF IC_LAYER::PRODUCT <> ""
			THEN
				ACCTDESCR$ = &
					STRING$(LEN(PD_PRODUCT::DESCRIPTION), &
					A"?"B)
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "Q0" + &
					IC_LAYER::PRODUCT) = 1%
				THEN
					ACCTDESCR$ = PD_PRODUCT::DESCRIPTION
				END IF

				ACCTDESCR$ = ENTR_3STRING(SCOPE, &
					SMG_WINDOW::WNUMBER, "4;43", TEMP$, &
					LEFT(ACCTDESCR$, 35%), 1%, "'E", &
					MVALUE)
			END IF


	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		IC_MAIN_LAYER = 0%

		SELECT MLOOP

		CASE 1%
			IF IC_LAYER::PRODUCT = ""
			THEN
				IC_MAIN_LAYER = 1%
			ELSE
				!
				! Is the input defined?
				!
				IC_MAIN_LAYER = FUNC_TESTENTRY(SMG_WINDOW, &
				IC_LAYER::PRODUCT, PD_PRODUCT::DESCRIPTION, &
				"IC", MLOOP, "PROD", &
				"Product", PD_MAIN_PRODUCT.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 35%), &
				4%, 43%, ,SMG$M_BOLD)

		CASE 2%
			IF IC_LAYER::LOCATION = ""
			THEN
				IC_MAIN_LAYER = 1%
			ELSE
				!
				! Is the input defined?
				!
				IC_MAIN_LAYER = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_LAYER::LOCATION, UTL_LOCATION::LOCNAME, &
					"IC", MLOOP, "PROD", &
					"Location", UTL_MAIN_LOCATION.ID)
			END IF

		END SELECT

	CASE OPT_DISPLAY

		!
		! Display name (ALSO IN TESTENTRY)
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(DISPLAYNAME$, 35%), 4%, 43%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			PRNT_DATE(IC_LAYER::POSTDATE, 8%), 14%, 25%, , &
			SMG$M_BOLD)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			PRNT_TIME(IC_LAYER::POSTTIME, 1%), 15%, 25%, , &
			SMG$M_BOLD)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			IC_LAYER::BATCH, 16%, 25%, , SMG$M_BOLD)

	!
	! Set IC_LAYER_OLD value
	!
20500	CASE OPT_SETOLD
		IC_LAYER_OLD = IC_LAYER

	!
	! Restore IC_LAYER_OLD value
	!
	CASE OPT_RESETOLD
		IC_LAYER = IC_LAYER_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		IC_LAYER2 = IC_LAYER

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		IC_LAYER = IC_LAYER2

		IC_LAYER::POSTDATE = DATE_TODAY
		IC_LAYER::POSTTIME = TIME_NOW
		IC_LAYER::BATCH    = "??????"

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Product#       Loc  Date     " + &
				"           Cost      Quantity"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "017,022,031,047"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = IC_LAYER::PRODUCT + " " + &
				IC_LAYER::LOCATION + " " + &
				IC_LAYER::TRANSDATE + " " + &
				FORMAT$(IC_LAYER::COST, &
					"#,###,###.#####") + " " + &
				FORMAT$(IC_LAYER::QUANTITY, &
					"#,###,###.###") &


			END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE IC_LAYER::PRODUCT+ &
				IC_LAYER::LOCATION+ &
				IC_LAYER::TRANSDATE, &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
