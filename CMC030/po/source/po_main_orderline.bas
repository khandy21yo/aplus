1	%TITLE "Purchase Order Journal Line Maintenance"
	%SBTTL "PO_MAIN_ORDERLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_MAIN_ORDERLINE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	.p
	!	The ^*Line-Items\* option will provide a screen
	!
	! Index:
	!	.x Purchase Orders>Line Items
	!	.x Line Items>Purchase Orders
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAIN_ORDERLINE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_MAIN_ORDERLINE
	!	$ DELETE PO_MAIN_ORDERLINE.OBJ;*
	!
	! Author:
	!
	!	06/19/90 - Aaron Redd
	!
	! Modification history:
	!
	!	07/16/90 - Kevin Handy
	!		Modified to look into register.
	!
	!	11/16/90 - Kevin Handy
	!		Changed vendor quanity from ##.### to ####.#
	!
	!	09/11/91 - Kevin Handy
	!		Allowed blank vendor product numbers again after
	!		Frank's undocumented changes.
	!
	!	12/23/91 - Kevin Handy
	!		Modified to look into own folder to see if line
	!		exists, and to fill in several blanks if it does.
	!
	!	12/23/91 - Kevin Handy
	!		Modified to default factors to 1 instead of 0.
	!
	!	01/22/92 - Dan Perkins
	!		Eliminated fields 3,5,6,8,10 which will no
	!		longer be used.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	02/11/92 - Dan Perkins
	!		More file layout changes.
	!
	!	03/17/92 - Dan Perkins
	!		Added formatting option to VEN_PRICE field.
	!
	!	04/01/92 - Dan Perkins
	!		Read Vendor Price form product cost file.  Fixed
	!		FIND so it would work.  Don't stop on product
	!		DESCRIPTION or UOM fields if the product is defined.
	!
	!	04/03/92 - Dan Perkins
	!		Don't display prod description or UOM if product
	!		is undefined.
	!		NOTE:	SCOPE.EXIT = 0% is commented out to allow use
	!			of UP and DOWN arrow keys.  If problems with
	!			SCOPE.EXIT% occurr, uncomment this line.
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	06/10/92 - Dan Perkins
	!		Fixed problem finding Vendor Part Number.
	!
	!	11/16/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/14/93 - Dan Perkins
	!		Compensate for undefined unit of measure if
	!		a good product is entered without a valid UOM.
	!
	!	04/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/04/94 - Kevin Handy
	!		Added parameter to PO_READ_REGLINE.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/22/95 - Kevin Handy
	!		Format source closer to 80 columns.
	!
	!	06/22/95 - Kevin Handy
	!		Modified calculation of vendor price to handle any
	!		our/vendor factors, then disabled it due to the fact
	!		that VEN_PRICE is really OUR_PRICE.
	!
	!	06/22/95 - Kevin Handy
	!		Modified to recursively call self to re-display
	!		information instead of trying to change all format
	!		routines to display consistantly
	!
	!	06/12/96 - Kevin Handy
	!		Reformat source code.
	!		Lose lots of commented out code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	10/28/97 - Kevin Handy
	!		Allow entry of product description even if a part
	!		number is entered.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excess %PAGE's
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	06/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/10/2002 - Kevin Handy
	!		Fix view so it has a chance of working.
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.HB"
	MAP (PO_ORDERJOUR)	PO_ORDERJOUR_CDD	PO_ORDERJOUR

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.HB"
	MAP (PO_ORDERLINE)	PO_ORDERLINE_CDD	PO_ORDERLINE
	MAP (PO_ORDERLINE_OLD)	PO_ORDERLINE_CDD	PO_ORDERLINE_OLD, &
							PO_ORDERLINE2, &
							PO_ORDERLINE_HOLD

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP (PO_PARTCROSS)	PO_PARTCROSS_CDD	PO_PARTCROSS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_MEASURE.HB"
	MAP (UTL_MEASURE)	UTL_MEASURE_CDD		UTL_MEASURE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE
	DECLARE			PO_REG_LINE_CDD		PO_REG_LINE_READ

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	DECLARE			PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	DECLARE			PC_COST_CDD		PC_COST_READ

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PO_ORDERJOUR) &
		BATCH_NO$ = 2%, &
		PO_ORDERJOUR.CH%, &
		PO_ORDERJOUR.READONLY%

	COM (CH_PO_ORDERLINE) &
		PO_ORDERLINE.CH%, &
		PO_ORDERLINE.READONLY%

	COM (CH_UOM_FLAG) &
		UOM_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG   FUNCTION PO_READ_REG_LINE
	EXTERNAL REAL	FUNCTION PC_READ_COST

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	SELECT MOPTION

	!**********************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!**********************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR  = "PO Order Line Items"
		SMG_WINDOW::NHELP  = "PO_MAIN_ORDERLINE"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::HSIZE  = 74%
		SMG_WINDOW::VSIZE  =  6%
		SMG_WINDOW::HPOS   =  3%
		SMG_WINDOW::VPOS   =  8%
		SMG_WINDOW::NITEMS =  6%
		SMG_WINDOW::FLAGS  =  0%
		SMG_WINDOW::HVIEW  = 74%
		SMG_WINDOW::VVIEW  =  6%
		SMG_WINDOW::VHPOS  =  3%
		SMG_WINDOW::VVPOS  =  8%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Line"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		COM (FRM) FRM$(6%)

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PO_ORDERLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PO_ORDERLINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PO_MAIN_ORDERLINE = ERR
			CONTINUE 770
		END WHEN

		PO_ORDERLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.OPN"
		USE
			PO_MAIN_ORDERLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PO_ORDERLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PO_ORDERLINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PO_ORDERLINE.CH%
		WHEN ERROR IN
			RESET #PO_ORDERLINE.CH%
			GET #PO_ORDERLINE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!**********************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!**********************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	1, 2, "(01) Line#", &
			2, 2, "(02) Product", &
			3, 2, "(03) Description", &
			4, 2, "(04) Unit of Measure", &
			5, 2, "(05) Vendor Product", &
			6, 2, "(06) Price", &
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

	!**********************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!**********************************************************************
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	!SCOPE.EXIT% = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Line Number\*
	!	.b
	!	.lm +5
	!	The ^*Line Number\* field is used to identify each line item in the body of a
	!	Purchase Order.  This number is a key number used to record receipts in the
	!	Receiving Journal and to record invoices in the Accounts Payable Purchases
	!	Journal.  Consequently, the system tracks products ordered, received, and
	!	invoiced.
	!	.b
	!	This four (4) character Line Number is automatically assigned by the system.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Line Items>Line Number
	!	.x Line Number>Purchase Order Journal Line Items
	!
	!--
			IF TEMP$ = "Add" AND (MFLAG AND 1%) = 0%
			THEN
				GOSUB 28500
			END IF

			PO_ORDERLINE::PO_LINE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;24", TEMP$, &
				PO_ORDERLINE::PO_LINE, MFLAG, "~L0'LLL", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(PO_MAIN_REGLINE.ID, "VX  ") = 1%
				THEN
					PO_ORDERLINE::PO = PO_REG_LINE::PO_LINE
				END IF
				GOTO E0Loop
			END SELECT

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Product Number\*
	!	.b
	!	.lm +5
	!	The ^*Product Number\* field records the stock number used by the user to
	!	identify the item being ordered. If no part number is specified here, this
	!	line will not be interfaced into the Inventory System.
	!	.b
	!	Pressing the ^*List Choices\* key will cause the defined
	!	Product Numbers to be displayed.
	!	.b
	!	This field accepts up to fourteen (14) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Line Items>Product Number
	!	.x Product Number>Purchase Order Journal Line Items
	!
	!--
			PO_ORDERLINE::OUR_PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;24", TEMP$, &
				PO_ORDERLINE::OUR_PRODUCT, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					PO_ORDERLINE::OUR_PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO E0Loop
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field describes the item being ordered.  The description
	!	is automatically displayed if the stock number for the item is identified, i.e.
	!	the item is recorded in the Inventory Control system.  If the item is not
	!	identified, the user may manually enter a description in this field.
	!	.b
	!	This field will accommodate up to forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Line Items>Item Description
	!	.x Item Description>Purchase Order Journal Line Items
	!
	!--
 !			MFLAG = MFLAG OR 1% IF PO_ORDERLINE::OUR_PRODUCT <> ""

			PO_ORDERLINE::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;24", TEMP$, &
				PO_ORDERLINE::DESCRIPTION, MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Unit Of Measure\*
	!	.b
	!	.lm +5
	!	The ^*Unit Of Measure\* field should contain the unit of measure used by
	!	the user relative to the item being ordered.
	!	.p
	!	This field will default to the unit of measure defined in the
	!	product description file and automatically display that default.
	!	.p
	!	This field will contain two (2) alphanumeric characters.
	!
	! Index:
	!	.x Purchase Order Journal>Line Items>User's Unit of Measure
	!	.x User's Unit of Measure>Purchase Order Journal Line Items
	!	.x Purchase Order Journal>Line Items>Unit of Measure
	!	.x Unit of Measure>Purchase Order Journal Line Items
	!
	!--
			MFLAG = MFLAG OR 1% IF PO_ORDERLINE::OUR_PRODUCT <> "" &
				AND UOM_FLAG% = 0%

			PO_ORDERLINE::OUR_UOM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;24", TEMP$, &
				PO_ORDERLINE::OUR_UOM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "VX") = 1%
				THEN
					PO_ORDERLINE::OUR_UOM = &
						UTL_MEASURE::CODE
				END IF
				GOTO E0Loop
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Vendor Product Number\*
	!	.b
	!	.lm +5
	!	The ^*Vendor Product Number\* field records the stock number
	!	used by the vendor to identify the item being ordered.
	!	.b
	!	Pressing the ^*List Choices\* key will cause the defined
	!	Vendor Product Numbers to be displayed.
	!	.b
	!	This field accepts up to fourteen (14) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Line Items>Product Number
	!	.x Product Number>Purchase Order Journal Line Items
	!
	!--
			PO_ORDERLINE::VEN_PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;24", TEMP$, &
				PO_ORDERLINE::VEN_PRODUCT, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PO_MAIN_PRODCROSS.ID, "V1" + &
					PO_ORDERJOUR::VENDOR + &
					PO_ORDERLINE::OUR_PRODUCT) = 1%
				THEN
					PO_ORDERLINE::VEN_PRODUCT = &
						PO_PARTCROSS::VENPROD
				END IF
				GOTO E0Loop
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Vendor Price\*
	!	.b
	!	.lm +5
	!	The ^*Vendor's Price\* field enters the price of the
	!	item being ordered, expressed in reference to the vendor's unit of measure.
	!	.b
	!	This field will accept a number as large as 99,999.999
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Line Items>Expected Price
	!	.x Expected Price>Purchase Order Journal Line Items
	!
	!--
			PO_ORDERLINE::VEN_PRICE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;24", TEMP$, &
				PO_ORDERLINE::VEN_PRICE, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		SELECT MLOOP

		CASE 1%
			IF PO_ORDERLINE::PO = ""
			THEN
				PO_MAIN_ORDERLINE = 1%
				GOTO 32767
			END IF

			IF MVALUE = "ADD"
			THEN
				WHEN ERROR IN
					GET #PO_ORDERLINE.CH%, &
						KEY #0% EQ PO_ORDERLINE::PO + &
						PO_ORDERLINE::PO_LINE, &
						REGARDLESS
				USE
					CONTINUE 20310 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				PO_MAIN_ORDERLINE = 1%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Line already in use", 1%)
				GOTO 28000
			END IF


20310			IF MVALUE = "ADD"
			THEN
			!
			! Check if this order number already in register file
			!
				IF PO_READ_REG_LINE(PO_ORDERLINE::PO, &
					PO_ORDERLINE::PO_LINE, &
					"EQ", PO_REG_LINE_READ, &
					PO_REG_SUB_LINE_READ, QTY(), &
					"") = CMC$_NORMAL
				THEN
					CALL ENTR_3MESSAGE(SCOPE, &
						"Line Already Exists in Register", 1%)

					PO_ORDERLINE::PO          = &
						PO_REG_LINE_READ::PO
					PO_ORDERLINE::PO_LINE     = &
						PO_REG_LINE_READ::PO_LINE
					PO_ORDERLINE::OUR_PRODUCT = &
						PO_REG_LINE_READ::PRODUCT
					PO_ORDERLINE::VEN_PRICE   = &
						PO_REG_SUB_LINE_READ::PRICE

					V% = PO_MAIN_ORDERLINE(SMG_WINDOW, &
						OPT_ENTRY, I%, 1%, "") &
						FOR I% = 1% TO 6%


				END IF
			END IF

		CASE 2%
			!
			! Is the input defined?
			!
			IF PO_ORDERLINE::OUR_PRODUCT <> ""
			THEN
				ST%, PO_MAIN_ORDERLINE = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					PO_ORDERLINE::OUR_PRODUCT, &
					PD_PRODUCT::DESCRIPTION, &
					"PD", MLOOP, "PRODUCT", &
					"Product number", PD_MAIN_PRODUCT.ID)

				PO_ORDERLINE::DESCRIPTION = &
					PD_PRODUCT::DESCRIPTION

				!
				! If we're adding and the product is defined,
				! then load in and display defaults for the
				! description, UOM, and cost if known.
				!
				IF ST% = 0%
				THEN
					PO_ORDERLINE::OUR_UOM = PD_PRODUCT::UOM
					UOM_FLAG% = 0%

					PO_ORDERLINE::VEN_PRICE = &
						PC_READ_COST( &
						PO_ORDERLINE::OUR_PRODUCT, &
						PO_ORDERJOUR::FROMLOCATION, &
						PO_ORDERJOUR::PODATE, "")

					IF MAIN_WINDOW(PO_MAIN_PRODCROSS.ID, &
						"Q1" + PO_ORDERJOUR::VENDOR + &
						PO_ORDERLINE::OUR_PRODUCT) = 1%
					THEN
						PO_ORDERLINE::VEN_PRODUCT = &
							PO_PARTCROSS::VENPROD

					END IF

					V% = PO_MAIN_ORDERLINE(SMG_WINDOW, &
						OPT_ENTRY, I%, 1%, "") &
						FOR I% = 1% TO 6%

				END IF
			END IF

		CASE 3%
			IF PO_ORDERLINE::DESCRIPTION = "" AND &
				PO_ORDERLINE::OUR_PRODUCT = ""
			THEN
				PO_MAIN_ORDERLINE = 1%
			END IF

		CASE 4%
			UOM_FLAG%, PO_MAIN_ORDERLINE = &
				FUNC_TESTENTRY(SMG_WINDOW, &
				PO_ORDERLINE::OUR_UOM, UTL_MEASURE::CODE, &
				"PO", MLOOP, "UOM", &
				"Unit of Measure", UTL_MAIN_MEASURE.ID)

		END SELECT

	!
	! Set PO_ORDERLINE_OLD value
	!
20500	CASE OPT_SETOLD
		PO_ORDERLINE_OLD = PO_ORDERLINE

	!
	! Restore PO_ORDERLINE_OLD value
	!
	CASE OPT_RESETOLD
		PO_ORDERLINE = PO_ORDERLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PO_ORDERLINE2 = PO_ORDERLINE

		IF MFLAG = 1%
		THEN
			SELECT MLOOP

			CASE 0%
				FRM$(6%) = "#,###,###.##"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PO_ORDERLINE = PO_ORDERLINE2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PO_ORDERLINE::PO = MVALUE

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #PO_ORDERLINE.CH%, &
				KEY #0% GE PO_ORDERLINE::PO + &
				PO_ORDERLINE::PO_LINE, REGARDLESS

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
			MVALUE = "  Line Product        Description" + &
				"          UOM " + &
				"VenProduct            Price"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,022,043,047,062"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PO_ORDERLINE::PO_LINE + " " + &
				PO_ORDERLINE::OUR_PRODUCT + " " + &
				LEFT(PO_ORDERLINE::DESCRIPTION, 20%) + " " + &
				PO_ORDERLINE::OUR_UOM + "  " + &
				PO_ORDERLINE::VEN_PRODUCT + " " + &
				FORMAT$(PO_ORDERLINE::VEN_PRICE, "######.##")
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
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% EQ MVALUE, &
					REGARDLESS
				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
			USE
				IF ERR = 154%
				THEN
					SLEEP 1%
					RETRY
				END IF

				CONTINUE 28000
			END WHEN

			SMG_WINDOW::CURREC = 0%

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
					FIND #PO_ORDERLINE.CH%, &
						KEY #0% GE MVALUE + &
						PO_ORDERLINE::PO_LINE, &
						REGARDLESS
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
			IF PO_ORDERLINE::PO = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			PO_ORDERLINE::PO = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

28500	!
	! Assign new line number
	!
	WORK_ITEM$ = "0"
	PO_ORDERLINE_HOLD = PO_ORDERLINE
	CHECK_PONUM$ = PO_ORDERLINE::PO

	WHEN ERROR IN
		FIND #PO_ORDERLINE.CH%, &
			KEY #0% GE PO_ORDERLINE::PO + "", &
			REGARDLESS
	USE
		CONTINUE 28600
	END WHEN

28510	WHEN ERROR IN
		GET #PO_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE 28600
	END WHEN

	GOTO 28600 IF PO_ORDERLINE::PO <> CHECK_PONUM$

	WORK_ITEM$ = PO_ORDERLINE::PO_LINE

	GOTO 28510

28600	PO_ORDERLINE = PO_ORDERLINE_HOLD

	WHEN ERROR IN
		TEMP% = VAL%(WORK_ITEM$) + 1%
	USE
		TEMP% = 1%
	END WHEN

	PO_ORDERLINE::PO_LINE = FORMAT$(TEMP%, "<0>###")

	RETURN

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
