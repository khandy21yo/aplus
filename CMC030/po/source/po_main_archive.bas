1	%TITLE "Maintain Purchase Order Archive"
	%SBTTL "PO_MAIN_ARCHIVE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_MAIN_ARCHIVE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.b
	!	.lm +5
	!	The ^*Maintain Purchase Order Register\* option
	!	maintains the PO register.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order>Maintenance
	!	.x Maintenance>Purchase Orders
	!	.x Register>Purchase Orders
	!	.x Purchase Order>Register
	!
	! Option:
	!
	!	PO_MAIN_ARCHIVE$SUB_LINE_ITEMS
	!	PO_MAIN_SUB_LINE$HELP
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAIN_ARCHIVE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_MAIN_ARCHIVE
	!	$ DELETE PO_MAIN_ARCHIVE.OBJ;*
	!
	! Author:
	!
	!	08/23/91 - JEFF BEARD
	!
	! Modification history:
	!
	!	06/12/92 -  Kevin Handy
	!		Clean up (check)
	!
	!	05/09/94 - Kevin Handy
	!		Modified to maintain the same file that the purge
	!		process creates. Never changed since Frank decided
	!		to change the name of the archive files.
	!
	!	05/09/94 - Kevin Handy
	!		Gaack. Gave up trying to make original source work,
	!		so took PO_MAIN_REGLINE and modified for archive.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/28/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" inn several places.
	!
	!	05/29/97 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE
	MAP (PO_REG_LINE_OLD)	PO_REG_LINE_CDD		PO_REG_LINE_OLD, PO_REG_LINE2

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PO.OPEN]PO_TYPE.HB"
	MAP (PO_TYPE)		PO_TYPE_CDD		PO_TYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_MEASURE.HB"
	MAP (UTL_MEASURE)	UTL_MEASURE_CDD		UTL_MEASURE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL STRING FUNCTION CONV_STRING

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PO_REG_LINE) &
		PO_REG_LINE.CH%, &
		PO_REG_LINE.READONLY%, &
		PO_REG_LINE.CH_ARC%, &
		PO_REG_LINE.READONLY_ARC%

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	!
	! Decide what the user wants to do
	!
	SELECT MOPTION

	!********************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!********************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR  = "Purchase Order Register Maintenance"
		SMG_WINDOW::NHELP  = "PO_MAIN_REGLINE"
		SMG_WINDOW::HSIZE  = 76%
		SMG_WINDOW::VSIZE  = 15%
		SMG_WINDOW::HVIEW  = 76%
		SMG_WINDOW::VVIEW  = 15%
		SMG_WINDOW::HPOS   =  2%
		SMG_WINDOW::VPOS   =  2%
		SMG_WINDOW::NITEMS = 12%
		SMG_WINDOW::FLAGS  =  0%

		SMG_WINDOW::NKEYS = 5%
		SMG_WINDOW::KNAME(0%) = "po_Number"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "po_Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 3%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
			SMG_WINDOW::KFIELD(1%, 3%) = 2%
		SMG_WINDOW::KNAME(2%) = "Vendor"
			SMG_WINDOW::KFIELD(2%, 0%) = 3%
			SMG_WINDOW::KFIELD(2%, 1%) = 4%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%
			SMG_WINDOW::KFIELD(2%, 3%) = 2%
		SMG_WINDOW::KNAME(3%) = "Batch"
			SMG_WINDOW::KFIELD(3%, 0%) = 3%
			SMG_WINDOW::KFIELD(3%, 1%) = 11%
			SMG_WINDOW::KFIELD(3%, 2%) = 1%
			SMG_WINDOW::KFIELD(3%, 3%) = 2%
		SMG_WINDOW::KNAME(4%) = "Product"
			SMG_WINDOW::KFIELD(4%, 0%) = 3%
			SMG_WINDOW::KFIELD(4%, 1%) = 6%
			SMG_WINDOW::KFIELD(4%, 2%) = 1%
			SMG_WINDOW::KFIELD(4%, 3%) = 2%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PO_REG_LINE.CH_ARC% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PO_REG_LINE.READONLY_ARC%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.ARC"
		USE
			CONTINUE 760 IF ERR = 10%
			PO_MAIN_REGLINE = ERR
			CONTINUE 770
		END WHEN

		PO_REG_LINE.READONLY_ARC% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.ARC"
		USE
			PO_MAIN_REGLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PO_REG_LINE.READONLY_ARC% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PO_REG_LINE.CH_ARC%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PO_REG_LINE.CH_ARC%
		WHEN ERROR IN
			RESET #PO_REG_LINE.CH_ARC%
			GET #PO_REG_LINE.CH_ARC%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

	!********************************************************************
	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
	!********************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	2,  3, "(01) PO Number", &
			3,  3, "(02) PO Line", &
			4,  3, "(03) PO Type", &
			5,  3, "(04) Vendor", &
			7,  3, "(05) Location", &
			8,  3, "(06) Product #", &
			9,  3, "(07) Description", &
			10, 3, "(08) UOM", &
			11, 3, "(09) Order Date", &
			12, 3, "(10) Open_Closed", &
			13, 3, "(11) Batch", &
			14, 3, "(12) Period", &
			0,  0, ""

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

20200	!********************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!********************************************************************
	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Purchase Order Number\*
	!	.p
	!	The ^*Purchase Order Number\* field enters the purchase
	!	order number of the item ordered.
	!	.p
	!	The field will accommodate ten (10) alphanumeric characters.
	!
	! Index:
	!	.x Regline>Purchase Order Number
	!	.x Purchase Order Number>Regline
	!
	!--
			PO_REG_LINE::PO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;21", TEMP$, &
				PO_REG_LINE::PO, MFLAG OR 2%, "~R 'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Purchase Order Line\*
	!	.p
	!	The ^*Line Number\* field is used to specify which line
	!	item this entry is on the Purchase Order, and is used
	!	in the Receiving Journal and the Accounts Payable
	!	invoicing to match up.
	!	.p
	!	This field will accommodate four (4) alphanumeric characters.
	!
	! Index:
	!	.x Regline>Line Number
	!	.x Line Number>Regline
	!
	!--
			PO_REG_LINE::PO_LINE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;21", TEMP$, &
				PO_REG_LINE::PO_LINE, MFLAG, "~L0'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Purchase Order Type\*
	!	.p
	!	The ^*Purchase Order Type\* field is used to specify
	!	which type of entry this is on the Purchase Order.
	!	.p
	!	Pressing the ^*List Choices\* key while the cursor is positioned on
	!	this field will cause the defined Purchase Order Types to be displayed.
	!	.p
	!	This field will accommodate two (2) alphanumeric characters.
	!
	! Index:
	!	.x Regline>PO Type
	!	.x PO Type>Regline
	!
	!--
			PO_REG_LINE::PO_TYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;21", TEMP$, &
				PO_REG_LINE::PO_TYPE, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(PO_MAIN_TYPE.ID, "VX  ") = 1%
				THEN
					PO_REG_LINE::PO_TYPE = &
						PO_TYPE::POTYPE
				END IF
				GOTO E0Loop

			END SELECT

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Vendor Number\*
	!	.p
	!	The ^*Vendor Number\* field enters the
	!	vendor number from which the PO was ordered.
	!	.p
	!	Pressing the ^*List Choices\* key while the cursor is positioned on
	!	this field will cause the defined Vendors to be displayed.
	!	.p
	!	This field will accommodate ten (10) alphanumeric characters.
	!
	! Index:
	!	.x Regline>Vendor Number
	!	.x Vendor Number>Regline
	!
	!--
			PO_REG_LINE::VENDOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;21", TEMP$, &
				PO_REG_LINE::VENDOR, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(AP_MAIN_VENDOR.ID, "VX") = 1%)
				THEN
					PO_REG_LINE::VENDOR = AP_VENDOR::VENNUM
				END IF
				GOTO E0Loop
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Location\*
	!	.p
	!	The ^*Location\* field specifies which of the
	!	companies locations placed this purchase order.
	!	.p
	!	Pressing the ^*List Choices\* key while the cursor is positioned on
	!	this field will cause the defined Locations to be displayed.
	!	.p
	!	This field will accommodate four (4) alphanumeric characters.
	!
	! Index:
	!	.x Regline>Location
	!	.x Location>Regline
	!
	!--
			PO_REG_LINE::FROMLOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;21", TEMP$, &
				PO_REG_LINE::FROMLOCATION, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
					"VX  ") = 1%
				THEN
					PO_REG_LINE::FROMLOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO E0Loop
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Product Number\*
	!	.p
	!	The ^*Product Number\* specifies the product
	!	number of the item in consideration.
	!	.p
	!	Pressing the ^*List Choices\* key while the cursor is positioned on
	!	this field will cause the defined Product Numbers to be displayed.
	!	.p
	!	This field will accommodate fourteen (14) alphanumeric characters.
	!
	! Index:
	!	.x Product Number>Regline
	!	.x Regline>Product Number
	!
	!--
			PO_REG_LINE::PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;21", TEMP$, &
				PO_REG_LINE::PRODUCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%)
				THEN
					PO_REG_LINE::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO E0Loop
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Description\*
	!	.p
	!	The ^*Description\* field is used to describe the product
	!	for the PO.
	!
	! Index:
	!	.x Regline>Item Description
	!	.x Item Description>Regline
	!
	!--
			PO_REG_LINE::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;21", TEMP$, &
				PO_REG_LINE::DESCRIPTION, MFLAG, &
				"'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Unit Of Measure\*
	!	.p
	!	The ^*Unit Of Measure\* field should contain the unit of measure used by
	!	the user relative to the product being considered.
	!	.p
	!	This field will default to the unit of measure defined in the
	!	product description file and automatically display that default.
	!	.p
	!	Pressing the ^*List Choices\* key while the cursor is positioned on
	!	this field will cause the defined Units of Measure to be displayed.
	!	.p
	!	This field will contain two (2) alphanumeric characters.
	!
	! Index:
	!	.x Unit Of Measure>Regline
	!	.x Regline>Unit Of Measure
	!
	!--
			PO_REG_LINE::UOM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;21", TEMP$, &
				PO_REG_LINE::UOM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "VX") = 1%)
				THEN
					PO_REG_LINE::UOM = UTL_MEASURE::CODE
				END IF
				GOTO E0Loop
			END IF

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Order Date\*
	!	.p
	!	The ^*Order Date\* field enters the date on which the Purchase
	!	Order is effective.
	!	.p
	!	The format for entry is MMDDYYYY.
	!
	! Index:
	!	.x Regline> Order Date
	!	.x Order Date>Regline
	!
	!--
			PO_REG_LINE::ORDDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;21", TEMP$, &
				PO_REG_LINE::ORDDATE, MFLAG, "8", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Open__Closed Flag\*
	!	.p
	!	The ^*Open__Closed Flag\* field indicates
	!	whether a Purchase Order is still open or active,
	!	or whether the Purchase Order has been closed
	!	and will not accept activity.
	!	.p
	!	This field will contain one (1) alphanumeric character.
	!
	! Index:
	!	.x Open__Closed Flag>Regline
	!	.x Regline>Open__Closed Flag
	!
	!--
			PO_REG_LINE::OPEN_CLOSE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;21", TEMP$, &
				PO_REG_LINE::OPEN_CLOSE, MFLAG, "'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Batch Number\*
	!	.p
	!	The ^*Batch Number\* is used to specify the Purchase
	!	Order batch or group that was posted to the Register.
	!	Normally, there will be no maintenance on this field.
	!	.p
	!	This field accepts six (6) alphanumeric characters.
	!
	! Index:
	!	.x Regline>Batch Number
	!	.x Batch Number>Regline
	!
	!--
			PO_REG_LINE::BATCH = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;21", TEMP$, &
				PO_REG_LINE::BATCH, MFLAG, "'E", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Posting Period\*
	!	.p
	!	The Posting Period field will indicate the accounting period
	!	into which this batch was posted.
	!	.p
	!	This field will accommodate six (6) alphanumeric characters.
	!
	! Index:
	!	.x Regline>Our Unit Of Measure
	!	.x Our Unit Of Measure>Regline
	!
	!--
			PO_REG_LINE::PERIOD = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;21", TEMP$, &
				PO_REG_LINE::PERIOD, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$
	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		SELECT MLOOP

		CASE 1% TO 2%
			IF (PO_REG_LINE::PO = "" AND MLOOP = 1) OR &
				(PO_REG_LINE::PO_LINE = "" AND MLOOP = 2)
			THEN
				PO_MAIN_REGLINE = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #PO_REG_LINE.CH_ARC%, &
							KEY #0% EQ PO_REG_LINE::PO + &
							PO_REG_LINE::PO_LINE, &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
					END WHEN

					PO_MAIN_REGLINE = 2%

					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				END IF
			END IF

		CASE 3%
			!
			! Is the input defined?
			!
			PO_MAIN_REGLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				PO_REG_LINE::PO_TYPE, PO_TYPE::DESCR, &
				"PO", SCOPE::PRG_PROGRAM, "PO TYPE", &
				"PO Type", PO_MAIN_TYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PO_TYPE::DESCR, &
				4%, 35%, , SMG$M_BOLD)

		CASE 4%
			!
			! Is the input defined?
			!
			ST%, PO_MAIN_REGLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				PO_REG_LINE::VENDOR, AP_VENDOR::VENNAM, &
				"AP", SCOPE::PRG_PROGRAM, "VENDOR NAME", &
				"Vendor Number", AP_MAIN_VENDOR.ID)

			IF ST% <> 0%
			THEN
				AP_VENDOR::POCITY = &
					STRING$(LEN(AP_VENDOR::POCITY), A"?"B)

				AP_VENDOR::POSTATE = &
					STRING$(LEN(AP_VENDOR::POSTATE), A"?"B)

				AP_VENDOR::POZIP = &
					STRING$(LEN(AP_VENDOR::POZIP), A"?"B)
			END IF

			CITYLINE$ = TRM$(AP_VENDOR::POCITY) + ", " + &
				AP_VENDOR::POSTATE  + " "  + &
				TRM$(AP_VENDOR::POZIP)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::VENNAM, &
				5%, 35%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				CITYLINE$, &
				6%, 35%, , SMG$M_BOLD)

		CASE 5%
			!
			! Is the input defined?
			!
			PO_MAIN_REGLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				PO_REG_LINE::FROMLOCATION, &
				UTL_LOCATION::LOCNAME, &
				"UTL", SCOPE::PRG_PROGRAM, "LOCATION NAME", &
				"Location Number", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, &
				7%, 35%, , SMG$M_BOLD)

		CASE 6%
			!
			! Is the input defined?
			!
			IF PO_REG_LINE::PRODUCT <> ""
			THEN
				PO_MAIN_REGLINE = FUNC_TESTENTRY(SMG_WINDOW, &
					PO_REG_LINE::PRODUCT, &
					PD_PRODUCT::PRODUCT_NUM, &
					"PD", SCOPE::PRG_PROGRAM, "PRODUCT", &
					"Product number", PD_MAIN_PRODUCT.ID)

				PO_REG_LINE::DESCRIPTION = &
					PD_PRODUCT::DESCRIPTION
				PO_REG_LINE::UOM = PD_PRODUCT::UOM

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					PO_REG_LINE::DESCRIPTION, 9%, 21%,, SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					PO_REG_LINE::UOM, 10%, 21%,, SMG$M_BOLD)

			END IF

		CASE 7%
			IF PO_REG_LINE::DESCRIPTION = ""
			THEN
				PO_MAIN_REGLINE = 1%
			END IF

		CASE 8%
			!
			! Is the input defined for vendor UOM
			!
			PO_MAIN_REGLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				PO_REG_LINE::UOM, UTL_MEASURE::DESCRIPTION, &
				"UTL", SCOPE::PRG_PROGRAM, "UOM", &
				"Unit of Measure", UTL_MAIN_MEASURE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_MEASURE::DESCRIPTION, &
				10%, 35%, , SMG$M_BOLD)

		END SELECT

		%PAGE

	!
	! Display additional information
	!
	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(PO_MAIN_TYPE.ID, &
				"Q0" + PO_REG_LINE::PO_TYPE) <> 1%
			THEN
				PO_TYPE::DESCR  = &
					STRING$(LEN(PO_TYPE::DESCR), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PO_TYPE::DESCR, 4%, 35%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, &
				"Q0" + PO_REG_LINE::VENDOR)

			IF ST% <> 1%
			THEN
				AP_VENDOR::VENNAM  = &
					STRING$(LEN(AP_VENDOR::VENNAM), A"?"B)

				AP_VENDOR::CITY  = &
					STRING$(LEN(AP_VENDOR::POCITY), A"?"B)

				AP_VENDOR::STATE  = ""
				AP_VENDOR::ZIP  = ""
			END IF

			CITYLINE$ = TRM$(AP_VENDOR::POCITY) + ", " + &
				AP_VENDOR::STATE    + " "  + &
				TRM$(AP_VENDOR::ZIP)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::VENNAM, 5%, 35%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				CITYLINE$, 6%, 35%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"Q0" + PO_REG_LINE::FROMLOCATION) <> 1%
			THEN
				UTL_LOCATION::LOCNAME  = &
					STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 7%, 35%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(UTL_MAIN_MEASURE.ID, &
				"Q0" + PO_REG_LINE::UOM) <> 1%
			THEN
				UTL_MEASURE::DESCRIPTION  = &
					STRING$(LEN(UTL_MEASURE::DESCRIPTION), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_MEASURE::DESCRIPTION, 10%, 35%,, SMG$M_BOLD)
		END IF

	!
	! Set PO_REG_LINE_OLD value
	!
20500	CASE OPT_SETOLD
		PO_REG_LINE_OLD = PO_REG_LINE

	!
	! Restore PO_REG_LINE_OLD value
	!
	CASE OPT_RESETOLD
		PO_REG_LINE = PO_REG_LINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PO_REG_LINE2 = PO_REG_LINE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PO_REG_LINE = PO_REG_LINE2

	!
	! SET DEFAULTS FOR NEXT ADD
	!
	CASE OPT_AFTEROPT
		PO_REG_LINE2::PO      = PO_REG_LINE::PO

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  PO Number   Line Typ Vendor     Loc  " + &
				"Product #      UM OrdDate  O/C Batch"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "014,019,023,034,039,054,057,066,070"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT) + &
				"  "  + &
				PO_REG_LINE::PO_LINE + " "   + &
				PO_REG_LINE::PO_TYPE + "  "  + &
				PO_REG_LINE::VENDOR + " "   + &
				PO_REG_LINE::FROMLOCATION + " "   + &
				PO_REG_LINE::PRODUCT + " "   + &
				PO_REG_LINE::UOM + " "   + &
				PRNT_DATE(PO_REG_LINE::ORDDATE, 6%) + " "   + &
				PO_REG_LINE::OPEN_CLOSE + "   " + &
				PO_REG_LINE::BATCH

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #PO_REG_LINE.CH_ARC%, &
				KEY #0% GE PO_REG_LINE::PO + &
				PO_REG_LINE::PO_LINE, REGARDLESS

		CASE 1%
			FIND #PO_REG_LINE.CH_ARC%, &
				KEY #1% GE PO_REG_LINE::PO_TYPE + &
				PO_REG_LINE::PO + PO_REG_LINE::PO_LINE, &
				REGARDLESS

		CASE 2%
			FIND #PO_REG_LINE.CH_ARC%, &
				KEY #2% GE PO_REG_LINE::VENDOR + &
				PO_REG_LINE::PO + PO_REG_LINE::PO_LINE, &
				REGARDLESS

		CASE 3%
			FIND #PO_REG_LINE.CH_ARC%, &
				KEY #3% GE PO_REG_LINE::BATCH + &
				PO_REG_LINE::PO + PO_REG_LINE::PO_LINE, &
				REGARDLESS

		CASE 4%
			FIND #PO_REG_LINE.CH_ARC%, &
				KEY #4% GE PO_REG_LINE::PRODUCT + &
				PO_REG_LINE::PO + PO_REG_LINE::PO_LINE, &
				REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

	%PAGE

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
