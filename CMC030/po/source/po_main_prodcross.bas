1	%TITLE "Vendor-Product Cross Reference"
	%SBTTL "PO_MAIN_PRODCROSS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_MAIN_PRODCROSS(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.p
	!	The ^*vendor-Product\* cross reference defines
	!	a cross reference table between a vendors part number and
	!	your part number.
	!
	! Index:
	!	.x Vendor>Cross Reference
	!	.x Product>Cross Reference
	!	.x Cross Reference>Vendor
	!	.x Cross Reference>Product
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAIN_PRODCROSS/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PO_MAIN_PRODCROSS
	!	$ DELETE PO_MAIN_PRODCROSS.OBJ;*
	!
	! Author:
	!
	!	03/20/90 - Kevin Handy
	!
	! Modification history:
	!
	!	04/28/92 - Dan Perkins
	!		Changed to accomodate addition of
	!		PRIORITY, and MINQTY fields to record.
	!
	!	04/29/92 - Kevin Handy
	!		Clean up (check)
	!
	!	11/25/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	04/12/93 - Kevin Handy
	!		Clean up (check)
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
	!	11/02/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!
	!	12/11/2001 - Kevin Handy
	!		Expand size of minqty field from 9999 to 9999999.
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include Files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! Map's and records
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP (PO_PARTCROSS)	PO_PARTCROSS_CDD	PO_PARTCROSS
	MAP (PO_PARTCROSS_OLD)	PO_PARTCROSS_CDD	PO_PARTCROSS_OLD
	MAP (PO_PARTCROSS_DEF)	PO_PARTCROSS_CDD	PO_PARTCROSS_DEF

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_MEASURE.HB"
	MAP (UTL_MEASURE)	UTL_MEASURE_CDD		UTL_MEASURE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PO_PARTCROSS) &
		PO_PARTCROSS.CH%, &
		PO_PARTCROSS.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
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

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Part Number Cross Reference Maintenance"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "PO_MAIN_PRODCROSS"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 11%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 7%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 9%
		SMG_WINDOW::HVIEW = 114%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 7%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Priority"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Vendor"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PO_PARTCROSS.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PO_PARTCROSS.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PO_MAIN_PRODCROSS = ERR
			CONTINUE 770
		END WHEN

		PO_PARTCROSS.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.OPN"
		USE
			PO_MAIN_PRODCROSS = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PO_PARTCROSS.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PO_PARTCROSS.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN = PO_PARTCROSS.CH%
		WHEN ERROR IN
			RESET #PO_PARTCROSS.CH%
			GET #PO_PARTCROSS.CH%, REGARDLESS
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

		DATA	02,05, "(01) Priority", &
			03,05, "(02) Vendor#", &
			04,05, "(03) Vendor Product#", &
			05,05, "(04) Description", &
			06,05, "(05) Unit of Measure", &
			07,05, "(06) Vendor Factor", &
			08,05, "(07) Our Factor", &
			09,05, "(08) Minimum Quantity", &
			10,05, "(09) Req Days Lead Time", &
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

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	! +++
	! Abstract:FLD001
	!	^*(01) Priority\*
	!	.lm +5
	!	.b
	!	The ^*Priority\* field
	!	enters a priority code which ranks a specific vendor with regard to
	!	preference in supplying a product.  Any alphanumeric character may be entered
	!	in this field.
	!	.b
	!	An entry is required in this field.
	!	.b
	!	The field will accommodate one (1) alphanumeric character.
	!	.lm -5
	!
	! Index:
	!	.x Vendor>Priority
	!	.x Priority>Vendor
	!	.x Vendor>Cross Reference>Priority
	!
	!--
			PO_PARTCROSS::PRIORITY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;30", TEMP$, &
				PO_PARTCROSS::PRIORITY, MFLAG, "'E", MVALUE)

		CASE 2%
	! +++
	! Abstract:FLD002
	!	^*(02) Vendor _#\*
	!	.LM +5
	!	.b
	!	The ^*Vendor _#\* field
	!	enters a vendor number as defined in the vendor master file.
	!	.b
	!	The field will accommodate up to fourteen (10) alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will
	!	cause a list of vendors to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Vendor>Number
	!	.x Vendor>Cross Reference>Number
	!
	!--
			PO_PARTCROSS::VENDOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;30", TEMP$, &
				PO_PARTCROSS::VENDOR, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(AP_MAIN_VENDOR.ID, "VX") = 1%)
				THEN
					PO_PARTCROSS::VENDOR = &
						AP_VENDOR::VENNUM
				END IF
				GOTO E0Loop
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Product _#\*
	!	.lm +5
	!	.b
	!	The ^*Product _#\* field
	!	enters the product _# used by a vendor for a product.
	!	.b
	!	The field will accommodate up to fourteen (14) alphanumeric characters.
	!
	! Index:
	!	.x Product>Number
	!	.x Vendor>Cross Reference>Product _#
	!
	!--
			PO_PARTCROSS::VENPROD = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;30", TEMP$, &
				PO_PARTCROSS::VENPROD, MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Description\*
	!	.lm +5
	!	.b
	!	The ^*Description\* field
	!	enters a description used by a vendor for a specific product.
	!	.b
	!	The field will accommodate up to forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Product>Description
	!	.x Tables>Product Description
	!	.x Vendor>Cross Reference>Product Description
	!
	!--
			PO_PARTCROSS::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;30", TEMP$, &
				PO_PARTCROSS::DESCR, MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Unit Of Measure\*
	!	.lm +5
	!	.b
	!	The ^*Unit of Measure\* field
	!	enters a unit of measure code which is used by the vendor.
	!	.b
	!	The field will accommodate two (2) alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\*, while the cursor is located at this field, will
	!	cause valid Units of Measure codes to be displayed.
	!
	! Index:
	!	.x Unit of Measure
	!	.x Tables>Unit of Measure
	!	.x Vendor>Cross Reference>Unit of Measure
	!
	!--
			PO_PARTCROSS::VENUOM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;30", TEMP$, &
				PO_PARTCROSS::VENUOM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "VX") = 1%)
				THEN
					PO_PARTCROSS::VENUOM = &
						UTL_MEASURE::CODE
				END IF
				GOTO E0Loop
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Vendor Factor\*
	!	.lm +5
	!	.b
	!	The ^*Vendor Factor\* field
	!	enters a factor which is used in ordering a product from a vendor.
	!	For example, if a vendor sells a product in rolls of 150 feet, the product
	!	would be ordered from the vendor in ^*rolls\*.  The vendor factor would be
	!	^*1.0\*.  Since the unit of measure would be the vendor's unit of measure, the
	!	factor would normally be 1.0
	!	.b
	!	The field will accommodate a number up to 9999.999.
	!	.lm -5
	!
	! Index:
	!	.x Vendor>Cross Reference>Factor
	!	.x Factor>Vendor
	!
	!--
			PO_PARTCROSS::VENFAC = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;30", TEMP$, &
				PO_PARTCROSS::VENFAC, MFLAG, "####.###", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Our Factor\*
	!	.lm +5
	!	.b
	!	The ^*Our Factor\* field
	!	enters the factor which indicates the relationship between the
	!	way a product is ordered from a vendor and how it is recorded by a user.  For
	!	example, if a vendor sells a product in rolls of 150 feet, the product would
	!	be ordered from the vendor as one (1) roll, but would be received and recorded
	!	in inventory as one hundred-fifty (150) feet.  The ^*Our Factor\* field would
	!	contain ^*150\*.
	!	.b
	!	The field will accommodate a number up to 9999.999.
	!	.lm -5
	!
	! Index:
	!	.x Factor>Our
	!	.x Vendor>Cross Reference>Our Factor
	!	.x Our>Factor
	!
	!--
			PO_PARTCROSS::FACTOR = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;30", TEMP$, &
				PO_PARTCROSS::FACTOR, MFLAG, "####.###", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Minimum Order Quantity\*
	!	.lm +5
	!	.b
	!	The ^*Minimum Order Quantity\* field
	!	enters the minimum purchase quantity required by
	!	a vendor.
	!	.b
	!	The field will accommodate a number up to 9999.
	!	.lm -5
	!
	! Index:
	!	.x Vendor>Cross Reference>Minimum Quantity
	!	.x Minimum Quantity>Vendor
	!	.x Product>Minimum Quantity
	!	.x Minimum Quantity>Product
	!
	!--
			PO_PARTCROSS::MINQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;30", TEMP$, &
				PO_PARTCROSS::MINQTY, MFLAG, "#######", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Required Days Lead Time\*
	!	.lm +5
	!	.b
	!	The ^*Required Days Lead Time\* field
	!	enters the expected length of time (in days)
	!	that it normally takes to receive a product from a vendor.
	!	.b
	!	The field will accommodate up to four (4) numeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Vendor>Cross Reference>Lead Time
	!	.x Lead Time>Vendor
	!	.x Product>Lead Time
	!	.x Lead Time>Product
	!
	!--
			PO_PARTCROSS::LEAD = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;30", TEMP$, &
				PO_PARTCROSS::LEAD * 1.0, MFLAG, &
				"####", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PO_MAIN_PRODCROSS = 0%

		SELECT MLOOP

		CASE 1%
			IF PO_PARTCROSS::PRIORITY = ""
			THEN
				PO_MAIN_PRODCROSS = 1%
			END IF

			!
			! See if priority already exists
			!
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #PO_PARTCROSS.CH%, &
						KEY #0% EQ PO_PARTCROSS::PRODUCT + &
						PO_PARTCROSS::PRIORITY, REGARDLESS

				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				PO_MAIN_PRODCROSS = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Duplicate Priority", 1%)
				GOTO 32767
			END IF

		CASE 2%
			!
			! Is the input defined?
			!
			PO_MAIN_PRODCROSS = FUNC_TESTENTRY( SMG_WINDOW, &
				PO_PARTCROSS::VENDOR, &
				AP_VENDOR::VENNAM, &
				"PO", MLOOP, "PRG", &
				"Vendor", AP_MAIN_VENDOR.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::VENNAM, &
				3%, 45%, , SMG$M_BOLD)

		CASE 5%
			!
			! Is the input defined?
			!
			PO_MAIN_PRODCROSS = FUNC_TESTENTRY( SMG_WINDOW, &
				PO_PARTCROSS::VENUOM, &
				UTL_MEASURE::DESCRIPTION, &
				"PO", MLOOP, "PRG", &
				"Unit of Measure", UTL_MAIN_MEASURE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_MEASURE::DESCRIPTION, &
				6%, 45%, , SMG$M_BOLD)

		CASE 6%
			IF PO_PARTCROSS::VENFAC = 0.0
			THEN
				PO_MAIN_PRODCROSS = 1%
			END IF

		CASE 7%
			IF PO_PARTCROSS::FACTOR = 0.0
			THEN
				PO_MAIN_PRODCROSS = 1%
			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			AP_VENDOR::VENNAM = &
				STRING$(LEN(AP_VENDOR::VENNAM), A"?") &
				IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, &
					"Q0" + PO_PARTCROSS::VENDOR) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::VENNAM, 3%, 45%, , SMG$M_BOLD)

		END IF

	!
	! Set PO_PARTCROSS_OLD value
	!
20500	CASE OPT_SETOLD
		PO_PARTCROSS_OLD = PO_PARTCROSS

	!
	! Restore PO_PARTCROSS_OLD value
	!
	CASE OPT_RESETOLD
		PO_PARTCROSS = PO_PARTCROSS_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PO_PARTCROSS_DEF = PO_PARTCROSS

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PO_PARTCROSS          = PO_PARTCROSS_DEF
		PO_PARTCROSS::PRODUCT = MVALUE

		IF MFLAG = 1%
		THEN
			PO_PARTCROSS::VENUOM  = PD_PRODUCT::UOM &
				IF PO_PARTCROSS::VENUOM = ""

			PO_PARTCROSS::DESCR  = PD_PRODUCT::DESCRIPTION &
				IF PO_PARTCROSS::DESCR = ""

			PO_PARTCROSS::VENFAC  = 1.0 &
				IF PO_PARTCROSS::VENFAC = 0.0

			PO_PARTCROSS::FACTOR  = 1.0 &
				IF PO_PARTCROSS::FACTOR = 0.0

		END IF

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)

		CASE 1%
			MVALUE = "  Priority VendorNumb Vendor_Product " + &
				"Description" + SPACE$(30%) + &
				"UOM VendFact  OurFact MinOrdQty Lead"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "011,022,037,078,082,091,100,110"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PO_PARTCROSS::PRIORITY + "        " + &
				PO_PARTCROSS::VENDOR + " " + &
				PO_PARTCROSS::VENPROD + " " + &
				PO_PARTCROSS::DESCR + " " + &
				PO_PARTCROSS::VENUOM + "  " + &
				FORMAT$(PO_PARTCROSS::VENFAC, &
					"####.###") + " " + &
				FORMAT$(PO_PARTCROSS::FACTOR, &
					"####.###") + "      " + &
				FORMAT$(PO_PARTCROSS::MINQTY, "####") + " " + &
				FORMAT$(PO_PARTCROSS::LEAD, "####")
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PO_PARTCROSS::PRODUCT + &
				PO_PARTCROSS::PRIORITY, REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE PO_PARTCROSS::VENDOR + &
				PO_PARTCROSS::PRODUCT, REGARDLESS

		END SELECT

	!
	! Sub Window
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
						PO_PARTCROSS::PRIORITY, &
						REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN

			CASE 1%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #1% GE PO_PARTCROSS::VENDOR + &
						MVALUE, REGARDLESS
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
			IF PO_PARTCROSS::PRODUCT = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			PO_PARTCROSS::PRODUCT = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
