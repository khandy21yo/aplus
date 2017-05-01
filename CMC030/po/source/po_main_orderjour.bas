1	%TITLE "Maintain Purchase Order Journal"
	%SBTTL "PO_MAIN_ORDERJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_MAIN_ORDERJOUR(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	Purchase orders are maintained through the ^*Maintain Purchase
	!	Order Journal\* option.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order>Maintenance
	!	.x Maintenance>Purchase Orders
	!	.x Journal>Purchase Orders
	!	.x Purchase Order>Journal
	!
	! Option:
	!
	!	PO_MAIN_ORDERJOUR$LINE_ITEMS
	!	PO_MAIN_ORDERJOUR_LINE$HELP
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAIN_ORDERJOUR/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_MAIN_ORDERJOUR
	!	$ DELETE PO_MAIN_ORDERJOUR.OBJ;*
	!
	! Author:
	!
	!	06/13/90 - Aaron Redd
	!
	! Modification history:
	!
	!	07/05/90 - Kevin Handy
	!		Modified to use ENTR_3PO.
	!
	!	07/16/90 - Kevin Handy
	!		Modified to look into register file.
	!
	!	10/24/90 - Kevin Handy
	!		Modified to display vendor name on screen,
	!		and to allow blanks in fields 17-20.
	!
	!	01/22/92 - Dan Perkins
	!		Eliminated fields 12 and 14, receive date,
	!		and discount.  Defaulted field 11 to today's
	!		date.
	!
	!	02/07/92 - Dan Perkins
	!		More file layout changes.  Changed screen
	!		display layout.  Added some fields and
	!		changed others.  Added F-17 key to various fields.
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/03/92 - Dan Perkins
	!		Changed from ENTR_3PO to ENTR_3STRING function after
	!		discussion with powers that be.  ENTR_3STRING is RSET
	!		by using (MVALUE OR 2%).
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	12/22/92 - Frank F. Starman
	!		Display record if already exists.
	!
	!	01/08/93 - Dan Perkins
	!		Automatically increment PO number if desired.
	!
	!	11/04/94 - Kevin Handy
	!		Added paramerter to PO_READ_REGLINE.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/01/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/02/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

	%INCLUDE "SOURCE:[PO.OPEN]PO_ACKNOWLEDGE.HB"
	MAP (PO_ACKNOWLEDGE)	PO_ACKNOWLEDGE_CDD	PO_ACKNOWLEDGE

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.HB"
	MAP (PO_ORDERJOUR)	PO_ORDERJOUR_CDD	PO_ORDERJOUR
	MAP (PO_ORDERJOUR_OLD) PO_ORDERJOUR_CDD	PO_ORDERJOUR_OLD, PO_ORDERJOUR2

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE
	DECLARE			PO_REG_LINE_CDD		PO_REG_LINE_READ

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	DECLARE			PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	%INCLUDE "SOURCE:[PO.OPEN]PO_TYPE.HB"
	MAP (PO_TYPE)		PO_TYPE_CDD		PO_TYPE

	%INCLUDE "SOURCE:[PO.OPEN]PO_NOTES.HB"
	MAP (PO_NOTES)		PO_NOTES_CDD		PO_NOTES

	%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.HB"
	MAP (PO_CONTROL)	PO_CONTROL_CDD		PO_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.HB"
	MAP (UTL_STATE)		UTL_STATE_CDD		UTL_STATE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_FOB.HB"
	MAP (UTL_FOB)		UTL_FOB_CDD		UTL_FOB

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION PO_READ_REG_LINE
	EXTERNAL LONG   FUNCTION FUNC_INCREMENT
	EXTERNAL STRING	FUNCTION CONV_STRING

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

	COM (CH_PO_CONTROL) &
		PO_CONTROL.CH%

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
		SMG_WINDOW::DESCR = "Purchase Order Entry Journal " + BATCH_NO$
		SMG_WINDOW::NHELP = "PO_MAIN_ORDERJOUR"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  =  2%
		SMG_WINDOW::VPOS  =  2%
		SMG_WINDOW::NITEMS= 25%
		SMG_WINDOW::FLAGS =  0%

		SMG_WINDOW::NKEYS =  3%
		SMG_WINDOW::KNAME(0%) = "Purchase_order"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Vendor"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
		SMG_WINDOW::KNAME(2%) = "po_Type"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 2%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PO_ORDERJOUR.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PO_ORDERJOUR.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PO_MAIN_ORDERJOUR = ERR
			CONTINUE 770
		END WHEN

		PO_ORDERJOUR.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.OPN"
		USE
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PO_ORDERJOUR.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PO_ORDERJOUR.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PO_ORDERJOUR.CH%
		WHEN ERROR IN
			RESET #PO_ORDERJOUR.CH%
			GET #PO_ORDERJOUR.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

	!++
	! Abstract:LINE_ITEMS
	!	^*Line&_ Items\*
	!	.b
	!	.lm +5
	!	The ^*Line&_ items\* function
	!	accesses a scrolling window where each individual
	!	line item in the body of a purchase order can be entered.  The fields provided
	!	for each line item include the following:
	!	.table 3,25
	!	.te
	!	Line number
	!	.te
	!	User's Product Number
	!	.te
	!	Product Description
	!	.te
	!	User's Unit of Measure
	!	.te
	!	Vendor's Product Number
	!	.te
	!	Vendor's Price
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Line Items
	!	.x Line Items>Purchase Order Journal
	!
	!--

	!********************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!********************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1,  2, "(01) PO Number", &
			2,  2, "(02) PO Type", &
			3,  2, "(03) Vendor", &
			6,  2, "(04) Location", &
			7,  2, "(05) Date", &
			8,  2, "(06) Buyer", &
			9,  2, "(07) Operator", &
			2, 35, "(08) Name", &
			3, 35, "(09) Address1", &
			4, 35, "(10) Address2", &
			5, 35, "(11) City", &
			6, 35, "(12) State", &
			7, 35, "(13) Zip Code", &
			8, 35, "(14) Country", &
			11, 2, "(15)        (16)    (17)    (18)      (19)       (20)        (21)     ", &
			15, 2, "(22) Note 1", &
			16, 2, "(23) Note 2", &
			17, 2, "(24) Note 3", &
			18, 2, "(25) Note 4", &
			1, 35, " Ship to:", &
			12, 2, "VendorRep   Col/PP  Terms   Carrier   FOB Code   AcknowCode  FormPrinted", &
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
	!	The ^*Purchase Order Number\* field is entered manually by the
	!	user.  This gives the option of using any alpha_numeric sequence
	!	the user wishes.
	!	.p
	!	The field will accommodate ten (10) alphanumeric characters.
	!
	! Index:
	!	.x Purchase Order Journal>Purchase Order Number
	!	.x Purchase Order Number>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::PO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;18", TEMP$, &
				PO_ORDERJOUR::PO, MFLAG OR 2%, "~R 'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(PO_MAIN_REGLINE.ID, "VX  ") = 1%
				THEN
					PO_ORDERJOUR::PO = &
						PO_REG_LINE::PO
				END IF
				GOTO E0Loop
			END SELECT

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Purchase Order Type\*
	!	.b
	!	.lm +5
	!	The ^*PO Type\* field enters the purchase order type for
	!	a specific order.
	!	.b
	!	The field will accommodate two (2) alphanumeric characters.
	!	.b
	!	Standard ASCII purchase order types can be installed in the purchase order
	!	type table.  The user may append user defined types to the Standard ASCII
	!	file or create the file specific to the user's requirements.  Valid Types
	!	may be viewed by pressing List Choices or additional types may be added by
	!	pressing the F17 key.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Purchase Order Type
	!	.x Purchase Order Type>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::POTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;18", TEMP$, &
				PO_ORDERJOUR::POTYPE, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(PO_MAIN_TYPE.ID, "VX  ") = 1%
				THEN
					PO_ORDERJOUR::POTYPE = PO_TYPE::POTYPE
				END IF
				GOTO E0Loop

			CASE SMG$K_TRM_F17
				V% = MAIN_WINDOW(PO_MAIN_TYPE.ID, "M")
				PO_ORDERJOUR::POTYPE = PO_TYPE::POTYPE
				GOTO E0Loop
			END SELECT

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Vendor\*
	!	.b
	!	.lm +5
	!	The ^*Vendor\* field enters the vendor number which has been
	!	assigned to the specific vendor.
	!	When a valid vendor number is entered, the name and address of the vendor
	!	will automatically be displayed in the appropriate fields.
	!	.b
	!	Valid Vendors may be viewed by pressing the ^*List Choices\* key or additional
	!	vendors may be added by pressing the F17 key.
	!	.b
	!	The field will accommodate up to ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Vendor Number
	!	.x Vendor Number>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::VENDOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;18", TEMP$, &
				PO_ORDERJOUR::VENDOR, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, "VX  ") = 1%
				THEN
					PO_ORDERJOUR::VENDOR = AP_VENDOR::VENNUM
				END IF
				GOTO E0Loop

			CASE SMG$K_TRM_F17
				V% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, "M")
				PO_ORDERJOUR::VENDOR = AP_VENDOR::VENNUM
				GOTO E0Loop
			END SELECT

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) From Locaction\*
	!	.b
	!	.lm +5
	!	The ^*From Location\* field enters the user's location
	!	code from which the order is originated.
	!	.b
	!	The field will accommodate up to four (4) alphanumeric characters and
	!	must be a valid location code as designated in the company profile
	!	location table.
	!	.b
	!	Valid location codes will be displayed by pressing the ^*List Choices\*
	!	key or additional location codes may be added by pressing the F17 key.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>From Location Number
	!	.x From Location Number>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::FROMLOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;18", TEMP$, &
				PO_ORDERJOUR::FROMLOCATION, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
					"VX  ") = 1%
				THEN
					PO_ORDERJOUR::FROMLOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO E0Loop
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field enters the date on which the Purchase
	!	Order is effected.  The system will automatically default to the current date
	!	but the date may be overridden by entering the correct date and pressing
	!	^*Return\*.
	!	.b
	!	The format for entry is MMDDYYYY.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal> Date
	!	.x Date>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::PODATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;18", TEMP$, &
				PO_ORDERJOUR::PODATE, MFLAG, "8", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Buyer\*
	!	.p
	!	The ^*Buyer\* field enters the code which identifies the buyer.
	!	.p
	!	The field will accommodate ten (10) alphanumeric characters.
	!
	! Index:
	!	.x Purchase Order Journal>Buyer
	!	.x Buyer>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::BUYER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;18", TEMP$, &
				PO_ORDERJOUR::BUYER, MFLAG, "'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters the name or initials of the
	!	operator who enters an order.
	!	.b
	!	An entry is required in this field.
	!	.b
	!	The field will accommodate up to ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Operator
	!	.x Operator> Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::OPERATOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;18", TEMP$, &
				PO_ORDERJOUR::OPERATOR, MFLAG, "'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Ship to Name\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Name\* field is automatically entered with the name
	!	associated with the location entered in field (04).  The entry may be
	!	overridden by entering the correct "Ship To" name and pressing ^*Return\*.
	!	.b
	!	The field will accommodate up to thirty (30) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Ship to Person/Company
	!	.x Ship to Person/Company>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::TONAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;50", TEMP$, &
				PO_ORDERJOUR::TONAME, MFLAG, "'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Address 1\*
	!	.b
	!	.lm +5
	!	The ^*Address 1\* field is automatically entered with the address associated
	!	with the location entered in field (04), and is the first line of the "Ship To"
	!	address.  This field may be manually overridden by entering the correct
	!	address and pressing ^*Return\*.
	!	.b
	!	The field will accommodate up to twenty-five (25) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Ship to Address
	!	.x Ship to Address>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::TOADD1 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;50", TEMP$, &
				PO_ORDERJOUR::TOADD1, MFLAG, "'E", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Address 2\*
	!	.b
	!	.lm +5
	!	The ^*Address 2\* field is automatically entered with the address associated
	!	with the location entered in field (04), and is the second line of the
	!	"Ship To" address.  This field may be manually overridden by entering the
	!	correct address and pressing ^*Return\*.
	!	.b
	!	The field will accommodate up to twenty five (25) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Ship to Address
	!	.x Ship To Address>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::TOADD2 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;50", TEMP$, &
				PO_ORDERJOUR::TOADD2, MFLAG, "'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) City\*
	!	.b
	!	.lm +5
	!	The ^*City\* field is automatically entered with the city associated
	!	with the location entered in field (04), and is the "Ship To" city.
	!	This field may be manually overridden by entering the correct City
	!	and pressing ^*Return\*.
	!	.b
	!	The field will accommodate up to fifteen (15) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Ship to City
	!	.x Ship to City>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::TOCITY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;50", TEMP$, &
				PO_ORDERJOUR::TOCITY, MFLAG, "'E", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field is automatically entered with the state
	!	associated with the location entered in field (04), and is the
	!	"Ship To" state.  This field may be manually overridden by entering
	!	the correct State and pressing ^*Return\*.
	!	.b
	!	The field will accommodate two (2) alphanumeric characters and should
	!	be the standard two character Post Office code for the state.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Ship to State
	!	.x Ship to State>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::TOSTATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;50", TEMP$, &
				PO_ORDERJOUR::TOSTATE, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(UTL_MAIN_STATE.ID, "V0US  ") = 1%
				THEN
					PO_ORDERJOUR::TOSTATE = UTL_STATE::STATE
				END IF
				GOTO E0Loop
			END IF

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Zip Code\*
	!	.b
	!	.lm +5
	!	The ^*Zip Code\* field is automatically entered with the zip code associated
	!	with the location entered in field (04), and is the "Ship To" zip code.  This
	!	field may be manually overridden by entering the correct Zip Code and pressing
	!	^*Return\*.
	!	.b
	!	The field will accommodate up to ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Ship to Zip Code
	!	.x Ship to Zip Code>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::TOZIP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;50", TEMP$, &
				PO_ORDERJOUR::TOZIP, MFLAG, "'E", MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	^*(14) Country\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field is available to enter the country if
	!	a Customer is located in a foreign country.  This field will
	!	automatically default to the (US) United States.
	!	.b
	!	This field may contain up to two (2) alphabetic characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Country
	!	.x Country>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::TOCOUNTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;50", TEMP$, PO_ORDERJOUR::TOCOUNTRY, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "VX" + &
					PO_ORDERJOUR::TOCOUNTRY) = 1%
				THEN
					PO_ORDERJOUR::TOCOUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO E0LOOP

			END SELECT

		CASE 15%
	!++
	! Abstract:FLD015
	!	^*(15) Vendor Salesman\*
	!	.b
	!	.lm +5
	!	The ^*Vendor Salesman\* field may be used to enter the name of the salesman who
	!	represents the vendor.
	!	.b
	!	The field will accommodate up to ten (10) alphabetic characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Salesman
	!	.x Salesman>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::SALESMAN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;2", TEMP$, &
				PO_ORDERJOUR::SALESMAN, MFLAG, "'E", MVALUE)

		CASE 16%
	!++
	! Abstract:FLD016
	!	^*(16) Collect or Pre-paid\*
	!	.b
	!	.lm +5
	!	The ^*Collect or Pre-paid\* field may be used to choose if the
	!	purchase order freight will be on a collect or pre-paid basis.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*C\* - Collect
	!	.te
	!	^*P\* - Pre-paid
	!	.lm -5
	!	.end table
	!
	! Index:
	!	.x Purchase Order Journal>Collect or Pre-paid
	!	.x Collect or Pre-paid>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::COL_PPD = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;14", TEMP$, &
				PO_ORDERJOUR::COL_PPD, MFLAG, "'E", MVALUE)

		CASE 17%
	!++
	! Abstract:FLD017
	!	^*(17) Terms\*
	!	.b
	!	.lm +5
	!	The ^*Terms\* field enters payment terms.  A Standard
	!	ASCII Terms Table is provided with the system.  The user may add to the
	!	Standard ASCII table or create a table which is user specific.
	!	.b
	!	Pressing the ^*List Choices\* key will cause the terms codes as
	!	defined in the Terms Table to be displayed.  Additional terms codes may
	!	be added by pressing the F17 key.
	!	.b
	!	The field will accommodate two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Terms
	!	.x Terms>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::TERMS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;22", TEMP$, &
				PO_ORDERJOUR::TERMS, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(UT_MAIN_TERMS.ID, "VX  ") = 1%
				THEN
					PO_ORDERJOUR::TERMS = UTL_TERMS::CODE
				END IF
				GOTO E0Loop
			END IF

		CASE 18%
	!++
	! Abstract:FLD018
	!	^*(18) Carrier\*
	!	.b
	!	.lm +5
	!	The ^*Carrier\* field enters the code representing the
	!	carrier or method of shipment.  A Standard ASCII Carrier Table is provided
	!	with the system.  The user may add records to the Standard ASCII table or
	!	create a table that is user specific.
	!	.b
	!	Pressing the ^*List Choices\* key
	!	will cause the codes in the Carrier Table to be displayed.
	!	Additional Carrier codes may be added by pressing the F17 key.
	!	.b
	!	The field will accommodate two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Carrier
	!	.x Carrier>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::CARRIER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;30", TEMP$, &
				PO_ORDERJOUR::CARRIER, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, "VX  ") = 1%
				THEN
					PO_ORDERJOUR::CARRIER = &
						UTL_CARRIER::CODE
				END IF
				GOTO E0Loop
			END IF

		CASE 19%
	!++
	! Abstract:FLD019
	!	^*(19) FOB Code\*
	!	.b
	!	.lm +5
	!	The ^*FOB Code\* field enters the FOB code.
	!	.b
	!	Pressing the ^*List Choices\* key will cause the codes in the
	!	FOB Code Table to be displayed.  Additional FOB codes may be added
	!	by pressing the F17 key.
	!	.b
	!	The field will accommodate two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>FOB Code
	!	.x FOB Code>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::FOB = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;40", TEMP$, &
				PO_ORDERJOUR::FOB, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(UT_MAIN_FOB.ID, "VX  ") = 1%
				THEN
					PO_ORDERJOUR::FOB = UTL_FOB::FOBCODE
				END IF
				GOTO E0Loop
			END IF

		CASE 20%
	!++
	! Abstract:FLD020
	!	^*(20) AcknowCode\*
	!	.b
	!	.lm +5
	!	The ^*AcknowCode\* field enters an acknowledgement code.
	!	.b
	!	Examples of Acknowledgement Codes might be:
	!	.table 3,25
	!	.te
	!	^*RE\* - Acknowledge upon receipt
	!	.te
	!	^*SH\* - Acknowledge before shipment
	!	.end table
	!	Pressing the ^*List Choices\* key will cause valid Acknowledgement
	!	Codes to be displayed.  Additional acknowledgement codes may be added by
	!	pressing the F17 key.
	!	.b
	!	The field will accommodate two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Acknowledgement Code
	!	.x Acknowledgement Code>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::ACKNOW = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;51", TEMP$, &
				PO_ORDERJOUR::ACKNOW, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PO_MAIN_ACKNOWLEDGE.ID, &
					"V0  ") = 1%
				THEN
					PO_ORDERJOUR::ACKNOW = &
						PO_ACKNOWLEDGE::CODE
				END IF
				GOTO E0Loop
			END IF

		CASE 21%
	!++
	! Abstract:FLD021
	!	^*(21) Form Printed\*
	!	.b
	!	.lm +5
	!	The ^*Form Printed\* field confirms or suppresses the
	!	printing of a purchase order form.  The default setting is "N"
	!	which indicates that the form has not yet been printed.  When the form is
	!	printed the field will change to "Y" to indicate that the form has been
	!	printed at least once.  If it is necessary to reprint a form, change the
	!	field to "N" to indicate that the form has not yet been printed.
	!	.b
	!	Pressing the ^*List Choices\* key will cause the valid choices for
	!	this field to be displayed.  Valid choices are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes (form has been printed)
	!	.te
	!	^*N\* - No  (form has not been printed)
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Confirm Form Print
	!	.x Confirm Form Print>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::PRINTFORM = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;63", TEMP$, &
				PO_ORDERJOUR::PRINTFORM, MFLAG, "'", MVALUE)

		CASE 22% TO 25%
		SCOPE::PRG_ITEM = "FLD022"
	!++
	! Abstract:FLD022
	!	^*(22), (23), (24), (25) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* fields enters up to four lines of notes
	!	in reference to the order.
	!	.b
	!	Pressing the ^*List Choices\* key will cause pre-defined
	!	Notes to be displayed from the Notes Description File.
	!	.b
	!	Each line will accommodate up to forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Journal>Notes
	!	.x Notes>Purchase Order Journal
	!
	!--
			PO_ORDERJOUR::NOTE(MLOOP - 22%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				STR$(MLOOP - 7%) + ";18", TEMP$, &
				PO_ORDERJOUR::NOTE(MLOOP - 22%), &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PO_MAIN_NOTES.ID, "VX  ") = 1%
				THEN
					PO_ORDERJOUR::NOTE(MLOOP - 22%) = &
						PO_NOTES::DESCR
				END IF
				GOTO E0Loop
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		PO_MAIN_ORDERJOUR = 0%

		SELECT MLOOP

		CASE 1%
			IF PO_ORDERJOUR::PO = ""
			THEN
				PO_MAIN_ORDERJOUR = 1%
				GOTO 32767
			END IF

			!
			! See if PO Number is already in use in Journal
			!
			IF MVALUE = "ADD"
			THEN
				WHEN ERROR IN
					GET #PO_ORDERJOUR.CH%, &
						KEY #0% EQ PO_ORDERJOUR::PO + "", &
						REGARDLESS
				USE
					CONTINUE 20350 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				PO_MAIN_ORDERJOUR = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Order Already Exists", 1%)
				GOTO 32767
			END IF

			!
			! Check if this order number already in register file
			!
20350			IF MVALUE = "ADD"
			THEN
				IF PO_READ_REG_LINE(PO_ORDERJOUR::PO, &
					"", "EQ", PO_REG_LINE_READ, &
					PO_REG_SUB_LINE_READ, QTY(), &
					"") = CMC$_NORMAL
				THEN
					CALL ENTR_3MESSAGE(SCOPE, "Order Already Exists in Register", 1%)

					PO_ORDERJOUR::PO	= PO_REG_LINE_READ::PO
					PO_ORDERJOUR::POTYPE	= PO_REG_LINE_READ::PO_TYPE
					PO_ORDERJOUR::PODATE	= PO_REG_LINE_READ::ORDDATE
					PO_ORDERJOUR::VENDOR	= PO_REG_LINE_READ::VENDOR
					PO_ORDERJOUR::FROMLOCATION	= PO_REG_LINE_READ::FROMLOCATION

					SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						PO_ORDERJOUR::PO, 1%, 18%,, SMG$M_BOLD)

					SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						PO_ORDERJOUR::POTYPE, 2%, 18%,, SMG$M_BOLD)

					SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						PO_ORDERJOUR::VENDOR, 3%, 18%,, SMG$M_BOLD)

					SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						PO_ORDERJOUR::FROMLOCATION, 6%, 18%,, SMG$M_BOLD)

					SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						PRNT_DATE(PO_ORDERJOUR::PODATE, 8%), 7%, 18%,, SMG$M_BOLD)

				END IF
			END IF

		CASE 2%
			!
			! Is the input defined?
			!
			PO_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
				PO_ORDERJOUR::POTYPE, PO_TYPE::DESCR, &
				"PO", MLOOP, "PO TYPE", &
				"PO Type", PO_MAIN_TYPE.ID)

		CASE 3%
			!
			! Is the input defined?
			!
			ST%, PO_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
				PO_ORDERJOUR::VENDOR, AP_VENDOR::VENNAM, &
				"PO", MLOOP, "VENDOR NAME", &
				"Vendor Number", AP_MAIN_VENDOR.ID)

			IF ST% <> 0%
			THEN
				AP_VENDOR::POCITY = &
					STRING$(LEN(AP_VENDOR::CITY), A"?"B)

				AP_VENDOR::POSTATE = &
					STRING$(LEN(AP_VENDOR::STATE), A"?"B)

				AP_VENDOR::POZIP = &
					STRING$(LEN(AP_VENDOR::POZIP), A"?"B)
			END IF

			CITYLINE% = LEN(AP_VENDOR::POCITY  + "  " + &
					AP_VENDOR::POSTATE + " "  + &
					AP_VENDOR::POZIP)

			CITYLINE$ = TRM$(AP_VENDOR::POCITY) + ", " + &
					AP_VENDOR::POSTATE  + " "  + &
					TRM$(AP_VENDOR::POZIP)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AP_VENDOR::VENNAM, 20%), 4%, 3%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				CITYLINE$ + SPACE$(CITYLINE% - LEN(CITYLINE$)), &
				5%, 3%,, SMG$M_BOLD)

		CASE 4%
			!
			! Is the input defined?
			!
			ST%, PO_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
				PO_ORDERJOUR::FROMLOCATION, UTL_LOCATION::LOCNAME, &
				"PO", MLOOP, "LOCATION NAME", &
				"Location Number", UTL_MAIN_LOCATION.ID)

			!
			! If we're adding and the location is defined,
			! then load in and display defaults for the to address
			!
			IF (MVALUE = "ADD") AND (ST% = 0%)
			THEN
				PO_ORDERJOUR::TONAME	= UTL_LOCATION::LOCNAME
				PO_ORDERJOUR::TOADD1	= UTL_LOCATION::SHPADDRESS1
				PO_ORDERJOUR::TOADD2	= UTL_LOCATION::SHPADDRESS2
				PO_ORDERJOUR::TOCITY	= UTL_LOCATION::SHPCITY
				PO_ORDERJOUR::TOSTATE	= UTL_LOCATION::SHPSTATE
				PO_ORDERJOUR::TOZIP	= UTL_LOCATION::SHPZIP
				PO_ORDERJOUR::TOCOUNTRY	= UTL_LOCATION::SHPCOUNTRY

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					PO_ORDERJOUR::TONAME, &
					2%, 50%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					PO_ORDERJOUR::TOADD1, &
					3%, 50%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					PO_ORDERJOUR::TOADD2, &
					4%, 50%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					PO_ORDERJOUR::TOCITY, &
					5%, 50%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					PO_ORDERJOUR::TOSTATE, &
					6%, 50%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					PO_ORDERJOUR::TOZIP, &
					7%, 50%, , SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					PO_ORDERJOUR::TOCOUNTRY, &
					8%, 50%, , SMG$M_BOLD)
			END IF

		CASE 7%
			!
			! Entry required in this field
			!
			IF PO_ORDERJOUR::OPERATOR = ""
			THEN
				PO_MAIN_ORDERJOUR = 1%
			END IF

		CASE 12%
			!
			! Is the input defined?
			!
			PO_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
				"US" + PO_ORDERJOUR::TOSTATE, &
				UTL_STATE::DESCR, &
				"PO", MLOOP, "STATE CODE", &
				"State code", UTL_MAIN_STATE.ID)

		CASE 17%
			!
			! Is the input defined?
			!
			IF PO_ORDERJOUR::TERMS <> ""
			THEN
				PO_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
					PO_ORDERJOUR::TERMS, UTL_TERMS::DESCR, &
					"PO", MLOOP, "TERMS", &
					"Terms", UT_MAIN_TERMS.ID)
			END IF

		CASE 18%
			!
			! Is the input defined?
			!
			IF PO_ORDERJOUR::CARRIER <> ""
			THEN
				PO_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
					PO_ORDERJOUR::CARRIER, &
					UTL_CARRIER::DESCR, &
					"PO", MLOOP, "CARRIER CODE", &
					"Carrier Code", UT_MAIN_CARRIER.ID)
			END IF

		CASE 19%
			!
			! Is the input defined?
			!
			IF PO_ORDERJOUR::FOB <> ""
			THEN
				PO_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
					PO_ORDERJOUR::FOB, UTL_FOB::DESCR, &
					"PO", MLOOP, "FOB CODE", &
					"FOB Code", UT_MAIN_FOB.ID)
			END IF

		CASE 20%
			!
			! Is the input defined?
			!
			IF PO_ORDERJOUR::ACKNOW <> ""
			THEN
				PO_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
					PO_ORDERJOUR::ACKNOW, &
					PO_ACKNOWLEDGE::DESCR, &
					"PO", MLOOP, "ACKNOWLEDGEMENT CODE", &
					"Acknowledgement code", &
					PO_MAIN_ACKNOWLEDGE.ID)
			END IF

		END SELECT

	%PAGE

	!
	! Display additional information
	!
	CASE OPT_DISPLAY
		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			CITYLINE% = LEN(AP_VENDOR::POCITY  + "  " + &
				AP_VENDOR::POSTATE + " "  + &
				AP_VENDOR::POZIP)

			ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, &
				"Q0" + PO_ORDERJOUR::VENDOR)

			IF ST% <> 1%
			THEN
				AP_VENDOR::VENNAM  = &
					STRING$(LEN(AP_VENDOR::VENNAM), A"?"B)

				AP_VENDOR::POCITY  = &
					STRING$(LEN(AP_VENDOR::POCITY), A"?"B)

				AP_VENDOR::POSTATE  = &
					STRING$(LEN(AP_VENDOR::POSTATE), A"?"B)

				AP_VENDOR::POZIP  = &
					STRING$(LEN(AP_VENDOR::POZIP), A"?"B)
			END IF

			CITYLINE$ = TRM$(AP_VENDOR::POCITY) + ", " + &
				AP_VENDOR::POSTATE  + " "  + &
				TRM$(AP_VENDOR::POZIP)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AP_VENDOR::VENNAM, 20%), 4%, 3%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				SPACE$(30%), 5%, 3%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				CITYLINE$ + SPACE$(CITYLINE% - LEN(CITYLINE$)), &
				5%, 3%,, SMG$M_BOLD)

		END IF

	!
	! Set PO_ORDERJOUR_OLD value
	!
20500	CASE OPT_SETOLD
		PO_ORDERJOUR_OLD = PO_ORDERJOUR

	!
	! Restore PO_ORDERJOUR_OLD value
	!
	CASE OPT_RESETOLD
		PO_ORDERJOUR = PO_ORDERJOUR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PO_ORDERJOUR2 = PO_ORDERJOUR

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PO_ORDERJOUR = PO_ORDERJOUR2

		IF MFLAG = 1%
		THEN
			GOSUB GetRec IF PO_ORDERJOUR::PO = ""

			PO_ORDERJOUR::PODATE = DATE_TODAY &
				IF PO_ORDERJOUR::PODATE = ""

			PO_ORDERJOUR::TOCOUNTRY = "US" &
				IF PO_ORDERJOUR::TOCOUNTRY = ""

			PO_ORDERJOUR::PRINTFORM = "N" &
				IF PO_ORDERJOUR::PRINTFORM = ""
		END IF

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  PO Number   PType Vendor     Loc  " + &
				"Date     C/P Terms Car FOB Ack PrntPO"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "014,020,031,036,045,049,055,059,063,067"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = CONV_STRING(PO_ORDERJOUR::PO, CMC$_LEFT) + "  "   + &
				PO_ORDERJOUR::POTYPE + "    " + &
				PO_ORDERJOUR::VENDOR + " "    + &
				PO_ORDERJOUR::FROMLOCATION + " "    + &
				PRNT_DATE(PO_ORDERJOUR::PODATE, 6%) + " "    + &
				PO_ORDERJOUR::COL_PPD + "   "  + &
				PO_ORDERJOUR::TERMS + "    " + &
				PO_ORDERJOUR::CARRIER + "  "   + &
				PO_ORDERJOUR::FOB + "  "   + &
				PO_ORDERJOUR::ACKNOW + "  "   + &
				PO_ORDERJOUR::PRINTFORM

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #PO_ORDERJOUR.CH%, &
				KEY #0% GE PO_ORDERJOUR::PO + "", &
				REGARDLESS

		CASE 1%
			FIND #PO_ORDERJOUR.CH%, &
				KEY #1% GE PO_ORDERJOUR::VENDOR + &
				PO_ORDERJOUR::PO, REGARDLESS

		CASE 2%
			FIND #PO_ORDERJOUR.CH%, &
				KEY #2% GE PO_ORDERJOUR::POTYPE + &
				PO_ORDERJOUR::PO, REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

	%PAGE

 GetRec:
	!
	! Open control file
	!
20800	IF PO_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.CRE"
		USE
			CONTINUE 20850 IF ERR = 9% OR ERR = 155%
			EXIT HANDLER
		END WHEN
	END IF


	!
	! Get the PO_CONTROL record
	!
	WHEN ERROR IN
		GET #PO_CONTROL.CH%, RECORD 1%
		V% = FUNC_INCREMENT(PO_CONTROL::LAST_PO)
		UPDATE #PO_CONTROL.CH%
	USE
		CONTINUE 20850 IF ERR = 9% OR ERR = 155%
		EXIT HANDLER
	END WHEN

	GOTO AssgNumber

20850	PO_CONTROL::LAST_PO = "1"

	PUT #PO_CONTROL.CH%

 AssgNumber:
	PO_ORDERJOUR::PO = PO_CONTROL::LAST_PO

	UNLOCK #PO_CONTROL.CH%

	RETURN

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

29900	!
	! Handle no header for Line function here
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Sorry, but there is no current header item", 0%)

32767	END FUNCTION
