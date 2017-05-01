1	%TITLE "Accounts Payable Vendor Maintenance"
	%SBTTL "AP_MAIN_VENDOR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_VENDOR(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.P
	!	Specific information relative to each vendor is entered and maintained through
	!	the ^*Maintain Vendor File\* option.
	!
	! Index:
	!	.x Vendor>Maintenance
	!
	! Option:
	!
	!	AP_MAIN_VENDOR$CONTACT
	!
	! COMPILE:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_VENDOR/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_VENDOR
	!	$ DELETE AP_MAIN_VENDOR.OBJ;*
	!
	! Author:
	!
	!	07/29/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	05/19/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	07/26/89 - Kevin Handy
	!		Completed implementation of checking into
	!		AP_OPEN and AP_CLOSED files to disable deleting
	!		or changing active vendors.
	!
	!	05/24/91 - Kevin Handy
	!		Modified so that if open or closed file did not
	!		exist, the program would not die with an error 5.
	!
	!	07/03/91 - Craig Tanner
	!		Added list of choises to country field.
	!
	!	08/05/91 - Frank F. Starman
	!		Check for a valid due date and pad zero.
	!
	!	11/18/93 - Kevin Handy
	!		Fixed bug in testing for eraseing a vendor.
	!		Wouldn't alwayes test correctly.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/19/95 - Kevin Handy
	!		Fix format parameter in ENTR_3PHONE.
	!
	!	10/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	08/24/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD	AP_VENDOR
	MAP (AP_VENDOR_OLD)	AP_VENDOR_CDD	AP_VENDOR_OLD, AP_VENDOR2

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP (AP_CLOSE)		AP_CLOSE_CDD	AP_CLOSE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)		UTL_COUNTRY_CDD		UTL_COUNTRY

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AP_VENDOR) &
		AP_VENDOR.CH%

	COM (CH_AP_OPEN) &
		AP_OPEN.CH%

	COM (CH_AP_CLOSE) &
		AP_CLOSE.CH%

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	%PAGE

	SELECT MOPTION

	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Accounts Payable Vendor Maintenance"
		SMG_WINDOW::NHELP = "AP_MAIN_VENDOR"
		SMG_WINDOW::CHAN  = AP_VENDOR.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 25%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 3%
		SMG_WINDOW::KNAME(0%) = "Vendor-number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Name-vendor"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
		SMG_WINDOW::KNAME(2%) = "Alpha-sort"
			SMG_WINDOW::KFIELD(2%, 0%) = 1%
			SMG_WINDOW::KFIELD(2%, 1%) = 3%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF AP_VENDOR.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_VENDOR.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AP_MAIN_VENDOR = ERR
			CONTINUE 770
		END WHEN

		AP_VENDOR.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
		USE
			AP_MAIN_VENDOR = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AP_VENDOR.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AP_VENDOR.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = AP_VENDOR.CH%
		RESET #AP_VENDOR.CH%
		WHEN ERROR IN
			GET #AP_VENDOR.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

	!
	! Modify the menu
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " conTact "

	!
	! Optional menu items
	!
5000	CASE OPT_MOREMENU
		SELECT SCOPE::PRG_ITEM

		!
		! Line option
		!
		CASE "conTact"
	!++
	! Abstract:CONTACT
	!	^*Contact\*
	!	.p
	!	^*Contact\* contains information concerning the person the company
	!	will reach regarding questions and problems with that particular vendor.
	!
	! Index:
	!	.x Contact>Vendor Maintenance
	!	.x Vendor Maintenance>Contact
	!
	!--
			!
			! Make sure there is a header
			!
			V% = MAIN_JOURNAL(AP_MAIN_CONTACT.ID, "")
		END SELECT

20100	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	02, 02, "(01) Vendor Number", &
			03, 02, "(02) Vendor Name", &
			02, 38, "(03) Alpha Sort", &
			06, 02, "(04) Addr", &
			07, 02, "(05) Addr", &
			08, 02, "(06) City", &
			09, 02, "(07) State", &
			10, 02, "(08) Zip", &
			11, 02, "(09) Country", &
			12, 02, "(10) Phone", &
			06, 38, "(11) PO Addr", &
			07, 38, "(12) PO Addr", &
			08, 38, "(13) PO City", &
			09, 38, "(14) PO State", &
			10, 38, "(15) PO Zip", &
			11, 38, "(16) PO Country", &
			12, 38, "(17) PO Phone", &
			14, 02, "(18) Federal ID", &
			15, 02, "(19) 1099(Y/N)", &
			16, 02, "(20) Purge(Y/N)", &
			15, 22, "(21) Due Days", &
			16, 22, "(22) Due Date", &
			14, 47, "(23) Discount Days", &
			15, 47, "(24) Discount Date", &
			16, 47, "(25) Discount %", &
			05, 07, "REMITTANCE ADDRESS", &
			05, 43, "PURCHASE ORDER ADDRESS", &
			16, 69, "%", &
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

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Vendor Number\*
	!	.P
	!	The ^*Vendor Number\* field assigns an identification
	!	number of one (1) to ten (10) alphanumeric characters which will be used
	!	for referencing a vendor. It is recommended that all vendor numbers
	!	be the same length.
	!
	! Index:
	!	.x Vendor>Number
	!	.x Number>Vendor
	!
	!--

			AP_VENDOR::VENNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;21", TEMP$, &
				AP_VENDOR::VENNUM, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Vendor Name\*
	!	.P
	!	The ^*Vendor Name\* field contains the name of the vendor
	!	as it is to appear on all reports including any checks which will
	!	be written to the vendor.
	!	.p
	!	The field will contain up to forty (40)
	!	alphanumeric characters.
	!
	! Index:
	!	.x Vendor>Name
	!	.x Name>Vendor
	!
	!--

			AP_VENDOR::VENNAM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;21", TEMP$, &
				AP_VENDOR::VENNAM, MFLAG, "'E", &
				MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Alpha Sort\*
	!	.P
	!	The ^*Alpha Sort\* field contains the vendor name
	!	which will be referenced when a request is made to print certain
	!	reports in alphabetical order. The field will accommodate up to
	!	fifteen (15) alphanumeric characters.
	!	.p
	!	The field will automatically default to the value of the first
	!	fifteen (15) characters in the vendor name field, which in many cases
	!	would be the appropriate alpha sort value. In the case of vendor
	!	names such as "The City of Bayside" or "James Brown", the alpha sort
	!	fields would be edited to contain the values of "Bayside City" and
	!	"Brown James", respectively. In order to achieve more accurate sorts,
	!	it is recommended that articles, prepositions, and special characters,
	!	such as commas, apostrophes, dashes and ampersands, etc. be omitted from
	!	the alpha sort field.
	!
	! Index:
	!	.x Alpha Sort>Vendor
	!	.x Vendor>Alpha Sort
	!
	!--

			AP_VENDOR::ALPSRT = EDIT$(AP_VENDOR::VENNAM, 32%) &
				IF (TEMP1$ = "Add") AND &
				((MFLAG AND 1%) = 0%) AND &
				(AP_VENDOR::ALPSRT = "")

			AP_VENDOR::ALPSRT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;57", TEMP$, &
				AP_VENDOR::ALPSRT, MFLAG, &
				"'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Address\*
	!	.P
	!	This ^*Address\* field contains the first line street address to which
	!	remittances will be mailed. It is recommended that for a one line
	!	street address this field be left blank and field (5) be used instead.
	!	.P
	!	This field will contain up to twenty-five (25) alphanumeric
	!	characters.
	!
	! Index:
	!	.x Address>Vendor
	!	.x Vendor>Address
	!
	!--

			AP_VENDOR::ADD1 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;12", TEMP$, &
				AP_VENDOR::ADD1, MFLAG, "'E", &
				MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Address\*
	!	.P
	!	This ^*Address\* field contains the second line street address to
	!	which remittances will be mailed. It is recommended that this
	!	field be used if there is a single line street address.
	!	.P
	!	This field will contain up to twenty-one (21) alphanumeric
	!	characters.
	!
	! Index:
	!	.x Address>Vendor
	!	.x Vendor>Address
	!
	!--

			AP_VENDOR::ADD2 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;12", TEMP$, &
				AP_VENDOR::ADD2, MFLAG, "'E", &
				MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) City\*
	!	.P
	!	This ^*City\* field is to contain the name of the city to which each remittance
	!	will be mailed.
	!	.p
	!	The field will contain up to fifteen (15) alphanumeric characters.
	!
	! Index:
	!	.x City>Vendor
	!	.x Vendor>City
	!
	!--

			AP_VENDOR::CITY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;18", TEMP$, &
				AP_VENDOR::CITY, MFLAG, "'E", &
				MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) State\*
	!	.P
	!	The ^*State\* field contains the state to which remittances
	!	will be mailed.
	!	.p
	!	The standard two (2) character state or province postal codes should be used.
	!
	! Index:
	!	.x State>Vendor
	!	.x Vendor>State
	!
	!--

			AP_VENDOR::STATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;18", TEMP$, &
				AP_VENDOR::STATE, MFLAG, "'E", &
				MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Zip\*
	!	.p
	!	The ^*Zip\* field contains the zip or postal code to which remittances will
	!	be mailed.
	!	.p
	!	This field will contain up to ten (10) alphanumeric
	!	characters.
	!
	! Index:
	!	.x Zip Code>Vendor
	!	.x Vendor>Zip Code
	!
	!--

			AP_VENDOR::ZIP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;18", TEMP$, &
				AP_VENDOR::ZIP, MFLAG, "'E", &
				MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Country\*
	!	.p
	!	The ^*Country\* field contains the country to which
	!	remittances will be mailed when country designation is necessary.
	!	.p
	!	This field will contain up to two (2) alphabetic characters.
	!
	! Index:
	!	.x Country>Vendor
	!	.x Vendor>Country
	!
	!--

			AP_VENDOR::COUNTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;18", TEMP$, &
				LEFT$(AP_VENDOR::COUNTRY, 2%), MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "V0" + &
					LEFT$(AP_VENDOR::COUNTRY, 2%)) = 1%
				THEN
					AP_VENDOR::COUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO Reenter
			END IF

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Phone\*
	!	.P
	!	The ^*Phone\* field contains the vendor's accounts
	!	receivable office phone number.
	!	.p
	!	The field will contain up to ten (10) numeric characters.  It is not necessary
	!	to enter special characters when entering a phone number, they will
	!	automatically be inserted by the system.
	!
	! Index:
	!	.x Phone>Vendor
	!	.x Telephone>Vendor
	!	.x Vendor>Phone
	!	.x Vendor>Telephone
	!
	!--

			AP_VENDOR::PHONE = ENTR_3PHONE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;18", TEMP$, AP_VENDOR::PHONE, &
				MFLAG, 0%, MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	^*(11) Purchase Order Address\*
	!	.P
	!	The ^*Address\* field should contain the first line street address to
	!	which purchase orders will be mailed. It is recommended that, if there
	!	is only one line in the street address, this field be left blank and field
	!	(12) be used to enter the street address.
	!	.P
	!	This field will contain up to twenty-five (25) alphanumeric characters.
	!
	! Index:
	!	.x Address>Purchase Order>Vendor
	!	.x Purchase Order>Address>Vendor
	!	.x Vendor>Purchase Order>Address
	!
	!--

			AP_VENDOR::POADD1 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;51", TEMP$, &
				AP_VENDOR::POADD1, MFLAG, "'E", &
				MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	^*(12) Purchase Order Address\*
	!	.P
	!	The ^*Address\* field is meant to contain the second line of the street
	!	address to which the purchase order will be mailed. It is recommended that
	!	this field be used also in the event there is a single line street address.
	!	.P
	!	This field will contain up to twenty-one (21) alphanumeric characters.
	!
	! Index:
	!	.x Address>Purchase Order>Vendor
	!	.x Purchase Order>Address>Vendor
	!	.x Vendor>Purchase Order>Address
	!
	!--

			AP_VENDOR::POADD2 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;51", TEMP$, &
				AP_VENDOR::POADD2, MFLAG, "'E", &
				MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	^*(13) Purchase Order City\*
	!	.P
	!	The ^*City\* field contains the city to which purchase
	!	orders will be mailed.
	!	.p
	!	The field will contain up to fifteen (15)
	!	alphanumeric characters.
	!
	! Index:
	!	.x Purchase Order>City>Vendor
	!	.x City>Purchase Order>Vendor
	!	.x Vendor>Purchase Order>City
	!
	!--

			AP_VENDOR::POCITY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;57", TEMP$, &
				AP_VENDOR::POCITY, MFLAG, "'E", &
				MVALUE)

		CASE 14%

	!++
	! Abstract:FLD014
	!	^*(14) Purchase Order State\*
	!	.P
	!	The ^*State\* field contains the state or province to
	!	which purchase orders will be sent.
	!	.p
	!	Standard two (2) character state or province postal codes should be
	!	used.
	!
	! Index:
	!	.x Purchase Order>State>Vendor
	!	.x State>Purchase Order>Vendor
	!	.x Vendor>Purchase Order>State
	!
	!--

			AP_VENDOR::POSTATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;57", TEMP$, &
				AP_VENDOR::POSTATE, MFLAG, "'E", &
				MVALUE)

		CASE 15%

	!++
	! Abstract:FLD015
	!	^*(15) Purchase Order Zip Code\*
	!	.p
	!	The ^*Zip Code\* field should contain the zip or postal code to which
	!	purchase orders will be mailed.
	!	.p
	!	This field will contain up to ten (10) alphanumeric characters.
	!
	! Index:
	!	.x Purchase Order>Zip Code>Vendor
	!	.x Zip Code>Purchase Order>Vendor
	!	.x Vendor>Purchase Order>Zip Code
	!
	!--

			AP_VENDOR::POZIP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;57", TEMP$, &
				AP_VENDOR::POZIP, MFLAG, "'E", &
				MVALUE)

		CASE 16%

	!++
	! Abstract:FLD016
	!	^*(16) Purchase Order Country\*
	!	.p
	!	The ^*Country\* field contains the country to which
	!	purchase orders will be mailed when country designation is necessary.
	!	.p
	!	This field will contain up to two (2) alphabetic characters.
	!
	! Index:
	!	.x Country>Purchase Order>Vendor
	!	.x Purchase Order>Country>Vendor
	!	.x Vendor>Purchase Order>Country
	!
	!--

			AP_VENDOR::POCOUNTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;57", TEMP$, &
				AP_VENDOR::POCOUNTRY, MFLAG, "'E", &
				MVALUE)

		CASE 17%

	!++
	! Abstract:FLD017
	!	^*(17) Purchase Order Phone\*
	!	.P
	!	The ^*Phone\* field should contain a vendor's sales or order entry office
	!	phone number.
	!	.p
	!	The field will accommodate ten (10) numeric characters.  It is not necessary
	!	to enter special characters when entering a phone number.  The special
	!	characters will automatically be inserted by the system.
	!
	! Index:
	!	.x Phone>Purchase Order>Vendor
	!	.x Purchase Order>Phone>Vendor
	!	.x Vendor>Purchase Order>Phone
	!
	!--

			AP_VENDOR::POPHONE = ENTR_3PHONE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;57", TEMP$, AP_VENDOR::POPHONE, &
				MFLAG, 0%, MVALUE)

		CASE 18%

	!++
	! Abstract:FLD018
	!	^*(18) Federal Identification\*
	!	.P
	!	The ^*Federal Identification\* field contains the a social
	!	security number or other Federal Identification number of a vendor for whom a
	!	Form 1099 will be printed at year end.
	!	.P
	!	This field will contain thirteen (13) alphanumeric characters.
	!
	! Index:
	!	.x Federal Identification>Vendor
	!	.x Vendor>Federal Identification
	!
	!--

			AP_VENDOR::FEDID = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;18", TEMP$, &
				AP_VENDOR::FEDID, MFLAG, "'E", &
				MVALUE)

		CASE 19%

	!++
	! Abstract:FLD019
	!	^*(19) 1099 Flag\*
	!	.p
	!	The ^*1099\* flag will default to ^*N\* for No. If a vendor is to
	!	receive a Form 1099 at year end, this field should contain a ^*Y\* for
	!	Yes.
	!	.note
	!	If the ^*1099\* flag is set to ^*Y\*, field 18 (^*Federal Identification\*)
	!	should contain the appropriate Federal ID or Social Security number.
	!	.end note
	!
	! Index:
	!	.x 1099 Flag>Vendor
	!	.x Vendor>1099 Flag
	!
	!--

			AP_VENDOR::FLG1099 = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;18", TEMP$, &
				AP_VENDOR::FLG1099, MFLAG, "!", &
				MVALUE)

		CASE 20%

	!++
	! Abstract:FLD020
	!	^*(20) Purge Flag\*
	!	.P
	!	The ^*Purge\* flag will default to ^*N\* for No when entering a new
	!	vendor record. This flag would be set to ^*Y\* for Yes only when a
	!	vendor would become inactive, when there would be no open items
	!	in the Accounts Payable Open file, and when it would no longer be
	!	necessary to retain any information in the Accounts Payable History
	!	file.
	!
	! Index:
	!	.x Purge Flag>Vendor
	!	.x Vendor>Purge Flag
	!
	!--

			AP_VENDOR::PURGE = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;18", TEMP$, &
				AP_VENDOR::PURGE, MFLAG, "!", &
				MVALUE)

		CASE 21%

	!++
	! Abstract:FLD021
	!	^*(21) Due Days\*
	!	.p
	!	The ^*Due Days\* field defines a vendor's terms in
	!	reference to the number of elapsed days until an invoice is due, as in "N/10".
	!	A significant value in this field will cause the system to automatically
	!	calculate the due date (by adding the field to the invoice
	!	date) and enter the calculated date in the "due date" field in the purchases
	!	journal record. The calculated date may be overridden in the purchases journal
	!	by entering an alternative date.
	!	.p
	!	This field will contain up to four (4) numeric characters.
	!	.note
	!	The terms specifying when a vendor's invoices are due should be entered in
	!	either this field ^&or\& field (22) Due Date, but ^&not\&#^&both\&.
	!	.end note
	!
	! Index:
	!	.x Due Days>Terms>Vendor
	!	.x Terms>Due Days>Vendor
	!	.x Vendor>Terms>Due Days
	!
	!--

			AP_VENDOR::DUEDAYS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;36", TEMP$, &
				AP_VENDOR::DUEDAYS * 1.0, MFLAG, &
				"####", MVALUE)

		CASE 22%

	!++
	! Abstract:FLD022
	!	^*(22) Due Date\*
	!	.p
	!	The ^*Due Date\* field defines a vendor's terms
	!	when charges are due on a specified day of the month, as in "N/10th".
	!	A significant value in this field will cause the system to automatically
	!	determine the due date to be the specified day of the month following
	!	the invoice date and enter that date in the purchases journal record.
	!	The calculated date may be overridden in the purchases journal by
	!	entering an alternative date.
	!	.p
	!	This field will contain up to two (2) numeric characters.
	!	.note
	!	The terms specifying when a vendor's invoices are due
	!	should be entered in either this field ^&or\& field (21)
	!	Due Days, but ^&not\&#^&both\&.
	!	.end note
	!
	! Index:
	!	.x Due Date>Terms>Vendor
	!	.x Terms>Due Date>Vendor
	!	.x Vendor>Terms>Due Date
	!
	!--

			AP_VENDOR::DUEDATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;36", TEMP$, &
				AP_VENDOR::DUEDATE, MFLAG, "~L0'E", &
				MVALUE)

		CASE 23%

	!++
	! Abstract:FLD023
	!	^*(23) Discount Days\*
	!	.p
	!	The ^*Discount Days\* field defines a vendor's
	!	discount terms when discounts may be taken if payment is made within
	!	a specified number of days after the invoice date, as in "2/10". A
	!	significant value in this field will cause the system to automatically
	!	calculate the discount date (by adding the field to
	!	the invoice date) and enter the calculated date in the purchases
	!	journal record. The calculated date may be overridden in the
	!	purchases journal by entering an alternative date.
	!	.p
	!	This field will contain up to four (4) numeric characters.
	!	.note
	!	The terms specifying a vendor's discount policy should
	!	be entered in either this field ^&or\& field (24) Discount
	!	Date, but ^&not\&#^&both\&.
	!	.end note
	!
	! Index:
	!	.x Discount Days>Vendor
	!	.x Vendor>Discount Days
	!
	!--

			AP_VENDOR::DISDAYS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;66", TEMP$, &
				AP_VENDOR::DISDAYS * 1.0, MFLAG, &
				"####", MVALUE)

		CASE 24%

	!++
	! Abstract:FLD024
	!	^*(24) Discount Date\*
	!	.p
	!	The ^*Discount Date\* field defines a vendor's
	!	discount terms when discounts may be taken if payment is made on or
	!	before a specified date, as in "2/10th". A significant value in
	!	this field will cause the system to automatically determine the
	!	discount date to be the next occurrance of the designated date and
	!	enter that date in the purchases journal record. The determined
	!	date may be overridden in the purchases journal by entering an
	!	alternative date.
	!	.p
	!	The field will contain up to two (2) numeric characters.
	!	.note
	!	The terms specifying a vendor's discount policy should
	!	be entered in either this field ^&or\& field (23) Discount
	!	Days, but ^&not\&#^&both\&.
	!	.end note
	!
	! Index:
	!	.x Discount Date>Vendor
	!	.x Vendor>Discount Date
	!
	!--

			AP_VENDOR::DISDATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;66", TEMP$, &
				AP_VENDOR::DISDATE, MFLAG, "'E", &
				MVALUE)

		CASE 25%

	!++
	! Abstract:FLD025
	!	^*(25) Discount %\*
	!	.P
	!	The ^*Discount %\* field defines a vendor's
	!	standard discount percentage. A significant value in this field
	!	will cause the system to automatically calculate the discount
	!	amount and enter the calculated amount in the purchases journal
	!	record. The calculated discount amount may be overridden in the
	!	purchases journal by entering an alternative amount.
	!	.P
	!	This field will contain up to three (3) characters including
	!	a decimal point.
	!
	! Index:
	!	.x Discount>Percent>Vendor
	!	.x Percent>Discount>Vendor
	!	.x Vendor>Discount>Percent
	!
	!--

			AP_VENDOR::DISCPER = (ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;66", TEMP$, &
				AP_VENDOR::DISCPER * .01, MFLAG, &
				"###.##", MVALUE)) * 100.

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AP_MAIN_VENDOR = 0%

		SELECT MLOOP

		CASE 1%
			IF AP_VENDOR::VENNUM = ""
			THEN
				AP_MAIN_VENDOR = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #AP_VENDOR.CH%, &
							KEY #0% EQ AP_VENDOR::VENNUM + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AP_MAIN_VENDOR = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)
				ELSE
					IF (MVALUE = "CHANGE") AND AP_VENDOR_OLD::VENNUM <> AP_VENDOR::VENNUM
					THEN
						GOSUB 28000

						IF ACTIVE_FLAG% <> 0%
						THEN
							CALL ENTR_3MESSAGE(SCOPE, "Vendor has activity.  " + &
								"Change not allowed", 0%)
							AP_MAIN_VENDOR = 2%
						END IF
					END IF
				END IF
			END IF

		CASE 22%

20310			WHEN ERROR IN
				V% = VAL%(AP_VENDOR::DUEDATE)
			USE
				CONTINUE 20320
			END WHEN
			GOTO ExitFunction

20320			CALL ENTR_3MESSAGE(SCOPE, "Invalid Due Date", 1%)
			AP_MAIN_VENDOR = 1%

		END SELECT

	!
	! Test option
	!
	CASE OPT_TESTOPT
		AP_MAIN_VENDOR = 0%

		IF EDIT$(MVALUE, -1%) = "ERASE"
		THEN
			AP_VENDOR_OLD::VENNUM = AP_VENDOR::VENNUM

			GOSUB 28000

			IF ACTIVE_FLAG% <> 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, "Vendor has activity.  " + &
					"Erase not allowed", 0%)
				AP_MAIN_VENDOR = 1%
			END IF
		END IF

20500	CASE OPT_SETOLD
		AP_VENDOR_OLD = AP_VENDOR

	!
	! Restore AP_VENDOR_OLD value
	!
	CASE OPT_RESETOLD
		AP_VENDOR = AP_VENDOR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AP_VENDOR2 = AP_VENDOR

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AP_VENDOR = AP_VENDOR2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
				MVALUE = " Ven Number   " + &
				"Vendor Name                             " + &
				"Alpha Sort"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,054"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AP_VENDOR::VENNUM + "  " + &
				AP_VENDOR::VENNAM + " " + &
				AP_VENDOR::ALPSRT

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

			CASE 0%
				FIND #AP_VENDOR.CH%, KEY #0% &
					GE AP_VENDOR::VENNUM + "", REGARDLESS
			CASE 1%
				FIND #AP_VENDOR.CH%, KEY #1% &
					GE AP_VENDOR::VENNAM + "", REGARDLESS
			CASE 2%
				FIND #AP_VENDOR.CH%, KEY #2% &
					GE AP_VENDOR::ALPSRT + "", REGARDLESS

		END SELECT
	END SELECT

 ExitFunction:
	EXIT FUNCTION

28000	!****************************************************************
	! Test to see if this is an active vendor number
	!****************************************************************

	ACTIVE_FLAG% = 0%

	!
	! Search open file (open if necessary)
	!
	WHEN ERROR IN
		IF AP_OPEN.CH% = 0%
		THEN
			%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
		END IF

		FIND #AP_OPEN.CH%, &
			KEY #0% EQ AP_VENDOR_OLD::VENNUM + "", &
			REGARDLESS
	USE
		CONTINUE 28010 IF ERR = 155% OR ERR = 9% OR ERR = 5%
		EXIT HANDLER
	END WHEN

	AP_VENDOR::VENNUM = AP_VENDOR_OLD::VENNUM

	ACTIVE_FLAG% = -1%

	RETURN

28010	!
	! Search closed file (open if necessary)
	!
	WHEN ERROR IN
		IF AP_CLOSE.CH% = 0%
		THEN
			%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.OPN"
		END IF

		FIND #AP_CLOSE.CH%, &
			KEY #0% EQ AP_VENDOR_OLD::VENNUM + "", &
			REGARDLESS
	USE
		CONTINUE 28090 IF ERR = 155% OR ERR = 9% OR ERR = 5%
		EXIT HANDLER
	END WHEN

	AP_VENDOR::VENNUM = AP_VENDOR_OLD::VENNUM

	ACTIVE_FLAG% = -1%

28090	RETURN

29000	!******************************************************************
	! Trap errors
	!******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
