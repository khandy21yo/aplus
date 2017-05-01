1	%TITLE "Maintain Name/Address file"
	%SBTTL "AR_MAIN_35CUSTOM"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_35CUSTOM(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
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
	!	The ^*Maintain Name/Address File\* option
	!	maintains pertinent information relative to each customer.
	!	.lm -5
	!
	! Index:
	!	.x Masterfile>Maintain
	!	.x Customer>Maintain Masterfile
	!	.x Maintain>Customer Masterfile
	!	.x Address>Maintain Customer
	!	.x Maintain>Customer Address
	!	.x Add>Customer
	!	.x Erase>Customer
	!	.x Change>Customer
	!	.x Customer>Add
	!	.x Customer>Erase
	!	.x Customer>Change
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_35CUSTOM/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AR_MAIN_35CUSTOM
	!	$ DELETE AR_MAIN_35CUSTOM.OBJ;*
	!
	! Author:
	!
	!	06/19/90 - Lance Williams
	!
	! Modification history:
	!
	!	08/19/91 - Frank F. Starman
	!		Check for F17 key.
	!
	!	08/29/91 - Dan Perkins
	!		Added F14 key to "state" field.
	!		Changed "V0" keys to "VX".
	!
	!	11/06/91 - Kevin Handy
	!		Added '%' to key #'s in "find"s and "get"s.
	!
	!	11/06/91 - Kevin Handy
	!		Fixed bug where the state would default to "US"
	!		on an add.  Changed the country to default to "US".
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/01/92 - Kevin Handy
	!		Added test for Location and Terms (24 and 25).
	!
	!	12/21/92 - Kevin Handy
	!		Fixed bug in looking up location.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/18/93 - Kevin Handy
	!		Fixed bug where the erase did not always check
	!		for open/close information correctly.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/19/95 - Kevin Handy
	!		Fix format paramater in call to ENTR_3PHONE.
	!
	!	05/26/95 - Kevin Handy
	!		Added ability to flag cash customers with a
	!		"C" for the status code instead of a "A".
	!
	!	06/07/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! Maps and CDD
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM
	MAP (AR_35CUSTOM_OLD)	AR_35CUSTOM_CDD		AR_35CUSTOM_OLD
	MAP (AR_35CUSTOM_DEF)	AR_35CUSTOM_CDD		AR_35CUSTOM_DEF

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSTYPE.HB"
	MAP (AR_CUSTYPE)	AR_CUSTYPE_CDD		AR_CUSTYPE

	%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.HB"
	MAP (OE_CATEGORY)	OE_CATEGORY_CDD		OE_CATEGORY

	%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.HB"
	MAP (OE_SALESTAX)	OE_SALESTAX_CDD		OE_SALESTAX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.HB"
	MAP (UTL_STATE)		UTL_STATE_CDD		UTL_STATE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD		AR_CLOSED

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AR_35CUSTOM) &
		AR_35CUSTOM.CH%, &
		AR_35CUSTOM.READONLY%

	COM (TT_AR_MAIN_35CUSTOM) &
		ECTITLE$ = 32%, &
		EC$(2%) = 32%, &
		STITLE$ = 30%, &
		SSTAT$(7%) = 30%

	COM (TT_AR_SALTAX) &
		TAXTITLE$ = 20%, &
		TAXTYPE$(5%) = 40%


	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION FUNC_TESTENTRY

	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS

	%PAGE

	ON ERROR GOTO 29000

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
		SMG_WINDOW::DESCR = "Accounts Receivable Customer"
		SMG_WINDOW::NHELP = "AR_MAIN_35CUSTOM"
		SMG_WINDOW::CHAN  = AR_35CUSTOM.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 31%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::LWIDTH  = 78%
		SMG_WINDOW::LHEIGHT = 15%
		SMG_WINDOW::LHPOS   = 2%
		SMG_WINDOW::LVPOS   = 5%
		SMG_WINDOW::LLAST   = 1%
		SMG_WINDOW::LTITLE(0%) = "First Page"
		SMG_WINDOW::LPAGE(0%) = 20%
		SMG_WINDOW::LTITLE(1%) = "Last Page"
		SMG_WINDOW::LPAGE(1%) = 31%

		SMG_WINDOW::NKEYS = 4%
		SMG_WINDOW::KNAME(0%) = "customer_Number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 4%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
		SMG_WINDOW::KNAME(2%) = "Category"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 5%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%
		SMG_WINDOW::KNAME(3%) = "Alpha"
			SMG_WINDOW::KFIELD(3%, 0%) = 1%
			SMG_WINDOW::KFIELD(3%, 1%) = 3%

		STITLE$ = "Status   Description"
		SSTAT$(0%) = "5"
		SSTAT$(1%) = "A      Active"
		SSTAT$(2%) = "I      Inactive"
		SSTAT$(3%) = "P      Purge"
		SSTAT$(4%) = "T      Terminated"
		SSTAT$(5%) = "C      Cash"

		!
		! List of types
		!
		ECTITLE$ = "Method Description"
		EC$(0%) = "2"
		EC$(1%) = "O    Open Billing"
		EC$(2%) = "B    Balance Forward Billing"

		!
		! Tax type
		!
		TAXTITLE$ = "Type   Description"
		TAXTYPE$(0%) = "4"
		TAXTYPE$(1%) = "1    Taxable"
		TAXTYPE$(2%) = "4    Resale"
		TAXTYPE$(3%) = "5    Out of state"
		TAXTYPE$(4%) = "6    Church, School, and Government"

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Get info required for main file
		!
		IF AR_35CUSTOM.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_35CUSTOM.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_35CUSTOM = ERR
			CONTINUE 770
		END WHEN

		AR_35CUSTOM.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
		USE
			AR_MAIN_35CUSTOM = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_35CUSTOM.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AR_35CUSTOM.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_35CUSTOM.CH%

		WHEN ERROR IN
			RESET #AR_35CUSTOM.CH%
			GET #AR_35CUSTOM.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

	!
	! Modify the menu
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " conTact"

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
	!	.b
	!	.lm +5
	!	^*Contact\* contains information concerning the person the company
	!	will reach regarding questions or problems with that particular vendor.
	!	.lm -5
	!
	! Index:
	!	.x Contact>Customer Address Maintenance
	!	.x Customer Address Maintenance>Contact
	!
		!--
			!
			! Make sure there is a header
			!
			V% = MAIN_JOURNAL(AR_MAIN_CONTACT.ID, "")

		END SELECT

	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SELECT MLOOP
		!
		! Main screen
		!
		CASE 0%

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1,  1, "(01) Customer", &
			2,  1, "(02) Name", &
			3,  1, "(03) Alpha", &
			4,  1, "(04) Cust Type", &
			5,  1, "(05) Category", &
			6,  1, "(06) Onset Date", &
			7,  1, "(07) Stat Flag", &
			8,  1, "(08) Stat Date", &
			9,  1, "(09) Add1", &
			10,  1, "(10) Add2", &
			11,  1, "(11) Add3", &
			12,  1, "(12) City", &
			12, 33, "(13) State", &
			13,  1, "(14) Zip", &
			13, 33, "(15) Country", &
			14,  1, "(16) Phone", &
			14, 33, "(17) County", &
			15,  1, "(18) Method ", &
			16,  1, "(19) Statement", &
			17,  1, "(20) Srvc Chrg", &
			0,  0, ""

			RESTORE

		READ XPOS, YPOS, XSTR$

		I% = 0%

		WHILE  (XPOS <> 0%)

			I% = I% + 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS, YPOS) &
				IF SMG_WINDOW::HFLAG(I%) < 2%

			READ XPOS, YPOS, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)


		!
		! 1st page
		!
		CASE 1%
			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE( &
				SMG_WINDOW::LWINDOW)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::LWINDOW)

			DATA	1,  1, "(21) Tax Code", &
				2,  1, "(22) Tax Flag", &
				3,  1, "(23) Tax Exempt", &
				4,  1, "(24) Location", &
				5,  1, "(25) Terms", &
				6,  1, "(26) Carrier", &
				7,  1, "(27) Salesman", &
				8,  1, "(28) Credit Lim", &
				9,  1, "(29) Discount", &
				10,  1, "(30) Backorder", &
				0,  0, ""

			RESTORE

			XPOS = -1%

			READ XPOS, YPOS, XSTR$ UNTIL XPOS = 0%

			READ XPOS, YPOS, XSTR$

			I% = SMG_WINDOW::LPAGE(0%)

			WHILE  (XPOS <> 0%)

				I% = I% + 1%

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::LWINDOW, &
					XSTR$, XPOS, YPOS) &
					IF SMG_WINDOW::HFLAG(I%) < 2%

				READ XPOS, YPOS, XSTR$
			NEXT

			SMG_STATUS% = SMG$END_DISPLAY_UPDATE( &
				SMG_WINDOW::LWINDOW)

		END SELECT

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
	!	.x Customer>Number
	!	^*(01) Customer\*
	!	.b
	!	.lm +5
	!	The ^*Customer\* field assigns an identification number
	!	to be used for referencing a particular customer. It is recommended
	!	that all customer numbers be the same length.
	!	.b
	!	Ten spaces are available for the entry.
	!	.b
	!	An example might be: ^*PUBJ\* - John Q. Public
	!	.lm -5
	!
	! Index:
	!	.x Number>Customer
	!
	!--

			AR_35CUSTOM::CUSNUM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;17", TEMP$, &
				AR_35CUSTOM::CUSNUM, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Customer>Name
	!	^*(02) Name\*
	!	.b
	!	.lm +5
	!	The ^*Name\* field contains the name of the
	!	customer as it will appear on all records and reports throughout
	!	the system.
	!	.b
	!	Up to fifty characters may be entered.
	!	.lm -5
	!
	! Index:
	!	.x Name>Customer
	!
	!--

			AR_35CUSTOM::CUSNAM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;17", TEMP$, &
				AR_35CUSTOM::CUSNAM, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x customer>Alpha Sort
	!	^*(03) Alpha\*
	!	.b
	!	.lm +5
	!	The ^*Alpha\* field contents allows the file to be sorted,
	!	listed, or reported in an alphabetical order different from
	!	the name field.
	!	This allows for a listing by last name, for example.
	!	.b
	!	Fifteen spaces are available for the entry.
	!	.b
	!	It is recommended that special characters not be included in
	!	this field, i.e. the name "O'Connor" would be entered as
	!	"OConnor".
	!	.lm -5
	!
	! Index:
	!	.x Alpha Sort>customer
	!
	!--

			AR_35CUSTOM::ALPSRT = EDIT$(AR_35CUSTOM::CUSNAM, 32%) &
				IF (TEMP1$ = "Add") AND &
					((MFLAG AND 1%) = 0%) AND &
					(AR_35CUSTOM::ALPSRT = "")

			AR_35CUSTOM::ALPSRT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;17", TEMP$, &
				AR_35CUSTOM::ALPSRT, MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	.x Type>Customer
	!	^*(04) Customer Type\*
	!	.b
	!	.lm +5
	!	The ^*Customer Type\* field defines the
	!	customers type.
	!	.b
	!	Example:
	!	.table 3,25
	!	.te
	!	^*01\* - Regular
	!	.te
	!	^*02\* - Distributor
	!	.te
	!	^*03\* - Retailer
	!	.end table
	!	The field will accommodate two alpha-numeric characters.
	!	.b
	!	Valid customer types may be viewed by pressing ^*List Choices\*.
	!	Additional customer types may be added by pressing the ^*F17\* key.
	!	.lm -5
	!
	! Index:
	!	.x Customer>Type
	!
	!--

			AR_35CUSTOM::TTYPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;17", TEMP$, &
				AR_35CUSTOM::TTYPE, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(AR_MAIN_CUSTYPE.ID, "VX") = 1%
				THEN
					AR_35CUSTOM::TTYPE = AR_CUSTYPE::CUSTYPE
				END IF
				GOTO ReEnter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(AR_MAIN_CUSTYPE.ID, "M")
				AR_35CUSTOM::TTYPE = AR_CUSTYPE::CUSTYPE
				GOTO ReEnter

			END SELECT


		CASE 5%

	!++
	! Abstract:FLD005
	!	.x Customer>Category
	!	^*(05) Category\*
	!	.b
	!	.lm +5
	!	The ^*Category Type\* field contains a
	!	code which will identify a category to place the customer in.
	!	.b
	!	Example:
	!	.table 3,25
	!	.te
	!	^*RE\* - Regular
	!	.te
	!	^*WH\* - Wholesale
	!	.te
	!	^*DE\* - Dealer
	!	.te
	!	^*EL\* - Electrical
	!	.end table
	!	The field will accept four characters.
	!	.b
	!	Valid category types may be viewed by pressing ^*List Choices\*.
	!	Additional category codes may be added by pressing the ^*F17\* key.
	!	.lm -5
	!
	! Index:
	!	.x Category>Customer
	!
	!--

			AR_35CUSTOM::CATEGORY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;17", TEMP$, &
				AR_35CUSTOM::CATEGORY, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(OE_MAIN_CATEGORY.ID, "VX") = 1%
				THEN
					AR_35CUSTOM::CATEGORY = &
						OE_CATEGORY::ORDCAT
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(OE_MAIN_CATEGORY.ID, "M")
				AR_35CUSTOM::CATEGORY = &
					OE_CATEGORY::ORDCAT
				GOTO Reenter

			END SELECT


		CASE 6%

	!++
	! Abstract:FLD006
	!	.x Customer>Address
	!	^*(06) Onset Date\*
	!	.b
	!	.lm +5
	!	The ^*Onset Date\* field contains the
	!	onset date for the customer (the date the customer was added
	!	to the system).
	!	.B
	!	The field will default to the current date.  To accept the
	!	date press ^*Return\*, to override the default enter the correct
	!	date and press ^*Return\*.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Address>Customer
	!
	!--

			AR_35CUSTOM::BDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;17", TEMP$, &
				AR_35CUSTOM::BDATE, MFLAG, "'E", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	.x Customer>Address
	!	^*(07) Status Flag\*
	!	.b
	!	.lm +5
	!	The ^*Status Flag\* field contains the current
	!	Status for this customer.
	!	.b
	!	Valid status flags are:
	!	.table 3,25
	!	.te
	!	^*A\* - Active
	!	.te
	!	^*I\* - Inactive
	!	.te
	!	^*P\* - Purged
	!	.te
	!	^*T\* - Terminated
	!	.end table
	!	An entry is required in this field.
	!	.b
	!	Valid status flags may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Address>Customer
	!
	!--


			AR_35CUSTOM::SSTATUS = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;17", TEMP$, &
				AR_35CUSTOM::SSTATUS, MFLAG, "!", MVALUE, &
				SSTAT$(), STITLE$, "008"), -1%)



		CASE 8%

	!++
	! Abstract:FLD008
	!	.x Customer>Status Date
	!	^*(08) Status Date\*
	!	.b
	!	.lm +5
	!	The ^*Status Date\* field contains the date
	!	of the last status change (field 07) for the customer.
	!	Usually used to know when a customer was terminated.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Address>Status Date
	!
	!--

			AR_35CUSTOM::EDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;17", TEMP$, &
				AR_35CUSTOM::EDATE, MFLAG, "'E", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	.x Customer>Address
	!	^*(09) Address 1\*
	!	.b
	!	.lm +5
	!	The ^*Address 1\* field contains the address for the
	!	customer. It is recommended that in the event of a one line street
	!	address, this field be left blank and field (10) be used for the street
	!	address.
	!	.b
	!	The field will accommodate 25 characters.
	!	.lm -5
	!
	! Index:
	!	.x Address>Customer
	!
	!--

			AR_35CUSTOM::ADD1 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;17", TEMP$, &
				AR_35CUSTOM::ADD1, MFLAG, "'E", MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	.x Address>Customer
	!	^*(10) Address 2\*
	!	.b
	!	.lm +5
	!	The ^*Address 2\* field contains the second line address
	!	for the customer. It is recommended that in the event there is a single
	!	line street address, this field be used for the entry.
	!	.b
	!	The field will accommodate 25 characters.
	!	.lm -5
	!
	! Index:
	!	.x Customer>Address
	!
	!--

			AR_35CUSTOM::ADD2 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;17", TEMP$, &
				AR_35CUSTOM::ADD2, MFLAG, "'E", MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	.x Customer>Address
	!	^*(11) Address 3\*
	!	.b
	!	.lm +5
	!	The ^*Address 3\* field contains the address for the
	!	customer. It is recommended that in the event of a one line street
	!	address, this field be left blank.
	!	.b
	!	The field will accommodate 25 characters.
	!	.lm -5
	!
	! Index:
	!	.x Address>Customer
	!
	!--

			AR_35CUSTOM::ADD3 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;17", TEMP$, &
				AR_35CUSTOM::ADD3, MFLAG, "'E", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	.x Customer>City
	!	^*(12) City\*
	!	.b
	!	.lm +5
	!	The ^*City\* field contains the city in which the
	!	customer is located.
	!	.b
	!	Fifteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x City>Customer
	!
	!--

			AR_35CUSTOM::CITY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;17", TEMP$, &
				AR_35CUSTOM::CITY, MFLAG, "'E", MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	.x customer>State
	!	^*(13) State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field contains the State postal code which
	!	will represent the State in which
	!	the customer is located.
	!	.b
	!	The field will accommodate the two character state postal code.
	!	.lm -5
	!
	! Index:
	!	.x State>customer
	!
	!--

			AR_35CUSTOM::STATE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;46", TEMP$, &
				AR_35CUSTOM::STATE, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(UTL_MAIN_STATE.ID, &
					"VX" + AR_35CUSTOM::COUNTRY) = 1%
				THEN
					AR_35CUSTOM::STATE = UTL_STATE::STATE
				END IF
				GOTO  ReEnter

			END SELECT

		CASE 14%

	!++
	! Abstract:FLD014
	!	.x customer>Zip
	!	^*(14) Zip\*
	!	.b
	!	.lm +5
	!	The ^*Zip\* field contains the zip or postal
	!	code for the area in which a customer is located.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Zip>customer
	!
	!--

			AR_35CUSTOM::ZIP = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;17", TEMP$, &
				AR_35CUSTOM::ZIP, MFLAG, "'E", MVALUE)

		CASE 15%

	!++
	! Abstract:FLD015
	!	.x customer>Country
	!	^*(15) Country\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field contains the country if
	!	a customer is located in a foreign country.
	!	.b
	!	Two spaces are available for the entry.
	!	.b
	!	Valid Country codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Country>customer
	!
	!--

			AR_35CUSTOM::COUNTRY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;46", TEMP$, &
				AR_35CUSTOM::COUNTRY, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "VX" + &
					AR_35CUSTOM::COUNTRY) = 1%
				THEN
					AR_35CUSTOM::COUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "M")
				AR_35CUSTOM::COUNTRY = &
					UTL_COUNTRY::COUNTRY
				GOTO Reenter

			END SELECT

		CASE 16%

	!++
	! Abstract:FLD016
	!	.x customer>Phone
	!	^*(16) Phone\*
	!	.b
	!	.lm +5
	!	The ^*Phone\* field contains customer's main telephone number.
	!	Additional phone numbers may be entered in the 'contact' table.
	!	.b
	!	It is not required
	!	to enter the special characters (/-), the system will insert them
	!	automatically.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.X customer>Telephone
	!	.x Phone>customer
	!	.x Telephone>customer
	!
	!--

			AR_35CUSTOM::PHONE = &
				ENTR_3PHONE(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;17", TEMP$, &
				AR_35CUSTOM::PHONE, 2% OR MFLAG, 0%, MVALUE)

		CASE 17%

	!++
	! Abstract:FLD017
	!	.x customer>County
	!	^*(17) County\*
	!	.b
	!	.lm +5
	!	The ^*County\* field contains
	!	the County in which a particular customer is located.
	!	.b
	!	This field may be left blank.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x County>customer
	!
	!--

			AR_35CUSTOM::COUNTY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;46", TEMP$, &
				AR_35CUSTOM::COUNTY, MFLAG, "'E", MVALUE)

		CASE 18%

	!++
	! Abstract:FLD018
	!	.x Accounts Receivable>Method of Accounting for
	!	^*(18) Method\*
	!	.b
	!	.lm +5
	!	The ^*Method\* field chooses either the "Open
	!	Item" or the "Balance Forward" method of accounting
	!	for this customer.
	!	The "Open Item" is usually the prefered method.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*O\* - Open Item Method
	!	.te
	!	^*B\* - Balance Forward Method
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Method>Accounting for Accounts Receivable
	!	.x Method>Open Item
	!	.x Open Item>Method of Accounting
	!	.x Method>Balance Forward
	!	.x Balance Forward>Method of Accounting
	!	.x Accounting>Method for Accounts Receivable
	!
	!--

			AR_35CUSTOM::METHOD = EDIT$( ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;17", &
				TEMP$ + " (O/B)", AR_35CUSTOM::METHOD, &
				MFLAG OR 16%, &
				"'", MVALUE, EC$(), ECTITLE$, "007"), -1%)

		CASE 19%

	!++
	! Abstract:FLD019
	!	.x Customer>Statement Flag
	!	^*(19) Statement\*
	!	.b
	!	.lm +5
	!	The ^*Statement\* field allows the option of printing or not
	!	printing a statement for a particular customer.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	Valid values may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Statement>Flag
	!	.x Flag>Statement
	!
	!--

			AR_35CUSTOM::STMTFLG = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;17", TEMP$, &
				AR_35CUSTOM::STMTFLG, MFLAG, "'E", MVALUE)

		CASE 20%

	!++
	! Abstract:FLD020
	!	.x Customer>Service Charge
	!	^*(20) Service Charge\*
	!	.b
	!	.lm +5
	!	The ^*Service Charge\* field indicates whether or not
	!	a customer is to be charged a service charge on past due balances.
	!	.TABLE 3,25
	!	.TE
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	Valid values may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Service Charge>Customer
	!
	!--

			AR_35CUSTOM::SERCHRG = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;17", TEMP$, &
				AR_35CUSTOM::SERCHRG, MFLAG, "'E", MVALUE)

		CASE 21%

	!++
	! Abstract:FLD021
	!	^*(21) Tax Code\*
	!	.b
	!	.lm +5
	!	The ^*Tax Code\* field defines
	!	the tax jurisdiction in which a sales tax is applicable.
	!	.b
	!	Two spaces are available for the entry.
	!	.b
	!	Valid tax codes may be viewed by pressing ^*List Choices\*.
	!	Additional tax codes may be added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!
	!--

			AR_35CUSTOM::TAXCODE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"1;17", TEMP$, &
				AR_35CUSTOM::TAXCODE, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(OE_MAIN_SALESTAX.ID, "VX") = 1%
				THEN
					AR_35CUSTOM::TAXCODE = &
						OE_SALESTAX::TAXCODE
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(OE_MAIN_SALESTAX.ID, "M")
				AR_35CUSTOM::TAXCODE = &
					OE_SALESTAX::TAXCODE
				GOTO Reenter

			END SELECT

		CASE 22%

	!++
	! Abstract:FLD022
	!	.x Sales Tax>Flag
	!	^*(22) Tax Flag\*
	!	.b
	!	.lm +5
	!	The ^*Tax Flag\* field defines the sales taxability status
	!	of a customer.
	!	.b
	!	Valid choices are:
	!	.table 3,25
	!	.te
	!	^*1\* - Taxable
	!	.te
	!	^*4\* - Resale
	!	.te
	!	^*5\* - Out of State
	!	.te
	!	^*6\* - Church, School, or Government
	!	.end table
	!	Valid tax flags may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Sales Tax>Status
	!	.x Status>Sale Taxability
	!
	!--

			AR_35CUSTOM::TAXFLAG = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::LWINDOW, "02;17", TEMP$, &
				AR_35CUSTOM::TAXFLAG, MFLAG, "!", MVALUE, &
				TAXTYPE$(), TAXTITLE$, "007"), -1%)



		CASE 23%

	!++
	! Abstract:FLD023
	!	.x Sales Tax>Resale License
	!	^*(23) Tax Exemption\*
	!	.b
	!	.lm +5
	!	The ^*Tax Exemption\* field defines the sales tax exemption
	!	or resale license number as applicable.
	!	.b
	!	Fifteen (15) spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Exemption Number>Sales Tax
	!	.x Resale License Number
	!	.x Sales Tax>Exemption Number
	!
	!--

			AR_35CUSTOM::TAXEXEMP = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"3;17", TEMP$, &
				AR_35CUSTOM::TAXEXEMP, MFLAG, "'E", MVALUE)

		CASE 24%

	!++
	! Abstract:FLD024
	!	.x Location>Number
	!	^*(24) Location Number\*
	!	.b
	!	.lm +5
	!	The ^*Location Number\* field defines
	!	the customers billing location.
	!	.b
	!	The field will accommodate four characters.
	!	.b
	!	Valid location numbers may be viewed by pressing ^*List Choices\*.
	!	Additional location codes may be added by pressing the ^*F17\* key.
	!	.lm -5
	!
	! Index:
	!
	!--


			AR_35CUSTOM::LOCATION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"4;17", TEMP$, &
				AR_35CUSTOM::LOCATION, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%
				THEN
					AR_35CUSTOM::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "M")
				AR_35CUSTOM::LOCATION = &
					UTL_LOCATION::LOCATION
				GOTO Reenter

			END SELECT

		CASE 25%

	!++
	! Abstract:FLD025
	!	^*(25) Terms Code\*
	!	.b
	!	.lm +5
	!	The ^*Terms Code\* field defines
	!	the payment terms, such as "Net 30".
	!	.b
	!	Two spaces are available for the entry.
	!	.b
	!	Valid payment term codes may be viewed by pressing ^*List Choices\*.
	!	Additional terms codes may be entered by pressing the ^*F17\* key.
	!	.lm -5
	!
	! Index:
	!
	!--
			AR_35CUSTOM::TERMS = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"5;17", TEMP$, &
				AR_35CUSTOM::TERMS, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UT_MAIN_TERMS.ID, "VX") = 1%
				THEN
					AR_35CUSTOM::TERMS = &
						UTL_TERMS::CODE
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(UT_MAIN_TERMS.ID, "M")
				AR_35CUSTOM::TERMS = UTL_TERMS::CODE
				GOTO Reenter

			END SELECT

		CASE 26%

	!++
	! Abstract:FLD026
	!	^*(26) Carrier (Ship Via)\*
	!	.b
	!	.lm +5
	!	The ^*Carrier (Ship Via)\* field
	!	identifies the customers prefered carrier (Ship Via).
	!	.b
	!	The field will accommodate a two character code.
	!	.b
	!	Valid carrier codes may be viewed by pressing ^*List Choices\*.
	!	Additional carrier codes may be entered by pressing the ^*F17\* key.
	!	.lm -5
	!
	! Index:
	!
	!--

			AR_35CUSTOM::CARRIER = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"6;17", TEMP$, &
				AR_35CUSTOM::CARRIER, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, "VX") = 1%
				THEN
					AR_35CUSTOM::CARRIER = &
						UTL_CARRIER::CODE
				END IF
				GOTO ReEnter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(UT_MAIN_CARRIER.ID, "M")
				AR_35CUSTOM::CARRIER = UTL_CARRIER::CODE
				GOTO ReEnter

			END SELECT

		CASE 27%

	!++
	! Abstract:FLD027
	!	^*(27) Salesman Number\*
	!	.b
	!	.lm +5
	!	The ^*Salesman Number\* field
	!	specifies the salesman responsible for the customer.
	!	.b
	!	Ten spaces are available for the entry.
	!	.b
	!	Valid salesman numbers may be viewed by pressing ^*List Choices\*.
	!	Additional salesman numbers may be entered by pressing the ^*F17\* key.
	!	.lm -5
	!
	! Index:
	!
	!--

			AR_35CUSTOM::SALESMAN = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"7;17", TEMP$, &
				AR_35CUSTOM::SALESMAN, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "VX") = 1%
				THEN
					AR_35CUSTOM::SALESMAN = &
						SA_SALESMAN::SALESMAN
				END IF
				GOTO ReEnter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "M")
				AR_35CUSTOM::SALESMAN = &
					SA_SALESMAN::SALESMAN
				GOTO ReEnter

			END SELECT

		CASE 28%

	!++
	! Abstract:FLD028
	!	^*(28) Credit Limit\*
	!	.b
	!	.lm +5
	!	The ^*Credit Limit\* field defines the
	!	credit limit that is allocated for a particular customer.
	!	.b
	!	The field will accept a figure as large as $9,999,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--


			AR_35CUSTOM::CREDITLIM = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"8;17", TEMP$, &
			AR_35CUSTOM::CREDITLIM, MFLAG, "#,###,###.##", MVALUE)

		CASE 29%

	!++
	! Abstract:FLD029
	!	^*(29) Discount\*
	!	.b
	!	.lm +5
	!	The ^*Discount\* field defines the
	!	percentage discount which will be given to this customer.
	!	.b
	!	Example: If the discount to be given is 10%, the entry
	!	in this field would be made as 10.00.
	!	.lm -5
	!
	! Index:
	!
	!--

			AR_35CUSTOM::DISCOUNT = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"9;17", TEMP$, &
			AR_35CUSTOM::DISCOUNT, MFLAG, "#,###,###.##", MVALUE)

		CASE 30%

	!++
	! Abstract:FLD030
	!	^*(30) Backorder\*
	!	.b
	!	.lm +5
	!	The ^*Backorder\* field indicates whether
	!	shipments to this customer can be placed on backorder.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	Valid entries may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--

			AR_35CUSTOM::BACKORDER = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::LWINDOW, &
				"10;17", TEMP$, &
				AR_35CUSTOM::BACKORDER, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	CASE OPT_TESTENTRY
		AR_MAIN_35CUSTOM = 0%

		SELECT MLOOP

		CASE 1%
			IF AR_35CUSTOM::CUSNUM = ""
			THEN
				AR_MAIN_35CUSTOM = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #AR_35CUSTOM.CH%, &
							KEY #0% EQ AR_35CUSTOM::CUSNUM + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AR_MAIN_35CUSTOM = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				ELSE
					IF (MVALUE = "CHANGE") AND &
						AR_35CUSTOM_OLD::CUSNUM <> &
						AR_35CUSTOM::CUSNUM
					THEN
						GOSUB 28000

						IF ACTIVE_FLAG% <> 0%
						THEN
							CALL ENTR_3MESSAGE(SCOPE, "Vendor has activity.  " + &
								"Change not allowed", 0%)

							AR_MAIN_35CUSTOM = 2%
						END IF
					END IF
				END IF
			END IF

		CASE 4%
			!
			! Display the descriptions for customer type
			!
			AR_MAIN_35CUSTOM = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_35CUSTOM::TTYPE, &
				AR_CUSTYPE::CUSTYPE, &
				"AR", MLOOP, "PROG", &
				"Customer Type ", AR_MAIN_CUSTYPE.ID)

		CASE 18%
			AR_MAIN_35CUSTOM = 1% &
				IF AR_35CUSTOM::METHOD <> "O" AND &
				AR_35CUSTOM::METHOD <> "B"

		CASE 24%
			IF (AR_35CUSTOM::LOCATION <> "    ")
			THEN
				AR_MAIN_35CUSTOM = FUNC_TESTENTRY(SMG_WINDOW, &
					AR_35CUSTOM::LOCATION, &
					UTL_LOCATION::LOCATION, &
					"AR", MLOOP, "PROG", &
					"Location ", UTL_MAIN_LOCATION.ID)
			END IF

		CASE 25%
			IF (AR_35CUSTOM::TERMS <> "  ")
			THEN
				AR_MAIN_35CUSTOM = FUNC_TESTENTRY(SMG_WINDOW, &
					AR_35CUSTOM::TERMS, &
					UTL_TERMS::CODE, &
					"AR", MLOOP, "PROG", &
					"Terms ", UT_MAIN_TERMS.ID)
			END IF

		END SELECT

	CASE OPT_TESTOPT
		AR_MAIN_35CUSTOM = 0%

		IF EDIT$(MVALUE, -1%) = "ERASE"
		THEN
			AR_35CUSTOM_OLD::CUSNUM = AR_35CUSTOM::CUSNUM

			GOSUB 28000

			IF ACTIVE_FLAG% <> 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Vendor has activity.  Erase not allowed", 0%)

				AR_MAIN_35CUSTOM = 1%
			END IF
		END IF

20500	CASE OPT_SETOLD
		AR_35CUSTOM_OLD = AR_35CUSTOM

	CASE OPT_RESETOLD
		AR_35CUSTOM = AR_35CUSTOM_OLD

	CASE OPT_SETDEFAULT
		AR_35CUSTOM_DEF = AR_35CUSTOM

	CASE OPT_RESETDEFAULT
		AR_35CUSTOM = AR_35CUSTOM_DEF

		AR_35CUSTOM::BDATE = DATE_TODAY &
			IF AR_35CUSTOM::BDATE = ""

		AR_35CUSTOM::SSTATUS = "A" &
			IF AR_35CUSTOM::SSTATUS = ""

		AR_35CUSTOM::COUNTRY = "US" &
			IF AR_35CUSTOM::COUNTRY = ""

	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%

		MVALUE = "  #          Name                      Address" + &
		"                                       City, State" + &
		"               Zip Code    Ty Cat"

		CASE 2%
			MVALUE = "013,039,085,111,123,126"

		CASE 3%
			MVALUE = &
				AR_35CUSTOM::CUSNUM + " " + &
				LEFT(AR_35CUSTOM::CUSNAM, 25%) + " " + &
				LEFT(TRM$(AR_35CUSTOM::ADD1) + " " + &
					TRM$(AR_35CUSTOM::ADD2) + &
					SPACE$(45%), 45%) + " " + &
				LEFT(TRM$(AR_35CUSTOM::CITY) + "  " + &
					TRM$(AR_35CUSTOM::STATE) + " " + &
					TRM$(AR_35CUSTOM::COUNTRY) + &
					SPACE$(25%), 25%)          + " " + &
				AR_35CUSTOM::ZIP + "  " + &
				AR_35CUSTOM::TTYPE + " " + &
				AR_35CUSTOM::CATEGORY

		END SELECT

	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #AR_35CUSTOM.CH%, &
				KEY #0% GE AR_35CUSTOM::CUSNUM + "", &
				REGARDLESS

		CASE 1%
			FIND #AR_35CUSTOM.CH%, &
				KEY #1% GE AR_35CUSTOM::TTYPE + &
				AR_35CUSTOM::CUSNUM, REGARDLESS

		CASE 2%
			FIND #AR_35CUSTOM.CH%, &
				KEY #2% GE AR_35CUSTOM::CATEGORY + &
				AR_35CUSTOM::CUSNUM, REGARDLESS

		CASE 3%
			FIND #AR_35CUSTOM.CH%, &
				KEY #3% GE AR_35CUSTOM::ALPSRT + "", &
				REGARDLESS

		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Add records
		!
 !		CASE "Add"
			!
			! Add line items also
			!
 !			AR_MAIN_35CUSTOM = MAIN_JOURNAL(AR_MAIN_CONTACT.ID, "A")

		!
		! Change records
		!
		CASE "Change"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF AR_35CUSTOM_OLD::CUSNUM <> AR_35CUSTOM::CUSNUM
			THEN
				TEMP$ = AR_35CUSTOM::CUSNUM + ""
				AR_35CUSTOM = AR_35CUSTOM_OLD
				AR_MAIN_35CUSTOM = &
					MAIN_JOURNAL(AR_MAIN_CONTACT.ID, &
					"C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			AR_MAIN_35CUSTOM = MAIN_JOURNAL(AR_MAIN_CONTACT.ID, "E")

		END SELECT

	END SELECT

	EXIT FUNCTION

28000	!****************************************************************

	ACTIVE_FLAG% = 0%

	IF AR_OPEN.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
		USE
			CONTINUE 28010 IF ERR = 155% OR ERR = 9% OR ERR = 5%
			EXIT HANDLER
		END WHEN
	END IF

	WHEN ERROR IN
		FIND #AR_OPEN.CH%, &
			KEY #0% EQ AR_35CUSTOM_OLD::CUSNUM + "", &
			REGARDLESS
	USE
		CONTINUE 28010 IF ERR = 155% OR ERR = 9% OR ERR = 5%
		EXIT HANDLER
	END WHEN

	AR_35CUSTOM::CUSNUM = AR_35CUSTOM_OLD::CUSNUM

	ACTIVE_FLAG% = -1%

	RETURN

28010	IF AR_CLOSED.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
		USE
			CONTINUE 28090 IF ERR = 155% OR ERR = 9% OR ERR = 5%
			EXIT HANDLER
		END WHEN
	END IF

	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, &
			KEY #0% EQ AR_35CUSTOM_OLD::CUSNUM + "", &
			REGARDLESS
	USE
		CONTINUE 28090 IF ERR = 155% OR ERR = 9% OR ERR = 5%
		EXIT HANDLER
	END WHEN

	AR_35CUSTOM::CUSNUM = AR_35CUSTOM_OLD::CUSNUM

	ACTIVE_FLAG% = -1%

28090	RETURN

29000	!*********************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
