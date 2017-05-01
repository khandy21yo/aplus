1	%TITLE "Query Name/Address file"
	%SBTTL "AR_MAIN_QUERYCUST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_QUERYCUST(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991 BY
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
	!	The ^*Query Name/Address File\* option
	!	queries for information relative to each customer.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_QUERYCUST/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AR_MAIN_QUERYCUST
	!	$ DELETE AR_MAIN_QUERYCUST.OBJ;*
	!
	! Author:
	!
	!	07/12/91 - Val James "Gimmyabreak" Allen
	!
	! Modification history:
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/19/95 - Kevin Handy
	!		Fix format parameter to ENTR_3PHONE
	!
	!	05/26/95 - Kevin Handy
	!		Added status of "C" for cash customers.
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
	!	06/08/99 - Kevin Handy
	!		Lose line 28090 (Dead Code)
	!
	!	11/29/99 - Kevin Handy
	!		Lose map for AR_CLOSED, which was never used
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
	MAP (AR_35CUSTOM)		AR_35CUSTOM_CDD	AR_35CUSTOM
	MAP (AR_35CUSTOM_OLD)	AR_35CUSTOM_CDD	AR_35CUSTOM_OLD, AR_35CUSTOM2

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSTYPE.HB"
	MAP (AR_CUSTYPE)		AR_CUSTYPE_CDD	AR_CUSTYPE

	%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.HB"
	MAP (OE_CATEGORY)		OE_CATEGORY_CDD	OE_CATEGORY

	%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.HB"
	MAP (OE_SALESTAX)		OE_SALESTAX_CDD	OE_SALESTAX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)		UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)		UTL_COUNTRY_CDD		UTL_COUNTRY

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD			UTL_TERMS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)		UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)		SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD	AR_OPEN

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AR_35CUSTOM) &
		AR_35CUSTOM.CH%, &
		AR_35CUSTOM.READONLY%

	COM (TT_AR_MAIN_QUERYCUST) &
		ECTITLE$ = 32%, &
		EC$(2%) = 32%, &
		STITLE$ = 30%, &
		SSTAT$(7%) = 30%

	COM (TT_AR_SALTAXLED) &
		TAXTITLE$ = 20%, &
		TAXTYPE$(7%) = 40%


	!
	! External functions
	!

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
		SMG_WINDOW::NHELP = "AR_MAIN_QUERYCUST"
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
		SSTAT$(0%) = "4"
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

760		!
		! Open with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
		USE
			AR_MAIN_QUERYCUST = ERR
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
		MVALUE = "Find Next Restore Help View Page eXit"




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

		DATA	1,  1, "Customer", &
			2,  1, "Name", &
			3,  1, "Alpha", &
			4,  1, "Cust Type", &
			5,  1, "Category", &
			6,  1, "Onset Date", &
			7,  1, "Stat Flag", &
			8,  1, "Stat Date", &
			9,  1, "Add1", &
			10,  1, "Add2", &
			11,  1, "Add3", &
			12,  1, "City", &
			12, 33, "State", &
			13,  1, "Zip", &
			13, 33, "Country", &
			14,  1, "Phone", &
			14, 33, "County", &
			15,  1, "Method ", &
			16,  1, "Statement", &
			17,  1, "Srvc Chrg", &
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

			DATA	1,  1, "Tax Code", &
				2,  1, "Tax Flag", &
				3,  1, "Tax Exempt", &
				4,  1, "Location", &
				5,  1, "Terms", &
				6,  1, "Carrier", &
				7,  1, "Salesman", &
				8,  1, "Credit Lim", &
				9,  1, "Discount", &
				10,  1, "Backorder", &
				0,  0, ""
			RESTORE
			XPOS = -1%
			READ XPOS, YPOS, XSTR$ UNTIL XPOS = 0%
			READ XPOS, YPOS, XSTR$
			I% = SMG_WINDOW::LPAGE(0%)
			WHILE  (XPOS <> 0%)
				I% = I% + 1%
				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
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
	!	^*(01) Customer\*
	!	.b
	!	.lm +5
	!	The ^*Customer\* field is used to assign an identification number
	!	to be used for referencing a particular customer. It is recommended
	!	that all customer numbers be the same length.
	!	.b
	!	The field will accommodate up to ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Customer>Number
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
	!	.ts 55
	!	^*(02) Name	50 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Name\* field is provided to enter the name of the
	!	customer as it will appear on all reports.
	!	.lm -5
	!
	! Index:
	!	.x customer>Name
	!	.x Name>customer
	!
	!--

			AR_35CUSTOM::CUSNAM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;17", TEMP$, &
				AR_35CUSTOM::CUSNAM, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.ts 55
	!	^*(03) Alpha	15 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Alpha\* field contents enables the file to be sorted,
	!	listed, or reported in alphabetical order.
	!	.b
	!	It is recommended that special characters not be included in
	!	this field, i.e. the name "O'Connor" would be entered as
	!	"OConnor".
	!	.lm -5
	!
	! Index:
	!	.x customer>Alpha Sort
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
	!	^*(04) Customer Type\*
	!	.b
	!	.lm +5
	!	The ^*Customer Type\* field enters the
	!	customer type.
	!	.b
	!	Pressing ^*List Choices\* will display a list of valid customer
	!	type codes.
	!	.lm -5
	!
	! Index:
	!
	!--

			AR_35CUSTOM::TTYPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;17", TEMP$, &
				AR_35CUSTOM::TTYPE, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(AR_MAIN_CUSTYPE.ID, "V0") = 1%
				THEN
					AR_35CUSTOM::TTYPE = AR_CUSTYPE::CUSTYPE
				END IF
				GOTO ReEnter
			END IF


		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Category\*
	!	.b
	!	.lm +5
	!	The ^*Category Type\* field is provided to enter a user defined
	!	code which will identify a category.
	!	.lm -5
	!
	! Index:
	!
	!--

			AR_35CUSTOM::CATEGORY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;17", TEMP$, &
				AR_35CUSTOM::CATEGORY, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(OE_MAIN_CATEGORY.ID, "V0") = 1%
				THEN
					AR_35CUSTOM::CATEGORY = &
						OE_CATEGORY::ORDCAT
				END IF
				GOTO Reenter
			END IF


		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Onset Date\*
	!	.b
	!	.lm +5
	!	The ^*Onset Date\* field enters the
	!	onset date for the customer.
	!	.lm -5
	!
	! Index:
	!	.x customer>Address
	!	.x Address>customer
	!
	!--

			AR_35CUSTOM::BDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;17", TEMP$, &
				AR_35CUSTOM::BDATE, MFLAG, "'E", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Status Flag\*
	!	.b
	!	.lm +5
	!	The ^*Status Flag\* field enters the
	!	Status for this customer.
	!	.b
	!	An entry is required in this field. The field will
	!	accommodate one (1) character.
	!	.b
	!	Pressing ^*List Choices\* will provide a list of valid status codes.
	!	.lm -5
	!
	! Index:
	!	.x customer>Address
	!	.x Address>customer
	!
	!--


			AR_35CUSTOM::SSTATUS = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;17", TEMP$, &
				AR_35CUSTOM::SSTATUS, MFLAG, "!", MVALUE, &
				SSTAT$(), STITLE$, "008"), -1%)

		CASE 8%

	!++
	! Abstract:FLD008
	!	.ts 55
	!	^*(08) Status Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Status Date\* field enters the date
	!	of the last status change for the customer.
	!	.lm -5
	!
	! Index:
	!	.x customer>Address
	!	.x Address>customer
	!
	!--

			AR_35CUSTOM::EDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;17", TEMP$, &
				AR_35CUSTOM::EDATE, MFLAG, "'E", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	.ts 55
	!	^*(09) Address 1	25 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Address 1\* field enters the address for the
	!	customer. It is recommended that in the event of a one line street
	!	address, this field be left blank and field (10) be used for the street
	!	address.
	!	.lm +5
	!
	! Index:
	!	.x customer>Address
	!	.x Address>customer
	!
	!--

			AR_35CUSTOM::ADD1 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;17", TEMP$, &
				AR_35CUSTOM::ADD1, MFLAG, "'E", MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	.ts 55
	!	^*(10) Address 2	21 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Address 2\* field enters the second line address
	!	for the customer. It is recommended that in the event there is a single
	!	line street address, this field be used for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Address>customer
	!	.x customer>Address
	!
	!--

			AR_35CUSTOM::ADD2 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;17", TEMP$, &
				AR_35CUSTOM::ADD2, MFLAG, "'E", MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	.ts 55
	!	^*(11) Address 3	25 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Address 3\* field enters the address for the
	!	customer. It is recommended that in the event of a one line street
	!	address, this field be left blank.
	!	.lm -5
	!
	! Index:
	!	.x customer>Address
	!	.x Address>customer
	!
	!--

			AR_35CUSTOM::ADD3 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;17", TEMP$, &
				AR_35CUSTOM::ADD3, MFLAG, "'E", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	.ts 55
	!	^*(12) City	15 Characters\*
	!	.b
	!	.lm +5
	!	The ^*City\* field is provided to enter the city in which the
	!	customer is located.
	!	.lm -5
	!
	! Index:
	!	.x customer>City
	!	.x City>customer
	!
	!--

			AR_35CUSTOM::CITY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;17", TEMP$, &
				AR_35CUSTOM::CITY, MFLAG, "'E", MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	.ts 55
	!	^*(13) State	2 Characters\*
	!	.b
	!	.lm +5
	!	The ^*State\* field is provided to enter the State in which
	!	the customer is located.
	!	.b
	!	The field should contain the appropriate U.S. state postal code.
	!	.lm -5
	!
	! Index:
	!	.x customer>State
	!	.x State>customer
	!
	!--

			AR_35CUSTOM::STATE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;46", TEMP$, &
				AR_35CUSTOM::STATE, MFLAG, "'E", MVALUE)

		CASE 14%

	!++
	! Abstract:FLD014
	!	.ts 55
	!	^*(14) Zip	10 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Zip\* field is provided to enter the zip or postal
	!	code for the area in which a customer is located.
	!	.lm -5
	!
	! Index:
	!	.x customer>Zip
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
	!	.ts 55
	!	^*(15) Country	2 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field is available to enter the country if
	!	a customer is located in a foreign country.
	!	.lm -5
	!
	! Index:
	!	.x customer>Country
	!	.x Country>customer
	!
	!--

			AR_35CUSTOM::COUNTRY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;46", TEMP$, &
				AR_35CUSTOM::COUNTRY, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "V0" + &
					AR_35CUSTOM::COUNTRY) = 1%
				THEN
					AR_35CUSTOM::COUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO Reenter
			END IF

		CASE 16%

	!++
	! Abstract:FLD016
	!	^*(16) Phone\*
	!	.b
	!	.lm +5
	!	The ^*Phone\* field is for entry of a customer's telephone number.
	!	.b
	!	The field may contain ten (10) numeric characters.  It is not required
	!	to enter the special characters.  The system will insert them
	!	automatically.
	!	.lm -5
	!
	! Index:
	!	.x customer>Phone
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
	!	.ts 55
	!	^*(17) County	2 Characters\*
	!	.b
	!	.lm +5
	!	The ^*County\* field enters the code
	!	for the County in which a particular customer is located.
	!	.lm -5
	!
	! Index:
	!	.x customer>County
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
	!	.ts 55
	!	^*(18) Method	O or B\*
	!	.b
	!	.lm +5
	!	The ^*Method\* field choosees either the Open
	!	Item or the Balance Forward method of accounting for accounts receivable.
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
	!	.x Accounts Receivable>Method of Accounting for
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
				TEMP$ + " (O/B)", AR_35CUSTOM::METHOD, MFLAG, &
				"'", MVALUE, EC$(), ECTITLE$, "007"), -1%)

		CASE 19%

	!++
	! Abstract:FLD019
	!	^*(19) Statement\*
	!	.b
	!	.lm +5
	!	The ^*Statement\* field allows the option of printing or not
	!	printing a statement for a particular customer.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!
	! Index:
	!	.x customer>Statement Flag
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
	!	.ts 55
	!	^*(20) Service charge	Y or N\*
	!	.b
	!	.lm +5
	!	The ^*Service Charge\* field is used to indicate whether or not
	!	a customer is to be charged a service charge on past due balances.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x customer>Service Charge
	!	.x Service Charge>customer
	!
	!--

			AR_35CUSTOM::SERCHRG = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;17", TEMP$, &
				AR_35CUSTOM::SERCHRG, MFLAG, "'E", MVALUE)

		CASE 21%

	!++
	! Abstract:FLD021
	!	.ts 55
	!	^*(21) Tax Code	2 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Tax Code\* field enters a code representing
	!	the jurisdiction in which a sales tax is applicable.
	!	.lm -5
	!
	! Index:
	!
	!--

			AR_35CUSTOM::TAXCODE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"1;17", TEMP$, &
				AR_35CUSTOM::TAXCODE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(OE_MAIN_SALESTAX.ID, "V0") = 1%
				THEN
					AR_35CUSTOM::TAXCODE = &
						OE_SALESTAX::TAXCODE
				END IF
				GOTO Reenter
			END IF

		CASE 22%

	!++
	! Abstract:FLD022
	!	.ts 55
	!	^*(22) Tax Flag	1,2,3,4\*
	!	.b
	!	.lm +5
	!	The ^*Tax Flag\* field enters the sales taxability status
	!	of a customer.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*1\* - Taxable
	!	.te
	!	^*2\* - Resale
	!	.te
	!	^*3\* - Out of State
	!	.te
	!	^*4\* - Church, School, or Government
	!	.end table
	!	Pressing ^*List Choices\* will cause the valid choices to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Sales Tax>Flag
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
	!	.x Sales Tax>Exemption Number
	!	^*(23) Tax Exemption\*
	!	.b
	!	.lm +5
	!	The ^*Tax Exemption\* field enters the sales tax exemption
	!	or resale license number as applicable.
	!	.lm -5
	!
	! Index:
	!	.x Sales Tax>Resale License
	!	.x Exemption Number>Sales Tax
	!	.x Resale License Number
	!
	!--

			AR_35CUSTOM::TAXEXEMP = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"3;17", TEMP$, &
				AR_35CUSTOM::TAXEXEMP, MFLAG, "'E", MVALUE)

		CASE 24%

	!++
	! Abstract:FLD024
	!	.ts 55
	!	^*(24) Location Number	4 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Location Number\* field
	!	enters a user defined code to identify
	!	a tax location.
	!	.lm -5
	!
	! Index:
	!	.x Location>Number
	!
	!--


			AR_35CUSTOM::LOCATION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"4;17", TEMP$, &
				AR_35CUSTOM::LOCATION, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					AR_35CUSTOM::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF


		CASE 25%

	!++
	! Abstract:FLD025
	!	.ts 55
	!	^*(25) Terms Code	2 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Terms Code\* field is provided to enter a user defined
	!	number which will identify a particular Terms.
	!	.lm -5
	!
	! Index:
	!
	!--
			AR_35CUSTOM::TERMS = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"5;17", TEMP$, &
				AR_35CUSTOM::TERMS, MFLAG, "'E", MVALUE)



			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UT_MAIN_TERMS.ID, "V0") = 1%
				THEN
					AR_35CUSTOM::TERMS = &
						UTL_TERMS::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 26%

	!++
	! Abstract:FLD026
	!	.ts 55
	!	^*(26) Carrier (Ship Via)	2 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Carrier (Ship Via)\* field is provided to enter a user defined
	!	number which will identify a particular carrier (Ship Via).
	!	.lm -5
	!
	! Index:
	!
	!--

			AR_35CUSTOM::CARRIER = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"6;17", TEMP$, &
				AR_35CUSTOM::CARRIER, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, "V0") = 1%
				THEN
					AR_35CUSTOM::CARRIER = &
						UTL_CARRIER::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 27%

	!++
	! Abstract:FLD027
	!	.ts 55
	!	^*(27) Salesman Number	10 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Salesman Number\* field is to be enter with a number which
	!	will reference a particular salesman.
	!	.lm -5
	!
	! Index:
	!
	!--


			AR_35CUSTOM::SALESMAN = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"7;17", TEMP$, &
				AR_35CUSTOM::SALESMAN, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "V0") = 1%
				THEN
					AR_35CUSTOM::SALESMAN = &
						SA_SALESMAN::SALESMAN
				END IF
				GOTO Reenter
			END IF

		CASE 28%

	!++
	! Abstract:FLD028
	!	^*(28) Credit Limit\*
	!	.b
	!	.lm +5
	!	The ^*Credit Limit\* field enters the
	!	credit limit that is allocated for a particular customer.
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
	!	The ^*Discount\* field enters the
	!	discount given for a particular customer.
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
	!	.ts 55
	!	^*(30) Backorder	Y or N\*
	!	.b
	!	.lm +5
	!	The ^*Backorder\* field decides whether
	!	shipments to this customer can be placed on backorder.
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
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


20500	CASE OPT_SETOLD
		AR_35CUSTOM_OLD = AR_35CUSTOM

	CASE OPT_RESETOLD
		AR_35CUSTOM = AR_35CUSTOM_OLD

	CASE OPT_SETDEFAULT
		AR_35CUSTOM2 = AR_35CUSTOM

	CASE OPT_RESETDEFAULT
		AR_35CUSTOM = AR_35CUSTOM2
		AR_35CUSTOM::BDATE = DATE_TODAY &
			IF AR_35CUSTOM::BDATE =""
		AR_35CUSTOM::SSTATUS = "A" &
			IF AR_35CUSTOM::SSTATUS =""

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%

			MVALUE = "  #          Name                      " + &
				"Address" + &
				"                                       " + &
				"City, State" + &
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
					SPACE$(25%), 25%) + " " + &
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
				AR_35CUSTOM::CUSNUM, &
				REGARDLESS

		CASE 2%
			FIND #AR_35CUSTOM.CH%, &
				KEY #2% GE AR_35CUSTOM::CATEGORY + &
				AR_35CUSTOM::CUSNUM, &
				REGARDLESS

		CASE 3%
			FIND #AR_35CUSTOM.CH%, &
				KEY #3% GE AR_35CUSTOM::ALPSRT + "", &
				REGARDLESS

		END SELECT


	END SELECT

	EXIT FUNCTION

 !28090	RETURN

29000	!*********************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
