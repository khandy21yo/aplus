1	%TITLE "Order Entry Journal"
	%SBTTL "OE_MAIN_ORDERJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_ORDERJOUR(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1990 BY
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
	!	The Order Entry Journal transactions are maintained through the
	!	^*Order Entry Journal\* option.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Order Entry Journal
	!	.x Batch Number>User
	!	.x User Batch Number
	!	.x Journal>Entry Maintain
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_ORDERJOUR/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN OE_MAIN_ORDERJOUR
	!	$ DELETE OE_MAIN_ORDERJOUR.OBJ;*
	!
	! Author:
	!
	!
	! Modification history:
	!
	!	06/25/91 - Craig Tanner
	!		Added list choises to country feild.
	!
	!	08/19/91 - Dan Perkins
	!		F17 key feature added.
	!
	!	08/28/91 - Dan Perkins
	!		F14 key feature added to some fields.
	!		"VO" changed to "VX" on lookup.
	!
	!	11/06/91 - Dan Perkins
	!		Display salesman names along with
	!		salesman codes.
	!
	!	12/12/91 - Dan Perkins
	!		Fixed logic on SHIPTO salesman to display
	!		the SHIPTO salesman if record is found,
	!		else display the AR_35CUSTOM salesman.
	!		We were getting garbage until this fix.
	!
	!	02/28/92 - Dan Perkins
	!		Changed SCOPE.COM to CODES.INC and CMC$NORMAL to
	!		CMC$_NORMAL.  Maybe this will work if it needs to.
	!
	!	04/09/92 - Dan Perkins
	!		Rset ORDERNUMBER.  Use MLOOOP in FUNC_TESTENTRY.
	!
	!	04/16/92 - Dan Perkins
	!		Allow formatting of nummeric fields with /SET option.
	!
	!	06/09/92 - Dan Perkins
	!		Moved CASE 10 under OPT_TESTENTRY to MAST programs
	!		so this program could be used with OE and MO systems.
	!
	!	07/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/15/92 - Dan Perkins
	!		Added code to display Customer Aging information.
	!
	!	12/17/92 - Kevin Handy
	!		Fix pressing uparrow on notes.
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/27/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in several places.
	!
	!	06/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/13/98 - Kevin Handy
	!		Add TAXFLAG parameter to OE_READ_SALESTAX.
	!
	!	12/16/98 - Kevin Handy
	!		Make sure new deposit number field is blanked out.
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"

	!
	! Include CDD's
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR
	MAP (OE_ORDERJOUR_ONE)  OE_ORDERJOUR_CDD	OE_ORDERJOUR_ONE
	MAP (OE_ORDERJOUR_OLD)	OE_ORDERJOUR_CDD	OE_ORDERJOUR_OLD
	COM (OE_ORDERJOUR_PAGE)	OE_ORDERJOUR_CDD	OE_ORDERJOUR_PAGE
	COM (OE_ORDERJOUR_DEF)  OE_ORDERJOUR_CDD	OE_ORDERJOUR2

	%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.HB"
	MAP (OE_SALESTAX)	OE_SALESTAX_CDD		OE_SALESTAX
	DECLARE			OE_SALESTAX_CDD		OE_SALESTAX_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.HB"
	MAP (OE_SHIPTO)		OE_SHIPTO_CDD		OE_SHIPTO

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD		AR_CUSBAL
	DIM			AR_CUSBAL_CDD		ARRAY_CUSBAL(50%)

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.HB"
	MAP (UTL_STATE)		UTL_STATE_CDD		UTL_STATE

	%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.HB"
	MAP (OE_CONTROL)	OE_CONTROL_CDD		OE_CONTROL

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERTYPE.HB"
	MAP (OE_ORDERTYPE)	OE_ORDERTYPE_CDD	OE_ORDERTYPE

	%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.HB"
	MAP (OE_CATEGORY)	OE_CATEGORY_CDD		OE_CATEGORY

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN
	DECLARE			SA_SALESMAN_CDD		SA_SALESMAN_EXAM

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	DECLARE			SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM

	!
	! Common areas
	!
	COM (CH_OE_ORDERJOUR) &
		OE_ORDERJOUR.CH%

	COM (BATCH_NO) &
		BATCH_NO$ = 3%

	COM (CH_OE_CONTROL) &
		OE_CONTROL.CH%

	COM (TT_OE_TAXFLAG) &
		TAXFLAG_T$ = 40%, &
		TAXFLAG$(4%) = 40%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION AR_FUNC_AGE
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG	FUNCTION OE_READ_SALESTAX
	EXTERNAL LONG	FUNCTION SA_EXAM_SALESMAN

	!
	! Declare data
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
		SMG_WINDOW::DESCR = "Order Entry Journal " + BATCH_NO$
		SMG_WINDOW::NHELP = "OE_MAIN_ORDERJOUR"
		SMG_WINDOW::CHAN  = OE_ORDERJOUR.CH%
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 23%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::LWIDTH  = 76%
		SMG_WINDOW::LHEIGHT = 15%
		SMG_WINDOW::LHPOS   = 2%
		SMG_WINDOW::LVPOS   = 5%
		SMG_WINDOW::LLAST   = 1%
		SMG_WINDOW::LTITLE(0%) = "First Page"
		SMG_WINDOW::LPAGE(0%) = 10%
		SMG_WINDOW::LTITLE(1%) = "Last Page"
		SMG_WINDOW::LPAGE(1%) = 23%

		SMG_WINDOW::NKEYS = 3%
		SMG_WINDOW::KNAME(0%) = "Order_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "sale_Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
		SMG_WINDOW::KNAME(2%) = "Customer_number"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 5%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%

		COM (OE_MAIN_ORDERJOUR_FRM) FRM$(23%)

		!
		! TAX FLAGS
		!
		TAXFLAG_T$ = "Flag Description                   "
		TAXFLAG$(0%) = "4"
		TAXFLAG$(1%) = "1    Taxable                       "
		TAXFLAG$(2%) = "4    Resale                        "
		TAXFLAG$(3%) = "5    Out of State                  "
		TAXFLAG$(4%) = "6    Church, School, and Government"


		CALL READ_DEFAULTS(SMG_WINDOW)

20010		GOTO 20040 IF OE_ORDERJOUR.CH% > 0%

		!
		! Open OE_ORDERJOUR
		!
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.CRE"

20040		SMG_WINDOW::CHAN  = OE_ORDERJOUR.CH%
		WHEN ERROR IN
			RESET #OE_ORDERJOUR.CH%
			GET #OE_ORDERJOUR.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN


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

			DATA	1,  1, "(01) Order #", &
				2,  1, "(02) Order Date", &
				3,  1, "(03) Sale Type", &
				4,  1, "(04) Sale Cat.", &
				5,  1, "(05) Customer #", &
				9,  1, "(06) Ship to ", &
				15, 1, "(07) Cust Po#", &
				16, 1, "(08) Ship Date", &
				17, 1, "(09) Location", &
				18, 1, "(10) Operator", &
				13, 1, "     City/St. ", &
				14, 1, "     ZIP/Cntry ", &
				0,  0, ""

			RESTORE

		READ XPOS, YPOS, XSTR$
		I% = 0%
		WHILE (XPOS <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS, YPOS) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS, YPOS, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! 2nd page
		!
		CASE 1%
			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE( &
				SMG_WINDOW::LWINDOW)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::LWINDOW)

			DATA	1,  1, "(11) Ship VIA", &
				2,  1, "(12) Terms", &
				3,  1, "(13) Ord Disc %", &
				4,  1, "(14) Misc. Chrg", &
				5,  1, "(15) Freight", &
				6,  1, "(16) Tax Code", &
				7,  1, "(17) Tax Flag", &
				8,  1, "(18) Sales Tax %", &
				9,  1, "(19) Salesman", &
				10, 1, "(20) Comm %", &
				11, 1, "(21) Paid Amt", &
				12, 1, "(22) Check #", &
				13, 1, "(23) Notes", &
				0,  0, ""

			RESTORE
			XPOS = -1%
			READ XPOS, YPOS, XSTR$ UNTIL XPOS = 0%
			READ XPOS, YPOS, XSTR$

			I% = SMG_WINDOW::LPAGE(0%)
			WHILE (XPOS <> 0%)
				I% = I% + 1%
				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					XSTR$, XPOS, YPOS) &
					IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
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
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.x Order Number
	!	^*(01) Order Number\*
	!	.b
	!	.lm +5
	!	The ^*Order Number\* field enters the order number for a
	!	specific order. The system will automatically increment the order number to
	!	be the next higher number after the last order entered. The user may override
	!	the system assigned number by entering a different number and pressing
	!	^*Return\*, or accept the system assigned number by pressing ^*Return\*.
	!	If the user overrides a system assigned number, the next order number
	!	will start with the overridden number plus one.
	!	.b
	!	The field will accept 10 characters.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_ORDERJOUR::ORDNUM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;17", TEMP$, OE_ORDERJOUR::ORDNUM, &
				MFLAG OR 2%, "~R 'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Order Date
	!	^*(02) Order Date\*
	!	.b
	!	.lm +5
	!	The ^*Order Date\* field enters the date a particular order
	!	was received.
	!	.b
	!	The field will automatically default to the system date.
	!	To accept the system date, press ^*Return\*, to override the
	!	date, enter the correct date and press ^*Return\*.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry>Order Date
	!
	!--

			OE_ORDERJOUR::ORDDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;17", TEMP$, OE_ORDERJOUR::ORDDATE, MFLAG, &
				"'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x Sale Type
	!	^*(03) Sale Type\*
	!	.b
	!	.lm +5
	!	The ^*Sale Type\* field enters the
	!	code which will identify the type of sale to be entered.
	!	.b
	!	Valid codes may be viewed by pressing ^*List Choices\*.
	!	Additional Sale Type codes may be entered by pressing ^*F17\*.
	!	.b
	!	The field will accommodate two characters.
	!	.lm -5
	!
	! Index:
	!	.x Sale Entry>Sale Type
	!
	!--
			OE_ORDERJOUR::ORDTYPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;17", TEMP$, &
				OE_ORDERJOUR::ORDTYPE, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(OE_MAIN_ORDERTYPE.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::ORDTYPE = &
						OE_ORDERTYPE::ORDTYPE
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17
				V% = MAIN_WINDOW(OE_MAIN_ORDERTYPE.ID, "M")
					OE_ORDERJOUR::ORDTYPE = &
						OE_ORDERTYPE::ORDTYPE
				GOTO ReEnter

			END SELECT

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Sale Category
	!	^*(04) Sale Category\*
	!	.b
	!	.lm +5
	!	The ^*Sale Category\* field enters the
	!	sale category code relative to this specific order.
	!	.b
	!	Valid codes may be viewed by pressing ^*List Choices\*.
	!	Additional sale category codes may be entered by pressing the ^*F17\* key.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Sale Entry>Order Sale
	!
	!--
			OE_ORDERJOUR::ORDCAT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;17", TEMP$, &
				OE_ORDERJOUR::ORDCAT, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(OE_MAIN_CATEGORY.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::ORDCAT = &
						OE_CATEGORY::ORDCAT
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(OE_MAIN_CATEGORY.ID, "M")
				OE_ORDERJOUR::ORDCAT = OE_CATEGORY::ORDCAT
				GOTO ReEnter

			END SELECT

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Customer<Number
	!	^*(05) Customer _#\*
	!	.b
	!	.lm +5
	!	The ^*Customer _#\* field enters a number which
	!	references a particular customer. When a valid _# has been entered,
	!	the system will automatically display the customer information.
	!	.b
	!	Valid customer _#'s may be viewed by pressing ^*List Choices\*.
	!	.b
	!	The field will accommodate 10 characters.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry>Customer Number
	!
	!--
			OE_ORDERJOUR::CUSNUM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;17", TEMP$, &
				OE_ORDERJOUR::CUSNUM, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::CUSNUM = &
						AR_35CUSTOM::CUSNUM
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17
				V% = MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "M")
				OE_ORDERJOUR::CUSNUM = AR_35CUSTOM::CUSNUM
				GOTO ReEnter

			END SELECT

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x Ship to
	!	^*(06) Ship to\*
	!	.b
	!	.lm +5
	!	The ^*Ship to\* field
	!	enters the name and address to which an order is to be shipped. If the
	!	name and address to which the order is to be shipped is the customer
	!	entered in field (05), the information will be duplicated in field (06) by
	!	pressing ^*Return\*. Any customer may have one or more "ship to"
	!	records defined in the Customer Master file. By entering the number
	!	of a valid "ship to" name and address, the related name and address
	!	will be displayed in the record.
	!	.b
	!	Valid ship to names and addresses may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry>Ship to
	!
	!--
 Fld006Line:
			SCOPE::PRG_ITEM = "FLD006"
			OE_ORDERJOUR::SHIPLIN = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;17", TEMP$, &
				OE_ORDERJOUR::SHIPLIN, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(OE_MAIN_SHIPTO.ID, &
					"VX" + OE_ORDERJOUR::CUSNUM) = 1%
				THEN
					OE_ORDERJOUR::SHIPLIN = &
						OE_SHIPTO::LINES
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17
				IF MAIN_WINDOW(OE_MAIN_SHIPTO.ID, &
					"M0" + OE_ORDERJOUR::CUSNUM) = 1%
				THEN
					OE_ORDERJOUR::SHIPLIN = OE_SHIPTO::LINES
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_UP
				GOTO ExitFunction

			END SELECT

	!
	! Skip the rest of 06 if an address has been found
	!
		IF (TEMP$ = "Add" OR TEMP$ = "Change") AND (MFLAG AND 1%) = 0%
		THEN
			GOSUB LoadShip
			MFLAG = MFLAG OR 1% IF V% = 1%
		END IF

	!++
	! Abstract:FLD006A
	!	.lm +5
	!	^*Ship Name\*
	!	.b
	!	.lm +5
	!	The ^*Ship Name\* field enters
	!	the name for shipping the order.
	!	.lm -5
	!
	! Index:
	!	.x Ship Name
	!
	!--
 Fld006Name:
			SCOPE::PRG_ITEM = "FLD006A"

			OE_ORDERJOUR::SHIPNAM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;28", TEMP$, &
				OE_ORDERJOUR::SHIPNAM, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006Line

			END SELECT

	!++
	! Abstract:FLD006B
	!	.x Ship to Address 1
	!	^*Ship to Address 1\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 1\* field enters
	!	the first line of the address for shipping the order.
	!	.b
	!	Twenty five spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
 Fld006Add1:
			SCOPE::PRG_ITEM = "FLD006B"

			OE_ORDERJOUR::ADD1 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;28", TEMP$, &
				OE_ORDERJOUR::ADD1, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006Name

			END SELECT

	!++
	! Abstract:FLD006C
	!	.x Ship to Address 2
	!	.ts 55
	!	^* Ship to Address 2\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 2\* field enters
	!	the second line of the address for shipping the order.
	!	.b
	!	Twenty five spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
 Fld006Add2:
			SCOPE::PRG_ITEM = "FLD006C"

			OE_ORDERJOUR::ADD2 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;28", TEMP$, &
				OE_ORDERJOUR::ADD2, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006Add1

			END SELECT

	!++
	! Abstract:FLD006D
	!	.x Ship to Address 3
	!	.ts 55
	!	^*Ship to Address 3\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 3\* field enters
	!	the third line of the address for shipping the order.
	!	.b
	!	Twenty five spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
 Fld006Add3:
			SCOPE::PRG_ITEM = "FLD006D"

			OE_ORDERJOUR::ADD3 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;28", TEMP$, &
				OE_ORDERJOUR::ADD3, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006Add2

			END SELECT

	!++
	! Abstract:FLD006E
	!	.x Customer>City
	!	.ts 55
	!	^*City\*
	!	.b
	!	.lm +5
	!	The ^*City\* field enters the city in which the
	!	customer is located.
	!	.b
	!	Fifteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x City>Customer
	!
	!--
 Fld006City:
			SCOPE::PRG_ITEM = "FLD006E"

			OE_ORDERJOUR::CITY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;28", TEMP$, &
				OE_ORDERJOUR::CITY, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006Add3

			END SELECT

	!++
	! Abstract:FLD006F
	!	.x Customer>State
	!	^*State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field enters the State in which
	!	the customer is located.
	!	.b
	!	The field will accommodate a two (2) character State postal
	!	code.
	!	.lm -5
	!
	! Index:
	!	.x State>Customer
	!
	!--
 Fld006State:
			SCOPE::PRG_ITEM = "FLD006F"

			OE_ORDERJOUR::STATE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;44", TEMP$, &
				OE_ORDERJOUR::STATE, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006City

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(UTL_MAIN_STATE.ID, &
					"VX" + OE_ORDERJOUR::COUNTRY) = 1%
				THEN
					OE_ORDERJOUR::STATE = &
						UTL_STATE::STATE
				END IF
				GOTO  ReEnter

			END SELECT

	!++
	! Abstract:FLD006G
	!	.x Customer>Zip
	!	^*Zip\*
	!	.b
	!	.lm +5
	!	The ^*Zip\* field enters the zip or postal
	!	code for the area in which a customer is located.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Zip>Customer
	!
	!--
 Fld006Zip:
			SCOPE::PRG_ITEM = "FLD006G"

			OE_ORDERJOUR::ZIP = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;28", TEMP$, &
				OE_ORDERJOUR::ZIP, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006State

			END SELECT

	!++
	! Abstract:FLD006H
	!	.x Customer>Country
	!	^*Country\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field enters the country if
	!	a customer is located in a foreign country.
	!	.b
	!	Foreign country codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Country>Customer
	!
	!--

 Fld006Country:
			SCOPE::PRG_ITEM = "FLD006H"

			OE_ORDERJOUR::COUNTRY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;39", TEMP$, &
				OE_ORDERJOUR::COUNTRY, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006Zip

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "VX" + &
					OE_ORDERJOUR::COUNTRY) = 1%
				THEN
					OE_ORDERJOUR::COUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO ReEnter

			END SELECT


		CASE 7%
	!++
	! Abstract:FLD007
	!	.x Customer>Purchase Order
	!	^*(07) Customer PO _#\*
	!	.b
	!	.lm +5
	!	The ^*Customer PO _#\* field enters a customer's purchase
	!	order number.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Sales Entry>Customer Purchase Order
	!	.x Purchase Order>Customer
	!
	!--
			OE_ORDERJOUR::CUSTPO = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;17", TEMP$, &
				OE_ORDERJOUR::CUSTPO, MFLAG, &
				"'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	.x Ship Date
	!	^*(08) Ship Date\*
	!	.b
	!	.lm +5
	!	The ^*Ship Date\* field indicates the date that an
	!	order is requested or expected to be shipped.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry>Ship Date
	!
	!--
			IF (TEMP$ = "Add") AND (OE_ORDERJOUR::SHIPDATE = "")
			THEN
				OE_ORDERJOUR::SHIPDATE = OE_ORDERJOUR::ORDDATE
			END IF

			OE_ORDERJOUR::SHIPDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;17", TEMP$, OE_ORDERJOUR::SHIPDATE, MFLAG, &
				"'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	.x Location>Number
	!	^*(09) Location Number\*
	!	.b
	!	.lm +5
	!	The ^*Location Number\* field
	!	enters a user defined code which identifies a company location from
	!	which the shipment is to be made.
	!	.b
	!	Valid locations may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Sales Entry>Location Number
	!
	!--
			IF (TEMP$ = "Add") AND (OE_ORDERJOUR::LOCATION = "")
			THEN
				OE_ORDERJOUR::LOCATION = AR_35CUSTOM::LOCATION
			END IF

			OE_ORDERJOUR::LOCATION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;17", TEMP$, &
				OE_ORDERJOUR::LOCATION, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "M")
				OE_ORDERJOUR::LOCATION = &
					UTL_LOCATION::LOCATION
				GOTO ReEnter

			END SELECT

		CASE 10%
	!++
	! Abstract:FLD010
	!	.x Order Entry>Operator
	!	^*(10) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters the name or initials of the
	!	operator entering the order.
	!	.b
	!	Valid Operator codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	An entry is required in this field.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Operator
	!
	!--
			OE_ORDERJOUR::OPERATOR = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"18;17", TEMP$, &
				OE_ORDERJOUR::OPERATOR, MFLAG, &
				"'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	.x Ship VIA
	!	^*(11) Ship VIA\*
	!	.b
	!	.lm +5
	!	The ^*Ship VIA\* field enters a user defined
	!	code which identifies a particular carrier or method of shipment.
	!	.b
	!	When a valid code is entered, the description of the Ship via code
	!	will be displayed to the right of the code.
	!	.b
	!	Valid Ship Via codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	The field will accommodate two characters.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry>Ship VIA
	!
	!--
			OE_ORDERJOUR::SHIPVIA = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"1;18", TEMP$, &
				OE_ORDERJOUR::SHIPVIA, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::SHIPVIA = &
						UTL_CARRIER::CODE
				END IF
				GOTO ReEnter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(UT_MAIN_CARRIER.ID, "M")
				OE_ORDERJOUR::SHIPVIA = &
					UTL_CARRIER::CODE
				GOTO ReEnter

			END SELECT

		CASE 12%
	!++
	! Abstract:FLD012
	!	.x Terms Code
	!	^*(12) Terms Code\*
	!	.b
	!	.lm +5
	!	The ^*Terms Code\* field enters a user defined number which will
	!	identify the payment terms of the order.
	!	.b
	!	When a valid Terms code has been entered, the description of the
	!	terms will be displayed to the right of the terms code.
	!	.b
	!	Valid Terms codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry>Terms Code
	!
	!--
			IF (TEMP$ = "Add") AND (OE_ORDERJOUR::TERMS = "")
			THEN
				OE_ORDERJOUR::TERMS = AR_35CUSTOM::TERMS
			END IF

			OE_ORDERJOUR::TERMS = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"2;18", TEMP$, &
				OE_ORDERJOUR::TERMS, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UT_MAIN_TERMS.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::TERMS = &
						UTL_TERMS::CODE
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17

				IF MAIN_WINDOW(UT_MAIN_TERMS.ID, "M") = 1%
				THEN
					OE_ORDERJOUR::TERMS = &
						UTL_TERMS::CODE
				END IF
				GOTO Reenter

			END SELECT

		CASE 13%
	!++
	! Abstract:FLD013
	!	.x Order>Discount
	!	^*(13) Order Discount\*
	!	.b
	!	.lm +5
	!	The ^*Order Discount\* field enters a discount percentage
	!	which pertains to the entire order. If there is a standard order discount
	!	percentage entered in a customer master file, that discount percentage will
	!	default in this field. The default value may be overridden by entering the
	!	correct percentage and pressing ^*Return\*.
	!	.b
	!	^*Note:\* If the discount percentage is 10% the entry would be
	!	made as 10.00.
	!	.b
	!	If there is no order discount percentage, this field should be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Discount
	!	.x Order Entry>Order Discount
	!
	!--
		OE_ORDERJOUR::DISC  = ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"3;18", TEMP$, OE_ORDERJOUR::DISC, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	.x Miscellaneous Charges
	!	^*(14) Miscellaneous Charges\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Charges\* field enters any
	!	miscellaneous charges related to a specific order. A miscellaneous
	!	credit could also be recorded by entering a negative amount in this
	!	field.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99
	!	.lm -5
	!
	! Index:
	!	.x Order Entry>Miscellaneous Charges
	!
	!--
			OE_ORDERJOUR::MISC = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"4;18", TEMP$, &
				OE_ORDERJOUR::MISC, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	.x Freight
	!	^*(15) Freight\*
	!	.b
	!	.lm +5
	!	The ^*Freight\* field enters the freight charges related
	!	to a specific order.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry>Freight
	!
	!--
			OE_ORDERJOUR::FREIGHT = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"5;18", TEMP$, &
				OE_ORDERJOUR::FREIGHT, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 16%
	!++
	! Abstract:FLD016
	!	.x Tax Code
	!	^*(16) Tax Code\*
	!	.b
	!	.lm +5
	!	The ^*Tax Code\* field identifies the Sales Tax
	!	jurisdiction relating to a specific customer. This field will default
	!	to the Tax Code in the Customer Master file. The Tax code may be overridden
	!	by entering the correct code and pressing ^*Return\*.  Two spaces are available
	!	for the entry.
	!	.b
	!	Valid Tax codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry>Tax Code
	!
	!--
			OE_ORDERJOUR::TAXCODE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"6;18", TEMP$, &
				OE_ORDERJOUR::TAXCODE, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(OE_MAIN_SALESTAX.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::TAXCODE = &
						OE_SALESTAX::TAXCODE
				END IF
				GOTO Reenter

			END SELECT

		CASE 17%
	!++
	! Abstract:FLD017
	!	.x Tax Flag
	!	^*(17) Tax Flag\*
	!	.b
	!	.lm +5
	!	The ^*Tax Flag\* field enters a flag code which
	!	identifies the status of an order as it relates to sales tax.
	!	This field will default to the Tax Flag entered in a Customer Master file.
	!	The code may be overridden by entering the correct code and pressing ^*Return\*.
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
	!	^*6\* - Church, School, and Government
	!	.end table
	!	Valid tax codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry>Tax Flag
	!
	!--
			OE_ORDERJOUR::TAXFLAG = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::LWINDOW, &
				"7;18", TEMP$, &
				OE_ORDERJOUR::TAXFLAG, MFLAG, "!", MVALUE, &
				TAXFLAG$(), TAXFLAG_T$, "005")

		CASE 18%
	!++
	! Abstract:FLD018
	!	.x Sales Tax
	!	^*(18) Sales Tax\*
	!	.b
	!	.lm +5
	!	The ^*Sales Tax\* field enters the percentage of sales tax
	!	that will be charged on a specific order.
	!	.b
	!	This field will default to the one in the Customer Master File
	!	if the Tax Flag field contains a "1". If the Tax Flag field is a "4", "5",
	!	or "6", this field should be left blank.
	!	.b
	!	If the percent of sales tax to be charged is different from the default,
	!	this percent may be overridden by entering the correct percent and pressing
	!	^*Return\*.
	!	.b
	!	^*Note:\* If the sales tax percent is to be 5%, the entry would be made
	!	as 5.00.
	!	.lm -5
	!
	! Index:
	!
	!--
 !			IF (TEMP$ = "Add") AND (OE_ORDERJOUR::TAXFLAG = "1")
			IF (TEMP$ = "Add")
			THEN
				! Use function to find out tax percentages and
				! total them up.
				V% = OE_READ_SALESTAX(AR_35CUSTOM::TAXCODE, &
					AR_35CUSTOM::TAXFLAG, OE_SALESTAX_READ)

				OE_ORDERJOUR::SALESTAX = &
					OE_SALESTAX_READ::STATETAX + &
					OE_SALESTAX_READ::COUNTYTAX + &
					OE_SALESTAX_READ::CITYTAX

			END IF

			OE_ORDERJOUR::SALESTAX = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"8;18", TEMP$, OE_ORDERJOUR::SALESTAX, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)


		CASE 19%
	!++
	! Abstract:FLD019
	!	.x Salesman 1
	!	^*(19) Salesman\*
	!	.b
	!	.lm +5
	!	The ^*Salesman 1\* field enters the code for the salesman
	!	or broker assigned to the specific customer for which the order is being
	!	entered. When a valid Salesman code has been entered, the name of the salesman
	!	or broker will be displayed to the right of the code entered.
	!	.b
	!	The value of the field will default to the Salesman code entered in the Customer
	!	Master file. The default may be overridden by entering the correct salesman
	!	code and pressing ^*Return\*.
	!	.b
	!	Valid Salesman codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry>Salesman 1
	!
	!--
			OE_ORDERJOUR::SALESMAN = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"9;18", TEMP$, &
				OE_ORDERJOUR::SALESMAN, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::SALESMAN = &
						SA_SALESMAN::SALESMAN
				END IF
				GOTO Reenter

			END SELECT

		CASE 20%
	!++
	! Abstract:FLD020
	!	.x Sales Commission
	!	^*(20) Commission %\*
	!	.b
	!	.lm +5
	!	The ^*Commission %\* field
	!	enters the commission percentage for the related salesman or broker. This field
	!	will default to the commission percentage entered in the Salesman Master file.
	!	The default may be overridden by entering the correct Commission % and
	!	pressing ^*Return\*.
	!	.b
	!	Note: If the amount of commission is to be 10%, the entry would be made
	!	as 10.00.
	!	.lm -5
	!
	! Index:
	!	.x Commission %
	!	.x Order Entry>Commission %
	!
	!--
			OE_ORDERJOUR::SALCOMM = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"10;18", TEMP$, OE_ORDERJOUR::SALCOMM, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 21%
	!++
	! Abstract:FLD021
	!	.x Paid Amount
	!	^*(21) Paid Amount\*
	!	.b
	!	.lm +5
	!	The ^*Paid Amount\* field may be used in the event that a customer makes a
	!	payment at the time an order is made.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.b
	!	^*Note:\* This entry is for informational purposes only and entering
	!	the information does not negate the necessity of making an entry in
	!	the Cash Receipts Journal.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_ORDERJOUR::AMTPAID = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"11;18", TEMP$, &
				OE_ORDERJOUR::AMTPAID, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 22%
	!++
	! Abstract:FLD022
	!	.x Check Number
	!	^*(22) Check Number\*
	!	.b
	!	.lm +5
	!	The ^*Check Number\* field may be used to enter the check number in the event
	!	a customer makes a payment at the time the order is placed.
	!	.b
	!	^*Note:\* This entry is for
	!	informational purposes only and entering the information does not negate the
	!	necessity of entering the payment made into the Cash Receipts Journal.
	!	.b
	!	Six spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_ORDERJOUR::CHECK = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"12;18", TEMP$, &
				OE_ORDERJOUR::CHECK, MFLAG, &
				"'E", MVALUE)

		CASE 23%
	!++
	! Abstract:FLD023
	!	.x Notes
	!	^*(23) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* field enters any free formatted notes
	!	relative to the order.
	!	.b
	!	Forty spaces are available for entry.
	!	.lm -5
	!
	! Index:
	!
	!--
 FirstNote:
			OE_ORDERJOUR::NOTES(0%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"13;18", TEMP$, &
				OE_ORDERJOUR::NOTES(0%), MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_UP
				GOTO BypassNotes
			END SELECT

			GOTO BypassNotes IF OE_ORDERJOUR::NOTES(0%) = "" &
				AND OE_ORDERJOUR::NOTES(1%) = "" &
				AND OE_ORDERJOUR::NOTES(2%) = ""

 SecondNote:
			OE_ORDERJOUR::NOTES(1%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"14;18", TEMP$, &
				OE_ORDERJOUR::NOTES(1%), MFLAG, &
				"'E", MVALUE)

			GOTO BypassNotes IF OE_ORDERJOUR::NOTES(1%) = "" &
				AND OE_ORDERJOUR::NOTES(2%) = ""

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_UP
				GOTO FirstNote
			END SELECT

			OE_ORDERJOUR::NOTES(2%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"15;18", TEMP$, &
				OE_ORDERJOUR::NOTES(2%), MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_UP
				GOTO SecondNote
			END SELECT

 BypassNotes:

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		OE_MAIN_ORDERJOUR = 0%

		SELECT MLOOP

		CASE 1%

			!
			! Must have an order number
			!
			IF OE_ORDERJOUR::ORDNUM = ""
			THEN
				OE_MAIN_ORDERJOUR = 1%
				GOTO 32767
			END IF

			!
			! See if the order is already in the Jouranl
			!
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #OE_ORDERJOUR.CH%, &
						KEY #0% EQ OE_ORDERJOUR::ORDNUM, &
						REGARDLESS
				USE
					CONTINUE 20350 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				OE_MAIN_ORDERJOUR = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
				GOTO 32767
			END IF

20350			!
			! See if order number is already in Register file
			!
			IF (MVALUE = "ADD")
			THEN
				IF OE_READ_REGHEADER(OE_ORDERJOUR::ORDNUM, &
					OE_REGHEADER_READ) = CMC$_NORMAL
				THEN
					OE_MAIN_ORDERJOUR = 1%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Order Already Exists in register", 1%)
				END IF
			END IF

		CASE 3%
			!
			! Display the descriptions for order type
			!
			OE_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_ORDERJOUR::ORDTYPE, &
				OE_ORDERTYPE::DESCRIPTION, &
				"OE", MLOOP, "PROG", &
				"Sale Type", OE_MAIN_ORDERTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_ORDERTYPE::DESCRIPTION, &
				3%, 23%, , SMG$M_BOLD)

		CASE 4%
			!
			! Display the descriptions for category
			!
			IF OE_ORDERJOUR::ORDCAT <> ""
			THEN
				PS_MAIN_TICKETJOUR = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ORDERJOUR::ORDCAT, &
					OE_CATEGORY::DESCRIPTION, &
					"OE", MLOOP, "PROG", &
					"Sales Category", OE_MAIN_CATEGORY.ID)
			ELSE
				OE_CATEGORY::DESCRIPTION = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_CATEGORY::DESCRIPTION, 4%, 23%, , SMG$M_BOLD)

		CASE 5%
			IF FUNC_TESTENTRY(SMG_WINDOW, &
				OE_ORDERJOUR::CUSNUM, &
				AR_35CUSTOM::CUSNAM, &
				"OE", MLOOP, "PROG", &
				"Customer", AR_MAIN_35CUSTOM.ID) = 1%
			THEN
				OE_MAIN_ORDERJOUR = 1%

				AR_35CUSTOM::ADD1 = &
					STRING$(LEN(AR_35CUSTOM::ADD1), A"?"B)

				AR_35CUSTOM::ADD2 = &
					STRING$(LEN(AR_35CUSTOM::ADD2), A"?"B)

				AR_35CUSTOM::CITY = &
					STRING$(LEN(AR_35CUSTOM::CITY), A"?"B)

				AR_35CUSTOM::STATE = &
					STRING$(LEN(AR_35CUSTOM::STATE), A"?"B)

				AR_35CUSTOM::ZIP = &
					STRING$(LEN(AR_35CUSTOM::ZIP), A"?"B)

				AR_35CUSTOM::DISCOUNT = 0.0
				AR_35CUSTOM::SALESMAN = ""
			END IF

			OE_ORDERJOUR::DISC = AR_35CUSTOM::DISCOUNT
			OE_ORDERJOUR::SALESMAN = AR_35CUSTOM::SALESMAN

			!
			! Display the descriptions for customer name
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::CUSNAM, 5%, 28%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD1, 6%, 28%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD2, 7%, 28%, , SMG$M_BOLD)

			TEXT$ = LEFT(EDIT$(EDIT$( &
				AR_35CUSTOM::CITY, 128%) + ", " + &
				AR_35CUSTOM::STATE + " " + &
				AR_35CUSTOM::ZIP, 16%) + &
				SPACE$(30%), 30%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TEXT$, 8%, 28%, , SMG$M_BOLD)

			IF OE_CONTROL::DSPLQTY = "Y"
			THEN
				!
				! Zero balances
				!
				CUSBAL(J%) = 0.0 FOR J% = 0% TO 4%
				SRVCHG = 0.0

				IF AR_FUNC_AGE(AR_35CUSTOM::CUSNUM, &
					AR_35CUSTOM::METHOD, &
					OE_ORDERJOUR::ORDDATE, "", &
					NUM_ACCT%, ARRAY_CUSBAL()) = 0%
				THEN
					!
					! Accumulate aging information
					!
					FOR LOOP% = 1% TO NUM_ACCT%
						!
						! Customer total
						!
						CUSBAL(J%) = CUSBAL(J%) + &
							ARRAY_CUSBAL(LOOP%)::AGING(J%) &
							FOR J% = 0% TO 4%

						SRVCHG = SRVCHG + &
							ARRAY_CUSBAL(LOOP%)::CHARGE

					NEXT LOOP%

					TEXT$ = "   Cur    "
					DAYS% = 1%

					FOR I% = 1% TO 4%
						DAYS% = DAYS% + AR_CONTROL::AGEPER(I%)
						TEXT$ = TEXT$ + FORMAT$(DAYS%, "###") + "    "
					NEXT I%

					TEXT$ = TEXT$ + " Bal CrLimit"

					SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						TEXT$, 1%, 27%, , SMG$M_REVERSE)

					!
					! Accumulate balance
					!
					BALANCE = &
						FUNC_ROUND(CUSBAL(0%) + &
						CUSBAL(1%) + &
						CUSBAL(2%) + &
						CUSBAL(3%) + &
						CUSBAL(4%) + &
						SRVCHG, 2%)
				END IF

				TEXT$ = FORMAT$(CUSBAL(0%), " ######") + &
					FORMAT$(CUSBAL(1%), " ######") + &
					FORMAT$(CUSBAL(2%), " ######") + &
					FORMAT$(CUSBAL(3%), " ######") + &
					FORMAT$(CUSBAL(4%), " ######") + &
					FORMAT$(BALANCE, " #######")   + &
					FORMAT$(AR_35CUSTOM::CREDITLIM, " #######")

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 2%, 26%, ,)
			END IF

		CASE 6%
			IF MAIN_WINDOW(OE_MAIN_SHIPTO.ID, &
				"Q0" + OE_ORDERJOUR::CUSNUM + &
				OE_ORDERJOUR::SHIPLIN) <> 1%
			THEN
				OE_SHIPTO::NOTES(I%) = "" FOR I% = 0% TO 2%
			ELSE
				OE_ORDERJOUR::SALESMAN = OE_SHIPTO::SALESMAN &
					IF OE_SHIPTO::SALESMAN <> ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				OE_SHIPTO::NOTES(0%), 10%, 35%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				OE_SHIPTO::NOTES(1%), 11%, 35%, , &
				SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				OE_SHIPTO::NOTES(2%), 12%, 35%, , SMG$M_BOLD)

		CASE 9%
			!
			! Display the descriptions for location name
			!
			OE_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_ORDERJOUR::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"OE", MLOOP, "PROG", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 17%, 34%, , SMG$M_BOLD)

		!CASE 10%
		!
		! Test for Operator entry and paging moved into MAST
		! programs.
		!

		CASE 11%
			!
			! Display the descriptions for carrier
			!
			IF OE_ORDERJOUR::SHIPVIA <> ""
			THEN
				OE_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ORDERJOUR::SHIPVIA, &
					UTL_CARRIER::DESCR, &
					"OE", MLOOP, "PROG", &
					"Carrier", UT_MAIN_CARRIER.ID)
			ELSE
				UTL_CARRIER::DESCR = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				UTL_CARRIER::DESCR, 1%, 28%, , SMG$M_BOLD)

		CASE 12%
			!
			! Display the descriptions for terms
			!
			IF OE_ORDERJOUR::TERMS <> ""
			THEN
				OE_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ORDERJOUR::TERMS, &
					UTL_TERMS::DESCR, &
					"OE", MLOOP, "PROG", &
					"Terms", UT_MAIN_TERMS.ID)
			ELSE
				UTL_TERMS::DESCR = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				UTL_TERMS::DESCR, 2%, 28%, , SMG$M_BOLD)

		CASE 19%
			IF OE_ORDERJOUR::SALESMAN <> ""
			THEN
				OE_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
					"S" + OE_ORDERJOUR::SALESMAN, &
					SA_SALESMAN::DESCR, &
					"OE", MLOOP, "PROG", &
					"Salesman", SA_MAIN_SALESMAN.ID)
			ELSE
				SA_SALESMAN::DESCR = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				SA_SALESMAN::DESCR, 9%, 35%, , SMG$M_BOLD)

			!
			! Get default salesman's commission % on adding
			!
			IF MVALUE = "ADD" AND OE_ORDERJOUR::SALESMAN<>""
			THEN
				V% = SA_EXAM_SALESMAN(OE_ORDERJOUR::SALESMAN, &
					SA_SALESMAN_EXAM, SB_SUBACCOUNT_EXAM)

				IF SA_SALESMAN::SALESMAN = &
					OE_ORDERJOUR::SALESMAN
				THEN
					OE_ORDERJOUR::SALCOMM = &
						SA_SALESMAN::COMMPER
				ELSE
					OE_MAIN_ORDERJOUR = 1%
					CALL HELP_34MESSAGE(SCOPE, &
						"Invalid Salesman Number", &
						"W", "SA_EXAM_SALESMAN", &
						"", "INVSAL")
				END IF
			END IF

		END SELECT

	CASE OPT_DISPLAY

		!
		! Display the descriptions
		!
		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			OE_ORDERTYPE::DESCRIPTION = &
				STRING$(LEN(OE_ORDERTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(OE_MAIN_ORDERTYPE.ID, &
				"Q0" + OE_ORDERJOUR::ORDTYPE) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(OE_ORDERTYPE::DESCRIPTION, 33%), &
				3%, 23%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			OE_CATEGORY::DESCRIPTION = &
				STRING$(LEN(OE_CATEGORY::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(OE_MAIN_CATEGORY.ID, &
				"Q0" + OE_ORDERJOUR::ORDCAT) <> 1%

			OE_CATEGORY::DESCRIPTION = "" &
				IF OE_ORDERJOUR::ORDCAT = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(OE_CATEGORY::DESCRIPTION, 33%), &
				4%, 23%, , SMG$M_BOLD)
		END IF


		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			CHK_FLG% = MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
				"Q0" + OE_ORDERJOUR::CUSNUM)

			IF CHK_FLG% <> 1%
			THEN
				AR_35CUSTOM::CUSNAM = &
					STRING$(LEN(AR_35CUSTOM::CUSNAM), A"?"B)

				AR_35CUSTOM::ADD1 = &
					STRING$(LEN(AR_35CUSTOM::ADD1), A"?"B)

				AR_35CUSTOM::ADD2 = &
					STRING$(LEN(AR_35CUSTOM::ADD2), A"?"B)

				AR_35CUSTOM::CITY = &
					STRING$(LEN(AR_35CUSTOM::CITY), A"?"B)

				AR_35CUSTOM::STATE = &
					STRING$(LEN(AR_35CUSTOM::STATE), A"?"B)

				AR_35CUSTOM::ZIP = &
					STRING$(LEN(AR_35CUSTOM::ZIP), A"?"B)
			END IF


			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::CUSNAM, 5%, 28%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD1, 6%, 28%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD2, 7%, 28%, , SMG$M_BOLD)

			TEXT$ = LEFT(EDIT$(EDIT$( &
				AR_35CUSTOM::CITY, 128%) + ", " + &
				AR_35CUSTOM::STATE + " " + &
				AR_35CUSTOM::ZIP, 16%) + &
				SPACE$(30%), 30%)

			TEXT$ = STRING$(LEN(TEXT$), A"?"B) &
				IF CHK_FLG% <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TEXT$, 8%, 28%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN
			OE_SHIPTO::NOTES(I%) = "" FOR I% = 0% TO 2% &
				IF MAIN_WINDOW(OE_MAIN_SHIPTO.ID, &
				"Q0" + OE_ORDERJOUR::CUSNUM + &
				OE_ORDERJOUR::SHIPLIN) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				OE_SHIPTO::NOTES(0%), 10%, 35%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				OE_SHIPTO::NOTES(1%), 11%, 35%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				OE_SHIPTO::NOTES(2%), 12%, 35%, , SMG$M_BOLD)

		END IF

		IF (SMG_WINDOW::HFLAG(9%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = &
				STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"Q0" + OE_ORDERJOUR::LOCATION) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 17%, 34%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(11%) AND 2%) = 0%
		THEN
			UTL_CARRIER::DESCR = "" &
				IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, &
				"Q0" + OE_ORDERJOUR::SHIPVIA) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				UTL_CARRIER::DESCR, 1%, 28%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(12%) AND 2%) = 0%
		THEN
			UTL_TERMS::DESCR = "" &
				IF MAIN_WINDOW(UT_MAIN_TERMS.ID, &
				"Q0" + OE_ORDERJOUR::TERMS) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				UTL_TERMS::DESCR, 2%, 28%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(19%) AND 2%) = 0%
		THEN
			IF OE_ORDERJOUR::SALESMAN = ""
			THEN
				SA_SALESMAN::DESCR = ""
			ELSE
				SA_SALESMAN::DESCR = &
					STRING$(LEN(SA_SALESMAN::DESCR), A"?"B) &
					IF  MAIN_WINDOW(SA_MAIN_SALESMAN.ID, &
					"Q0S" + OE_ORDERJOUR::SALESMAN) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				SA_SALESMAN::DESCR, 9%, 35%, , SMG$M_BOLD)
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			SPACE$(51%), 1%, 27%, ,)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			SPACE$(51%), 2%, 27%, ,)

	!
	! Set OE_ORDERJOUR_OLD value
	!
20500	CASE OPT_SETOLD
		OE_ORDERJOUR_OLD = OE_ORDERJOUR

	!
	! Restore OE_ORDERJOUR_OLD value
	!
	CASE OPT_RESETOLD
		OE_ORDERJOUR = OE_ORDERJOUR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_ORDERJOUR2 = OE_ORDERJOUR
		OE_ORDERJOUR2::DEPOSIT = ""

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(13%) = "##.##"
				FRM$(18%) = "##.###"
				FRM$(20%) = "##.##"
				FRM$(22%) = "##.##"
				FRM$(23%) = "##.##"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_ORDERJOUR = OE_ORDERJOUR2

		IF MFLAG = 1%
		THEN
			OE_ORDERJOUR::ORDDATE = DATE_TODAY &
				IF OE_ORDERJOUR::ORDDATE = ""
			OE_ORDERJOUR::STATE = "US" &
				IF OE_ORDERJOUR::STATE = ""

			GOSUB GetRec IF OE_ORDERJOUR::ORDNUM = ""

			OE_ORDERJOUR::COMMAMT  = 0.0
			OE_ORDERJOUR::COMMPERC = 0.0
		END IF
	!
	! View the Record.
	!
	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = "  OrderNumber     Date       Type   Category   CusNumber  ShipName"

		CASE 2%
			MVALUE = "017,029,035,047,058"

		CASE 3%
			MVALUE = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + "      "  + &
				PRNT_DATE(OE_ORDERJOUR::ORDDATE, 8%) + " " + &
				OE_ORDERJOUR::ORDTYPE + "     " + &
				OE_ORDERJOUR::ORDCAT + "       " + &
				OE_ORDERJOUR::CUSNUM + " " + &
				OE_ORDERJOUR::SHIPNAM

		END SELECT

	!
	! Find the Order Number.
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #OE_ORDERJOUR.CH%, &
				KEY #0% GE OE_ORDERJOUR::ORDNUM + "", &
				REGARDLESS

		CASE 1%
			FIND #OE_ORDERJOUR.CH%, KEY #1% GE &
				OE_ORDERJOUR::ORDTYPE + OE_ORDERJOUR::ORDNUM, &
				REGARDLESS

		CASE 2%
			FIND #OE_ORDERJOUR.CH%, KEY #2% GE &
				OE_ORDERJOUR::CUSNUM + OE_ORDERJOUR::ORDNUM, &
				REGARDLESS

		END SELECT


	END SELECT


 ExitFunction:
	EXIT FUNCTION

 LoadShip:

	IF EDIT$(OE_ORDERJOUR::SHIPLIN, -1%) <> ""
	THEN
		V% = MAIN_WINDOW(OE_MAIN_SHIPTO.ID, &
			"Q0" + OE_ORDERJOUR::CUSNUM + &
			OE_ORDERJOUR::SHIPLIN)

		IF V% = 1%
		THEN
			OE_ORDERJOUR::SHIPNAM  = OE_SHIPTO::SHIPNAM
			OE_ORDERJOUR::ADD1     = OE_SHIPTO::ADD1
			OE_ORDERJOUR::ADD2     = OE_SHIPTO::ADD2
			OE_ORDERJOUR::ADD3     = OE_SHIPTO::ADD3
			OE_ORDERJOUR::CITY     = OE_SHIPTO::CITY
			OE_ORDERJOUR::STATE    = OE_SHIPTO::STATE
			OE_ORDERJOUR::ZIP      = OE_SHIPTO::ZIP
			OE_ORDERJOUR::COUNTRY  = OE_SHIPTO::COUNTRY
			OE_ORDERJOUR::TAXCODE  = OE_SHIPTO::TAXCODE
			OE_ORDERJOUR::TAXFLAG  = AR_35CUSTOM::TAXFLAG
			OE_ORDERJOUR::SHIPVIA  = OE_SHIPTO::SHIPVIA
			OE_ORDERJOUR::LOCATION = OE_SHIPTO::LOCATION
		ELSE
			OE_ORDERJOUR::SHIPNAM = ""
			OE_ORDERJOUR::ADD1 = ""
			OE_ORDERJOUR::ADD2 = ""
			OE_ORDERJOUR::ADD3 = ""
			OE_ORDERJOUR::CITY = ""
			OE_ORDERJOUR::STATE = ""
			OE_ORDERJOUR::ZIP = ""
			OE_ORDERJOUR::COUNTRY = ""
		END IF
	ELSE
		V% = 1%
		OE_ORDERJOUR::SHIPNAM  = AR_35CUSTOM::CUSNAM
		OE_ORDERJOUR::ADD1     = AR_35CUSTOM::ADD1
		OE_ORDERJOUR::ADD2     = AR_35CUSTOM::ADD2
		OE_ORDERJOUR::ADD3     = AR_35CUSTOM::ADD3
		OE_ORDERJOUR::CITY     = AR_35CUSTOM::CITY
		OE_ORDERJOUR::STATE    = AR_35CUSTOM::STATE
		OE_ORDERJOUR::ZIP      = AR_35CUSTOM::ZIP
		OE_ORDERJOUR::COUNTRY  = AR_35CUSTOM::COUNTRY
		OE_ORDERJOUR::TAXCODE  = AR_35CUSTOM::TAXCODE
		OE_ORDERJOUR::TAXFLAG  = AR_35CUSTOM::TAXFLAG
		OE_ORDERJOUR::SHIPVIA  = AR_35CUSTOM::CARRIER
		OE_ORDERJOUR::LOCATION = AR_35CUSTOM::LOCATION
	END IF

	RETURN

 GetRec:
	!
	! Open OE_ORDERJOUR
	!
20800	IF OE_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.CRE"

			!
			! Get the OE_CONTROL record
			!
			GET #OE_CONTROL.CH%, RECORD 1%
			V% = FUNC_INCREMENT(OE_CONTROL::ORDNUM)
			UPDATE #OE_CONTROL.CH%
		USE
			CONTINUE 20850 IF ERR = 9% OR ERR = 155%
			EXIT HANDLER
		END WHEN
	END IF

	GOTO AssgNumber

20850	OE_CONTROL::ORDNUM      = "1"
	OE_CONTROL::PURGDATE    = ""
	OE_CONTROL::DSPLPRICE   = "N"
	OE_CONTROL::DSPLQTY     = "N"
	OE_CONTROL::LISTCODE    = ""
	OE_CONTROL::MISCTYPE    = ""
	OE_CONTROL::STATUS_FLAG = "0"
	OE_CONTROL::LAST_MEMO   = ""
	OE_CONTROL::LAST_INV    = ""
	OE_CONTROL::AGEPER(0)   = 5%
	OE_CONTROL::AGENAM(0)   = "1 to 5 days"
	OE_CONTROL::AGEPER(1)   = 5%
	OE_CONTROL::AGENAM(1)   = "6 to 10 days"
	OE_CONTROL::AGEPER(2)   = 5%
	OE_CONTROL::AGENAM(2)   = "11 to 15 days"
	OE_CONTROL::AGEPER(3)   = 15%
	OE_CONTROL::AGENAM(3)   = "16 to 30 days"
	OE_CONTROL::AGEPER(4)   = 0%
	OE_CONTROL::AGENAM(4)   = "31 days and over"

	PUT #OE_CONTROL.CH%

 AssgNumber:
	OE_ORDERJOUR::ORDNUM = OE_CONTROL::ORDNUM
	UNLOCK #OE_CONTROL.CH%

	RETURN

	%PAGE

29000	!*******************************************************************
	!	Help Errors
	!*******************************************************************

	!
	! Resume to display untrapped error
	!
	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:LINE
	!	^*Line\*
	!	.B
	!	.lm +5
	!	The ^*Line\* function
	!	enters a line of data for each inventory item
	!	ordered by a customer.
	!	.b
	!	The fields in the line item screen include:
	!	.table 3,25
	!	.te
	!	Product [Number]
	!	.te
	!	Quantity Ordered
	!	.te
	!	Quantity to Ship
	!	.te
	!	Quantity Backordered
	!	.te
	!	Unit Price
	!	.te
	!	Promo Amount Off
	!	.te
	!	Misc. Charges
	!	.te
	!	Discount %
	!	.te
	!	Unit Cost
	!	.te
	!	Request Date
	!	.te
	!	Line Notes
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Line Item>Sales Entry Journal
	!
	!--
	!+-+-+
	!++
	! Abstract:FORM
	!	^*forM\*
	!	.b
	!	.lm +5
	!	The ^*forM\* option prints the
	!	appropriate form(s) for a record(s) in a journal.  The system will
	!	automatically insert the appropriate batch number in the Batch Number
	!	field as well as the reference number of
	!	the record from which the ^*forM\* Command is accessed into the From and
	!	To fields.  If forms for records other than the record indicated in the
	!	From and To fields are to be printed, those
	!	field must be changed or blanked.
	!	.b
	!	In some journals, a record contains a field indicating whether or not a
	!	form for a record has been printed.  In those cases, when a form for a record
	!	is printed, the Form Printed field in the record is automatically changed from
	!	an ^*N\* for "no" to a ^*Y\* for "yes".  A form will not be reprinted for the
	!	related record unless the Form Printed field is manually changed to an ^*N\*.
	!	.b
	!	See the Help message for ^*form__direcT\* in the COMMAND Menu for additional
	!	information on how a form for a single record may be printed.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FORM_DIRECT
	!	^*form-direcT\*
	!	.lm +5
	!	.b
	!	The ^*form-direcT\* option prints
	!	a form for a journal record without having to access a report setting screen.
	!	The execution of the option will print only one form at a time.  The form
	!	printed is relative to the journal record from which the option is accessed.
	!	.b
	!	In some journals, a record contains a field indicating whether or not a form
	!	for a record has been printed.  In those cases, when a form for a record is
	!	printed, the Form Printed field in the record is automatically changed from an
	!	^*N\* for "no" to a ^*Y\* for "yes".  A form will not be reprinted for the
	!	related record unless the Form Printed field is manually changed to an
	!	^*N\*.
	!	.b
	!	Since there is no report setting screen accessed, the Destination Device
	!	and the Printer settings are defaulted to those settings established in
	!	the report setting screen accessed from the ^*forM\* option in the
	!	COMMAND Menu.
	!	.b
	!	See the Help message for ^*forM\* in the COMMAND Menu for additional
	!	information on how a form(s) may be printed.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:LINE_MANU
	!	^*line__manU\*
	!	.lm +5
	!	.b
	!	The ^*line__manU\* (or Manufacturing Order Entry Lines) option
	!	enters specific information required for a
	!	manufacturing order.
	!	.b
	!	The fields in the manufacturing line item screen include:
	!	.table 3,25
	!	.te
	!	Make
	!	.te
	!	Make Year
	!	.te
	!	Make Type
	!	.te
	!	Make Size
	!	.te
	!	Model Code
	!	.te
	!	Quantity Ordered
	!	.te
	!	Unit Price
	!	.te
	!	Unit Cost
	!	.te
	!	Requested Date
	!	.end table
	!	.b
	!	The Extended Price is calculated and displayed on the screen.
	!	.b
	!	Option in the COMMAND Menu provides access to the screen
	!	where various option records may be added.
	!	.lm -5
	!
	! Index:
	!
	!--
