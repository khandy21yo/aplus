1	%TITLE "Credit Memo Journal"
	%SBTTL "OE_MAIN_CREDITJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_CREDITJOUR(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	Credit Memo information is maintained through the
	!	^*Credit Memo Journal\*.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Credit Memo Journal
	!	.x Batch Number>User
	!	.x User Batch Number
	!	.x Journal>Entry Maintain
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_CREDITJOUR/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN OE_MAIN_CREDITJOUR
	!	$ DELETE OE_MAIN_CREDITJOUR.OBJ;*
	!
	! Author:
	!
	!	08/29/91 - Dan Perkins
	!
	! Modification history:
	!
	!	10/15/91 - Dan Perkins
	!		Added more fields into function
	!		to accomodate changes in file
	!		layout.
	!
	!	05/06/92 - Dan Perkins
	!		Changed SalesTax field to a
	!		percentage instead of a number.
	!		Allow formatting of nummeric fields with /SET option.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/26/92 - Dan Perkins
	!		Display TERMS on top of second page.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/01/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/09/93 - Dan Perkins
	!		Removed REASON field because it was placed in
	!		the LINE file.
	!
	!	08/25/93 - Frank F. Starman
	!		Display terms code with description.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	06/06/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/13/98 - Kevin Handy
	!		Add TAXFLAG parameter ot OE_READ_SALESTAX
	!
	!	10/16/98 - Kevin Handy
	!		Fix salestax bug (ar instead of oe flag used)
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
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
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! Include CDD
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITJOUR.HB"
	MAP (OE_CREDITJOUR)	OE_CREDITJOUR_CDD	OE_CREDITJOUR
	MAP (OE_CREDITJOUR_OLD)	OE_CREDITJOUR_CDD	OE_CREDITJOUR_OLD, &
							OE_CREDITJOUR_DEF

	%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.HB"
	MAP (OE_CONTROL)	OE_CONTROL_CDD		OE_CONTROL

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERTYPE.HB"
	MAP (OE_ORDERTYPE)	OE_ORDERTYPE_CDD	OE_ORDERTYPE

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.HB"
	DECLARE			OE_SALESTAX_CDD		OE_SALESTAX_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.HB"
	MAP (OE_SHIPTO)		OE_SHIPTO_CDD		OE_SHIPTO

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.HB"
	MAP (UTL_STATE)		UTL_STATE_CDD		UTL_STATE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	!
	! Include common
	!
	COM (CH_OE_CREDITJOUR) &
		BATCH_NO$ = 2%, &
		OE_CREDITJOUR.CH%

	COM (CH_OE_CONTROL) &
		OE_CONTROL.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG	FUNCTION OE_READ_SALESTAX

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
		SMG_WINDOW::DESCR = "Credit Memo Journal " + BATCH_NO$
		SMG_WINDOW::NHELP = "OE_MAIN_CREDITJOUR"
		SMG_WINDOW::CHAN  = OE_CREDITJOUR.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 128%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 16%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::LWIDTH  = 78%
		SMG_WINDOW::LHEIGHT = 15%
		SMG_WINDOW::LHPOS   = 2%
		SMG_WINDOW::LVPOS   = 5%
		SMG_WINDOW::LLAST   = 1%
		SMG_WINDOW::LTITLE(0%) = "First Page"
		SMG_WINDOW::LPAGE(0%)  = 8%
		SMG_WINDOW::LTITLE(1%) = "Last Page"
		SMG_WINDOW::LPAGE(1%)  = 16%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Memo_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Customer_number"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 5%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		COM (OE_MAIN_CREDITJOUR_FRM) FRM$(16%)

		CALL READ_DEFAULTS(SMG_WINDOW)

20010		GOTO 20040 IF OE_CREDITJOUR.CH% > 0%

		!
		! Open OE_CREDITJOUR
		!
		%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITJOUR.CRE"

20040		SMG_WINDOW::CHAN  = OE_CREDITJOUR.CH%
		WHEN ERROR IN
			RESET #OE_CREDITJOUR.CH%
			GET #OE_CREDITJOUR.CH%, REGARDLESS
		USE
			CONTINUE ExitFunction IF ERR = 11%
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

			DATA	1,   2, "(01) Memo #", &
				2,   2, "(02) Memo Date", &
				3,   2, "(03) Operator", &
				4,   2, "(04) Order Date", &
				5,   2, "(05) Sale Type", &
				6,   2, "(06) Customer Number", &
				11,  2, "(07) Ship to", &
				17,  2, "(08) Location", &
				1, 39, "Terms", &
				15,  2, "     City/St.", &
				16,  2, "     Zip/Cntry", &
				0,   0, ""

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

			DATA	1,  2, "(09) Handling", &
				2,  2, "(10) Discount", &
				3,  2, "(11) Misc. Charges", &
				4,  2, "(12) Misc. GL Account #", &
				5,  2, "(13) Freight", &
				6,  2, "(14) Sales Tax %", &
				7,  2, "(15) Salesman", &
				10, 2, "(16) Notes", &
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
	!	.x Memo Number
	!	^*(01) Memo Number\*
	!	.b
	!	.lm +5
	!	The ^*Memo Number\* field enters
	!	the identifying number of the credit memo.  The system assigned number may
	!	be used by pressing ^*RETURN\*, or another number may be used by entering
	!	the correct number and pressing ^*RETURN\*.  Each time a memo is added, the
	!	system assigned number will increment by 1.
	!	.b
	!	The field will accept up to 8 characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITJOUR::MEMONUM = ENTR_3STRING( &
				SCOPE, SMG_WINDOW::WNUMBER, &
				"1;25", TEMP$, OE_CREDITJOUR::MEMONUM, &
				MFLAG, "~L0'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Memo Date
	!	^*(02) Memo Date\*
	!	.b
	!	.lm +5
	!	The ^*Memo Date\* field enters
	!	the date of the credit memo transaction.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	The field will automatically default to the
	!	system date. To override the default, enter the
	!	correct date and press Return. To accept the system
	!	date, press Return.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITJOUR::MEMODATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;25", TEMP$, OE_CREDITJOUR::MEMODATE, &
				MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x Operator
	!	^*(03) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters
	!	the name of the operator issuing the Credit Memo.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITJOUR::OPERATOR = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;25", TEMP$, OE_CREDITJOUR::OPERATOR, &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Order Date
	!	^*(04) Order Date\*
	!	.b
	!	.lm +5
	!	The ^*Order Date\* field enters
	!	the date of the orignial order.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	The field will automatically default to the
	!	system date. To override the default, enter the
	!	correct date and press Return. To accept the default
	!	date, press Return.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITJOUR::ORDDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;25", TEMP$, OE_CREDITJOUR::ORDDATE, &
				MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Sale Type
	!	^*(05) Sale Type\*
	!	.b
	!	.lm +5
	!	The ^*Sale Type\* field enters
	!	the Type of Sale the Credit Memo is being issued for.
	!	.b
	!	The field will accept two characters.
	!	.b
	!	Valid Sale Types may be viewed by pressing ^*List Choices\*.
	!	Additional Sale Types may be added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITJOUR::ORDTYPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;25", TEMP$, OE_CREDITJOUR::ORDTYPE, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(OE_MAIN_ORDERTYPE.ID, "VX") = 1%
				THEN
					OE_CREDITJOUR::ORDTYPE = &
						OE_ORDERTYPE::ORDTYPE
				END IF
				GOTO Reenter

			END SELECT

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x Customer<Number
	!	^*(06) Customer _#\*
	!	.b
	!	.lm +5
	!	The ^*Customer _#\* field enters a number which
	!	references the customer for the original order.
	!	.b
	!	If the Customer _# entered is a valid customer number, the name and
	!	address information will be displayed on the screen.
	!	.b
	!	Valid Customer numbers may be viewed by pressing ^*List Choices\*.
	!	Additional Customer numbers may be added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITJOUR::CUSNUM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;25", TEMP$, OE_CREDITJOUR::CUSNUM, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "VX") = 1%
				THEN
					OE_CREDITJOUR::CUSNUM = &
						AR_35CUSTOM::CUSNUM
				END IF
				GOTO Reenter
			END SELECT

		CASE 7%
 Fld007Name:
	!++
	! Abstract:FLD007
	!	^*Ship To\*
	!	.b
	!	.lm +5
	!	The ^*Ship to\* field
	!	enters the name and address to which an order is
	!	to be shipped. If the name and address to which the order
	!	is to be shipped is the same as the customer name and address, the
	!	customer's name and address will be duplicated by pressing
	!	^*Return\*. Any customer may have one or more "ship to"
	!	records defined in the Customer Master file. By entering the
	!	number of a valid "ship to" name and address, the related
	!	name and address will be displayed in the field.
	!	.b
	!	Valid Ship To codes associated with this customer may be
	!	viewed by pressing ^*List Choices\*.
	!	.b
	!	If an invalid number is entered, the "ship to" name and address
	!	information will be blank.
	!	.lm -5
	!
	! Index:
	!	.x Ship To
	!
	!--
			OE_CREDITJOUR::SHIPNAM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;25", TEMP$, OE_CREDITJOUR::SHIPNAM, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO ExitFunction

			CASE SMG$K_TRM_F14
				OE_SHIPTO::SALESMAN = ""

				IF MAIN_WINDOW(OE_MAIN_SHIPTO.ID, "V0" &
					+ OE_CREDITJOUR::CUSNUM) = 1%
				THEN
					OE_CREDITJOUR::SHIPNAM = &
						OE_SHIPTO::SHIPNAM

					OE_CREDITJOUR::ADD1= &
						OE_SHIPTO::ADD1

					OE_CREDITJOUR::ADD2= &
						OE_SHIPTO::ADD2

					OE_CREDITJOUR::ADD3= &
						OE_SHIPTO::ADD3

					OE_CREDITJOUR::CITY= &
						OE_SHIPTO::CITY

					OE_CREDITJOUR::ZIP = &
						OE_SHIPTO::ZIP

					OE_CREDITJOUR::STATE= &
						OE_SHIPTO::STATE

					OE_CREDITJOUR::COUNTRY= &
						OE_SHIPTO::COUNTRY

					OE_CREDITJOUR::SALESMAN = &
						OE_SHIPTO::SALESMAN &
						IF OE_SHIPTO::SALESMAN <> ""
				END IF
				GOTO Reenter
			END SELECT

	!++
	! Abstract:FLD007B
	!	.x Ship to Address 1
	!	^*Ship to Address 1\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 1\* field enters
	!	the first line of the address for shipping the order.
	!	.b
	!	Up to twenty five characters will be accepted.
	!	.lm -5
	!
	! Index:
	!
	!--
 Fld007Add1:
			SCOPE::PRG_ITEM = "FLD007B"

			OE_CREDITJOUR::ADD1 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;25", TEMP$, &
				OE_CREDITJOUR::ADD1, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld007Name

			END SELECT

	!++
	! Abstract:FLD007C
	!	.x Ship to Address 2
	!	^* Ship to Address 2\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 2\* field enters
	!	the second line of the address for shipping the order.
	!	.b
	!	The field will accept up to 25 characters.
	!	.lm -5
	!
	! Index:
	!
	!--
 Fld007Add2:
			SCOPE::PRG_ITEM = "FLD007C"

			OE_CREDITJOUR::ADD2 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;25", TEMP$, &
				OE_CREDITJOUR::ADD2, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld007Add1

			END SELECT

	!++
	! Abstract:FLD007D
	!	.x Ship to Address 3
	!	^* Ship to Address 3\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 3\* field enters
	!	the third line of the address for shipping the order.
	!	.b
	!	The field will accept up to 25 characters.
	!	.lm -5
	!
	! Index:
	!
	!--
 Fld007Add3:
			SCOPE::PRG_ITEM = "FLD007D"

			OE_CREDITJOUR::ADD3 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;25", TEMP$, &
				OE_CREDITJOUR::ADD3, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld007Add2

			END SELECT

	!++
	! Abstract:FLD007E
	!	.x Customer>City
	!	^*City\*
	!	.b
	!	.lm +5
	!	The ^*City\* field enters the city in which the
	!	Customer is located.
	!	.b
	!	The field will accept 15 characters.
	!	.lm -5
	!
	! Index:
	!	.x City>Customer
	!
	!--
 Fld007City:
			SCOPE::PRG_ITEM = "FLD007E"

			OE_CREDITJOUR::CITY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;25", TEMP$, &
				OE_CREDITJOUR::CITY, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld007Add3

			END SELECT

	!++
	! Abstract:FLD007F
	!	.x Customer>State
	!	^*State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field enters the State in which
	!	the Customer is located.
	!	.b
	!	The field will accommodate a two (2) character State postal
	!	code.
	!	.lm -5
	!
	! Index:
	!	.x State>Customer
	!
	!--
 Fld007State:
			SCOPE::PRG_ITEM = "FLD007F"

			OE_CREDITJOUR::STATE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;44", TEMP$, &
				OE_CREDITJOUR::STATE, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld007City

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(UTL_MAIN_STATE.ID, "VX" &
					+  OE_CREDITJOUR::COUNTRY) = 1%
				THEN
					OE_CREDITJOUR::STATE = &
						UTL_STATE::STATE
				END IF
				GOTO  ReEnter

			END SELECT

	!++
	! Abstract:FLD007G
	!	.x Customer>Zip
	!	^*Zip\*
	!	.b
	!	.lm +5
	!	The ^*Zip\* field enters the zip or postal
	!	code for the area in which a Customer is located.
	!	.b
	!	The field will accept up to 10 characters.
	!	.lm -5
	!
	! Index:
	!	.x Zip>Customer
	!
	!--
 Fld007Zip:
			SCOPE::PRG_ITEM = "FLD007G"

			OE_CREDITJOUR::ZIP = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;25", TEMP$, &
				OE_CREDITJOUR::ZIP, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld007State

			END SELECT

	!++
	! Abstract:FLD007H
	!	.x Customer>Country
	!	^*Country\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field enters the country if
	!	the Customer is located in a foreign country.
	!	.b
	!	Two spaces are available for entry.
	!	.b
	!	Valid Country codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Country>Customer
	!
	!--

 Fld007Country:
			SCOPE::PRG_ITEM = "FLD007H"

			OE_CREDITJOUR::COUNTRY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;39", TEMP$, &
				OE_CREDITJOUR::COUNTRY, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld007Zip

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "VX" &
					+ OE_CREDITJOUR::COUNTRY) = 1%
				THEN
					OE_CREDITJOUR::COUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO ReEnter

			END SELECT

		CASE 8%
	!++
	! Abstract:FLD008
	!	.x Location>Number
	!	^*(08) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field
	!	enters a user defined code which identifies a company location from
	!	which the shipment was made.
	!	.b
	!	Four spaces are available for the entry.
	!	.b
	!	Valid Location codes may be viewed by pressing ^*List Choices\*.
	!	Additional Location codes may be added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITJOUR::LOCATION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;25", TEMP$, &
				OE_CREDITJOUR::LOCATION, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%
				THEN
					OE_CREDITJOUR::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter

			END SELECT

		CASE 9%
	!++
	! Abstract:FLD009
	!	.x Credit Memo>Handling
	!	^*(09) Handling\*
	!	.b
	!	.lm +5
	!	The ^*Handling\* field enters the amount of
	!	handling charges to be included in the credit memo.
	!	.b
	!	The field may contain a figure as large as 99,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Handling
	!
	!--
			OE_CREDITJOUR::HANDLING = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"1;23", TEMP$, &
				OE_CREDITJOUR::HANDLING, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	.x Order>Discount
	!	^*(11) Discount\*
	!	.b
	!	.lm +5
	!	The ^*Discount\* field enters a discount percentage
	!	which pertains to the credit memo.  If there is no such discount, the field
	!	would be left blank.
	!	.b
	!	As an example, if the discount is to be 10%, the entry
	!	would be made as 10.00.
	!	.lm -5
	!
	! Index:
	!	.x Discount
	!
	!--
			OE_CREDITJOUR::DISC = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"2;23", TEMP$, &
				OE_CREDITJOUR::DISC, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	.x Miscellaneous Charges
	!	^*(11) Miscellaneous Charges\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Charges\* field enters any
	!	miscellaneous charges related to the credit memo.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Order>Miscellaneous Charges
	!
	!--
			OE_CREDITJOUR::MISC = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"3;23", TEMP$, &
				OE_CREDITJOUR::MISC, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	.ts 55
	!	^*(12) Miscellaneous Account\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Account\* field enters
	!	the General Ledger Account number for the miscellaneous
	!	charge entered in field (12) for the credit memo.
	!	.b
	!	Valid Account Numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Miscellaneous Account
	!
	!--
			OE_CREDITJOUR::MISCACCT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"4;27", TEMP$, &
				OE_CREDITJOUR::MISCACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					OE_CREDITJOUR::MISCACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 13%
	!++
	! Abstract:FLD013
	!	.x Freight
	!	^*(13) Freight\*
	!	.b
	!	.lm +5
	!	The ^*Freight\* field enters the freight charges
	!	for the credit memo.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Order>Freight
	!
	!--
			OE_CREDITJOUR::FREIGHT = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"5;23", TEMP$, &
				OE_CREDITJOUR::FREIGHT, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 14%
	!++
	! Abstract:FLD015
	!	.x Salesmen
	!	^*(15) Salesmen\*
	!	.b
	!	.lm +5
	!	The ^*Salesmen\* field enters the code for the salesman
	!	or broker assigned to the specific customer related to the credit memo
	!	being issued.
	!	.b
	!	The value of the field will default to the Salesman field in the Customer
	!	Master file. The default may be overridden by entering the correct code and
	!	pressing ^*Return\*.
	!	.b
	!	Ten spaces are available for entry.
	!	.b
	!	Valid Salesman codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			IF (TEMP$ = "Add")
			THEN
				!
				! Use function to find out tax percentages and
				! total them up.
				!
				V% = OE_READ_SALESTAX(AR_35CUSTOM::TAXCODE, &
					AR_35CUSTOM::TAXFLAG, &
					OE_SALESTAX_READ)

				OE_CREDITJOUR::SALESTAX = &
					OE_SALESTAX_READ::STATETAX + &
					OE_SALESTAX_READ::COUNTYTAX + &
					OE_SALESTAX_READ::CITYTAX

			END IF

			OE_CREDITJOUR::SALESTAX = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"6;23", TEMP$, &
				OE_CREDITJOUR::SALESTAX, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	.x Salesmen
	!	^*(15) Salesmen\*
	!	.b
	!	.lm +5
	!	The ^*Salesmen\* field enters the code for the salesman
	!	or broker assigned to the specific customer related to the credit memo
	!	being issued.
	!	.b
	!	The value of the field will default to the Salesman field in the Customer
	!	Master file. The default may be overridden by entering the correct code and
	!	pressing ^*Return\*.
	!	.b
	!	Ten spaces are available for entry.
	!	.b
	!	Valid Salesman codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREDITJOUR::SALESMAN = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"7;23", TEMP$, OE_CREDITJOUR::SALESMAN, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "VX") = 1%
				THEN
					OE_CREDITJOUR::SALESMAN = &
						SA_SALESMAN::SALESMAN
				END IF
				GOTO Reenter

			END SELECT

		CASE 16%
	!++
	! Abstract:FLD016
	!	.x Notes
	!	^*(16) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* field enters any free formatted notes
	!	relative to the credit memo.
	!	.b
	!	The field will accept up to 80 characters.
	!	.lm -5
	!
	! Index:
	!
	!--
 FirstNote:
			OE_CREDITJOUR::NOTES(0%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"10;23", TEMP$, &
				OE_CREDITJOUR::NOTES(0%), MFLAG, &
				"'E", MVALUE)

			OE_CREDITJOUR::NOTES(1%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"11;23", TEMP$, &
				OE_CREDITJOUR::NOTES(1%), MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_UP
				GOTO FirstNote
			END SELECT


		END SELECT

		SCOPE::PRG_ITEM = TEMP$
	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		OE_MAIN_CREDITJOUR = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Fix OE_CREDITJOUR::MEMOMUM
			!
			IF OE_CREDITJOUR::MEMONUM = ""
			THEN
				OE_MAIN_CREDITJOUR = 1%
				GOTO ExitFunction
			END IF

			SELECT MVALUE

			CASE "ADD"
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, &
						KEY #0% EQ &
						OE_CREDITJOUR::MEMONUM + "", &
						REGARDLESS
				USE
					CONTINUE ExitFunction IF ERR = 155%
					EXIT HANDLER
				END WHEN

				OE_MAIN_CREDITJOUR = 2%

				CALL HELP_34MESSAGE(SCOPE, &
					"Memo number already selected", &
					"W", SCOPE::PRG_PROGRAM, &
					"OE_CREDITJOUR", "134")

				GOTO ExitFunction

			END SELECT

		CASE 3%
			!
			! Force Operator entry
			!
			IF OE_CREDITJOUR::OPERATOR = ""
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Must enter an operator", 1%)

				OE_MAIN_CREDITJOUR = 1%
				GOTO ExitFunction
			END IF

		CASE 5%
			OE_MAIN_CREDITJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_CREDITJOUR::ORDTYPE, &
				OE_ORDERTYPE::DESCRIPTION, &
				"OE", MLOOP, "PROG", &
				"Sale Type", OE_MAIN_ORDERTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_ORDERTYPE::DESCRIPTION, &
				5%, 45%, , SMG$M_BOLD)

		CASE 6%
			!
			! Display descriptions for customer name
			!
			IF FUNC_TESTENTRY(SMG_WINDOW, &
				OE_CREDITJOUR::CUSNUM, &
				AR_35CUSTOM::CUSNAM, &
				"OE", MLOOP, "PROG", &
				"Customer", AR_MAIN_35CUSTOM.ID) = 1%
			THEN
				OE_MAIN_CREDITJOUR = 1%

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

				AR_35CUSTOM::TERMS = &
					STRING$(LEN(AR_35CUSTOM::TERMS), A"?"B)

			ELSE
				OE_CREDITJOUR::SHIPNAM  = AR_35CUSTOM::CUSNAM
				OE_CREDITJOUR::ADD1     = AR_35CUSTOM::ADD1
				OE_CREDITJOUR::ADD2     = AR_35CUSTOM::ADD2
				OE_CREDITJOUR::ADD3     = AR_35CUSTOM::ADD3
				OE_CREDITJOUR::CITY     = AR_35CUSTOM::CITY
				OE_CREDITJOUR::STATE    = AR_35CUSTOM::STATE
				OE_CREDITJOUR::COUNTRY  = AR_35CUSTOM::COUNTRY
				OE_CREDITJOUR::ZIP      = AR_35CUSTOM::ZIP
				OE_CREDITJOUR::SALESMAN = AR_35CUSTOM::SALESMAN

				UTL_TERMS::DESCR = "" &
					IF MAIN_WINDOW(UT_MAIN_TERMS.ID, &
					"Q0" + AR_35CUSTOM::TERMS) <> 1%

			END IF

			SMG_STATUS% = SMG$PUT_CHARS( SMG_WINDOW::WNUMBER, &
				UTL_TERMS::CODE + " " + UTL_TERMS::DESCR, &
				1%, 45%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::CUSNAM, 6%, 37%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD1, 7%, 30%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS( SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD2, 8%, 30%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::CITY + " " + AR_35CUSTOM::STATE + &
				" " + AR_35CUSTOM::ZIP, 9%, 30%, , SMG$M_BOLD)

		CASE 8%
			OE_MAIN_CREDITJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_CREDITJOUR::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"OE", MLOOP, "PROG", &
				"Location", &
				UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 17%, 45%, , SMG$M_BOLD)

		CASE 11%
			IF OE_CREDITJOUR::MISC = 0.0 AND MVALUE = "ADD"
			THEN
				OE_CREDITJOUR::MISCACCT = ""

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					OE_CREDITJOUR::MISCACCT, &
					4%, 25%, , SMG$M_BOLD)

				GL_CHART::DESCR = ""

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					GL_CHART::DESCR, 4%, 45%, , SMG$M_BOLD)

				MLOOP = MLOOP + 1%

			END IF

		CASE 12%
			OE_MAIN_CREDITJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_CREDITJOUR::MISCACCT, &
				GL_CHART::ACCT, &
				"OE", MLOOP, "PROG", &
				"Misc Account", &
				GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 4%, 45%, , SMG$M_BOLD)

		CASE 15%
			IF OE_CREDITJOUR::SALESMAN <> ""
			THEN
				OE_MAIN_CREDITJOUR = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					"S" + OE_CREDITJOUR::SALESMAN, &
					SA_SALESMAN::DESCR, &
					"OE", MLOOP, "PROG", &
					"Salesman", SA_MAIN_SALESMAN.ID)

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					SA_SALESMAN::DESCR, &
					7%, 45%, , SMG$M_BOLD)

			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			OE_ORDERTYPE::DESCRIPTION = &
				STRING$(LEN(OE_ORDERTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(OE_MAIN_ORDERTYPE.ID, &
				"Q0" + OE_CREDITJOUR::ORDTYPE) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_ORDERTYPE::DESCRIPTION, &
				5%, 45%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN

			IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
				"Q0" + OE_CREDITJOUR::CUSNUM) <> 1%
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

				AR_35CUSTOM::TERMS = &
					STRING$(LEN(AR_35CUSTOM::TERMS), A"?"B)
			END IF

			UTL_TERMS::DESCR = "" &
				IF MAIN_WINDOW(UT_MAIN_TERMS.ID, &
				"Q0" + AR_35CUSTOM::TERMS) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_TERMS::CODE + " " + UTL_TERMS::DESCR, &
				1%, 45%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::CUSNAM, 6%, 37%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD1, 7%, 30%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD2, 8%, 30%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::CITY + " " + &
				AR_35CUSTOM::STATE + " " + AR_35CUSTOM::ZIP, &
				9%, 30%, , SMG$M_BOLD)

		END IF

		IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = &
				STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"Q0" + OE_CREDITJOUR::LOCATION) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 17%, 45%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(12%) AND 2%) = 0%
		THEN
			IF OE_CREDITJOUR::MISCACCT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), A"?"B) &
					IF  MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"Q0" + OE_CREDITJOUR::MISCACCT) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 4%, 45%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(15%) AND 2%) = 0%
		THEN
			IF OE_CREDITJOUR::SALESMAN = ""
			THEN
				SA_SALESMAN::DESCR = ""
			ELSE
				SA_SALESMAN::DESCR = &
					STRING$(LEN(SA_SALESMAN::DESCR), A"?"B) &
					IF  MAIN_WINDOW(SA_MAIN_SALESMAN.ID, &
					"Q0S" + OE_CREDITJOUR::SALESMAN) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				SA_SALESMAN::DESCR, 7%, 45%, , SMG$M_BOLD)
		END IF

	!
	! Set OE_CREDITJOUR_OLD value
	!
	CASE OPT_SETOLD
		OE_CREDITJOUR_OLD = OE_CREDITJOUR

	!
	! Restore OE_CREDITJOUR_OLD value
	!
	CASE OPT_RESETOLD
		OE_CREDITJOUR = OE_CREDITJOUR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_CREDITJOUR_DEF = OE_CREDITJOUR

		IF MFLAG = 1%
		THEN
			SELECT MLOOP

			CASE 0%
				FRM$(9%) = "#,###,###.##"
				FRM$(10%) = "#,###,###.##"
				FRM$(11%) = "#,###,###.##"
				FRM$(13%) = "#,###,###.##"
				FRM$(14%) = "##.###"

			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_CREDITJOUR = OE_CREDITJOUR_DEF

		IF MFLAG = 1%
		THEN
			OE_CREDITJOUR::ORDDATE = DATE_TODAY &
				IF OE_CREDITJOUR::ORDDATE = ""

			OE_CREDITJOUR::REASON = ""
		END IF

20800		IF OE_CONTROL.CH% <= 0%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.MOD"
			USE
				CONTINUE ExitFunction IF ERR = 5%
				EXIT HANDLER
			END WHEN
		END IF

20810		IF MFLAG = 1%
		THEN
			WHEN ERROR IN
				GET #OE_CONTROL.CH%, RECORD 1%
				V% = FUNC_INCREMENT(OE_CONTROL::LAST_MEMO)
				UPDATE #OE_CONTROL.CH%
				UNLOCK #OE_CONTROL.CH%
			USE
				CONTINUE ExitFunction IF ERR = 155%
				EXIT HANDLER
			END WHEN

			OE_CREDITJOUR::MEMONUM = OE_CONTROL::LAST_MEMO

			OE_CREDITJOUR::MEMODATE = DATE_TODAY &
				IF TRM$(OE_CREDITJOUR::MEMODATE) = ""
		END IF

	!
	! View the Record.
	!
	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "  Memo #   MemoDate CR OrdrDate ST CusNum     Loc  " + &
				"Operator       Handling     Discount      MiscChg" + &
				"      Freight        SalTax"

		CASE 2%
			MVALUE = "011,020,023,032,035,046,051,062,075,088,101,114"

		CASE 3%
			MVALUE = OE_CREDITJOUR::MEMONUM + " " + &
				PRNT_DATE(OE_CREDITJOUR::MEMODATE, 6%) + " " + &
				OE_CREDITJOUR::REASON + " " + &
				PRNT_DATE(OE_CREDITJOUR::ORDDATE, 6%) + " " + &
				OE_CREDITJOUR::ORDTYPE + " " + &
				OE_CREDITJOUR::CUSNUM + " " + &
				OE_CREDITJOUR::LOCATION + " " + &
				OE_CREDITJOUR::OPERATOR + " " + &
				FORMAT$(OE_CREDITJOUR::HANDLING, &
					"#,###,###.##") + " " + &
				FORMAT$(OE_CREDITJOUR::DISC, &
					"#,###,###.##") + " " + &
				FORMAT$(OE_CREDITJOUR::MISC, &
					"#,###,###.##") + " " + &
				FORMAT$(OE_CREDITJOUR::FREIGHT, &
					"#,###,###.##") + "  " + &
				FORMAT$(OE_CREDITJOUR::SALESTAX, "#,###,###.##")

		END SELECT

	!
	! Find the Order Number.
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #OE_CREDITJOUR.CH%, &
				KEY #0% GE OE_CREDITJOUR::MEMONUM + "", &
				REGARDLESS

		CASE 1%
			FIND #OE_CREDITJOUR.CH%, &
				KEY #1% GE OE_CREDITJOUR::CUSNUM + &
				OE_CREDITJOUR::MEMONUM, REGARDLESS

		CASE 2%
			FIND #OE_CREDITJOUR.CH%, &
				KEY #2% GE OE_CREDITJOUR::REASON + &
				OE_CREDITJOUR::CUSNUM + &
				OE_CREDITJOUR::MEMONUM, REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

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
	!	.lm +5
	!	.b
	!	Accessing the ^*Line\* function maintains
	!	the following information concerning a credit memo.
	!	.table 3,25
	!	.te
	!	Product _#
	!	.te
	!	Quantity Credited
	!	.te
	!	Quantity into Invoice
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
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD006B
	!	.x Ship to Address 1
	!	^*Ship to Address 1\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 1\* field enters
	!	the first line of the address (shipping) for the credit memo.
	!	.b
	!	The field will accommodate 25 characters.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD006C
	!	.x Ship to Address 2
	!	^* Ship to Address 2\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 2\* field enters
	!	the second line of the address (shipping) for the credit memo.
	!	.b
	!	The field will accommodate 25 characters.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD006D
	!	.x Ship to Address 3
	!	^* Ship to Address 3\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 3\* field enters
	!	the third line of the address (shipping) for the credit memo.
	!	.b
	!	The field will accommodate 25 characters.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD006E
	!	.x Customer>City
	!	^*City\*
	!	.b
	!	.lm +5
	!	The ^*City\* field enters the city in which the
	!	Customer is located for the credit memo.
	!	.b
	!	The field will accept up to 15 characters.
	!	.lm -5
	!
	! Index:
	!	.x City>Customer
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD006F
	!	.x Customer>State
	!	^*State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field enters the State in which
	!	the Customer is located.
	!	.b
	!	The field will accommodate a two (2) character State postal
	!	code.
	!	.lm -5
	!
	! Index:
	!	.x State>Customer
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD006G
	!	.x Customer>Zip
	!	^*Zip\*
	!	.b
	!	.lm +5
	!	The ^*Zip\* field enters the zip or postal
	!	code for the area in which a Customer is located.
	!	.b
	!	The field will accept up to 10 characters.
	!	.lm -5
	!
	! Index:
	!	.x Zip>Customer
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD006H
	!	.x Customer>Country
	!	^*Country\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field enters the country if
	!	a Customer is located in a foreign country.
	!	.b
	!	The field will accept a two character code.
	!	.b
	!	Valid Country codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Country>Customer
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD014
	!	.x Freight
	!	^*(14) Freight\*
	!	.b
	!	.lm +5
	!	The ^*Freight\* field enters the freight charges
	!	for the credit memo.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Order>Freight
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD017
	!	.x Notes
	!	^*(17) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* field enters any free formatted notes
	!	relative to the credit memo.
	!	.b
	!	The field will accept up to 80 characters.
	!	.lm -5
	!
	! Index:
	!
	!--
