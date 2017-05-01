1	%TITLE "Order Entry Register"
	%SBTTL "OE_MAIN_REGHEADER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_REGHEADER(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Order Entry Register\* option
	!	maintains the Register Journal.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Order Entry Register
	!	.x Batch Number>User
	!	.x User Batch Number
	!	.x Journal>Entry Maintain
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_REGHEADER/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN OE_MAIN_REGHEADER
	!	$ DELETE OE_MAIN_REGHEADER.OBJ;*
	!
	! Author:
	!
	!	06/19/90 - Lance Williams
	!
	! Modification history:
	!
	!
	!	06/25/91 - Craig Tanner
	!		Added list choises to country field.
	!
	!	08/28/91 - Dan Perkins
	!		Realigned View Screen.
	!
	!	10/08/91 - Frank F. Starman
	!		Add Batch Number.
	!
	!	10/31/91 - Dan Perkins
	!		Add Batch Number to View Screen.
	!		Add F14 keys to salesmen fields.
	!
	!	03/22/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	04/09/92 - Dan Perkins
	!		Rset ORDER NUMBER.  Substitute MLOOP in place of
	!		PRG_PROGRAM in FUNC_TESTENTRY.
	!
	!	05/21/92 - Dan Perkins
	!		Fixed problen in OPT_FIND. Key 4 was looking at
	!		Key 3 instead of Key 4.
	!
	!	06/01/92 - Dan Perkins
	!		Changed program fields to reflect changes in
	!		file layout.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	02/01/93 - Dan Perkins
	!		Added Payment and Notes fields.
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
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
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! Include CDD's
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER
	MAP (OE_REGHEADER_OLD) OE_REGHEADER_CDD OE_REGHEADER_OLD, OE_REGHEADER2
	COM (OE_REGHEADER_PAGE)	OE_REGHEADER_CDD	OE_REGHEADER_PAGE

	%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.HB"
	MAP (OE_SALESTAX)	OE_SALESTAX_CDD		OE_SALESTAX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERTYPE.HB"
	MAP (OE_ORDERTYPE)	OE_ORDERTYPE_CDD	OE_ORDERTYPE

	%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.HB"
	MAP (OE_CATEGORY)	OE_CATEGORY_CDD		OE_CATEGORY

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	!
	! Common Statements
	!
	COM (CH_OE_REGHEADER) &
		OE_REGHEADER.CH%

	COM (TT_OE_MAIN_REGHEADER) &
		STITLE$      = 30%, &
		SSTAT$(6%)   = 30%, &
		TAXTITLE$    = 20%, &
		TAXTYPE$(7%) = 40%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Order Register"
		SMG_WINDOW::NHELP = "OE_MAIN_REGHEADER"
		SMG_WINDOW::HSIZE = 74%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 74%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 24%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::LWIDTH  = 74%
		SMG_WINDOW::LHEIGHT = 15%
		SMG_WINDOW::LHPOS   = 2%
		SMG_WINDOW::LVPOS   = 5%
		SMG_WINDOW::LLAST   = 1%
		SMG_WINDOW::LTITLE(0%) = "First Page"
		SMG_WINDOW::LPAGE(0%)  = 10%
		SMG_WINDOW::LTITLE(1%) = "Last Page"
		SMG_WINDOW::LPAGE(1%)  = 24%

		SMG_WINDOW::NKEYS = 5%
		SMG_WINDOW::KNAME(0%) = "Order_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::KNAME(1%) = "sale_Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		SMG_WINDOW::KNAME(2%) = "sale_Category"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 3%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%

		SMG_WINDOW::KNAME(3%) = "customer_Num"
			SMG_WINDOW::KFIELD(3%, 0%) = 2%
			SMG_WINDOW::KFIELD(3%, 1%) = 7%
			SMG_WINDOW::KFIELD(3%, 2%) = 1%

		SMG_WINDOW::KNAME(4%) = "Batch"
			SMG_WINDOW::KFIELD(4%, 0%) = 2%
			SMG_WINDOW::KFIELD(4%, 1%) = 22%
			SMG_WINDOW::KFIELD(4%, 2%) = 1%


		STITLE$ = "Status   Description"
		SSTAT$(0%) = "2"
		SSTAT$(1%) = "O      Order"
		SSTAT$(2%) = "T      Ticket"
		SSTAT$(3%) = "C      Closed"

		!
		! Tax type
		!
		TAXTITLE$ = "Type    Description"
		TAXTYPE$(0%) = "4"
		TAXTYPE$(1%) = "1     Taxable"
		TAXTYPE$(2%) = "4     Resale"
		TAXTYPE$(3%) = "5     Out of state"
		TAXTYPE$(4%) = "6     Church, School, and Government"

		CALL READ_DEFAULTS(SMG_WINDOW)

20010		GOTO 20040 IF OE_REGHEADER.CH% > 0%

		!
		! Open OE_REGHEADER
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.CRE"
		USE
			MVALUE = "OE_REGHEADER"
			EXIT HANDLER
		END WHEN

20040		SMG_WINDOW::CHAN  = OE_REGHEADER.CH%
		WHEN ERROR IN
			RESET #OE_REGHEADER.CH%
			GET #OE_REGHEADER.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			MVALUE = "OE_REGHEADER"
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
				2,  1, "(02) Sale Type", &
				3,  1, "(03) Sale Cat.", &
				4,  1, "(04) Order Date", &
				5,  1, "(05) Act Status", &
				6,  1, "(06) Status Date", &
				7,  1, "(07) Customer #", &
				11, 1, "(08) Ship to ", &
				17, 1, "(09) Cust Po#", &
				18, 1, "(10) Location", &
				15, 1, "     City/St. ", &
				16, 1, "     ZIP/Cntry ", &
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
		! 1st page
		!
		CASE 1%
			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::LWINDOW)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::LWINDOW)

			DATA	1,  1, "(11) Ship VIA", &
				2,  1, "(12) Terms", &
				3,  1, "(13) Ord Disc %", &
				4,  1, "(14) Tax Code", &
				5,  1, "(15) Tax Flag", &
				6,  1, "(16) Operator", &
				7,  1, "(17) Salesman", &
				8,  1, "(18) Comm %", &
				9,  1, "(19) Pack #", &
				10, 1, "(20) Batch", &
				11, 1, "(21) Amt Paid", &
				13, 1, "(22) Notes", &
				0,  0, ""

			RESTORE
			XPOS = -1%
			READ XPOS, YPOS, XSTR$ UNTIL XPOS = 0%
			READ XPOS, YPOS, XSTR$

			I% = SMG_WINDOW::LPAGE(0%)

			WHILE (XPOS <> 0%)
				I% = I% + 1%

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					XSTR$, XPOS, YPOS) &
					IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%

				READ XPOS, YPOS, XSTR$
			NEXT

			SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::LWINDOW)

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
	!	The ^*Order Number\* field contains the order number which was assigned
	!	or entered at the time the order was originally entered.
	!	.b
	!	Ten spaces are available for the entry.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGHEADER::ORDNUM  = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;18", TEMP$, OE_REGHEADER::ORDNUM, &
				MFLAG OR 2%, "~R 'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Sale Type
	!	^*(02) Sale Type\*
	!	.b
	!	.lm +5
	!	The ^*Sale Type\* field enters a user defined
	!	code which will identify the type of sale to be made.
	!	.b
	!	Valid Sale Type codes may be viewed by pressing List Choices.
	!	Additional Sales types may be entered by pressing ^*F17\*.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGHEADER::ORDTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;18", TEMP$, OE_REGHEADER::ORDTYPE, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(OE_MAIN_ORDERTYPE.ID, "V0") = 1%
				THEN
					OE_REGHEADER::ORDTYPE = &
						OE_ORDERTYPE::ORDTYPE
				END IF
				GOTO Reenter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x Sale Category
	!	^*(03) Sale Category\*
	!	.b
	!	.lm +5
	!	The ^*Sale Category\* field enters the user defined category for
	!	a specific sale.
	!	.b
	!	Valid Category codes may be viewed by pressing ^*List Choices\*.
	!	Additional Category codes may be entered by pressing ^*F17\*.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGHEADER::ORDCAT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;18", TEMP$, OE_REGHEADER::ORDCAT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(OE_MAIN_CATEGORY.ID, "V0") = 1%
				THEN
					OE_REGHEADER::ORDCAT = &
						OE_CATEGORY::ORDCAT
				END IF
				GOTO Reenter
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Date
	!	^*(04) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field enters the date the order was produced.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--
			IF (TEMP$ = "Add") AND (OE_REGHEADER::ORDDATE = "")
			THEN
				OE_REGHEADER::ORDDATE = DATE_TODAY
			END IF

			OE_REGHEADER::ORDDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;18", TEMP$, OE_REGHEADER::ORDDATE, MFLAG, &
				"'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Activity Status
	!	^*(05) Activity Status\*
	!	.b
	!	.lm +5
	!	The ^*Activity Status\* field contains one of the
	!	following valid codes:
	!	.table 3,25
	!	.te
	!	^*O\* - Open
	!	.te
	!	^*T\* - Ticket
	!	.te
	!	^*C\* - Closed
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGHEADER::ASTATUS = &
				EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;18", TEMP$, OE_REGHEADER::ASTATUS, &
				MFLAG, "!", MVALUE, &
				SSTAT$(), STITLE$, "008"), -1%)

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x Status Date
	!	^*(06) Status Date\*
	!	.b
	!	.lm +5
	!	The ^*Status Date\* field contains the date of the current
	!	order.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGHEADER::SDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;18", TEMP$, OE_REGHEADER::SDATE, MFLAG, &
				"'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	.x Customer
	!	^*(07) Customer\*
	!	.b
	!	.lm +5
	!	The ^*Customer\* field contains the identification number referencing a
	!	particular customer.
	!	.b
	!	Valid Customer codes may be viewed by pressing ^*List Choices\*.
	!	Additional Customer codes may be entered by pressing ^*F17\*.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGHEADER::CUSNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;18", TEMP$, OE_REGHEADER::CUSNUM, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "VX") = 1%
				THEN
					OE_REGHEADER::CUSNUM = &
						AR_35CUSTOM::CUSNUM
				END IF
				GOTO Reenter
			END IF

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Ship-to Name\*
	!	.b
	!	.lm +5
	!	The ^*Ship-to Name\* field contains the name and address of the
	!	person or institution where the order is to be shipped.
	!	.lm -5
	!
	! Index:
	!	.x Ship-to Name
	!
	!--
			SCOPE::PRG_ITEM = "FLD008"
			IF (TEMP$ = "Add") AND (OE_REGHEADER::SHIPNAM = "")
			THEN
				OE_REGHEADER::SHIPNAM = AR_35CUSTOM::CUSNAM
			END IF

			OE_REGHEADER::SHIPNAM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;18", TEMP$, OE_REGHEADER::SHIPNAM, MFLAG, &
				"'E", MVALUE)

	!++
	! Abstract:FLD008A
	!	^*Ship to Address 1\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 1\* field enters
	!	the first line of the address for shipping the order.
	!	.lm -5
	!
	! Index:
	!	.x Ship to Address 1
	!
	!--
			SCOPE::PRG_ITEM = "FLD008A"

			IF (TEMP$ = "Add") AND (OE_REGHEADER::ADD1 = "")
			THEN
				OE_REGHEADER::ADD1 = AR_35CUSTOM::ADD1
			END IF

			OE_REGHEADER::ADD1 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;18", TEMP$, OE_REGHEADER::ADD1, MFLAG, &
				"'E", MVALUE)

	!++
	! Abstract:FLD008B
	!	^* Ship to Address 2\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 2\* field enters
	!	the second line of the address for shipping the order.
	!	.lm -5
	!
	! Index:
	!	.x Ship to Address 2
	!
	!--
			SCOPE::PRG_ITEM = "FLD008B"

			IF (TEMP$ = "Add") AND (LEFT$(AR_35CUSTOM::ADD2, 1%) &
				<> "?") AND (OE_REGHEADER::ADD2 = "")
			THEN
				OE_REGHEADER::ADD2 = AR_35CUSTOM::ADD2
			END IF

			OE_REGHEADER::ADD2 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"13;18", TEMP$, OE_REGHEADER::ADD2, MFLAG, &
				"'E", MVALUE)

	!++
	! Abstract:FLD008C
	!	^* Ship to Address 3 \*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 3\* field enters
	!	the third line of the address for shipping the order.
	!	.lm -5
	!
	! Index:
	!	.x Ship to Address 3
	!
	!--
			SCOPE::PRG_ITEM = "FLD008C"

			OE_REGHEADER::ADD3 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"14;18", TEMP$, OE_REGHEADER::ADD3, MFLAG, &
				"'E", MVALUE)

	!++
	! Abstract:FLD008D
	!	.x Ship to>City
	!	^*City\*
	!	.b
	!	.lm +5
	!	The ^*City\* field enters the City where the
	!	order is to be shipped.
	!	.b
	!	Fifteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x City>Ship to
	!
	!--
			SCOPE::PRG_ITEM = "FLD008D"

			IF (TEMP$ = "Add") AND (OE_REGHEADER::CITY = "")
			THEN
				OE_REGHEADER::CITY = AR_35CUSTOM::CITY
			END IF

			OE_REGHEADER::CITY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"15;18", TEMP$, OE_REGHEADER::CITY, MFLAG, &
				"'E", MVALUE)

	!++
	! Abstract:FLD008E
	!	.x Ship to>State
	!	^*State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field contains the code for the state to which the order
	!	is to be shipped.
	!	.b
	!	Valid State codes (2 Character) are defined by the U.S. Postal Service.
	!	.lm -5
	!
	! Index:
	!	.x State>Ship to
	!
	!--
			SCOPE::PRG_ITEM = "FLD008E"

			IF (TEMP$ = "Add") AND (OE_REGHEADER::STATE = "")
			THEN
				OE_REGHEADER::STATE = AR_35CUSTOM::STATE
			END IF

			OE_REGHEADER::STATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"15;34", TEMP$, OE_REGHEADER::STATE, MFLAG, &
				"'E", MVALUE)

	!++
	! Abstract:FLD008F
	!	.x Ship to>Zip
	!	^*Zip\*
	!	.b
	!	.lm +5
	!	The ^*Zip\* field contains the zip or postal code of the address to
	!	which a specific order is to be shipped.
	!	.b
	!	The field will accommodate ten characters.
	!	.lm -5
	!
	! Index:
	!	.x Zip>Ship to
	!
	!--
			SCOPE::PRG_ITEM = "FLD008F"

			IF (TEMP$ = "Add") AND (OE_REGHEADER::ZIP = "")
			THEN
				OE_REGHEADER::ZIP = AR_35CUSTOM::ZIP
			END IF

			OE_REGHEADER::ZIP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"16;18", TEMP$, OE_REGHEADER::ZIP, MFLAG, &
				"'E", MVALUE)

	!++
	! Abstract:FLD008G
	!	.x Ship to>Country
	!	^*Country\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field contains the standard ASCII code for a
	!	country to which a particular order is to be shipped. This field is to
	!	be left blank unless the shipment is to be made to a
	!	foreign country.
	!	.b
	!	Valid Country codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	The field will accommodate two characters.
	!	.lm -5
	!
	! Index:
	!	.x Country>Ship to
	!
	!--
			SCOPE::PRG_ITEM = "FLD008G"

			IF (TEMP$ = "Add") AND (OE_REGHEADER::COUNTRY = "")
			THEN
				OE_REGHEADER::COUNTRY = AR_35CUSTOM::COUNTRY
			END IF

			OE_REGHEADER::COUNTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"16;29", TEMP$, OE_REGHEADER::COUNTRY, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "VX" + &
					OE_REGHEADER::COUNTRY) = 1%
				THEN
					OE_REGHEADER::COUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO Reenter
			END IF

		CASE 9%
	!++
	! Abstract:FLD009
	!	.x Customer PO.
	!	^*(09) Customer PO.\*
	!	.b
	!	.lm +5
	!	The ^*Customer PO.\* field contains the customer purchase order number.
	!	.b
	!	The field will accept up to ten characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGHEADER::CUSTPO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"17;18", TEMP$, OE_REGHEADER::CUSTPO, MFLAG, &
				"'E", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	.x Location Number
	!	^*(10) Location Number\*
	!	.b
	!	.lm +5
	!	The ^*Location Number\* field contains a user defined code to identify
	!	a company location.
	!	.b
	!	Valid locations may be viewed by pressing ^*List Choices\*.
	!	Additional location codes may be entered by pressing ^*F17\*.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			IF (TEMP$ = "Add") AND (OE_REGHEADER::LOCATION = "")
			THEN
				OE_REGHEADER::LOCATION = AR_35CUSTOM::LOCATION
			END IF

			OE_REGHEADER::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"18;18", TEMP$, OE_REGHEADER::LOCATION, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%
				THEN
					OE_REGHEADER::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 11%
	!++
	! Abstract:FLD011
	!	.x Ship VIA
	!	^*(11) Ship VIA\*
	!	.b
	!	.lm +5
	!	The ^*Ship VIA\* field contains a user defined number identifying a
	!	particular carrier or method of shipment for a particular order.
	!	.b
	!	Valid ship VIA codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	The field will accept two characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			IF (TEMP$ = "Add") AND (OE_REGHEADER::SHIPVIA = "")
			THEN
				OE_REGHEADER::SHIPVIA = AR_35CUSTOM::CARRIER
			END IF

			OE_REGHEADER::SHIPVIA = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, &
				"1;18", TEMP$, &
				OE_REGHEADER::SHIPVIA, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, "VX") = 1%
				THEN
					OE_REGHEADER::SHIPVIA = &
						UTL_CARRIER::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 12%
	!++
	! Abstract:FLD012
	!	.x Terms Code
	!	^*(12) Terms Code\*
	!	.b
	!	.lm +5
	!	The ^*Terms Code\* field contains a user defined number which identifies
	!	specific terms.
	!	.b
	!	Valid terms codes may be viewed by pressing ^*List Choices\*.
	!	Additional terms codes may be entered by pressing ^*F17\*.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			IF (TEMP$ = "Add") AND (OE_REGHEADER::TERMS = "")
			THEN
				OE_REGHEADER::TERMS = AR_35CUSTOM::TERMS
			END IF

			OE_REGHEADER::TERMS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, &
				"2;18", TEMP$, &
				OE_REGHEADER::TERMS, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UT_MAIN_TERMS.ID, "VX") = 1%
				THEN
					OE_REGHEADER::TERMS = &
						UTL_TERMS::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 13%
	!++
	! Abstract:FLD013
	!	.x Amount Paid
	!	^*(21) Amount Paid\*
	!	.b
	!	.lm +5
	!	The ^*Amount Paid\* field contains the amount of a payment of
	!	any type.
	!	.lm -5
	!
	! Index:
	!
	!--
		OE_REGHEADER::DISC  = ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
			"3;18", TEMP$, &
			OE_REGHEADER::DISC, MFLAG, "##.##", MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	.ts 55
	!	^*(14) Tax Code\*
	!	.lm +5
	!	.b
	!	The ^*Tax Code\* field contains the sales tax code for a specific
	!	order.
	!	.lm -5
	!
	! Index:
	!	.x Tax Code
	!
	!--
			IF (TEMP$ = "Add") AND (OE_REGHEADER::TAXCODE = "")
			THEN
				OE_REGHEADER::TAXCODE = AR_35CUSTOM::TAXCODE
			END IF

			OE_REGHEADER::TAXCODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "4;18", TEMP$, &
				OE_REGHEADER::TAXCODE, MFLAG, "'E", MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	.x Tax Flag
	!	^*(15) Tax Flag\*
	!	.b
	!	.lm +5
	!	The ^*Tax Flag\* field contains the appropriate sales tax flag related to
	!	the order.
	!	.b
	!	Valid sales tax flags are:
	!	.table 3,25
	!	^*1\* - Taxable
	!	.te
	!	^*4\* - Resale
	!	.te
	!	^*5\* - Out of State
	!	.te
	!	^*6\* - Church, School, and Government
	!	.end table
	!	Valid Tax Flags may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGHEADER::TAXFLAG = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::LWINDOW, "5;18", TEMP$, &
				OE_REGHEADER::TAXFLAG, MFLAG, "!", MVALUE, &
				TAXTYPE$(), TAXTITLE$, "008"), -1%)

		CASE 16%
	!++
	! Abstract:FLD016
	!	.x Order Entry>Operator
	!	^*(16) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters the name or initials of the
	!	operator entering the order.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Operator
	!
	!--
			OE_REGHEADER::OPERATOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "6;18", TEMP$, &
				OE_REGHEADER::OPERATOR, MFLAG, &
				"'E", MVALUE)

		CASE 17%
	!++
	! Abstract:FLD017
	!	.x Salesmen
	!	^*(17) Salesmen\*
	!	.b
	!	.lm +5
	!	The ^*Salesmen 1\* field contains the code representing the salesman or
	!	broker to which commissions will be credited as related to the specific order.
	!	.b
	!	Valid Salesman codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			IF (TEMP$ = "Add") AND (OE_REGHEADER::SALESMAN = "")
			THEN
				OE_REGHEADER::SALESMAN= AR_35CUSTOM::SALESMAN
			END IF

			OE_REGHEADER::SALESMAN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "7;18", TEMP$, &
				OE_REGHEADER::SALESMAN, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "VX") = 1%
				THEN
					OE_REGHEADER::SALESMAN = &
						SA_SALESMAN::SALESMAN
				END IF
				GOTO Reenter
			END IF

		CASE 18%
	!++
	! Abstract:FLD018
	!	.x Salesmen Commission 1
	!	^*(18) Salesmen Commission\*
	!	.b
	!	.lm +5
	!	The ^*Salesmen Commission 1\* field contains the commission percentage
	!	as relates to a specific order which will be credited to a salesman or broker.
	!	.b
	!	Example:  If the commission % is to be 10%, the entry would be made as 10.00.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGHEADER::SALCOMM = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::LWINDOW, "8;18", TEMP$, &
				OE_REGHEADER::SALCOMM, MFLAG, "##.##", MVALUE)

		CASE 19%
	!++
	! Abstract:FLD019
	!	.ts 55
	!	^*(19) Pack _#\*
	!	.b
	!	.lm +5
	!	The ^*Pack _#\* field contains the shipping number of the package
	!	or packages related to a specific order.
	!	.lm -5
	!
	! Index:
	!	.x Pack _#
	!
	!--
			OE_REGHEADER::SHIPNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "9;18", TEMP$, &
				OE_REGHEADER::SHIPNO, MFLAG, "'E", MVALUE)

		CASE 20%
	!++
	! Abstract:FLD020
	!	^*(20) Batch\*
	!	.b
	!	.lm +5
	!	The ^*Batch\* field is system generated.
	!	.b
	!	Entry in this field is not required.
	!	.lm -5
	!
	! Index:
	!	.x Batch
	!
	!--
			OE_REGHEADER::BATCH = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, "10;18", TEMP$, &
				OE_REGHEADER::BATCH, MFLAG, "'E", MVALUE)

		CASE 21%
	!++
	! Abstract:FLD013
	!	.x Amount Paid
	!	^*(21) Amount Paid\*
	!	.b
	!	.lm +5
	!	The ^*Amount Paid\* field contains the amount of a payment of
	!	any type.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_REGHEADER::AMTPAID = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::LWINDOW, "11;18", TEMP$, &
				OE_REGHEADER::AMTPAID, &
				MFLAG, "###,###.##", MVALUE)

		CASE 22%
	!++
	! Abstract:FLD022
	!	.x Notes
	!	^*(22) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* field enters any free formatted notes
	!	relative to the record.
	!	.b
	!	Forty spaces are available for entry.
	!	.lm -5
	!
	! Index:
	!
	!--
 FirstNote:
			OE_REGHEADER::NOTES(0%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"13;18", TEMP$, OE_REGHEADER::NOTES(0%), &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_UP
				GOTO BypassNotes
			END SELECT

			GOTO BypassNotes IF OE_REGHEADER::NOTES(0%) = "" &
				AND OE_REGHEADER::NOTES(1%) = "" &
				AND OE_REGHEADER::NOTES(2%) = ""

 SecondNote:
			OE_REGHEADER::NOTES(1%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"14;18", TEMP$, OE_REGHEADER::NOTES(1%), &
				MFLAG, "'E", MVALUE)

			GOTO BypassNotes IF OE_REGHEADER::NOTES(1%) = "" &
				AND OE_REGHEADER::NOTES(2%) = ""

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_UP
				GOTO FirstNote
			END SELECT

			OE_REGHEADER::NOTES(2%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"15;18", TEMP$, OE_REGHEADER::NOTES(2%), &
				MFLAG, "'E", MVALUE)

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

		OE_MAIN_REGHEADER = 0%

		SELECT MLOOP

		CASE 1%

			!
			! Fix OE_REGHEADER::ORDNUM
			!
			IF OE_REGHEADER::ORDNUM = ""
			THEN
				OE_MAIN_REGHEADER = 1%
			END IF

		CASE 2%
			!
			! Display the descriptions for order type
			!
			OE_MAIN_REGHEADER = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_REGHEADER::ORDTYPE, &
				OE_ORDERTYPE::DESCRIPTION, &
				"OE", MLOOP, "PROG", &
				"Order Type", OE_MAIN_ORDERTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_ORDERTYPE::DESCRIPTION, &
				2%, 24%, , SMG$M_BOLD)

		CASE 3%
			!
			! Display the descriptions for category
			!
			IF OE_REGHEADER::ORDCAT <> ""
			THEN
				OE_MAIN_REGHEADER = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_REGHEADER::ORDCAT, &
					OE_CATEGORY::DESCRIPTION, &
					"OE", MLOOP, "PROG", &
					"Order Category",OE_MAIN_CATEGORY.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					OE_CATEGORY::DESCRIPTION, &
					3%, 24%, , SMG$M_BOLD)
			END IF

		CASE 4%
			IF OE_REGHEADER::ORDDATE = ""
			THEN
				OE_MAIN_REGHEADER = 1%
			ELSE
				!
				! Get the next record
				!
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #OE_REGHEADER.CH%, &
							KEY #0% EQ OE_REGHEADER::ORDNUM + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					OE_MAIN_REGHEADER = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)

				END IF

			END IF

		CASE 7%
			!
			! Display the descriptions for customer name
			!
			ST%, OE_MAIN_REGHEADER = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_REGHEADER::CUSNUM, &
				AR_35CUSTOM::CUSNAM, &
				"OE", MLOOP, "PROG", &
				"Customer", AR_MAIN_35CUSTOM.ID)

			IF ST% <> 0%
			THEN
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
				AR_35CUSTOM::CUSNAM, &
				7%, 29%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD1, &
				8%, 29%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD2, &
				9%, 29%, , SMG$M_BOLD)

			TEXT$ = AR_35CUSTOM::CITY + " " + AR_35CUSTOM::STATE + &
				" " + AR_35CUSTOM::ZIP

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TEXT$, &
				10%, 29%, , SMG$M_BOLD)

		CASE 8%
			!
			! Display the address
			!
			IF OE_REGHEADER::CUSNUM = ""
			THEN
				OE_REGHEADER::SHIPNAM = ""
				OE_REGHEADER::ADD1 = ""
				OE_REGHEADER::ADD2 = ""
				OE_REGHEADER::ADD3 = ""
				OE_REGHEADER::CITY = ""
				OE_REGHEADER::STATE = ""
				OE_REGHEADER::ZIP = ""
				OE_REGHEADER::COUNTRY = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_REGHEADER::SHIPNAM, &
				11%, 18%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_REGHEADER::ADD1, &
				12%, 18%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_REGHEADER::ADD2, &
				13%, 18%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_REGHEADER::ADD3, &
				14%, 18%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_REGHEADER::CITY, &
				15%, 18%, , SMG$M_BOLD)
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_REGHEADER::STATE, &
				15%, 34%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_REGHEADER::ZIP, &
				16%, 18%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_REGHEADER::COUNTRY, &
				16%, 29%, , SMG$M_BOLD)

		CASE 10%
			!
			! Display the descriptions for location name
			!
			OE_MAIN_REGHEADER = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_REGHEADER::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"OE", MLOOP, "PROG", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, &
				18%, 35%, , SMG$M_BOLD)

		CASE 11%
			!
			! Display the descriptions for carrier
			!
			IF OE_REGHEADER::SHIPVIA = ""
			THEN
				UTL_CARRIER::DESCR = ""
			ELSE
				OE_MAIN_REGHEADER = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_REGHEADER::SHIPVIA, &
					UTL_CARRIER::DESCR, &
					"OE", MLOOP, "PROG", &
					"Carrier", UT_MAIN_CARRIER.ID)
			END IF

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					UTL_CARRIER::DESCR, &
					1%, 28%, , SMG$M_BOLD)
		CASE 12%
			!
			! Display the descriptions for terms
			!
			IF OE_REGHEADER::TERMS = ""
			THEN
				UTL_TERMS::DESCR = ""
			ELSE
				OE_MAIN_REGHEADER = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_REGHEADER::TERMS, &
					UTL_TERMS::DESCR, &
					"OE", MLOOP, "PROG", &
					"Terms", UT_MAIN_TERMS.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				UTL_TERMS::DESCR, &
				2%, 28%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		!
		! Display the descriptions
		!
		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			OE_ORDERTYPE::DESCRIPTION = &
				STRING$(LEN(OE_ORDERTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(OE_MAIN_ORDERTYPE.ID, &
				"Q0" + OE_REGHEADER::ORDTYPE) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(OE_ORDERTYPE::DESCRIPTION, 33%), &
				2%, 24%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			OE_CATEGORY::DESCRIPTION = &
				STRING$(LEN(OE_CATEGORY::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(OE_MAIN_CATEGORY.ID, &
				"Q0" + OE_REGHEADER::ORDCAT) <> 1%

			OE_CATEGORY::DESCRIPTION = "" &
				IF OE_REGHEADER::ORDCAT = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(OE_CATEGORY::DESCRIPTION, 33%), &
				3%, 24%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			AR_35CUSTOM::CUSNAM = &
				STRING$(LEN(AR_35CUSTOM::CUSNAM), A"?"B) &
				IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
				"Q0" + OE_REGHEADER::CUSNUM) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::CUSNAM,7%,29%, , SMG$M_BOLD)

			AR_35CUSTOM::ADD1 = &
				STRING$(LEN(AR_35CUSTOM::ADD1), A"?"B) &
				IF OE_REGHEADER::CUSNUM = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD1, &
				8%, 29%, , SMG$M_BOLD)

			AR_35CUSTOM::ADD2 = &
				STRING$(LEN(AR_35CUSTOM::ADD2), A"?"B) &
				IF OE_REGHEADER::CUSNUM = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD2, &
				9%, 29%, , SMG$M_BOLD)

			TEXT$ = AR_35CUSTOM::CITY + " " + AR_35CUSTOM::STATE + " " + AR_35CUSTOM::ZIP

			TEXT$ = &
				STRING$(LEN(TEXT$), A"?"B) &
				IF OE_REGHEADER::CUSNUM = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TEXT$, &
				10%, 29%, , SMG$M_BOLD)
		END IF

		IF OE_REGHEADER::CUSNUM = ""
		THEN
			OE_REGHEADER::SHIPNAM = ""
			OE_REGHEADER::ADD1 = ""
			OE_REGHEADER::ADD2 = ""
			OE_REGHEADER::ADD3 = ""
			OE_REGHEADER::CITY = ""
			OE_REGHEADER::STATE = ""
			OE_REGHEADER::ZIP = ""
			OE_REGHEADER::COUNTRY = ""
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::SHIPNAM, &
			11%, 18%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::ADD1, &
			12%, 18%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::ADD2, &
			13%, 18%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::ADD3, &
			14%, 18%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::CITY, &
			15%, 18%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::STATE, &
			15%, 34%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::ZIP, &
			16%, 18%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::COUNTRY, &
			16%, 29%, , SMG$M_BOLD)

		UTL_CARRIER::DESCR = &
			STRING$(LEN(UTL_CARRIER::DESCR), A"?"B) &
			IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, &
			"Q0" + OE_REGHEADER::SHIPVIA) <> 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
			UTL_CARRIER::DESCR, &
			1%, 28%, , SMG$M_BOLD)

		UTL_TERMS::DESCR = &
			STRING$(LEN(UTL_TERMS::DESCR), A"?"B) &
				IF MAIN_WINDOW(UT_MAIN_TERMS.ID, &
				"Q0" + OE_REGHEADER::TERMS) <> 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
			UTL_TERMS::DESCR, &
			2%, 28%, , SMG$M_BOLD)

		UTL_LOCATION::LOCNAME = &
			STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
			IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
			"Q0" + OE_REGHEADER::LOCATION) <> 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			UTL_LOCATION::LOCNAME, &
			18%, 35%, , SMG$M_BOLD)

	!
	! Set OE_REGHEADER_OLD value
	!
20500	CASE OPT_SETOLD
		OE_REGHEADER_OLD = OE_REGHEADER

	!
	! Restore OE_REGHEADER_OLD value
	!
	CASE OPT_RESETOLD
		OE_REGHEADER = OE_REGHEADER_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_REGHEADER2 = OE_REGHEADER

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_REGHEADER = OE_REGHEADER2

		OE_REGHEADER::COMMAMT = 0.0

	!
	! View the Record.
	!
	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "  Order Num  Order Date Type Category CusNumber   CustPo     Batch"

		CASE 2%
			MVALUE = "013,024,029,038,050,061"

		CASE 3%
			MVALUE = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + " "     + &
				PRNT_DATE(OE_REGHEADER::ORDDATE, 8%) + " " + &
				OE_REGHEADER::ORDTYPE + "   " + &
				OE_REGHEADER::ORDCAT + "     " + &
				OE_REGHEADER::CUSNUM + "  " + &
				OE_REGHEADER::CUSTPO + " " + &
				OE_REGHEADER::BATCH

		END SELECT

	!
	! Find the Order Number.
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #OE_REGHEADER.CH%, &
				KEY #0% GE OE_REGHEADER::ORDNUM + "", &
				REGARDLESS

		CASE 1%
			FIND #OE_REGHEADER.CH%, &
				KEY #1% GE OE_REGHEADER::ORDTYPE + &
				OE_REGHEADER::ORDNUM, REGARDLESS

		CASE 2%
			FIND #OE_REGHEADER.CH%, &
				KEY #2% GE OE_REGHEADER::ORDCAT + &
				OE_REGHEADER::ORDNUM, REGARDLESS

		CASE 3%
			FIND #OE_REGHEADER.CH%, &
				KEY #3% GE OE_REGHEADER::CUSNUM + &
				OE_REGHEADER::ORDNUM, REGARDLESS

		CASE 4%
			FIND #OE_REGHEADER.CH%, &
				KEY #4% GE OE_REGHEADER::BATCH + &
				OE_REGHEADER::ORDNUM, REGARDLESS

		END SELECT
28900
	END SELECT

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
	! Abstract:FLD008ADD1
	!	^*Ship to Address 1\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 1\* field enters
	!	the first line of the address for shipping the order.
	!	.lm -5
	!
	! Index:
	!	.x Ship to Address 1
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD008ADD2
	!	^* Ship to Address 2\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 2\* field enters
	!	the second line of the address for shipping the order.
	!	.lm -5
	!
	! Index:
	!	.x Ship to Address 2
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD008ADD3
	!	^* Ship to Address 3\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 3\* field enters
	!	the third line of the address for shipping the order.
	!	.lm -5
	!
	! Index:
	!	.x Ship to Address 3
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD008CITY
	!	.x Customer>City
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
	!+-+-+
	!++
	! Abstract:FLD008COUNTRY
	!	.x Customer>Country
	!	^*Country\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field enters the country if
	!	a Customer is located in a foreign country.
	!	.b
	!	Valid Country codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Country>Customer
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD008NAME
	!	^*(08) Ship Name\*
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
	!+-+-+
	!++
	! Abstract:FLD008STATE
	!	.x Customer>State
	!	^*State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field enters the State in which
	!	the Customer is located.
	!	.b
	!	Valid State codes (2 character) are provided by the U.S. Postal Service.
	!	.lm -5
	!
	! Index:
	!	.x State>Customer
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD008ZIP
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
