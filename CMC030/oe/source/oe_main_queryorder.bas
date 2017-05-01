1	%TITLE "Order Entry Query"
	%SBTTL "OE_MAIN_QUERYORDER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_QUERYORDER(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Order Entry Query\* option
	!	views on-line order register records.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_QUERYORDER/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN OE_MAIN_QUERYORDER
	!	$ DELETE OE_MAIN_QUERYORDER.OBJ;*
	!
	! Author:
	!
	!	01/07/91 - Val James Allen
	!
	! Modification history:
	!
	!	08/26/91 - Dan Perkins
	!		Modified view screen.
	!
	!	03/20/92 - Dan Perkins
	!		Removed OE_REGHEADER_PAGE which seemed to
	!		do nothing from map statement.
	!
	!	06/02/92 - Dan Perkins
	!		Changed fields to accomodate changes in Regheader
	!		file layout.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/12/95 - Kevin Handy
	!		Lose OE_ORDERLINE definitions, since they are never
	!		used.
	!
	!	01/27/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in several places.
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
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! Include CDD's
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER
	MAP (OE_QUERYORDER_DEF)	OE_REGHEADER_CDD	OE_REGHEADER_OLD, OE_REGHEADER2

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

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	DECLARE		OE_REGLINE_CDD	OE_REGLINE_READ

	!
	! This common area must be mapped in both the main program and
	! in OE_MAIN_ORDERLINE.
	!
	COM (CH_OE_REGHEADER) &
		BATCH_NO$ = 2%, &
		OE_REGHEADER.CH%

	COM (TT_OE_MAIN_REGHEADER) &
		STITLE$ = 30%, &
		SSTAT$(6%) = 30%, &
		TAXTITLE$ = 20%, &
		TAXTYPE$(7%) = 40%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION OE_READ_REGLINE

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
		SMG_WINDOW::DESCR = "Examine Order Register Journal"
		SMG_WINDOW::NHELP = "OE_MAIN_QUERYORDER"
		SMG_WINDOW::CHAN  = OE_REGHEADER.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 19%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::LWIDTH  = 78%
		SMG_WINDOW::LHEIGHT = 15%
		SMG_WINDOW::LHPOS   = 2%
		SMG_WINDOW::LVPOS   = 5%
		SMG_WINDOW::LLAST   = 1%
		SMG_WINDOW::LTITLE(0%) = "First Page"
		SMG_WINDOW::LPAGE(0%) = 10%
		SMG_WINDOW::LTITLE(1%) = "Last Page"
		SMG_WINDOW::LPAGE(1%) = 19%

		SMG_WINDOW::NKEYS = 4%
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


		STITLE$ = "Status   Description"
		SSTAT$(0%) = "2"
		SSTAT$(1%) = "O      Open"
		SSTAT$(2%) = "C      Closed"

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

			DATA	1,  1, "Order #", &
				2,  1, "Sale Type", &
				3,  1, "Sale Cat.", &
				4,  1, "Order Date", &
				5,  1, "Act Status", &
				6,  1, "Status Date", &
				7,  1, "Customer #", &
				11,  1, "Ship to ", &
				15,  1, "City/St. ", &
				16,  1, "ZIP/Cntry ", &
				17,  1, "Cust Po#", &
				18,  1, "Location", &
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

			DATA	1,  1, "Ship VIA", &
				2,  1, "Terms", &
				3,  1, "Ord Disc %", &
				4,  1, "Tax Code", &
				5,  1, "Tax Flag", &
				6,  1, "Salesman", &
				7,  1, "Comm %", &
				8,  1, "Operator", &
				9,  1, "Pack #", &
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
			OE_REGHEADER::ORDNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;17", TEMP$, &
				OE_REGHEADER::ORDNUM, MFLAG OR 2%, &
				"~R 'E", MVALUE)
		CASE 2%
			OE_REGHEADER::ORDTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;17", TEMP$, &
				OE_REGHEADER::ORDTYPE, MFLAG, &
				"'E", MVALUE)

		CASE 3%
			OE_REGHEADER::ORDCAT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;17", TEMP$, &
				OE_REGHEADER::ORDCAT, MFLAG, &
				"'E", MVALUE)

		CASE 4%
			OE_REGHEADER::ORDDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;17", TEMP$, OE_REGHEADER::ORDDATE, MFLAG, &
				"'E", MVALUE)

		CASE 5%
			OE_REGHEADER::ASTATUS = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;17", TEMP$, OE_REGHEADER::ASTATUS, &
				MFLAG, "!", MVALUE, &
				SSTAT$(), STITLE$, "008"), -1%)

		CASE 6%
			OE_REGHEADER::SDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;17", TEMP$, OE_REGHEADER::SDATE, MFLAG, &
				"'E", MVALUE)

		CASE 7%
			OE_REGHEADER::CUSNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;17", TEMP$, &
				OE_REGHEADER::CUSNUM, MFLAG, &
				"'E", MVALUE)

		CASE 8%
			OE_REGHEADER::SHIPNAM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;17", TEMP$, &
				OE_REGHEADER::SHIPNAM, MFLAG, &
				"'E", MVALUE)

			OE_REGHEADER::ADD1 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;17", TEMP$, &
				OE_REGHEADER::ADD1, MFLAG, &
				"'E", MVALUE)

			OE_REGHEADER::ADD2 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"13;17", TEMP$, &
				OE_REGHEADER::ADD2, MFLAG, &
				"'E", MVALUE)

			OE_REGHEADER::ADD3 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"14;17", TEMP$, &
				OE_REGHEADER::ADD3, MFLAG, &
				"'E", MVALUE)

			OE_REGHEADER::CITY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"15;17", TEMP$, &
				OE_REGHEADER::CITY, MFLAG, &
				"'E", MVALUE)

			OE_REGHEADER::STATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"15;33", TEMP$, &
				OE_REGHEADER::STATE, MFLAG, &
				"'E", MVALUE)

			OE_REGHEADER::ZIP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"16;17", TEMP$, &
				OE_REGHEADER::ZIP, MFLAG, &
				"'E", MVALUE)

			OE_REGHEADER::COUNTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"16;28", TEMP$, &
				OE_REGHEADER::COUNTRY, MFLAG, &
				"'E", MVALUE)

		CASE 9%
			OE_REGHEADER::CUSTPO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"17;17", TEMP$, &
				OE_REGHEADER::CUSTPO, MFLAG, &
				"'E", MVALUE)

		CASE 10%
			OE_REGHEADER::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"18;17", TEMP$, &
				OE_REGHEADER::LOCATION, MFLAG, &
				"'E", MVALUE)

		CASE 11%
			OE_REGHEADER::SHIPVIA = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, &
				"1;18", TEMP$, &
				OE_REGHEADER::SHIPVIA, MFLAG, &
				"'E", MVALUE)

		CASE 12%
			OE_REGHEADER::TERMS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, &
				"2;18", TEMP$, &
				OE_REGHEADER::TERMS, MFLAG, &
				"'E", MVALUE)

		CASE 13%
			OE_REGHEADER::DISC  = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::LWINDOW, &
				"3;18", TEMP$, &
				OE_REGHEADER::DISC, MFLAG, "##.##", MVALUE)

		CASE 14%
			OE_REGHEADER::TAXCODE= ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, &
				"4;18", TEMP$, &
				OE_REGHEADER::TAXCODE, MFLAG, "'E", MVALUE)

		CASE 15%
			OE_REGHEADER::TAXFLAG = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::LWINDOW, &
				"5;18", TEMP$, &
				OE_REGHEADER::TAXFLAG, MFLAG, "!", MVALUE, &
				TAXTYPE$(), TAXTITLE$, "008"), -1%)

		CASE 16%
			OE_REGHEADER::SALESMAN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, &
				"6;18", TEMP$, &
				OE_REGHEADER::SALESMAN, MFLAG, &
				"'E", MVALUE)

		CASE 17%
			OE_REGHEADER::SALCOMM  = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::LWINDOW, &
				"7;18", TEMP$, &
				OE_REGHEADER::SALCOMM, MFLAG, "##.##", MVALUE)

		CASE 18%
			OE_REGHEADER::OPERATOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, &
				"8;18", TEMP$, &
				OE_REGHEADER::OPERATOR, MFLAG, &
				"'E", MVALUE)

		CASE 19%
			OE_REGHEADER::SHIPNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::LWINDOW, &
				"9;18", TEMP$, &
				OE_REGHEADER::SHIPNO, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

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
				2%, 23%, , SMG$M_BOLD)
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
				3%, 23%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(7%) AND 2%) = 0%
		THEN
			AR_35CUSTOM::CUSNAM = &
				STRING$(LEN(AR_35CUSTOM::CUSNAM), A"?"B) &
				IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
				"Q0" + OE_REGHEADER::CUSNUM) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::CUSNAM, 7%, 28%, , SMG$M_BOLD)

			AR_35CUSTOM::ADD1 = &
				STRING$(LEN(AR_35CUSTOM::ADD1), A"?"B) &
				IF OE_REGHEADER::CUSNUM = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD1, 8%, 28%, , SMG$M_BOLD)

			AR_35CUSTOM::ADD2 = &
				STRING$(LEN(AR_35CUSTOM::ADD2), A"?"B) &
				IF OE_REGHEADER::CUSNUM = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD2, 9%, 28%, , SMG$M_BOLD)

			TEXT$ = AR_35CUSTOM::CITY + " " + AR_35CUSTOM::STATE + " " + AR_35CUSTOM::ZIP

			TEXT$ = STRING$(LEN(TEXT$), A"?"B) &
				IF OE_REGHEADER::CUSNUM = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TEXT$, 10%, 28%, , SMG$M_BOLD)
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
			OE_REGHEADER::SHIPNAM, 11%, 17%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::ADD1, 12%, 17%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::ADD2, 13%, 17%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::ADD3, 14%, 17%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::CITY, 15%, 17%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::STATE, 15%, 33%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::ZIP, 16%, 17%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGHEADER::COUNTRY, 16%, 28%, , SMG$M_BOLD)

		UTL_CARRIER::DESCR = &
			STRING$(LEN(UTL_CARRIER::DESCR), A"?"B) &
			IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, &
			"Q0" + OE_REGHEADER::SHIPVIA) <> 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
			UTL_CARRIER::DESCR, 1%, 28%, , SMG$M_BOLD)

		UTL_TERMS::DESCR = &
			STRING$(LEN(UTL_TERMS::DESCR), A"?"B) &
			IF MAIN_WINDOW(UT_MAIN_TERMS.ID, &
			"Q0" + OE_REGHEADER::TERMS) <> 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
			UTL_TERMS::DESCR, 2%, 28%, , SMG$M_BOLD)

		UTL_LOCATION::LOCNAME = &
			STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
			IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
			"Q0" + OE_REGHEADER::LOCATION) <> 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			UTL_LOCATION::LOCNAME, 18%, 34%, , SMG$M_BOLD)


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

	!
	! View the Record.
	!
	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "  Order Number Date        Type  Cat  CusNumber  CustPo         Amount Itm"

		CASE 2%
			MVALUE = "015,027,033,038,049,060,071"

		CASE 3%

			TOTAL = 0.0
			LIN$ = "    "
			COUNT% = 0%
			WHILE OE_READ_REGLINE(OE_REGHEADER::ORDNUM, LIN$, "GT", &
				OE_REGLINE_READ, QTY()) = CMC$_NORMAL

				LIN$ = OE_REGLINE_READ::LIN
				TOTAL = TOTAL + QTY(9%)
				COUNT% = COUNT% + 1%
			NEXT

			MVALUE = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "   "    + &
				PRNT_DATE(OE_REGHEADER::ORDDATE, 8%) + "  " + &
				OE_REGHEADER::ORDTYPE + "    " + &
				OE_REGHEADER::ORDCAT + " " + &
				OE_REGHEADER::CUSNUM + " " + &
				OE_REGHEADER::CUSTPO + " " + &
				FORMAT$(TOTAL, "###,###.##") + " " + &
				FORMAT$(COUNT%, "###")

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
	! Abstract:CUSTOMERLOOKUP
	!	^*Customerlookup\*
	!	.b
	!	.lm +5
	!	The ^*Customerlookup\* function
	!	views a customer master file record.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:LINESUMMARY
	!	^*LineSummary\*
	!	.b
	!	.lm +5
	!	The ^*LineSummary\* function
	!	displays a summary of each line item as follows:
	!	.table 3,25
	!	.te
	!	Line _#
	!	.te
	!	Product
	!	.te
	!	Quantity Ordered
	!	.te
	!	Quantity Shipped
	!	.te
	!	Quantity Canceled
	!	.te
	!	Quantity Remaining
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:LINEDETAIL
	!	^*Linedetail\*
	!	.b
	!	.lm +5
	!	The ^*Linedetail\* function
	!	displays the status of line items of a specific order. The user has the option
	!	to begin the display at any selected line item. The information displayed
	!	includes the following:
	!	.table 3,25
	!	.te
	!	Line [Item _#]
	!	.te
	!	Type
	!	.te
	!	Product Code
	!	.te
	!	Quantity
	!	.te
	!	Transaction Date
	!	.te
	!	Unit Price
	!	.te
	!	Promo Off
	!	.te
	!	Disc. %
	!	.te
	!	Unit Cost
	!	.te
	!	Ship To
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.xLine Detail
	!
	!--
