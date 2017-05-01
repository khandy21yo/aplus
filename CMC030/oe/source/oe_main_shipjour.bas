1	%TITLE "Order Shipping Journal"
	%SBTTL "OE_MAIN_SHIPJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_SHIPJOUR(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Order Shipping Journal\* option
	!	maintains the order shipping information.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_SHIPJOUR/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN OE_MAIN_SHIPJOUR
	!	$ DELETE OE_MAIN_SHIPJOUR.OBJ;*
	!
	! Author:
	!
	!
	! Modification history:
	!
	!	02/28/92 - Kevin Handy
	!		Changed "CMC$NORMAL" to "CMC$_NORMAL".
	!
	!	03/20/92 - Dan Perkins
	!		Removed unused code.
	!
	!	04/10/92 - Dan Perkins
	!		Rset ORDER NUMBER.  Use FUNC_TESTENTRY to test input.
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/15/96 - Kevin Handy
	!		Lose extra '&' before 'end if'.
	!		Reformat source code.
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
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! Include CDD's
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPJOUR.HB"
	MAP (OE_SHIPJOUR)	OE_SHIPJOUR_CDD		OE_SHIPJOUR
	MAP (OE_SHIPJOUR_DEF)	OE_SHIPJOUR_CDD		OE_SHIPJOUR_OLD, OE_SHIPJOUR2

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	!
	! This common area must be mapped in both the main program and
	! in OE_MAIN_SHIPLINE.
	!
	COM (CH_OE_SHIPJOUR) &
		INVLOCATION$ = 4%, &
		BATCH_NO$ = 2%, &
		OE_SHIPJOUR.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Order Shipping Journal " + BATCH_NO$
		SMG_WINDOW::NHELP = "OE_MAIN_SHIPJOUR"
		SMG_WINDOW::CHAN  = OE_SHIPJOUR.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Order Number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		CALL READ_DEFAULTS(SMG_WINDOW)

20010		GOTO 20040 IF OE_SHIPJOUR.CH% > 0%

		!
		! Open OE_SHIPJOUR
		!
		%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPJOUR.CRE"

20040		SMG_WINDOW::CHAN  = OE_SHIPJOUR.CH%
		WHEN ERROR IN
			RESET #OE_SHIPJOUR.CH%
			GET #OE_SHIPJOUR.CH%, REGARDLESS
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

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

			DATA	3,  5, "(01) Order #", &
				4,  5, "(02) Ship Date", &
				5,  5, "(03) Ship Via", &
				6,  5, "(04) Operator", &
				7,  5, "(05) Notes", &
				3,  33, "Release Number", &
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
	!	The ^*Order Number\* field enters the order
	!	number for the journal entry.
	!	.b
	!	An entry is required in this field.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_SHIPJOUR::ORDNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;20", TEMP$, &
				OE_SHIPJOUR::ORDNUM, MFLAG OR 2%, &
				"~R 'E", MVALUE)


			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(OE_MAIN_REGHEADER.ID, "V0") = 1%
				THEN
					OE_SHIPJOUR::ORDNUM = &
						OE_REGHEADER::ORDNUM
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Date
	!	^*(02) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field enters the date the order
	!	was shipped.
	!	.b
	!	The field will default to the current date.  To accept the
	!	current date, press ^*Return\*.  To override the default date,
	!	enter the correct date and press ^*Return\*.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_SHIPJOUR::SHIPDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;20", TEMP$, OE_SHIPJOUR::SHIPDATE, MFLAG, &
				"'E", MVALUE)

		CASE 3%


	!++
	! Abstract:FLD003
	!	.x Ship Via
	!	^*(03) Ship Via\*
	!	.b
	!	.lm +5
	!	The ^*Ship Via\* field enters the code which
	!	will identify the carrier for this shipment.
	!	.b
	!	Valid ship Via codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	The field will accommodate two characters.
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_SHIPJOUR::SHIPVIA = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;20", TEMP$, &
				OE_SHIPJOUR::SHIPVIA, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, "V0") = 1%
				THEN
					OE_SHIPJOUR::SHIPVIA = &
						UTL_CARRIER::CODE
				END IF
				GOTO Reenter
			END IF


		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Operator
	!	^*(04) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters
	!	the name or initials of the operator responsible for shipping the order.
	!	.b
	!	An entry is required in this field.
	!	.b
	!	The field will accept up to ten characters.
	!	.b
	!	Valid operator codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--


			OE_SHIPJOUR::OPERATOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;20", TEMP$, &
				OE_SHIPJOUR::OPERATOR, MFLAG, &
				"'E", MVALUE)


		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* field enters notes or comments
	!	concerning this particular order.
	!	.lm -5
	!
	! Index:
	!	.x Notes
	!
	!--

			OE_SHIPJOUR::NOTES(0%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;18", TEMP$, &
				OE_SHIPJOUR::NOTES(0%), MFLAG, &
				"'E", MVALUE)

			GOTO Bypassnotes IF OE_SHIPJOUR::NOTES(0%) = "" AND &
				OE_SHIPJOUR::NOTES(1%) = "" AND &
				OE_SHIPJOUR::NOTES(2%) = ""


			OE_SHIPJOUR::NOTES(1%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;18", TEMP$, &
				OE_SHIPJOUR::NOTES(1%), MFLAG, &
				"'E", MVALUE)

			GOTO Bypassnotes IF OE_SHIPJOUR::NOTES(1%) = "" AND &
				OE_SHIPJOUR::NOTES(2%) = ""

			OE_SHIPJOUR::NOTES(2%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;18", TEMP$, &
				OE_SHIPJOUR::NOTES(2%), MFLAG, &
				"'E", MVALUE)

 Bypassnotes:

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		OE_MAIN_SHIPJOUR = 0%

		SELECT MLOOP

		CASE 1%
			IF(OE_READ_REGHEADER(OE_SHIPJOUR::ORDNUM, &
				OE_REGHEADER_READ) <> CMC$_NORMAL) AND &
				(OE_REGHEADER_READ::ORDNUM <> OE_SHIPJOUR::ORDNUM)
			THEN
				CALL HELP_34MESSAGE(SCOPE, &
					"undefined order number", &
					"W", SCOPE::PRG_PROGRAM, "", "UNDORD")

	!++
	! Warning:UNDORD
	!	^*Undefined Order\*
	!	.b
	!	^*Explanation\*
	!	.b
	!	.lm +5
	!	Selected order number doesn't exist in Order Register
	!	file.
	!	.b
	!	.lm -5
	!	^*User Action\*
	!	.b
	!	.lm +5
	!	Select order number from the Order Register file. Pressing
	!	^*List Choices\* will display a list of all valid order numbers.
	!	.lm -5
	!
	! Index:
	!	.x Undefined Order
	!
	!--

				OE_MAIN_SHIPJOUR = 1%
			END IF

			INVLOCATION$ = OE_REGHEADER_READ::LOCATION

			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #OE_SHIPJOUR.CH%, &
						KEY #0% EQ OE_SHIPJOUR::ORDNUM + "", &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				OE_MAIN_SHIPJOUR = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
			END IF

		CASE 2%

			IF OE_SHIPJOUR::SHIPDATE = ""
			THEN
				OE_MAIN_SHIPJOUR = 1%
			END IF

		CASE 3%
			!
			! Display the descriptions for carrier
			!
			OE_MAIN_SHIPJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_SHIPJOUR::SHIPVIA, &
				UTL_CARRIER::DESCR, &
				"OE", MLOOP, "PROG", &
				"Carrier", &
				UTL_MAIN_CARRIER.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_CARRIER::DESCR, &
				5%, 28%, , SMG$M_BOLD)

		CASE 4%
			IF OE_SHIPJOUR::OPERATOR = ""
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Must enter operator name/code", 1%)
				OE_MAIN_SHIPJOUR = 1%
			END IF


		END SELECT

	CASE OPT_DISPLAY

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_SHIPJOUR::SHIPNO, 3%, 50%, , SMG$M_BOLD)

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			UTL_CARRIER::DESCR = &
				STRING$(LEN(UTL_CARRIER::DESCR), A"?"B) &
				IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, &
				"Q0" + OE_SHIPJOUR::SHIPVIA) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_CARRIER::DESCR, &
				5%, 28%, , SMG$M_BOLD)
		END IF

	!
	! Set OE_SHIPJOUR_OLD value
	!
20500	CASE OPT_SETOLD
		OE_SHIPJOUR_OLD = OE_SHIPJOUR

	!
	! Restore OE_SHIPJOUR_OLD value
	!
	CASE OPT_RESETOLD
		OE_SHIPJOUR = OE_SHIPJOUR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_SHIPJOUR2 = OE_SHIPJOUR

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_SHIPJOUR = OE_SHIPJOUR2
		OE_SHIPJOUR::SHIPNO = ""
		OE_SHIPJOUR::SHIPDATE = DATE_TODAY &
			IF TRM$(OE_SHIPJOUR::SHIPDATE)=""
	!
	! View the Record.
	!
	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%

			MVALUE = "  OrderNumber ShipDate "

		CASE 2%

			MVALUE = "014"

		CASE 3%

			MVALUE = CONV_STRING(OE_SHIPJOUR::ORDNUM, CMC$_LEFT) + "      " + &
				PRNT_DATE(OE_SHIPJOUR::SHIPDATE, 8%)

		END SELECT

	!
	! Find the Order Number.
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #OE_SHIPJOUR.CH%, &
				KEY #0% GE OE_SHIPJOUR::ORDNUM + "", &
				REGARDLESS

		END SELECT

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
