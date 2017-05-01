1	%TITLE "Ship To Address Maintenance"
	%SBTTL "OE_MAIN_SHIPTO"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_SHIPTO(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	The ^*Ship To Address Maintenance\* function enters the customer's
	!	various Ship To Addresses and the information pertaining to those addresses.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_SHIPTO/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN OE_MAIN_SHIPTO
	!	$ DELETE OE_MAIN_SHIPTO.OBJ;*
	!
	! Author:
	!
	!	06/18/90 - Lance Williams
	!
	! Modification history:
	!
	!	06/25/91 - Craig Tanner
	!		Added list choises to country field.
	!
	!	10/04/91 - Frank F. Starman
	!		Added the field ::LINES under OPT_FIND.
	!
	!	10/08/91 - Frank F. Starman
	!		Added notes.
	!
	!	12/02/91 - Dan Perkins
	!		Added Phone Field to program resulting
	!		from file layout change.  Rearanged field
	!		display on screen to accomodate new field.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	10/12/92 - Dan Perkins
	!		Moved contitional field testing form OPT_ENTRY to
	!		OPT_RESETDEFAULT.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/25/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/20/95 - Kevin Handy
	!		Fix last parameter to entr_3phone.
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include Statements
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.HB"
	MAP (OE_SHIPTO)		OE_SHIPTO_CDD		OE_SHIPTO
	MAP (OE_SHIPTO_OLD)	OE_SHIPTO_CDD		OE_SHIPTO_OLD, &
							OE_SHIPTO_DEF

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	!
	! Common Statements
	!
	COM (CH_OE_SHIPTO) &
		OE_SHIPTO.CH%, &
		OE_SHIPTO.READONLY%

	COM (CH_AR_35CUSTOM) &
		AR_35CUSTOM.CH%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Order Entry Shipping Address"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "OE_MAIN_SHIPTO"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 14%
		SMG_WINDOW::HPOS = 3%
		SMG_WINDOW::VPOS = 5%
		SMG_WINDOW::NITEMS = 16%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 14%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 5%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Customer Number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)


700		!
		! Declare channels
		!
		IF OE_SHIPTO.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_SHIPTO.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			OE_MAIN_SHIPTO = ERR
			CONTINUE 770
		END WHEN

		OE_SHIPTO.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.OPN"
		USE
			OE_MAIN_SHIPTO = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_SHIPTO.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_SHIPTO.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = OE_SHIPTO.CH%
		WHEN ERROR IN
			RESET #OE_SHIPTO.CH%
			GET #OE_SHIPTO.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN


	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!******************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	01,01, "(01) Ship Loc", &
			02,01, "(02) Ship Name", &
			03,01, "(03) Address 1", &
			04,01, "(04) Address 2", &
			05,01, "(05) Address 3", &
			06,01, "(06) City", &
			06,37, "(07) State", &
			07,01, "(08) Zip Code", &
			07,37, "(09) Country", &
			08,01, "(10) Phone", &
			09,01, "(11) Ship VIA", &
			10,01, "(12) Location ", &
			11,01, "(13) Salesman", &
			12,01, "(14) Tax Code", &
			13,01, "(15) Tax Exempt #", &
			11,37, "(16) Notes:", &
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
	!	.x Ship Location
	!	^*(01) Ship Location\*
	!	.b
	!	.lm +5
	!	The ^*Ship Loc\* field enters
	!	a unique code identifying the ship-to address for the selected customer.
	!	This allows for a nearly unlimited number of ship-to addresses for a single
	!	customer.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm +5
	!
	! Index:
	!
	!--
			OE_SHIPTO::LINES = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;19", TEMP$, &
				OE_SHIPTO::LINES, MFLAG, &
				"'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Ship Name
	!	^*(02) Ship Name\*
	!	.b
	!	.lm +5
	!	The ^*Ship Name\* field enters
	!	the name to whom the order will be shipped.
	!	.b
	!	Up to fifty characters may be entered.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SHIPTO::SHIPNAM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;19", TEMP$, &
				OE_SHIPTO::SHIPNAM, MFLAG, &
				"'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x Ship to Address 1
	!	^*(03) Ship to Address 1\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 1\* field enters
	!	the first line of the address for shipping the order.
	!	.b
	!	Up to twenty five characters may be entered.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SHIPTO::ADD1 = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;19", TEMP$, &
				OE_SHIPTO::ADD1, MFLAG, &
				"'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Ship to Address 2
	!	^*(04) Ship to Address 2\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 2\* field enters
	!	the second line of the address for shipping the order.
	!	.b
	!	The field will accommodate twenty five characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SHIPTO::ADD2 = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;19", TEMP$, &
				OE_SHIPTO::ADD2, MFLAG, &
				"'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Ship to Address 3
	!	^*(05) Ship to Address 3\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 3\* field enters
	!	the third line of the address for shipping the order.
	!	.b
	!	Twenty five characters may be entered.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SHIPTO::ADD3 = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;19", TEMP$, &
				OE_SHIPTO::ADD3, MFLAG, &
				"'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x Customer>City
	!	^*(06) City\*
	!	.b
	!	.lm +5
	!	The ^*City\* field enters the city in which the
	!	customer is located.
	!	.b
	!	The field will accommodate fifteen characters.
	!	.lm -5
	!
	! Index:
	!	.x City>Customer
	!
	!--
			OE_SHIPTO::CITY = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;19", TEMP$, &
				OE_SHIPTO::CITY, MFLAG, &
				"'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	.x Customer>State
	!	^*(07) State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field enters the state in which
	!	the customer is located.
	!	.b
	!	The field will accommodate a two (2) character state postal
	!	code.
	!	.lm -5
	!
	! Index:
	!	.x State>Customer
	!
	!--
			OE_SHIPTO::STATE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;50", TEMP$, &
				OE_SHIPTO::STATE, MFLAG, &
				"'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	.x Customer>Zip
	!	^*(08) Zip\*
	!	.b
	!	.lm +5
	!	The ^*Zip\* field enters the zip or postal
	!	code for the area in which a customer is located.
	!	.b
	!	Up to ten characters may be entered.
	!	.lm -5
	!
	! Index:
	!	.x Zip>Customer
	!
	!--
			OE_SHIPTO::ZIP = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;19", TEMP$, &
				OE_SHIPTO::ZIP, MFLAG, &
				"'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	.x Customer>Country
	!	^*(09) Country\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field enters the country if
	!	a customer is located in a foreign country.
	!	.b
	!	Valid country codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Two characters will be accepted.
	!	.lm -5
	!
	! Index:
	!	.x Country>Customer
	!
	!--
			OE_SHIPTO::COUNTRY = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;50", TEMP$, &
				OE_SHIPTO::COUNTRY, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "V0" + &
					OE_SHIPTO::COUNTRY) = 1%
				THEN
					OE_SHIPTO::COUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO Reenter
			END IF

		CASE 10%
	!++
	! Abstract:FLD010
	!	.x Ship To>Phone
	!	^*(10) Phone\*
	!	.b
	!	.lm +5
	!	The ^*Phone\* field is for entry of a customer's telephone number.
	!	.b
	!	It is not required
	!	to enter the special characters. The system will insert them
	!	automatically.
	!	.b
	!	The field will accept up to ten characters.
	!	.lm -5
	!
	! Index:
	!	.X Ship To>Telephone
	!	.x Phone>Ship To
	!	.x Telephone>Ship To
	!
	!--
			OE_SHIPTO::PHONE = ENTR_3PHONE(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;19", TEMP$, &
				OE_SHIPTO::PHONE, 2% OR MFLAG, 0%, MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	.x Ship VIA
	!	^*(11) Ship VIA\*
	!	.b
	!	.lm +5
	!	The ^*Ship VIA\* field enters a user defined
	!	number which will identify a particular carrier.
	!	.b
	!	Valid ship via codes may be viewed by pressing ^*List Choices\*.
	!	Additional ship to codes may be added by pressing the ^*F17\* key.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SHIPTO::SHIPVIA = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;19", TEMP$, &
				OE_SHIPTO::SHIPVIA, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, "V0") = 1%
				THEN
					OE_SHIPTO::SHIPVIA = &
						UTL_CARRIER::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 12%
	!++
	! Abstract:FLD012
	!	.x Location>Number
	!	^*(12) Location Number\*
	!	.b
	!	.lm +5
	!	The ^*Location Number\* field
	!	enters a user defined code to identify
	!	a company location.
	!	.b
	!	Valid codes may be viewed by pressing ^*List Choices\*.
	!	Additional location numbers may be added by pressing the ^*F17\* key.
	!	.b
	!	Up to four characters will be accepted.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SHIPTO::LOCATION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;19", TEMP$, &
				OE_SHIPTO::LOCATION, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					OE_SHIPTO::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 13%
	!++
	! Abstract:FLD013
	!	.x Salesman
	!	^*(13) Salesman\*
	!	.b
	!	.lm +5
	!	The ^*Salesman\* field enters the identifying number of
	!	the salesman.
	!	.b
	!	Valid salesman codes may be viewed by pressing ^*List Choices\*.
	!	Additional salesman codes may be added by pressing the ^*F17\* key.
	!	.b
	!	The field will accommodate ten characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SHIPTO::SALESMAN = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;19",TEMP$, OE_SHIPTO::SALESMAN, MFLAG, &
				"'E", MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	.x Tax Code
	!	^*(14) Tax Code\*
	!	.b
	!	.lm +5
	!	The ^*Tax Code\* field enters a user defined
	!	code which will identify a certain tax code.
	!	.b
	!	Valid tax codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	The field will accept up to two characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SHIPTO::TAXCODE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;19",TEMP$, OE_SHIPTO::TAXCODE, MFLAG, &
				"'E", MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	.x Tax Exemptions
	!	^*(15) Tax Exemption _#\*
	!	.b
	!	.lm +5
	!	The ^*Tax Exemption _#\* field enters the sales tax
	!	exemption or resale license number as applicable.
	!	.b
	!	Up to fifteen characters may be entered.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SHIPTO::TAXEXEMP= ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;19",TEMP$, OE_SHIPTO::TAXEXEMP, MFLAG, &
				"'E", MVALUE)

		CASE 16%
	!++
	! Abstract:FLD016
	!	.x Notes
	!	^*(16) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* field enters any free formatted notes
	!	relative to the shipping instruction.
	!	.b
	!	The field will accommodate an entry of up to forty characters.
	!	.lm -5
	!
	! Index:
	!
	!--
 FirstNote:
			OE_SHIPTO::NOTES(0%) = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;37", TEMP$, OE_SHIPTO::NOTES(0%), MFLAG, &
				"'E", MVALUE)

			IF OE_SHIPTO::NOTES(0%) = ""
			THEN
				OE_SHIPTO::NOTES(1%) = ""
				OE_SHIPTO::NOTES(2%) = ""

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					OE_SHIPTO::NOTES(1%), 13%, 37%)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					OE_SHIPTO::NOTES(2%), 14%, 37%)

				GOTO BypassNotes
			END IF

 SecondNote:
			OE_SHIPTO::NOTES(1%) = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;37", TEMP$, OE_SHIPTO::NOTES(1%), MFLAG, &
				"'E", MVALUE)

			IF OE_SHIPTO::NOTES(1%) = ""
			THEN
				OE_SHIPTO::NOTES(2%) = ""

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					OE_SHIPTO::NOTES(2%), 14%, 37%)

				GOTO BypassNotes
			END IF

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_UP
				GOTO FirstNote
			END SELECT

			OE_SHIPTO::NOTES(2%) = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;37", TEMP$, OE_SHIPTO::NOTES(2%), MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_UP
				GOTO SecondNote
			END SELECT
 BypassNotes:

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

	OE_MAIN_SHIPTO = 0%

	CASE OPT_DISPLAY

		!
		! Display the descriptions
		!

	! Set OE_SHIPTO_OLD value
	!
20500	CASE OPT_SETOLD
		OE_SHIPTO_OLD = OE_SHIPTO

	!
	! Restore OE_SHIPTO_OLD value
	!
	CASE OPT_RESETOLD
		OE_SHIPTO = OE_SHIPTO_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_SHIPTO_DEF = OE_SHIPTO

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_SHIPTO = OE_SHIPTO_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		OE_SHIPTO::CUSNUM  = MVALUE

		IF MFLAG = 1%
		THEN
			OE_SHIPTO::SHIPNAM= AR_35CUSTOM::CUSNAM &
				IF OE_SHIPTO::SHIPNAM = ""

			OE_SHIPTO::ADD1 = AR_35CUSTOM::ADD1 &
				IF OE_SHIPTO::ADD1 = ""

			OE_SHIPTO::ADD2 = AR_35CUSTOM::ADD2 &
				IF OE_SHIPTO::ADD2 = ""

			OE_SHIPTO::CITY = AR_35CUSTOM::CITY &
				IF OE_SHIPTO::CITY = ""

			OE_SHIPTO::STATE = AR_35CUSTOM::STATE &
				IF OE_SHIPTO::STATE = ""

			OE_SHIPTO::ZIP = AR_35CUSTOM::ZIP &
				IF OE_SHIPTO::ZIP = ""

			OE_SHIPTO::COUNTRY = AR_35CUSTOM::COUNTRY &
				IF OE_SHIPTO::COUNTRY = ""

			OE_SHIPTO::PHONE = AR_35CUSTOM::PHONE &
				IF OE_SHIPTO::PHONE = ""

			OE_SHIPTO::SHIPVIA = AR_35CUSTOM::CARRIER &
				IF OE_SHIPTO::SHIPVIA = ""

			OE_SHIPTO::LOCATION = AR_35CUSTOM::LOCATION &
				IF OE_SHIPTO::LOCATION = ""

		END IF

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE OE_SHIPTO::CUSNUM + &
				OE_SHIPTO::LINES, REGARDLESS

		END SELECT

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Line Ship_name                 " + &
				"Address                   City            St"
		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,033,059,075"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = OE_SHIPTO::LINES + " " + &
				LEFT(OE_SHIPTO::SHIPNAM, 25%) + " " + &
				LEFT$(EDIT$(OE_SHIPTO::ADD1 + " " + &
					OE_SHIPTO::ADD2 + " " + &
					OE_SHIPTO::ADD3, 16%) + &
					SPACE$(25%), 25%) + " " + &
				OE_SHIPTO::CITY + " " + &
				OE_SHIPTO::STATE

		END SELECT

	!
	! Handle array of records
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
				FIND #SMG_WINDOW::CHAN, KEY #0% EQ MVALUE, REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000
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
						OE_SHIPTO::LINES, REGARDLESS
				USE
					CONTINUE 28000
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
			IF OE_SHIPTO::CUSNUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			OE_SHIPTO::CUSNUM = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
