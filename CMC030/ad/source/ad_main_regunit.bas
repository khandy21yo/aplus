1	%TITLE "Units Register"
	%SBTTL "AD_MAIN_REGUNIT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_REGUNIT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Units Register\* stores depreciation
	!	units for all assets for which the unit of production depreciation
	!	method is applicable.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_REGUNIT/LINE
	!	$ LIB/REP  FUNC_LIB:CMCFUN AD_MAIN_REGUNIT
	!	$ DELETE AD_MAIN_REGUNIT.OBJ;*
	!
	! Author:
	!
	!	12/15/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/20/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/07/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	10/03/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/19/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/08/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.HB"
	MAP (AD_REGUNIT)	AD_REGUNIT_CDD		AD_REGUNIT
	MAP (AD_REGUNIT_OLD)	AD_REGUNIT_CDD		AD_REGUNIT_OLD, &
							AD_REGUNIT2

	%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.HB"
	MAP (AD_OBJECT)		AD_OBJECT_CDD		AD_OBJECT

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)	AD_35ASSET_CDD		AD_35ASSET

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!*************************************************************
		! Set up information
		!*************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Unit register"
		SMG_WINDOW::NHELP = "AD_MAIN_REGUNIT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 6%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Asset_num"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		AD_REGUNIT::POST_DATE = DATE_TODAY
		AD_REGUNIT::POST_TIME = TIME_NOW

		AD_REGUNIT::BATCH = ""

		CALL READ_DEFAULTS(SMG_WINDOW)

20700		IF (AD_REGUNIT.CH% <= 0%)
		THEN
			CALL ASSG_CHANNEL(AD_REGUNIT.CH%, STAT%)
			IF STAT%
			THEN
				AD_MAIN_REGUNIT = 1%
			END IF

			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Unable to open AD_REGUNIT file " + NUM1$(ERR), 0%)
				AD_MAIN_REGUNIT = 1%
				CONTINUE 27000
			END WHEN
		END IF

20710		SMG_WINDOW::CHAN  = AD_REGUNIT.CH%

		WHEN ERROR IN
			RESET #AD_REGUNIT.CH%
			GET #AD_REGUNIT.CH%, REGARDLESS
		USE
			CONTINUE 27000 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	04,05, "(01) Asset #", &
			05,05, "(02) Object ", &
			06,05, "(03) Period", &
			07,05, "(04) Date", &
			08,05, "(05) StationMan", &
			09,05, "(06) Units", &
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

	!
	! Enter/Display/Default
	!
20800	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Asset _#\*
	!	.b
	!	.lm +5
	!	The ^*Asset Number\* field enters an
	!	asset number from the Asset Master file.
	!	.b
	!	An entry is required in this field. The field may
	!	contain up to ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Asset>Number
	!	.x Number>Asset
	!
	!--

			AD_REGUNIT::ASSET_NUM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;23", TEMP$, &
				AD_REGUNIT::ASSET_NUM, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_ASSET.ID, "V0") = 1%
				THEN
				AD_REGUNIT::ASSET_NUM = &
						AD_35ASSET::ASSET_NUM
				END IF
				GOTO Reenter
			END IF


		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Object\*
	!	.b
	!	.lm +5
	!	The ^*Object\* field enters the object code
	!	from the Object Description File.
	!	.b
	!	An entry is required in this field. The field will
	!	accommodate one (1) character.
	!	.b
	!	Pressing ^*<List Choices>\* will provide a list of
	!	valid codes.
	!	.lm -5
	!
	! Index:
	!	.x Object
	!
	!--

			AD_REGUNIT::DEP_OBJECT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;23", TEMP$, &
				AD_REGUNIT::DEP_OBJECT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_OBJECT.ID, "V0") = 1%
				THEN
					AD_REGUNIT::DEP_OBJECT = &
						AD_OBJECT::DEP_OBJECT
				END IF
				GOTO Reenter
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field contains the Period from the Utility
	!	system which will indicate when an asset with a particular
	!	object code was depreciated.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Period
	!
	!--

			AD_REGUNIT::PERIOD = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;23", TEMP$, &
				AD_REGUNIT::PERIOD, MFLAG, "'E", &
				MVALUE)


		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field stores the date which was entered
	!	in the Units of Production Journal.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Date
	!
	!--

			AD_REGUNIT::ACTION_DATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;23", TEMP$, &
				AD_REGUNIT::ACTION_DATE, MFLAG, "'E", &
				MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) StationMan\*
	!	.b
	!	.lm +5
	!	The ^*StationMan\* field is entered automatically
	!	with the name of the person responsible for production
	!	when specific units were produced.
	!	.b
	!	The field will accommodate ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x StationMan
	!
	!--

			AD_REGUNIT::STATIONMAN = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;23", TEMP$, &
				AD_REGUNIT::STATIONMAN, MFLAG, "'E", &
				MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Units\*
	!	.b
	!	.lm +5
	!	The ^*Units\* field is automatically entered with
	!	the Units information which was entered in the Units of
	!	Production Journal.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Units
	!
	!--

			AD_REGUNIT::QUANTITY = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;23",TEMP$, AD_REGUNIT::QUANTITY, MFLAG, &
				"#,###,###.#", MVALUE)


		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20850	CASE OPT_TESTENTRY
		AD_MAIN_REGUNIT = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			AD_MAIN_REGUNIT = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_REGUNIT::ASSET_NUM, &
				AD_35ASSET::DESCRIPTION, &
				"AR", MLOOP, "ASSET", &
				"Asset number", AD_MAIN_ASSET.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AD_35ASSET::DESCRIPTION, 4%, 38%, , SMG$M_BOLD)

		CASE 2%
			!
			! Is the input defined?
			!
			AD_MAIN_REGUNIT = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_REGUNIT::DEP_OBJECT, &
				AD_OBJECT::DESCRIPTION, &
				"AR", MLOOP, "OBJ", &
				"Object", AD_MAIN_OBJECT.ID)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			DISPLAYNAME$ = STRING$(40%, 63%)
			DISPLAYNAME$ = AD_35ASSET::DESCRIPTION &
				IF MAIN_WINDOW(AD_MAIN_ASSET.ID, &
				"Q0" + AD_REGUNIT::ASSET_NUM) = 1%

			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				DISPLAYNAME$, 4%, 38%, , SMG$M_BOLD)
		END IF

	!
	! Set AD_REGUNIT_OLD value
	!
20900	CASE OPT_SETOLD
		AD_REGUNIT_OLD = AD_REGUNIT

	!
	! Restore AD_REGUNIT_OLD value
	!
	CASE OPT_RESETOLD
		AD_REGUNIT = AD_REGUNIT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_REGUNIT2 = AD_REGUNIT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_REGUNIT = AD_REGUNIT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Asset#     Object Period" + &
				" Date     StationMan    Quantity"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,020,027,036,047,059"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_REGUNIT::ASSET_NUM + " " + &
				AD_REGUNIT::DEP_OBJECT + "      " + &
				AD_REGUNIT::PERIOD + " " + &
				AD_REGUNIT::ACTION_DATE + " " + &
				AD_REGUNIT::STATIONMAN + " " + &
				FORMAT$(AD_REGUNIT::QUANTITY, &
					"#,###,###.#")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_REGUNIT::ASSET_NUM + &
					AD_REGUNIT::DEP_OBJECT + &
					AD_REGUNIT::PERIOD, &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
