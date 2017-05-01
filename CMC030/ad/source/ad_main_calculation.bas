1	%TITLE "Depreciation Ledger"
	%SBTTL "AD_MAIN_CALCULATION"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_CALCULATION(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Depreciation Ledger\* contains information concerning
	!	depreciation dollar amounts and units which were stored at the
	!	time the last depreciation calculation was executed.
	!	.b
	!	The ^*Depreciation Ledger\* file will be recreated each time
	!	the depreciation calculation routine is executed.
	!	.lm -5
	!
	! Index:
	!	.x Depreciation>Ledger
	!	.x Ledger>Depreciation
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_CALCULATION/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_CALCULATION
	!	$ DELETE AD_MAIN_CALCULATION.OBJ;*
	!
	! Author:
	!
	!	12/23/87 - Frank F. Starman
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
	!	04/05/95 - Kevin Handy
	!		(V3.6)
	!		Updated to version 3.6 source standards.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit.
	!
	!	09/26/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/19/97 - Kevin Handy
	!		Use integer in #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/07/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.HB"
	MAP (AD_CALCULATION)	AD_CALCULATION_CDD	AD_CALCULATION
	MAP (AD_CALCULATION_OLD)AD_CALCULATION_CDD	AD_CALCULATION_OLD, AD_CALCULATION2

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)	AD_35ASSET_CDD		AD_35ASSET

	%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.HB"
	MAP (AD_OBJECT)		AD_OBJECT_CDD		AD_OBJECT

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.HB"
	MAP (AD_CONTROLOBJ)	AD_CONTROLOBJ_CDD	AD_CONTROLOBJ

	MAP (CH_AD_CALCULATION) AD_CALCULATION.CH%, &
				AD_CALCULATION.READONLY%

	COM (TABLE_AD_STATUS) &
		STATITLE$ = 25%, &
		STA$(4%) = 25%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Asset period ledger"
		SMG_WINDOW::NHELP = "AD_MAIN_CALCULATION"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 6%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Asset_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "Object"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		!
		! Category
		!
		STATITLE$ = "Code  Description"
		STA$(0%) = "04"
		STA$(1%) = "A     Active"
		STA$(2%) = "F     Fully depreciated"
		STA$(3%) = "R     Retired"
		STA$(4%) = "X     No status"

		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF AD_CALCULATION.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AD_CALCULATION.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AD_MAIN_CALCULATION = ERR
			CONTINUE 770
		END WHEN

		AD_CALCULATION.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.OPN"
		USE
			AD_MAIN_CALCULATION = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AD_CALCULATION.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AD_CALCULATION.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = AD_CALCULATION.CH%
		WHEN ERROR IN
			RESET #AD_CALCULATION.CH%
			GET #AD_CALCULATION.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	04,08, "(01) Asset #", &
			05,08, "(02) Object", &
			07,08, "(03) Act Status", &
			08,08, "(04) CUR Amount", &
			09,08, "(05) CUR Units", &
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
20200	CASE OPT_ENTRY
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
	!	An entry in this field is required. This field may
	!	contain up to ten (10) alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\* will provide a list of
	!	valid Asset numbers.
	!	.lm -5
	!
	! Index:
	!	.x Asset>Number
	!	.x Number>Asset
	!
	!--

			AD_CALCULATION::ASSET_NUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;26",TEMP$, AD_CALCULATION::ASSET_NUM, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_ASSET.ID, "V0") = 1%
				THEN
					AD_CALCULATION::ASSET_NUM = &
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


			AD_CALCULATION::DEP_OBJECT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;26",	TEMP$, AD_CALCULATION::DEP_OBJECT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_OBJECT.ID, "V0") = 1%
				THEN
					AD_CALCULATION::DEP_OBJECT = &
						AD_OBJECT::DEP_OBJECT
				END IF
				GOTO Reenter
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Activity Status\*
	!	.b
	!	.lm +5
	!	The ^*Activity Status\* field enters the
	!	Status for an asset.
	!	.b
	!	An entry is required in this field. The field will
	!	accommodate one (1) character.
	!	.b
	!	Pressing ^*<List Choices>\* will provide a list of valid codes.
	!	.lm -5
	!
	! Index:
	!	.x Activity Status
	!
	!--

			AD_CALCULATION::DEP_STATUS = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;26", TEMP$, AD_CALCULATION::DEP_STATUS, &
				MFLAG, "'E", MVALUE, STA$(), &
				STATITLE$, "005")

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Current Amount\*
	!	.b
	!	.lm +5
	!	The ^*Current Amount\* field contains the accumulated amount
	!	depreciated on a particular asset.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Current Amount
	!
	!--

			AD_CALCULATION::AMOUNT_CUR = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;26",TEMP$, AD_CALCULATION::AMOUNT_CUR, &
				MFLAG, "##,###,###.##", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Current Units\*
	!	.b
	!	.lm +5
	!	The ^*Current Units\* field contains the accumulated
	!	number of units depreciated.
	!	.lm -5
	!
	! Index:
	!	.x Current Units
	!
	!--

			AD_CALCULATION::UNIT_CUR = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"09;26",TEMP$, AD_CALCULATION::UNIT_CUR, &
				MFLAG, "##,###,###.#", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AD_MAIN_CALCULATION = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			AD_MAIN_CALCULATION = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_CALCULATION::ASSET_NUM, &
				AD_35ASSET::DESCRIPTION, &
				"AD", MLOOP, "ASSETT", &
				"Asset number", AD_MAIN_ASSET.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AD_35ASSET::DESCRIPTION, 37%), &
				4%, 41%, , SMG$M_BOLD)

		CASE 2%
			!
			! Is the input defined?
			!
			AD_MAIN_CALCULATION = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_CALCULATION::DEP_OBJECT, &
				AD_OBJECT::DESCRIPTION, &
				"AD", MLOOP, "OBJ", &
				"Object", AD_MAIN_OBJECT.ID)

			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, &
						KEY #0% EQ AD_CALCULATION::ASSET_NUM + &
						AD_CALCULATION::DEP_OBJECT, REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				AD_MAIN_CALCULATION = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 0%)
			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF MAIN_WINDOW(AD_MAIN_CONTROLOBJ.ID, &
			"Q0" + AD_CALCULATION::DEP_OBJECT) = 1%
		THEN
			LASTDEP$ = AD_CONTROLOBJ::LASTDEP
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Depreciated to the period " + LASTDEP$, 2%, 10%)

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			ASSETNAME$ = STRING$(40%, 63%)
			ASSETNAME$ = AD_35ASSET::DESCRIPTION &
				IF MAIN_WINDOW(AD_MAIN_ASSET.ID, &
				"Q0" + AD_CALCULATION::ASSET_NUM) = 1%
			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(ASSETNAME$, 37%), &
				4%, 41%, , SMG$M_BOLD)
		END IF


	!
	! Set AD_CALCULATION_OLD value
	!
20500	CASE OPT_SETOLD
		AD_CALCULATION_OLD = AD_CALCULATION

	!
	! Restore AD_CALCULATION_OLD value
	!
	CASE OPT_RESETOLD
		AD_CALCULATION = AD_CALCULATION_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_CALCULATION2 = AD_CALCULATION

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_CALCULATION = AD_CALCULATION2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE ="  Asset#     Object Status     DepAmount        Units"
		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,020,027,042,055"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_CALCULATION::ASSET_NUM + " " + &
				AD_CALCULATION::DEP_OBJECT + "      " + &
				AD_CALCULATION::DEP_STATUS + "      " + &
				FORMAT$(AD_CALCULATION::AMOUNT_CUR, &
					"##,###,###.##") + " " + &
				FORMAT$(AD_CALCULATION::UNIT_CUR, &
					"##,###,###.#")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_CALCULATION::ASSET_NUM + &
				AD_CALCULATION::DEP_OBJECT, &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE AD_CALCULATION::DEP_OBJECT + &
				AD_CALCULATION::ASSET_NUM, &
				REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
