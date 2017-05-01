1	%TITLE "Asset Balance Register"
	%SBTTL "AD_MAIN_BALANCE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_BALANCE(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Asset Balance\* register contains information concerning
	!	total depreciation dollar amounts and units which have been
	!	accumulated to date.
	!	.b
	!	The Activity Status Flag indicates the following status for
	!	each asset:
	!	.lm 15
	!	.list 0,"*"
	!	.b
	!	.le
	!	^*A\* = Active
	!	.le
	!	^*R\* = Retired
	!	.le
	!	^*F\* = Fully Depreciated
	!	.le
	!	^*_*\* = No Status
	!	.b
	!	.lm -10
	!	This file is updated each time the Asset Period Ledger is updated.
	!	The AD system will automatically maintain this file. Although manual
	!	maintenance is allowed, it is not recommended.
	!	.els
	!
	! Index:
	!	.x Asset Balance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_BALANCE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_BALANCE
	!	$ DELETE AD_MAIN_BALANCE.OBJ;*
	!
	! Author:
	!
	!	11/15/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/25/92 - Kevin Handy
	!		Added "KEY #0%" to a get to remove syntax errors
	!		whilch would not allow it to compile from
	!		someones undocumented and untested change.
	!
	!	03/18/92 - Dan Perkins
	!		Commented out code relating to ERA$ which is not used.
	!
	!	04/17/92 - Dan Perkins
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
	!		Update to version 3.6 formats.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit.
	!
	!	09/25/96 - Kevin Handy
	!		Reformat source code.
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

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.HB"
	MAP (AD_BALANCE)	AD_BALANCE_CDD		AD_BALANCE
	MAP (AD_BALANCE_OLD)	AD_BALANCE_CDD		AD_BALANCE_OLD, AD_BALANCE2

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)	AD_35ASSET_CDD		AD_35ASSET

	%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.HB"
	MAP (AD_OBJECT)		AD_OBJECT_CDD		AD_OBJECT

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.HB"
	MAP (AD_CONTROLOBJ)	AD_CONTROLOBJ_CDD	AD_CONTROLOBJ

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AD_BALANCE) AD_BALANCE.CH%, &
		AD_BALANCE.READONLY%

	COM (TABLE_AD_STATUS) &
		STATITLE$ = 25%, &
		STA$(4%)  = 25%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	%PAGE

	SELECT MOPTION

	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Asset balances"
		SMG_WINDOW::NHELP = "AD_MAIN_BALANCE"
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
		IF AD_BALANCE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AD_BALANCE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AD_MAIN_BALANCE = ERR
			CONTINUE 770
		END WHEN

		AD_BALANCE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.OPN"
		USE
			AD_MAIN_BALANCE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AD_BALANCE.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AD_BALANCE.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = AD_BALANCE.CH%
		WHEN ERROR IN
			RESET #AD_BALANCE.CH%
			GET #AD_BALANCE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE


20100	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	04,08, "(01) Asset #", &
			05,08, "(02) Object", &
			07,08, "(03) Act Status", &
			08,08, "(04) Last Period", &
			09,08, "(05) CTD Amount", &
			10,08, "(06) CTD Units", &
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
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Asset _#\*
	!	.b
	!	.lm +5
	!	The ^*Asset _#\* field enters an
	!	asset number as established in the Asset Master file.
	!	.b
	!	An entry in this field is required. This field may
	!	contain up to ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Asset>Number
	!	.x Number>Asset
	!
	!--


			AD_BALANCE::ASSET_NUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;26",TEMP$, AD_BALANCE::ASSET_NUM, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_ASSET.ID, "V0") = 1%
				THEN
					AD_BALANCE::ASSET_NUM = &
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
	!	Pressing ^*<List Choices>\* while the cursor is located
	!	at this field will provide a list of valid object codes.
	!	.lm -5
	!
	! Index:
	!	.x Object
	!
	!--


			AD_BALANCE::DEP_OBJECT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;26",	TEMP$, AD_BALANCE::DEP_OBJECT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_OBJECT.ID, "V0") = 1%
				THEN
					AD_BALANCE::DEP_OBJECT = &
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
	!	Status for this asset.
	!	.b
	!	An entry is required in this field. The field will
	!	accommodate one (1) character.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this
	!	field will provide a list of valid status codes.
	!	.lm -5
	!
	! Index:
	!	.x Activity Status
	!
	!--


			AD_BALANCE::DEP_STATUS = ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;26", TEMP$, AD_BALANCE::DEP_STATUS, &
				MFLAG, "'E", MVALUE, STA$(), &
				STATITLE$, "005")

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Last Period\*
	!	.b
	!	.lm +5
	!	The ^*Last Period\* field stores the
	!	last period when an asset, with a related code,
	!	was depreciated.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Last Period
	!
	!--


			!IF MAIN_WINDOW(AD_MAIN_CONTROLOBJ.ID,'Q0'+AD_BALANCE::DEP_OBJECT) = 1%
			!THEN
			!	ERA$ = AD_CONTROLOBJ::ERA
			!END IF

			AD_BALANCE::LASTPER = ENTR_PERIOD(SMG_WINDOW::WNUMBER, &
				"8;26",	TEMP$, AD_BALANCE::LASTPER, &
				MFLAG, "", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) CTD Amount\*
	!	.b
	!	.lm +5
	!	The ^*CTD Amount\* field contains the dollar amount
	!	of depreciation accumulated to date.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x CTD Amount
	!
	!--


			AD_BALANCE::AMOUNT_CTD = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;26", TEMP$, AD_BALANCE::AMOUNT_CTD, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) CTD Units\*
	!	.b
	!	.lm +5
	!	The ^*CTD Units\* field contains the number of
	!	Depreciated Units accumulated to date.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x CTD Units
	!
	!--


			AD_BALANCE::UNIT_CTD = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;26", TEMP$, AD_BALANCE::UNIT_CTD, MFLAG, &
				"##,###,###.#", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AD_MAIN_BALANCE = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			AD_MAIN_BALANCE = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_BALANCE::ASSET_NUM, &
				AD_35ASSET::DESCRIPTION, &
				"AR", MLOOP, "ACCT", &
				"Asset number", AD_MAIN_ASSET.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AD_35ASSET::DESCRIPTION, 37%), &
				4%, 41%, , SMG$M_BOLD)

		CASE 2%
			!
			! Is the input defined?
			!
			AD_MAIN_BALANCE = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_BALANCE::DEP_OBJECT, &
				AD_OBJECT::DESCRIPTION, &
				"AR", MLOOP, "ACCT", &
				"Object", AD_MAIN_OBJECT.ID)

			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, KEY #0% &
						EQ AD_BALANCE::ASSET_NUM + &
						AD_BALANCE::DEP_OBJECT, REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				AD_MAIN_BALANCE = 2%
				CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 0%)
			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			ASSETNAME$ = STRING$(40%, 63%)
			ASSETNAME$ = AD_35ASSET::DESCRIPTION &
				IF MAIN_WINDOW(AD_MAIN_ASSET.ID, &
				"Q0" + AD_BALANCE::ASSET_NUM) = 1%
			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(ASSETNAME$, 37%), &
				4%, 41%, , SMG$M_BOLD)
		END IF

	!
	! Set AD_BALANCE_OLD value
	!
20500	CASE OPT_SETOLD
		AD_BALANCE_OLD = AD_BALANCE

	!
	! Restore AD_BALANCE_OLD value
	!
	CASE OPT_RESETOLD
		AD_BALANCE = AD_BALANCE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_BALANCE2 = AD_BALANCE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_BALANCE = AD_BALANCE2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE ="  Asset#     Object Status LastPeriod " + &
				"   DepAmount       Units"
		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,020,027,038,051,063"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_BALANCE::ASSET_NUM + " " + &
				AD_BALANCE::DEP_OBJECT + "      " + &
				AD_BALANCE::DEP_STATUS + "      " + &
				AD_BALANCE::LASTPER + "     " + &
				FORMAT$(AD_BALANCE::AMOUNT_CTD, &
					"#########.##") + " " + &
				FORMAT$(AD_BALANCE::UNIT_CTD, &
					"#########.#")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_BALANCE::ASSET_NUM + &
					AD_BALANCE::DEP_OBJECT, &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE AD_BALANCE::DEP_OBJECT + &
					AD_BALANCE::ASSET_NUM, &
				REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

29000	!******************************************************************
	! Trap errors
	!******************************************************************

	ON ERROR GO BACK

32767	!******************************************************************
	! End of AD_MAIN_BALANCE function
	!******************************************************************
	END FUNCTION
