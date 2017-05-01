1	%TITLE "Asset Account Table"
	%SBTTL "AD_MAIN_ACCOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_ACCOUNT(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Depreciation Account\* table determines
	!	which General Ledger accounts will be affected when posting the calculated
	!	depreciation to the General Ledger. In the table,
	!	location and asset type combinations are assigned and
	!	correlated with specific accumulated depreciation and
	!	depreciation expense accounts.
	!	.lm -5
	!
	! Index:
	!	.x Depreciation Accounts
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_ACCOUNT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_ACCOUNT
	!	$ DELETE AD_MAIN_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	12/04/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/17/92 - Dan Perkins
	!		Cleaned up code relating to FUNC_TESTENTRY.
	!
	!	04/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	02/11/93 - Dan Perkins
	!		Change "V0" to "VX" on chard of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/04/95 - Kevin Handy
	!		(V3.6)
	!		Modified source to V3.6 standards.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit.
	!
	!	07/03/96 - Kevin Handy
	!		Reformat source code.
	!		Lose commented out code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/16/98 - Kevin Handy
	!		Lose an excessive number of %PAGE
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

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION FUNC_TESTENTRY

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_ACCOUNT.HB"
	MAP (AD_ACCOUNT)	AD_ACCOUNT_CDD		AD_ACCOUNT
	MAP (AD_ACCOUNT_OLD)	AD_ACCOUNT_CDD AD_ACCOUNT_OLD, AD_ACCOUNT2

	%INCLUDE "SOURCE:[AD.OPEN]AD_ASSTYPE.HB"
	MAP (AD_ASSTYPE)	AD_ASSTYPE_CDD		AD_ASSTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AD_ACCOUNT) AD_ACCOUNT.CH%

	%PAGE

	!
	! Set up error trapping
	!
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
		SMG_WINDOW::DESCR = "Asset Account Table"
		SMG_WINDOW::NHELP = "AD_MAIN_ACCOUNT"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 5%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Location"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF AD_ACCOUNT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AD_ACCOUNT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_ACCOUNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AD_MAIN_ACCOUNT = ERR
			CONTINUE 770
		END WHEN

		AD_ACCOUNT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_ACCOUNT.OPN"
		USE
			AD_MAIN_ACCOUNT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AD_ACCOUNT.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AD_ACCOUNT.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = AD_ACCOUNT.CH%
		WHEN ERROR IN
			RESET #AD_ACCOUNT.CH%
			GET #AD_ACCOUNT.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

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

		DATA	05,  05, "(01) Location", &
			06,  05, "(02) Asset Type", &
			07,  05, "(03) Asset Acct", &
			08,  05, "(04) Dep Acct", &
			09,  05, "(05) Expense Acct", &
			0,   0, ""

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
	!	^*(01) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field contains the location
	!	code for a particular record.
	!	.b
	!	The field will accommodate four (4) alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at
	!	this field will display a list of valid location codes.
	!	.lm -5
	!
	! Index:
	!	.x Location>Asset
	!	.x Asset>Location
	!
	!--


			AD_ACCOUNT::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;23", TEMP$, &
				AD_ACCOUNT::LOCATION, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
					THEN
					AD_ACCOUNT::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF


		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Asset Type\*
	!	.b
	!	.lm +5
	!	The ^*Asset Type\* field contains the Asset Type
	!	code for a particular record.
	!	.b
	!	The field will accommodate one (1) character.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at
	!	this field will display a list of valid asset type codes.
	!	.lm -5
	!
	! Index:
	!	.x Asset>Type
	!	.x Type>Asset
	!
	!--


			AD_ACCOUNT::ASSET_TYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;23", TEMP$, &
				AD_ACCOUNT::ASSET_TYPE, MFLAG, "'E", &
				MVALUE)


			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_ASSTYPE.ID, "V0") = 1%
				THEN
					AD_ACCOUNT::ASSET_TYPE = &
						AD_ASSTYPE::ASSET_TYPE
				END IF
				GOTO Reenter
			END IF


		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Asset Account\*
	!	.b
	!	.lm +5
	!	The ^*Asset Account\* field contains the
	!	asset account number to which this record has reference.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located
	!	at this field will provide a list of valid asset account
	!	numbers.
	!	.lm -5
	!
	! Index:
	!	.x Account>Asset
	!	.x Asset Account
	!	.x Asset Account>Depreciation Account Table
	!
	!--


			AD_ACCOUNT::ASS_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;23", TEMP$, &
				AD_ACCOUNT::ASS_ACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					AD_ACCOUNT::ASS_ACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF


		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Depreciation  Account\*
	!	.b
	!	.lm +5
	!	The ^*Depreciation Account\* field contains the
	!	depreciation account number to which this record has reference.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located
	!	at this field will provide a list of valid depreciation account codes.
	!	.lm -5
	!
	! Index:
	!	.x Depreciation Account>Depreciation Account Table
	!	.x Depreciation Account Table>Depreciation Account
	!
	!--


			AD_ACCOUNT::DEP_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"08;23", TEMP$, &
				AD_ACCOUNT::DEP_ACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					AD_ACCOUNT::DEP_ACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Expense Account\*
	!	.b
	!	.lm +5
	!	The ^*Expense Account\* field contains the
	!	expense account number to which this record has reference.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located
	!	at this field will provide a list of valid expense account codes.
	!	.lm -5
	!
	! Index:
	!	.x Expense Account
	!	.x Account>Expense
	!	.x Expense Account>Depreciation Account Table
	!
	!--


			AD_ACCOUNT::EXP_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"09;23", TEMP$, &
				AD_ACCOUNT::EXP_ACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					AD_ACCOUNT::EXP_ACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	!******************************************************************
	! Test values
	!******************************************************************
	CASE OPT_TESTENTRY
		AD_MAIN_ACCOUNT = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			AD_MAIN_ACCOUNT = FUNC_TESTENTRY( SMG_WINDOW, &
				AD_ACCOUNT::LOCATION, UTL_LOCATION::LOCNAME, &
				"AD", MLOOP, "LOC", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(UTL_LOCATION::LOCNAME, 35%), 5%, &
				42%, , SMG$M_BOLD)

		CASE 2%
			!
			! Is the input defined?
			!
			AD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_ACCOUNT::ASSET_TYPE, &
				AD_ASSTYPE::DESCRIPTION, &
				"AD", MLOOP, "PROG", &
				"Account ", AD_MAIN_ASSTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AD_ASSTYPE::DESCRIPTION, 6%, 42%, , SMG$M_BOLD)

		CASE 3%
			!
			! Is the input defined?
			!
			AD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_ACCOUNT::ASS_ACCT, &
				GL_CHART::DESCR, &
				"AD", MLOOP, "PROG", &
				"Account ", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 35%), 7%, 42%, , SMG$M_BOLD)

		CASE 4%
			!
			! Is the input defined?
			!
			AD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_ACCOUNT::DEP_ACCT, &
				GL_CHART::DESCR, &
				"AD", MLOOP, "PROG", &
				"Account ", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 35%), 8%, 42%, , SMG$M_BOLD)

		CASE 5%
			!
			! Is the input defined?
			!
			AD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_ACCOUNT::EXP_ACCT, &
				GL_CHART::DESCR, &
				"AD", MLOOP, "PROG", &
				"Account ", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 35%), 9%, 42%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			STORENAME$ = STRING$(40%, 63%)
			STORENAME$ = UTL_LOCATION::LOCNAME &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"Q0" + AD_ACCOUNT::LOCATION) = 1%
			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(STORENAME$, 35%), 5%, 42%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			TYPENAME$ = STRING$(20%, 63%)
			IF MAIN_WINDOW(AD_MAIN_ASSTYPE.ID, &
				"Q0" + AD_ACCOUNT::ASSET_TYPE) = 1%
			THEN
				TYPENAME$ = AD_ASSTYPE::DESCRIPTION + &
					SPACE$(10%)
			END IF

			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TYPENAME$, 6%, 42%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			ACCOUNTNAME$ = STRING$(40%, 63%)

			ACCOUNTNAME$ = GL_CHART::DESCR &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"Q0" + AD_ACCOUNT::ASS_ACCT) = 1%

			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(ACCOUNTNAME$, 35%), 7%, 42%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			ACCOUNTNAME$ = STRING$(40%, 63%)
			ACCOUNTNAME$ = GL_CHART::DESCR &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + AD_ACCOUNT::DEP_ACCT) = 1%

			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(ACCOUNTNAME$, 35%), 8%, 42%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			ACCOUNTNAME$ = STRING$(40%, 63%)
			ACCOUNTNAME$ = GL_CHART::DESCR &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + AD_ACCOUNT::EXP_ACCT) = 1%
			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(ACCOUNTNAME$, 35%), 9%, 42%, , SMG$M_BOLD)
		END IF

20500	!******************************************************************
	! Set GL_OBJECT_OLD value
	!******************************************************************
	CASE OPT_SETOLD
		AD_ACCOUNT_OLD = AD_ACCOUNT

	!******************************************************************
	! Restore GL_OBJECT_OLD value
	!******************************************************************
	CASE OPT_RESETOLD
		AD_ACCOUNT = AD_ACCOUNT_OLD

	!******************************************************************
	! Set default value
	!******************************************************************
	CASE OPT_SETDEFAULT
		AD_ACCOUNT2 = AD_ACCOUNT

	!******************************************************************
	! Restore default value
	!******************************************************************
	CASE OPT_RESETDEFAULT
		AD_ACCOUNT = AD_ACCOUNT2

	!***********************************************************
	! View header
	!***********************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Loc  AssType AssAccount         " + &
				"DepAccount         ExpAccount"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,015,034,053"


		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_ACCOUNT::LOCATION + " " + &
				AD_ACCOUNT::ASSET_TYPE + "      " + &
				AD_ACCOUNT::ASS_ACCT + " " + &
				AD_ACCOUNT::DEP_ACCT + " " + &
				AD_ACCOUNT::EXP_ACCT

		END SELECT

	!******************************************************************
	! Find
	!******************************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_ACCOUNT::LOCATION + &
					AD_ACCOUNT::ASSET_TYPE, &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
