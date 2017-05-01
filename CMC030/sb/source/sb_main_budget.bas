1	%TITLE "Sub Ledger Budget Maintenance"
	%SBTTL "SB_MAIN_BUDGET"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SB_MAIN_BUDGET(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1989 BY
	!
	! Computer Management Center, Inc.
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
	!	.p
	!	This program maintains Sub Ledger Budget Maintenance file.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SB_SOURCE:SB_MAIN_BUDGET/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN SB_MAIN_BUDGET
	!	$ DELETE SB_MAIN_BUDGET.OBJ;*
	!
	! Author:
	!
	!	02/03/89 - B. Craig Larsen
	!
	! Modification history:
	!
	!	11/13/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	03/02/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	04/28/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/05/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[SB.OPEN]SB_BUDGET.HB"
	MAP	(SB_BUDGET)		SB_BUDGET_CDD	SB_BUDGET
	MAP	(SB_BUDGET_OLD)	SB_BUDGET_CDD	SB_BUDGET_OLD, SB_BUDGET2

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP	(SB_SUBACCOUNT)		SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)		GL_CHART_CDD	GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_SB_BUDGET) &
		SB_BUDGET.CH%, &
		SB_BUDGET.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Budget Maintenance"
		SMG_WINDOW::NHELP = "SB_MAIN_BUDGET"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 11%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 8%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 6%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Operation"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%
		SMG_WINDOW::KNAME(1%) = "Period"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%

		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 8%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF SB_BUDGET.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if it was
			! that way from the last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF SB_BUDGET.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_BUDGET.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			SB_MAIN_BUDGET = ERR
			CONTINUE 770
		END WHEN

		SB_BUDGET.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_BUDGET.OPN"
		USE
			SB_MAIN_BUDGET = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		SB_BUDGET.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(SB_BUDGET.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = SB_BUDGET.CH%
		WHEN ERROR IN
			RESET #SB_BUDGET.CH%
			GET #SB_BUDGET.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	02,03, "(01) Operation", &
			03,03, "(02) GL Account", &
			04,03, "(03) Period", &
			05,03, "(04) Amount", &
			06,03, "(05) Units", &
			07,03, "(06) Hours", &
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
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
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
	!	^*(01) Operation\*
	!	.p
	!	The ^*Operation\* field enters the number which indicates
	!	the operation for which the budget pertains.
	!
	! Index:
	!	.x Operation>Budget Maintenance
	!	.x Budget Maintenance>Operation
	!
	!--


			SB_BUDGET::OPERATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;20",	TEMP$, SB_BUDGET::OPERATION, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) General Ledger Account\*
	!	.p
	!	The ^*General Ledger Account\* number field is provided to enter the General
	!	Ledger account which
	!	The field will accommodate eighteen (18) characters.
	!	.p
	!	Pressing ^*<List Choices>\* while the cursor is located at
	!	this field will provide a list of valid General Ledger Account Numbers.
	!
	! Index:
	!	.x General Ledger Account
	!
	!--


			SB_BUDGET::ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;20",	TEMP$, SB_BUDGET::ACCOUNT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					SB_BUDGET::ACCOUNT = GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Period\*
	!	.p
	!	The ^*Period\* field enters the period for which the
	!	budget pertains.
	!
	! Index:
	!	.x Period>Budget Maintenance
	!	.x Budget Maintenance>Period
	!
	!--


			SB_BUDGET::PERIOD = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"04;20", TEMP$, SB_BUDGET::PERIOD, &
				MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Amount\*
	!	.p
	!	The ^*Amount\* field enters the amount budgeted to
	!	the specified operation and period.
	!
	! Index:
	!	.x Amount>Budget Maintenance
	!	.x Budget Maintenance>Amount
	!	.x Amount>Budget Maintenance
	!	.x Budget Maintenance>Amount
	!
	!--


			SB_BUDGET::AMOUNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;20",TEMP$, SB_BUDGET::AMOUNT, MFLAG, &
				"###,###,###.##", MVALUE)


		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Units\*
	!	.p
	!	The ^*Units\* field enters the number of units budgeted
	!	for the specified operation and period.
	!
	! Index:
	!	.x Units>Budget Maintenance
	!	.x Budget Maintenance>Units
	!
	!--


			SB_BUDGET::UNITS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;20",TEMP$, SB_BUDGET::UNITS, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Hours\*
	!	.p
	!	The ^*Hours\* field enters the number of hours budgeted
	!	for the specified operation and period.
	!
	! Index:
	!	.x Hours>Budget Maintenance
	!	.x Budget Maintenance>Hours
	!
	!--


			SB_BUDGET::HOURS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;20",TEMP$, SB_BUDGET::HOURS, MFLAG, &
				"###,###,###.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		SB_MAIN_BUDGET = 0%

		SELECT MLOOP

		CASE 2%
			IF SB_BUDGET::ACCOUNT <> ""
			THEN
				!
				! Is the Account defined?
				!
				SB_MAIN_BUDGET = FUNC_TESTENTRY(SMG_WINDOW, &
					SB_BUDGET::ACCOUNT, &
					GL_CHART::DESCR, &
					"SB", SCOPE::PRG_PROGRAM, "ACCOUNT", &
					"Account", GL_MAIN_CHART.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					GL_CHART::DESCR, 3%, 39%, , SMG$M_BOLD)

			END IF

		CASE 3%
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, KEY #0% EQ &
						SB_BUDGET::SYSTEM + &
						SB_BUDGET::SUBACCOUNT + &
						SB_BUDGET::OPERATION + &
						SB_BUDGET::ACCOUNT + &
						SB_BUDGET::PERIOD, &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				SB_MAIN_BUDGET = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + SB_BUDGET::ACCOUNT) <> 1%

			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 3%, 39%, , SMG$M_BOLD)
		END IF

	!
	! Set SB_BUDGET_OLD value
	!
20500	CASE OPT_SETOLD
		SB_BUDGET_OLD = SB_BUDGET

	!
	! Restore SB_BUDGET_OLD value
	!
	CASE OPT_RESETOLD
		SB_BUDGET = SB_BUDGET_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		SB_BUDGET2 = SB_BUDGET

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		SB_BUDGET = SB_BUDGET2
		SB_BUDGET::SYSTEM = &
			LEFT(MVALUE, LEN(SB_BUDGET::SYSTEM))
		SB_BUDGET::SUBACCOUNT = &
			RIGHT(MVALUE, LEN(SB_BUDGET::SYSTEM) + 1%)

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE ="  Opration Account            Period " + &
				"   Amount        Units        Hours"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "011,030,037,050,063"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = SB_BUDGET::OPERATION + " " + &
				SB_BUDGET::ACCOUNT + " " + &
				SB_BUDGET::PERIOD + " " + &
				FORMAT$(SB_BUDGET::AMOUNT, "#########.##") + " " + &
				FORMAT$(SB_BUDGET::UNITS, "#########.##") + " " + &
				FORMAT$(SB_BUDGET::HOURS, "#########.##")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE SB_BUDGET::SYSTEM + &
				SB_BUDGET::SUBACCOUNT + &
				SB_BUDGET::OPERATION + &
				SB_BUDGET::ACCOUNT + &
				SB_BUDGET::PERIOD, REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE SB_BUDGET::PERIOD + &
				SB_BUDGET::SYSTEM + &
				SB_BUDGET::SUBACCOUNT, REGARDLESS

		END SELECT

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
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% EQ MVALUE, &
					REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000 IF ERR = 155%
				EXIT HANDLER
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
						KEY #0% GE SB_BUDGET::SYSTEM + &
						SB_BUDGET::SUBACCOUNT + &
						SB_BUDGET::OPERATION + &
						SB_BUDGET::ACCOUNT + &
						SB_BUDGET::PERIOD, &
						REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN

			CASE 1%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #1% GE SB_BUDGET::PERIOD + &
						SB_BUDGET::SYSTEM + &
						SB_BUDGET::SUBACCOUNT, &
						REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
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
			IF SB_BUDGET::SYSTEM + SB_BUDGET::SUBACCOUNT = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			SB_BUDGET::SYSTEM = &
				LEFT(MVALUE, LEN(SB_BUDGET::SYSTEM))
			SB_BUDGET::SUBACCOUNT = &
				RIGHT(MVALUE, LEN(SB_BUDGET::SYSTEM) + 1%)

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
