1	%TITLE "Sales Account Balances"
	%SBTTL "SA_MAIN_BALANCE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SA_MAIN_BALANCE(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Sales Account Balances\* option contains a history file which shows all
	!	past activity and views the balances for the salesmen up
	!	to the current date.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_MAIN_BALANCE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN SA_MAIN_BALANCE
	!	$ DELETE SA_MAIN_BALANCE.OBJ;*
	!
	! Author:
	!	??????? - UNKNOWN
	!
	! Modification history:
	!
	!	05/24/91 - J. Shad Rydalch
	!		Modified for subaccount file changes & fixed
	!		problem with view keys.
	!
	!	11/16/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/07/96 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE
	MAP (SB_BALANCE_OLD)	SB_BALANCE_CDD		SB_BALANCE_OLD
	MAP (SB_BALANCE_DEF)	SB_BALANCE_CDD		SB_BALANCE_DEF

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_SB_BALANCE) &
		SB_BALANCE.CH%, &
		SB_BALANCE.READONLY%

	!
	! Default System
	!
	DEF_SYSTEM$ = "SB"

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
	! necesSBry that have not already been opened.
	!
	CASE OPT_INIT

	!******************************************************************
	! Set up information
	!******************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Sales Account Balances"
		SMG_WINDOW::NHELP = "SA_MAIN_BALANCE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 11%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Salesmen"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%
		SMG_WINDOW::KNAME(1%) = "Period"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		!
		! Category
		!

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF SB_BALANCE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if it was
			! that way from the last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF SB_BALANCE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			SA_MAIN_BALANCE = ERR
			CONTINUE 770
		END WHEN

		SB_BALANCE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.OPN"
		USE
			SA_MAIN_BALANCE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		SB_BALANCE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(SB_BALANCE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = SB_BALANCE.CH%
		WHEN ERROR IN
			RESET #SB_BALANCE.CH%
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


		DATA	05,03, "(01) Salesman #", &
			06,03, "(02) GL Account", &
			07,03, "(03) Period", &
			08,03, "(04) Amount", &
			09,03, "(05) Beg Amount", &
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
	!	^*(01) Salesman Number\*
	!	.p
	!	The ^*Salesman Number\* field enters of a number which
	!	will reference a particular salesman.
	!	.p
	!	The field will accommodate ten (10) alphanumeric characters.
	!
	! Index:
	!
	!--
			SB_BALANCE::SUBACCOUNT = &
				ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"5;20",TEMP$, SB_BALANCE::SUBACCOUNT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "VX") = 1%
				THEN
					SB_BALANCE::SUBACCOUNT = &
						SA_SALESMAN::SALESMAN
				END IF
				GOTO ReEnter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) General Ledger Account\*
	!	.p
	!	The ^*General Ledger Account\* field requires an input of the Account number
	!	in which there is question. The entered account number displays all activity
	!	in this account for the salesman number entered.
	!
	! Index:
	!	.x Account
	!
	!--
			SB_BALANCE::ACCOUNT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;20",	TEMP$, SB_BALANCE::ACCOUNT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					SB_BALANCE::ACCOUNT = GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Period\*
	!	.p
	!	The ^*Period\* field requires entry of the period in question for the current
	!	balances up to the specified time period.
	!
	! Index:
	!	.x Period
	!
	!--
			SB_BALANCE::PERIOD = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;20", TEMP$, SB_BALANCE::PERIOD, &
				MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Amount\*
	!	.p
	!	The ^*Amount\* field contains the dollar amount recorded against the General
	!	Ledger Account for the current period and salesman number specified.
	!
	! Index:
	!	.x Amount
	!
	!--
			SB_BALANCE::AMOUNT = &
				ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				"08;20",TEMP$, SB_BALANCE::AMOUNT, MFLAG, &
				"###,###,###.##", MVALUE)


		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Beginning Amount\*
	!	.p
	!	The ^*Beginning Amount\* field contains the dollar amount balance for the
	!	general ledger account and salesman number at the beginning of the period.
	!
	! Index:
	!	.x Beginning Amount
	!
	!--
			SB_BALANCE::BEG_AMOUNT = &
				ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				"09;20",TEMP$, SB_BALANCE::BEG_AMOUNT, MFLAG, &
				"###,###,###.##", MVALUE)


		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		SA_MAIN_BALANCE = 0%

		SELECT MLOOP

		CASE 1%
			IF SB_BALANCE::SUBACCOUNT <> ""
			THEN
				!
				! Is the Sub Account defined?
				!
				SA_MAIN_BALANCE = FUNC_TESTENTRY(SMG_WINDOW, &
					"S" + SB_BALANCE::SUBACCOUNT, &
					SA_SALESMAN::SALESMAN, &
					"SB", MLOOP, "SUBACCOUNT", &
					"Salesman#", SA_MAIN_SALESMAN.ID)

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SA_SALESMAN::DESCR, 5%, 39%,, &
					SMG$M_BOLD)

			END IF

		CASE 2%
			IF SB_BALANCE::ACCOUNT <> ""
			THEN
				!
				! Is the Account defined?
				!
				SA_MAIN_BALANCE = FUNC_TESTENTRY(SMG_WINDOW, &
					SB_BALANCE::ACCOUNT, &
					GL_CHART::ACCT, &
					"SB", MLOOP, "ACCOUNT", &
					"Account", GL_MAIN_CHART.ID)

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					GL_CHART::DESCR, 6%, 39%, , SMG$M_BOLD)

			END IF

		CASE 3%
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, KEY #0% EQ &
						SB_BALANCE::SYSTEM + &
						SB_BALANCE::SUBACCOUNT + &
						SB_BALANCE::OPERATION + &
						SB_BALANCE::ACCOUNT + &
						SB_BALANCE::PERIOD, REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				SA_MAIN_BALANCE = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			SA_SALESMAN::DESCR = STRING$(LEN(SA_SALESMAN::DESCR), &
				A"?"B) &
				IF MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "Q0" + &
				"S" + SB_BALANCE::SUBACCOUNT) <> 1%
			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				SA_SALESMAN::DESCR, 5%, 39%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + SB_BALANCE::ACCOUNT) <> 1%

			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 6%, 39%, , SMG$M_BOLD)
		END IF

	!
	! Set SB_BALANCE_OLD value
	!
20500	CASE OPT_SETOLD
		SB_BALANCE_OLD = SB_BALANCE

	!
	! Restore SB_BALANCE_OLD value
	!
	CASE OPT_RESETOLD
		SB_BALANCE = SB_BALANCE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		SB_BALANCE_DEF = SB_BALANCE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		SB_BALANCE = SB_BALANCE_DEF
		SB_BALANCE::SYSTEM = DEF_SYSTEM$
		SB_BALANCE::OPERATION = ""

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE ="  Salesman#  Account #          " + &
				" Period        Amount    " + &
				" BegAmount    "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "012,032,040,054"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = SB_BALANCE::SUBACCOUNT	+ " "  + &
				SB_BALANCE::ACCOUNT + "  " + &
				SB_BALANCE::PERIOD + "  " + &
				FORMAT$(SB_BALANCE::AMOUNT, &
					"#,###,###.##") + "  " + &
				FORMAT$(SB_BALANCE::BEG_AMOUNT, "#,###,###.##")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE SB_BALANCE::SYSTEM + &
				SB_BALANCE::SUBACCOUNT + &
				SB_BALANCE::OPERATION + &
				SB_BALANCE::ACCOUNT + &
				SB_BALANCE::PERIOD, REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE SB_BALANCE::PERIOD + &
				SB_BALANCE::SYSTEM + &
				SB_BALANCE::SUBACCOUNT, REGARDLESS

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
					KEY #0% EQ DEF_SYSTEM$, &
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
						KEY #0% GE SB_BALANCE::SYSTEM + &
						SB_BALANCE::SUBACCOUNT + &
						SB_BALANCE::OPERATION + &
						SB_BALANCE::ACCOUNT + &
						SB_BALANCE::PERIOD, REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN

			CASE 1%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #1% GE SB_BALANCE::PERIOD + &
						SB_BALANCE::SYSTEM + &
						SB_BALANCE::SUBACCOUNT, REGARDLESS
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

			SMG_WINDOW::CURREC = 0% &
				IF SB_BALANCE::SYSTEM = DEF_SYSTEM$


		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
