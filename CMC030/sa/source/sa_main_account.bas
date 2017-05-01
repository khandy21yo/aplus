1	%TITLE "Sales Account Maintenance"
	%SBTTL "SA_MAIN_ACCOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SA_MAIN_ACCOUNT(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_MAIN_ACCOUNT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN SA_MAIN_ACCOUNT
	!	$ DELETE SA_MAIN_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	07/05/90 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	08/15/91 - Frank F. Starman
	!		Change codes for account group.
	!
	!	11/30/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	04/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/03/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT
	MAP (SB_ACCOUNT_OLD)	SB_ACCOUNT_CDD		SB_ACCOUNT_OLD
	MAP (SB_ACCOUNT_DEF)	SB_ACCOUNT_CDD		SB_ACCOUNT_DEF

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_SB_ACCOUNT) &
		SB_ACCOUNT.CH%, &
		SB_ACCOUNT.READONLY%

	COM (TT_SA_MAIN_ACCOUNT) &
		ACCTITLE$ = 20%, &
		ACCTYPE$(5%) = 40%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	SYSTEM$ = "SA"

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
		SMG_WINDOW::DESCR = "Sales Analysis Accounts"
		SMG_WINDOW::NHELP = "SA_MAIN_ACCOUNT"
		SMG_WINDOW::HSIZE = 70%
		SMG_WINDOW::VSIZE = 8%
		SMG_WINDOW::HPOS  = 4%
		SMG_WINDOW::VPOS  = 11%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 2%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Account"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 70%
		SMG_WINDOW::VVIEW = 8%
		SMG_WINDOW::VHPOS = 4%
		SMG_WINDOW::VVPOS = 11%

		!
		! Tax type
		!
		ACCTITLE$ = "Group   Description"
		ACCTYPE$(0%) = "3"
		ACCTYPE$(1%) = "COMM  Commission"
		ACCTYPE$(2%) = "COS   Cost of Sale"
		ACCTYPE$(3%) = "SALE  Sale"

		!
		! Read Defaults
		!

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF SB_ACCOUNT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if it was
			! that way from the last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF SB_ACCOUNT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			SA_MAIN_ACCOUNT = ERR
			CONTINUE 770
		END WHEN

		SB_ACCOUNT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
		USE
			SA_MAIN_ACCOUNT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		SB_ACCOUNT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(SB_ACCOUNT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = SB_ACCOUNT.CH%
		WHEN ERROR IN
			RESET #SB_ACCOUNT.CH%
			GET #SB_ACCOUNT.CH%, REGARDLESS
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

		DATA	03,04, "(01) GL Account Number", &
			04,04, "(02) Account Group", &
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
	!	^*(01) Account Number\*
	!	.p
	!	The General Ledger ^*Account Number\* field
	!	enters a number which
	!	will reference the system to a General Ledger Account.
	!	.p
	!	The field will accommodate eighteen (18) alphanumeric
	!	characters.
	!	The account can be wildcarded with "?" in place of
	!	wildcard numbers
	!
	! Index:
	!
	!--

			SB_ACCOUNT::ACCOUNT = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"3;27",TEMP$, SB_ACCOUNT::ACCOUNT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%
				THEN
					SB_ACCOUNT::ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF


			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(GL_MAIN_CHART.ID, "M")
				SB_ACCOUNT::ACCOUNT = GL_CHART::ACCT
				GOTO ReEnter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Account Group\*
	!	.p
	!	The General Ledger ^*Account Group\* field classifies the
	!	accounts within broad categories of groups.  Examples of groups include
	!	Commission and Expenses.
	!
	! Index:
	!
	!--
			SB_ACCOUNT::ACCTGROUP = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;27", TEMP$, &
				SB_ACCOUNT::ACCTGROUP, MFLAG, "'E", MVALUE, &
				ACCTYPE$(), ACCTITLE$, "008"), -1%)


		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		SA_MAIN_ACCOUNT = 0%

		SELECT MLOOP

		CASE 1%
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, KEY #0% EQ &
						SB_ACCOUNT::SYSTEM + &
						SB_ACCOUNT::ACCOUNT, REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				SA_MAIN_ACCOUNT = 2%
				CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)
			END IF

			SA_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
				SB_ACCOUNT::ACCOUNT, &
				GL_CHART::ACCT, &
				"SB", MLOOP, "PROG", &
				"Account ", GL_MAIN_CHART.ID)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			! Display description for GL Account if it is Defined
			! If it has a wildcard '?' in it display mask overlay

			IF INSTR(1%, SB_ACCOUNT::ACCOUNT, "?")
			THEN
				GL_CHART::DESCR = "Account Mask Overlay"
			ELSE
				GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), &
					A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
					SB_ACCOUNT::ACCOUNT) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 3%, 46%, , SMG$M_BOLD)
		END IF

	!
	! Set SB_ACCOUNT_OLD value
	!
20500	CASE OPT_SETOLD
		SB_ACCOUNT_OLD = SB_ACCOUNT

	!
	! Restore SB_ACCOUNT_OLD value
	!
	CASE OPT_RESETOLD
		SB_ACCOUNT = SB_ACCOUNT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		SB_ACCOUNT_DEF = SB_ACCOUNT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		SB_ACCOUNT = SB_ACCOUNT_DEF
		SB_ACCOUNT::SYSTEM = SYSTEM$

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE ="  Account             Description" + &
				"                               Group"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "021,063"

		!
		! Convert current record into text
		!
		CASE 3%
			IF INSTR(1%, SB_ACCOUNT::ACCOUNT, "?")
			THEN
				GL_CHART::DESCR = "Account Mask Overlay"
			ELSE
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
					SB_ACCOUNT::ACCOUNT) <> 1%
			END IF

			MVALUE = SB_ACCOUNT::ACCOUNT + "  " + &
				GL_CHART::DESCR      + "  " + &
				SB_ACCOUNT::ACCTGROUP

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE SYSTEM$ + &
				SB_ACCOUNT::ACCOUNT, REGARDLESS

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
				FIND #SMG_WINDOW::CHAN, KEY #0% EQ SYSTEM$, REGARDLESS

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
						KEY #0% GE SYSTEM$ + &
						SB_ACCOUNT::ACCOUNT, REGARDLESS
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
				IF SB_ACCOUNT::SYSTEM = SYSTEM$


		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!*********************************************************************
	! Trap errors
	!*********************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
