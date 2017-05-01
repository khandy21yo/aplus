1	%TITLE "CHART OF ACCOUNTS MAINTENANCE"
	%SBTTL "GL_MAIN_CHART"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_CHART(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:1002
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Chart of Accounts Maintenance\* routine is used to
	!	enter and maintain the Chart of Accounts.
	!	.b
	!	In addition to account number and account description
	!	information, codes are entered which will accommodate the
	!	desired presentation in the financial statements.
	!	.lm -5
	!
	! Index:
	!	.x Chart of Accounts>Maintenance
	!	.x Maintain>Chart of Accounts
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_CHART/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_MAIN_CHART
	!	$ DELETE GL_MAIN_CHART.OBJ;*
	!
	! Author:
	!
	!	02/17/89 - B. Craig Larsen
	!
	! Modification history:
	!
	!	12/10/90 - Kevin Handy
	!		Fixed numerous references to "GL_MAIN_CHART" to read
	!		"GL_MAIN_CHART" (because of Frank's name change
	!		which he didn't dare list anywhere).
	!
	!	12/26/90 - Kevin Handy
	!		Fix View option so it doesn't chop off the first
	!		character of some fields.
	!
	!	01/02/91 - Kevin Handy
	!		Fixed bug where history was not being blanked out
	!		on an add.
	!
	!	06/01/92 - Frank F. Starman
	!		Allow summary flag only 1,2, or 3. Display warnings
	!		if flag is 2 or 3.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/10/95 - Kevin Handy
	!		Lose references to GL_CHARTEX file.
	!
	!	11/20/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/02/96 - Kevin Handy
	!		Remove lots of commented out code.
	!		Lost reference to UTL_SYSTEM and GL_CATEGORY,
	!		which were never used.
	!		Reformat source code.
	!		Merge in code from GL_MAIN_CHART so there is only
	!		one version of this module.
	!
	!	07/03/96 - Kevin Handy
	!		Moved GL_MAIN_CHARTEX into this GL_MAIN_CHART.
	!
	!	08/09/96 - Kevin Handy
	!		Removed error trap for 890, which doesn't exist.
	!
	!	11/20/96 - Kevin Handy
	!		Lose excess number of 'page' directives.
	!
	!	05/10/97 - Kevin Handy
	!		Reformat source code
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	09/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%PAGE

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART
	MAP	(GL_CHART_OLD) GL_CHART_CDD	GL_CHART_OLD, GL_CHART2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%

	COM (TT_GL_CHART) &
		ACCTITLE$ = 20%, &
		ACCTYPE$(6%) = 20%, &
		SUMTITLE$ = 20%, &
		SUMMARY$(5%) = 20%

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	SELECT MOPTION

	!***********************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!***********************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Chart of Accounts Maintenance"
		SMG_WINDOW::NHELP = "GL_MAIN_CHART"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 7%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 4%
		SMG_WINDOW::KNAME(0%) = "Account"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Cashflow"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 5%
		SMG_WINDOW::KNAME(2%) = "Workcapital"
			SMG_WINDOW::KFIELD(2%, 0%) = 1%
			SMG_WINDOW::KFIELD(2%, 1%) = 6%
		SMG_WINDOW::KNAME(3%) = "Balcode"
			SMG_WINDOW::KFIELD(3%, 0%) = 1%
			SMG_WINDOW::KFIELD(3%, 1%) = 7%

		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

		!
		! Define account type
		!
		ACCTITLE$ = "Type   Description"
		ACCTYPE$(0%) = "6"
		ACCTYPE$(1%) = "A    Assets"
		ACCTYPE$(2%) = "L    Liability"
		ACCTYPE$(3%) = "O    Owners Equity"
		ACCTYPE$(4%) = "S    Income Summary"
		ACCTYPE$(5%) = "R    Revenue"
		ACCTYPE$(6%) = "E    Expense"

		!
		! Define summary flag
		!
		SUMTITLE$ = "Code   Description"
		SUMMARY$(0%) = "4"
		SUMMARY$(1%) = "1    Full detail"
		SUMMARY$(2%) = "2    By date"
		SUMMARY$(3%) = "3    One line"
		SUMMARY$(4%) = "4    By batch"

700		!
		! Declare channels
		!
		IF GL_CHART.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_CHART.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_CHART = ERR
			CONTINUE 770
		END WHEN

		GL_CHART.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
		USE
			GL_MAIN_CHART = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_CHART.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_CHART.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = GL_CHART.CH%
		WHEN ERROR IN
			RESET #GL_CHART.CH%
			GET #GL_CHART.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

20100	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	5, 10, "(01) Account", &
			6, 10, "(02) Description", &
			7, 10, "(03) Type", &
			8, 10, "(04) Summary Flag", &
			11, 10, "(05) Cash Flow", &
			12, 10, "(06) Work Capital", &
			13, 10, "(07) Bal/inc Code", &
			10, 10, "** Financial Report Codes **", &
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


20200	!
	! Enter/Display/Default
	!
	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	.x Account
	!	^*(01) Account\*
	!	.b
	!	.lm +5
	!	The complete descriptive name of the ^*Account\* field is General Ledger
	!	Chart of Account Number, though it is sometimes referred to as
	!	General Ledger Number, Account Number, or Account.  This field
	!	cannot be set to null. A value must be entered when a record
	!	is being added.
	!	.b
	!	^*Note:  After an account number has been added
	!	and transactions have been posted to the account, this field cannot
	!	and should not be changed.\*
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_CHART::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;38", TEMP$, &
				GL_CHART::ACCT, MFLAG, "'E", MVALUE)


		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Description
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field contains a description of the account.
	!	.b
	!	A dash or minus sign is used in the description as notification to the
	!	financial report writer. When the consolidation feature is being
	!	used, the description after that point will be truncated. The
	!	reason is to allow the user to key in the department or location
	!	after the dash and not have it appear on the consolidated financial.
	!	.b
	!	Forty spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_CHART::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;38", TEMP$, &
				GL_CHART::DESCR, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Type>Chart Of Accounts Maintenance
	!	^*(03) Type\*
	!	.b
	!	.lm +5
	!	The ^*Type\* field enters a code which defines the
	!	^*Type\* of account being added. Valid ^*Type\* codes are listed below:
	!	.table 3,25
	!	.te
	!	^*A\* - Assets (Real Account)
	!	.te
	!	^*L\* - Liability (Real Account)
	!	.te
	!	^*O\* - Owners Equity (Real Account)
	!	.te
	!	^*S\* - Income Summary (Real Account)
	!	.te
	!	^*R\* - Revenue (Nominal Account)
	!	.te
	!	^*E\* - Expense (Nominal Account)
	!	.end table
	!	Valid Type codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Chart of Accounts Maintenance>Type
	!
	!--

			GL_CHART::ACCTYPE = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;38", TEMP$, GL_CHART::ACCTYPE, &
				MFLAG, "'", MVALUE, ACCTYPE$(), &
				ACCTITLE$, "005")

		CASE 4%

	!++
	! Abstract:FLD004
	!	.x Summary Flag
	!	^*(04) Summary Flag\*
	!	.b
	!	.lm +5
	!	The ^*Summary Flag\* field is provided to control the level of detail to be
	!	printed in the General Ledger report. Valid flags are defined as follows:
	!	.table 3,25
	!	.te
	!	^*1\* - Full Detail
	!	.te
	!	^*2\* - By Date
	!	.te
	!	^*3\* - By Batch
	!	.end table
	!	Valid Summary Flags may be viewed by pressing ^*<List Choices>\* while the
	!	cursor is located at this field.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_CHART::SUMMARY = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;38", TEMP$, GL_CHART::SUMMARY, &
				MFLAG, "'", MVALUE, SUMMARY$(), &
				SUMTITLE$, "005")

		CASE 5%

	!++
	! Abstract:FLD005
	!	.x Cash Flow
	!	^*(05) Cash Flow\*
	!	.b
	!	.lm +5
	!	The ^*Cash Flow\* field defines how each account will be
	!	presented in the Cash flow statement.  It is a
	!	secondary key, duplicates are allowed, and it can have a null value.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_CHART::FLOW = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;38", TEMP$, &
				GL_CHART::FLOW, MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	.x Working Capital
	!	^*(06) Work Capital\*
	!	.b
	!	.lm +5
	!	The ^*Work Capital\* field is provided to define how each account will
	!	be presented in the statement of working capital.  It is a secondary
	!	key, duplicates are allowed, and it can have a null value.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_CHART::WORK = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;38", TEMP$, &
				GL_CHART::WORK, MFLAG, "'E", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	.x Balance Sheet/Income Statement Code
	!	^*(07) Balance Sheet/Income Statement Code\*
	!	.b
	!	.lm +5
	!	The ^*Balance Sheet/Income Statement Code\* field
	!	defines how each account will be presented in the financial statements.
	!	It is a secondary key, duplicates are allowed, and it can have a null
	!	value.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_CHART::FINTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"13;38", TEMP$, &
				GL_CHART::FINTYPE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$


20300	!***********************************************************
	! Test values
	!***********************************************************
	CASE OPT_TESTENTRY
		GL_MAIN_CHART = 0%

		SELECT MLOOP

		CASE 1%
			IF GL_CHART::ACCT = ""
			THEN
				GL_MAIN_CHART = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #GL_CHART.CH%, &
							KEY #0% EQ GL_CHART::ACCT + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					GL_MAIN_CHART = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				END IF
			END IF

		CASE 4%
			SELECT GL_CHART::SUMMARY
			CASE "2", "3"
				CALL HELP_34MESSAGE(SCOPE, &
					"posting summary is on", &
					"W", SCOPE::PRG_PROGRAM, "", "SUMMFLAG")
			END SELECT

		END SELECT


	!***********************************************************
	! Test option
	!***********************************************************
	CASE OPT_TESTOPT
		TESTOPT%, GL_MAIN_CHART = 0%

		IF EDIT$(MVALUE, -1%) = "ERASE"
		THEN
			TESTOPT%, GL_MAIN_CHART = 1% IF &
				GL_CHART::RUNDOL <> 0.0 OR &
				GL_CHART::RUNUNIT <> 0.0 OR &
				GL_CHART::RUNHOUR <> 0.0 OR &
				GL_CHART::CURDOL <> 0.0 OR &
				GL_CHART::CURUNIT <> 0.0 OR &
				GL_CHART::CURHOUR <> 0.0

			FOR LOOP% = 0% TO 20%
				TESTOPT%, GL_MAIN_CHART = 1% IF &
					GL_CHART::DOLLAR(I%) <> 0.0 OR &
					GL_CHART::UNIT(I%) <> 0.0 OR &
					GL_CHART::HOUR(I%) <> 0.0
			NEXT LOOP%

			IF TESTOPT% <> 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Account has a balance.  " + &
					"Erase not allowed", 0%)
			END IF
		END IF


20500	!***********************************************************
	! Set GL_CHART_OLD value
	!***********************************************************
	CASE OPT_SETOLD
		GL_CHART_OLD = GL_CHART


	!***********************************************************
	! Restore GL_CHART_OLD value
	!***********************************************************
	CASE OPT_RESETOLD
		GL_CHART = GL_CHART_OLD


	!***********************************************************
	! Set default value
	!***********************************************************
	CASE OPT_SETDEFAULT
		!
		! Load in defaults for chart
		!
		FOR I% = 0% TO 20%
			GL_CHART::DOLLAR(I%) = 0.0
			GL_CHART::UNIT(I%) = 0.0
			GL_CHART::HOUR(I%) = 0.0
		NEXT I%
		GL_CHART::CPERIOD = 0%
		GL_CHART::RUNDOL = 0.0
		GL_CHART::RUNHOUR = 0.0
		GL_CHART::RUNUNIT = 0.0
		GL_CHART::CURDOL = 0.0
		GL_CHART::CURHOUR = 0.0
		GL_CHART::CURUNIT = 0.0
		GL_CHART::BATCH = ""

		GL_CHART2 = GL_CHART



	!***********************************************************
	! Restore default value
	!***********************************************************
	CASE OPT_RESETDEFAULT
		GL_CHART = GL_CHART2


	!***********************************************************
	! View header
	!***********************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Account            Description                              " + &
				"CashFlow WorkCapital  BalCode"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "021,062,071,084"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = GL_CHART::ACCT + " " + &
				GL_CHART::DESCR + "  " + &
				GL_CHART::FLOW + "     " + &
				GL_CHART::WORK + "        " + &
				GL_CHART::FINTYPE

		END SELECT


	!***********************************************************
	! Find
	!***********************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			!
			! FIND according to Primary Key
			!	(Account number)
			!
			FIND #GL_CHART.CH%, KEY #0% GE GL_CHART::ACCT + "", &
				REGARDLESS

		CASE 1%
			!
			! FIND according to First Alternate Key
			!	(Cash flow)
			!
			FIND #GL_CHART.CH%, KEY #1% GE GL_CHART::FLOW + "", &
				REGARDLESS

		CASE 2%
			!
			! FIND according to Second Alternate Key
			!	(Work capital)
			!
			FIND #GL_CHART.CH%, KEY #2% GE GL_CHART::WORK + "", &
				REGARDLESS

		CASE 3%
			!
			! FIND according to Third Alternate Key
			!	(Financial type)
			!
			FIND #GL_CHART.CH%, KEY #3% GE GL_CHART::FINTYPE + "", &
				REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	!***********************************************************
	! End of GL_MAIN_CHART function
	!***********************************************************
	END FUNCTION
