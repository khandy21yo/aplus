1	%TITLE "State Tax Profile Maintenance"
	%SBTTL "PR_MAIN_TAX_PROFILE_S"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_TAX_PROFILE_S(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	! Abstract:HELP
	!	.p
	!	The ^*State Tax Profile Maintenance\* program maintains the State tax profile
	!	file.
	!
	! Index:
	!	.x State Tax Profile Maintenance
	!	.x Maintenance>State Tax Profile
	!
	! Option:
	!
	!	PR_MAIN_TAX_PROFILE_S$DISTRIBUTION
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_TAX_PROFILE_S/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_MAIN_TAX_PROFILE_S
	!	$ DELETE PR_MAIN_TAX_PROFILE_S.OBJ;*
	!
	! Author:
	!
	!	09/17/87 - Kevin Handy
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	03/24/92 - Kevin Handy
	!		Added "SUTA Id number" in addition to "State ID No".
	!
	!	04/23/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	12/08/92 - Kevin Handy
	!		Modified to allow blank OST Lia Acct and OST Exp
	!		Acct again.
	!
	!	04/14/93 - Kevin Handy
	!		Clean up (check)
	!
	!	07/02/93 - Kevin Handy
	!		Changed title "SUTA ID" to "SUI ID" to be more
	!		consistant in this program.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/26/95 - Kevin Handy
	!		Lose unecessary "IF" around distribution menu item.
	!		If it isn't in the menu then they can't call it.
	!		Lost unecessary externals.
	!		Reformat closer to 80 columns.
	!
	!	10/22/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP (PR_TAX_PROFILE_F)		PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S
	MAP (PR_TAX_PROFILE_S_OLD) PR_TAX_PROFILE_S_CDD PR_TAX_PROFILE_S_OLD, &
		PR_TAX_PROFILE_S2

	!
	! Need to include _F version so that variable length record
	! business will work.
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	MAP (PR_TAX_PROFILE) OH_APPLY_FLAG$ = 1%

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_TAX_PROFILE_F) &
		PR_TAX_PROFILE.CH%, &
		PR_TAX_PROFILE.READONLY%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_TAX_PROFILE_S) RARRAY_RECORD RARRAY(300%)

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "State Tax Profile"
		SMG_WINDOW::NHELP = "PR_MAIN_TAX_PROFILE_S"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 14%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 4%
		SMG_WINDOW::BOTLIN = 18%
		SMG_WINDOW::LINREC = 5%

		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

700		!
		! Declare channels
		!
		IF PR_TAX_PROFILE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_TAX_PROFILE.READONLY%
			GOTO 790
		END IF


750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_TAX_PROFILE_S = ERR
			CONTINUE 770
		END WHEN

		PR_TAX_PROFILE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"
		USE
			PR_MAIN_TAX_PROFILE_S = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_TAX_PROFILE.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_TAX_PROFILE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_TAX_PROFILE.CH%
		WHEN ERROR IN
			RESET #PR_TAX_PROFILE.CH%
			GET #PR_TAX_PROFILE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			" (01)  (02) St ID                                              " + &
			"(08) % (09) Max ", &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"       (03) Sui ID    Liability Acct         " + &
			"Expense Acct       (13) % (14) Max ", &
			2%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			SPACE$(80%), 3%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 5%
			A% = VAL%(MID("006,021,044,063,070", I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)
		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! More options
	!
	CASE OPT_OPTLIST
		IF OH_APPLY_FLAG$ = "D"
		THEN
			MVALUE = MVALUE + " disTribution"
		END IF

	!
	! Handle additional options
	!
	CASE OPT_MOREMENU
		SELECT EDIT$(MVALUE, -1%)

		CASE "DISTRIBUTION"
	!++
	! Abstract:DISTRIBUTION
	!	^*Distribution\*
	!	.p
	!	The ^*Distribution\* option accesses the routine
	!	which will indicate how each individual tax is distributed.
	!
	! Index:
	!	.x Distribution
	!
	!--
			PR_MAIN_TAX_PROFILE_S = &
				MAIN_JOURNAL(PR_MAIN_TAX_PROFILE_FRI.ID, "")

		END SELECT

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)
		XLINE1$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
		XLINE2$ = NUM1$(SMG_WINDOW::CURLIN + 2%)
		XLINE3$ = NUM1$(SMG_WINDOW::CURLIN + 3%)

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) State\*
	!	.p
	!	The ^*State\* field
	!	enters a two (2) character Post Office code designating
	!	the specific State to which the record refers.
	!
	! Index:
	!	.x State Tax Profile>State
	!	.x State>State Tax Profile
	!
	!--

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(04) WH ", &
				SMG_WINDOW::CURLIN, 22%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(05) Min Wage", &
				SMG_WINDOW::CURLIN, 45%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(06) OST", &
				SMG_WINDOW::CURLIN + 1%, 22%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(07) ", &
				SMG_WINDOW::CURLIN + 1%, 45%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"                                    " + &
				"        (10)", &
				SMG_WINDOW::CURLIN + 2%, 22%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(11) SUI", &
				SMG_WINDOW::CURLIN + 3%, 22%, , SMG$M_REVERSE)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(12) ", &
				SMG_WINDOW::CURLIN + 3%, 45%, , SMG$M_REVERSE)

			PR_TAX_PROFILE_S::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_TAX_PROFILE_S::CODE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Tax Id Number\*
	!	.p
	!	The ^*Tax Identification Number\* field
	!	enters the State Withholding
	!	Tax identification number for the specific State to which the
	!	record relates.
	!
	! Index:
	!	.x State Tax Profile>Tax Identification Number
	!	.x Tax Identification Number>State Tax Profile
	!	.x Identification Number>State Tax Profile
	!
	!--

			PR_TAX_PROFILE_S::REPNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";7", TEMP$, &
				PR_TAX_PROFILE_S::REPNO, MFLAG, &
				"'LLLLLLLLLLLLL", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) SUTA Id Number\*
	!	.p
	!	The ^*SUTA Tax Identification Number\* field
	!	enters the SUTA
	!	Tax identification number for the specific State to which the
	!	record relates.
	!
	! Index:
	!	.x State Tax Profile>Tax Identification Number
	!	.x Tax Identification Number>State Tax Profile
	!	.x Identification Number>State Tax Profile
	!
	!--

			PR_TAX_PROFILE_S::SUTANO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE3$ + ";7", TEMP$, &
				PR_TAX_PROFILE_S::SUTANO, MFLAG, &
				"'LLLLLLLLLLLLL", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Liability Account - WH\*
	!	.p
	!	The ^*Liability Account - Withholding\* field
	!	enters the General Ledger account
	!	which will be credited with the amount of State Withholding Taxes
	!	withheld from the employees.
	!	.p
	!	This account can be masked against the account number defined in
	!	the employee master record.
	!
	! Index:
	!	.x State Tax Profile>Withholding Tax Liability Account
	!	.x Withholding Tax Liability Account>State Tax Profile
	!	.x Liability Account>State Tax Profile
	!
	!--

			PR_TAX_PROFILE_S::WH_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";31", TEMP$, &
				PR_TAX_PROFILE_S::WH_ACCT, &
				MFLAG, "'LLLLLLLLLLL", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_S::WH_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Minimum Wage\*
	!	.p
	!	The ^*Minimum Wage\* field
	!	enters the current minimum hourly wage rate for
	!	the State to which the record relates.
	!
	! Index:
	!	.x State Tax Profile>Minimum Wage
	!	.x Minimum Wage>State Tax Profile
	!
	!--

			PR_TAX_PROFILE_S::MIN_WAGE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";58", TEMP$, &
				PR_TAX_PROFILE_S::MIN_WAGE, &
				MFLAG, "##.##", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Liability Account - OST\*
	!	.p
	!	The ^*Liability Account - Other State Taxes\* field
	!	enters the General
	!	Ledger account which will be credited with the amounts withheld from
	!	employees for other State taxes. Other State Taxes generally refers
	!	to State Disability Insurance Taxes.
	!
	! Index:
	!	.x State Tax Profile>Other State Tax>Liability Account
	!	.x Other State Tax>Liability Account>State Tax Profile
	!	.x Liability Account>State Tax Profile
	!
	!--

			PR_TAX_PROFILE_S::OST_LIA_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE1$ + ";31", TEMP$, &
				PR_TAX_PROFILE_S::OST_LIA_ACCT, &
				MFLAG, "'LLLLLLLLLLL", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_S::OST_LIA_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Expense Account - OST\*
	!	.p
	!	The ^*Expense Account - OST\* field
	!	enters the General Ledger account which
	!	will be debited when the Other State Tax expense is recorded.
	!
	! Index:
	!	.x State Tax Profile>Other State Tax>Expense Account
	!	.x Other State Tax>Expense Account>State Tax Profile
	!	.x Expense Account>State Tax Profile
	!
	!--

			PR_TAX_PROFILE_S::OST_EX_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE1$ + ";50", TEMP$, &
				PR_TAX_PROFILE_S::OST_EX_ACCT, &
				MFLAG, "'LLLLLLLLLLL", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_S::OST_EX_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) OST %\*
	!	.p
	!	The ^*OST %\* field
	!	enters the Other State Tax rate for a specific
	!	state. The rate is expressed in the terms of a percent, hence a two and
	!	one-half percent rate is entered as ^*2.5\* rather than ^*.025\*.
	!
	! Index:
	!	.x State Tax Profile>Other State Tax>Percent
	!	.x Other State Tax>Percent>State Tax Profile
	!
	!--

			PR_TAX_PROFILE_S::OST_PCT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE1$ + ";64", TEMP$, &
				PR_TAX_PROFILE_S::OST_PCT, MFLAG, &
				"##.###", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) OST Maximum\*
	!	.p
	!	The ^*OST Maximum\* field
	!	enters the individual employee's earnings maximum after which
	!	Other State Taxes are no longer applicable. This field is a Year to Date
	!	taxable total and if used, then field nine is ignored.
	!
	! Index:
	!	.x State Tax Profile>OST Maximum
	!	.x OST Maximum>State Tax Profile
	!
	!--

			PR_TAX_PROFILE_S::OST_MAX = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE1$ + ";71", TEMP$, &
				PR_TAX_PROFILE_S::OST_MAX, MFLAG, &
				"########", MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) OST Maximum\*
	!	.p
	!	The ^*OST Maximum\* field
	!	enters the individual employee's earnings
	!	maximum after which Other State Taxes are no longer applicable on the Year to
	!	Date earnings.
	!	This field divides the year to date by the number of pay periods to find
	!	the maximum per pay period. If field eight is being used, this field is
	!	ignored.
	!
	! Index:
	!	.x State Tax Profile>State Unemployment Tax>Earnings Maximum
	!	.x State Unemployment Tax>Earnings Maximum>State Tax Profile
	!	.x Earnings Maximum>State Tax Profile
	!
	!--

			PR_TAX_PROFILE_S::OST_DEDMAX = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE2$ + ";71", TEMP$, &
				PR_TAX_PROFILE_S::OST_DEDMAX, MFLAG, &
				"########", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Liability Account-SUI\*
	!	.p
	!	The ^*Liability Account-SUI\* field
	!	enters the General Ledger Account which will be credited
	!	when the State Unemployment Tax is recorded.
	!	.p
	!	This account can be masked against the account number defined in
	!	the employee master record.
	!
	! Index:
	!	.x SUI Liability Account>State Tax Profile
	!	.x Liability Account>State Tax Profile
	!	.x State Tax Profile>SUI Liability Account
	!	.y mask
	!	.y accountmask
	!
	!--
			PR_TAX_PROFILE_S::SUI_LIA_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE3$ + ";31", TEMP$, &
				PR_TAX_PROFILE_S::SUI_LIA_ACCT, &
				MFLAG, "'LLLLLLLLLLL", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_S::SUI_LIA_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Expense Account-SUI\*
	!	.p
	!	The ^*Expense Account-SUI\* field
	!	enters the General Ledger account which will be debited
	!	when the State Unemployment Tax is recorded.
	!	.p
	!	This account can be masked against the account number defined in
	!	the employee master record.
	!
	! Index:
	!	.x State Unemployment Tax Expense Account>State Tax Profile
	!	.x Expense Account>State Tax Profile
	!	.x State Tax Profile>State Unemployment Tax Expense Account
	!	.y mask
	!	.y accountmask
	!
	!--
			PR_TAX_PROFILE_S::SUI_EX_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE3$ + ";50", TEMP$, &
				PR_TAX_PROFILE_S::SUI_EX_ACCT, &
				MFLAG, "'LLLLLLLLLLL", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_S::SUI_EX_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) SUI %\*
	!	.p
	!	The ^*SUI %\* field
	!	enters the State Unemployment Tax Rate for a specific state. The rate is
	!	expressed in terms of a percent, hence a two and one-half percent rate is
	!	entered as ^*2.5\* rather than ^*.025\*.
	!
	! Index:
	!	.x SUI Percent>State Tax Profile
	!	.x State Tax Profile>SUI Percent
	!
	!--
			PR_TAX_PROFILE_S::SUI_PCT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE3$ + ";64", TEMP$, &
				PR_TAX_PROFILE_S::SUI_PCT, MFLAG, &
				"##.###", MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	^*(14) SUI Maximum \*
	!	.p
	!	The ^*SUI Maximum\* field
	!	enters the individual employee's earnings maximum after which State
	!	Unemployment Taxes are no longer applicable.
	!
	! Index:
	!	.x SUI Maximum>State Tax Profile
	!	.x State Tax Profile>SUI Maximum
	!
	!--
			PR_TAX_PROFILE_S::SUI_MAX = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE3$ + ";71", TEMP$, &
				PR_TAX_PROFILE_S::SUI_MAX, MFLAG, &
				"########", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_TAX_PROFILE_S = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Don't allow blank account numbers
			!
			IF PR_TAX_PROFILE_S::CODE = ""
			THEN
				PR_MAIN_TAX_PROFILE_S = 1%
				CALL ENTR_3MESSAGE(SCOPE, "Illegal code", 1%)
				EXIT FUNCTION
			END IF

		CASE 4%
			!
			! Is the input defined?
			!
			PR_MAIN_TAX_PROFILE_S = FUNC_TESTENTRY( SMG_WINDOW, &
				PR_TAX_PROFILE_S::WH_ACCT, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

		CASE 6%
			!
			! Is the input defined?
			!
			IF TRM$(PR_TAX_PROFILE_S::OST_LIA_ACCT) <> ""
			THEN
				PR_MAIN_TAX_PROFILE_S = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					PR_TAX_PROFILE_S::OST_LIA_ACCT, &
					GL_CHART::DESCR, &
					"PR", MLOOP, "PRG", &
					"Account", GL_MAIN_CHART.ID)
			END IF

		CASE 7%
			!
			! Is the input defined?
			!
			IF TRM$(PR_TAX_PROFILE_S::OST_EX_ACCT) <> ""
			THEN
				PR_MAIN_TAX_PROFILE_S = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					PR_TAX_PROFILE_S::OST_EX_ACCT, &
					GL_CHART::DESCR, &
					"PR", MLOOP, "PRG", &
					"Account", GL_MAIN_CHART.ID)
			END IF

		CASE 11%
			!
			! Is the input defined?
			!
			PR_MAIN_TAX_PROFILE_S = FUNC_TESTENTRY( SMG_WINDOW, &
				PR_TAX_PROFILE_S::SUI_LIA_ACCT, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

		CASE 12%
			!
			! Is the input defined?
			!
			PR_MAIN_TAX_PROFILE_S = FUNC_TESTENTRY( SMG_WINDOW, &
				PR_TAX_PROFILE_S::SUI_EX_ACCT, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

		END SELECT

	!
	! Set PR_TAX_PROFILE_S_OLD value
	!
20500	CASE OPT_SETOLD
		PR_TAX_PROFILE_S_OLD = PR_TAX_PROFILE_S

	!
	! Restore PR_TAX_PROFILE_S_OLD value
	!
	CASE OPT_RESETOLD
		PR_TAX_PROFILE_S = PR_TAX_PROFILE_S_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_TAX_PROFILE_S2 = PR_TAX_PROFILE_S

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_TAX_PROFILE_S = PR_TAX_PROFILE_S2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_TAX_PROFILE_S::AUTH = "S"

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_TAX_PROFILE.CH%, &
				KEY #0% GE PR_TAX_PROFILE_S::AUTH + "", &
				REGARDLESS
		END SELECT

	!
	! Handle array of records
	!
27000	CASE OPT_ARRAY

		!
		! Select sub-option of array
		!
		SELECT MLOOP

		!
		! Load array with line items
		!
		CASE 1%

			!
			! Empty array
			!
			SMG_WINDOW::TOTREC = 0%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE "S", &
					REGARDLESS
			USE
				CONTINUE 28000
			END WHEN

27120			!
			! Get a record
			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 28000 IF ERR = 11%
				EXIT HANDLER
			END WHEN

			IF PR_TAX_PROFILE_S::AUTH = "S"
			THEN
				!
				! Add information to array
				!
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				GOTO 27120
			END IF

		!
		! Remove one element of the array
		!
		CASE 2%
			!
			! Remove item pointed to by Mflag
			!
			RARRAY(I%) = RARRAY(I% + 1%) &
				FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%

		!
		! Set array item to current record
		!
		CASE 3%
			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)

		!
		! Load in current record, locked
		!
		CASE 4%
27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		!
		! Load in current record, unlocked
		!
		CASE 5%
			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		!
		! Change the current record's key to match header.  The
		! new key probably passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			PR_TAX_PROFILE_S::AUTH = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
