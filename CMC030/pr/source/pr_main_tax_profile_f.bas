1	%TITLE "PR Tax Profile Maintenance"
	%SBTTL "PR_MAIN_TAX_PROFILE_F"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_TAX_PROFILE_F(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	! Abstract:HELP
	!	.p
	!	The ^*Payroll Tax and GL Profile\* option
	!	maintains tables which assign General Ledger expense and
	!	liability account numbers to which the various Federal, State, City
	!	and Local Taxes will be posted.
	!	.p
	!	In addition, other data maintained in the Payroll Tax Profile
	!	files includes:
	!	.LIST "*"
	!	.LE
	!	Tax Identification Numbers
	!	.LE
	!	Unemployment Tax Rates
	!	.LE
	!	Unemployment Tax Maximum Earnings Limits
	!	.LE
	!	General Ledger Payroll Cash Account
	!	.LE
	!	Accrued Payroll Account
	!	.LE
	!	Minimum Hourly Wage Rates
	!	.ELS
	!
	! Index:
	!	.x Payroll Tax>Profile
	!
	! Option:
	!
	!	PR_MAIN_TAX_PROFILE_F$DISTRIBUTION
	!	PR_MAIN_TAX_PROFILE_F$STATE
	!	PR_MAIN_TAX_PROFILE_F$CITY
	!	PR_MAIN_TAX_PROFILE_F$COUNTY
	!	PR_MAIN_TAX_PROFILE_F$SCHOOL
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_TAX_PROFILE_F/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_MAIN_TAX_PROFILE_F
	!	$ DELETE PR_MAIN_TAX_PROFILE_F.OBJ;*
	!
	! Author:
	!
	!	09/16/87 - Kevin Handy
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	07/06/89 - Kevin Handy
	!		Modified so menu doesn't give update options
	!		when no write access.
	!
	!	04/23/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/20/2004 - Kevin Handy
	!		Added direct deposit field.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F
	MAP (PR_TAX_PROFILE_F2) PR_TAX_PROFILE_F_CDD PR_TAX_PROFILE_F_OLD, &
		PR_TAX_PROFILE_F2

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

	MAP (PR_TAX_PROFILE) OH_APPLY_FLAG$ = 1%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION MAIN_JOURNAL
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Payroll Tax Profile Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_TAX_PROFILE_F"
		SMG_WINDOW::CHAN  = PR_TAX_PROFILE.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 13%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

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

		CALL READ_DEFAULTS(SMG_WINDOW)

750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_TAX_PROFILE_F = ERR
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
			PR_MAIN_TAX_PROFILE_F = ERR
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
			GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "F  ", REGARDLESS
		USE
			CONTINUE 20045 IF ERR = 11% OR ERR = 155%
			EXIT HANDLER
		END WHEN

		EXIT FUNCTION

20045		!
		! Create a new record
		!
		PR_TAX_PROFILE_F::AUTH	= "F"
		PR_TAX_PROFILE_F::CODE	= "  "

		PUT #PR_TAX_PROFILE.CH%

	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1,  1, "(01) Tax Id #", &
			2,  1, "(02) FED Liability Ac", &
			3,  1, "(03) FICA Expense Ac", &
			4,  1, "(04) FICA Lia Ac Empr", &
			5,  1, "(05) FICA Lia Ac Empe", &
			6,  1, "(06) FUI Expense Ac", &
			7,  1, "(07) FUI Liability Ac", &
			8,  1, "(08) FUI %", &
			9,  1, "(09) FUI Maximum", &
			10,  1, "(10) Cash Account", &
			11,  1, "(11) Accrued Payroll Ac", &
			12,  1, "(12) Minimum Wage", &
			13,  1, "(13) Direct Deposit ID", &
			0,  0, ""

		RESTORE

		READ XPOS, YPOS, XSTR$
		I% = 0%
		WHILE (XPOS <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS, YPOS) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS, YPOS, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! More options
	!
	CASE OPT_OPTLIST
		IF PR_TAX_PROFILE.READONLY%
		THEN
			IF OH_APPLY_FLAG$ = "D"
			THEN
				MVALUE = "Help eXit disTribution State cIty cOunty schooL"
			ELSE
				MVALUE = "Help eXit State cIty cOunty schooL"
			END IF
		ELSE
			IF OH_APPLY_FLAG$ = "D"
			THEN
				MVALUE = "Change Blank Help eXit disTribution State cIty cOunty schooL"
			ELSE
				MVALUE = "Change Blank Help eXit State cIty cOunty schooL"
			END IF
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
	!	.b
	!	The Distribution table is only used when the
	!	"Overhead Apply Flag" in the control file
	!	is set to "D" (Distribute by Department).
	!
	! Index:
	!	.x Distribution>Profile
	!	.x Profile>Distribution
	!
	!--
			IF OH_APPLY_FLAG$ = "D"
			THEN
				PR_MAIN_TAX_PROFILE_F = &
					MAIN_JOURNAL(PR_MAIN_TAX_PROFILE_FRI.ID, "")
			END IF

		CASE "STATE"
	!++
	! Abstract:STATE
	!	^*State\*
	!	.p
	!	The ^*State\* function
	!	accesses the State Payroll Tax Profile file.
	!	.p
	!	The State Payroll Tax Profile file
	!	maintains the following information for each applicable State:
	!	.b
	!	.LIST 0,"*"
	!	.LE
	!	State Identification Code
	!	.LE
	!	State Withholding Tax Identification Number
	!	.LE
	!	State Withholding Tax Liability Account
	!	.LE
	!	State Minimum Hourly Wage
	!	.LE
	!	Other State Tax Liability Account
	!	.LE
	!	State Unemployment Tax Liability Account
	!	.LE
	!	State Unemployment Tax Expense Account
	!	.LE
	!	State Unemployment Tax Rate
	!	.LE
	!	State Unemployment Maximum Earnings Limit
	!	.ELS
	!
	! Index:
	!	.x State Payroll Tax Profile
	!	.x State>Function
	!
	!--
			PR_MAIN_TAX_PROFILE_F = &
				MAIN_JOURNAL(PR_MAIN_TAX_PROFILE_S.ID, "")

		CASE "CITY"
	!++
	! Abstract:CITY
	!	^*City\*
	!	.p
	!	The ^*City\* function
	!	accesses the City Payroll Tax Profile file.
	!	.p
	!	The City Payroll Tax Profile file
	!	maintains the following information for each applicable city:
	!	.b
	!	.LIST 0,"*"
	!	.LE
	!	City Identification Code
	!	.LE
	!	City Withholding Tax Identification Number
	!	.LE
	!	City Withholding Tax Liability Account
	!	.ELS
	!
	! Index:
	!	.x Payroll Tax Profile>City
	!	.x City>Payroll Tax Profile
	!
	!--
			PR_MAIN_TAX_PROFILE_F = &
				MAIN_JOURNAL(PR_MAIN_TAX_PROFILE_C.ID, "")

		CASE "COUNTY"
	!++
	! Abstract:COUNTY
	!	^*County\*
	!	.p
	!	the ^*County\* function
	!	accesses the County Payroll Tax Profile file.
	!	.p
	!	The County Payroll Tax Profile file
	!	maintains the following information for each applicable county:
	!	.b
	!	.list 0,"*"
	!	.le
	!	County Identification Code
	!	.le
	!	County Withholding Identification Number
	!	.le
	!	County Withholding Tax Liability Account
	!	.els
	!
	! Index:
	!	.x Payroll Tax Profile>County
	!	.x County>Payroll Tax Profile
	!
	!--
			PR_MAIN_TAX_PROFILE_F = &
				MAIN_JOURNAL(PR_MAIN_TAX_PROFILE_D.ID, "")

		CASE "SCHOOL"
	!++
	! Abstract:SCHOOL
	!	^*School\*
	!	.p
	!	The ^*School\* function
	!	accesses the School District Payroll Tax Profile
	!	file.
	!	.p
	!	The School District Payroll Tax Profile file
	!	maintains the following information for each applicable
	!	School District:
	!	.b
	!	.LIST 0,"*"
	!	.LE
	!	School District Identification Number
	!	.LE
	!	School District Withholding Tax Identification Number
	!	.LE
	!	School District Withholding Tax Liability Account
	!	.ELS
	!
	! Index:
	!	.x Payroll Tax Profile>School Withholding Tax
	!	.x School Withholding Tax>Payroll Tax Profile
	!
	!--
			PR_MAIN_TAX_PROFILE_F = &
				MAIN_JOURNAL(PR_MAIN_TAX_PROFILE_E.ID, "")

		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT


	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SCOPE::SCOPE_EXIT = 0%

 E0Loop:	SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Tax Identification _#\*
	!	.p
	!	The ^*Tax Identification _#\* field records the appropriate
	!	Tax Identification Number. The field will accommodate up to twenty
	!	(20) alphanumeric characters.
	!
	! Index:
	!	.x Tax Identification>Tax Profile
	!	.x Tax Profile>Tax Identification
	!
	!--

			PR_TAX_PROFILE_F::REPNO  = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;25", TEMP$, &
				PR_TAX_PROFILE_F::REPNO, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Federal Liability Account\*
	!	.p
	!	The ^*Federal Liability Account\* field
	!	records the General Ledger account number to
	!	which the taxes withheld from employees will be credited.
	!	.p
	!	This account can be masked against the account number defined in
	!	the employee master record.
	!
	! Index:
	!	.x Tax Profile>Federal Liability Account
	!	.x Federal Liability Account>Tax Profile
	!	.x Liability Account>Tax Profile
	!	.y mask
	!	.y accountmask
	!
	!--

			PR_TAX_PROFILE_F::WH_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;25", TEMP$, &
				PR_TAX_PROFILE_F::WH_ACCT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_F::WH_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) FICA Employer Expense Account\*
	!	.p
	!	The ^*FICA Employer Expense Account\* field
	!	enters the General Ledger account which
	!	will be debited with the employer's portion of FICA taxes.
	!	.p
	!	This account can be masked against the account number defined in
	!	the employee master record.
	!
	! Index:
	!	.x Tax Profile>FICA Employer Expense Account
	!	.x FICA Employer Expense Account>Tax Profile
	!	.x Expense Account>Tax Profile
	!	.y mask
	!	.y accountmask
	!
	!--

			PR_TAX_PROFILE_F::FICA_EX_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;25", TEMP$, &
				PR_TAX_PROFILE_F::FICA_EX_ACCT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_F::FICA_EX_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) FICA Employer Liabililty Account\*
	!	.p
	!	The ^*FICA Employer Liability Account\* field
	!	enters the General Ledger account which
	!	will be credited with the employer's portion of FICA tax liability.
	!	.p
	!	This account can be masked against the account number defined in
	!	the employee master record.
	!
	! Index:
	!	.x Tax Profile>FICA Employer Liability Account
	!	.x FICA Employer Liability Account>Tax Profile
	!	.x Liability Account>Tax Profile
	!	.y mask
	!	.y accountmask
	!
	!--

			PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPR = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;25", TEMP$, &
				PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPR, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPR = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) FICA Employee Liabililty Account\*
	!	.p
	!	The ^*FICA Employee Liability Account\* field
	!	enters the General Ledger account which
	!	will be credited with the amounts of FICA tax withheld from employees.
	!	.p
	!	This account can be masked against the account number defined in
	!	the employee master record.
	!
	! Index:
	!	.x Tax Profile>FICA Employee Liability Account
	!	.x FICA Employee Liability Account>Tax Profile
	!	.x Liability Account>Tax Profile
	!	.y mask
	!	.y accountmask
	!
	!--

			PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;25", TEMP$, &
				PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPE, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPE = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) FUI Expense Account\*
	!	.p
	!	The ^*FUI Expense Account\* field
	!	enters the General Ledger account which will
	!	be debited when Federal Unemployment Tax is recorded.
	!	.p
	!	This account can be masked against the account number defined in
	!	the employee master record.
	!
	! Index:
	!	.x Tax Profile>Federal Unemployment Tax Expense
	!	.x Federal Unemployment Tax>Expense Account>Tax Profile
	!	.x Expense Account>Tax Profile
	!	.y mask
	!	.y accountmask
	!
	!--

			PR_TAX_PROFILE_F::FUI_EX_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;25", TEMP$, &
				PR_TAX_PROFILE_F::FUI_EX_ACCT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_F::FUI_EX_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) FUI Liability Account\*
	!	.p
	!	The ^*FUI Liability Account\* field
	!	enters the General Ledger account which will be
	!	credited when Federal Unemployment Tax liability is recorded.
	!	.p
	!	This account can be masked against the account number defined in
	!	the employee master record.
	!
	! Index:
	!	.x Tax Profile>Federal Unemployment Tax Liability Account
	!	.x Federal Unemployment Tax Liability Account>Tax Profile
	!	.x Liability Account>Tax Profile
	!	.y Mask
	!	.y accountmask
	!
	!--

			PR_TAX_PROFILE_F::FUI_LIA_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;25", TEMP$, &
				PR_TAX_PROFILE_F::FUI_LIA_ACCT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_F::FUI_LIA_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) FUI Percent\*
	!	.p
	!	The ^*FUI Percent\* field
	!	enters the applicable rate for Federal Unemployment Taxes
	!	for a particular calendar year. The rate is expressed as a percentage,
	!	hence 8/10's of one percent must be entered as ^*.8\*, rather than as
	!	^*.008\*.
	!
	! Index:
	!	.x Tax Profile>Federal Unemployment Tax Percent
	!	.x Federal Unemployment Tax Percent>Tax Profile
	!
	!--

			PR_TAX_PROFILE_F::FUI_PCT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;25", TEMP$, &
				PR_TAX_PROFILE_F::FUI_PCT, MFLAG, &
				"##.###", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) FUI Maximum\*
	!	.p
	!	The ^*FUI Maximum\* field
	!	enters the individual employee's earnings limit after which
	!	Federal Unemployment Tax liability is not applicable.
	!
	! Index:
	!	.x Tax Profile>Federal Unemployment Tax Maximum
	!	.x Federal Unemployment Tax Maximum>Tax Profile
	!
	!--

			PR_TAX_PROFILE_F::FUI_MAX = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;25", TEMP$, &
				PR_TAX_PROFILE_F::FUI_MAX, MFLAG, &
				"###,###.##", MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Cash Account\*
	!	.p
	!	The ^*Cash Account\* field
	!	enters the specific General Ledger cash account which
	!	will be credited when payroll checks are written.
	!	.p
	!	This account is not masked.
	!
	! Index:
	!	.x Tax Profile>Cash Account
	!	.x Cash Account>Tax Profile
	!
	!--

			PR_TAX_PROFILE_F::CASH_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;25", TEMP$, &
				PR_TAX_PROFILE_F::CASH_ACCT, MFLAG, &
				"'E", MVALUE)


			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_F::CASH_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 11%

	!++
	! Abstract:FLD011
	!	^*(11) Accrued PR Account\*
	!	.p
	!	The ^*Accrued Payroll Account\* field
	!	enters the General Ledger account which will be
	!	credited when any accrued payroll expenses are posted to the General
	!	Ledger.
	!	.p
	!	This account can be masked against the account number defined in
	!	the employee master record.
	!
	! Index:
	!	.x Tax Profile>Accrued Payroll Account
	!	.x Accrued Payroll Account>Tax Profile
	!	.y mask
	!	.y accountmask
	!
	!--

			PR_TAX_PROFILE_F::PR_ACCRUAL_ACCT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;25", TEMP$, &
				PR_TAX_PROFILE_F::PR_ACCRUAL_ACCT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TAX_PROFILE_F::PR_ACCRUAL_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 12%

	!++
	! Abstract:FLD012
	!	^*(12) Minimum Wage\*
	!	.p
	!	The ^*Minimum Wage\* field
	!	enters a current minimum hourly wage rate.
	!
	! Index:
	!	.x Tax Profile>Minimum Wage
	!	.x Minimum Wage>Tax Profile
	!
	!--

			PR_TAX_PROFILE_F::MIN_WAGE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;25", TEMP$, &
				PR_TAX_PROFILE_F::MIN_WAGE, MFLAG, &
				"##.##", MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	^*(13) Direct Deposit ID\*
	!	.p
	!	This field records the appropriate
	!	Tax Identification Number for Direct Deposit purposes.
	!	The field will accommodate up to twenty
	!	(20) alphanumeric characters.
	!
	! Index:
	!	.x Direct Deposit>Tax Profile
	!	.x Tax Profile>Direct Deposit
	!
	!--

			PR_TAX_PROFILE_F::DIRECT  = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;25", TEMP$, &
				PR_TAX_PROFILE_F::DIRECT, MFLAG, "'E", MVALUE)



		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	CASE OPT_TESTENTRY

		PR_MAIN_TAX_PROFILE_F = 0%

		SELECT MLOOP

		CASE 2%

			TEMP$ = PR_TAX_PROFILE_F::WH_ACCT
			TEMPY% = 2%

		CASE 3%

			TEMP$ = PR_TAX_PROFILE_F::FICA_EX_ACCT
			TEMPY% = 3%

		CASE 4%

			TEMP$ = PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPR
			TEMPY% = 4%

		CASE 5%

			TEMP$ = PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPE
			TEMPY% = 5%

		CASE 6%

			TEMP$ = PR_TAX_PROFILE_F::FUI_EX_ACCT
			TEMPY% = 6%

		CASE 7%

			TEMP$ = PR_TAX_PROFILE_F::FUI_LIA_ACCT
			TEMPY% = 7%

		CASE 10%

			TEMP$ = PR_TAX_PROFILE_F::CASH_ACCT
			TEMPY% = 10%

		CASE 11%

			TEMP$ = PR_TAX_PROFILE_F::PR_ACCRUAL_ACCT
			TEMPY% = 11%

		CASE ELSE
			EXIT FUNCTION

		END SELECT

		TEMPX% = 45%

		IF TEMP$ <> "" AND INSTR(1%, TEMP$, "?") = 0%
		THEN
			PR_MAIN_TAX_PROFILE_F = FUNC_TESTENTRY( SMG_WINDOW, &
				TEMP$, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(GL_CHART::DESCR, 20%), &
			TEMPY%, TEMPX%, , SMG$M_BOLD)


20500	CASE OPT_SETOLD
		PR_TAX_PROFILE_F_OLD = PR_TAX_PROFILE_F

	CASE OPT_RESETOLD
		PR_TAX_PROFILE_F = PR_TAX_PROFILE_F_OLD

	CASE OPT_DISPLAY

		FOR TEMP% = 1% TO 12%

			SELECT TEMP%

			CASE 2%

				TEMP$ = PR_TAX_PROFILE_F::WH_ACCT
				TEMPY% = 2%

			CASE 3%

				TEMP$ = PR_TAX_PROFILE_F::FICA_EX_ACCT
				TEMPY% = 3%

			CASE 4%

				TEMP$ = PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPR
				TEMPY% = 4%

			CASE 5%

				TEMP$ = PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPE
				TEMPY% = 5%

			CASE 6%

				TEMP$ = PR_TAX_PROFILE_F::FUI_EX_ACCT
				TEMPY% = 6%

			CASE 7%

				TEMP$ = PR_TAX_PROFILE_F::FUI_LIA_ACCT
				TEMPY% = 7%

			CASE 10%

				TEMP$ = PR_TAX_PROFILE_F::CASH_ACCT
				TEMPY% = 10%

			CASE 11%

				TEMP$ = PR_TAX_PROFILE_F::PR_ACCRUAL_ACCT
				TEMPY% = 11%

			CASE ELSE
				GOTO DisplaySkip

			END SELECT

			TEMPX% = 45%

			IF (SMG_WINDOW::HFLAG(TEMPY%) AND 2%) = 0%
			THEN
				GL_CHART::DESCR = ""

				GL_CHART::DESCR = "Acct Overlay Mask" &
					IF INSTR(1%, TEMP$, "?")

				IF TEMP$ <> "" AND INSTR(1%, TEMP$, "?") = 0%
				THEN
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
						"Q0" + TEMP$) <> 1%
					THEN
						GL_CHART::DESCR = &
							STRING$(LEN(GL_CHART::DESCR), 63%)
					END IF
				END IF

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT(GL_CHART::DESCR, 20%), &
					TEMPY%, TEMPX%, , SMG$M_BOLD)
			END IF

 DisplaySkip:
		NEXT TEMP%


	CASE OPT_SETDEFAULT
		PR_TAX_PROFILE_F2 = PR_TAX_PROFILE_F

	CASE OPT_RESETDEFAULT
		PR_TAX_PROFILE_F = PR_TAX_PROFILE_F2

	END SELECT

	EXIT FUNCTION

29000	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
