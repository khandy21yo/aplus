1	%TITLE "PR Workmans Comp Description Maintenance"
	%SBTTL "PR_MAIN_WC_DESCR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_WC_DESCR(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Maintain Workmen's Compensation Codes\* option
	!	accesses the Workmen's Compensation
	!	Table file where a record is maintained for each Workmen's
	!	Compensation code.
	!	.p
	!	The following data is maintained in each record in the file:
	!	.LIST "*"
	!	.LE
	!	Workmen's Compensation Code.
	!	.LE
	!	Code Description.
	!	.LE
	!	General Ledger account to which liability
	!	in reference to a specific Code will be
	!	credited.
	!	.LE
	!	General Ledger account(s) to which insurance
	!	liability calculations will be charged.
	!	.LE
	!	State codes for each State where insurance
	!	liability could feasibly exist relative to
	!	a specific Code.
	!	.LE
	!	Effective dates for each premium rate or
	!	combination of rates.
	!	.LE
	!	Insurance premium rates for Workmen's
	!	Compensation, Property Liability and
	!	Personal Injury Liability insurances.
	!	.LE
	!	Flags to indicate whether overtime premium
	!	labor costs are subject to Workmen's
	!	Compensation, Property Liability or
	!	Personal Injury Liability insurance
	!	premium calculations in reference to each
	!	Code.
	!	.ELS
	!	.p
	!	There must be one or more General Ledger subject labor accounts
	!	which relate to the specific Workmen's Compensation Code. These are
	!	identified in the definition section of the Workmen's Compensation
	!	Table record.
	!	.p
	!	There may be one or more subject labor operations which relate
	!	to the specific Workmen's Compensation Code. These are identified
	!	in the definition section of the Workmen's Compensation record.
	!	.p
	!	The purpose of establishing Code records in the Workmen's
	!	Compensation Table is to enable the system to calculate accrued
	!	premiums relative to Workmen's Compensation, Property Liability and
	!	Personal Injury insurances and post those calculations to specified
	!	General Ledger accounts.
	!
	! Index:
	!	.x Workmen's Compensation>Table
	!	.x Tables>Workmen's Compensation
	!
	! Option:
	!
	!	PR_MAIN_WC_DESCR$INSURANCE
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_WC_DESCR/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_WC_DESCR
	!	$ DELETE PR_MAIN_WC_DESCR.OBJ;*
	!
	! Author:
	!
	!	12/04/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	01/09/91 - Kevin Handy
	!		Removed the PR_WC_DEFINITION file.
	!
	!	04/24/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.HB"
	MAP (PR_WC_DESCR)	PR_WC_DESCR_CDD	PR_WC_DESCR
	MAP (PR_WC_DESCR2)	PR_WC_DESCR_CDD	PR_WC_DESCR_OLD, PR_WC_DESCR2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! This common area must be mapped in both the main program and
	! in PR_MAIN_WC_DESCR.
	!
	COM (CH_PR_WC_DESCR) &
		PR_WC_DESCR.CH%, &
		PR_WC_DESCR.READONLY%


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
	DECLARE RFA TEMP_RFA

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Workmen's Comp Description Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_WC_DESCR"
		SMG_WINDOW::CHAN  = PR_WC_DESCR.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%
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
		IF PR_WC_DESCR.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_WC_DESCR.READONLY%
			GOTO 790
		END IF

750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_WC_DESCR = ERR
			CONTINUE 770
		END WHEN

		PR_WC_DESCR.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.OPN"
		USE
			PR_MAIN_WC_DESCR = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_WC_DESCR.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_WC_DESCR.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_WC_DESCR.CH%
		WHEN ERROR IN
			RESET #PR_WC_DESCR.CH%
			GET #PR_WC_DESCR.CH%, REGARDLESS
			UNLOCK	#PR_WC_DESCR.CH%
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	2,  5, "(01) Code", &
			2, 40, "(02) Description", &
			4,  5, "(03) Liability Account", &
			5,  5, "(04) Expense   Account", &
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
	! Optional menu items
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " inSurance"

	!
	! Handle additional options
	!
20050	CASE OPT_MOREMENU

		!
		! Make sure there is a header
		!
		WHEN ERROR IN
			TEMP_RFA = GETRFA(PR_WC_DESCR.CH%)
		USE
			CONTINUE 32767
		END WHEN

		SELECT EDIT$(MVALUE, -1%)

		!
		! Pay
		!
		CASE "INSURANCE"
			PR_MAIN_WC_DESCR = MAIN_JOURNAL(PR_MAIN_WC_INSURANCE.ID, "")
	!++
	! Abstract:INSURANCE
	!	^*Insurance\*
	!	.p
	!	The ^*Insurance\* function
	!	accesses the Workmen's Compensation
	!	Table file where a record is maintained for each applicable Workman's
	!	Compensation code.
	!	.p
	!	The following data is maintained in each record in the file:
	!	.LIST "*"
	!	.LE
	!	State codes for each State where insurance liability
	!	could feasibly exist relative to a specific Code.
	!	.LE
	!	Effective dates for each premium rate or combination
	!	of premium rates.
	!	.LE
	!	Insurance rates for Workmen's Compensation, Property
	!	Liability and Personal Injury Liability insurances.
	!	.LE
	!	Flags to indicate whether overtime premium labor
	!	costs are subject to Workmen's Compensation, Property
	!	Liability or Personal Injury Liability insurance
	!	premium calculations in reference to each Code.
	!	.ELS
	!
	! Index:
	!	.x Insurance>Workmen's Compensation>Rates
	!	.x Insurance>Property Liability>Rates
	!	.x Insurance>Personal Injury>Rates
	!	.x Workmen's Compensation>Premium Rates
	!	.x Property Liability Insurance>Premium Rates
	!	.x Personal Injury Liability Insurance>Premium Rates
	!	.x Overtime>Premium Pay>Subject to WC Insurance Premiums
	!	.x Overtime>Premium Pay>Subject to Property Liability Ins Premiums
	!	.x Overtime>Premium Pay>Subject to Personal Injury Ins Premiums
	!	.x Workmen's Compensation>Table>Insurance Function
	!	.x Functions>Insurance
	!
	!--

		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Add record
		!
		CASE "Add"
			!
			! Add any line items under the header
			!
			PR_MAIN_WC_DESCR = MAIN_JOURNAL(PR_MAIN_WC_INSURANCE.ID, "A")

		!
		! Change records
		!
		CASE "Change", "Blank", "Initialize"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF PR_WC_DESCR_OLD::CODE <> PR_WC_DESCR::CODE
			THEN
				TEMP$ = PR_WC_DESCR::CODE + ""
				PR_WC_DESCR = PR_WC_DESCR_OLD
				PR_MAIN_WC_DESCR = MAIN_JOURNAL(PR_MAIN_WC_INSURANCE.ID, "C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			PR_MAIN_WC_DESCR = MAIN_JOURNAL(PR_MAIN_WC_INSURANCE.ID, "E")

		END SELECT


	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Code\*
	!	.p
	!	The ^*Code\* field
	!	records a workmen's compensation code number.
	!
	! Index:
	!	.x Code>Workmen's Compensation>Table
	!	.x Workmen's Compensation>Table>Code
	!
	!--

			PR_WC_DESCR::CODE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;15", TEMP$, &
				PR_WC_DESCR::CODE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.p
	!	The ^*Description\* field
	!	enters a workmen's compensation code description.
	!
	! Index:
	!	.x Description>Workmen's Compensation Table
	!	.x Workmen's Compensation>Table>Description
	!
	!--

			PR_WC_DESCR::DESCR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;57", TEMP$, &
				PR_WC_DESCR::DESCR, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Liability Account\*
	!	.p
	!	The ^*Liability Account\* field
	!	enters the General Ledger liability
	!	account which will be credited with the workmen's compensation
	!	calculation related to a specific workman's compensation code.
	!
	! Index:
	!	.x Liability Account>Workmen's Compensation Table
	!	.x Workmen's Compensation>Table>Liability Account
	!
	!--

			PR_WC_DESCR::LIA_ACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;30", TEMP$, &
				PR_WC_DESCR::LIA_ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_WC_DESCR::LIA_ACCT = GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Expense Account\*
	!	.p
	!	The ^*Expense Account\* field
	!	enters a General Ledger account or
	!	"account mask" which will be charged with the workmen's compensation
	!	calculation related to a specific workmen's compensation code.
	!	.p
	!	An "account mask" is an account "number" in which question marks
	!	are inserted in some or all positions in the field. If a mask contains
	!	a question mark in all positions it is equal to the defined account(s)
	!	which it is masking.
	!	.p
	!	If one or more positions in the field contain a character other
	!	than a question mark, it is equal to the defined account(s) which it
	!	is masking only in respect to the positions which contain question
	!	marks.
	!	.p
	!	The chart below illustrates the masking concept:
	!	.b
	!	.list 0,"*"
	!	.le
	!	^&Defined Accounts\&###^&Account Mask\&###^&Masked Account\&
	!	.b
	!	.le
	!	####70010-01###########???85-??########70085-01#################
	!	.le
	!	####70010-02############"##############70085-02#################
	!	.le
	!	####75010-01############"##############75085-01#################
	!	.le
	!	####75010-03############"##############75085-02#################
	!	.le
	!	####86145-03############"##############86185-03#################
	!	.le
	!	####86145-04############"##############86185-04#################
	!	.els
	!
	! Index:
	!	.x Expense Account>Workmen's Compensation Table
	!	.x Workmen's Compensation>Table>Expense Account
	!
	!--

			PR_WC_DESCR::EX_ACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;30", TEMP$, &
				PR_WC_DESCR::EX_ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_WC_DESCR::EX_ACCT = GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		PR_MAIN_WC_DESCR = 0%

		SELECT MLOOP

		CASE 1%
			IF PR_WC_DESCR::CODE = ""
			THEN
				PR_MAIN_WC_DESCR = 1%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Blank code not allowed", 1%)
			END IF

		CASE 3%, 4%
			TEMP$ = PR_WC_DESCR::LIA_ACCT
			TEMP$ = PR_WC_DESCR::EX_ACCT IF MLOOP = 4%

			IF TEMP$ <> "" AND INSTR(1%,TEMP$, "?") = 0%
			THEN
				!
				! Is the input defined?
				!
				PR_MAIN_WC_DESCR = FUNC_TESTENTRY(SMG_WINDOW, &
					TEMP$, &
					GL_CHART::DESCR, &
					"PR", MLOOP, "PRG", &
					"Account", GL_MAIN_CHART.ID)
			END IF

		END SELECT

	!
	! Set PR_WC_DESCR_OLD value
	!
20500	CASE OPT_SETOLD
		PR_WC_DESCR_OLD = PR_WC_DESCR

	!
	! Restore PR_WC_DESCR_OLD value
	!
	CASE OPT_RESETOLD
		PR_WC_DESCR = PR_WC_DESCR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_WC_DESCR2 = PR_WC_DESCR

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_WC_DESCR = PR_WC_DESCR2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Code       Description              " + &
				"Liability Account    Expense Account    "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "011,036,058"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PR_WC_DESCR::CODE + "     " + &
				PR_WC_DESCR::DESCR + "     " + &
				PR_WC_DESCR::LIA_ACCT + "   " + &
				PR_WC_DESCR::EX_ACCT
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_WC_DESCR.CH%, &
				KEY #0% GE PR_WC_DESCR::CODE + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

29900	!
	! Handle no header for Line function here
	!
	CALL ENTR_3MESSAGE(SCOPE, "Sorry, but there is no current record", 0%)

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:DEFINITION
	!	^*Definition\*
	!	.p
	!	The ^*Definition\* function
	!	accesses the Workmen's Compensation
	!	Definition screen in the Workman's Compensation Table.
	!	.p
	!	The Workmen's Compensation Definition screen
	!	enters General Ledger accounts and operation codes and/or combinations
	!	thereof which relate to a specific Workmen's Compensation code.
	!
	! Index:
	!	.x Definition>Function>Workmen's Compensation Table
	!	.x Workmen's Compensation Table>Definition Function
	!	.x Functions>Definitions
	!
	!--
