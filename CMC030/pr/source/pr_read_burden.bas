1	%TITLE "Read Burden Rate from O/H and WC File"
	%SBTTL "PR_READ_BURDEN"
	%IDENT "V3.6a Calico"

	SUB PR_READ_BURDEN( &
		PR_OVERHEAD_DEF.CH%, &
		PR_OVERHEAD_DESC.CH%, &
		PR_WC_DESCR.CH%, &
		PR_WC_INSURANCE.CH%, &
		PR_TAX_PKG.CH%, &
		PR_ERNDED_DEF.CH%, &
		EXP_ACCT$, &
		OPER$, &
		PAY_CODE$, &
		TAX_PKG$, &
		EFFDAT$, &
		SUBJECT_GRS, &
		SUBJECT_HRS, &
		SUBJECT_PREM, &
		WC_DEFCODE$, &
		WC_BURDEN, &
		WC_EXP$, &
		WC_LIA$, &
		OH_BURDEN, &
		OH_EXP$, &
		OH_APP$, &
		UP_BURDEN(), &
		UP_EXP$, &
		UP_LIA$(), &
		PREM_ACC$, &
		PREM_AMT)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho
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
	! Computer Management Center assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Computer Management Center.
	!
	!++
	!
	! Abstract:HELP
	!	.p
	!	This function returns the burden amount, overhead expense
	!	accounts, and liability accounts.  It also returns the
	!	premium account and amount if premium is to be applied
	!	to a different account
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	EXP_ACCT$ = Expense account number
	!	OPER$ = Operation number
	!		The account number and operation
	!		number are used to search the
	!		burden tables to determine the burden.
	!		If this search fails then the system
	!		will set the operation to null and
	!		try the search again.
	!	PAY_CODE$ = Pay code.
	!		The pay code is used to determine
	!		if the pay will be subject to
	!		WC or not.
	!	TAX_PKG$ = Tax Package for WC and LI
	!	EFFDAT$ = Effective date
	!	SUBJECT_GRS = Subject Gross amount
	!	SUBJECT_HRS = Subject regular hours
	!	SUBJECT_PREM = Subject Premium amount
	!	WC_DEFCODE = Workmen Compensation Default code
	!
	! Output:
	!
	!	WC_BURDEN = WC Burden amount
	!	WC_EXP$ = WC account number
	!	WC_LIA$ = WC liability account number
	!	OH_BURDEN = OH Burden amount
	!	OH_EXP$ = OH expense account number
	!	OH_APP$ = OH applied account number
	!	UP_BURDEN() = Union/Pension burden
	!	UP_EXP$ = Union/Pension Expense account number
	!	UP_LIA$() = Union/Pension Liability account number
	!	PREM_ACC$ = Premium account number
	!	PREM_AMT = Premium amount
	!
	! Example:
	!
	!	CALL PR_READ_BURDEN("270-0000, &
	!		"WELDING", &
	!		"CA", &
	!		"19871015", &
	!		320, &
	!		80, &
	!		0, &
	!		"6505", &
	!		WC_BURDEN, &
	!		WC_EXP$, &
	!		WC_LIA$, &
	!		OH_BURDEN, &
	!		OH_EXP$, &
	!		OH_APP$, &
	!		UP_BURDEN(), &
	!		UP_EXP$, &
	!		UP_LIA$(), &
	!		PREM_ACC$, &
	!		PREM_AMT)
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_READ_BURDEN/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_READ_BURDEN
	!	$ DELETE PR_READ_BURDEN.OBJ;*
	!
	! Author:
	!
	!	12/4/87 - Robert Peterson
	!
	! Modification history:
	!
	!	4/15/87 - Robert Peterson
	!		Add test for pay code to determine if this
	!		pay item is subject to WC or not.
	!
	!	05/22/89 - Frank F. Starman
	!		Modified to search for SI code if looking for WC and
	!		added case "5" under insurance method.
	!		WARNING:  METHOD is not the same as in PR_SPEC_CALCULATE
	!
	!	09/21/89 - Frank F. Starman
	!		Corrected error trapping on line 2000 and 3000;
	!		made it readable.
	!
	!	08/23/90 - Kevin Handy
	!		Modified search of PR_WC_DESCR to skip get if record
	!		already in memory.  This is an attempt to cut down
	!		on disk IO and speed up the final post.
	!
	!	01/09/91 - Kevin Handy
	!		Removed PR_WC_DEFINITION file.
	!
	!	07/14/91 - Kevin Handy
	!		Modified to lose common area PR_READ_BURDEN
	!		and to pass channels through the parameter
	!		list instead.
	!
	!	01/26/93 - Kevin Handy
	!		Modified so that instead of setting OH_OPER to
	!		NULL, it now sets it to spaces so that the defaults
	!		require a blank operation code instead of just
	!		picking the first overhead item with the correct
	!		account number.
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Remove SUBJECT% parameter from PR_READ_SUBJTAX.
	!
	!	07/17/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DEF.HB"
	MAP (PR_OVERHEAD_DEF)	PR_OVERHEAD_DEF_CDD	PR_OVERHEAD_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DESC.HB"
	MAP (PR_OVERHEAD_DESC)	PR_OVERHEAD_DESC_CDD	PR_OVERHEAD_DESC

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.HB"
	MAP (PR_WC_DESCR)	PR_WC_DESCR_CDD	PR_WC_DESCR

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.HB"
	MAP (PR_WC_INSURANCE)	PR_WC_INSURANCE_CDD	PR_WC_INSURANCE

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP (PR_TAX_PKG)	PR_TAX_PKG_CDD	PR_TAX_PKG

	DIM INSURANCE(100%)

	%PAGE

	EFFDAT$ = DATE_TODAY IF EFFDAT$ = ""
	WC_BURDEN = 0.0
	WC_EXP$ = ""
	WC_LIA$ = ""
	OH_BURDEN = 0.0
	OH_EXP$ = ""
	OH_APP$ = ""
	PREM_ACC$ = ""
	PREM_AMT = 0.0
	INS_LOOP% = 0%

	!
	! Set up operation codes to each burden area
	!
	OH_OPER$ = OPER$
	WC_OPER$ = OPER$

2000	!**********************************************************
	! Get overhead data first
	!**********************************************************
	WHEN ERROR IN
		GET #PR_OVERHEAD_DEF.CH%, &
			KEY #1% EQ EXP_ACCT$ + OH_OPER$, &
			REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 9%

		IF ERR = 155%
		THEN
			IF TRM$(EXP_ACCT$) <> "" AND OH_OPER$ <> ""
			THEN
				!
				! Ignore operation
				!
				OH_OPER$ = ""
				CONTINUE 2000
			ELSE
				CONTINUE 3000
			END IF
		END IF

		FILENAME$ = "PR_OVERHEAD_DEF"
		CONTINUE HelPError
	END WHEN

2010	!
	! Get overhead description file
	!
	WHEN ERROR IN
		GET #PR_OVERHEAD_DESC.CH%, &
			KEY #0% EQ PR_OVERHEAD_DEF::OVH_KEY, &
			REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_OVERHEAD_DESC"
		CONTINUE HelpError
	END WHEN

	!
	! Overhead applied account
	!
	OH_APP$ = PR_OVERHEAD_DESC::OVRHD_ACCT

	!
	! Expense account
	!
	IF INSTR(1%, PR_OVERHEAD_DESC::EX_ACCT, "?")
	THEN
		CALL GL_ASSG_ACCMASK(PR_OVERHEAD_DESC::EX_ACCT, &
			EXP_ACCT$, &
			OH_EXP$)
	ELSE
		OH_EXP$ = PR_OVERHEAD_DESC::EX_ACCT
	END IF

	!
	! Compute the burden
	!
	IF PR_OVERHEAD_DESC::BASIS = "1"
	THEN
		OH_BURDEN = FUNC_ROUND(SUBJECT_HRS * PR_OVERHEAD_DESC::RATE, 2%)
	ELSE
		OH_BURDEN = FUNC_ROUND((SUBJECT_GRS - SUBJECT_PREM) * &
			PR_OVERHEAD_DESC::RATE / 100.0, 2%)
	END IF

	!
	! Where does the premium go to
	!
	IF PR_OVERHEAD_DESC::PREM_ACCT <> ""
	THEN
		PREM_ACC$ = PR_OVERHEAD_DESC::PREM_ACCT
		PREM_AMT = SUBJECT_PREM
	END IF

3000	!*******************************************************************
	! Handle Workmans Comp
	!*******************************************************************

	CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
		"WCI", &
		"P", &
		PAY_CODE$, &
		TAXABLE%, &
		REPORTABLE%)

	GOTO 4000 IF TAXABLE%

3010	!
	! Get overhead description file
	!
	IF PR_WC_DESCR::CODE <> WC_DEFCODE$
	THEN
		WHEN ERROR IN
			GET #PR_WC_DESCR.CH%, KEY #0% EQ WC_DEFCODE$, REGARDLESS
		USE
			CONTINUE 4000 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_WC_DESCR"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Liability account
	!
	WC_LIA$ = PR_WC_DESCR::LIA_ACCT

	!
	! Expense account
	!
	IF INSTR(1%, PR_WC_DESCR::EX_ACCT, "?")
	THEN
		CALL GL_ASSG_ACCMASK(PR_WC_DESCR::EX_ACCT, &
			EXP_ACCT$, &
			WC_EXP$)
	ELSE
		WC_EXP$ = PR_WC_DESCR::EX_ACCT
	END IF

3020	!
	! Look up tax state
	!
	WHEN ERROR IN
		GET #PR_TAX_PKG.CH%, KEY #0% EQ TAX_PKG$ + "SI", REGARDLESS
		STATE$ = PR_TAX_PKG::CODE
	USE
		STATE$ = ""

		CONTINUE 4000 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

3030	!
	! Look up insurance record
	!
	WHEN ERROR IN
		FIND #PR_WC_INSURANCE.CH%, &
			KEY #0% EQ PR_WC_DESCR::CODE + STATE$, &
			REGARDLESS
	USE
		CONTINUE 4000 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_WC_INSURANCE"
		CONTINUE HelpError
	END WHEN

	TEST_INS_TYPE$ = ""

 GetNextRec:
3040	WHEN ERROR IN
		GET #PR_WC_INSURANCE.CH%, REGARDLESS
	USE
		CONTINUE TotalIns IF ERR = 11%
		FILENAME$ = "PR_WC_INSURANCE"
		CONTINUE HelpError
	END WHEN

	GOTO TotalIns IF PR_WC_INSURANCE::CODE <> PR_WC_DESCR::CODE OR &
		PR_WC_INSURANCE::STATE <> STATE$

	IF PR_WC_INSURANCE::EFFDAT <= EFFDAT$
	THEN
		IF TEST_INS_TYPE$ <> PR_WC_INSURANCE::INS_TYPE OR INS_LOOP% = 0%
		THEN
			INS_LOOP% = INS_LOOP% + 1%
			TEST_INS_TYPE$ = PR_WC_INSURANCE::INS_TYPE
		END IF

		SELECT PR_WC_INSURANCE::METHOD

		CASE "1"
			INSURANCE(INS_LOOP%) = (SUBJECT_GRS - SUBJECT_PREM) * &
				PR_WC_INSURANCE::EMPLR_RATE

		CASE "2"
			INSURANCE(INS_LOOP%) = SUBJECT_GRS * &
				PR_WC_INSURANCE::EMPLR_RATE

		CASE "3"
			INSURANCE(INS_LOOP%) = SUBJECT_HRS * &
				PR_WC_INSURANCE::EMPLR_RATE

		CASE "4"
			INSURANCE(INS_LOOP%) = SUBJECT_HRS * &
				PR_WC_INSURANCE::EMPLR_RATE

		CASE "5"
			INSURANCE(INS_LOOP%) = SUBJECT_HRS * &
				PR_WC_INSURANCE::EMPLR_RATE / 8.0

		END SELECT

	END IF

	GOTO GetNextRec

 TotalIns:
	WC_BURDEN = WC_BURDEN + INSURANCE(LOOP%) &
		FOR LOOP% = 1% TO INS_LOOP%

	WC_BURDEN = FUNC_ROUND(WC_BURDEN, 2%)

 ExitSub:
4000	!
	EXIT SUB

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitSub

	END SUB
