1	%TITLE "Fprint W2 forms"
	%SBTTL "PR_FORM_EMP_LASER_W2"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 2007 BY
	!	Software Solutions, Inc.
	!	Idaho Falls, Idaho.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is
	! not supported by Software Solutions, Inc.
	!
	!++
	! ID:PREW2
	!
	! Abstract:HELP
	!	.b
	!	This option will print W2 forms that have been entered using
	!	the maintenance procedure.
	!
	! Index:
	!
	! Option:
	!	PR_FORM_EMP_LASER_W2$CONFIRM
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_FORM_EMP_LASER_W2
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_FORM_EMP_LASER_W2, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_FORM_EMP_LASER_W2.OBJ;*
	!
	! Author:
	!
	!	01/30/2007 - Kevin Handy
	!		Based on PR_FORM_EMP_W2 and PR_FORM_LASER_W2
	!
	! Modification history:
	!
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PR_STRUCTURE.INC"

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM FORM_GROUP_CDD FORM_GROUP(10%)	! Max of 10 groups

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT
	DECLARE			UTL_REPORT_CDD		UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_W2.HB"
	MAP (PR_EMP_W2)	PR_EMP_W2_CDD	PR_EMP_W2

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP (PR_TAX_TABLE) PR_TAX_TABLE_CDD PR_TAX_TABLE

	MAP (PR_TEMP) &
		PR_TEMP.TTYPE$ = 2%, &
		PR_TEMP.CODE$ = 2%, &
		PR_TEMP.EMPNUM$ = 10%, &
		PR_TEMP.TAXABLE_WAGE(2%), &
		PR_TEMP.TAXES(2%)

	DIM PR_TAXES_STRUCT PR_TAXES(50%)

	RECORD W2_FORM_CDD
		! Company
		STRING	COMP_NAME = 40%
		STRING	COMP_ADD1 = 40%
		STRING	COMP_ADD2 = 40%
		STRING	COMP_CITY = 20%
		STRING	COMP_STATE = 2%
		STRING	COMP_ZIP = 10%
		! Employee
		STRING	EMP_NUM = 10%
		STRING	EMP_NAME = 40%
		STRING	EMP_ADD1 = 40%
		STRING	EMP_ADD2 = 40%
		STRING	EMP_CITY = 20%
		STRING	EMP_STATE = 2%
		STRING	EMP_ZIP = 10%
		STRING	EMP_SSN = 11%
		! Flags
		STRING	STAT_EMP = 1%
		STRING	DECEASED = 1%
		STRING	PEN_PLAN = 1%
		STRING	SICK_PAY = 1%
		STRING	LEGAL_REP = 1%
		STRING	EMP_942 = 1%
		STRING	SUBTOTAL = 1%
		STRING	DEF_COMP = 1%
		STRING	VOID = 1%
		! Other
		REAL	ALL_TIPS
		REAL	AD_EIC
		REAL	FRINGE_BENEFIT
		REAL	QUAL
		REAL	NONQUAL
		REAL	SOCTIP
		! Federal Tax
		REAL	FEDTAX_WH
		REAL	FED_WAGE
		REAL	SSTAX_WH
		REAL	SSTAX_WH_HI
		REAL	SS_WAGE
		REAL	SS_WAGE_HI
		REAL	SS_TIPS
		STRING	FED_REPNO = 20%
		! State Tax
		REAL	STATETAX_WH(7%)
		REAL	STATE_WAGE(7%)
		REAL	STATE_SDI(7%)
		STRING	STATE_ID(7%) = 2%
		STRING	STATE_REPNO(7%) = 20%
		! 401K
		REAL	K401A(5%)
		STRING	K401C(5%) = 1%
		! Local Taxes
		LONG LOCALCOUNT
		STRING LOCALNAME(7%) = 4%
		REAL LOCALWAGE(7%)
		REAL LOCALTAX(7%)
		! OTHER
		REAL	OTHER(5%)
		STRING	OTHERC(5%) = 1%
		REAL DEP_CARE
	END RECORD

	!
	! (0) = BLANK
	! (1) = EMPLOYEE
	! (2) = SUBTOTAL
	! (3) = GRAND TOTAL
	!
	MAP (EMP_W2_FORM) &
		W2_FORM_CDD W2_FORM(3%)

	MAP (W2_OTHER) &
		STATE%, &
		PAGE_COUNT%, &
		SUMMARY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_FORMINIT
	EXTERNAL LONG	FUNCTION OUTP_FORMPRINT
 !	EXTERNAL LONG	FUNCTION PR_FUNC_READTAXES
	EXTERNAL LONG   FUNCTION OUTP_INITFORM


	!
	! Declarations
	!
	DECLARE INTEGER CONSTANT LIB_INDEX = 200%	! Size of the array
	DECLARE INTEGER CONSTANT FORM_HIGH = 2%	! Forms per sheet
	DECLARE INTEGER CONSTANT FORM_LENGTH = 33%	! Individual form length
	DECLARE INTEGER CONSTANT FORM_TOTAL = 42%	! How often for subtotal
 !	DECLARE INTEGER CONSTANT FORM_TOTAL = 0%	* How often for subtotal

	!
	! Dimension statements
	!
	DIM LIB_INDEX$(LIB_INDEX), RFA LIB_RFA(LIB_INDEX)

	%PAGE

	!
	! Constants
	!

	! Company
	W2_FORM(0%)::COMP_NAME	= ""
	W2_FORM(0%)::COMP_ADD1	= ""
	W2_FORM(0%)::COMP_ADD2	= ""
	W2_FORM(0%)::COMP_CITY	= ""
	W2_FORM(0%)::COMP_STATE	= ""
	W2_FORM(0%)::COMP_ZIP	= ""
	! Employee
	W2_FORM(0%)::EMP_NUM	= ""
	W2_FORM(0%)::EMP_NAME	= ""
	W2_FORM(0%)::EMP_ADD1	= ""
	W2_FORM(0%)::EMP_ADD2	= ""
	W2_FORM(0%)::EMP_CITY	= ""
	W2_FORM(0%)::EMP_STATE	= ""
	W2_FORM(0%)::EMP_ZIP	= ""
	W2_FORM(0%)::EMP_SSN	= ""
	! Flags
	W2_FORM(0%)::STAT_EMP	= ""
	W2_FORM(0%)::DECEASED	= ""
	W2_FORM(0%)::PEN_PLAN	= ""
	W2_FORM(0%)::SICK_PAY	= ""
	W2_FORM(0%)::LEGAL_REP	= ""
	W2_FORM(0%)::EMP_942	= ""
	W2_FORM(0%)::SUBTOTAL	= ""
	W2_FORM(0%)::DEF_COMP	= ""
	W2_FORM(0%)::VOID	= ""
	! Federal Tax
	W2_FORM(0%)::FED_REPNO	= ""
	! State Tax
	FOR I% = 0% TO 5%
		W2_FORM(0%)::STATE_ID(I%) = ""
		W2_FORM(0%)::STATE_REPNO(I%) = ""
	NEXT I%
	! 401K
	FOR I% = 0% TO 5%
		W2_FORM(0%)::K401C(I%)	= ""
	NEXT I%
	! Local Tax
	W2_FORM(0%)::LOCALCOUNT = 0%
	FOR I% = 0% TO 7%
		W2_FORM(0%)::LOCALNAME(I%) = ""
		W2_FORM(0%)::LOCALWAGE(I%) = 0.0
		W2_FORM(0%)::LOCALTAX(I%) = 0.0
	NEXT I%
	! OTHER
	FOR I% = 0% TO 5%
		W2_FORM(0%)::OTHERC(I%)	= ""
	NEXT I%
	DEP_CARE = 0.0

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "PREW2 "

	!
	! Look up device
	!
	CALL READ_DEVICE("PR_FORM", PR_FORM.DEV$, STAT%)

200	!*******************************************************************
	! Ask for W2 form name
	!*******************************************************************
	CALL LIBR_INDEX(PR_FORM.DEV$ + "PR_FORM", "W2*", LIB_INDEX$(), &
		LIB_RFA())

210	X% = ENTR_3CHOICE(SCOPE, "", "", LIB_INDEX$(), "", &
		8%, "W2 Form", "", 0%)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	END SELECT

	IF X% > 0%
	THEN
		W2_FORM$ = LIB_INDEX$(X%)
	ELSE
		GOTO 210
	END IF

	!*******************************************************************
	! Initilize W2 form
	!*******************************************************************

	!
	! Get form from the PR form library
	!
	SMG_STATUS% = OUTP_FORMINIT(PR_FORM.DEV$ + "PR_FORM", W2_FORM$, &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "W2 form is missing", &
			"E", SCOPE::PRG_PROGRAM, W2_FORM$, NUM1$(SMG_STATUS%))
		GOTO ExitProgram
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_BODY% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-BODY"
			FRM_BODY% = I%

		CASE "FRM-CODES"
			FRM_CODES$ = EDIT$(SEG$(FORM_TEXT$, &
				FORM_GROUP(I%)::POINTER, &
				FORM_GROUP(I% + 1%)::POINTER - 1%), 4% + 2%)

		END SELECT

	NEXT I%

	!***************************************************************
	! Open all of the files
	!***************************************************************

300	!
	! Open Payroll Employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_W2.OPN"
	USE
		FILENAME$ = "PR_EMP_W2"
		CONTINUE HelpError
	END WHEN

425	!
	! Open LOCATION file, Set up company name as the default
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	W2_FORM(0%)::COMP_NAME = &
		STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B)
	W2_FORM(0%)::COMP_ADD1 = ""
	W2_FORM(0%)::COMP_ADD2 = ""
	W2_FORM(0%)::COMP_CITY = &
		STRING$(LEN(UTL_LOCATION::CITY), A"?"B)
	W2_FORM(0%)::COMP_STATE = &
		STRING$(LEN(UTL_LOCATION::STATE), A"?"B)
	W2_FORM(0%)::COMP_ZIP = &
		STRING$(LEN(UTL_LOCATION::ZIP), A"?"B)

430	!
	! Open REPORT file
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	!
	! Set user variables
	!
	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	!
	! Abstract:FLD01
	!	^*(01) Year\*
	!	.b
	!	.lm +5
	!	Specifies which payroll year is to be printed.
	!	.lm -5
	!
	! Index:
	!	.x W-2 Forms>Year
	!	.x Year>W-2 Forms
	!
	!--

	MAINLOCATION$ = LEFT$(UTL_REPORTX::OPTDEF(1%), 4%)

	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ MAINLOCATION$, &
			REGARDLESS
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	W2_FORM(0%)::COMP_NAME	= UTL_LOCATION::LOCNAME
	W2_FORM(0%)::COMP_ADD1	= UTL_LOCATION::ADDRESS1
	W2_FORM(0%)::COMP_ADD2	= UTL_LOCATION::ADDRESS2
	W2_FORM(0%)::COMP_CITY	= UTL_LOCATION::CITY
	W2_FORM(0%)::COMP_STATE	= UTL_LOCATION::STATE
	W2_FORM(0%)::COMP_ZIP	= UTL_LOCATION::ZIP

	!++
	!
	! Abstract:FLD02
	!	^*(02) Quarter\*
	!	.b
	!	.lm +5
	!	Specifies which quarter is to be used as the last quarter
	!	for the forms.
	!	^*Note:\* This field should normally be *4,
	!	and should only be changed
	!	to a different value for debugging problems.
	!	.lm -5
	!
	! Index:
	!	.x W-2 Forms>Quarter
	!	.x Quarter>W-2 Forms
	!
	!--

470	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"
		GET #PR_TAX_TABLE.CH%, KEY #0% EQ "F  ", REGARDLESS
	USE
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	FICA_EMPR_PCT = (PR_TAX_TABLE::FICA_EMPR_PCT) / 10000.0
	FICA_EMPE_PCT = (PR_TAX_TABLE::FICA_EMPE_PCT) / 10000.0
	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT

	FICA_EMPR_PCT_HI = (PR_TAX_TABLE::FICA_EMPR_PCT_HI) / 10000.0
	FICA_EMPE_PCT_HI = (PR_TAX_TABLE::FICA_EMPE_PCT_HI) / 10000.0
	FICA_LIMIT_HI = PR_TAX_TABLE::FICA_LIMIT_HI

	!
	! Give the additional digit when possible
	!
	IF FICA_EMPR_PCT > 0.100
	THEN
		FICA_EMPR_PCT = FICA_EMPR_PCT / 10.0
		FICA_EMPE_PCT = FICA_EMPE_PCT / 10.0
		FICA_EMPR_PCT_HI = FICA_EMPR_PCT_HI / 10.0
		FICA_EMPE_PCT_HI = FICA_EMPE_PCT_HI / 10.0
	END IF


480	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$

	!
	! GOTO aligment routine
	!
	GOSUB Alignment

	%PAGE

2000	!*******************************************************************
	! Read through Payroll master file
	!*******************************************************************

	WHEN ERROR IN
		RESET #PR_EMP_W2.CH%, KEY #1%
	USE
		FILENAME$ = "PR_EMP_W2"
		CONTINUE HelpError
	END WHEN

	TEST_42% = 0%
	FORM_COUNT% = 0%
	UTL_REPORTX::LINENO = 0%

	W2_FORM(2%) = W2_FORM(0%)
	W2_FORM(3%) = W2_FORM(0%)

2010	!
	! Skip out if last page has been printed
	!
	GOTO ExitProgram &
		IF (PAGE_COUNT% > UTL_REPORTX::ENDP) AND (UTL_REPORTX::ENDP <> 0%)

	WHEN ERROR IN
		GET #PR_EMP_W2.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		CONTINUE 2010 IF ERR = 155%
		FILENAME$ = "PR_EMP_W2"
		CONTINUE HelpError
	END WHEN

	GOTO 2010 IF PR_EMP_W2::LOCATION <> MAINLOCATION$

	!
	! Initialize W2 form values for employee
	!
	W2_FORM(1%) = W2_FORM(0%)
	STATE% = 0%
	EMP_K401% = 0%
	OTHER% = 0%

	W2_FORM(1%)::EMP_NUM	= PR_EMP_W2::EMPNUM
	W2_FORM(1%)::EMP_NAME	= PR_EMP_W2::EMPNAME
	W2_FORM(1%)::EMP_ADD1	= PR_EMP_W2::ADD1
	W2_FORM(1%)::EMP_ADD2	= PR_EMP_W2::ADD2
	W2_FORM(1%)::EMP_CITY	= PR_EMP_W2::CITY
	W2_FORM(1%)::EMP_STATE	= PR_EMP_W2::STATE
	W2_FORM(1%)::EMP_ZIP	= PR_EMP_W2::ZIP
	W2_FORM(1%)::EMP_SSN	= PRNT_SSN(PR_EMP_W2::SSN, 0%)

	!***********************************************************
	! Look up federal wages and taxes
	!***********************************************************

	W2_FORM(1%)::FEDTAX_WH = PR_EMP_W2::TAXES(0%)
	W2_FORM(1%)::FED_WAGE = PR_EMP_W2::TAXABLE(0%)
	W2_FORM(I%)::FED_REPNO  = PR_EMP_W2::TAXES_ID(0%) &
		FOR I% = 0% TO 3%


	!***********************************************************
	! Look up FICA (OASDI) wages and taxes
	!***********************************************************

	W2_FORM(1%)::SS_WAGE = PR_EMP_W2::TAXABLE(1%)
	W2_FORM(1%)::SS_WAGE = FICA_LIMIT &
		IF W2_FORM(1%)::SS_WAGE > FICA_LIMIT
	W2_FORM(1%)::SSTAX_WH = PR_EMP_W2::TAXES(1%)

	!***********************************************************
	! Look up FICA (HI) wages and taxes
	!***********************************************************

	W2_FORM(1%)::SS_WAGE_HI = PR_EMP_W2::TAXABLE(2%)
	W2_FORM(1%)::SS_WAGE_HI = FICA_LIMIT_HI &
		IF W2_FORM(1%)::SS_WAGE_HI > FICA_LIMIT_HI

	W2_FORM(1%)::SSTAX_WH_HI = PR_EMP_W2::TAXES(2%)


2130	!***********************************************************
	! Look up state wages and taxes
	!***********************************************************

	STATE% = STATE% + 1%

	W2_FORM(1%)::STATETAX_WH(STATE%) = PR_EMP_W2::TAXES(3%)
	W2_FORM(1%)::STATE_ID(STATE%) = PR_EMP_W2::TAXES_STATE(3%)
	W2_FORM(1%)::STATE_WAGE(STATE%) = PR_EMP_W2::TAXABLE(3%)
	W2_FORM(1%)::STATE_SDI(STATE%) = 0.0

	W2_FORM(I%)::STATE_REPNO(STATE%) = PR_EMP_W2::TAXES_ID(3%) &
		FOR I% = 0% TO 3%


	!***********************************************************
	! Look up state wages and taxes
	!***********************************************************

	W2_FORM(1%)::LOCALCOUNT = 0%
	W2_FORM(1%)::LOCALNAME(W2_FORM(1%)::LOCALCOUNT) = ""
	W2_FORM(1%)::LOCALTAX(W2_FORM(1%)::LOCALCOUNT) = 0.0
	W2_FORM(1%)::LOCALWAGE(W2_FORM(1%)::LOCALCOUNT) = 0.0


	!***********************************************************
	! Look up State Disability Insurance (SX)
	!***********************************************************


	!***********************************************************
	! Look up State Disability Insurance (SI)
	!***********************************************************


2200	!*******************************************************************
	! Scan through PR_REG_ERNDED file for anything to put in box
	! 16A (if selected)
	!*******************************************************************


2300	!*****************************************************************
	! Print form
	!*****************************************************************

	!
	! Any reason to print this form?
	!
	SUM = W2_FORM(1%)::FED_WAGE + &
		W2_FORM(1%)::STATE_WAGE(1%) + &
		W2_FORM(1%)::SS_WAGE + &
		W2_FORM(1%)::FEDTAX_WH + &
		W2_FORM(1%)::STATETAX_WH(1%)

	IF (SUM <> 0.0)
	THEN
		!
		! Print Employee W2 form
		!
		EMP_COUNT% = EMP_COUNT% + 1%
		SUMMARY% = 1%
		GOSUB SetValue

		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Total W2 forms
		!
		SUMMARY% = 2%		! Subtotal
		GOSUB SummarizeEmp

		SUMMARY% = 3%		! Total
		GOSUB SummarizeEmp

		!
		! Check to see if 42nd form
		!
		TEST_42% = TEST_42% + 1%
		IF (TEST_42% >= FORM_TOTAL - 1%) AND (FORM_TOTAL <> 0%)
		THEN
			GOSUB Subtotal
		END IF
	END IF

2990	GOTO 2010

	%Page

 SummarizeEmp:
	!*******************************************************************
	! Subtotal/Total employee information
	!*******************************************************************

	! Other

	W2_FORM(SUMMARY%)::ALL_TIPS = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::ALL_TIPS + &
		W2_FORM(1%)::ALL_TIPS, 2%)
	W2_FORM(SUMMARY%)::AD_EIC = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::AD_EIC + &
		W2_FORM(1%)::AD_EIC, 2%)
	W2_FORM(SUMMARY%)::DEP_CARE = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::DEP_CARE + &
		W2_FORM(1%)::DEP_CARE, 2%)
	W2_FORM(SUMMARY%)::FRINGE_BENEFIT = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::FRINGE_BENEFIT + &
		W2_FORM(1%)::FRINGE_BENEFIT, 2%)

		! Federal Tax
	W2_FORM(SUMMARY%)::FEDTAX_WH = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::FEDTAX_WH + &
		W2_FORM(1%)::FEDTAX_WH, 2%)
	W2_FORM(SUMMARY%)::FED_WAGE = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::FED_WAGE + &
		W2_FORM(1%)::FED_WAGE, 2%)
	W2_FORM(SUMMARY%)::SSTAX_WH = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::SSTAX_WH + &
		W2_FORM(1%)::SSTAX_WH, 2%)
	W2_FORM(SUMMARY%)::SSTAX_WH_HI = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::SSTAX_WH_HI + &
		W2_FORM(1%)::SSTAX_WH_HI, 2%)
	W2_FORM(SUMMARY%)::SS_WAGE = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::SS_WAGE + &
		W2_FORM(1%)::SS_WAGE, 2%)
	W2_FORM(SUMMARY%)::SS_WAGE_HI = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::SS_WAGE_HI + &
		W2_FORM(1%)::SS_WAGE_HI, 2%)
	W2_FORM(SUMMARY%)::SS_TIPS = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::SS_TIPS + &
		W2_FORM(1%)::SS_TIPS, 2%)

	! State Tax
	FOR I% = 1% TO 5%
		J% = 0%
		J% = K% IF (W2_FORM(SUMMARY%)::STATE_ID(K%) = &
			W2_FORM(1%)::STATE_ID(I%)) OR &
			((W2_FORM(SUMMARY%)::STATE_ID(K%) = "") AND (J% = 0%)) &
			FOR K% = 0% TO 5%
		J% = 5% IF J% = 0%

		W2_FORM(SUMMARY%)::STATETAX_WH(J%) = &
			FUNC_ROUND(W2_FORM(SUMMARY%)::STATETAX_WH(J%) + &
			W2_FORM(1%)::STATETAX_WH(I%), 2%)
		W2_FORM(SUMMARY%)::STATE_WAGE(J%) = &
			FUNC_ROUND(W2_FORM(SUMMARY%)::STATE_WAGE(J%) + &
			W2_FORM(1%)::STATE_WAGE(I%), 2%)
		W2_FORM(SUMMARY%)::STATE_SDI(J%) = &
			FUNC_ROUND(W2_FORM(SUMMARY%)::STATE_SDI(J%) + &
			W2_FORM(1%)::STATE_SDI(I%), 2%)
		W2_FORM(SUMMARY%)::STATE_ID(J%) = &
			W2_FORM(1%)::STATE_ID(I%)
		W2_FORM(SUMMARY%)::STATE_REPNO(J%) = &
			W2_FORM(1%)::STATE_REPNO(I%)
	NEXT I%

	! Local Tax
	FOR I% = 1% TO W2_FORM(1%)::LOCALCOUNT

		FOR J% = 1% TO W2_FORM(SUMMARY%)::LOCALCOUNT

			IF W2_FORM(1%)::LOCALNAME(I%) = &
				W2_FORM(SUMMARY%)::LOCALNAME(J%)
			THEN
				W2_FORM(SUMMARY%)::LOCALWAGE(J%) = &
					W2_FORM(SUMMARY%)::LOCALWAGE(J%) + &
					W2_FORM(1%)::LOCALWAGE(I%)

				W2_FORM(SUMMARY%)::LOCALTAX(J%) = &
					W2_FORM(SUMMARY%)::LOCALTAX(J%) + &
					W2_FORM(1%)::LOCALTAX(I%)

				GOTO EndLocal
			END IF
		NEXT J%

		J%, W2_FORM(SUMMARY%)::LOCALCOUNT = &
			W2_FORM(SUMMARY%)::LOCALCOUNT + 1%

		W2_FORM(SUMMARY%)::LOCALNAME(J%) = &
			W2_FORM(1%)::LOCALNAME(I%)

		W2_FORM(SUMMARY%)::LOCALWAGE(J%) = &
			W2_FORM(1%)::LOCALWAGE(I%)

		W2_FORM(SUMMARY%)::LOCALTAX(J%) = &
			W2_FORM(1%)::LOCALTAX(I%)

 EndLocal:
	NEXT I%

	!
	! 401K Info
	!
	FOR I% = 1% TO 5%
		J% = 0%
		J% = K% IF (W2_FORM(SUMMARY%)::K401C(K%) = &
			W2_FORM(1%)::K401C(I%)) OR &
			((W2_FORM(SUMMARY%)::K401C(K%) = "") AND (J% = 0%)) &
			FOR K% = 1% TO 5%
		J% = 5% IF J% = 0%

		W2_FORM(SUMMARY%)::K401A(J%) = &
			FUNC_ROUND(W2_FORM(SUMMARY%)::K401A(J%) + &
			W2_FORM(1%)::K401A(I%), 2%)
		W2_FORM(SUMMARY%)::K401C(J%) = &
			W2_FORM(1%)::K401C(I%)
	NEXT I%

	!
	! OTHER
	!
	FOR I% = 1% TO 5%
		J% = 0%
		J% = K% IF (W2_FORM(SUMMARY%)::OTHERC(K%) = &
			W2_FORM(1%)::OTHERC(I%)) OR &
			((W2_FORM(SUMMARY%)::OTHERC(K%) = "") AND (J% = 0%)) &
			FOR K% = 1% TO 5%
		J% = 5% IF J% = 0%

		W2_FORM(SUMMARY%)::OTHER(J%) = &
			FUNC_ROUND(W2_FORM(SUMMARY%)::OTHER(J%) + &
			W2_FORM(1%)::OTHER(I%), 2%)
		W2_FORM(SUMMARY%)::OTHERC(J%) = &
			W2_FORM(1%)::OTHERC(I%)
	NEXT I%

	RETURN

 Subtotal:
	!*******************************************************************
	! Print subtotal form
	!*******************************************************************

	SUMMARY% = 2%

	!
	! Set employee name to total
	!
	W2_FORM(2%)::EMP_NUM	= ""
	W2_FORM(2%)::EMP_NAME	= "*********************"
	W2_FORM(2%)::EMP_ADD1	= "*****  Subtotal  ****"
	W2_FORM(2%)::EMP_ADD2	= "*********************"
	W2_FORM(2%)::EMP_CITY	= QTRFLAG$
	W2_FORM(2%)::EMP_STATE	= ""
	W2_FORM(2%)::EMP_ZIP	= ""
	W2_FORM(2%)::EMP_SSN	= ""
	W2_FORM(2%)::SUBTOTAL	= "X"

	GOSUB SetValue

	GOTO ExitProgram IF UTL_REPORTX::STAT

	W2_FORM(2%) = W2_FORM(0%)

	TEST_42% = 0%

	RETURN

	%PAGE

 ExitTotal:
	!*********************************************************************
	! Print Grand total
	!*********************************************************************
	IF (TEST_42% > 0%) AND (FORM_TOTAL <> 0%)
	THEN
		!
		! Print to bottom of form
		!
		FORM% = FORM_COUNT% - (INT(FORM_COUNT% / FORM_HIGH) * FORM_HIGH)
		FORM% = FORM_HIGH IF FORM% = 0%

		FORM_COUNT% = FORM_COUNT% + FORM% - 1%

		!
		! Page counter
		!
		PAGE_COUNT% = INT(FORM_COUNT% / FORM_HIGH) + 1%

		FORM% = (FORM% - 1%) * FORM_LENGTH

		CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
			FOR LOOP% = 1% TO FORM%

		GOSUB Subtotal
	END IF

	!******************************************************************
	! Print grand total
	!******************************************************************

	SUMMARY% = 3%

	!
	! Set employee name to total
	!
	W2_FORM(3%)::EMP_NUM	= ""
	W2_FORM(3%)::EMP_NAME	= "*********************"
	W2_FORM(3%)::EMP_ADD1	= "Total Employees " + NUM1$(EMP_COUNT%)
	W2_FORM(3%)::EMP_ADD2	= "*********************"
	W2_FORM(3%)::EMP_CITY	= QTRFLAG$
	W2_FORM(3%)::EMP_STATE	= ""
	W2_FORM(3%)::EMP_ZIP	= ""
	W2_FORM(3%)::EMP_SSN	= ""

	GOSUB SetValue

	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = 1% TO FORM_LENGTH * (FORM_HIGH - 1%)

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	CLOSE PR_EMP_W2.CH%

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Change the width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 80%)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 Alignment:
	!*******************************************************************
	! Print alignment form, if desireable
	!*******************************************************************

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	UTL_REPORTX::LINENO = 0%
	UTL_REPORTX::PAGENO = 0%

	SCOPE::PRG_ITEM = "CONFIRM"
	!++
	! Abstract:CONFIRM
	!
	!
	! Index:
	!
	!
	!--
	JUNK$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", "Do you want an alignment form?  " + &
		"Confirm then press <Do> ", &
		"N", 0%, "'E", "")

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	SELECT SCOPE::SCOPE_EXIT

	!
	! An exit key was typed
	!
	CASE 3%, SMG$K_TRM_CTRLZ, SMG$K_TRM_F10
		GOTO ExitProgram

	!
	! Return, etc. act as next screen
	!
	CASE 10%, 12%, 13%, SMG$K_TRM_F7, SMG$K_TRM_DO

	!
	! Case else
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Alignment

	END SELECT

	GOTO AlignmentReturn IF JUNK$ <> "Y"

	FOR I% = 1% TO FORM_HIGH
		!
		! Print the body of the form
		!
		TEST_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_BODY%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			1%)

                FORM_COUNT% = FORM_COUNT% + 1%

		IF (FORM_COUNT% AND 1%) = 0%
		THEN
			CALL WRIT_STRING(UTL_REPORTX::TOLOCAL, TOLOCAL$)
			PRINT #UTL_REPORTX::CHAN, TOLOCAL$;
			CALL OUTP_FORMFF(UTL_REPORTX)
			CALL WRIT_STRING(UTL_REPORTX::TOSCREEN, TOSCREEN$)
			PRINT #UTL_REPORTX::CHAN, TOSCREEN$;
		ELSE
			CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
				FOR LOOP% = TEST_COUNT% + 1% TO FORM_LENGTH
		END IF

	NEXT I%

	FORM_COUNT% = 0%

	!
	! Do they need another?
	!
	GOTO Alignment

 AlignmentReturn:
	RETURN

	%Page

 SetValue:
	!********************************************************************
	! Set values and print form
	!********************************************************************

	!
	! Page counter
	!
	PAGE_COUNT% = INT(FORM_COUNT% / FORM_HIGH) + 1%

	!
	! Print the body of the Form
	!
	IF (PAGE_COUNT% >= UTL_REPORTX::STARTP) AND &
		((PAGE_COUNT% <= UTL_REPORTX::ENDP) OR (UTL_REPORTX::ENDP = 0%))
	THEN
		TEST_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_BODY%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

                FORM_COUNT% = FORM_COUNT% + 1%

		IF (FORM_COUNT% AND 1%) = 0%
		THEN
			CALL WRIT_STRING(UTL_REPORTX::TOLOCAL, TOLOCAL$)
			PRINT #UTL_REPORTX::CHAN, TOLOCAL$;
			CALL OUTP_FORMFF(UTL_REPORTX)
			CALL WRIT_STRING(UTL_REPORTX::TOSCREEN, TOSCREEN$)
			PRINT #UTL_REPORTX::CHAN, TOSCREEN$;
		ELSE
			CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
				FOR LOOP% = TEST_COUNT% + 1% TO FORM_LENGTH
		END IF

	ELSE
		!
		! Increment form counter
		!
		FORM_COUNT% = FORM_COUNT% + 1%
	END IF

	RETURN

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

	END

20000	SUB FORM_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	!*******************************************************************
	! This function is used to return the value of a field back to the
	! form printer.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Define maps
	!
	RECORD W2_FORM_CDD
		! Company
		STRING	COMP_NAME = 40%
		STRING	COMP_ADD1 = 40%
		STRING	COMP_ADD2 = 40%
		STRING	COMP_CITY = 20%
		STRING	COMP_STATE = 2%
		STRING	COMP_ZIP = 10%
		! Employee
		STRING	EMP_NUM = 10%
		STRING	EMP_NAME = 40%
		STRING	EMP_ADD1 = 40%
		STRING	EMP_ADD2 = 40%
		STRING	EMP_CITY = 20%
		STRING	EMP_STATE = 2%
		STRING	EMP_ZIP = 10%
		STRING	EMP_SSN = 11%
		! Flags
		STRING	STAT_EMP = 1%
		STRING	DECEASED = 1%
		STRING	PEN_PLAN = 1%
		STRING	SICK_PAY = 1%
		STRING	LEGAL_REP = 1%
		STRING	EMP_942 = 1%
		STRING	SUBTOTAL = 1%
		STRING	DEF_COMP = 1%
		STRING	VOID = 1%
		! Other
		REAL	ALL_TIPS
		REAL	AD_EIC
		REAL	FRINGE_BENEFIT
		REAL	QUAL
		REAL	NONQUAL
		REAL	SOCTIP
		! Federal Tax
		REAL	FEDTAX_WH
		REAL	FED_WAGE
		REAL	SSTAX_WH
		REAL	SSTAX_WH_HI
		REAL	SS_WAGE
		REAL	SS_WAGE_HI
		REAL	SS_TIPS
		STRING	FED_REPNO = 20%
		! State Tax
		REAL	STATETAX_WH(7%)
		REAL	STATE_WAGE(7%)
		REAL	STATE_SDI(7%)
		STRING	STATE_ID(7%) = 2%
		STRING	STATE_REPNO(7%) = 20%
		! 401K
		REAL	K401A(5%)
		STRING	K401C(5%) = 1%
		! Local Taxes
		LONG LOCALCOUNT
		STRING LOCALNAME(7%) = 4%
		REAL LOCALWAGE(7%)
		REAL LOCALTAX(7%)
		! OTHER
		REAL	OTHER(5%)
		STRING	OTHERC(5%) = 1%
		REAL	DEP_CARE
	END RECORD

	MAP (W2_OTHER) &
		STATE%, &
		PAGE_COUNT%, &
		SUMMARY%

	!
	! (0) = BLANK
	! (1) = EMPLOYEE
	! (2) = SUBTOTAL
	! (3) = GRAND TOTAL
	!
	MAP (EMP_W2_FORM) &
		W2_FORM_CDD W2_FORM(3%)

	%PAGE

	!
	! Set up default values
	!
	REALVALUE = 0.0
	TEXTVALUE$ = "?????????????????????????????????????"

	!
	! Pick by variable
	!
	SELECT VARNAME$

	!*************************************************************
	! Company
	!*************************************************************
	CASE "COMP_NAME"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::COMP_NAME

	CASE "COMP_ADD1"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::COMP_ADD1

	CASE "COMP_ADD2"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::COMP_ADD2

	CASE "COMP_CITY"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::COMP_CITY

	CASE "COMP_STATE"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::COMP_STATE

	CASE "COMP_ZIP"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::COMP_ZIP

	!************************************************************
	! Employee
	!************************************************************
	CASE "EMP_NUM"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_NUM

	CASE "EMP_NAME"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_NAME

	CASE "EMP_ADD1"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_ADD1

	CASE "EMP_ADD2"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_ADD2

	CASE "EMP_CITY"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_CITY

	CASE "EMP_STATE"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_STATE

	CASE "EMP_ZIP"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_ZIP

	CASE "EMP_SSN"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_SSN

	!************************************************************
	! Flags
	!************************************************************
	CASE "STAT_EMP"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STAT_EMP

	CASE "DECEASED"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::DECEASED

	CASE "PEN_PLAN"
		! Gets an X if have a pension plan (401k, ...)
		TEXTVALUE$ = ""
		TEXTVALUE$ = "X" &
			IF INSTR(1%, " DEFGH", &
			W2_FORM(SUMMARY%)::K401C(I%)) > 1% &
			FOR I% = 1% TO 5%

	CASE "SICK_PAY"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::SICK_PAY

	CASE "LEGAL_REP"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::LEGAL_REP

	CASE "EMP_942"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_942

	CASE "SUBTOTAL"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::SUBTOTAL

	CASE "DEF_COMP"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::DEF_COMP

	CASE "VOID"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::VOID

	!************************************************************
	! Other
	!************************************************************
	CASE "ALL_TIPS"
		REALVALUE = W2_FORM(SUMMARY%)::ALL_TIPS

	CASE "AD_EIC"
		REALVALUE = W2_FORM(SUMMARY%)::AD_EIC

	CASE "DEP_CARE"
		REALVALUE = W2_FORM(SUMMARY%)::DEP_CARE

	CASE "FRINGE_BENEFIT"
		REALVALUE = W2_FORM(SUMMARY%)::FRINGE_BENEFIT

	CASE "NONQUAL"
		REALVALUE = W2_FORM(SUMMARY%)::NONQUAL

	CASE "QUAL"
		REALVALUE = W2_FORM(SUMMARY%)::QUAL

	CASE "SOCTIP"
		REALVALUE = W2_FORM(SUMMARY%)::SOCTIP

	CASE "PAGE_COUNT"
		REALVALUE = PAGE_COUNT%

	!************************************************************
	! Federal tax
	!************************************************************
	CASE "FEDTAX_WH"
		REALVALUE = W2_FORM(SUMMARY%)::FEDTAX_WH

	CASE "FED_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::FED_WAGE

	CASE "SSTAX_WH"
		REALVALUE = W2_FORM(SUMMARY%)::SSTAX_WH

	CASE "SSTAX_WH_HI"
		REALVALUE = W2_FORM(SUMMARY%)::SSTAX_WH_HI

	CASE "SS_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::SS_WAGE

	CASE "SS_WAGE_HI"
		REALVALUE = W2_FORM(SUMMARY%)::SS_WAGE_HI

	CASE "SS_TIPS"
		REALVALUE = W2_FORM(SUMMARY%)::SS_TIPS

	CASE "FED_REPNO"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::FED_REPNO

	!************************************************************
	! State tax
	!************************************************************
	CASE "STATETAX_WH"
		REALVALUE = W2_FORM(SUMMARY%)::STATETAX_WH(1%)
	CASE "STATETAX2_WH"
		REALVALUE = W2_FORM(SUMMARY%)::STATETAX_WH(2%)
	CASE "STATETAX3_WH"
		REALVALUE = W2_FORM(SUMMARY%)::STATETAX_WH(3%)
	CASE "STATETAX4_WH"
		REALVALUE = W2_FORM(SUMMARY%)::STATETAX_WH(4%)
	CASE "STATETAX5_WH"
		REALVALUE = W2_FORM(SUMMARY%)::STATETAX_WH(5%)

	CASE "STATE_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_WAGE(1%)
	CASE "STATE2_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_WAGE(2%)
	CASE "STATE3_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_WAGE(3%)
	CASE "STATE4_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_WAGE(4%)
	CASE "STATE5_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_WAGE(5%)

	CASE "STATE_ID"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_ID(1%)
	CASE "STATE2_ID"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_ID(2%)
	CASE "STATE3_ID"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_ID(3%)
	CASE "STATE4_ID"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_ID(4%)
	CASE "STATE5_ID"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_ID(5%)

	CASE "STATE_SDI"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_SDI(1%)
	CASE "STATE2_SDI"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_SDI(2%)
	CASE "STATE3_SDI"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_SDI(3%)
	CASE "STATE4_SDI"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_SDI(4%)
	CASE "STATE5_SDI"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_SDI(5%)

	CASE "STATE_REPNO"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_REPNO(1%)
	CASE "STATE2_REPNO"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_REPNO(2%)
	CASE "STATE3_REPNO"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_REPNO(3%)
	CASE "STATE4_REPNO"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_REPNO(4%)
	CASE "STATE5_REPNO"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_REPNO(5%)

	CASE "LOCAL_NAME"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::LOCALNAME(1%)
	CASE "LOCAL2_NAME"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::LOCALNAME(2%)
	CASE "LOCAL3_NAME"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::LOCALNAME(3%)

	CASE "LOCAL_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::LOCALWAGE(1%)
 !
 ! Use montanna wage
 !
 !		REALVALUE = W2_FORM(SUMMARY%)::STATE_WAGE(I%) &
 !			IF W2_FORM(SUMMARY%)::STATE_ID(I%) = "MT" AND &
 !			W2_FORM(SUMMARY%)::LOCALTAX(1%) <> 0.0 &
 !			FOR I% = 1% TO 5%

	CASE "LOCAL2_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::LOCALWAGE(2%)
	CASE "LOCAL3_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::LOCALWAGE(3%)

	CASE "LOCAL_TAX"
		REALVALUE = W2_FORM(SUMMARY%)::LOCALTAX(1%)
	CASE "LOCAL2_TAX"
		REALVALUE = W2_FORM(SUMMARY%)::LOCALTAX(2%)
	CASE "LOCAL3_TAX"
		REALVALUE = W2_FORM(SUMMARY%)::LOCALTAX(3%)

	!***********************************************************
	! 401K
	!***********************************************************
	CASE "K401C1"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::K401C(1%)
	CASE "K401A1"
		REALVALUE = W2_FORM(SUMMARY%)::K401A(1%)
	CASE "K401C2"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::K401C(2%)
	CASE "K401A2"
		REALVALUE = W2_FORM(SUMMARY%)::K401A(2%)
	CASE "K401C3"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::K401C(3%)
	CASE "K401A3"
		REALVALUE = W2_FORM(SUMMARY%)::K401A(3%)
	CASE "K401C4"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::K401C(4%)
	CASE "K401A4"
		REALVALUE = W2_FORM(SUMMARY%)::K401A(4%)
	CASE "K401C5"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::K401C(5%)
	CASE "K401A5"
		REALVALUE = W2_FORM(SUMMARY%)::K401A(5%)

	!***********************************************************
	! OTHER
	!***********************************************************
	CASE "OTHERC", "OTHER1C"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::OTHERC(1%)
	CASE "OTHER", "OTHER1"
		REALVALUE = W2_FORM(SUMMARY%)::OTHER(1%)
	CASE "OTHER2C"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::OTHERC(2%)
	CASE "OTHER2"
		REALVALUE = W2_FORM(SUMMARY%)::OTHER(2%)
	CASE "OTHER3C"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::OTHERC(3%)
	CASE "OTHER3"
		REALVALUE = W2_FORM(SUMMARY%)::OTHER(3%)
	CASE "OTHER4C"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::OTHERC(4%)
	CASE "OTHER4"
		REALVALUE = W2_FORM(SUMMARY%)::OTHER(4%)
	CASE "OTHER5C"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::OTHERC(5%)
	CASE "OTHER5"
		REALVALUE = W2_FORM(SUMMARY%)::OTHER(5%)

	END SELECT

	END SUB
	!+-+-+
	!++
	! Abstract:YEAR
	!	.ts 55
	!	^*Year	YYYY\*
	!	.b
	!	.lm +5
	!	The ^*Year\* field allows for entry of the year for which this report is
	!	to print.
	!	.lm -5
	!
	! Index:
	!	.x Year>Print W-2 Forms
	!	.x Print W-2 Forms>Year
	!
	!--
