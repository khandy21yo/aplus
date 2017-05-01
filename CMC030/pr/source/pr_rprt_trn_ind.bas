1	%TITLE "Payroll Indirect Register Report"
	%SBTTL "PR_RPRT_TRN_IND"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! ID:PR008
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Employer Payroll Taxes\* report option lists the following
	!	information for a specific payroll folder:
	!	.table 3,25
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name
	!	.te
	!	Earnings Subject to FICA Tax
	!	.lm +3
	!	(for each employee)
	!	.lm -3
	!	.te
	!	Earnings Subject to Federal Unemployment Tax
	!	.lm +3
	!	(for each employee)
	!	.lm -3
	!	.te
	!	Earnings Subject to State Unemployment Tax
	!	.lm +3
	!	(for each employee)
	!	.lm -3
	!	.te
	!	Total Earnings Subject to FICA Tax
	!	.te
	!	Total Earnings Subject to Federal Unemployment Tax
	!	.te
	!	Total Earnings Subject to State Unemployment Tax
	!	.te
	!	FICA Expense Calculation
	!	.te
	!	Federal Unemployment Expense Calculation
	!	.te
	!	State Unemployment Expense Calculation
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Employer Payroll Taxes>Report
	!	.x Report>Employer Payroll Taxes
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_IND/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_IND, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_IND.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	03/17/89 - Kevin Handy
	!		Modified to handle taxable/nontaxable flag correctly.
	!
	!	03/17/89 - Kevin Handy
	!		Modified to handle taxable difference between
	!		earnings and deductions.
	!
	!	04/25/89 - Kevin Handy
	!		Fixed bug where taxable/nontaxable from history
	!		was handled backwards for ytd amt earned.
	!
	!	04/26/89 - Kevin Handy
	!		Fixed bug where it was adding updated amounts to YTD
	!		instead of subtracting them.
	!
	!	05/29/90 - Kevin Handy
	!		Modified handling of the FACTOR calculation to
	!		spread around the non-taxable amounts.
	!
	!	06/04/90 - Kevin Handy
	!		Fixed minor bug in 05/29/90 modification.
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent either to a spreadsheet or to a DIF file.
	!
	!	09/27/90 - Kevin Handy
	!		Worked on bug where SUTA limit was not working
	!		correctly.
	!
	!	01/11/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in the error trapping.
	!
	!	01/17/91 - Kevin Handy
	!		Added column for HI part of fica.
	!
	!	05/31/91 - Kevin Handy
	!		Unrolled initilization for-next loops so there is
	!		only one set of loops instead of several.
	!
	!	11/05/91 - Kevin Handy
	!		Modified so that SUI_MAX of 0.0 will mean that there
	!		is no limit instead of meaning that the limit is 0.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to only look at "P" and "D" types in
	!		PR_REG_ERNDED (Ignore "A" types).
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	12/01/92 - Kevin Handy
	!		Defined SI as a tax type.
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Removed SUBJECT% parameter from PR_READ_SUBJTAX.
	!		Removed disable_unsolicited_input call.
	!
	!	05/17/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	05/22/95 - Kevin Handy
	!		Sort error trapping.
	!
	!	09/12/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/19/97 - Kevin Handy
	!		Handle FH code.
	!
	!	05/12/97 - Kevin Handy
	!		Use one more digit for fica rates
	!
	!	05/17/97 - Kevin Handy
	!		Fix bug looking up state info.
	!
	!	05/29/97 - Kevin Handy
	!		More bugs with SUI code
	!
	!	07/24/97 - Kevin Handy
	!		Allow old FICA rates to work until I get
	!		everyone switched over.
	!
	!	05/29/98 - Kevin Handy
	!		Modified to handle new 'F' final deduction type
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE				UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)		PR_TRN_PAY_CDD		PR_TRN_PAY
	MAP	(PR_HIS_PAY)		PR_TRN_PAY_CDD		PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP	(PR_TRN_DED)		PR_TRN_DED_CDD		PR_TRN_DED
	MAP	(PR_HIS_DED)		PR_TRN_DED_CDD		PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP	(PR_REG_TAXES)		PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP	(PR_REG_ERNDED)		PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)		PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP	(PR_TAX_TABLE)		PR_TAX_TABLE_CDD	PR_TAX_TABLE

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

	!
	! Need to include _F version so that variable length record
	! business will work.
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP	(PR_TAX_PKG)		PR_TAX_PKG_CDD		PR_TAX_PKG

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE
	DECLARE INTEGER CONSTANT MAX_TAX = 5
	DECLARE INTEGER CONSTANT MAX_STATE = 12

	!
	! Dimension
	!
	DIM EMP_NT_CUR(MAX_TAX), &
		EMP_NT_YTD(MAX_TAX), &
		EMP_WH_CODE$(MAX_TAX, MAX_STATE), &
		EMP_WH_CODE%(MAX_TAX), &
		SUI_CODE$(30%), &
		TOTAL_SUI_WAGES(30%), &
		SUI_PCT(30%), &
		SUI_MAX(30%), &
		PKG_WH_CODE$(MAX_TAX), &
		EMP_SUBJ_CUR(MAX_TAX, MAX_STATE), &
		EMP_SUBJ_YTD(MAX_TAX, MAX_STATE), &
		EMP_SUBJ_DED(MAX_TAX, MAX_STATE)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Set variables to initial values
	!
	SUBJECT_TYPE_TABLE$ = "FIR!FHR!FUI!SUI!OST!"
	TAX_TYPE_TABLE$ = "FI!FH!FW!SW!SX!CW!DW!EW!SU!SI!"

 Init:
	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Payroll Date\*
	!	.p
	!	The ^*Payroll Date\* field entesr the date
	!	of the particular payroll which is to be printed.
	!	.p
	!	This field requires an entry. The format for entry is
	!	MMDDYYYY or MMDDYY.
	!	.note
	!	Because this program uses the payroll register in calculating
	!	when limits are exceeded, you should run this report for a folder
	!	only after all previous folders have been updated, and before
	!	future folders are updated.
	!	.end note
	!
	! Index:
	!	.x Payroll Date>Employer Payroll Taxes
	!	.x Employer Payroll Taxes>Payroll Date
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)
	YYYY$ = LEFT(BATCH_NO$, 4%)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.p
	!	The ^*From Item\* number field causes
	!	the printing to begin with this
	!	particular item. The value must be in agreement with
	!	field (04).
	!	.p
	!	A blank field will cause the report to start with the first
	!	item in the file.
	!
	! Index:
	!	.x From Item>Employer Payroll Taxes
	!	.x Employer Payroll Taxes>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To Item\* number field causes the
	!	report to end printing with a particular item.
	!	The value must be in agreement with field (04).
	!	.p
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!
	! Index:
	!	.x To Item>Employer Payroll Taxes
	!	.x Employer Payroll Taxes>To Item
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Sort (NU,NA,SO,LO)\*
	!	.p
	!	The ^*Sort\* code field causes the report
	!	to print in a particular order.
	!	.p
	!	The valid sort codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	NA = Name
	!	.le
	!	SO = Alphbetical (last name first)
	!	.le
	!	LO = Location
	!	.els
	!	.lm -10
	!	.p
	!	An entry is required in this field, and only the codes listed
	!	above are valid.
	!
	! Index:
	!	.x Sort>Employer Payroll Taxes
	!	.x Employer Payroll Taxes>Sort
	!
	!--


	SELECT SORTBY$

	CASE "NU"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))

	CASE "NA"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))

	CASE "SN"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SSN))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SSN))

	CASE "LO"
		K_NUM% = 4%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::LOCATION))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::LOCATION))

	CASE ELSE
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SORT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SORT))

	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		CONTINUE 315 IF ERR = 5%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	GOTO 320

315	!
	! Open Pay history folder if journal not found
	!
	USE_HISTORY% = -1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	PR_TRN_PAY.CH% = PR_HIS_PAY.CH%

320	!
	! Open Deduction folder
	!
	WHEN ERROR IN
		IF USE_HISTORY% = 0%
		THEN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
		ELSE
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"
			PR_HIS_DED.CH% = PR_TRN_DED.CH%
		END IF
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

330	!
	! Open ERNDED Definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

340	!
	! Open TaxWH register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

350	!
	! Open ERNDED register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

360	!
	! Open Tax Table file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"

		GET #PR_TAX_TABLE.CH%, KEY #0% EQ "F", REGARDLESS
	USE
		CONTINUE 370 IF ERR = 5% OR ERR = 155%
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	FICA_EMPR_PCT = (PR_TAX_TABLE::FICA_EMPR_PCT) / 10000.0
	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT

	FICA_EMPR_PCT_HI = (PR_TAX_TABLE::FICA_EMPR_PCT_HI) / 10000.0
	FICA_LIMIT_HI = PR_TAX_TABLE::FICA_LIMIT_HI

	IF FICA_EMPR_PCT > 0.100
	THEN
		FICA_EMPR_PCT = FICA_EMPR_PCT / 10.0
		FICA_EMPR_PCT_HI = FICA_EMPR_PCT_HI / 10.0
	END IF

	CLOSE PR_TAX_TABLE.CH%

370	!
	! Open Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"
	USE
		CONTINUE 380 IF ERR = 5%
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

375	WHEN ERROR IN
		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "F  ", REGARDLESS
	USE
		CONTINUE 380 IF ERR = 155%
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	FUI_PCT = PR_TAX_PROFILE_F::FUI_PCT / 100.0
	FUI_MAX = PR_TAX_PROFILE_F::FUI_MAX

380	!
	! Open Tax package
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
	USE
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Employer Payroll Tax Expense Report"
	TITLE$(2%) = "For the Payroll Folder Dated:  " + &
		PRNT_DATE(BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = SPACE$(42%) + &
		"----------------Subject Pay----------------"
	TITLE$(5%) = "EmpNum     EmpName                        " + &
		"      FICA         HI        FUI        SUI        OST"
	TITLE$(6%) = "."

	!
	! Line layouts
	!
	LYT_LINE = "$EmpNum:010,$EmpName:041,VFICA:052,VHI:063,VFUI:074," + &
		"VSUI:085,VOST:096"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_EMP_MASTER.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_EMP_MASTER.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$

	CASE "NU"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "NA"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "SN"
		GOTO ExitTotal IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "DP"
		GOTO ExitTotal IF (PR_EMP_MASTER::DEPT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	FOR I% = 1% TO MAX_TAX
		FOR J% = 0% TO MAX_STATE
			EMP_SUBJ_CUR(I%, J%) = 0.0
			EMP_SUBJ_YTD(I%, J%) = 0.0
			EMP_SUBJ_DED(I%, J%) = 0.0
			EMP_WH_CODE$(I%, J%) = ""
		NEXT J%
	NEXT I%

	FOR I% = 1% TO MAX_TAX
		EMP_NT_CUR(I%) = 0.0
		EMP_NT_YTD(I%) = 0.0
		EMP_WH_CODE%(I%) = 0%
	NEXT I%

	!
	! Set up for fica and fui
	!
	EMP_WH_CODE%(1%) = 1%
	EMP_WH_CODE%(2%) = 1%


17100	!
	! Get pay detail information
	!
	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17550 IF ERR = 155%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

17110	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! If history the set history map into journal
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	GOTO 17110 IF PR_TRN_PAY::PTYPE = "A"

	GOTO 17200 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM)

17120	PKG_WH_CODE$(4%) = EDIT$(PR_EMP_MASTER::SUI_SW, -1%)

	IF PKG_WH_CODE$(4%) = ""
	THEN
		WHEN ERROR IN
			GET #PR_TAX_PKG.CH%, &
				KEY #0% EQ PR_TRN_PAY::TAX_PKG + "SW", &
				REGARDLESS
		USE
			CONTINUE 17130 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TAX_PKG"
			CONTINUE HelpError
		END WHEN

		PKG_WH_CODE$(4%) = PR_TAX_PKG::CODE
	END IF

17130	PKG_WH_CODE$(5%) = ""

	WHEN ERROR IN
		GET #PR_TAX_PKG.CH%, &
			KEY #0% EQ PR_TRN_PAY::TAX_PKG + "SX", &
			REGARDLESS
	USE
		CONTINUE 17140 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

	PKG_WH_CODE$(5%) = PR_TAX_PKG::CODE

17140	FOR TAX_TYPE% = 1% TO MAX_TAX
		!
		! See if taxable
		!
		SUBJECT_CODE$ = MID(SUBJECT_TYPE_TABLE$, &
			(TAX_TYPE% - 1%) * 4% + 1%, 3%)

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			SUBJECT_CODE$, &
			"P", &
			PR_TRN_PAY::CODE, &
			TAXABLE%, &
			REPORTABLE%)

		GOTO 17170 IF TAXABLE%

		WH_LOOP% = 1%

		IF TAX_TYPE% > 3%
		THEN
			GOTO 17150 IF PKG_WH_CODE$(TAX_TYPE%) = &
				EMP_WH_CODE$(TAX_TYPE%, WH_LOOP%) &
				FOR WH_LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%)

			EMP_WH_CODE%(TAX_TYPE%), WH_LOOP% = &
				EMP_WH_CODE%(TAX_TYPE%) + 1%
			EMP_WH_CODE$(TAX_TYPE%, WH_LOOP%) = &
				PKG_WH_CODE$(TAX_TYPE%)

		END IF

17150		EMP_SUBJ_CUR(TAX_TYPE%, WH_LOOP%) = &
			FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, WH_LOOP%) + &
			PR_TRN_PAY::GROSS, 2%)

		IF (PR_TRN_PAY::UPDATE_FLAG AND 1%) = 1%
		THEN
			!
			! Subtract out of YTD if was added there by update
			!
			EMP_SUBJ_YTD(TAX_TYPE%, WH_LOOP%) = &
				FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, WH_LOOP%) - &
				PR_TRN_PAY::GROSS, 2%)
		END IF

		EMP_SUBJ_CUR(TAX_TYPE%, 0%) = &
			FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, 0%) + &
			PR_TRN_PAY::GROSS, 2%)

17170	NEXT TAX_TYPE%

	GOTO 17110

17200	!-------------------------------------------------------------------
	! Get Tax/Ded detail information
	!-------------------------------------------------------------------

	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

17210	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	!
	! If history the set history map into journal
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_DED = PR_HIS_DED
	END IF

	IF PR_EMP_MASTER::EMPNUM <> PR_TRN_DED::EMPNUM
	THEN
		GOTO 17300
	END IF

	!
	! See if this is a tax
	!
	GOTO 17290 IF INSTR(1%, TAX_TYPE_TABLE$, PR_TRN_DED::CODE)

	!
	! Check Deductions
	!
	GOTO 17270 IF PR_TRN_DED::DTYPE = "T" OR PR_TRN_DED::DTYPE = "M"

	FOR TAX_TYPE% = 1% TO MAX_TAX

		!
		! See if taxable and reportable
		!
		SUBJECT_CODE$ = MID(SUBJECT_TYPE_TABLE$, &
			(TAX_TYPE% - 1%) * 4% + 1%, 3%)

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			SUBJECT_CODE$, &
			PR_TRN_DED::DTYPE, &
			PR_TRN_DED::CODE, &
			TAXABLE%, &
			REPORTABLE%)

		!
		! Must handle deductions different from earnings.
		! Nontaxable deductions are added to amount,
		! nontaxable earnings are not.
		!
		IF (TAXABLE% = 0% AND PR_TRN_DED::DTYPE <> "D" AND &
			PR_TRN_DED::DTYPE <> "F") OR &
			(TAXABLE% <> 0% AND (PR_TRN_DED::DTYPE = "D" OR &
			PR_TRN_DED::DTYPE = "F"))
		THEN
			IF TAX_TYPE% < 4%
			THEN
				EMP_SUBJ_CUR(TAX_TYPE%, 1%) = &
					FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, 1%) - &
						PR_TRN_DED::AMOUNT, 2%)

				IF (PR_TRN_DED::UPDATE_FLAG AND 1%) = 1%
				THEN
					EMP_SUBJ_YTD(TAX_TYPE%, WH_LOOP%) = &
						FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, &
						WH_LOOP%) + &
						PR_TRN_DED::AMOUNT, 2%)
				END IF
			ELSE
				EMP_NT_CUR(TAX_TYPE%) = &
					FUNC_ROUND(EMP_NT_CUR(TAX_TYPE%) + &
					PR_TRN_DED::AMOUNT, 2%)

				IF (PR_TRN_DED::UPDATE_FLAG AND 1%) = 1%
				THEN
					EMP_NT_YTD(TAX_TYPE%) = &
						FUNC_ROUND(EMP_NT_YTD(TAX_TYPE%) - &
						PR_TRN_DED::AMOUNT, 2%)
				END IF
			END IF
		END IF

	NEXT TAX_TYPE%

	GOTO 17290

17270	!
	! Add noncompensation items
	!


17290	!
	! Loop back for next deduction record
	!
	GOTO 17210

17300	!****************************************************************
	! Look up state taxable wages
	!****************************************************************

	TAX_TYPE% = 4%

	WHEN ERROR IN
		FIND #PR_REG_TAXES.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17400 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

17320	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE 17400 IF ERR = 11%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 17400 IF PR_REG_TAXES::EMPNUM <> PR_EMP_MASTER::EMPNUM

	GOTO 17320 IF PR_REG_TAXES::TTYPE <> "SW"

	PR_REG_TAXES::CODE = PR_EMP_MASTER::SUI_SW &
		IF EDIT$(PR_EMP_MASTER::SUI_SW, -1%) <> ""

	GOTO 17330 &
		IF PR_REG_TAXES::CODE = EMP_WH_CODE$(TAX_TYPE%, WH_LOOP%) &
		FOR WH_LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%)

	EMP_WH_CODE%(TAX_TYPE%), WH_LOOP% = EMP_WH_CODE%(TAX_TYPE%) + 1%
	EMP_WH_CODE$(TAX_TYPE%, WH_LOOP%) = PKG_WH_CODE$(TAX_TYPE%)

17330	EMP_SUBJ_YTD(TAX_TYPE%, WH_LOOP%) = &
		FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, WH_LOOP%) + &
		PR_REG_TAXES::REPORTABLE(I%), 2%) &
		FOR I% = 0% TO 3%

	GOTO 17320

17400	!--------------------------------------------------------------------
	! Get subject wages from ernded file for FICA and FUI.
	! Get Not taxable wages for SUI
	!--------------------------------------------------------------------
	WHEN ERROR IN
		FIND #PR_REG_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17500 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

17420	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 17500 IF ERR = 11%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 17500 IF PR_REG_ERNDED::EMPNUM <> PR_EMP_MASTER::EMPNUM

	GOTO 17420 IF PR_REG_ERNDED::ETYPE <> "P" &
		AND PR_REG_ERNDED::ETYPE <> "D" AND &
		PR_REG_ERNDED::ETYPE <> "F"

	FOR TAX_TYPE% = 1% TO MAX_TAX

		!
		! See if taxable and reportable
		!
		SUBJECT_CODE$ = MID(SUBJECT_TYPE_TABLE$, &
			(TAX_TYPE% - 1%) * 4% + 1%, 3%)

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			SUBJECT_CODE$, &
			PR_REG_ERNDED::ETYPE, &
			PR_REG_ERNDED::CODE, &
			TAXABLE%, &
			REPORTABLE%)

		TOTAL = 0.0

		TOTAL = FUNC_ROUND(TOTAL + PR_REG_ERNDED::QTR_DOLL(LOOP%), 2%) &
			FOR LOOP% = 0% TO 3%

		IF TAX_TYPE% < 4%
		THEN
			EMP_SUBJ_YTD(TAX_TYPE%, 1%) = &
				FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, 1%) + &
				TOTAL, 2%) &
				IF PR_REG_ERNDED::ETYPE <> "D" AND &
				PR_REG_ERNDED::ETYPE <> "F" AND TAXABLE% = 0%

			EMP_SUBJ_YTD(TAX_TYPE%, 1%) = &
				FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, 1%) - &
				TOTAL, 2%) &
				IF (PR_REG_ERNDED::ETYPE = "D" OR &
				PR_REG_ERNDED::ETYPE = "F") AND TAXABLE%

		ELSE
			EMP_NT_YTD(TAX_TYPE%) = &
				FUNC_ROUND(EMP_NT_YTD(TAX_TYPE%) - &
				TOTAL, 2%) &
				IF PR_REG_ERNDED::ETYPE <> "D" AND &
				PR_REG_ERNDED::ETYPE <> "F" AND TAXABLE% = 0%

			EMP_NT_YTD(TAX_TYPE%) = &
				FUNC_ROUND(EMP_NT_YTD(TAX_TYPE%) + &
				TOTAL, 2%) &
				IF (PR_REG_ERNDED::ETYPE = "D" OR &
				PR_REG_ERNDED::ETYPE = "F") AND TAXABLE%

		END IF

	NEXT TAX_TYPE%

	GOTO 17420

17500	!
	! Allocate taxable wages for the current period
	!
	FOR TAX_TYPE% = 4% TO MAX_TAX

		FACTOR = 1.0
		FACTOR = 1.0 - (EMP_NT_CUR(TAX_TYPE%) / &
			EMP_SUBJ_CUR(TAX_TYPE%, 0%)) &
			IF EMP_SUBJ_CUR(TAX_TYPE%, 0%) <> 0.0

		TOTAL_TO_DIST = FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, 0%) - &
			EMP_NT_CUR(TAX_TYPE%), 2%)

		FOR LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%) - 1%
			EMP_SUBJ_CUR(TAX_TYPE%, LOOP%) = &
				FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, LOOP%) * &
				FACTOR, 2%)
			TOTAL_TO_DIST = FUNC_ROUND(TOTAL_TO_DIST - &
				EMP_SUBJ_CUR(TAX_TYPE%, LOOP%), 2%)
		NEXT LOOP%

		EMP_SUBJ_CUR(TAX_TYPE%, EMP_WH_CODE%(TAX_TYPE%)) = &
			FUNC_ROUND(TOTAL_TO_DIST, 2%)

	NEXT TAX_TYPE%

	!
	! Allocate taxable wages YTD
	!
	FOR TAX_TYPE% = 4% TO MAX_TAX

		FACTOR = 1.0
		FACTOR = 1.0 - (EMP_NT_YTD(TAX_TYPE%) / &
			EMP_SUBJ_YTD(TAX_TYPE%, 0%)) &
			IF EMP_SUBJ_YTD(TAX_TYPE%, 0%) <> 0.0

		TOTAL_TO_DIST = FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, 0%) - &
			EMP_NT_YTD(TAX_TYPE%), 2%)

		FOR LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%) - 1%
			EMP_SUBJ_YTD(TAX_TYPE%, LOOP%) = &
				FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, LOOP%) * &
				FACTOR, 2%)
			TOTAL_TO_DIST = FUNC_ROUND(TOTAL_TO_DIST - &
				EMP_SUBJ_YTD(TAX_TYPE%, LOOP%), 2%)
		NEXT LOOP%

		EMP_SUBJ_YTD(TAX_TYPE%, EMP_WH_CODE%(TAX_TYPE%)) = &
			FUNC_ROUND(TOTAL_TO_DIST, 2%)

	NEXT TAX_TYPE%

17510	!--------------------------------------------------------------------
	! Determine FICA Taxes
	!--------------------------------------------------------------------

	FICA_WAGES = 0.0

	EMP_SUBJ_YTD(1%, 1%) = EMP_SUBJ_YTD(1%, 1%) + EMP_SUBJ_CUR(1%, 1%)

	IF EMP_SUBJ_YTD(1%, 1%) - EMP_SUBJ_CUR(1%, 1%) < FICA_LIMIT
	THEN
		FICA_EXCESS = EMP_SUBJ_YTD(1%, 1%) - FICA_LIMIT
		FICA_EXCESS = 0.0 IF FICA_EXCESS < 0.0
		FICA_WAGES = EMP_SUBJ_CUR(1%, 1%) - FICA_EXCESS
	END IF

	TOTAL_FICA_WAGES = TOTAL_FICA_WAGES + FICA_WAGES

	!--------------------------------------------------------------------
	! Determine FICA Taxes (HI)
	!--------------------------------------------------------------------

	FICA_WAGES_HI = 0.0

	EMP_SUBJ_YTD(2%, 1%) = EMP_SUBJ_YTD(2%, 1%) + EMP_SUBJ_CUR(2%, 1%)

	IF EMP_SUBJ_YTD(2%, 1%) - EMP_SUBJ_CUR(2%, 1%) < FICA_LIMIT_HI
	THEN
		FICA_EXCESS_HI = EMP_SUBJ_YTD(2%, 1%) - FICA_LIMIT_HI
		FICA_EXCESS_HI = 0.0 IF FICA_EXCESS_HI < 0.0
		FICA_WAGES_HI = EMP_SUBJ_CUR(2%, 1%) - FICA_EXCESS_HI
	END IF

	TOTAL_FICA_WAGES_HI = TOTAL_FICA_WAGES_HI + FICA_WAGES_HI

17520	!--------------------------------------------------------------------
	! Determine FUI Taxes
	!--------------------------------------------------------------------

	FUI_WAGES = 0.0

	EMP_SUBJ_YTD(3%, 1%) = EMP_SUBJ_YTD(3%, 1%) + EMP_SUBJ_CUR(3%, 1%)

	IF EMP_SUBJ_YTD(3%, 1%) - EMP_SUBJ_CUR(3%, 1%) < FUI_MAX
	THEN
		FUI_EXCESS = EMP_SUBJ_YTD(3%, 1%) - FUI_MAX
		FUI_EXCESS = 0.0 IF FUI_EXCESS < 0.0
		FUI_WAGES = EMP_SUBJ_CUR(3%, 1%) - FUI_EXCESS
	END IF

	TOTAL_FUI_WAGES = TOTAL_FUI_WAGES + FUI_WAGES

17530	!-------------------------------------------------------------------
	! Detemine SUI taxes
	!-------------------------------------------------------------------

	SUI_WAGES = 0.0

	FOR LOOP% = 1% TO EMP_WH_CODE%(4%)
		GOTO 17540 IF SUI_CODE$(I%) = EMP_WH_CODE$(4%, LOOP%) &
			FOR I% = 1% TO SUI_LOOP%

		SUI_LOOP%,I% = SUI_LOOP% + 1%
		SUI_CODE$(I%) = EMP_WH_CODE$(4%, LOOP%)
		SUI_PCT(I%) = 0.0
		SUI_MAX(I%) = 0.0

		WHEN ERROR IN
			GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "S" + &
				EMP_WH_CODE$(4%, LOOP%), REGARDLESS
		USE
			CONTINUE 17540 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TAX_PROFILE"
			CONTINUE HelpError
		END WHEN

		SUI_PCT(I%) = PR_TAX_PROFILE_S::SUI_PCT / 100.0
		SUI_MAX(I%) = PR_TAX_PROFILE_S::SUI_MAX

17540		EMP_SUBJ_YTD(4%, LOOP%) = EMP_SUBJ_YTD(4%, LOOP%) + &
			EMP_SUBJ_CUR(4%, LOOP%)

		IF (EMP_SUBJ_YTD(4%, LOOP%) - &
			EMP_SUBJ_CUR(4%, LOOP%) < SUI_MAX(I%)) OR &
			(SUI_MAX(I%) = 0.0)
		THEN
			IF (SUI_MAX(I%) = 0.0)
			THEN
				SUI_EXCESS = 0.0
			ELSE
				SUI_EXCESS = EMP_SUBJ_YTD(4%, LOOP%) - &
					SUI_MAX(I%)
				SUI_EXCESS = 0.0 IF SUI_EXCESS < 0.0
			END IF
			SUI_WAGES = FUNC_ROUND(SUI_WAGES + &
				EMP_SUBJ_CUR(4%, LOOP%) - &
				SUI_EXCESS, 2%)
			TOTAL_SUI_WAGES(I%) = FUNC_ROUND(TOTAL_SUI_WAGES(I%) + &
				EMP_SUBJ_CUR(4%, LOOP%) - SUI_EXCESS, 2%)
			TOTAL_SUI_WAGES = FUNC_ROUND(TOTAL_SUI_WAGES + &
				EMP_SUBJ_CUR(4%, LOOP%) - SUI_EXCESS, 2%)
		END IF

	NEXT LOOP%

17542	!-------------------------------------------------------------------
	! Detemine OST taxes
	!-------------------------------------------------------------------

	OST_WAGES = 0.0

	FOR LOOP% = 1% TO EMP_WH_CODE%(5%)
		GOTO 17545 IF OST_CODE$(I%) = EMP_WH_CODE$(5%, LOOP%) &
			FOR I% = 1% TO OST_LOOP%

		OST_LOOP%,I% = OST_LOOP% + 1%
		OST_CODE$(I%) = EMP_WH_CODE$(5%, LOOP%)
		OST_PCT(I%) = 0.0
		OST_MAX(I%) = 0.0
		OST_DEDMAX(I%) = 0.0

		WHEN ERROR IN
			GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "S" + &
				EMP_WH_CODE$(5%, LOOP%), REGARDLESS
		USE
			CONTINUE 17545 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TAX_PROFILE"
			CONTINUE HelpError
		END WHEN

		OST_PCT(I%) = PR_TAX_PROFILE_S::OST_PCT / 100.0
		OST_MAX(I%) = PR_TAX_PROFILE_S::OST_MAX
		OST_DEDMAX(I%) = &
			PR_TAX_PROFILE_S::OST_DEDMAX / PR_EMP_MASTER::PAYFREQ

17545		IF OST_MAX(I%) <> 0.0
		THEN
			EMP_SUBJ_YTD(5%, LOOP%) = EMP_SUBJ_YTD(5%, LOOP%) + &
				EMP_SUBJ_CUR(5%, LOOP%)
			IF EMP_SUBJ_YTD(5%, LOOP%) - EMP_SUBJ_CUR(5%, LOOP%) < &
				OST_MAX(I%)
			THEN
				OST_EXCESS = EMP_SUBJ_YTD(5%, LOOP%) - &
					OST_MAX(I%)
				OST_EXCESS = 0.0 IF OST_EXCESS < 0.0
				OST_WAGES = FUNC_ROUND(OST_WAGES + &
					EMP_SUBJ_CUR(5%, LOOP%) - &
					OST_EXCESS, 2%)
				TOTAL_OST_WAGES(I%) = &
					FUNC_ROUND(TOTAL_OST_WAGES(I%) + &
					EMP_SUBJ_CUR(5%, LOOP%) - &
					OST_EXCESS, 2%)
				TOTAL_OST_WAGES = FUNC_ROUND(TOTAL_OST_WAGES + &
					EMP_SUBJ_CUR(5%, LOOP%) - &
					OST_EXCESS, 2%)
			END IF
		ELSE
			EMP_SUBJ_DED(5%, LOOP%) = EMP_SUBJ_DED(5%, LOOP%) + &
				EMP_SUBJ_CUR(5%, LOOP%)
			IF EMP_SUBJ_DED(5%, LOOP%) - EMP_SUBJ_CUR(5%, LOOP%) < &
				OST_DEDMAX(I%)
			THEN
				OST_EXCESS = EMP_SUBJ_DED(5%, LOOP%) - &
					OST_DEDMAX(I%)
				OST_EXCESS = 0.0 IF OST_EXCESS < 0.0
				OST_WAGES = FUNC_ROUND(OST_WAGES + &
					EMP_SUBJ_CUR(5%, LOOP%) - &
					OST_EXCESS, 2%)
				TOTAL_OST_WAGES(I%) = &
					FUNC_ROUND(TOTAL_OST_WAGES(I%) + &
					EMP_SUBJ_CUR(5%, LOOP%) - OST_EXCESS, 2%)
				TOTAL_OST_WAGES = FUNC_ROUND(TOTAL_OST_WAGES + &
					EMP_SUBJ_CUR(5%, LOOP%) - OST_EXCESS, 2%)
			END IF
		END IF

	NEXT LOOP%

	!--------------------------------------------------------------------
	! Print subject data
	!--------------------------------------------------------------------

	TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + " " + &
		FORMAT$(FICA_WAGES, "#######.## ") + &
		FORMAT$(FICA_WAGES_HI, "#######.## ") + &
		FORMAT$(FUI_WAGES, "#######.## ") + &
		FORMAT$(SUI_WAGES, "#######.## ") + &
		FORMAT$(OST_WAGES, "#######.##")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17550	!
	! Next employee
	!
	GOTO 17020

	%Page

 ExitTotal:
	!
	! Handle end of report
	!
	TEXT$ = LEFT("Total Subject Wages " + SPACE$(42%), 42%) + &
		FORMAT$(TOTAL_FICA_WAGES, "#######.## ") + &
		FORMAT$(TOTAL_FICA_WAGES_HI, "#######.## ") + &
		FORMAT$(TOTAL_FUI_WAGES, "#######.## ") + &
		FORMAT$(TOTAL_SUI_WAGES, "#######.## ") + &
		FORMAT$(TOTAL_OST_WAGES, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "FICA OASDI Expense" + &
		FORMAT$(FUNC_ROUND(FICA_EMPR_PCT * TOTAL_FICA_WAGES, 2%), &
		"###,###.## ") + &
		FORMAT$(TOTAL_FICA_WAGES, "###,###.## ") + &
		FORMAT$(FICA_EMPR_PCT * 100.0, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "FICA HI Expense   " + &
		FORMAT$(FUNC_ROUND(FICA_EMPR_PCT_HI * &
		TOTAL_FICA_WAGES_HI, 2%), "###,###.## ") + &
		FORMAT$(TOTAL_FICA_WAGES_HI, "###,###.## ") + &
		FORMAT$(FICA_EMPR_PCT_HI * 100.0, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "FUI Expense       " + &
		FORMAT$(FUNC_ROUND(FUI_PCT * TOTAL_FUI_WAGES, 2%), &
		"###,###.## ") + &
		FORMAT$(TOTAL_FUI_WAGES, "###,###.## ") + &
		FORMAT$(FUI_PCT * 100.0, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR LOOP% = 1% TO SUI_LOOP%

		TEXT$ = "SUI Expense     " + &
			LEFT(SUI_CODE$(LOOP%) + "  ", 2%) + &
			FORMAT$(FUNC_ROUND(SUI_PCT(LOOP%) * &
				TOTAL_SUI_WAGES(LOOP%), 2%), "###,###.## ") + &
			FORMAT$(TOTAL_SUI_WAGES(LOOP%), "###,###.## ") + &
			FORMAT$(SUI_PCT(LOOP%) * 100.0, "###.##%")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

	NEXT LOOP%

	FOR LOOP% = 1% TO OST_LOOP%

		IF TOTAL_OST_WAGES(LOOP%) <> 0.0
		THEN
			TEXT$ = "OST Expense     " + &
				LEFT(OST_CODE$(LOOP%) + "  ", 2%) + &
				FORMAT$(FUNC_ROUND(OST_PCT(LOOP%) * &
					TOTAL_OST_WAGES(LOOP%), 2%), &
					"###,###.## ") + &
				FORMAT$(TOTAL_OST_WAGES(LOOP%), "###,###.## ") + &
				FORMAT$(OST_PCT(LOOP%) * 100.0, "###.##%")
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF
	NEXT LOOP%

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
