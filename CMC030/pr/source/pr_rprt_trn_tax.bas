1	%TITLE "Payroll Tax Register Report"
	%SBTTL "PR_RPRT_TRN_TAX"
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
	! ID:PR007
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Employee Payroll Taxes\* report option produces a report which
	!	lists the following information for a specified payroll folder:
	!	.b
	!	^*Note:\* The Employee Payroll Taxes report should be run after the calculate.
	!	.table 3,25
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name
	!	.te
	!	Total Pay (Earnings)
	!	.te
	!	Total Non-Compensation Pay
	!	.te
	!	FICA Taxable Earnings
	!	.te
	!	FICA Tax Withheld
	!	.te
	!	Federal Taxable Earnings
	!	.te
	!	Federal Taxes Withheld
	!	.te
	!	State(s) Taxable Earnings
	!	.te
	!	Applicable State(s)
	!	.te
	!	Applicable State(s) Taxes Withheld
	!	.te
	!	Other State(s) Taxable Earnings
	!	.te
	!	Applicable State(s)
	!	.te
	!	Other State(s) Taxes
	!	.te
	!	City(ies) Taxable Earnings
	!	.te
	!	Applicable City(ies)
	!	.te
	!	Applicable City(ies) Taxes Withheld
	!	.te
	!	County(ies) Taxable Earnings
	!	.te
	!	Applicable County(ies)
	!	.te
	!	Applicable County(ies) Taxes Withheld
	!	.te
	!	School District Taxable Earnings
	!	.te
	!	Applicable School District
	!	.te
	!	Applicable School District Taxes Withheld
	!	.te
	!	Total Pay (Earnings)
	!	.te
	!	Total Non-Compensation Pay
	!	.te
	!	Total Taxable Earnings in each category
	!	.te
	!	Employee Count
	!	.te
	!	Total Federal Deposit Information
	!	.break
	!	i.e. Employee and Employer FICA and
	!	Federal Taxes Withheld
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Payroll Taxes>Employee>Report
	!	.x Report>Payroll Taxes>Employee
	!	.x Employee>Payroll Taxes>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_TAX/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_TAX, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_TAX.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	03/07/89 - Frank F. Starman
	!		Replace school tax with SUI
	!
	!	02/17/89 - Kevin Handy
	!		Modified to check taxable/nontaxable flag correctly.
	!
	!	03/21/89 - Kevin Handy
	!		Fixed problem handling deductions taxable/nontaxable.
	!
	!	03/27/89 - Kevin Handy
	!		Fixed bug where it was using CHW instead of CWH for
	!		the city tax table.
	!
	!	05/18/89 - Kevin Handy
	!		Modifications to clean up printing section,
	!		set up to always print status codes, even if
	!		amount is zero. Fix problem where "sort by"
	!		field used 'LO' in one place and 'DP' in another.
	!
	!	05/18/89 - Kevin Handy
	!		Modified so that printing by location
	!		will page between locations, and the current
	!		location is displayed on the top of the page.
	!
	!	08/07/89 - Kevin Handy
	!		Modified so that if there was no tax record,
	!		and thus the status is not known, it will
	!		search the PR_EMP_STATUS file for the
	!		status to display.
	!
	!	01/04/89 - Kevin Handy
	!		Modified to use structures instead of dozens
	!		of arrays.
	!
	!	05/24/90 - Kevin Handy
	!		Started coding for spreadsheet info so that I
	!		could try to solve a totaling problem.
	!
	!	05/25/90 - Kevin Handy
	!		Fix distribution of state taxable amount
	!		so that it isn't so goofy.
	!
	!	05/25/90 - Kevin Handy
	!		Fix federal total so that it totals the federal
	!		taxable instead of the FICA taxable.
	!
	!	05/25/90 - Kevin Handy
	!		Modified to use TAXABLE and REPORTABLE fields
	!		as stored in the deduction file instead of
	!		attempting to re-calculate these things.
	!
	!	07/13/90 - Kevin Handy
	!		Fixed problem with loop structure, where it
	!		would still print out items even though they
	!		belong to a previous employee.
	!
	!	07/17/90 - Kevin Handy
	!		Fixed problem that caused report to mark everyones
	!		FICA as being calculated incorrectly.
	!
	!	07/17/90 - Kevin Handy
	!		Continue of 07/13/90 problem.  Hopefully it is
	!		completely fixed now.
	!
	!	12/14/90 - Kevin Handy
	!		Added ::ADDEXEMPT.
	!
	!	01/15/91 - Craig Tanner
	!		Added YYYY$ to filename$ in error trapping.
	!
	!	06/04/91 - Kevin Handy
	!		Removed junk code in error trapping.
	!
	!	07/25/91 - Kevin Handy
	!		Modified to calculate employers FICA tax from the
	!		taxable employee fica amount, instead of as a
	!		ratio of employee FICA to employer FICA rates,
	!		which is off because of rounding errors.
	!
	!		NOTE: This program does not consider the HI limits
	!		and thus is not accurate when that limit is reached,
	!		and assumes employer FICA and employee FICA amounts
	!		are the same.  This should be fixed..
	!
	!	08/01/91 - Kevin Handy
	!		Modifications to make employer FICA calculation
	!		more accurate by looking at the differing FICA
	!		and HI limits.
	!
	!	08/20/91 - Kevin Handy
	!		Modified to trap error at 17300 when FICA record
	!		does not exist for an employee.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	01/08/92 - Kevin Handy
	!		Modified to handle errors at 17300 for any error,
	!		and not just 155.
	!
	!	09/28/92 - Kevin Handy
	!		Fixed bug where had <OUTP_LINE(1%> to read
	!		<OUTP_LINE("">
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/14/93 - Kevin Handy
	!		Added a REGARDLESS to the get of the quarterly
	!		tax file.
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Removed unsolicited_input stuff.
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	04/10/97 - Kevin Handy
	!		Loose lots of hard coded values, and make
	!		modifications to handle FH fica code.
	!
	!	05/12/97 - Kevin Handy
	!		Use one more digit for fica rates.
	!
	!	05/17/97 - Kevin Handy
	!		Fix a couple of problems with Federal tax.
	!		Use integer for #key
	!
	!	07/24/97 - Kevin Handy
	!		Allow old FICA rates to work until I get
	!		everyone converted over.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP	(PR_TAX_TABLE)		PR_TAX_TABLE_CDD	PR_TAX_TABLE

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP	(PR_REG_TAXES)		PR_REG_TAXES_CDD	PR_REG_TAXES

	!
	! Structures local to program
	!
	RECORD TOTAL_RECORD
		STRING CODE = 2%
		GFLOAT TAXABLE
		GFLOAT TAX
		STRING STAT = 1%
		LONG EXEMPT
		LONG ADDEXEMPT
		LONG EMPCOUNT
	END RECORD

	DIM TOTAL_RECORD EMPLOYEE(10%, 10%)
	DIM TOTAL_RECORD LOCAL(10%, 30%)
	DIM TOTAL_RECORD TOTAL(10%, 30%)

	!
	! Dimension
	!
	DIM EMPLOYEE_CODES%(10%), &
		TOTAL_WH_CODE%(10%), &
		LOCAL_WH_CODE%(10%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Other Variables
	!
	SUBJECT_TYPE_TABLE$ = "FHE!FIE!FWH!SWH!OST!SUI!CWH!DWH!EWH!SWC"
	TAX_TYPE_TABLE$ = "FH!FI!FW!SW!SX!SU!CW!DW!EW!SI!"

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Payroll Date\* field enters a particular
	!	payroll date which is to be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Payroll Date>Tax Register
	!	.x Tax Register>Payroll Date
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)
	YYYY$ = LEFT(BATCH_NO$, 4%)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters a particular
	!	item with which the report will begin printing.
	!	The value must be in agreement with field
	!	(04).
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Tax Register
	!	.x Tax Register>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters a particular
	!	item with which the report will end printing.
	!	This field must correspond with field (04).
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Tax Register
	!	.x Tax Register>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	.ts 55
	!	^*(04) Sort	NU,NA,SO,LO\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field causes the report
	!	to print in a particular order.
	!	.b
	!	Valid sort codes are:
	!	.table 3,25
	!	.te
	!	^*NU\*	Number
	!	.te
	!	^*NA\*	Name
	!	.te
	!	^*SO\*	Alphabetical (last name first)
	!	.te
	!	^*LO\*	Location
	!	.end table
	!	This field requires an entry.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Tax Register
	!	.x Tax Register>Sort
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
	! Open history Pay folder if journal not found
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
			PR_TRN_DED.CH% = PR_HIS_DED.CH%
		END IF
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

360	!
	! Open Tax Table file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"

		GET #PR_TAX_TABLE.CH%, KEY #0% EQ "F", REGARDLESS
	USE
		CONTINUE 370 IF ERR = 5%
		CONTINUE 370 IF ERR = 155%
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	FICA_EMPR_PCT = (PR_TAX_TABLE::FICA_EMPR_PCT) / 10000.0
	FICA_EMPE_PCT = (PR_TAX_TABLE::FICA_EMPE_PCT) / 10000.0
	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT
	FICA_EMPR_PCT_HI= (PR_TAX_TABLE::FICA_EMPR_PCT_HI) / 10000.0
	FICA_EMPE_PCT_HI= (PR_TAX_TABLE::FICA_EMPE_PCT_HI) / 10000.0
	FICA_LIMIT_HI = PR_TAX_TABLE::FICA_LIMIT_HI

	IF FICA_EMPR_PCT > 0.10
	THEN
		FICA_EMPR_PCT = FICA_EMPR_PCT / 10.0
		FICA_EMPE_PCT = FICA_EMPE_PCT / 10.0
		FICA_EMPR_PCT_HI = FICA_EMPR_PCT_HI / 10.0
		FICA_EMPE_PCT_HI = FICA_EMPE_PCT_HI / 10.0
	END IF

	CLOSE PR_TAX_TABLE.CH%

370	!
	! Open TaxWH register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		CONTINUE 380
	END WHEN

380	!

	%PAGE

 ReportTitle:
	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Employee Payroll Taxes Report"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		MID(BATCH_NO$, 5%, 2%) + "/" + &
		MID(BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(BATCH_NO$, 4%)

	SELECT SORTBY$
	CASE "NU"
		TITLE$(3%) = "By Employee Number"

	CASE "NA"
		TITLE$(3%) = "By Employee Name"

	CASE "SN"
		TITLE$(3%) = "By Soc. Sec. Number"

	CASE "LO"
		TITLE$(3%) = "By Location"

	CASE ELSE
		TITLE$(3%) = "By Alpha Sort"

	END SELECT

	TITLE$(4%) = ""

	TITLE$(5%) = "                                      Pay/        FICA" + &
		"     Federal        State          OST         SUI         " + &
		"City       County"

	TITLE$(6%) = "Emp #      Name                    NonComp Taxable/Tax" + &
		" Taxable/Tax  Taxable/Tax  Taxable/Tax  Taxable/Tax  Taxable/Tax" + &
		"  Taxable/Tax"

	TITLE$(7%) = ""

	LYT_LINE$ = "$EMPNUM:11,$EMPNAME:32,VPAY:42,VFICABLE:54," + &
		"VFEDBLE:64,VSTBLE:77,$STATE:79"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	YTD_FICA_EARN = 0.0
	YTD_HI_EARN = 0.0

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

	LOCAL_FLAG% = 0%

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
	CASE "LO"
		GOTO ExitTotal IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOSUB LocalTotal IF (PR_EMP_MASTER::LOCATION <> THIS_LOCATION$)

	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	PAY, NON_COMP = 0.0

	EMPLOYEE_CODES%(I%) = 0% FOR I% = 1% TO 10%

	!
	! Set up for federal taxes
	!
	FOR I% = 1% TO 3%
		EMPLOYEE_CODES%(I%) = 1%
		EMPLOYEE(I%, 1%)::TAXABLE = 0.0
		EMPLOYEE(I%, 1%)::TAX = 0.0
		EMPLOYEE(I%, 1%)::CODE = ""
		EMPLOYEE(I%, 1%)::STAT = ""
		EMPLOYEE(I%, 1%)::EXEMPT = 0%
		EMPLOYEE(I%, 1%)::ADDEXEMPT = 0%
	NEXT I%

17100	!
	! Get pay detail information
	!
	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17450 IF ERR = 155% OR ERR = 9%
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
	! If history then set history into journal map
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	GOTO 17200 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM)

	GOTO 17110 IF PR_TRN_PAY::PTYPE = "A"

	PAY = FUNC_ROUND(PAY + PR_TRN_PAY::GROSS, 2%)

	GOTO 17110

17200	!
	! Get Tax/Ded detail information
	!
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
	! If history then set history into journal map
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_DED = PR_HIS_DED
	END IF

	GOTO 17300 IF PR_EMP_MASTER::EMPNUM <> PR_TRN_DED::EMPNUM

	!
	! See if this is a tax
	!
	TAX_TYPE% = (INSTR(1%, TAX_TYPE_TABLE$, PR_TRN_DED::CODE) + 2%) / 3%

	!
	! Jump to deduction if not tax
	!
	GOTO 17290 IF TAX_TYPE% = 0%

	WH_LOOP% = 1%

	IF TAX_TYPE% > 3%
	THEN
		!
		! State, local tax
		!
		GOTO 17240 IF PR_TRN_DED::TAX_CODE = &
			EMPLOYEE(TAX_TYPE%,WH_LOOP%)::CODE &
			FOR WH_LOOP% = 1% TO EMPLOYEE_CODES%(TAX_TYPE%)

		EMPLOYEE_CODES%(TAX_TYPE%), WH_LOOP% = &
			EMPLOYEE_CODES%(TAX_TYPE%) + 1%

		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::CODE = PR_TRN_DED::TAX_CODE
		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAXABLE = 0.0
		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAX = 0.0
		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::STAT = ""
		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::EXEMPT = 0%
		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::ADDEXEMPT = 0%
	END IF

17240	EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAX = &
		FUNC_ROUND(EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAX + &
		PR_TRN_DED::AMOUNT, 2%)
	EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAXABLE = &
		FUNC_ROUND(EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAXABLE + &
		PR_TRN_DED::TAXABLE, 2%)

	EMPLOYEE(TAX_TYPE%,WH_LOOP%)::STAT = PR_TRN_DED::SSTATUS
	EMPLOYEE(TAX_TYPE%,WH_LOOP%)::EXEMPT = PR_TRN_DED::EXEMPT
	EMPLOYEE(TAX_TYPE%,WH_LOOP%)::ADDEXEMPT = PR_TRN_DED::ADDEXEMPT

	GOTO 17290

17290	!
	! Loop back for next deduction record
	!
	GOTO 17210

17300	!****************************************************************

	!
	! Look up YTD FICA, and calculate if employee is FICA or HI
	! over/under.
	!
	EMP_FICA_EARN = 0.0
	EMP_HI_EARN = 0.0

	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM + "FI  ", &
			REGARDLESS
	USE
		CONTINUE 17305
	END WHEN

	EMP_FICA_EARN = EMP_FICA_EARN + PR_REG_TAXES::REPORTABLE(I%) &
		FOR I% = 0% TO 3%

17305	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM + "FH  ", &
			REGARDLESS
	USE
		CONTINUE 17310
	END WHEN

	EMP_HI_EARN = EMP_HI_EARN + PR_REG_TAXES::REPORTABLE(I%) &
		FOR I% = 0% TO 3%

17310	!
	! Calculate FICA reportable for this period
	!
	EMP_FICA_EARN = EMP_FICA_EARN + EMPLOYEE(1%, 1%)::TAXABLE &
		IF USE_HISTORY% = 0%
	PAY_FICA_EARN = EMPLOYEE(1%, 1%)::TAXABLE
	IF EMP_FICA_EARN > FICA_LIMIT
	THEN
		EXCESS = EMP_FICA_EARN - FICA_LIMIT
		PAY_FICA_EARN = PAY_FICA_EARN - EXCESS
		PAY_FICA_EARN = 0.0 IF PAY_FICA_EARN < 0.0
	END IF
	YTD_FICA_EARN = YTD_FICA_EARN + PAY_FICA_EARN

	!
	! Calculate HI reportable for this period
	!
	EMP_HI_EARN = EMP_HI_EARN + EMPLOYEE(1%, 1%)::TAXABLE &
		IF USE_HISTORY% = 0%
	PAY_HI_EARN = EMPLOYEE(1%, 1%)::TAXABLE
	IF EMP_HI_EARN > FICA_LIMIT_HI
	THEN
		EXCESS = EMP_HI_EARN - FICA_LIMIT_HI
		PAY_HI_EARN = PAY_HI_EARN - EXCESS
		PAY_HI_EARN = 0.0 IF PAY_HI_EARN < 0.0
	END IF
	YTD_HI_EARN = YTD_HI_EARN + PAY_HI_EARN

17400	!
	! Allocate taxable wages
	!
	WORK_LOOP% = 1%

	WORK_LOOP% = EMPLOYEE_CODES%(LOOP%) &
		IF WORK_LOOP% < EMPLOYEE_CODES%(LOOP%) &
		FOR LOOP% = 3% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

	FOR LOOP% = 1% TO WORK_LOOP%
		TAXABLE_TEXT$ = ""
		TAX_TEXT$ = ""

		IF LOOP% = 1%
		THEN
			!
			! Tax Basis
			!
			TAXABLE_TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
				LEFT(PR_EMP_MASTER::EMPNAME, 22%) + &
				FORMAT$(PAY, "######.##   ")

			TAXABLE_TEXT$ = LEFT(TAXABLE_TEXT$ + SPACE$(45%), 45%)

			FICA_TEST = FUNC_ROUND(PAY_FICA_EARN * FICA_EMPE_PCT + &
				PAY_HI_EARN * FICA_EMPE_PCT_HI, 2%)

			IF ABS(FICA_TEST - (EMPLOYEE(1%, LOOP%)::TAX + &
				EMPLOYEE(2%, LOOP%)::TAX)) <= 0.02
			THEN
				FICA_TEST$ = " "
			ELSE
				FICA_TEST$ = "*"
			END IF

			TAXABLE_TEXT$ = TAXABLE_TEXT$ + &
				FORMAT$(EMPLOYEE(1%, LOOP%)::TAXABLE, "<%>#####.##") + &
				FICA_TEST$

			!
			! Taxes
			!
			TAX_TEXT$ = SPACE$(33%) + &
				FORMAT$(NON_COMP, "######.##   ")

			TAX_TEXT$ = LEFT(TAX_TEXT$ + SPACE$(45%), 45%)

			TAX_TEXT$ = TAX_TEXT$ + &
				FORMAT$(EMPLOYEE(1%, LOOP%)::TAX + &
				EMPLOYEE(2%, LOOP%)::TAX, &
				"<%>#####.## ")
		END IF

		TAXABLE_TEXT$ = LEFT(TAXABLE_TEXT$ + SPACE$(55%), 55%)
		TAX_TEXT$ = LEFT(TAX_TEXT$ + SPACE$(55%), 55%)

		FOR TAX_TYPE% = 3% TO LEN(SUBJECT_TYPE_TABLE$) / 4%
			!
			! Taxes
			!
			IF EMPLOYEE_CODES%(TAX_TYPE%) >= LOOP%
			THEN
				TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$( &
					EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE, &
					"######.##") + &
					LEFT(EMPLOYEE(TAX_TYPE%, LOOP%)::CODE + &
					SPACE$(3%), 3%)

				TAX_TEXT$ = TAX_TEXT$ + &
					FORMAT$(EMPLOYEE(TAX_TYPE%, LOOP%)::TAX, &
					"<%>####.##") + &
					LEFT(EMPLOYEE(TAX_TYPE%, LOOP%)::STAT + " ", 1%) + &
					FORMAT$(EMPLOYEE(TAX_TYPE%, LOOP%)::EXEMPT, "##/") + &
					FORMAT$(EMPLOYEE(TAX_TYPE%, LOOP%)::EXEMPT, "##")
			ELSE
				TAX_TEXT$ = TAX_TEXT$ + "             "
				TAXABLE_TEXT$ = TAXABLE_TEXT$ + "             "
			END IF

		NEXT TAX_TYPE%

		!
		! Print Taxes
		!
		IF EDIT$(TAXABLE_TEXT$, -1%) <> "" OR EDIT$(TAX_TEXT$, -1%) <> ""
		THEN
			LOCAL_FLAG% = -1%

			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), &
				LEFT(TAXABLE_TEXT$, 132%), 1%)

			GOTO ExitProgram IF UTL_REPORTX::STAT

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				LEFT(TAX_TEXT$, 132%), 0%)

			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

17420	NEXT LOOP%

	!
	! Total codes
	!
	TOTAL_PAY = TOTAL_PAY + PAY
	TOTAL_NON_COMP = TOTAL_NON_COMP + NON_COMP

	TOTAL(1%, 1%)::TAXABLE = TOTAL(1%, 1%)::TAXABLE + &
		EMPLOYEE(1%, 1%)::TAXABLE
	TOTAL(1%, 1%)::TAX = TOTAL(1%, 1%)::TAX + EMPLOYEE(1%, 1%)::TAX
		! FICA OASDI

	TOTAL(2%, 1%)::TAXABLE = TOTAL(2%, 1%)::TAXABLE + &
		EMPLOYEE(2%, 1%)::TAXABLE
	TOTAL(2%, 1%)::TAX = TOTAL(2%, 1%)::TAX + EMPLOYEE(2%, 1%)::TAX
		! FICA OASDI

	TOTAL(3%, 1%)::TAXABLE = TOTAL(3%, 1%)::TAXABLE + &
		EMPLOYEE(3%, 1%)::TAXABLE
	TOTAL(3%, 1%)::TAX = TOTAL(3%, 1%)::TAX + EMPLOYEE(3%, 1%)::TAX
		! Federal

	FOR TAX_TYPE% = 4% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

		FOR LOOP% = 1% TO EMPLOYEE_CODES%(TAX_TYPE%)

			GOTO 17430 IF EMPLOYEE(TAX_TYPE%, LOOP%)::CODE = &
				TOTAL(TAX_TYPE%, I%)::CODE &
					FOR I% = 1% TO TOTAL_WH_CODE%(TAX_TYPE%)

			TOTAL_WH_CODE%(TAX_TYPE%), I% = TOTAL_WH_CODE%(TAX_TYPE%) + 1%
			TOTAL(TAX_TYPE%, I%)::CODE = &
				EMPLOYEE(TAX_TYPE%,LOOP%)::CODE

17430			TOTAL(TAX_TYPE%,I%)::TAXABLE = TOTAL(TAX_TYPE%,I%)::TAXABLE + &
				EMPLOYEE(TAX_TYPE%,LOOP%)::TAXABLE

			TOTAL(TAX_TYPE%,I%)::TAX = TOTAL(TAX_TYPE%,I%)::TAX + &
				EMPLOYEE(TAX_TYPE%,LOOP%)::TAX

			TOTAL(TAX_TYPE%, I%)::EMPCOUNT = TOTAL(TAX_TYPE%, I%)::EMPCOUNT + 1% &
				IF EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE <> 0.0

		NEXT LOOP%

	NEXT TAX_TYPE%

	!
	! Total codes
	!
	LOCAL_PAY = LOCAL_PAY + PAY
	LOCAL_NON_COMP = LOCAL_NON_COMP + NON_COMP

	FOR TAX_TYPE% = 1% TO 3%
		LOCAL(TAX_TYPE%, 1%)::TAXABLE = &
			LOCAL(TAX_TYPE%, 1%)::TAXABLE + &
			EMPLOYEE(TAX_TYPE%, 1%)::TAXABLE
		LOCAL(TAX_TYPE%, 1%)::TAX = LOCAL(TAX_TYPE%, 1%)::TAX + &
			EMPLOYEE(TAX_TYPE%, 1%)::TAX
	NEXT TAX_TYPE%

	FOR TAX_TYPE% = 4% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

		FOR LOOP% = 1% TO EMPLOYEE_CODES%(TAX_TYPE%)

			GOTO 17440 IF EMPLOYEE(TAX_TYPE%, LOOP%)::CODE = &
				LOCAL(TAX_TYPE%, I%)::CODE &
					FOR I% = 1% TO LOCAL_WH_CODE%(TAX_TYPE%)

			LOCAL_WH_CODE%(TAX_TYPE%), I% = LOCAL_WH_CODE%(TAX_TYPE%) + 1%
			LOCAL(TAX_TYPE%, I%)::CODE = EMPLOYEE(TAX_TYPE%,LOOP%)::CODE

			LOCAL(TAX_TYPE%, I%)::TAXABLE = 0.0
			LOCAL(TAX_TYPE%, I%)::TAX = 0.0
			LOCAL(TAX_TYPE%, I%)::EMPCOUNT = 0%

17440			LOCAL(TAX_TYPE%,I%)::TAXABLE = &
				LOCAL(TAX_TYPE%,I%)::TAXABLE + &
				EMPLOYEE(TAX_TYPE%,LOOP%)::TAXABLE

			LOCAL(TAX_TYPE%,I%)::TAX = LOCAL(TAX_TYPE%,I%)::TAX + &
				EMPLOYEE(TAX_TYPE%,LOOP%)::TAX

			LOCAL(TAX_TYPE%, I%)::EMPCOUNT = &
				LOCAL(TAX_TYPE%, I%)::EMPCOUNT + 1% &
				IF EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE <> 0.0

		NEXT LOOP%

	NEXT TAX_TYPE%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

17450	!
	! Next employee
	!
	GOTO 17020

	%Page

 ExitTotal:
	GOSUB LocalTotal IF (SORTBY$ = "LO")

	!
	! Handle end of report
	!
	TOTAL(2%, 1%)::CODE = ""

	WORK_LOOP% = 1%

	WORK_LOOP% = TOTAL_WH_CODE%(LOOP%) &
		IF WORK_LOOP% < TOTAL_WH_CODE%(LOOP%) &
		FOR LOOP% = 4% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

	FOR LOOP% = 1% TO WORK_LOOP%
		TAXABLE_TEXT$ = "Total Taxable                    "
		TAX_TEXT$ = LEFT("      Tax" + SPACE$(33%), 33%)

		IF LOOP% = 1%
		THEN
			TAXABLE_TEXT$ = TAXABLE_TEXT$ + &
				FORMAT$(TOTAL_PAY, "######.##   ")

			TAX_TEXT$ = TAX_TEXT$ + &
				FORMAT$(TOTAL_NON_COMP, "######.##   ")
		END IF

		TAXABLE_TEXT$ = LEFT(TAXABLE_TEXT$ + SPACE$(45%), 45%)
		TAX_TEXT$ = LEFT(TAX_TEXT$ + SPACE$(45%), 45%)

		!
		! Print Taxable wages
		!
		TAXABLE_TEXT$ = TAXABLE_TEXT$ + &
			FORMAT$(TOTAL(1%, LOOP%)::TAXABLE, "<%>#####.## ")

		TAX_TEXT$ = TAX_TEXT$ + &
			FORMAT$(TOTAL(1%, LOOP%)::TAX + &
			TOTAL(2%, LOOP%)::TAX, "<%>#####.## ")

		FOR TAX_TYPE% = 3% TO LEN(SUBJECT_TYPE_TABLE$) / 4%
			IF TOTAL(TAX_TYPE%,LOOP%)::TAXABLE <> 0.0 OR &
				TOTAL(TAX_TYPE%, LOOP%)::TAX <> 0.0
			THEN
				TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$( &
					TOTAL(TAX_TYPE%, LOOP%)::TAXABLE, &
					"######.##") + &
					LEFT(TOTAL(TAX_TYPE%, LOOP%)::CODE + &
					SPACE$(4%), 4%)

				TAX_TEXT$ = TAX_TEXT$ + &
					FORMAT$(TOTAL(TAX_TYPE%, LOOP%)::TAX, &
					"<%>#####.##")
				TAX_TEXT$ = TAX_TEXT$ + "    "
			ELSE
				TAXABLE_TEXT$ = TAXABLE_TEXT$ + "             "
				TAX_TEXT$ = TAX_TEXT$ + "             "
			END IF
		NEXT TAX_TYPE%

		!
		! Print Taxes
		!
		COUNT_TEXT$ = LEFT("      Employee Count" + SPACE$(55%), 55%)

		FOR TAX_TYPE% = 3% TO LEN(SUBJECT_TYPE_TABLE$) / 4%
			COUNT_TEXT$ = COUNT_TEXT$ + &
				FORMAT$(TOTAL(TAX_TYPE%, LOOP%)::EMPCOUNT, &
				"<%>########    ")
		NEXT TAX_TYPE%

		IF EDIT$(TAXABLE_TEXT$, -1%) <> "" OR &
			EDIT$(TAX_TEXT$, -1%) <> "" OR &
			EDIT$(COUNT_TEXT$, -1%) <> ""
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				LEFT(TAXABLE_TEXT$, 132%), 3%)


			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				LEFT(TAX_TEXT$, 132%), 0%)


			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				LEFT(COUNT_TEXT$, 132%), 0%)

		END IF

	NEXT LOOP%

	!
	! Print federal deposit
	!
	FICA_EMPE = TOTAL(1%, 1%)::TAX + TOTAL(2%, 1%)::TAX
	FICA_EMPR = FUNC_ROUND(YTD_FICA_EARN * FICA_EMPR_PCT, 2%) + &
		FUNC_ROUND(YTD_HI_EARN * FICA_EMPR_PCT_HI, 2%)
	FED_TAX = TOTAL(3%, 1%)::TAX

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 5%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Fica Employee       " + &
		FORMAT$(FICA_EMPE, "###,###.##"), 0%)

	IF FICA_EMPE_PCT <> 0.0
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Fica Employer       " + &
			FORMAT$(FICA_EMPR, "###,###.##"), 0%)
	ELSE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Fica Employer            N/A  ", 0%)
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Federal Withholding " + &
		FORMAT$(FED_TAX, "###,###.##"), 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Total Tax Deposit   " + &
		FORMAT$(FICA_EMPE + FICA_EMPR + FED_TAX, &
		"###,###.##"), 0%)

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

	!*******************************************************************
	! Print Local Total
	!*******************************************************************
 LocalTotal:
18500	GOTO 18590 IF LOCAL_FLAG% = 0%

	WORK_LOOP% = 1%

	WORK_LOOP% = LOCAL_WH_CODE%(LOOP%) &
		IF WORK_LOOP% < LOCAL_WH_CODE%(LOOP%) &
		FOR LOOP% = 4% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

	FOR LOOP% = 1% TO WORK_LOOP%
		TAXABLE_TEXT$ = "Total Local Taxable              "
		TAX_TEXT$     = "      Local Tax                  "

		IF LOOP% = 1%
		THEN
			TAXABLE_TEXT$ = TAXABLE_TEXT$ + &
				FORMAT$(LOCAL_PAY, "######.##   ")

			TAX_TEXT$ = TAX_TEXT$ + &
				FORMAT$(LOCAL_NON_COMP, "######.##   ")
		END IF

		TAXABLE_TEXT$ = LEFT(TAXABLE_TEXT$ + SPACE$(45%), 45%)
		TAX_TEXT$ = LEFT(TAX_TEXT$ + SPACE$(45%), 45%)

		!
		! Print Taxable wages
		!
		TAXABLE_TEXT$ = TAXABLE_TEXT$ + &
			FORMAT$(LOCAL(1%, LOOP%)::TAXABLE, &
			"<%>#####.## ")

		TAX_TEXT$ = TAX_TEXT$ + &
			FORMAT$(LOCAL(1%, LOOP%)::TAX + LOCAL(2%, LOOP%)::TAX, &
			"<%>#####.## ")


		FOR TAX_TYPE% = 3% TO LEN(SUBJECT_TYPE_TABLE$) / 4%
			IF LOCAL(TAX_TYPE%,LOOP%)::TAXABLE <> 0.0 OR &
				LOCAL(TAX_TYPE%, LOOP%)::TAX <> 0.0
			THEN
				TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$( &
					LOCAL(TAX_TYPE%, LOOP%)::TAXABLE, &
					"######.##") + &
					LEFT(LOCAL(TAX_TYPE%, LOOP%)::CODE + &
					SPACE$(4%), 4%)

				TAX_TEXT$ = TAX_TEXT$ + &
					FORMAT$(LOCAL(TAX_TYPE%, LOOP%)::TAX, &
					"<%>#####.##")
				TAX_TEXT$ = TAX_TEXT$ + "    "
			ELSE
				TAXABLE_TEXT$ = TAXABLE_TEXT$ + "             "
				TAX_TEXT$ = TAX_TEXT$ + "             "
			END IF
		NEXT TAX_TYPE%

		!
		! Print Taxes
		!
		COUNT_TEXT$ = LEFT("      Employee Count" + SPACE$(55%), 55%)

		FOR TAX_TYPE% = 3% TO LEN(SUBJECT_TYPE_TABLE$) / 4%
			COUNT_TEXT$ = COUNT_TEXT$ + &
				FORMAT$(LOCAL(TAX_TYPE%, LOOP%)::EMPCOUNT, &
				"<%>########    ")
		NEXT TAX_TYPE%

		IF EDIT$(TAXABLE_TEXT$, -1%) <> "" OR &
			EDIT$(TAX_TEXT$, -1%) <> "" OR &
			EDIT$(COUNT_TEXT$, -1%) <> ""
		THEN
			CALL OUTP_LINE(OUTP_LINE$, UTL_REPORTX, TITLE$(), &
				LEFT(TAXABLE_TEXT$, 132%), 0%)

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				LEFT(TAX_TEXT$, 132%), 0%)

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				LEFT(COUNT_TEXT$, 132%), 0%)

		END IF

	NEXT LOOP%

	TITLE$(3%) = "For Location " + PR_EMP_MASTER::LOCATION
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)

18590	!
	! Set up for next one
	!
	LOCAL_FLAG% = 0%

	TITLE$(3%) = "For Location " + PR_EMP_MASTER::LOCATION
	THIS_LOCATION$ = PR_EMP_MASTER::LOCATION

	FOR I% = 1% TO 10%
		FOR J% = 0% TO 10%
			LOCAL(I%, J%)::TAXABLE = 0.0
			LOCAL(I%, J%)::TAX = 0.0
			LOCAL(I%, J%)::CODE = ""
			LOCAL(I%, J%)::EMPCOUNT = 0%
		NEXT J%
	NEXT I%

	LOCAL_WH_CODE%(I%) = 0% FOR I% = 1% TO 10%

	RETURN

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
