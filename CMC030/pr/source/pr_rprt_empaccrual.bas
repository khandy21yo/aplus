1	%TITLE "Employee Accruals"
	%SBTTL "PR_RPRT_EMPACCRUAL"
	%IDENT "V3.6a Calico"

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
	! ID:PR011
	!
	! Abstract:HELP
	!	.p
	!	The ^*Print Employee Accruals\* option
	!	prints the current and total
	!	hours accrued for each benefit as related to each employee. The
	!	print routine also multiplies the accrued hours by the appropriate
	!	current rates to arrive at the dollar value of the accruals.
	!	.p
	!	Included in this report are the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Name
	!	.le
	!	Code
	!	.le
	!	Description
	!	.le
	!	Rate
	!	.le
	!	Hours Limit
	!	.le
	!	Hours Accrued this Period
	!	.le
	!	Dollars Accrued this Period
	!	.le
	!	Hours Balance
	!	.le
	!	Dollars Balance
	!	.els
	!
	! Index:
	!	.x Accrual>Print
	!	.x Print>Accrual
	!	.x Accrual>Report
	!	.x Report>Accrual
	!
	! Option:
	!
	! Author:
	!
	!	02/11/88 - Robert Peterson
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_EMPACCRUAL
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_EMPACCRUAL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_EMPACCRUAL.OBJ;*
	!
	! Modification history:
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	03/18/91 - Kevin Handy
	!		Added eval_date field to pr_read_rate.
	!
	!	05/22/91 - Kevin Handy
	!		Unwound error trapping.
	!		Fixed odd bits in error trapping. (Undefined lines,
	!		"ER" instead of "ERL", etc.)
	!
	!	05/22/91 - Kevin Handy
	!		Modified to summarise into a structure instead of
	!		six seperate arrays.
	!
	!	05/22/91 - Kevin Handy
	!		Modified grand total to print "TOTAL_xxx" instead
	!		of "xxx(loop%)".
	!
	!	05/22/91 - Kevin Handy
	!		Modified so that someone with a termination date
	!		of "00/00/0000" would still show up.
	!
	!	08/07/92 - Kevin Handy
	!		Added location wildcard for Northwest Center.
	!
	!	04/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/22/2000 - Kevin Handy
	!		Added EFF_DATE parameter tp PR_READ_RATE
	!
	!	12/06/2000 - Kevin Handy
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP (PR_EMP_STD_ERNDED) PR_EMP_STD_ERNDED_CDD PR_EMP_STD_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Array to hold totals
	!
	RECORD ACCT_RECORD
		STRING	ACCT = 18
		DOUBLE	DEBIT
		DOUBLE	CREDIT
		DOUBLE	HOURS
	END RECORD

	DECLARE INTEGER CONSTANT MAX_ACCT = 1000

	DIM ACCT_RECORD ACCT_TOTAL(MAX_ACCT)

	RECORD TYPE_RECORD
		STRING	ACC_TYPE = 2%
		STRING	ACC_DESC = 30%
		REAL	CUR_HRS
		REAL	CUR_DOL
		REAL	BAL_HRS
		REAL	BAL_DOL
	END RECORD

	DIM TYPE_RECORD ACC_TYPE(100%)

	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)

	!++
	! Abstract:FLD01
	!	^*(01) Accrual Date\*
	!	.p
	!	The ^*Accrual Date\* field
	!	enters the effective date of
	!	employee benefits accrual execution.
	!
	! Index:
	!	.x Accrual Date>Accrual Report
	!	.x Accrual Report>Accrual Date
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.p
	!	The ^*From Item\* field enters a
	!	item from which the report will start printing.
	!	The value must be in agreement with
	!	field (04).
	!	.p
	!	A blank field will cause the report to begin with the
	!	first item in the file.
	!
	! Index:
	!	.x From Item>Accrual Report
	!	.x Accrual Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*From Item\* field enters a
	!	item from which the report will end printing.
	!	The value must be in agreement with
	!	field (04).
	!	.p
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!
	! Index:
	!	.x To Item>Accrual Report
	!	.x Accrual Report>To Item
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Sort By (LO,NU,SO)\*
	!	.p
	!	The ^*Sort By (LO,NU,SO)\* field
	!	designates the order in which the
	!	report will be printed.
	!	.b
	!	.lm +5
	!	.list 0,"*"
	!	.le
	!	LO = Location Number order
	!	.le
	!	NU = Employee Number order
	!	.le
	!	SO = Alphabetical order
	!	.els
	!	.lm -5
	!	.p
	!	An entry is required in this field and only the above codes are valid.
	!
	! Index:
	!	.x Sort>Accrual Report
	!	.x Accrual Report>Sort
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

	DEFAULT_RATE = VAL(EDIT$(UTL_REPORTX::OPTDEF(4%), -1%))

	!++
	! Abstract:FLD05
	!	^*(05) Default Pay Rate\*
	!	.p
	!	The ^*Default Pay Rate\* field
	!	enters a default pay rate which
	!	will be used in the event an employee has no individual default pay
	!	rate in the Employee Master File Rate record.
	!
	! Index:
	!	.x Default Pay Rate>Accrual Report
	!	.x Pay Rate>Accrual Report
	!	.x Accrual Report>Default Pay Rate
	!
	!--


	CODE_NOT_POSTED$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Accrual Not Posted\*
	!	.p
	!	The ^*Accrual Not Posted\* field
	!	designates any specific
	!	benefits which are to be posted to the payroll files only, and not
	!	to the General Ledger. For example, if payroll benefits to be
	!	accrued for employees include vacation, sick pay and personal
	!	leave, but vacation benefits only were to be posted to the General
	!	Ledger, this field would include the codes for sick
	!	pay and personal leave, separated by a comma.
	!
	! Index:
	!	.x Accrual Not Posted>Accrual Report
	!	.x Accrual Report>Accrual Not Posted
	!
	!--

	LOCATION_WILDCARD$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 128%)

	!++
	! Abstract:FLD07
	!	^*(07) Location Wildcard\*
	!	.p
	!	The ^*Location Wildcard\* field
	!	designates any specific
	!	location(s) to include using the wildcard method.
	!
	! Index:
	!	.x Location Wildcard>Accrual Report
	!	.x Accrual Report>Location Wildcard
	!
	!--


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.OPN"
	USE
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Pay Deduction definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Chart file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Employee Accrual Report"
	TITLE$(2%) = "Accrual Date:  " + PRNT_DATE(BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = SPACE$(88%) + &
		"-Accrued This Period-   -------Balance------"
	TITLE$(5%) = "Emp #      Name                         " + &
		"Code Description               Rate   Hrs Limit      " + &
		"Hours    Dollars       Hours    Dollars"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$EmpNum:010,$EmpName:040,$Code:043,$Descr:062," + &
		"VHourlyRate:075,VLimit:086,VCurrentHrs:098," + &
		"VCurrentDol:109,VBalanceHrs:121,VBalanceDol:132"

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

	!
	! If the employee has been terminated before the accrual date
	! then accruals will not be calculated for this employee....
	!
	GOTO 17350 IF PR_EMP_MASTER::TERMDAY <= BATCH_NO$ AND &
		PR_EMP_MASTER::TERMDAY > "00000000"

	!
	! If the employee doesn't belong in the specified location
	!
	IF WILDCARD_LOCATION$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(TRM$(PR_EMP_MASTER::LOCATION), &
			WILDCARD_LOCATION$) = 0%
	END IF

17100	!
	! Look up standard earnings and deductions
	!
	WHEN ERROR IN
		FIND #PR_EMP_STD_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

17120	WHEN ERROR IN
		GET #PR_EMP_STD_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 11%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

	GOTO 17350 IF PR_EMP_STD_ERNDED::EMPNUM <> PR_EMP_MASTER::EMPNUM

	GOTO 17120 IF PR_EMP_STD_ERNDED::RTYPE <> "A"

17130	!
	! Look up ernded definition file
	!
	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%, &
			KEY #0% EQ "P" + PR_EMP_STD_ERNDED::CODE, &
			REGARDLESS
	USE
		PR_ERNDED_DEF::DESCR = STRING$(LEN(PR_ERNDED_DEF::DESCR), 63%)
		PR_ERNDED_DEF::DRCR_ACCT = &
			STRING$(LEN(PR_ERNDED_DEF::DRCR_ACCT), 63%)
		PR_ERNDED_DEF::ACCRUAL_ACCT = &
			STRING$(LEN(PR_ERNDED_DEF::ACCRUAL_ACCT), 63%)

		CONTINUE 17140 IF ERR = 155%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

17140	!
	! Print Accrual
	!
	CUR_HRS, CUR_DOL, BAL_HRS, BAL_DOL = 0.0

	CUR_HRS = PR_EMP_STD_ERNDED::RATE

	IF PR_EMP_STD_ERNDED::LIMIT <> 0.0
	THEN
		IF (CUR_HRS + PR_EMP_STD_ERNDED::ACCRUED - &
			PR_EMP_STD_ERNDED::CTDBAL) > PR_EMP_STD_ERNDED::LIMIT
		THEN
			CUR_HRS = PR_EMP_STD_ERNDED::LIMIT  - &
				(PR_EMP_STD_ERNDED::ACCRUED - &
				PR_EMP_STD_ERNDED::CTDBAL)
		END IF
	END IF

	BAL_HRS = (CUR_HRS + PR_EMP_STD_ERNDED::ACCRUED) - &
		PR_EMP_STD_ERNDED::CTDBAL

	!
	! Employee rate
	!
	CALL PR_READ_RATE(PR_EMP_MASTER::EMPNUM, &
		PR_EMP_MASTER::OPER, &
		BATCH_NO$, &
		PR_EMP_MASTER::RATE_TYPE, &
		PR_EMP_MASTER::RATE_CDE, &
		HOUR_RATE, &
		PIECE_RATE, &
		FACTOR, &
		STDEFF, &
		EVALDATE$, &
		EFF_DATE$)

	IF HOUR_RATE = 0.0
	THEN
		HOUR_RATE = DEFAULT_RATE
	END IF

	CUR_DOL = FUNC_ROUND(CUR_HRS * HOUR_RATE, 2%)
	BAL_DOL = FUNC_ROUND(BAL_HRS * HOUR_RATE, 2%)

	TEXT$ = PR_EMP_STD_ERNDED::EMPNUM + " " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 29%) + " " + &
		PR_EMP_STD_ERNDED::CODE + "  " + &
		LEFT(PR_ERNDED_DEF::DESCR, 17%) + "  " + &
		FORMAT$(HOUR_RATE, "#######.### ") + &
		FORMAT$(PR_EMP_STD_ERNDED::LIMIT, "#######.## ") + &
		FORMAT$(CUR_HRS, "#######.### ") + &
		FORMAT$(CUR_DOL, "#######.## ") + &
		FORMAT$(BAL_HRS, "#######.### ") + &
		FORMAT$(BAL_DOL, "#######.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Create posting transmittal for accruals to be posted to the
	! general ledger
	!
	IF COMP_STRING(EDIT$(PR_EMP_STD_ERNDED::CODE, -1%), &
		CODE_NOT_POSTED$) = 0% &
		OR EDIT$(CODE_NOT_POSTED$, -1%) = ""
	THEN
		!
		! Expense account
		!
		IF INSTR(1%, PR_ERNDED_DEF::ACCRUAL_ACCT, "?")
		THEN
			CALL GL_ASSG_ACCMASK(PR_ERNDED_DEF::ACCRUAL_ACCT, &
				PR_EMP_MASTER::ACCT, &
				ACCT_NUM$)
		ELSE
			ACCT_NUM$ = PR_ERNDED_DEF::ACCRUAL_ACCT
		END IF

		ACCT_TOTAL = CUR_DOL
		ACCT_HOURS = CUR_HRS
		GOSUB AddAcct

		ACCT_TOTAL = -CUR_DOL
		ACCT_HOURS = -CUR_HRS
		ACCT_NUM$ = PR_ERNDED_DEF::DRCR_ACCT
		GOSUB AddAcct
	END IF

	!
	! Accumulate accrual summary totals
	!
	GOTO AccSummary IF ACC_TYPE(LOOP%)::ACC_TYPE = PR_EMP_STD_ERNDED::CODE &
		FOR LOOP% = 1% TO ACC_TYPE_LOOP%

	LOOP%, ACC_TYPE_LOOP% = ACC_TYPE_LOOP% + 1%
	ACC_TYPE(LOOP%)::ACC_TYPE = PR_EMP_STD_ERNDED::CODE
	ACC_TYPE(LOOP%)::ACC_DESC = PR_ERNDED_DEF::DESCR

 AccSummary:
	ACC_TYPE(LOOP%)::CUR_HRS = ACC_TYPE(LOOP%)::CUR_HRS + CUR_HRS
	ACC_TYPE(LOOP%)::CUR_DOL = ACC_TYPE(LOOP%)::CUR_DOL + CUR_DOL

	ACC_TYPE(LOOP%)::BAL_HRS = ACC_TYPE(LOOP%)::BAL_HRS + BAL_HRS
	ACC_TYPE(LOOP%)::BAL_DOL = ACC_TYPE(LOOP%)::BAL_DOL + BAL_DOL


	GOTO 17120

17200	!
	! GOSUB Employee total
	!

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEMP$ = "Total"

	TOTAL_CUR_HRS, TOTAL_CUR_DOL, TOTAL_BAL_HRS, TOTAL_BAL_DOL = 0.0

	FOR LOOP% = 1% TO ACC_TYPE_LOOP%
		TEXT$ = LEFT(TEMP$ + SPACE$(43%), 43%) + &
			ACC_TYPE(LOOP%)::ACC_TYPE + "  " + &
			LEFT(ACC_TYPE(LOOP%)::ACC_DESC, 19%) + "  " + &
			SPACE$(19%) + &
			FORMAT$(ACC_TYPE(LOOP%)::CUR_HRS, "#######.### ") + &
			FORMAT$(ACC_TYPE(LOOP%)::CUR_DOL, "#######.## ") + &
			FORMAT$(ACC_TYPE(LOOP%)::BAL_HRS, "#######.### ") + &
			FORMAT$(ACC_TYPE(LOOP%)::BAL_DOL, "#######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEMP$ = ""

		TOTAL_CUR_HRS = TOTAL_CUR_HRS + ACC_TYPE(LOOP%)::CUR_HRS
		TOTAL_CUR_DOL = TOTAL_CUR_DOL + ACC_TYPE(LOOP%)::CUR_DOL
		TOTAL_BAL_HRS = TOTAL_BAL_HRS + ACC_TYPE(LOOP%)::BAL_HRS
		TOTAL_BAL_DOL = TOTAL_BAL_DOL + ACC_TYPE(LOOP%)::BAL_DOL

	NEXT LOOP%

	TEXT$ = LEFT("  Grand Total" + SPACE$(89%), 87%) + &
		FORMAT$(TOTAL_CUR_HRS, "#######.### ") + &
		FORMAT$(TOTAL_CUR_DOL, "#######.## ") + &
		FORMAT$(TOTAL_BAL_HRS, "#######.### ") + &
		FORMAT$(TOTAL_BAL_DOL, "#######.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Heading
	!
	TITLE$(4%) = "Account #           Description                  " + &
		"                   Debit                    Credi" + &
		"t           Hours"
	TITLE$(5%) = ""

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)

	FOR I% = 1% TO TOTAL_ACCT%

		GOTO 17450 IF ACCT_TOTAL(I%)::DEBIT = 0.0 AND &
			ACCT_TOTAL(I%)::CREDIT = 0.0 AND &
			ACCT_TOTAL(I%)::HOURS = 0.0

17420		GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), 63%)
		WHEN ERROR IN
			GET #GL_CHART.CH%, &
				KEY #0% EQ ACCT_TOTAL(I%)::ACCT, REGARDLESS
		USE
			CONTINUE 17430 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "GL_CHART"
			CONTINUE HelpError
		END WHEN

17430		TEXT$ = LEFT(ACCT_TOTAL(I%)::ACCT + SPACE$(18%), 18%) + "  " + &
			LEFT(GL_CHART::DESCR, 30%) + "     " + &
			FORMAT$(ACCT_TOTAL(I%)::DEBIT, &
				"<%>##,###,###,###.##        ") + &
			FORMAT$(-ACCT_TOTAL(I%)::CREDIT, &
				"<%>##,###,###,###.## ") + &
			FORMAT$(ACCT_TOTAL(I%)::HOURS, &
				"###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TOTAL_DEBIT = TOTAL_DEBIT + ACCT_TOTAL(I%)::DEBIT
		TOTAL_CREDIT = TOTAL_CREDIT + ACCT_TOTAL(I%)::CREDIT
		TOTAL_HOURS = TOTAL_HOURS + ACCT_TOTAL(I%)::HOURS

17450	NEXT I%

	TEXT$ = "                    " + &
		"Grand Totals                       " + &
		FORMAT$(TOTAL_DEBIT, &
			"<%>##,###,###,###.##        ") + &
		FORMAT$(-TOTAL_CREDIT, &
			"<%>##,###,###,###.## ") + &
		FORMAT$(TOTAL_HOURS, &
			"###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)



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

 AddAcct:
	!
	! Search Account balance list for currently existing account
	!
	GOTO AddAcct1 &
		IF (ACCT_TOTAL(I%)::ACCT = ACCT_NUM$) &
			FOR I% = 1% TO TOTAL_ACCT%

	!
	! Item not found, create it
	!
	I%, TOTAL_ACCT% = TOTAL_ACCT% + 1%

	WHILE (I% > 1%) AND (ACCT_TOTAL(I% - 1%)::ACCT > ACCT_NUM$)
		ACCT_TOTAL(I%) = ACCT_TOTAL(I% - 1%)
		I% = I% - 1%
	NEXT

	ACCT_TOTAL(I%)::ACCT = ACCT_NUM$
	ACCT_TOTAL(I%)::CREDIT = 0.0
	ACCT_TOTAL(I%)::DEBIT = 0.0
	ACCT_TOTAL(I%)::HOURS = 0.0

 AddAcct1:
	!
	! Add credit/debit amounts
	!
	IF ACCT_TOTAL > 0.0
	THEN
		ACCT_TOTAL(I%)::DEBIT = ACCT_TOTAL(I%)::DEBIT + ACCT_TOTAL
	ELSE
		ACCT_TOTAL(I%)::CREDIT = ACCT_TOTAL(I%)::CREDIT + ACCT_TOTAL
	END IF

	ACCT_TOTAL(I%)::HOURS = ACCT_TOTAL(I%)::HOURS + ACCT_HOURS

	RETURN

	%Page

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
