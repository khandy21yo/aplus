1	%TITLE "Payroll Tax Register Report"
	%SBTTL "PR_RPRT_QTR_REGISTER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1999 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:PR017
	!
	! Abstract:HELP
	!	.p
	!	The ^*Tax Register report\* provides a listing of each employee, their pay,
	!	their taxable pay, when necessary the jurisdiction, and the computed taxes on
	!	it. The report may be started, stopped, and sorted to the desires of the user.
	!	The fields included are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Name
	!	.le
	!	Pay
	!	.le
	!	Non Comp Pay
	!	.le
	!	FICA Taxable
	!	.le
	!	FICA Tax
	!	.le
	!	Federal Taxable
	!	.le
	!	Federal Tax
	!	.le
	!	State Taxable
	!	.le
	!	State Tax
	!	.le
	!	Other State Tax Taxable
	!	.le
	!	Other State Tax
	!	.le
	!	SUI Taxable
	!	.le
	!	SUI Tax
	!	.le
	!	City Taxable
	!	.le
	!	City Tax
	!	.le
	!	County Taxable
	!	.le
	!	County Tax
	!	.els
	!
	! Index:
	!	.x Tax Register >Report
	!	.x Report>Tax Register
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_QTR_REGISTER/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_QTR_REGISTER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_QTR_REGISTER.OBJ;*
	!
	! Author:
	!
	!	10/18/99 - Kevin Handy
	!
	! Modification history:
	!
	!	11/03/99 - Kevin Handy
	!		Added final code to handle full year
	!
	!	12/08/99 - Kevin Handy
	!		Lose unused function
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP	(PR_REG_TAXES)		PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP	(PR_REG_ERNDED)		PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP	(PR_TAX_TABLE)		PR_TAX_TABLE_CDD	PR_TAX_TABLE

	RECORD TOTAL_RECORD
		REAL	HOURS
		REAL	GROSS
		REAL	FEDERAL
		REAL	FICA
		REAL	STATE
		REAL	OST
		REAL	OTHER
	END RECORD

	!
	! Dimension
	!
	DECLARE TOTAL_RECORD &
		EMP_TOTAL, &
		LOCAL_TOTAL, &
		TOTAL_TOTAL

	DECLARE TOTAL_RECORD BLANK_TOTAL

	BLANK_TOTAL::HOURS = 0.0
	BLANK_TOTAL::GROSS = 0.0
	BLANK_TOTAL::FEDERAL = 0.0
	BLANK_TOTAL::FICA = 0.0
	BLANK_TOTAL::STATE = 0.0
	BLANK_TOTAL::OST = 0.0
	BLANK_TOTAL::OTHER = 0.0

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	QTR% = VAL%(UTL_REPORTX::OPTDEF(0%))
	!++
	!
	! Abstract:FLD01
	!	^*(01) Quarter\*
	!	.p
	!	The ^*Quarter\* field enters the calendar quarter for which
	!	this report will print.
	!	.p
	!	This field requires an entry. The allowed entries are *1,*2,*3,*4 for
	!	individual quarters, or *0 for the full year.
	!
	! Index:
	!	.x Quarter>Tax Register Report
	!	.x Tax Register Report>Quarter
	!
	!--

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	!
	! Abstract:FLD02
	!	^*(02) Year\*
	!	.p
	!	The ^*Year\* field enters the year for which this report
	!	is to print.
	!	.p
	!	The format for entry is YYYY and entry is required.
	!
	! Index:
	!	.x Year>Tax Register Report
	!	.x Tax Register Report>Year
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	!
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* setting causes the printing
	!	to begin with a particular item.
	!	.p
	!	A blank field will cause the report to start with the first item in the
	!	file.
	!
	! Index:
	!	.x From Item>Tax Register Report
	!	.x Tax Register Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	!
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.p
	!	The ^*To Item\* field causes the printing
	!	to end with a particular item.
	!	.p
	!	A blank field causes the report to end with the last item in the file.
	!
	! Index:
	!	.x To Item>Tax Register Report
	!	.x Tax Register Report>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	!
	! Abstract:FLD05
	!	^*(05) Sort (NU,NA,LO,SO)\*
	!	.p
	!	The ^*Sort\* field enters a code which causes
	!	the report to be sorted in the indicated manner.
	!	.p
	!	Valid codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	NU=Number
	!	.le
	!	NA=Name
	!	.le
	!	SO=Alphabetical (last name first)
	!	.le
	!	LO=Location
	!	.els
	!	.lm -10
	!	.p
	!	An entry is required in this field and only the above codes are valid.
	!
	! Index:
	!	.x Sort>Tax Register Report
	!	.x Tax Register Report>Sort
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
	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"

340	!
	! Open TaxWH register
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"

350	!
	! Open ERNDED register
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"

360	!
	! Open Tax Table file
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"

	GET #PR_TAX_TABLE.CH%, KEY #0% EQ "F", REGARDLESS

	FICA_EMPR_PCT = (PR_TAX_TABLE::FICA_EMPR_PCT + &
		PR_TAX_TABLE::FICA_EMPR_PCT_HI) / 10000.0
	FICA_EMPE_PCT = (PR_TAX_TABLE::FICA_EMPE_PCT + &
		PR_TAX_TABLE::FICA_EMPE_PCT_HI) / 10000.0

	!
	! Allow for an additional digit in FICA rate
	!
	IF FICA_EMPR_PCT > 0.100
	THEN
		FICA_EMPR_PCT = FICA_EMPR_PCT / 10.0
		FICA_EMPE_PCT = FICA_EMPE_PCT / 10.0
	END IF

	CLOSE PR_TAX_TABLE.CH%

370	!

	%PAGE

 ReportTitle:
	SELECT QTR%
	CASE 0%
		TEMP$ = "Year"
	CASE 1%
		TEMP$ = "1st Quarter"
	CASE 2%
		TEMP$ = "2nd Quarter"
	CASE 3%
		TEMP$ = "3rd Quarter"
	CASE ELSE
		TEMP$ = NUM1$(QTR%) + "th Quarter"
	END SELECT

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Employee Quarterly Payroll Taxes Report"
	TITLE$(2%) = "For the " + TEMP$ + " of " + YYYY$

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

	TITLE$(5%) = "Emp #      Name                   Hours      Gross" + &
		"  Federal    Fica    State       Ost    Other" + &
		"     Gross"

	TITLE$(6%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	IF FROM_ITEM$ = ""
	THEN
		RESET #PR_EMP_MASTER.CH%, KEY #K_NUM%
	ELSE
		FIND #PR_EMP_MASTER.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
	END IF

	LOCAL_FLAG% = 0%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	GET #PR_EMP_MASTER.CH%, REGARDLESS

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

	EMP_TOTAL = BLANK_TOTAL

17100	!*******************************************************************
	! Get tax detail information
	!*******************************************************************

	WHEN ERROR IN
		FIND #PR_REG_TAXES.CH%, KEY #0% EQ PR_EMP_MASTER::EMPNUM
	USE
		CONTINUE 17190
	END WHEN


17110	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE 17190
	END WHEN

	GOTO 17190 IF PR_REG_TAXES::EMPNUM <> PR_EMP_MASTER::EMPNUM

	IF QTR% = 0%
	THEN
		ITEM_TOTAL = 0.0
		ITEM_TOTAL = ITEM_TOTAL + &
			PR_REG_TAXES::TAX(I%) &
			FOR I% = 0% TO 3%
	ELSE
		ITEM_TOTAL = PR_REG_TAXES::TAX(QTR% - 1%)
	END IF

	SELECT PR_REG_TAXES::TTYPE

	CASE "FI", "FH"
		EMP_TOTAL::FICA = EMP_TOTAL::FICA + ITEM_TOTAL
	CASE "FW"
		EMP_TOTAL::FEDERAL = EMP_TOTAL::FEDERAL + ITEM_TOTAL
	CASE "SW"
		EMP_TOTAL::STATE = EMP_TOTAL::STATE + ITEM_TOTAL
	CASE "SX", "SI", "SU"
		EMP_TOTAL::OST = EMP_TOTAL::OST + ITEM_TOTAL
	CASE ELSE
		EMP_TOTAL::OTHER = EMP_TOTAL::OTHER + ITEM_TOTAL
	END SELECT

	GOTO 17110

17190	!

17200	!*******************************************************************
	! Get tax detail information
	!*******************************************************************

	FIND #PR_REG_ERNDED.CH%, KEY #0% GE PR_EMP_MASTER::EMPNUM, REGARDLESS

17210	GET #PR_REG_ERNDED.CH%, REGARDLESS

	GOTO 17400 IF (PR_EMP_MASTER::EMPNUM <> PR_REG_ERNDED::EMPNUM)

	SELECT PR_REG_ERNDED::ETYPE
	CASE "A"
		! IGNORE ACCRUALS

	CASE "P"
		!
		! Decide if we want to keep this one
		!
		IF QTR% = 0%
		THEN
			EMP_TOTAL::GROSS = FUNC_ROUND(EMP_TOTAL::GROSS + &
				PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
				FOR I% = 0% TO 3%
			EMP_TOTAL::HOURS = FUNC_ROUND(EMP_TOTAL::HOURS + &
				PR_REG_ERNDED::REG_HRS(I%) + &
				PR_REG_ERNDED::PRE_HRS(I%), 2%) &
				FOR I% = 0% TO 3%
		ELSE
			EMP_TOTAL::GROSS = FUNC_ROUND(EMP_TOTAL::GROSS + &
				PR_REG_ERNDED::QTR_DOLL(QTR% - 1%), 2%)
			EMP_TOTAL::HOURS = FUNC_ROUND(EMP_TOTAL::HOURS + &
				PR_REG_ERNDED::REG_HRS(QTR% - 1%) + &
				PR_REG_ERNDED::PRE_HRS(QTR% - 1%), 2%)
		END IF

	CASE ELSE
		!
		! Decide if we want to keep this one
		!
		IF QTR% = 0%
		THEN
			EMP_TOTAL::OTHER = FUNC_ROUND(EMP_TOTAL::OTHER + &
				PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
				FOR I% = 0% TO 3%
		ELSE
			EMP_TOTAL::OTHER = FUNC_ROUND(EMP_TOTAL::OTHER + &
				PR_REG_ERNDED::QTR_DOLL(QTR% - 1%), 2%)
		END IF
	END SELECT

	GOTO 17210


17400	!*******************************************************************
	! Print out this employee
	!*******************************************************************

	TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 20%) + " " + &
		FORMAT$(EMP_TOTAL::HOURS, "#####.## ") + &
		FORMAT$(EMP_TOTAL::GROSS, "######.## ") + &
		FORMAT$(EMP_TOTAL::FEDERAL, "#####.## ") + &
		FORMAT$(EMP_TOTAL::FICA, "#####.## ") + &
		FORMAT$(EMP_TOTAL::STATE, "#####.## ") + &
		FORMAT$(EMP_TOTAL::OST, "#####.## ") + &
		FORMAT$(EMP_TOTAL::OTHER, "#####.## ") + &
		FORMAT$(EMP_TOTAL::GROSS - EMP_TOTAL::FEDERAL - &
			EMP_TOTAL::FICA - EMP_TOTAL::STATE - &
			EMP_TOTAL::OST - EMP_TOTAL::OTHER, &
			"######.##")


	TOTAL_TOTAL::HOURS = FUNC_ROUND(TOTAL_TOTAL::HOURS + &
		EMP_TOTAL::HOURS, 2%)
	TOTAL_TOTAL::GROSS = FUNC_ROUND(TOTAL_TOTAL::GROSS + &
		EMP_TOTAL::GROSS, 2%)
	TOTAL_TOTAL::FEDERAL = FUNC_ROUND(TOTAL_TOTAL::FEDERAL + &
		EMP_TOTAL::FEDERAL, 2%)
	TOTAL_TOTAL::FICA = FUNC_ROUND(TOTAL_TOTAL::FICA + &
		EMP_TOTAL::FICA, 2%)
	TOTAL_TOTAL::STATE = FUNC_ROUND(TOTAL_TOTAL::STATE + &
		EMP_TOTAL::STATE, 2%)
	TOTAL_TOTAL::OST = FUNC_ROUND(TOTAL_TOTAL::OST + &
		EMP_TOTAL::OST, 2%)
	TOTAL_TOTAL::OTHER = FUNC_ROUND(TOTAL_TOTAL::OTHER + &
		EMP_TOTAL::OTHER, 2%)

	LOCAL_TOTAL::HOURS = FUNC_ROUND(LOCAL_TOTAL::HOURS + &
		EMP_TOTAL::HOURS, 2%)
	LOCAL_TOTAL::GROSS = FUNC_ROUND(LOCAL_TOTAL::GROSS + &
		EMP_TOTAL::GROSS, 2%)
	LOCAL_TOTAL::FEDERAL = FUNC_ROUND(LOCAL_TOTAL::FEDERAL + &
		EMP_TOTAL::FEDERAL, 2%)
	LOCAL_TOTAL::FICA = FUNC_ROUND(LOCAL_TOTAL::FICA + &
		EMP_TOTAL::FICA, 2%)
	LOCAL_TOTAL::STATE = FUNC_ROUND(LOCAL_TOTAL::STATE + &
		EMP_TOTAL::STATE, 2%)
	LOCAL_TOTAL::OST = FUNC_ROUND(LOCAL_TOTAL::OST + &
		EMP_TOTAL::OST, 2%)
	LOCAL_TOTAL::OTHER = FUNC_ROUND(LOCAL_TOTAL::OTHER + &
		EMP_TOTAL::OTHER, 2%)

	EMP_TOTAL = BLANK_TOTAL

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
	TEXT$ = "          " + " " + &
		SPACE$(20%) + " " + &
		FORMAT$(TOTAL_TOTAL::HOURS, "#####.## ") + &
		FORMAT$(TOTAL_TOTAL::GROSS, "######.## ") + &
		FORMAT$(TOTAL_TOTAL::FEDERAL, "#####.## ") + &
		FORMAT$(TOTAL_TOTAL::FICA, "#####.## ") + &
		FORMAT$(TOTAL_TOTAL::STATE, "#####.## ") + &
		FORMAT$(TOTAL_TOTAL::OST, "#####.## ") + &
		FORMAT$(TOTAL_TOTAL::OTHER, "#####.## ") + &
		FORMAT$(TOTAL_TOTAL::GROSS - TOTAL_TOTAL::FEDERAL - &
			TOTAL_TOTAL::FICA - TOTAL_TOTAL::STATE - &
			TOTAL_TOTAL::OST - TOTAL_TOTAL::OTHER, &
			"######.##")
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

 LocalTotal:
	GOTO 18590 IF LOCAL_FLAG% = 0%

	TEXT$ = "          " + " " + &
		SPACE$(20%) + " " + &
		FORMAT$(LOCAL_TOTAL::HOURS, "#####.## ") + &
		FORMAT$(LOCAL_TOTAL::GROSS, "######.## ") + &
		FORMAT$(LOCAL_TOTAL::FEDERAL, "#####.## ") + &
		FORMAT$(LOCAL_TOTAL::FICA, "#####.## ") + &
		FORMAT$(LOCAL_TOTAL::STATE, "#####.## ") + &
		FORMAT$(LOCAL_TOTAL::OST, "#####.## ") + &
		FORMAT$(LOCAL_TOTAL::OTHER, "#####.## ") + &
		FORMAT$(LOCAL_TOTAL::GROSS - LOCAL_TOTAL::FEDERAL - &
			LOCAL_TOTAL::FICA - LOCAL_TOTAL::STATE - &
			LOCAL_TOTAL::OST - LOCAL_TOTAL::OTHER, &
			"######.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TITLE$(3%) = "For Location " + PR_EMP_MASTER::LOCATION
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)

18590	!
	! Set up for next one
	!
	LOCAL_FLAG% = 0%

	TITLE$(3%) = "For Location " + PR_EMP_MASTER::LOCATION
	THIS_LOCATION$ = PR_EMP_MASTER::LOCATION

	LOCAL_TOTAL = BLANK_TOTAL

	RETURN

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	IF ERR=154%
	THEN
		SLEEP 1%
		RESUME
	END IF

	FILENAME$ = ""
	SELECT ERL

	!
	! Payroll master file
	!
	CASE 300%
		FILENAME$ = "PR_EMP_MASTER"

	!
	! Payroll master file
	!
	CASE 17000%
		RESUME ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"

	!
	! Payroll master file
	!
	CASE 17020%
		RESUME ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"

	!
	! Tax WH
	!
	CASE 340%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$

	!
	! ERNDED WH
	!
	CASE 350%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$

	!
	! Tax Table File
	!
	CASE 360%
		RESUME 370 IF ERR = 5% OR ERR = 155%
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$

	!
	! Tax register
	!
	CASE 17100%
		RESUME 17200

	!
	! Tax register
	!
	CASE 17200%, 17210%
		RESUME 17400

	END SELECT

	!
	! Untrapped error
	!
	RESUME HelpError

32767	END
