1	%TITLE "Labor Performance 1"
	%SBTTL "PR_RPRT_LABPER01"
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
	! ID:PR010
	!
	! Abstract:HELP
	!	.p
	!	The ^*Labor Performance Report\* prints a report which
	!	displays the work totals and
	!	evaluates the performance of the desired employees.  The following fields are
	!	included in this report:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Date
	!	.le
	!	Subaccount
	!	.le
	!	Operation
	!	.le
	!	Actual Hours
	!	.le
	!	Actual Units
	!	.le
	!	Actual Units per Hour
	!	.le
	!	Rate
	!	.le
	!	Standard Units per Hour
	!	.le
	!	Standard Rate
	!	.le
	!	Productivity
	!	.le
	!	Gross
	!	.els
	!
	! Index:
	!	.x Report>Labor Performance
	!	.x Labor Performance>Report
	!
	! Option:
	!
	! Author:
	!
	!	12/11/87 - Frank F. Starman
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_LABPER01
	!	$ LINK/EXE=PR_EXE:*.EXE PR_RPRT_LABPER01, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_LABPER01.OBJ;*
	!
	! Modification history:
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	04/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/11/94 - Kevin Handy
	!		Looking for problem for NWC. Reformatted some lines.
	!		made division by zero test more obvious.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/12/2000 - Kevin Handy
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	!
	! Dimension statements
	!
	DIM EMP_TOTAL(11%), AMT(11%), &
		SUB_TOTAL(11%), &
		OPER_TOTAL(11%), &
		LOC_TOTAL(11%), &
		DEPT_TOTAL(11%), &
		TOTAL(11%)

	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:FLD01
	!	^*(01) Payroll Folder Date\*
	!	.p
	!	The ^*Payroll Folder Date\* field causes
	!	the report to be printed for a particular
	!	payroll folder.
	!	.p
	!	This field requires an entry. The format for entry is MMDDYYYY
	!	or MMDDYY.
	!
	! Index:
	!	.x Payroll Folder Date
	!	.x Folder Date>Payroll
	!
	!--
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)

	!++
	! Abstract:FLD02
	!	^*(02) From Subaccount/Location\*
	!	.p
	!	The ^*From Subaccount/Location\* field causes the
	!	printing to begin with a certain Subaccount/Location.
	!	.p
	!	A blank field will cause the report to start with the first Subaccount/Location
	!	in the field.
	!
	! Index:
	!
	!--
	FROM_ITEM1$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Subaccount/Location\*
	!	.p
	!	The ^*To Subaccount/Location\* field causes the
	!	printing to end with a certain Subaccount/Location.
	!	.p
	!	A blank field  causes the report to end with the last Subaccount/Location
	!	in the field.
	!
	! Index:
	!
	!--
	TO_ITEM1$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	FROM_ITEM2$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) From Operation/Department\*
	!	.p
	!	The ^*From Operation/Department\* field causes the printing
	!	to begin with a particular Operation/Department.
	!	.p
	!	A blank field causes the report to start with the first Operation/Department in
	!	the file.
	!
	! Index:
	!
	!--

	TO_ITEM2$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)
	!++
	! Abstract:FLD05
	!	^*(05) To Operation/Department\*
	!	.p
	!	The ^*To Operation/Department\* field causes the printing
	!	to end with a particular Operation/Department.
	!	.p
	!	A blank field causes the report to end with the last Operation/Department in
	!	the file.
	!
	! Index:
	!
	!--

	NONPROD_CODE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)
	!++
	! Abstract:FLD06
	!	^*(06) Non-Productive Subaccount\*
	!	.p
	!	The ^*Non-Productive Subaccount\* field enters the
	!	account which will receive no immediate payment from a client, but the
	!	company must cover the cost themselves.
	!
	! Index:
	!	.x Non=Productive Subaccount
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Sort By (SO,LD)\*
	!	.p
	!	The ^*Sort\* field enters a value which
	!	determines the order in which the report will print.
	!	.p
	!	Valid settings are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	SO = Sort Order
	!	.le
	!	LD = Location/Department
	!	.els
	!	.p
	!	.lm -10
	!	An entry is required in this field. Only the above codes are
	!	valid.
	!
	! Index:
	!
	!--


	PAGEBREAK$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	SELECT SORTBY$
	CASE "SO"
		K_NUM% = 1%

		L1% = LEN(PR_TRN_PAY::SUBACC)
		L2% = LEN(PR_TRN_PAY::OPER)

		FROM_ITEM$ = LEFT(FROM_ITEM1$ + SPACE$(L1%), L1%) + &
			LEFT(FROM_ITEM2$ + SPACE$(L2%), L2%)

		TO_ITEM$ = LEFT(TO_ITEM1$ + SPACE$(L1%), L1%) + &
			LEFT(TO_ITEM2$ + SPACE$(L2%), L2%)

	CASE ELSE
		K_NUM% = 2%

		L1% = LEN(PR_TRN_PAY::LOCATION)
		L2% = LEN(PR_TRN_PAY::DEPT)

		FROM_ITEM$ = LEFT(FROM_ITEM1$ + SPACE$(L1%), L1%) + &
			LEFT(FROM_ITEM2$ + SPACE$(L2%), L2%)

		TO_ITEM$ = LEFT(TO_ITEM1$ + SPACE$(L1%), L1%) + &
			LEFT(TO_ITEM2$ + SPACE$(L2%), L2%)

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

	GOTO ReportTitle

315	!
	! Open Pay history if pay journal no found
	!
	USE_HISTORY% = -1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	PR_TRN_PAY.CH% = PR_HIS_PAY.CH%

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "Labor Performance Report Sorted by Location and Dept"
	TITLE$(1%) = "Labor Performance Report Sorted by SubAcct" &
		IF (SORTBY$ = "SO")
	TITLE$(2%) = "For the Payroll Folder Dated:  " + &
		PRNT_DATE(BATCH_NO$, 8%)
	TITLE$(3%) = " "

	!
	! Column headings
	!
	TITLE$(4%) = SPACE$(60%) + &
		"---------------Actual--------------- -------Std-------"
	TITLE$(5%) = "EmpNum     EmpName             Date     SubAcc     " + &
		"Oper       Hours    Units  Unit/hr      Rate  Unit/hr      " + &
		"Rate      Prod   Gross"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$EmpNum:010,$EmpName:028,DPRDate:039,$SubAcct:050," + &
		"$Oper:059,VActHours:067,VActUnits:076,VActUnitPerHr:085," + &
		"VRate:095,$RType:096,VSTDUnitsPerHr:104,VSTDRate:114," + &
		"VProd:123,VGross:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_TRN_PAY.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_TRN_PAY.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	LINE_PRINTED% = 0%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! If history then set history map to journal
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	GOTO 17020 IF PR_TRN_PAY::PTYPE = "A"

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "SO"
		GOTO ExitTotal IF PR_TRN_PAY::SUBACC + &
			PR_TRN_PAY::OPER > TO_ITEM$ AND &
			TO_ITEM$ <> ""
	CASE ELSE
		GOTO ExitTotal IF PR_TRN_PAY::LOCATION + &
			PR_TRN_PAY::DEPT > TO_ITEM$ AND &
			TO_ITEM$ <> ""
	END SELECT

17030	SELECT SORTBY$
	CASE "SO"
		IF TEST_SUBACC$ <> PR_TRN_PAY::SUBACC AND LINE_PRINTED%
		THEN
			!
			! Print total for an employee
			!
			GOSUB EmpTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			!
			! Print total for an operation
			!
			GOSUB OperTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			!
			! Print total for a subacc
			!
			GOSUB SubTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			LINE_PRINTED% = 0%
		END IF

		IF TEST_OPER$ <> PR_TRN_PAY::OPER AND LINE_PRINTED%
		THEN
			!
			! Print total for an employee
			!
			GOSUB EmpTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			!
			! Print total for an operation
			!
			GOSUB OperTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			LINE_PRINTED% = 0%
		END IF

	CASE ELSE
		IF TEST_LOCATION$ <> PR_TRN_PAY::LOCATION AND LINE_PRINTED%
		THEN
			!
			! Print total for an employee
			!
			GOSUB EmpTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			!
			! Print total for an Department
			!
			GOSUB DeptTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			!
			! Print total for a Location
			!
			GOSUB LocTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			LINE_PRINTED% = 0%
		END IF

		IF TEST_DEPT$ <> PR_TRN_PAY::DEPT AND LINE_PRINTED%
		THEN
			!
			! Print total for an employee
			!
			GOSUB EmpTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			!
			! Print total for an Dept
			!
			GOSUB DeptTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			LINE_PRINTED% = 0%
		END IF

	END SELECT

	GOTO 17100 IF TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM

	PR_EMP_MASTER::EMPNAME = STRING$(63%, LEN(PR_EMP_MASTER::EMPNAME))

	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, &
			KEY #0% EQ PR_TRN_PAY::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17050 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

17050	IF LINE_PRINTED%
	THEN
		!
		! Print total for an employee
		!
		GOSUB EmpTotal

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	EMP_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 11%

	TEXT_NAME$ = &
		PR_EMP_MASTER::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME

	GOTO ExitProgram IF UTL_REPORTX::STAT

17100	!
	! Set employee test value
	!
	TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM
	TEST_SUBACC$ = PR_TRN_PAY::SUBACC
	TEST_OPER$ = PR_TRN_PAY::OPER
	TEST_LOCATION$ = PR_TRN_PAY::LOCATION
	TEST_DEPT$ = PR_TRN_PAY::DEPT

	CALL PR_READ_SUBJOPER(PR_TRN_PAY::OPER, &
		PR_TRN_PAY::PR_END_DATE, &
		PIECE_RATE, &
		HOUR_RATE)

	AMT(1%) = PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR
	AMT(2%) = PR_TRN_PAY::PIECE
	IF AMT(1%) <> 0.0
	THEN
		AMT(3%) = FUNC_ROUND(AMT(2%) / AMT(1%), 2%)
	ELSE
		AMT(3%) = 0.0
	END IF
	AMT(4%) = PR_TRN_PAY::HOUR_RATE
	IF PIECE_RATE <> 0.0
	THEN
		AMT(5%) = FUNC_ROUND(HOUR_RATE / PIECE_RATE, 2%)
	ELSE
		AMT(5%) = 0.0
	END IF
	AMT(6%) = HOUR_RATE
	IF AMT(5%) <> 0.0
	THEN
		AMT(7%) = AMT(3%) / AMT(5%) * 100.
	ELSE
		AMT(7%) = 0.0
	END IF
	AMT(8%) = PR_TRN_PAY::GROSS

	AMT(9%), AMT(10%), AMT(11%) = 0.0

	IF COMP_STRING(PR_TRN_PAY::SUBACC, NONPROD_CODE$) AND &
		TRM$(NONPROD_CODE$) <> ""
	THEN
		AMT(9%) = AMT(1%)
	ELSE
		AMT(10%) = AMT(1%)
		AMT(11%) = AMT(8%)
	END IF

	TEXT$ = LEFT(TEXT_NAME$ + SPACE$(28%), 28%) + " " + &
		PRNT_DATE(PR_TRN_PAY::PR_END_DATE, 8%) + " " + &
		PR_TRN_PAY::SUBACC + " " + &
		PR_TRN_PAY::OPER + &
		FORMAT$(AMT(1%), "#####.## ") + &
		FORMAT$(AMT(2%), "#####.## ") + &
		FORMAT$(AMT(3%), "#####.## ") + &
		FORMAT$(AMT(4%), "####.####") + &
		PR_TRN_PAY::RTYPE + &
		FORMAT$(AMT(5%), " ####.## ") + &
		FORMAT$(AMT(6%), "####.#### ") + &
		FORMAT$(AMT(7%), "####.###%") + &
		FORMAT$(AMT(8%), "#####.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT_NAME$ = ""

	!
	! Take the weighted average for productivity
	!
	AMT(3%) = AMT(3%) * AMT(1%)
	AMT(5%) = AMT(5%) * AMT(1%)

	FOR LOOP% = 1% TO 11%
		EMP_TOTAL(LOOP%) = EMP_TOTAL(LOOP%) + AMT(LOOP%)
		SUB_TOTAL(LOOP%) = SUB_TOTAL(LOOP%) + AMT(LOOP%)
		OPER_TOTAL(LOOP%) = OPER_TOTAL(LOOP%) + AMT(LOOP%)
		LOC_TOTAL(LOOP%) = LOC_TOTAL(LOOP%) + AMT(LOOP%)
		DEPT_TOTAL(LOOP%) = DEPT_TOTAL(LOOP%) + AMT(LOOP%)
		TOTAL(LOOP%) = TOTAL(LOOP%) + AMT(LOOP%)
	NEXT LOOP%

	LINE_PRINTED% = -1%

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	IF LINE_PRINTED%
	THEN
		SELECT SORTBY$
		CASE "SO"
			!
			! Print total for an employee
			!
			GOSUB EmpTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			!
			! Print total for an operation
			!
			GOSUB OperTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			!
			! Print total for a subacc
			!
			GOSUB SubTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

		CASE ELSE
			!
			! Print total for an employee
			!
			GOSUB EmpTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			!
			! Print total for an Department
			!
			GOSUB DeptTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

			!
			! Print total for a Location
			!
			GOSUB LocTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT

		END SELECT
	END IF

	!*****************************************************************
	! Print total
	!*****************************************************************

	TEXT$ = SPACE$(39%) + &
		LEFT("Grand Total" + SPACE$(20%), 20%) + &
		FORMAT$(TOTAL(1%), "#####.## ") + &
		FORMAT$(TOTAL(2%), "#####.## ") + &
		SPACE$(45%) + &
		FORMAT$(TOTAL(8%), "#######.##")

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

	%Page

 EmpTotal:
	!*****************************************************************
	! Print Employee total
	!*****************************************************************
	TEXT$ = SPACE$(33%) + &
		LEFT("Emp Total" + SPACE$(26%), 26%) + &
		FORMAT$(EMP_TOTAL(1%), "#####.## ") + &
		FORMAT$(EMP_TOTAL(2%), "#####.## ") + &
		SPACE$(45%) + &
		FORMAT$(EMP_TOTAL(8%), "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 5%)

	GOTO EmpTotal1 IF UTL_REPORTX::STAT

	!
	! Print Average rate
	!
	TEMP = 0.0
	TEMP = EMP_TOTAL(11%) / EMP_TOTAL(10%) IF EMP_TOTAL(10%) <> 0.0
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"Average Rate (Prod Hrs)" + &
		FORMAT$(TEMP, "#####.####"), 0%)
	GOTO EmpTotal1 IF UTL_REPORTX::STAT

	!
	! Print average productivity level
	!
	TEMP = 0.0
	TEMP = EMP_TOTAL(3%) / EMP_TOTAL(5%) * 100.0 IF EMP_TOTAL(5%) <> 0.0
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"Wght Average Prod-Level" + &
		FORMAT$(TEMP, "#####.###%"), 0%)

	GOTO EmpTotal1 IF UTL_REPORTX::STAT

	!
	! Print productive hours
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"Productive Hours       " + &
		FORMAT$(EMP_TOTAL(10%), "#####.##"), 0%)

	GOTO EmpTotal1 IF UTL_REPORTX::STAT

	!
	! Print Non Productive Hours
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"Non - Productive Hours " + &
		FORMAT$(EMP_TOTAL(9%), "#####.##"), 0%)

	GOTO EmpTotal1 IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

 EmpTotal1:

	RETURN

	%Page

 SubTotal:
	!*****************************************************************
	! Print SubAcc total
	!*****************************************************************
	TEXT$ = SPACE$(37%) + &
		LEFT("Sub Total " + SPACE$(22%), 22%) + &
		FORMAT$(SUB_TOTAL(1%), "#####.## ") + &
		FORMAT$(SUB_TOTAL(2%), "#####.## ") + &
		SPACE$(45%) + &
		FORMAT$(SUB_TOTAL(8%), "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	IF PAGEBREAK$ = "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)
	ELSE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	!
	! Set test emp number to null
	!
	TEST_EMPNUM$ = ""

	SUB_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 10%

	RETURN

	%Page

 OperTotal:
	!*****************************************************************
	! Print Oper total
	!*****************************************************************
	TEXT$ = SPACE$(35%) + &
		LEFT("Oper Total " + SPACE$(24%), 24%) + &
		FORMAT$(OPER_TOTAL(1%), "#####.## ") + &
		FORMAT$(OPER_TOTAL(2%), "#####.## ") + &
		SPACE$(45%) + &
		FORMAT$(OPER_TOTAL(8%), "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Set test emp number to null
	!
	TEST_EMPNUM$ = ""

	OPER_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 10%

	RETURN

	%Page

 LocTotal:
	!*****************************************************************
	! Print Location total
	!*****************************************************************
	TEXT$ = SPACE$(37%) + &
		LEFT("Loc Total " + SPACE$(22%), 22%) + &
		FORMAT$(LOC_TOTAL(1%), "#####.## ") + &
		FORMAT$(LOC_TOTAL(2%), "#####.## ") + &
		SPACE$(45%) + &
		FORMAT$(LOC_TOTAL(8%), "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Set test emp number to null
	!
	TEST_EMPNUM$ = ""

	LOC_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 10%

	RETURN

	%Page

 DeptTotal:
	!*****************************************************************
	! Print Dept total
	!*****************************************************************
	TEXT$ = SPACE$(35%) + &
		LEFT("Dept Total " + SPACE$(24%), 24%) + &
		FORMAT$(DEPT_TOTAL(1%), "#####.## ") + &
		FORMAT$(DEPT_TOTAL(2%), "#####.## ") + &
		SPACE$(45%) + &
		FORMAT$(DEPT_TOTAL(8%), "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	IF PAGEBREAK$ = "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)
	ELSE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	!
	! Set test emp number to null
	!
	TEST_EMPNUM$ = ""

	DEPT_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 10%

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
