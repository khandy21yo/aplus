1	%TITLE "Labor Performance 2"
	%SBTTL "PR_RPRT_LABPER02"
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
	! ID:PR012
	!
	! Abstract:HELP
	!	.p
	!	The ^*Labor Performance Report 2\* provides an alternative method to print a
	!	report which displays the work totals and
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
	!	.x Report>Labor Performance 02
	!	.x Labor Performance 02>Report
	!
	! Option:
	!
	! Author:
	!
	!	12/11/87 - Frank F. Starman
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_LABPER02
	!	$ LINK/EXE=PR_EXE:*.EXE PR_RPRT_LABPER02, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_LABPER02.OBJ;*
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
	!		Modified to ignore "A" records.
	!
	!	04/15/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!		Lose commented out code.
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
	DIM EMP_TOTAL(11%), AMT(11%), LOC_TOTAL(11%), &
		DEPT_TOTAL(11%), TOTAL(11%)

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
	!	The ^*Payroll Folder Date\* field enters the particular
	!	payroll date which is to be printed.
	!	.p
	!	An entry is required in this field. The format for entry is
	!	MMDDYYYY or MMDDYY.
	!
	! Index:
	!	.x Payroll Date
	!	.x Date>Payroll
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)


	NONPROD_CODE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Non-Productive Operation\*
	!	.p
	!	The ^*Non-Productive Operation\* field enters the
	!	operation which will receive no immediate payment from a client, but the
	!	company must cover the cost themselves.
	!
	! Index:
	!	.x Non-Productive Operation
	!
	!--


	PAGEBREAK$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Page break\*
	!	.p
	!	The ^*Page Break\* field allows the user to determine if a page break will
	!	be placed between each item.  A ^*Y\* entry causes there to be page break,
	!	while a ^*N\* entry does not.
	!
	! Index:
	!
	!--

300	WHEN ERROR IN
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
	! Title
	!
	TITLE$(1%) = "Labor Performance Report Sorted by Location and Dept"
	TITLE$(2%) = "For the Payroll Folder Dated:  " + PRNT_DATE(BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Heading
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
		"$Oper:059,VActHours:067,VActUnits:076,VActUnitsPerHr:085," + &
		"VActRate:095,$RType:096,VSTDUnitPerHr:104,VSTDRate:114," + &
		"VProd:123,VGross:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	RESET #PR_EMP_MASTER.CH%, KEY #4%

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
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	!GOTO ExitTotal IF PR_EMP_MASTER::LOCATION + &
	!	PR_EMP_MASTER::DEPT > TO.ITEM$ AND &
	!	TO.ITEM$ <> ""

17100	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, KEY #0% EQ PR_EMP_MASTER::EMPNUM, REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	IF TEST_LOCATION$ <> PR_EMP_MASTER::LOCATION AND LINE_PRINTED%
	THEN
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

	IF TEST_DEPT$ <> PR_EMP_MASTER::DEPT AND LINE_PRINTED%
	THEN
		!
		! Print total for an Dept
		!
		GOSUB DeptTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	LINE_PRINTED% = 0%

	TEST_LOCATION$ = PR_EMP_MASTER::LOCATION
	TEST_DEPT$ = PR_EMP_MASTER::DEPT

	TEXT_NAME$ = PR_EMP_MASTER::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME

17110	!
	! Get next employee pay record
	!
	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 11%
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

	!
	! Goto to total employee if different employee number
	!
	GOTO 17200 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM)

	GOTO 17110 IF PR_TRN_PAY::PTYPE = "A"

	!
	! Set employee test value
	!
	CALL PR_READ_SUBJOPER(PR_TRN_PAY::OPER, &
		PR_TRN_PAY::PR_END_DATE, &
		PIECE.RATE, &
		HOUR.RATE)

	AMT(1%) = PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR
	AMT(2%) = PR_TRN_PAY::PIECE
	AMT(3%) = 0.0
	AMT(3%) = FUNC_ROUND(AMT(2%) / AMT(1%), 2%) IF AMT(1%) <> 0.0
	AMT(4%) = PR_TRN_PAY::HOUR_RATE
	AMT(5%) = 0.0
	AMT(5%) = FUNC_ROUND(HOUR.RATE / PIECE.RATE, 2%) IF PIECE.RATE <> 0.0
	AMT(6%) = HOUR.RATE
	AMT(7%) = 0.0
	AMT(7%) = AMT(3%) / AMT(5%) * 100.0 IF AMT(5%) <> 0.0
	AMT(8%) = PR_TRN_PAY::GROSS

	AMT(9%), AMT(10%), AMT(11%) = 0.0

	!
	! Check to see if productive hours
	!
	IF COMP_STRING(TRM$(PR_TRN_PAY::OPER), NONPROD_CODE$) AND TRM$(NONPROD_CODE$) <> ""
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
	! Check to see if productive hours
	!
	IF COMP_STRING(TRM$(PR_TRN_PAY::OPER), NONPROD_CODE$) AND &
		TRM$(NONPROD_CODE$) <> "" OR &
		PR_TRN_PAY::RTYPE <> "P"
	THEN
		AMT(3%), AMT(5%) = 0.0
	ELSE
		!
		! Take the weighted average for productivity
		!
		AMT(3%) = AMT(1%) * AMT(7%)
		AMT(5%) = AMT(1%)
	END IF

	FOR LOOP% = 1% TO 11%
		EMP_TOTAL(LOOP%) = EMP_TOTAL(LOOP%) + AMT(LOOP%)
		LOC_TOTAL(LOOP%) = LOC_TOTAL(LOOP%) + AMT(LOOP%)
		DEPT_TOTAL(LOOP%) = DEPT_TOTAL(LOOP%) + AMT(LOOP%)
		TOTAL(LOOP%) = TOTAL(LOOP%) + AMT(LOOP%)
	NEXT LOOP%

	LINE_PRINTED% = -1%

	GOTO 17110

17200	!
	! Print total for employee
	!
	IF LINE_PRINTED%
	THEN
		!
		! Print total for an employee
		!
		GOSUB EmpTotal

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	EMP_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 11%

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	IF LINE_PRINTED%
	THEN
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
	END IF

	!*****************************************************************
	! Print total
	!*****************************************************************

	TEXT$ = SPACE$(39%) + &
		LEFT("Grand Total" + SPACE$(20%), 20%) + &
		FORMAT$(TOTAL(1%), "#####.## ") + &
		SPACE$(54%) + &
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
	! Count the employees
	DEPT_COUNT% = DEPT_COUNT% + 1%

	TEXT$ = SPACE$(33%) + &
		LEFT("Emp Total" + SPACE$(26%), 26%) + &
		FORMAT$(EMP_TOTAL(1%), "#####.## ") + &
		SPACE$(54%) + &
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
	TEMP = EMP_TOTAL(3%) / EMP_TOTAL(5%) IF EMP_TOTAL(5%) <> 0.0
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

 LocTotal:
	!*****************************************************************
	! Print Location total
	!*****************************************************************

	TEXT$ = SPACE$(37%) + &
		LEFT("Loc Total " + TEST_LOCATION$ + SPACE$(22%), 22%) + &
		FORMAT$(LOC_TOTAL(1%), "#####.## ") + &
		SPACE$(54%) + &
		FORMAT$(LOC_TOTAL(8%), "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Print Average rate
	!
	TEMP = 0.0
	TEMP = LOC_TOTAL(11%) / LOC_TOTAL(10%) IF LOC_TOTAL(10%) <> 0.0
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Average Rate (Prod Hrs)" + &
			FORMAT$(TEMP, "#####.####"), 0%)
	GOTO LocTotal1 IF UTL_REPORTX::STAT

	!
	! Print average productivity level
	!
	TEMP = 0.0
	TEMP = LOC_TOTAL(3%) / LOC_TOTAL(5%) IF LOC_TOTAL(5%) <> 0.0
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Wght Average Prod-Level" + &
			FORMAT$(TEMP, "#####.###%"), 0%)

	GOTO LocTotal1 IF UTL_REPORTX::STAT

	!
	! Print productive hours
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Productive Hours       " + &
			FORMAT$(LOC_TOTAL(10%), "#####.##"), 0%)

	GOTO LocTotal1 IF UTL_REPORTX::STAT

	!
	! Print Non Productive Hours
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Non - Productive Hours " + &
			FORMAT$(LOC_TOTAL(9%), "#####.##"), 0%)

	GOTO LocTotal1 IF UTL_REPORTX::STAT

	!
	! Print Number of employees
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Employee count         " + &
			FORMAT$(LOC_COUNT%, "########"), 0%)

	GOTO LocTotal1 IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	LOC_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 11%

	LOC_COUNT% = 0%

 LocTotal1:
	RETURN

	%Page

 DeptTotal:
	!*****************************************************************
	! Print Dept total
	!*****************************************************************
	! Count the employees
	LOC_COUNT% = LOC_COUNT% + DEPT_COUNT%

	TEXT$ = SPACE$(35%) + &
		LEFT("Dept Total " + TEST_DEPT$ + SPACE$(24%), 24%) + &
		FORMAT$(DEPT_TOTAL(1%), "#####.## ") + &
		SPACE$(54%) + &
		FORMAT$(DEPT_TOTAL(8%), "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Print Average rate
	!
	TEMP = 0.0
	TEMP = DEPT_TOTAL(11%) / DEPT_TOTAL(10%) IF DEPT_TOTAL(10%) <> 0.0
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Average Rate (Prod Hrs)" + &
			FORMAT$(TEMP, "#####.####"), 0%)
	GOTO DeptTotal1 IF UTL_REPORTX::STAT

	!
	! Print average productivity level
	!
	TEMP = 0.0
	TEMP = DEPT_TOTAL(3%) / DEPT_TOTAL(5%) IF DEPT_TOTAL(5%) <> 0.0
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Wght Average Prod-Level" + &
			FORMAT$(TEMP, "#####.###%"), 0%)

	GOTO DeptTotal1 IF UTL_REPORTX::STAT

	!
	! Print productive hours
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Productive Hours       " + &
			FORMAT$(DEPT_TOTAL(10%), "#####.##"), 0%)

	GOTO DeptTotal1 IF UTL_REPORTX::STAT

	!
	! Print Non Productive Hours
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Non - Productive Hours " + &
			FORMAT$(DEPT_TOTAL(9%), "#####.##"), 0%)

	GOTO DeptTotal1 IF UTL_REPORTX::STAT

	!
	! Print Number of employees
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"Employee count         " + &
			FORMAT$(DEPT_COUNT%, "########"), 0%)

	GOTO LocTotal1 IF UTL_REPORTX::STAT

	DEPT_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 11%

	DEPT_COUNT% = 0%

	IF PAGEBREAK$ = "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)
	ELSE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

 DeptTotal1:
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
	!+-+-+
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
	!+-+-+
	!++
	! Abstract:FLD03
	!	^*(03) From Operation/Department\*
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
	!+-+-+
	!++
	! Abstract:FLD04
	!	^*(04) To Subaccount/Location\*
	!	.p
	!	The ^*To Subaccount/Location\* field causes the
	!	printingto end with a certain Subaccount/Location.
	!	.p
	!	A blank field causes the report to end with the last Subaccount/Location
	!	in the field.
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD05
	!	^*(05) To Operation/Department\*
	!	.p
	!	The ^*To Operation/Department\* field causes the printing
	!	to end with a particular Operation/Department.
	!	.p
	!	A blank field causes the report to end with the lastOperation/Department in
	!	the file.
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD07
	!	^*(07) Sort by (NU,NA,LO)\*
	!	.p
	!	The ^*Sort by\* field
	!	indicates how the report will be sorted.
	!	.p
	!	Valid codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	NA = Name
	!	.le
	!	LO = Location
	!	.els
	!	.lm -10
	!	.p
	!	An entry is required in this field. Only the above codes
	!	are valid.
	!
	! Index:
	!
	!--
