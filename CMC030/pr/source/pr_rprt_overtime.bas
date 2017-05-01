1	%TITLE "Employee Overtime Report"
	%SBTTL "PR_RPRT_OVERTIME"
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
	! Computer Management Center, Inc..
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:PR070
	!
	! Abstract:HELP
	!	.p
	!	The ^*Employee Overtime report\* lists all employees who have earned overtime
	!	rates in the defined period and for defined parameters. Included in this
	!	report are the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Location
	!	.le
	!	Department
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Minor Status
	!	.le
	!	Overtime Hours
	!	.le
	!	Overtime Amount
	!	.els
	!
	! Index:
	!	.x Employee Overtime>Report
	!	.x Report>Employee Overtime
	!	.x Overtime>Report
	!	.x Report>Overtime
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_OVERTIME/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_OVERTIME, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_OVERTIME.OBJ;*
	!
	! Author:
	!
	!	02/24/89 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	08/28/89 - Kevin Handy
	!		Fixed bug where report did not work for
	!		closed payroll files.
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
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
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/11/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/03/2000 - Kevin Handy
	!		Use A"x"B
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)	PR_TRN_PAY_CDD		PR_TRN_PAY
	MAP	(PR_HIS_PAY)	PR_TRN_PAY_CDD		PR_HIS_PAY

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	!
	! Abstract:FLD01
	!	^*(01) Payroll Date\*
	!	.p
	!	The ^*Payroll Date\* field enters the date of the payroll which
	!	is to be printed.
	!	.p
	!	The format for entry is MMDDYYYY or MMDDYY.
	!
	! Index:
	!	.x Payroll Date>Overtime Report
	!	.x Overtime Report>Payroll Date
	!
	!--
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	!
	! Abstract:FLD02
	!	^*(02) From Location _#\*
	!	.p
	!	The ^*From Location _#\* setting causes the printing
	!	to begin with a particular location.
	!	.p
	!	A blank field will cause the report to start with the first location in the
	!	file.
	!
	! Index:
	!	.x From Location>Overtime Report
	!	.x Overtime Report>From Location
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	!
	! Abstract:FLD03
	!	^*(03) To Location _#\*
	!	.p
	!	The ^*To Location _#\* setting causes the printing
	!	to end with a particular location.
	!	.p
	!	A blank field will cause the report to end with the last location in the file.
	!
	! Index:
	!	.x To Location>Overtime Report
	!	.x Overtime Report>To Location
	!
	!--

	DEPT$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	!
	! Abstract:FLD04
	!	^*(04) Department\*
	!	.p
	!	The ^*Department\* field signifies which department is to be examined in
	!	finding the overtime hours by inserting the department number. If all
	!	departments are to be examined, leave the field blank or insert an _*.
	!
	! Index:
	!	.x Department>Overtime Report
	!	.x Overtime Report>Department
	!
	! Datatype:TEXT
	! Size:6
	!--

	WCENTER$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)
	!++
	!
	! Abstract:FLD05
	!	^*(05) Workcenter\*
	!	.p
	!	The ^*Workcenter\* is a smaller division within the department. It may be
	!	left blank to get a report including all workcenters or individual centers
	!	may be identified.
	!
	! Index:
	!	.x Workcenter>Overtime Report
	!	.x Overtime Report>Workcenter
	!
	! Datatype:TEXT
	! Size:6
	!--

	LIMIT = VAL(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))
	!++
	!
	! Abstract:FLD06
	!	^*(06) Over Hours\*
	!	.p
	!	The ^*Over Hours\* sets a parameter for the report to follow in extracting
	!	people that have exceeded the overtime limit. For example, if the ^*Over
	!	Hours\* is set at ten (10), only those employees who have exceeded ten (10)
	!	hours of overtime will be included.
	!
	! Index:
	!	.x Over Hours>Overtime Report
	!	.x Overtime Report>Over Hours
	!
	! Datatype:FLOATING POINT
	! Size:8
	!--

	ONLY_TOTAL$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)
	!++
	!
	! Abstract:FLD07
	!	^*(07) Print Totals Only_?\*
	!	.p
	!	The ^*Print Totals\* command allows for only the report totals or the entire
	!	report to be printed. If ^*Y\* is entered, only the report totals will be
	!	printed. If ^*N\* is entered, the entire report will be printed.
	!
	! Index:
	!	.x Print Totals>Overtime Report
	!	.x Overtime Report>Print Totals
	!
	! Datatype:TEXT
	! Size:1
	! Required:Y
	! Valid Input: Y,N
	!--

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
	! Open Pay folder
	!
	USE_HISTORY% = -1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	PR_TRN_PAY.CH% = PR_HIS_PAY.CH%

	%PAGE

 ReportTitle:
	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Payroll Overtime Report"
	TITLE$(2%) = "For the Payroll Folder Dated:  " + &
		PRNT_DATE(BATCH_NO$, 8%) + &
		" and over" + FORMAT$(LIMIT, "###") + " hours"
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "Loc  Dep    WoCe EmpNum     EmpName              " + &
		"          MS  OvHours OvAmount"
	TITLE$(5%) = "."

	!
	! Line layouts
	!
	LYT_LINE$ = "$Location:004,$Dept:011,$WorkCenter:016," + &
		"$EmpNum:027,$EmpName:058,$MinorStat:061,VOvLimit:070,VOvAmt:079"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_TRN_PAY.CH%, KEY #2%
		ELSE
			FIND #PR_TRN_PAY.CH%, KEY #2% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_TRN_PAY"
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
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	GOTO GetNextRec IF PR_TRN_PAY::OVT_HR <= LIMIT

	GOTO GetNextRec IF PR_TRN_PAY::PTYPE = "A"

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_TRN_PAY::LOCATION > TO_ITEM$) AND &
		TO_ITEM$ <> ""
	GOTO GetNextRec IF COMP_STRING(PR_TRN_PAY::DEPT, DEPT$) = 0% AND &
		DEPT$ <> ""
	GOTO GetNextRec &
		IF COMP_STRING(PR_TRN_PAY::WORK_CENTER, WCENTER$) = 0% AND &
		WCENTER$ <> ""

17040	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, &
			KEY #0% EQ PR_TRN_PAY::EMPNUM, &
			REGARDLESS
	USE
		PR_EMP_MASTER::EMPNAME = &
			STRING$(LEN(PR_EMP_MASTER::EMPNAME), A"?"B)

		CONTINUE 17100 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

17100	AMOUNT = FUNC_ROUND(PR_TRN_PAY::HOUR_RATE * PR_TRN_PAY::FACTOR * &
		(PR_TRN_PAY::OVT_HR - LIMIT) / 100.0, 2%)

17110	MINOR_STAT = VAL%(LEFT(BATCH_NO$, 4%)) - &
		VAL%(LEFT(PR_EMP_MASTER::BIRTH, 4%))

	IF MINOR_STAT < 18
	THEN
		MINOR_STAT$ = FORMAT$(MINOR_STAT, "##")
	ELSE
		MINOR_STAT$ = "  "
	END IF

	!
	! Check and see if we print work center totals.
	!
	IF PR_TRN_PAY::LOCATION + PR_TRN_PAY::DEPT + &
		PR_TRN_PAY::WORK_CENTER <> TEST_LOCATION$ + TEST_DEPT$ + &
		TEST_WORK_CENTER$ AND FIRST_PASS%
	THEN
		GOSUB 18000 IF TEST_WORK_CENTER$ <> " "
		WC_TOTAL_H = 0.0
		WC_TOTAL_A = 0.0
	END IF

	!
	! Check and see if we print department totals.
	!
	IF PR_TRN_PAY::LOCATION + PR_TRN_PAY::DEPT <> &
		TEST_LOCATION$ + TEST_DEPT$ AND FIRST_PASS%
	THEN
		GOSUB 18100 IF TEST_DEPT$ <> " "
		DEPT_TOTAL_H = 0.0
		DEPT_TOTAL_A = 0.0
	END IF

	!
	! Check and see if we print location totals.
	!
	GOSUB 18200 IF PR_TRN_PAY::LOCATION <> TEST_LOCATION$ AND FIRST_PASS%

	IF ONLY_TOTAL$<>"Y"
	THEN
		TEXT$ = PR_TRN_PAY::LOCATION + " " + &
			PR_TRN_PAY::DEPT + " " + &
			PR_TRN_PAY::WORK_CENTER + " " + &
			PR_TRN_PAY::EMPNUM + " " + &
			PR_EMP_MASTER::EMPNAME + " " + &
			MINOR_STAT$ + " " + &
			FORMAT$(PR_TRN_PAY::OVT_HR - LIMIT, "#####.##") + " " + &
			FORMAT$(AMOUNT, "#####.##")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	FIRST_PASS% = -1%

	GRAND_TOTAL_H = GRAND_TOTAL_H + PR_TRN_PAY::OVT_HR - LIMIT
	GRAND_TOTAL_A = GRAND_TOTAL_A + AMOUNT

	LOC_TOTAL_H = LOC_TOTAL_H + PR_TRN_PAY::OVT_HR - LIMIT
	LOC_TOTAL_A = LOC_TOTAL_A + AMOUNT

	DEPT_TOTAL_H = DEPT_TOTAL_H + PR_TRN_PAY::OVT_HR - LIMIT
	DEPT_TOTAL_A = DEPT_TOTAL_A + AMOUNT

	WC_TOTAL_H = WC_TOTAL_H + PR_TRN_PAY::OVT_HR - LIMIT
	WC_TOTAL_A = WC_TOTAL_A + AMOUNT

	TEST_LOCATION$ = PR_TRN_PAY::LOCATION
	TEST_DEPT$ = PR_TRN_PAY::DEPT
	TEST_WORK_CENTER$ = PR_TRN_PAY::WORK_CENTER

	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Check for subtotals
	!
	GOSUB 18000 IF DEPT_TOTAL_H <> WC_TOTAL_H
	GOSUB 18100 IF DEPT_TOTAL_H <> LOC_TOTAL_H
	GOSUB 18200
	!
	! Handle end of report
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 0%)

	TEXT$ = SPACE$(28%) + "Grand Total"

	TEXT$ = LEFT(TEXT$ + SPACE$(34%), 62%) + &
		FORMAT$(GRAND_TOTAL_H, "#####.##") + " " + &
		FORMAT$(GRAND_TOTAL_A, "#####.##")

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

18000	! Subtotal Checks

	IF TRM$(TEST_WORK_CENTER$) <> "" AND &
		(PR_TRN_PAY::DEPT = TEST_DEPT$ OR &
		DEPT_TOTAL_H <> WC_TOTAL_H)
	THEN
		TEXT$ = TEST_LOCATION$ + " " + &
			TEST_DEPT$ + " " + &
			TEST_WORK_CENTER$ + " " + &
			SPACE$(09%) + " Work Center Total"
		TEXT$ = LEFT(TEXT$ + STRING$(34%, A"."B), 62%) + &
			FORMAT$(WC_TOTAL_H, "#####.##") + " " + &
			FORMAT$(WC_TOTAL_A, "#####.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF
	RETURN

18100	IF TRM$(TEST_DEPT$) <> "" AND &
		(PR_TRN_PAY::LOCATION = TEST_LOCATION$ &
		OR DEPT_TOTAL_H <> LOC_TOTAL_H)
	THEN
		TEXT$ = TEST_LOCATION$ + " " + &
			TEST_DEPT$ + " " + &
			SPACE$(16%) + " Department Total"
		TEXT$ = LEFT(TEXT$ + STRING$(34%, A"."B), 62%) + &
			FORMAT$(DEPT_TOTAL_H, "#####.##") + " " + &
			FORMAT$(DEPT_TOTAL_A, "#####.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF
	RETURN

18200	TEXT$ = TEST_LOCATION$ + " " + &
		SPACE$(23%) + " Location Total"
	TEXT$ = LEFT(TEXT$ + STRING$(34%, A"."B), 62%) + &
		FORMAT$(LOC_TOTAL_H, "#####.##") + " " + &
		FORMAT$(LOC_TOTAL_A, "#####.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
	LOC_TOTAL_H, LOC_TOTAL_A = 0.0
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
