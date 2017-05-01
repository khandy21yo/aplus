1	%TITLE "Employee List Report"
	%SBTTL "PR_RPRT_EMPLOYEELIST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:PR082
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Employee Report\* is a list of all the employees
	!	within specified parameters.
	!	.lm -5
	!
	! Index:
	!	.x Report>Employee List
	!	.x Employee List>Report
	!	.x Report>Employee List
	!	.x Employee List>Report
	!
	! Option:
	!
	! Author:
	!
	!	12/11/92 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_EMPLOYEELIST
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_EMPLOYEELIST, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_EMPLOYEELIST.OBJ;*
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/04/95 - Kevin Handy
	!		Add ability to page after department totals.
	!
	!	05/04/95 - Kevin Handy
	!		Add alternate print format.
	!
	!	08/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	11/15/96 - Kevin Handy
	!		Clean up.
	!		Look up department descriptions & print out
	!		when in location order.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/16/99 - Kevin Handy
	!		Add entry of date for rate (field 10) [ll]
	!		Change 'Employee Employee' to just 'Employee'
	!
	!	03/09/2000 - Kevin Handy
	!		Added count of employees by location, department,
	!		grand total.
	!		Use WHEN ERROR IN code.
	!
	!	03/22/2000 - Kevin Handy
	!		Add EFF_DATE parameter tp RE_READ_RATE
	!
	!	05/25/2000 - Kevin Handy
	!		Make WLDCRD do something.
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.HB"
	MAP (PR_EMP_RATE)	PR_EMP_RATE_CDD		PR_EMP_RATE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.HB"
	MAP (UTL_DEPARTMENT)	UTL_DEPARTMENT_CDD	UTL_DEPARTMENT

	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)
	!++
	!
	! Abstract:FLD01
	!	^*(01) Sort (NU,NA,SN,LO,SO)\*
	!	.p
	!	The ^*Sort\* field enters a code which will cause the
	!	report will be sorted in the indicated manner.
	!	.p
	!	The valid codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Nu=Number
	!	.le
	!	NA=Name
	!	.le
	!	SN=Social Security Number
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
	!	.x Sort>Employee List Report
	!	.x Employee List Report>Sort
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	!
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.p
	!	The ^*From Item\* setting causes the printing
	!	to begin with a particular item.
	!	.p
	!	A blank field will cause it to start with the first item in the file.
	!
	! Index:
	!	.x From Item>Employee List Report
	!	.x Employee List Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	!
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To Item\* setting causes the printing
	!	to end with a item in the file.
	!	.p
	!	A blank field causes the report to end with the last item in the file.
	!
	! Index:
	!	.x To Item>Employee List Report
	!	.x Employee List Report>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)
	!++
	!
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* setting prints a report including selected
	!	employees only using the wildcarding techniques.
	!	.p
	!	Valid wildcard characters are an asterisk (*) or a question mark (?). An
	!	asterisk (*) indicates all employees will be selected. A question mark (?) in
	!	a field position indicates an employee  with any character in that
	!	equivalent position will be selected.
	!
	! Index:
	!	.x Wildcard>Employee  Employee List Report
	!	.x Employee List Report>Wildcard
	!
	!--

	ONLYDISABLED$ = LEFT$(UTL_REPORTX::OPTDEF(4%), 1%)
	!++
	!
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* setting prints a report including selected
	!	employees only using the wildcarding techniques.
	!	.p
	!	Valid wildcard characters are an asterisk (*) or a question mark (?). An
	!	asterisk (*) indicates all employees will be selected. A question mark (?) in
	!	a field position indicates an employee  with any character in that
	!	equivalent position will be selected.
	!
	! Index:
	!	.x Wildcard>Employee  Employee List Report
	!	.x Employee List Report>Wildcard
	!
	!--

	PAGE_FLAG$ = LEFT(UTL_REPORTX::OPTDEF(5%), 1%)
	!++
	!
	! Abstract:FLD06
	!	^*(04) Page After Section\*
	!	.b
	!	This field allows page breaks to occur after the department/location
	!	changes.
	!
	! Index:
	!
	!--

	PRINT_INFO$ = LEFT(UTL_REPORTX::OPTDEF(6%), 2%)
	!++
	!
	! Abstract:FLD07
	!	^*(06) Print Information\*
	!	.b
	!	This field allows selecting what type of information will be printed on the
	!	report. Valid options are:
	!	.b
	!	*T*D Termination Date
	!	.br
	!	*W*D Weekly Dollars
	!
	! Index:
	!
	!--

	EXCLUDE$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Exclude Terminated Employee\*
	!	.p
	!	The ^*Exclude Terminated Employee\* field allows for the exclusion of all
	!	terminated Employees from the printed ^*Employee List report\*. A ^*Y\*
	!	input allows for the exclusion, while a ^*N\* input would cause the terminated
	!	to be included.
	!
	! Index:
	!	.x Exclude Terminated Employees>Employee List Report
	!	.x Employee List Report>Exclude Terminated Employees
	!
	!--

	TODATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(9%))

	!++
	! Abstract:FLD10
	!	^*(10) Date for Rate\*
	!	.p
	!	Select which date to use to determine which rate to display.
	!	A blank entry in this fiels will cause the current date to be used.
	!
	! Index:
	!	.x Date>Employee List Report
	!	.x Employee List Report>Date
	!
	!--

	SELECT SORT_BY$

	CASE "NU"
		K_NUM% = 0%

	CASE "NA"
		K_NUM% = 1%

	CASE "SN"
		K_NUM% = 3%

	CASE "LO"
		K_NUM% = 4%

	CASE "SO"
		K_NUM% = 2%

	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "UTL_DEPARTMENT"
		CONTINUE HelpError
	END WHEN

330	!

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Employee List Report"
	TITLE$(2%) = "."
	IF ONLYDISABLED$ = "Y"
	THEN
		TITLE$(3%) = "ONLY DISABLED EMPLOYEES"
	ELSE
		TITLE$(3%) = ""
	END IF

	!
	! Column headings
	!
	SELECT PRINT_INFO$

	CASE "WD"
		TITLE$(4%) = "EmployeNum EmployeeName                   " + &
			"SocSecNo    HireDate  Rate WeeklyRate"

	CASE "ND"
		TITLE$(4%) = "EmployeNum EmployeeName                   " + &
			"SocSecNo    HireDate TermDate"

	CASE ELSE
		TITLE$(4%) = "EmployeNum EmployeeName                   " + &
			"SocSecNo    HireDate TermDate   Rate"
	END SELECT

	TITLE$(5%) = "."

	!
	! Layout for printed lines
	!
	LYT_LINE$ = ""

	LAST_LOC$ = "%%%%%%"
	LAST_DEPT$ = "%%%%%%"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	EMPLOYEE_COUNT% = 0%
	LOCATION_COUNT% = 0%
	DEPARTMENT_COUNT% = 0%

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
	! Skip terminated employees
	!
	GOTO 17350 IF (EXCLUDE$ = "Y") AND &
		(PR_EMP_MASTER::TERMDAY > "00000000")

	!
	! Skip if only want disabled employees
	!
	IF ONLYDISABLED$ = "Y"
	THEN
		GOTO 17350 IF PR_EMP_MASTER::DISABLED <> "Y"
	END IF

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "NU"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PR_EMP_MASTER::EMPNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "NA"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PR_EMP_MASTER::EMPNAME, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "SN"
		GOTO ExitTotal IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PR_EMP_MASTER::SSN, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "LO"
		GOTO ExitTotal IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PR_EMP_MASTER::LOCATION, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		!
		! Handle change in location
		!
		IF (LAST_LOC$ <> PR_EMP_MASTER::LOCATION) OR &
			(LAST_DEPT$ <> PR_EMP_MASTER::DEPT)
		THEN
			GOSUB DepartmentTotal

			IF (LAST_LOC$ <> PR_EMP_MASTER::LOCATION)
			THEN
				GOSUB LocationTotal
			END IF

			GOSUB GetDepartment

			IF (PAGE_FLAG$ = "Y") AND (EMPLOYEE_COUNT% <> 0%)
			THEN
				CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, &
					TITLE$(), "", 3000%)
			ELSE
				CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, &
					TITLE$(), "", -2%)
			END IF

			TEXT$ = "Location: " + PR_EMP_MASTER::LOCATION + &
				"  Department: " + PR_EMP_MASTER::DEPT + &
				"  " + UTL_DEPARTMENT::DESCRIPTION

			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, &
				TITLE$(), TEXT$, 0%)
		END IF

		LAST_LOC$ = PR_EMP_MASTER::LOCATION
		LAST_DEPT$ = PR_EMP_MASTER::DEPT

	CASE "SO"
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PR_EMP_MASTER::SORT, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! If they only want the default rate, display that
	! and ignore looking in the rate file
	!
	OPERATION$ = PR_EMP_MASTER::OPER
	CALL PR_READ_RATE(PR_EMP_MASTER::EMPNUM, &
		OPERATION$, &
		TODATE$, &
		RATE_TYPE$, &
		RATE_CDE$, &
		RATE, &
		PIECE_RATE, &
		FACTOR%, &
		STDEFF, &
		EVAL_DATE$, &
		EFF_DATE$)

	GOSUB 18000

17350	GOTO  GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB DepartmentTotal
	GOSUB LocationTotal

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -1%)

	TEXT$ = "Total of " + NUM1$(EMPLOYEE_COUNT%) + " employees printed"
	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -2%)

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
	! Print subtotal by location if necessary
	!*******************************************************************
 LocationTotal:

	IF (SORT_BY$ = "LO") AND (LOCATION_COUNT% <> 0%)
	THEN
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = "Location " + TRM$(LAST_LOC$) + &
			" Total of " + NUM1$(LOCATION_COUNT%) + &
			" employees printed"
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -2%)

	END IF

	LOCATION_COUNT% = 0%

	RETURN

	%PAGE

	!*******************************************************************
	! Print subtotal by department if necessary
	!*******************************************************************
 DepartmentTotal:

	IF (SORT_BY$ = "LO") AND (DEPARTMENT_COUNT% <> 0%)
	THEN
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = "Department " + TRM$(LAST_DEPT$) + &
			" Total of " + NUM1$(DEPARTMENT_COUNT%) + &
			" employees printed"
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -2%)
	END IF

	DEPARTMENT_COUNT% = 0%

	RETURN

	%PAGE

18000	!
	! Print one line
	!
	IF PR_EMP_MASTER::HIREDAY = ""
	THEN
		HIREDAY$ = "        "
	ELSE
		HIREDAY$ = PRNT_DATE(PR_EMP_MASTER::HIREDAY, 6%)
	END IF

	IF PR_EMP_MASTER::TERMDAY = ""
	THEN
		TERMDAY$ = "        "
	ELSE
		TERMDAY$ = PRNT_DATE(PR_EMP_MASTER::TERMDAY, 6%)
	END IF

	IF PR_EMP_MASTER::PHONE = ""
	THEN
		PHONE$ = "        "
	ELSE
		PHONE$ = PRNT_PHONE(PR_EMP_MASTER::PHONE, 0%)
	END IF

	SELECT PRINT_INFO$

	CASE "WD"
		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			PR_EMP_MASTER::EMPNAME + " " + &
			PR_EMP_MASTER::SSN + " " + &
			HIREDAY$ + " " + &
			FORMAT$(RATE, "###.###") + &
			FORMAT$(RATE * 40, "######.##")

	CASE "ND"
		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			PR_EMP_MASTER::EMPNAME + " " + &
			PR_EMP_MASTER::SSN + " " + &
			HIREDAY$ + " " + &
			TERMDAY$ + " "

	CASE ELSE
		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			PR_EMP_MASTER::EMPNAME + " " + &
			PR_EMP_MASTER::SSN + " " + &
			HIREDAY$ + " " + &
			TERMDAY$ + " " + &
			FORMAT$(RATE, "###.###")
	END SELECT

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	EMPLOYEE_COUNT% = EMPLOYEE_COUNT% + 1%
	LOCATION_COUNT% = LOCATION_COUNT% + 1%
	DEPARTMENT_COUNT% = DEPARTMENT_COUNT% + 1%

	RATE = 0.0

	RETURN

	%PAGE

 GetDepartment:
18100	!*******************************************************************
	! Look for a description of the department
	!*******************************************************************

	WHEN ERROR IN
		GET #UTL_DEPARTMENT.CH%, &
			KEY #0% EQ PR_EMP_MASTER::LOCATION + &
			PR_EMP_MASTER::DEPT, &
			REGARDLESS
	USE
		UTL_DEPARTMENT::LOCATION = ""
		UTL_DEPARTMENT::DEPT_NUM = ""
		UTL_DEPARTMENT::DESCRIPTION = ""
		CONTINUE 18190
	END WHEN

18190	RETURN

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
