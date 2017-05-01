1	%TITLE "Employee List Report"
	%SBTTL "PR_RPRT_EMPLOYEELIST_02"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
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
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
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
	!	03/13/2000 - Kevin Handy
	!		Based on PR_RPRT_EMPLOYEELIST
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_EMPLOYEELIST_02
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_EMPLOYEELIST_02, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_EMPLOYEELIST_02.OBJ;*
	!
	! Modification history:
	!
	!	03/28/2000 - Kevin Handy
	!		Lose Weekly Rate and Vacatio Dollars
	!
	!	03/28/2000 - Kevin Handy
	!		Allow multiple vacation codes
	!
	!	05/25/2000 - Kevin Handy
	!		Additional changes for LL.
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	MAP	(PR_EMP_ACCRUAL)	PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL

	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize report
	!
	THIS_DATE$ = DATE_TODAY
	THIS_DATE% = DATE_DAYCODE(THIS_DATE$)

	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)
	!++
	!
	! Abstract:FLD01
	!	^*(01) Sort (NU,NA,SN,LO,SO)\*
	!	.p
	!	The ^*Sort\* field enters a code which will cause the
	!	report to be sorted in the indicated manner.
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
	!	to end with a particular item in the file.
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

	ACCCODE$ = TRM$(UTL_REPORTX::OPTDEF(4%))
	ACCLIST% = 0%
	I% = INSTR(1%, ACCCODE$, ",")
	WHILE I%
		ACCLIST% = ACCLIST% + 1%
		ACCLIST$(ACCLIST%) = LEFT(ACCCODE$, I% - 1%)
		ACCCODE$ = RIGHT(ACCCODE$, I% + 1%)
		I% = INSTR(1%, ACCCODE$, ",")
	NEXT
	IF ACCCODE$ <> ""
	THEN
		ACCLIST% = ACCLIST% + 1%
		ACCLIST$(ACCLIST%) = ACCCODE$
	END IF

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

	PRINT_INFO$ = TRM$(UTL_REPORTX::OPTDEF(6%))
	!++
	!
	! Abstract:FLD07
	!	^*(06) Print Information\*
	!	.b
	!	This field allows selecting what type of information will be printed on the
	!	report. Valid options are:
	!	.b
	!	*S Social Security Number
	!	.br
	!	*E Last Pay Rate
	!	.br
	!	*R Pay Rate
	!	.br
	!	*A Vacation Accrual
	!	.br
	!	*G General Legder Account Number
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

	WLDCRD_TRADE$ = EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)

	!++
	! Abstract:FLD09
	!	^*(09) Wildcard Trade\*
	!	.p
	!	The ^*WildCard Trade\* field specifies
	!	what trades (as defined in the employee master file)
	!	will be included.
	!
	! Index:
	!	.x Trade>Employee List Report
	!	.x Employee List Report>Trade
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
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "PR_EMP_ACCRUAL"
		CONTINUE HelpError
	END WHEN

340 !

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Employee List Report"
	TITLE$(2%) = "."
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TEXT$ = "EmployeNum EmployeeName                   "
	TEXT$ = TEXT$ + "SocSecNo     " IF INSTR(1%, PRINT_INFO$, "S")
	TEXT$ = TEXT$ + "HireDate "
	TEXT$ = TEXT$ + " LastInc  " IF INSTR(1%, PRINT_INFO$, "E")
	TEXT$ = TEXT$ + "  Rate  " IF INSTR(1%, PRINT_INFO$, "R")
	TEXT$ = TEXT$ + " VacHours" IF INSTR(1%, PRINT_INFO$, "A")
	TEXT$ = TEXT$ + " GL Account        " IF INSTR(1%, PRINT_INFO$, "G")

	TITLE$(4%) = TEXT$

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

	IF WLDCRD_TRADE$ <> ""
	THEN
		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PR_EMP_MASTER::TRADE, -1%), WLDCRD_TRADE$) = 0%
	END IF

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "NU"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				PR_EMP_MASTER::EMPNUM, -1%), WLDCRD$) = 0%
		END IF

	CASE "NA"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				PR_EMP_MASTER::EMPNAME, -1%), WLDCRD$) = 0%
		END IF

	CASE "SN"
		GOTO ExitTotal IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				PR_EMP_MASTER::SSN, -1%), WLDCRD$) = 0%
		END IF

	CASE "LO"
		GOTO ExitTotal IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				PR_EMP_MASTER::LOCATION, -1%), WLDCRD$) = 0%
		END IF

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

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				PR_EMP_MASTER::SORT, -1%), WLDCRD$) = 0%
		END IF
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

	!
	! Build up text string
	!
	TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + " "

	TEXT$ = TEXT$ + PR_EMP_MASTER::SSN + " " &
		IF INSTR(1%, PRINT_INFO$, "S")

	TEXT$ = TEXT$ + &
		HIREDAY$ + " "

	IF INSTR(1%, PRINT_INFO$, "E")
	THEN
		IF THIS_DATE% - DATE_DAYCODE(PR_EMP_MASTER::HIREDAY) < 90% OR &
			PR_EMP_MASTER::HIREDAY <= "00000000"
		THEN
			TEXT$ = TEXT$ + &
				"**********" + " "
		ELSE
			TEXT$ = TEXT$ + &
				PRNT_DATE(EFF_DATE$, 8%) + " "
		END IF
	END IF

	TEXT$ = TEXT$ + &
		FORMAT$(RATE, "###.###") + " " &
		IF INSTR(1%, PRINT_INFO$, "R")

	IF INSTR(1%, PRINT_INFO$, "A")
	THEN
		!
		! Scan for all accrual codes given on the command line
		!
		HOURS = 0.0

		FOR I% = 1% TO ACCLIST%
			WHEN ERROR IN
				GET #PR_EMP_ACCRUAL.CH%, &
					KEY #0% EQ PR_EMP_MASTER::EMPNUM + ACCLIST$(I%), &
					REGARDLESS
			USE
				PR_EMP_ACCRUAL::HOURSUNA = 0.0
				PR_EMP_ACCRUAL::HOURSAVA = 0.0
			END WHEN

			HOURS = HOURS + PR_EMP_ACCRUAL::HOURSAVA
		NEXT I%

		TEXT$ = TEXT$ + FORMAT$(HOURS, "######.# ")
	END IF

	TEXT$ = TEXT$ + &
		PR_EMP_MASTER::ACCT + " " &
		IF INSTR(1%, PRINT_INFO$, "G")

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
			KEY #0% EQ PR_EMP_MASTER::LOCATION + PR_EMP_MASTER::DEPT, &
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
