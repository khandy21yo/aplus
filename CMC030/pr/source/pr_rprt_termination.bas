1	%TITLE "Employee Termination Report"
	%SBTTL "PR_RPRT_TERMINATION"
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
	! ID:PR081
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Employee Termination Report\* lists all employees who have been
	!	terminated arranged under the following columnar headings:
	!	.table 3,25
	!	.te
	!	Location
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name
	!	.te
	!	Hire Date
	!	.te
	!	Operation
	!	.te
	!	Rate
	!	.te
	!	Termination Date
	!	.te
	!	Days
	!	.te
	!	Phone Number
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Employee Termination>Report
	!	.x Report>Employee Termination
	!
	! Option:
	!
	! Author:
	!
	!	02/08/89 - J. Shad Rydalch
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TERMINATION
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_TERMINATION, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TERMINATION.OBJ;*
	!
	! Modification history:
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	05/22/91 - Kevin Handy
	!		Modified to handle TERMDAY more consistantly.
	!
	!	05/22/91 - Kevin Handy
	!		Reformatted if-then-else.
	!
	!	 07/21/93 - Kevin Handy
	!		Removed rate information, since nobody seemed to
	!		want it, and it used massive amounts of paper.
	!		(I think this was taken from a rate report)
	!
	!	 07/21/93 - Kevin Handy
	!		Fixed up code for when the hire date was blank.
	!		It could give horrible output.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	05/25/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!++
	!
	! Abstract:FLD01
	!	^*(01) Sort (NU,NA,SN,LO,SO)\*
	!	.P
	!	The ^*Sort\* field enters a code which causes the
	!	report to be sorted in the indicated manner.
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
	!	.x Sort>Employee Termination Report
	!	.x Employee Termination Report>Sort
	!
	!--
	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	!
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.p
	!	The ^*From Item\* setting causes the printing
	!	to begin with a particular item.
	!	.p
	!	A blank field will cause the report to start with the first item in the file.
	!
	! Index:
	!	.x From Item>Employee Termination Report
	!	.x Employee Termination Report>From Item
	!
	!--
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	!
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To Item\* field causes the printing
	!	to end with a particular item.
	!	.p
	!	A blank field will cause the report to end with the last item.
	!
	! Index:
	!	.x To Item>Employee Termination Report
	!	.x Employee Termination Report>To Item
	!
	!--
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	!
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* setting enables the user to print a report including selected
	!	employees only using the wildcarding technique.
	!	.p
	!	Valid wildcard characters are an asterisk (_*) or a question mark (?). An
	!	asterisk (_*) indicates all terminated employees will be printed. A question
	!	mark (?) in a field position indicates an employee with any character in that
	!	equivalent position will be searched out.
	!
	! Index:
	!	.x Wildcard>Employee Termination Report
	!	.x Employee Termination Report>Wildcard
	!
	! Datatype:TEXT
	! Size:20
	!--
	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	!
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.p
	!	The ^*From Date\* field causes the printing
	!	to begin with a certain date.
	!	.p
	!	A blank field will cause the report to begin printing with the first date in
	!	the file.
	!
	! Index:
	!	.x From Date>Employee Termination Report
	!	.x Employee Termination Report>From Date
	!
	!--
	FROMDATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))

	!++
	!
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.p
	!	The ^*To Date\* setting causes the printing
	!	to end with a certain date.
	!	.p
	!	A blank field will cause the report to end with the last date in the file.
	!
	! Index:
	!	.x To Date>Employee Termination Report
	!	.x Employee Termination Report>To Date
	!
	!--
	TODATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(6%), 132%))

	!++
	!
	! Abstract:FLD08
	!	^*(08) Over Days\*
	!	.p
	!	The ^*Over Days\* field is used as a parameter by establishing where the
	!	report starts depending on the days since the employee termination. For
	!	example, if the ^*Over Days\* field contains a ten (10), the report will only
	!	print those employees that have been terminated for more that ten (10) days.
	!
	! Index:
	!	.x Over Days>Employee Termination Report
	!	.x Employee Termination Report>Over Days
	!
	!--
	LIMIT% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(7%), 132%))

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

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Employee Termination Report"
	TITLE$(2%) = "From:  " + PRNT_DATE(FROMDATE$, 6%) + &
		"  To:  " + PRNT_DATE(TODATE$, 6%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "Loc  EmployeNum EmployeeName                   " + &
		"HireDate TermDate   Days PhoneNumber"
	TITLE$(5%) = "."

	!
	! Line layouts
	!
	LYT_LINE = "$Location:004,$EmpNum:015,$EmpName:046," + &
		"DHireDate:055,DTerminDate:064," + &
		"VDaysWorking:071,PPhoneNumber:080"

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

	CASE "SO"
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PR_EMP_MASTER::SORT, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	GOTO 17350 IF PR_EMP_MASTER::TERMDAY < FROMDATE$ OR &
		PR_EMP_MASTER::TERMDAY > TODATE$

	!
	! Print one line
	!
	IF PR_EMP_MASTER::HIREDAY <= "00000000"
	THEN
		HIREDAY$ = "        "
	ELSE
		HIREDAY$ = PRNT_DATE(PR_EMP_MASTER::HIREDAY, 6%)
	END IF

	IF PR_EMP_MASTER::TERMDAY <= "00000000"
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

	IF HIREDAY$ = ""
	THEN
		DAYS% = 0%
	ELSE
		DAYS% = DATE_DAYCODE(PR_EMP_MASTER::TERMDAY) - &
			DATE_DAYCODE(PR_EMP_MASTER::HIREDAY)

		GOTO 17350 IF DAYS% < LIMIT%
	END IF

	TEXT$ = PR_EMP_MASTER::LOCATION + " " + &
		PR_EMP_MASTER::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + " " + &
		HIREDAY$ + " " + &
		TERMDAY$ + " " + &
		FORMAT$(DAYS%, "##,###") + " " + &
		PHONE$

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!

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
