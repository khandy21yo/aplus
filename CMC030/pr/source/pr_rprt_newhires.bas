1	%TITLE "New Hires Report"
	%SBTTL "PR_RPRT_NEWHIRES"
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
	! ID:PR082
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Employee New Hires Report\* is a list of all the newly hired employees
	!	within a specified time period and parameters. This report contains the
	!	following columnar headings:
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
	!	Address
	!	.te
	!	City
	!	.te
	!	State
	!	.te
	!	Phone Number
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>New Hires
	!	.x New Hires>Report
	!	.x Report>Employee New Hires
	!	.x Employee New Hires>Report
	!
	! Option:
	!
	! Author:
	!
	!	02/14/89 - J. Shad Rydalch
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_NEWHIRES
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_NEWHIRES, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_NEWHIRES.OBJ;*
	!
	! Modification history:
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to a spreadsheet or to a DIF file.
	!
	!	11/06/90 - Val Allen
	!		Modified for option to print terminated employees Y/N
	!
	!	05/22/91 - Kevin Handy
	!		Modified to handle TERMDAY more consistantly.
	!
	!	07/09/91 - Kevin Handy
	!		Added ability to print SSN's.
	!		Commented out junk code.
	!		Reformatted goofy if-then-else lines.
	!
	!	07/18/91 - Kevin Handy
	!		Added minimum age field and code.
	!
	!	10/21/92 - Kevin Handy
	!		Added "Default Rate Only" option.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Clean out commented out code.
	!
	!	01/29/96 - Kevin Handy
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...)
	!		in several places.
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/22/2000 - Kevin Handy
	!		Add EFF_DATE parameter to PR_READ_RATE
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.HB"
	MAP (PR_EMP_RATE)	PR_EMP_RATE_CDD		PR_EMP_RATE

	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize report
	!
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
	!	.x Sort>Employee New Hires Report
	!	.x Employee New Hires Report>Sort
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
	!	.x From Item>Employee New Hires Report
	!	.x Employee New Hires Report>From Item
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
	!	.x To Item>Employee New Hires Report
	!	.x Employee New Hires Report>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)
	!++
	!
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* setting includes selected
	!	employees only using the wildcarding techniques.
	!	.p
	!	Valid wildcard characters are an asterisk (*) or a question mark (?). An
	!	asterisk (*) indicates all employees will be selected. A question mark (?) in
	!	a field position indicates an employee  with any character in that
	!	equivalent position will be selected.
	!
	! Index:
	!	.x Wildcard>Employee  New Hires Report
	!	.x Employee New Hires Report>Wildcard
	!
	!--

	MIN_AGE% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)) * 12%
	THIS_DATE% = DATE_MONCODE(DATE_TODAY)

	!++
	!
	! Abstract:FLD05
	!	^*(04) Minimum Age\*
	!	.p
	!	The ^*Minimum Age\* field is used to limit the report
	!	to those employees that are at least a certain age.
	!
	! Index:
	!	.x Minimum Age>Employee  New Hires Report
	!	.x Employee New Hires Report>Minimum Age
	!
	! Datatype:TEXT
	! Size:20
	!--

	FROMDATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))
	!++
	!
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.p
	!	The ^*From Date\* field causes the printing
	!	to begin with a certain date.
	!	.p
	!	A blank field will cause the report to begin with the first date in the file.
	!
	! Index:
	!	.x From Date>Employee New Hires Report
	!	.x Employee New Hires Report>From Date
	!
	!--

	TODATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(6%), 132%))
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
	!	.x To Date>Employee New Hires Report
	!	.x Employee New Hires Report>To Date
	!
	!--

	EXCLUDE$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Exclude Terminated Employee\*
	!	.p
	!	The ^*Exclude Terminated Employee\* field allows for the exclusion of all
	!	terminated Employees from the printed ^*New Hires report\*. A ^*Y\*
	!	input allows for the exclusion, while a ^*N\* input would cause the terminated
	!	to be included.
	!
	! Index:
	!	.x Exclude Terminated Employees>Employee New Hires Report
	!	.x Employee New Hires Report>Exclude Terminated Employees
	!
	!--

	SSNFLAG$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(8%), -1%), 1%)

	!++
	! Abstract:FLD09
	!	^*(09) Show SSN\*
	!	.P
	!	The ^*Show SSN\* field is used to enable or disable
	!	printing the social security number for each employee.
	!
	! Index:
	!	.X Show SSN>New hires report
	!	.X New hires report>Show SSN
	!
	!--

	RATEFLAG$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(9%), -1%), 1%)

	!++
	! Abstract:FLD09
	!	^*(09) Default rate Only\*
	!	.P
	!	The ^*Default Rate Only\* field is used to
	!	control printing of either all rates for an employee,
	!	or just his default rate. A 'Z' will cause no rates
	!	to be displayed.
	!
	! Index:
	!	.X Default Rate>New hires report
	!	.X New hires report>Default Rate
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

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Employee New Hires Report"
	TITLE$(2%) = "From " + PRNT_DATE(FROMDATE$, 6%) + " To " + &
		PRNT_DATE(TODATE$, 6%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "Loc  EmployeNum EmployeeName                   " + &
		"HireDate Operation   Rate Address              " + &
		"City             ST PhoneNumber"
	TITLE$(5%) = "."

	!
	! Layout for printed lines
	!
	LYT_LINE$ = "$Location:004,$EmpNum:015,$EmpName:046," + &
		"DHireDate:055,$Oper:064,VRate:072,$Address:093," + &
		"$City:110,$State:113,PPhoneNum:122"

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

	GOTO 17350 IF PR_EMP_MASTER::HIREDAY < FROMDATE$ OR &
		PR_EMP_MASTER::HIREDAY > TODATE$

	THIS_AGE% = THIS_DATE% - DATE_MONCODE(PR_EMP_MASTER::BIRTH)
	THIS_AGE% = 0% IF THIS_AGE% < 0%
	GOTO 17350 IF THIS_AGE% < MIN_AGE%

	GOTO 17350 IF (EXCLUDE$ = "Y") AND &
		(PR_EMP_MASTER::TERMDAY > "00000000")


	!
	! If they do not want any rates
	!
	IF RATEFLAG$ = "Z"
	THEN
		OPERATION$ = PR_EMP_MASTER::OPER
		ARTE = 0.0
		GOSUB 18000
		GOTO  GetNextRec
	END IF

	!
	! If they only want the default rate, display that
	! and ignore looking in the rate file
	!
	IF RATEFLAG$ = "Y"
	THEN
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
		GOTO  GetNextRec
	END IF

17200	RATE = 0.0
	FLAG% = 0%
	LAST_OPER$ = ""
	OPERATION$ = SPACE$(LEN(PR_EMP_RATE::OPER))

	WHEN ERROR IN
		FIND #PR_EMP_RATE.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17250 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

17210	WHEN ERROR IN
		GET #PR_EMP_RATE.CH%, REGARDLESS
	USE
		CONTINUE 17250 IF ERR = 11%
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

	GOTO 17250 IF PR_EMP_RATE::EMPNUM <> PR_EMP_MASTER::EMPNUM

	IF PR_EMP_RATE::OPER <> LAST_OPER$ AND FLAG%
	THEN
		GOSUB 18000
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	IF PR_EMP_RATE::EFFDAT <= TODATE$
	THEN
		RATE = PR_EMP_RATE::HOUR_RATE
		OPERATION$ = PR_EMP_RATE::OPER
	END IF

	FLAG% = -1%
	LAST_OPER$ = PR_EMP_RATE::OPER
	GOTO 17210

17250	GOSUB 18000
	GOTO ExitProgram IF UTL_REPORTX::STAT

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

18000	!
	! Print one line
	!
	IF PR_EMP_MASTER::HIREDAY = ""
	THEN
		HIREDAY$ = "        "
	ELSE
		HIREDAY$ = PRNT_DATE(PR_EMP_MASTER::HIREDAY, 6%)
	END IF

	IF PR_EMP_MASTER::PHONE = ""
	THEN
		PHONE$ = "        "
	ELSE
		PHONE$ = PRNT_PHONE(PR_EMP_MASTER::PHONE, 0%)
	END IF

	TEXT$ = PR_EMP_MASTER::LOCATION + " " + &
		PR_EMP_MASTER::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + " " + &
		HIREDAY$ + " " + &
		OPERATION$ + " " + &
		FORMAT$(RATE, "<%>##.###") + " " + &
		PR_EMP_MASTER::ADD2 + " " + &
		PR_EMP_MASTER::CITY + " " + &
		PR_EMP_MASTER::STATE + " " + &
		PHONE$

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	IF SSNFLAG$ = "Y"
	THEN
		TEXT$ = "    " + " " + &
			"          " + " " + &
			PR_EMP_MASTER::SSN

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -1%)
	END IF

	RATE = 0.0

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
