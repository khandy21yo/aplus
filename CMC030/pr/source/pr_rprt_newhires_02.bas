1	%TITLE "Employee New Hires Report"
	%SBTTL "PR_RPRT_NEWHIRES_02"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988, 1989 BY
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
	! ID:PR085
	!
	! Abstract:HELP
	!	.p
	!	The ^*Employee New Hires report\* prints a listing of all the newly hired
	!	employees which contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Location
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Age
	!	.le
	!	Hire Date
	!	.le
	!	Operation
	!	.le
	!	Rate
	!	.le
	!	Address
	!	.le
	!	City
	!	.le
	!	State
	!	.le
	!	Phone Number
	!	.els
	!
	! Index:
	!	.x Employee New Hires>Report
	!	.x Report>Employee New Hires
	!
	! Option:
	!
	! Author:
	!
	!	02/14/89 - J. Shad Rydalch
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_NEWHIRES_02
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_NEWHIRES_02, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_NEWHIRES_02.OBJ;*
	!
	! Modification history:
	!
	!	06/05/1989 - B. Craig Larsen
	!		Modified to check the rate range table.
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to a spreadsheet or a DIF file.
	!
	!	07/18/91 - Kevin Handy
	!		Modified to have all the same report-settings fields
	!		that the PR_RPRT_NEWHIRES report has.
	!
	!	07/18/91 - Kevin Handy
	!		Modified to show everyones age, and not just those
	!		under 18.
	!
	!	07/18/91 - Kevin Handy
	!		Modified to blank age if no birthdate is given.
	!
	!	09/25/91 - Kevin Handy
	!		Added "LN" sort option. (Location - department -
	!		work center - employee number.
	!
	!	09/25/91 - Kevin Handy
	!		Added "LA" sort order.  Location - Alpha sort.
	!
	!	04/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	09/10/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/16/97 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.HB"
	MAP (PR_EMP_RATE)	PR_EMP_RATE_CDD		PR_EMP_RATE

	%INCLUDE "SOURCE:[PR.OPEN]PR_RATERANGE.HB"
	MAP	(PR_RATERANGE)	PR_RATERANGE_CDD	PR_RATERANGE

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)
	!++
	! Abstract:FLD01
	!	^*(01) Sort (NU,NA,LO,SO)\*
	!	.p
	!	The ^*Sort\* field enters a code which causes the report
	!	to be sorted in the indicated manner.
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
	!	LO = Location, department, name
	!	.le
	!	SO = Alphabetical
	!	.le
	!	SN = Social Security Number
	!	.le
	!	LN = Location, department, employee number
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
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.p
	!	The ^*From Item\* field causes the printing
	!	to begin with a certain item.
	!	.p
	!	A blank field causes the report to begin with the first item in the file.
	!
	! Index:
	!	.x From Item>Employee New Hires Report
	!	.x Employee New Hires Report>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To Item\* field causes the printing to end with a certain item.
	!	.p
	!	A blank field causes the report to end with the last item in the file.
	!
	! Index:
	!	.x To Item>Employee New Hires Report
	!	.x Employee New Hires Report>To Item
	!	.x Item>To
	!
	!--

	MIN_AGE% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)) * 12
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
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.p
	!	The ^*From Date\* field causes the printing
	!	to begin with a certain date.
	!	A blank field causes the report to begin with the first date in the file.
	!
	! Index:
	!	.x From Date
	!
	!--

	TODATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(6%), 132%))
	TODATE$ = DATE_TODAY IF TODATE$ = ""
	!++
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.p
	!	The ^*To Date\* causes the printing to end with a
	!	certain date.  A blank field causes the report to end with the last date in
	!	the file.
	!
	! Index:
	!	.x To Date
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
	!	^*(09) SHOW SSN\*
	!	.P
	!	The ^*Show SSN\* field is used to enable or disable
	!	printing the social security number for each employee.
	!
	! Index:
	!	.X Show SSN>New hires report
	!
	!--

200	!
	! Open employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

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

	CASE "LN"
250		!
		! Sort Payroll Employee master file
		!
		CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file", 16% + 1%)
		CALL ASSG_CHANNEL(PR_EMP_MASTER_TEMP.CH%, STAT%)

		WHEN ERROR IN
			OPEN "TEMP.TEMP" FOR OUTPUT AS FILE PR_EMP_MASTER_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				MAP PR_EMP_MASTER, &
				BUFFER 32%, &
				TEMPORARY, &
				PRIMARY KEY &
				( &
					PR_EMP_MASTER::LOCATION, &
					PR_EMP_MASTER::DEPT, &
					PR_EMP_MASTER::WORK_CENTER, &
					PR_EMP_MASTER::EMPNUM &
				), &
				ACCESS MODIFY, ALLOW NONE

			RESET #PR_EMP_MASTER.CH%
		USE
			FILENAME$ = "TEMP.TEMP"
			CONTINUE HelpError
		END WHEN

260		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, REGARDLESS
		USE
			CONTINUE 265 IF ERR = 11%		! end of file
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

		!
		! Check to see if it is a record we really want,
		! if there is not an errorTHEN we want it.
		!
		WHEN ERROR IN
			PUT #PR_EMP_MASTER_TEMP.CH%
		USE
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

		GOTO 260

265		K_NUM% = 0%
		PR_EMP_MASTER.CH% = PR_EMP_MASTER_TEMP.CH%

	CASE "LA"
270		!
		! Sort Payroll Employee master file
		!
		CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file", 16% + 1%)
		CALL ASSG_CHANNEL(PR_EMP_MASTER_TEMP.CH%, STAT%)

		WHEN ERROR IN
			OPEN "TEMP.TEMP" FOR OUTPUT AS FILE PR_EMP_MASTER_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				MAP PR_EMP_MASTER, &
				BUFFER 32%, &
				TEMPORARY, &
				PRIMARY KEY &
				( &
					PR_EMP_MASTER::LOCATION, &
					PR_EMP_MASTER::SORT &
				), &
				ACCESS MODIFY, ALLOW NONE

			RESET #PR_EMP_MASTER.CH%
		USE
			FILENAME$ = "TEMP.TEMP"
			CONTINUE HelpError
		END WHEN

280		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, REGARDLESS
		USE
			CONTINUE 285 IF ERR = 11%		! end of file
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

		!
		! Check to see if it is a record we really want,
		! if there is not an errorTHEN we want it.
		!
		WHEN ERROR IN
			PUT #PR_EMP_MASTER_TEMP.CH%
		USE
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

		GOTO 280

285		K_NUM% = 0%
		PR_EMP_MASTER.CH% = PR_EMP_MASTER_TEMP.CH%

	END SELECT

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_RATERANGE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "PR_RATERANGE"
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
	TITLE$(4%) = "Asterisk (*) Indicates Rate out of Range for Age."
	TITLE$(5%) = "  Loc  EmployeNum EmployeeName                  " + &
		"Age HireDate Oper        Rate Address              " + &
		"City             ST PhoneNumber"
	TITLE$(6%) = "."

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$WarnFlag:001,$Location:006,$EmpNum:017," + &
		"$EmpName:047,VTheAge:051,DHireDate:060,$Oper:069," + &
		"VRate:077,$Address:098,$City:115,$State:118,PPhone:127"

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
	CASE "NA"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "SN"
		GOTO ExitTotal IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "LO"
		GOTO ExitTotal IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "SO"
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	GOTO 17350 IF PR_EMP_MASTER::HIREDAY < FROMDATE$ OR &
		PR_EMP_MASTER::HIREDAY > TODATE$

	THIS_AGE% = THIS_DATE% - DATE_MONCODE(PR_EMP_MASTER::BIRTH)
	THIS_AGE% = 0% IF THIS_AGE% < 0%
	GOTO 17350 IF THIS_AGE% < MIN_AGE%

	GOTO 17350 IF (EXCLUDE$ = "Y") AND &
		(PR_EMP_MASTER::TERMDAY > "00000000")

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

	WHEN ERROR IN
		THE_AGE% = FIX((VAL(TODATE$) - &
			VAL(PR_EMP_MASTER::BIRTH)) / 10000%)
	USE
		THE_AGE% = 0%
	END WHEN

	THE_AGE% = 0% &
		IF (THE_AGE% < 0%) OR &
		(PR_EMP_MASTER::BIRTH <= "00000000") ! OR THE_AGE% >= 18%

18020	XWARN$ = " "
	WHEN ERROR IN
		GET #PR_RATERANGE.CH%, &
			KEY #0% EQ PR_EMP_MASTER::LOCATION + NUM1$(THE_AGE%), &
			REGARDLESS
	USE
		CONTINUE 18022
	END WHEN

	XWARN$ = "*" IF RATE < PR_RATERANGE::MIN_RATE OR &
		RATE > PR_RATERANGE::MAX_RATE

18022	TEXT$ = XWARN$	+ " " + &
		PR_EMP_MASTER::LOCATION + " " + &
		PR_EMP_MASTER::EMPNUM + " " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 29%) + " " + &
		FORMAT$(THE_AGE%, "<%>##") + " " + &
		HIREDAY$ + " " + &
		OPERATION$ + " " + &
		FORMAT$(RATE, "###.###") + " " + &
		PR_EMP_MASTER::ADD1 + " " + &
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
	PR_EMP_MASTER::EMPNAME = ""
	PR_EMP_MASTER::HIREDAY = ""
	PR_EMP_MASTER::ADD1 = ""
	PR_EMP_MASTER::CITY = ""
	PR_EMP_MASTER::STATE = ""
	PR_EMP_MASTER::PHONE = ""

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
	!+-+-+
	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* setting prints a report including selected
	!	employees only using the wildcarding techniques.
	!	.p
	!	Valid wildcard characters are an asterisk (_*) or a question mark (_?). An
	!	asterisk (_*) indicates all employees will be selected. A question mark (_?)
	!	in a field position indicates an employee with any character in that position
	!	will be selected.
	!
	! Index:
	!	.x Wildcard>Employee New Hires Report
	!	.x Employee New Hires Report>Wildcard
	!
	!--
