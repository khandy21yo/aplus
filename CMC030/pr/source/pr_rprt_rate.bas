1	%TITLE "PR Employee Rate Dump"
	%SBTTL "PR_RPRT_RATE"
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
	! ID:PR022
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Employee Rate Report\* option
	!	prints a report which lists all records in
	!	the Employee Masterfile pertaining to rates.
	!	.b
	!	The report includes the following column headings:
	!	.table 3,25
	!	.te
	!	Employee Number
	!	.te
	!	Operation
	!	.te
	!	Effective Date
	!	.te
	!	Type (i.e. Hourly, Units, Salary)
	!	.te
	!	Code
	!	.te
	!	Hourly Rate
	!	.te
	!	Unit Rate
	!	.te
	!	Overtime Factor
	!	.te
	!	Efficiency Rating
	!	.te
	!	Re-evaluation Date
	!	.te
	!	Re-evaluation Flag Notice (*)
	!	.end table
	!	The report is printed in employee number order with the least
	!	current effective dates listed first.
	!	.lm -5
	!
	! Index:
	!	.x Employee Rate>Report
	!	.x Report>Employee Rate
	!	.x Dump>Employee Rate
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_RATE
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_RATE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_RATE.OBJ;*
	!
	! Modification history:
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or to a DIF file.
	!
	!	10/22/90 - Kevin Handy
	!		Modified to double space between employees,
	!		and to page between locations.
	!
	!	11/06/90 - Val Allen
	!		Modified to show flag next to re-evaluation date
	!		when re-evaluation date older than current date.
	!		also fixed so that it would exclude on command
	!		(not always) the terminated employees.
	!
	!	05/22/91 - Kevin Handy
	!		Modified to handle TERMDAY more consistantly.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	09/30/91 - Kevin Handy
	!		Modified so that FROM and TO look accouding
	!		to the SORT.BY selection.
	!
	!	11/10/94 - Kevin Handy
	!		Added from/to operation.
	!
	!	11/10/94 - Kevin Handy
	!		Removed code that initialized PR_EMP_RATE record
	!		just before a GET for that data.
	!
	!	11/10/94 - Kevin Handy
	!		Modified to leave people off the report who don't
	!		have any rates.
	!
	!	11/10/94 - Kevin Handy
	!		Sorted error traps.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/12/2000 - Kevin Handy
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
	MAP	(PR_EMP_RATE)	PR_EMP_RATE_CDD	PR_EMP_RATE

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000


 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*From Item\* setting causes the printing
	!	to begin with a particular item.
	!	The value must be in agreement with field
	!	(03).
	!	.p
	!	A blank setting will cause the report to print all
	!	records in the file.
	!
	! Index:
	!	.x From Item>Employee Rate Record
	!	.x Employee Rate Record>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* setting causes the printing
	!	to end with a particular item. The
	!	value must be in agreement with field (03).
	!	.p
	!	A blank setting will cause the report to print all
	!	records in the file.
	!
	! Index:
	!	.x To Item>Employee Rate Report
	!	.x Employee Rate Report>To Item
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort by (NU, NA, LO)\*
	!	.p
	!	The ^*Sort by\* field indicates
	!	how the report is to be sorted.
	!	.p
	!	Valid settings are:
	!	.b
	!	.lm 15
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
	!	A setting is required in this field. Only the settings shown
	!	above are valid.
	!
	! Index:
	!	.x Sort>Employee Rate Report
	!	.x Employee Rate Report>Sort
	!
	!--


	EXCLUDE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Exclude Terminated Employee\*
	!	.p
	!	The ^*Exclude Terminated Employee\* field excludes all
	!	terminated Employees from the printed ^*Rate Report\*. A ^*Y\*
	!	input allows for the exclusion, while a ^*N\* input would cause the terminated
	!	to be included.
	!
	! Index:
	!	.x Exclude Terminated Employees>Employee Rate Report
	!	.x Employee Rate Report>Exclude Terminated Employees
	!
	!--

	WILDDEP$ = TRM$(UTL_REPORTX::OPTDEF(4%))

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard Department\*
	!	.p
	!	This field specifies which departments should be
	!	included using wildcard specifications.
	!	.p
	!	A blank entry here will include all departments,
	!
	! Index:
	!	.x Exclude Terminated Employees>Employee Rate Report
	!	.x Employee Rate Report>Exclude Terminated Employees
	!
	! Datatype:TEXT
	! Size:1
	! Valid Input: Y,N,y,n
	!--

	FROM_OPER$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) From Operation\*
	!	.p
	!
	! Index:
	!	.x From Operation>Employee Rate Report
	!	.x Employee Rate Report>From Operation
	!
	! Datatype:TEXT
	! Size:10
	!--

	TO_OPER$ = TRM$(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) To Operation\*
	!	.p
	!
	! Index:
	!	.x To Operation>Employee Rate Report
	!	.x Employee Rate Report>To Operation
	!
	! Datatype:TEXT
	! Size:10
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
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.OPN"
	USE
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

310	!
	! Open file
	!
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
	TITLE$(1%) = "Employee Rate Report"
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3) =  "                                          " + &
		"                                                       " + &
		"                         Eval Flag=*"
	TITLE$(4%) = "EmpNum     EmpName                    " + &
		"Loc  Dept   WC   Oper     Eff Date   T Co  HrRate  " + &
		" UtRate OT % E Rating Hire Date  Eval Date"
	TITLE$(5%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EmpNum:010,$EmpName:037,$Location:042,$Dept:049," + &
		"$WC:054,$Operation:063,DEffectiveDate:074,$RateType:076," + &
		"$RateCode:079,VHourRate:087,VPieceRate:097,VFactor:102," + &
		"VStdEffRating:111,DHireDate:122,DEvaluationDate:132"

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

	LAST_LOC$ = "!!!!!!!!"

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

	SKIP_FLAG% = -1%

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

	CASE "SO"
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	END SELECT

	IF WILDDEP$ <> ""
	THEN
		IF COMP_STRING(PR_EMP_MASTER::DEPT, WILDDEP$) = 0%
		THEN
			GOTO 17350
		END IF
	END IF

	GOTO 17350 &
		IF PR_EMP_MASTER::TERMDAY > "00000000" AND EXCLUDE$ = "Y"

	WORK_EMPNUM$ = PR_EMP_MASTER::EMPNUM
	WORK_EMPNAME$ = PR_EMP_MASTER::EMPNAME
	WORK_LOCATION$ = PR_EMP_MASTER::LOCATION
	WORK_DEPT$ = PR_EMP_MASTER::DEPT
	WORK_WC$ = PR_EMP_MASTER::WC

17030	WHEN ERROR IN
		FIND #PR_EMP_RATE.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

17040	WHEN ERROR IN
		GET #PR_EMP_RATE.CH%, REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 11%
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

	GOTO 17350 IF PR_EMP_RATE::EMPNUM <> PR_EMP_MASTER::EMPNUM

	GOTO 17040 IF PR_EMP_RATE::OPER < FROM_OPER$
	GOTO 17040 IF (TO_OPER$ <> "") AND &
		(PR_EMP_RATE::OPER > TO_OPER$)

17050	!
	! Print out one line
	!
	IF SKIP_FLAG%
	THEN
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 4%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		SKIP_FLAG% = 0%
	END IF

	IF (SORTBY$ = "LO") AND (LAST_LOC$ <> PR_EMP_MASTER::LOCATION)
	THEN
		IF LAST_LOC$ <> "!!!!!!!!"
		THEN
			CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 3000%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		LAST_LOC$ = PR_EMP_MASTER::LOCATION
	END IF
	!
	! Get current date and compare to evaluation date and flag if
	! re-evaluation date is older than today's date - ignore if
	! re-evaluation date is blank

	EVALFLAG$ = ""

	GOTO BuildText IF PR_EMP_RATE::EVAL_DATE = ""

	REP_DATE$ = DATE_TODAY

	BASE_DAY% = DATE_DAYCODE(REP_DATE$)

	AGE_DAY% = BASE_DAY% - DATE_DAYCODE(PR_EMP_RATE::EVAL_DATE)

	EVALFLAG$ = "*" IF AGE_DAY% > 0%


 BuildText:
	TEXT$ = LEFT(WORK_EMPNUM$ + SPACE$(10%), 10%) + " " + &
		LEFT(WORK_EMPNAME$ + SPACE$(26%), 26%) + " " + &
		LEFT(WORK_LOCATION$ + SPACE$(4%), 4%) + " " + &
		LEFT(WORK_DEPT$ + SPACE$(6%), 6%) + " " + &
		LEFT(WORK_WC$ + SPACE$(4%), 4%) + " " + &
		PR_EMP_RATE::OPER + " " + &
		PRNT_DATE(PR_EMP_RATE::EFFDAT, 8%) + " " + &
		PR_EMP_RATE::RATE_TYPE + " " + &
		PR_EMP_RATE::RATE_CDE + &
		FORMAT$(PR_EMP_RATE::HOUR_RATE, "####.###") + " " + &
		FORMAT$(PR_EMP_RATE::PIECE_RATE, "####.###") + " " + &
		FORMAT$(PR_EMP_RATE::FACTOR, "###%") + " " + &
		FORMAT$(PR_EMP_RATE::STDEFF, "####.##%") + " " + &
		PRNT_DATE(PR_EMP_MASTER::HIREDAY, 8%)  + " " + &
		PRNT_DATE(PR_EMP_RATE::EVAL_DATE, 8%)  + &
		EVALFLAG$


	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WORK_EMPNUM$ = ""
	WORK_EMPNAME$ = ""
	WORK_LOCATION$ = ""
	WORK_DEPT$ = ""
	WORK_WC$ = ""

	GOTO 17040

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
