1	%TITLE "Employee Rate Report"
	%SBTTL "PR_RPRT_RATE_02"
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
	! ID:PR084
	!
	! Abstract:HELP
	!	.p
	!	The ^*Employee Rate Report\* option
	!	menu prints a report which lists all records in
	!	the Employee Masterfile pertaining to rates.
	!	.p
	!	The report includes the following column headings:
	!	.b
	!	.ls 0,"o"
	!	.lm +15
	!	.le
	!	#Employee Number
	!	.le
	!	#Operation
	!	.le
	!	#Effective Date
	!	.le
	!	#Type (i.e. Hourly, Units, Salary)
	!	.le
	!	#Code
	!	.le
	!	#Hourly Rate
	!	.le
	!	#Unit Rate
	!	.le
	!	#Overtime Factor
	!	.le
	!	#Efficiency Rating
	!	.le
	!	#Re-evaluation Date
	!	.els
	!	.lm -15
	!	.p
	!	The report is printed in employee number order with the least
	!	current effective dates listed first.
	!
	! Index:
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_RATE_02
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_RATE_02, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_RATE_02.OBJ;*
	!
	! Modification history:
	!
	!	07/13/89 - Kevin Handy
	!		Modified so that program doesn't die if the
	!		PR_RATERANGE file doesn't exist.
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or to a DIF file.
	!
	!	05/22/91 - Kevin Handy
	!		Modified to handle TERMDAY more consistantly.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	03/13/92 - Kevin Handy
	!		Removed duplicate error trap (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check).
	!
	!	07/01/98 - Kevin Handy
	!		Fix bug in SORTBY (only handled by employee right)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/27/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_RATERANGE.HB"
	MAP	(PR_RATERANGE)	PR_RATERANGE_CDD	PR_RATERANGE

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
	!	A blank setting causes the report to print all
	!	records in the file.
	!
	! Index:
	!	.x From Item>Employee Rate Report
	!	.x Employee Rate Report>From Item
	!	.x Item>From
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
	!	A blank setting causes the report to print all
	!	records in the file.
	!
	! Index:
	!	.x To Item>Employee Rate Report
	!	.x Employee Rate Report>To Item
	!	.x Item>To
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	!++
	! Abstract:FLD03
	!	^*(03) Sort by (NU, NA, LO, SO)\*
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
	!	.le
	!	SO = Alphabetical
	!	.els
	!	.lm -10
	!	.p
	!	A setting is required in this field. Only the settings shown
	!	above are valid.
	!
	! Index:
	!	.x Sort By>Employe Rate Report
	!	.x Employe Rate Report>Sort By
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

320	!
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_RATERANGE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PR_RATERANGE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Employee Rate Report"
	TITLE$(2%) = ""

	!
	! Column headings
	!
	TITLE$(3%) = "* indicates rate out of range for age."
	TITLE$(4%) = "  EmpNum     EmpName                        " + &
		"Age Loc  Dept   WC   Oper     EffDate    T Co   HrRate " + &
		"UnittRate  OT% EffRating EvalDate"
	TITLE$(5%) = "."
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$WarningFlag:001,$EmpNum:011,$EmpName:032," + &
		"VAge:036,$Location:041,$Dept:048,$WC:053,$Oper:062," + &
		"DEffectiveDate:073,$RateType:075,$RateCode:078," + &
		"VHrRate:087,VUnitRate:096,VFactor:100," + &
		"VStdEffRating:109,DEvaluationDate:120"

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

	GOTO 17350 IF PR_EMP_MASTER::TERMDAY > "00000000"

	WORK_EMPNUM$ = PR_EMP_MASTER::EMPNUM
	WORK_EMPNAME$ = PR_EMP_MASTER::EMPNAME

	WORK_EMPAGE% = FIX((VAL(DATE_TODAY) - &
		VAL(PR_EMP_MASTER::BIRTH)) / 10000%)
	WORK_EMPAGE% = 0% IF WORK_EMPAGE% < 0% OR WORK_EMPAGE% >= 18%
	THE_AGE%     = WORK_EMPAGE%

17022	WORK_LOCATION$ = PR_EMP_MASTER::LOCATION
	WORK_DEPT$ = PR_EMP_MASTER::DEPT
	WORK_WC$ = PR_EMP_MASTER::WC

17030	PR_EMP_RATE::OPER	= ""
	PR_EMP_RATE::EFFDAT	= "00000000"
	PR_EMP_RATE::RATE_TYPE	= ""
	PR_EMP_RATE::RATE_CDE	= ""
	PR_EMP_RATE::HOUR_RATE	= 0.0
	PR_EMP_RATE::PIECE_RATE	= 0.0
	PR_EMP_RATE::FACTOR	= 0
	PR_EMP_RATE::STDEFF	= 0.0
	PR_EMP_RATE::EVAL_DATE	= "00000000"

	WHEN ERROR IN
		FIND #PR_EMP_RATE.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17050 IF ERR = 155% OR ERR = 9%
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

17045	XWARN$ = " "
	WHEN ERROR IN
		GET #PR_RATERANGE.CH%, KEY #0% EQ PR_EMP_MASTER::LOCATION + &
			NUM1$(THE_AGE%), REGARDLESS
	USE
		CONTINUE 17050
	END WHEN

	XWARN$ = "*" IF PR_EMP_RATE::HOUR_RATE < PR_RATERANGE::MIN_RATE &
		OR PR_EMP_RATE::HOUR_RATE > PR_RATERANGE::MAX_RATE

17050	!
	! Print out one line
	!
	TEXT$ = XWARN$ + &
		LEFT(WORK_EMPNUM$ + SPACE$(10%), 10%) + " " + &
		LEFT(WORK_EMPNAME$ + SPACE$(30%), 30%) + " " + &
		FORMAT$(THE_AGE%, "<%>##") + " " + &
		LEFT(WORK_LOCATION$ + SPACE$(4%), 4%) + " " + &
		LEFT(WORK_DEPT$ + SPACE$(6%), 6%) + " " + &
		LEFT(WORK_WC$ + SPACE$(4%), 4%) + " " + &
		PR_EMP_RATE::OPER + " " + &
		PRNT_DATE(PR_EMP_RATE::EFFDAT, 8%) + " " + &
		PR_EMP_RATE::RATE_TYPE + " " + &
		PR_EMP_RATE::RATE_CDE + " " + &
		FORMAT$(PR_EMP_RATE::HOUR_RATE, "####.###") + " " + &
		FORMAT$(PR_EMP_RATE::PIECE_RATE, "####.###") + " " + &
		FORMAT$(PR_EMP_RATE::FACTOR, "###% ") + " " + &
		FORMAT$(PR_EMP_RATE::STDEFF, "####.##%") + " " + &
		PRNT_DATE(PR_EMP_RATE::EVAL_DATE, 8%)

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WORK_EMPNUM$ = ""
	WORK_EMPNAME$ = ""
	WORK_EMPAGE% = 0%
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
	!+-+-+
	!++
	! Abstract:FLD04
	!	^*(04) Exclude Terminated Employees\*
	!	.p
	!	The ^*Exclude Terminated Employees\* field includes
	!	or excludes the terminated employees in the rate report.  A ^*Y\* entry
	!	excludes the terminated employees while a ^*N\* answer includes the terminated
	!	employees.
	!
	! Index:
	!	.x Exclude Terminated Employees
	!
	!--
