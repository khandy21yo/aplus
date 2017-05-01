1	%TITLE "PR Employee Rate Review"
	%SBTTL "PR_RPRT_RATE_REVIEW"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988, 1990 BY
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
	! ID:PR023
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Rate Review Report\* option
	!	option prints a report which lists all records
	!	in the Employee Masterfile which pertain to rates.
	!	.b
	!	The report is printed in re-evaluation date order and includes
	!	column headings for the following:
	!	.table 3,25
	!	.te
	!	Re-evaluation Date
	!	.te
	!	Employee Number
	!	.te
	!	Operation
	!	.te
	!	Effective Date
	!	.te
	!	Type (i.e. Hourly, Unit or Salary)
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
	!	.end table
	!
	! Index:
	!	.X Rate Review>Report
	!	.x Report>Rate Review
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_RATE_REVIEW
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_RATE_REVIEW, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_RATE_REVIEW.OBJ;*
	!
	! Modification history:
	!
	!	03/15/88 - Robert Peterson
	!		Changed the sort order and added employee name,
	!		page break, and print rate yes or no.
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	02/05/91 - Kevin Handy
	!		Modified to print out only the last rate for
	!		a given operation, and not any rate that needs
	!		to be re-evaluated.
	!
	!	03/06/91 - Kevin Handy
	!		Fixed bug where junk records were being displayed
	!		because the _LAST::EVAL_DATE was not being blanked.
	!
	!	05/22/91 - Kevin Handy
	!		Modified to handle TERMDAY more consistantly.
	!
	!	02/07/92 - Kevin Handy
	!		Fixed layout (spreadsheet) format.
	!
	!	04/20/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/29/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	11/29/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.HB"
	MAP	(PR_EMP_RATE)	PR_EMP_RATE_CDD	PR_EMP_RATE
	MAP	(PR_EMP_RATE_LAST)	PR_EMP_RATE_CDD	PR_EMP_RATE_LAST

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

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

	FROM_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(0%), 132%))

	!++
	! Abstract:FLD01
	!	^*(01) From Date\*
	!	.p
	!	The ^*From Date\* field causes the printing
	!	to begin with a particular date.
	!	.p
	!	The field requires an entry. The format for entry is MMDDYYYY
	!	or MMDDYY.
	!
	! Index:
	!	.x From Date>Rate Review Report
	!	.x Rate Review Report>From Date
	!
	!--

	TO_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))

	!++
	! Abstract:FLD02
	!	^*(02) To Date\*
	!	.p
	!	The ^*To Date\* field causes the printing
	!	to end with a particular date.
	!	.p
	!	An entry is required in this field. The format for entry is
	!	MMDDYYYY or MMDDYY.
	!
	! Index:
	!	.x To Date>Rate Review Report
	!	.x Rate Review Report>To Date
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort by (NU, NA, LO)\*
	!	.p
	!	The ^*Sort by\* field enters a code which will
	!	cause the report to be sorted in the indicated manner.
	!	.p
	!	Valid codes are:
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
	!	This field requires an entry and only the above codes are valid.
	!
	! Index:
	!	.x Sort By>Rate Review Report
	!	.x Rate Review Report>Sort By
	!
	!--


	PRINTRATE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Print Rate (Y/N)\*
	!	.p
	!	The ^*Print Rate (Y/N)\* setting will indicate whether the
	!	rate is to print.
	!	.p
	!	Valid settings are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Y = Yes
	!	.le
	!	N = No
	!	.els
	!	.lm -15
	!
	! Index:
	!	.x Print Rate>Rate Review Report
	!	.x Rate Review Report>Print Rate
	!
	! Datatype:TEXT
	! Size:1
	! Valid Input: Y,N,y,n
	!--

	PAGEBREAK$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Page Break\*
	!	.p
	!	The ^*Page Break\* field allows for a page break after each date included.
	!	A ^*Y\* input would allow for the break while a ^*N\* input
	!	would not.
	!
	! Index:
	!	.x Page Break>Rate Review Report
	!	.x Rate Review Report>Page Break
	!
	! Datatype:TEXT
	! Size:1
	! Valid Input: Y,N,y,n
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
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Employee Rate Evaulation Report"
	TITLE$(2%) = ""

	!
	! Column headings
	!
	TITLE$(3%) = "EmpNum     EmpName                        " + &
		"Loc   Dept    WC    EvalDate    Oper     EffDate    T Co   " + &
		"HrRate UnitRate   OT% EffRating"
	TITLE$(4%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$EmpNum:011,$EmpName:042,$Location:047,$Dept:056," + &
		"$WC:064,DEvaluationDate:076,$Oper:085,DEffectiveDate:096," + &
		"$RateType:098,$RateCode:101,VHrRate:110,VUnitRate:119," + &
		"VFactor:124,VStdEffRating:134"

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

	PASS_1% = 0%

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

	GOTO GetNextRec IF PR_EMP_MASTER::TERMDAY > "00000000"

17030	WHEN ERROR IN
		FIND #PR_EMP_RATE.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

	PR_EMP_RATE_LAST::EMPNUM = PR_EMP_MASTER::EMPNUM
	PR_EMP_RATE_LAST::OPER = ""
	PR_EMP_RATE_LAST::EFFDAT = ""
	PR_EMP_RATE_LAST::EVAL_DATE = ""

17040	WHEN ERROR IN
		GET #PR_EMP_RATE.CH%, REGARDLESS
	USE
		CONTINUE 17045 IF ERR = 11%
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

	IF PR_EMP_RATE::EMPNUM <> PR_EMP_MASTER::EMPNUM
	THEN
		GOSUB 17050	! Show last record for this employee
		GOTO 17350	! Leave
	END IF

	IF (PR_EMP_RATE::OPER <> PR_EMP_RATE_LAST::OPER)
	THEN
		GOSUB 17050	! Show last record
	END IF

	PR_EMP_RATE_LAST = PR_EMP_RATE

	GOTO 17040

17045	GOSUB 17050
	GOTO 17350

17050	!
	! Print out one line
	!
	GOTO 17090 IF (PR_EMP_RATE_LAST::EVAL_DATE < FROM_DATE$) OR &
		(PR_EMP_RATE_LAST::EVAL_DATE > TO_DATE$) OR &
		(PR_EMP_RATE_LAST::EVAL_DATE  = "")

	IF SORTBY$ = "LO" AND PAGEBREAK$ = "Y"
	THEN
		IF WORK_TEST$ <> PR_EMP_MASTER::LOCATION + PR_EMP_MASTER::DEPT &
			AND PASS_1%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)
		END IF
	END IF

	WORK_TEST$ = PR_EMP_MASTER::LOCATION + PR_EMP_MASTER::DEPT

	TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
		LEFT(PR_EMP_MASTER::EMPNAME + SPACE$(30%), 30%) + " " + &
		PR_EMP_MASTER::LOCATION + "  " + &
		PR_EMP_MASTER::DEPT + "  " + &
		PR_EMP_MASTER::WC + "  " + &
		PRNT_DATE(PR_EMP_RATE_LAST::EVAL_DATE, 8%) + "  " + &
		PR_EMP_RATE_LAST::OPER + " " + &
		PRNT_DATE(PR_EMP_RATE_LAST::EFFDAT, 8%) + " " + &
		PR_EMP_RATE_LAST::RATE_TYPE + " " + &
		PR_EMP_RATE_LAST::RATE_CDE + " "

	IF PRINTRATE$ <> "N"
	THEN
		TEXT$ = TEXT$ + &
			FORMAT$(PR_EMP_RATE_LAST::HOUR_RATE, "####.###") + " " + &
			FORMAT$(PR_EMP_RATE_LAST::PIECE_RATE, "####.###") + "  " + &
			FORMAT$(PR_EMP_RATE_LAST::FACTOR, "###%") + "  " + &
			FORMAT$(PR_EMP_RATE_LAST::STDEFF, "####.##%")
	END IF

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	PASS_1% = -1%

17090	RETURN

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
