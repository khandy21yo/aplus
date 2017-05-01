1	%TITLE "Employee Rate Changes Report"
	%SBTTL "PR_RPRT_RATECHANGE"
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
	! ID:PR083
	!
	! Abstract:HELP
	!	.p
	!	The ^*Rate Changes report\* lists all employees who's
	!	pay rates have changed during a selected time period.
	!	The information provided includes the
	!	following:
	!	.b
	!	.lm +12
	!	.ls 0,"o"
	!	.le
	!	Location
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Operation
	!	.le
	!	Effective Date of Previous Rate
	!	.le
	!	Previous Rate
	!	.le
	!	Effective Date of New Rate
	!	.le
	!	New Rate
	!	.le
	!	Difference Between Previous and New Rates
	!	.le
	!	Percent change
	!	.end list
	!
	! Index:
	!	.x Rate Changes>Report
	!	.x Report>Rate Changes
	!	.x Employee Rate Changes>Report
	!	.x Report>Employee Rate Changes
	!
	! Option:
	!
	! Author:
	!
	!	02/16/89 - Frank F. Starman
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_RATECHANGE
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_RATECHANGE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_RATECHANGE.OBJ;*
	!
	! Modification history:
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	04/16/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" and SPACE$(...)
	!		in several places.
	!
	!	02/07/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/11/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

	!++
	!
	! Abstract:FLD01
	!	^*(01) Sort (NU,NA,SN,LO,SO)\*
	!	.p
	!	The ^*Sort\* field enters a code which causes the
	!	report to be sorted in the indicated manner.
	!	.p
	!	The valid choices are:
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
	!	.x Sort>Employee Rate Changes Report
	!	.x Employee Rate Changes Report>Sort
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
	!	.x From Item>Employee Rate Changes Report
	!	.x Employee Rate Changes Report>From Item
	!
	!--
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	!
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To Item\* setting causes the printing
	!	to end with a particular item.
	!	.p
	!	A blank field causes the report to end with the last item in the file.
	!
	! Index:
	!	.x To Item>Employee Rate Changes Report
	!	.x Employee Rate Changes Report>To Item
	!
	!--
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	!
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* setting prints a report including selected
	!	employees only using the wildcarding techniques.
	!	.p
	!	Valid wildcard characters are an asterisk (_*) or a question mark (?). An
	!	asterisk (_*) indicates all employees will be printed. A question mark (?)
	!	in a field position indicates an employee with any character in that equivalent
	!	position will be printed.
	!
	! Index:
	!	.x Wildcard>Employee Rate Changes Report
	!	.x Employee Rate Changes Report>Wildcard
	!
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
	!	A blank field will cause the report to begin printing with the first date
	!	in the file.
	!
	! Index:
	!	.x From Date>Employee Rate Changes Report
	!	.x Employee Rate Changes Report>From Date
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
	!	.x To Date>Employee Rate Changes Report
	!	.x Employee Rate Changes Report>To Date
	!
	!--
	TODATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(6%), 132%))


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
	TITLE$(1%) = "Employee Rate Changes"
	TITLE$(2%) = "From " + PRNT_DATE(FROMDATE$, 6%) + " To " + &
		PRNT_DATE(TODATE$, 6%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "Loc  EmpNum     EmployeeName                   " + &
		"Operat   EffDate  PrevRate EffDate    NewRate   Diff    Perc"
	TITLE$(5%) = "."

	!
	! Line layouts
	!
	LYT_LINE = "$Location:004,$EmpNum:015,$EmpName:046,$Oper:055," + &
		"DPreviousDate:064,VPreviousRate:073,DNewDate:082," + &
		"VNewRate:092,VRateDiff:099,VDiffPercent:106"

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

17200	RATEPREV, RATENEW = 0.0
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
		IF PR_EMP_RATE::EFFDAT >= FROMDATE$
		THEN
			RATEPREV = RATENEW
			DATEPREV$ = DATENEW$
			RATENEW = PR_EMP_RATE::HOUR_RATE
			DATENEW$ = PR_EMP_RATE::EFFDAT
		ELSE
			RATENEW = PR_EMP_RATE::HOUR_RATE
			DATENEW$ = PR_EMP_RATE::EFFDAT
		END IF
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
	IF RATEPREV <> 0.0 AND RATEPREV <> RATENEW
	THEN
		TEXT$ = PR_EMP_MASTER::LOCATION + " " + &
			PR_EMP_MASTER::EMPNUM + " " + &
			PR_EMP_MASTER::EMPNAME + " " + &
			OPERATION$ + " " + &
			PRNT_DATE(DATEPREV$, 6%) + " " + &
			FORMAT$(RATEPREV, " ###.###") + " " + &
			PRNT_DATE(DATENEW$, 6%) + " " + &
			FORMAT$(RATENEW, "  ###.###") + " " + &
			FORMAT$(RATENEW - RATEPREV, "##.###") + " " + &
			FORMAT$((RATENEW - RATEPREV) / RATEPREV * 100.0, &
				"###.##%")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		PR_EMP_MASTER::EMPNAME = ""
	END IF
	RATEPREV, RATENEW = 0.0
	RETURN

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
