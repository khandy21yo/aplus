1	%TITLE "Payroll Year To Date Report"
	%SBTTL "PR_RPRT_YTD"
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
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:PR031
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Year to Date Payroll\* reports all Payroll for the year up to the
	!	present date. The following fields are included in this report:
	!	.table 3,25
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name
	!	.te
	!	Year to Date Dollars
	!	.te
	!	Year to Date Hours
	!	.te
	!	Year to Date Units
	!	.te
	!	Quarter to Date Dollars
	!	.te
	!	Quarter to Date Hours
	!	.te
	!	Quarter to Date Units
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Year to Date Payroll
	!	.x Year to Date Payroll>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_YTD/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_YTD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_YTD.OBJ;*
	!
	! Author:
	!
	!	03/23/87 - Robert Peterson
	!
	! Modification history:
	!
	!	04/03/89 - Kevin Handy
	!		Changed title from "Dump Report" to "YTD Payroll
	!		Report".
	!
	!	06/19/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	01/15/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Removed unsolicited_input stuff.
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
	!	11/30/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP	(PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

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
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Year	YYYY\*
	!	.b
	!	.lm +5
	!	The ^*Year\* field entesr the year for which this report
	!	is to print.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Year>Year to Date Payroll
	!	.x Year to Date Payroll>Year
	!
	!--

	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))

	!++
	! Abstract:FLD02
	!	.ts 55
	!	^*(02) Quarter	1,2,3,4\*
	!	.b
	!	.lm +5
	!	The ^*Quarter\* field enters the payroll quarter for which
	!	this report is to be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Quarter>Year to Date Payroll
	!	.x Year to Date Payroll>Quarter
	!
	!--


	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the report
	!	to begin printing with this particular item. The
	!	value must be in agreement with field (05).
	!	.b
	!	A blank field will cause the report to begin with the
	!	first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Year to Date Payroll
	!	.x Year to Date Payroll>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the report
	!	to end with this particular item. The value
	!	must be in agreement with field (05).
	!	.b
	!	A blank will cause the report to end with the last item in
	!	the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Year to Date Payroll
	!	.x Year to Date Payroll>To Item
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.ts 55
	!	^*(05) Sort by	NU,NA,LO,SO\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field enters a code which
	!	causes the report to be sorted in the indicated manner.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*NU\*	Number
	!	.te
	!	^*NA\*	Name
	!	.te
	!	^*LO\*	Location
	!	.te
	!	^*SO\*	Alphabetical (last name first)
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by>Year to Date Payroll
	!	.x Year to Date Payroll>Sort by
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
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

320	!
	! Open pay deduction file for year
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "YTD Payroll Report - " + NUM1$(QTR%) + &
		MID("stndrdth", QTR% * 2% - 1%, 2%) + " Quarter"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = SPACE$(41%) + &
		"----------------Year To Date---------------       " + &
		" -------------Quarter to Date-------------"
	TITLE$(5%) = "EmpNum     EmpName                       " + &
		"      Dollars          Hours          Units       " + &
		"      Dollars         Hours          Units"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EmpNum:010,$EmpName:041,VYTDDollars:054," + &
		"VYTDHours:069,VYTDUnits:084,VQTDDollars:104," + &
		"VQTDHours:118,VQTDUnits:132"

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
	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	EMP(LOOP%) = 0.0 FOR LOOP% = 1% TO 6%

17100	!
	! Get pay deduction information
	!
	WHEN ERROR IN
		FIND #PR_REG_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

17110	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 11%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 17200 IF PR_EMP_MASTER::EMPNUM <> PR_REG_ERNDED::EMPNUM

	GOTO 17110 IF PR_REG_ERNDED::ETYPE <> "P"

	EMP(1%) = EMP(1%) + PR_REG_ERNDED::QTR_DOLL(LOOP%) &
		FOR LOOP% = 0% TO (QTR% - 1%)
	EMP(2%) = EMP(2%) + PR_REG_ERNDED::REG_HRS(LOOP%) + &
		PR_REG_ERNDED::PRE_HRS(LOOP%) &
		FOR LOOP% = 0% TO (QTR% - 1%)
	EMP(3%) = EMP(3%) + PR_REG_ERNDED::UNITS(LOOP%) &
			FOR LOOP% = 0% TO (QTR% - 1%)
	EMP(4%) = EMP(4%) + PR_REG_ERNDED::QTR_DOLL(QTR% - 1%)
	EMP(5%) = EMP(5%) + PR_REG_ERNDED::REG_HRS(QTR% - 1%) + &
		PR_REG_ERNDED::PRE_HRS(QTR% - 1%)
	EMP(6%) = EMP(6%) + PR_REG_ERNDED::UNITS(QTR% - 1%)

	GOTO 17110

17200	!
	! Print employee total
	!
	TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + &
		FORMAT$(EMP(1%), "##,###,###.##  ") + &
		FORMAT$(EMP(2%), "##,###,###.##  ") + &
		FORMAT$(EMP(3%), "##,###,###.##       ") + &
		FORMAT$(EMP(4%), "##,###,###.## ") + &
		FORMAT$(EMP(5%), "##,###,###.## ") + &
		FORMAT$(EMP(6%), "##,###,###.## ")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	TOTAL(LOOP%) = TOTAL(LOOP%) + EMP(LOOP%) FOR LOOP% = 1% TO 6%

17350	!
	! Go to next employee
	!
	GOTO 17020


 ExitTotal:
	!
	! Handle end of report
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(34%) + "Total  " + &
		FORMAT$(TOTAL(1%), "##,###,###.##  ") + &
		FORMAT$(TOTAL(2%), "##,###,###.##  ") + &
		FORMAT$(TOTAL(3%), "##,###,###.##       ") + &
		FORMAT$(TOTAL(4%), "##,###,###.## ") + &
		FORMAT$(TOTAL(5%), "##,###,###.## ") + &
		FORMAT$(TOTAL(6%), "##,###,###.## ")

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
