1	%TITLE "Payroll Quarter Pay Hour Report"
	%SBTTL "PR_RPRT_QTR_PAYHOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:PR045
	!
	! Abstract:HELP
	!	.p
	!	This program prints a payroll Year to Date report which includes the following
	!	fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Name
	!	.le
	!	Social Security Number
	!	.le
	!	Start Date
	!	.le
	!	Year to Date Wages
	!	.le
	!	Year to Date Hours
	!	.le
	!	Quarter to Date Wages
	!	.le
	!	Quarter to Date Hours
	!	.els
	!
	! Index:
	!	.x Dollars and Hours Report
	!	.x Report>Dollars and Hours
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_QTR_PAYHOUR/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_QTR_PAYHOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_QTR_PAYHOUR.OBJ;*
	!
	! Author:
	!
	!	03/23/87 - Robert Peterson
	!
	! Modification history:
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	04/17/92 - Kevin Handy
	!		Added wildcard exclude so NWEST could leave out
	!		auto allowences.
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Removed unsolicited_input stuff.
	!
	!	09/11/96 - Kevin Handy
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
	!	12/12/2000 - Kevin Handy
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
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

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
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:FLD01
	!	^*(1) Year\*
	!	.p
	!	The ^*Year\* field enters the year for which this report is
	!	to print.
	!	.p
	!	The format for entry is YYYY and the field requires entry.
	!
	! Index:
	!	.x Year>Dollars and Hours Report
	!	.x Dollars and Hours Report>Year
	!
	!--

	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))
	!++
	! Abstract:FLD02
	!	^*(02) Quarter\*
	!	.p
	!	The ^*Quarter\* field enters the calendar quarter for which
	!	this report will print.
	!	.p
	!	This field requires an entry and will accommodate a one (1) digit number.
	!
	! Index:
	!	.x Quarter>Hours and Dollars Report
	!	.x Hours and Dollars Report>Quarter
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* field cause the printing
	!	to begin with a particular item. What is entered must correspond
	!	with field (05).
	!	.p
	!	A blank field will cause the report to start with the first item in the file.
	!
	! Index:
	!	.x From Item>Dollars and Hours Report
	!	.x Dollars and Hours Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.p
	!	The ^*To Item\* field causes the printing
	!	to end with a particular item. What is in this field must
	!	correspond with what is entered in field (05).
	!	.p
	!	A blank field will cause the report to end with the last item in the file.
	!
	! Index:
	!	.x To Item>Dollars and Hours Report
	!	.x Dollars and Hours Report>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	! Abstract:FLD05
	!	^*(05) Sort by (NU,NA,LO,SO)\*
	!	.p
	!	The ^*Sort by\* field enters a code which causes
	!	the report to be sorted in the indicated manner.
	!	.p
	!	The valid codes are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	NA = Name
	!	.le
	!	SO = Sort
	!	.le
	!	LO = Location
	!	.els
	!	.lm -10
	!	.p
	!	An entry is required in this field and only the above codes are valid.
	!
	! Index:
	!	.x Sort by>Dollars and Hours Report
	!	.x Dollars and Hours Report>Sort by
	!
	!--

	WLDCRD_LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)
	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Location\*
	!	.p
	!	The ^*Wildcard Location\* field enters a physical location
	!	designation for an employee of up to four (4) characters. The field may
	!	be blank to include all locations or the wildcarding techniques may be used to
	!	include selected locations.
	!
	! Index:
	!	.x Wildcard Location>Dollars and Hours Report
	!	.x Dollars and Hours Report>Wildcard Location
	!
	!--

	WILDCARD_EXCLUDE$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)
	!++
	! Abstract:FLD07
	!	^*(07) Wildcard Exclude\*
	!	.p
	!	Used to specify ^*Pay Codes\* that you want to
	!	exclude (leave out) from the report.
	!
	! Index:
	!	.x Wildcard Exclude>Dollars and Hours Report
	!	.x Dollars and Hours Report>Wildcard Exclude
	!
	! Datatype:TEXT
	! Size:15
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
	TITLE$(1%) = "Dollars and Hours Report for the - " + &
		NUM1$(QTR%) + MID("stndrdth", QTR% * 2% - 1%, 2%) + " Quarter"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = SPACE$(66%) + &
		"---------Year To Date---------      -------Quarter to Date--------"
	TITLE$(5%) = "Emp #      Name                       SSN         St Date         " + &
		"         Wages           Hours               Wages           Hours"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EmpNum:010,$EmpName:037,$SSN:049,$StartDate:058," + &
		"VYTDWages:080,VYTDHours:096,VQTDWages:117,VQTDHours:132"

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

	!
	! Check to see if wildcard then check to see if match
	!
	IF WLDCRD_LOCATION$ <> ""
	THEN
		GOTO 17020 IF COMP_STRING(EDIT$(PR_EMP_MASTER::LOCATION, -1%), &
			WLDCRD_LOCATION$) = 0%
	END IF

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

	IF (WILDCARD_EXCLUDE$ <> "")
	THEN
		GOTO 17110 IF COMP_STRING(EDIT$(PR_REG_ERNDED::CODE, -1%), &
			WILDCARD_EXCLUDE$) <> 0%
	END IF

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
		LEFT(PR_EMP_MASTER::EMPNAME, 26%) + " " + &
		LEFT(PRNT_SSN(PR_EMP_MASTER::SSN, 11%), 11%) + " " + &
		PRNT_DATE(PR_EMP_MASTER::HIREDAY, 10%) + "        " + &
		FORMAT$(EMP(1%), "###,###,###.##  ") + &
		FORMAT$(EMP(2%), "###,###,###.##       ") + &
		FORMAT$(EMP(4%), "###,###,###.## ") + &
		FORMAT$(EMP(5%), "###,###,###.## ")

	TOTAL(LOOP%) = TOTAL(LOOP%) + EMP(LOOP%) FOR LOOP% = 1% TO 6%

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Go to next employee
	!
	GOTO 17020


 ExitTotal:
	!
	! Handle end of report
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(59%) + "Total  " + &
			FORMAT$(TOTAL(1%), "###,###,###.##  ") + &
			FORMAT$(TOTAL(2%), "###,###,###.##       ") + &
			FORMAT$(TOTAL(4%), "###,###,###.## ") + &
			FORMAT$(TOTAL(5%), "###,###,###.## ")

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
