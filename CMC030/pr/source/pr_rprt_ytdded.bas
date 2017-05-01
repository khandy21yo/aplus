1	%TITLE "Quarterly Retirement Report"
	%SBTTL "PR_RPRT_YTDDED"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:PR041
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Quarterly Retirement Report\* option
	!	allows employees YTD wages and Quarterly Retirement costs
	!	to be calculated and printed.
	!	.b
	!	The Quarterly Retirement Report will provide the following fields:
	!	.table 3,25
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name
	!	.te
	!	Social Security Number
	!	.te
	!	Wages
	!	.te
	!	Retirement Deduction
	!	.te
	!	Employers Portions of Retirement Deduction
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Retirement Report
	!	.x Report>Retirement
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_YTDDED/LINE
	!	$ LINK/EXECUTABLE=PR_EXE:PR_RPRT_YTDDED PR_RPRT_YTDDED, -
	!			FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_YTDDED.OBJ;*
	!
	! Author:
	!
	!	04/18/91 - Craig Tanner
	!
	! Modification history:
	!
	!	04/22/91 - Kevin Handy
	!		Modified to leave off employees that have zero deduction
	!		amount, even if they have wages.
	!
	!	04/22/91 - Kevin Handy
	!		Added grand totals.
	!
	!	04/30/91 - Kevin Handy
	!		Changed to sort from highest salary to lowest,
	!		instead of highest deduction to lowest.
	!
	!	05/01/91 - Kevin Handy
	!		Modified to print anyone who earned anything,
	!		and seperated terminated from non-terminated
	!		employees.
	!
	!	05/22/91 - Kevin Handy
	!		Modified to handle TERMDAY more consistantly.
	!
	!	07/14/91 - Kevin Handy
	!		Removed error trapping for 320, which doesn't exist.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to only look at "P" and "D" type in
	!		PR_REG_ERNDED (Ignore "A" types).
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/09/96 - Kevin Handy
	!		modified source closer to 80 columns.
	!
	!	01/09/96 - Kevin Handy
	!		Added SOTY_BY$ option. (KBJ)
	!
	!	01/10/96 - Kevin Handy
	!		Fix bug in titles created by 01/09/96 change.
	!
	!	04/30/1997 - Kevin Handy
	!		Modifications while looking for a penny problem.
	!		Use a struct for PR_TEMP.
	!		Don't store as double, except where necessary.
	!
	!	05/29/98 - Kevin Handy
	!		Handle new 'F' deduction code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/17/2000 - Kevin Handy
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
	MAP (PR_REG_ERNDED) PR_REG_ERNDED_CDD PR_REG_ERNDED

	RECORD PR_TEMP_CDD
		STRING EMPNUM = 10%
		STRING EMPNAME = 30%
		STRING SSN = 12%
		STRING TERMDAY = 8%
		DECIMAL(12, 2) DYTDWAGE
		REAL YTDWAGE
		REAL DEDUCT
		REAL EPR_DEDUCT
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM.EMP$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Employee _#\*
	!	.b
	!	.lm +5
	!	The ^*From Employee _#\* field causes the report
	!	to begin printing with a particular Employee _#.
	!	.b
	!	A blank field will cause the report to start with the first
	!	Employee _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x Employee>Retirement Report
	!	.x Retirement Report>Employee
	!
	!--

	TO.EMP$	= EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Employee _#\*
	!	.b
	!	.lm +5
	!	The ^*To Employee _#\* field causes the printing
	!	to end with a particular Employee _#.
	!	.b
	!	A blank field will cause the report to end with the last Employee _#
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x Employee>Retirement Report
	!	.x Retirement Report>Employee
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	for wildcard techniques.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Retirement Report
	!	.x Retirement Report>Wildcard
	!
	!--

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	.ts 55
	!	^*(04) Year	YYYY\*
	!	.b
	!	.lm +5
	!	The ^*Year\* field enters the year for which
	!	this report is to be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Year>Retirement Report
	!	.x Retirement Report>Year
	!
	!--

	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(4%), -1%))
	IF QTR% = 0%
	THEN
		FROM_QTR% = 0%
		TO_QTR% = 3%
		QTR_TITLE$ = "For the Year of " + YYYY$
	ELSE
		FROM_QTR% = QTR% - 1%
		TO_QTR% = QTR% - 1%
		QTR_TITLE$ = "For the " + NUM1$(QTR%) + " Quarter of " + YYYY$
	END IF

	!++
	! Abstract:FLD05
	!	.ts 55
	!	^*(05) Quarter	0,1,2,3,4\*
	!	.b
	!	.lm +5
	!	The ^*Quarter\* field determines the quarter
	!	for which the report is to print.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Quarter>Retirement Report
	!	.x Retirement Report>Quarter
	!
	!--

	DED_CODE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	.ts 55
	!	^*(06) Deduction Code	2 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Deduction Code\* field enters the
	!	code assigned to the employees' retirement deduction.
	!	.lm -5
	!
	! Index:
	!	.x Code>Retirement Report
	!	.x Retirement Report>Code
	!
	!--

	EPR_RATE = VAL(EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)) / 100.0

	!++
	! Abstract:FLD07
	!	.ts 55
	!	^*(07) Employers Percent	XX.XX\*
	!	.b
	!	.lm +5
	!	The ^*Employers Percent\* field enters the
	!	percentage of the employee retirement contribution that the
	!	employer will match.
	!	.b
	!	This field requires an entry. The format for entry is XX.XX
	!	(for example 50.00 for 50%).
	!	.lm -5
	!
	! Index:
	!	.x Percent>Retirement Report
	!	.x Retirement Report>Percent
	!
	!--

	EPR_LIMIT = VAL(EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)) / 100.0

	!++
	! Abstract:FLD08
	!	.ts 55
	!	^*(08) Employers Limit	XXX.XX\*
	!	.b
	!	.lm +5
	!	The ^*Employers Limit\* specifies the maximum amount that the
	!	employer will match, based on the employees earnings.
	!	.b
	!	An entry is required in this field. The format for entry is XXX.XX
	!	(for example 3.00 for 3%).
	!	.lm -5
	!
	! Index:
	!	.x Limit>Retirement Report
	!	.x Retirement Report>Limit
	!
	!--

	TERMINATED$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(8%), -1%), 1%)

	!++
	! Abstract:FLD09
	!	.ts 55
	!	^*(09) Terminated	Y or N\*
	!	.b
	!	.lm +5
	!	The ^*Terminated\* field specifies if the report should
	!	show terminated, or current employees. The report will
	!	show one or the other, but not both.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Terminated>Retirement Report
	!	.x Retirement Report>Terminated
	!
	!--

	SORT_BY$ = LEFT(UTL_REPORTX::OPTDEF(9%), 2%)

	!++
	! Abstract:FLD10
	!	.ts 55
	!	^*(10) Sort By	WA,NU,NA,SN\*
	!	.b
	!	.lm +5
	!	Determines the order in which employees will be sorted on
	!	the report.
	!	.b
	!	.list 0,"*"
	!	.le
	!	*W*A Wage
	!	.le
	!	*N*U Employee Number
	!	.le
	!	*N*A Employee Name
	!	.le
	!	*S*N Social Security Number
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Terminated>Retirement Report
	!	.x Retirement Report>Terminated
	!
	!--

300	!
	! Employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Earnings and Deduction register
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"

330	!
	! Open Temporary file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

	SELECT SORT_BY$

	CASE "NU"
		OPEN "PR_TEMP.TMP" FOR OUTPUT AS FILE #PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			PRIMARY KEY PR_TEMP::EMPNUM DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE

	CASE "NA"
		OPEN "PR_TEMP.TMP" FOR OUTPUT AS FILE #PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			PRIMARY KEY PR_TEMP::EMPNAME DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE

	CASE "SN"
		OPEN "PR_TEMP.TMP" FOR OUTPUT AS FILE #PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			PRIMARY KEY PR_TEMP::SSN DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE

	CASE ELSE
		OPEN "PR_TEMP.TMP" FOR OUTPUT AS FILE #PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			PRIMARY KEY PR_TEMP::DYTDWAGE DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE
	END SELECT

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "YEAR TO DATE DEDUCTION"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = QTR_TITLE$
	IF TERMINATED$ = "Y"
	THEN
		TITLE$(4%) = "Terminated Employees Only"
	ELSE
		IF TERMINATED$ = "B"
		THEN
			TITLE$(4%) = "Both Current and Terminated Employees"
		ELSE
			TITLE$(4%) = "Current Employees Only"
		END IF
	END IF

	SELECT SORT_BY$
	CASE "NU"
		TITLE$(5%) = "SORTED BY EMPLOYEE NUMBER"
	CASE "NA"
		TITLE$(5%) = "SORTED BY EMPLOYEE NAME"
	CASE "SN"
		TITLE$(5%) = "SORTED BY SOCIAL SECURITY NUMBER"
	CASE ELSE
		TITLE$(5%) = "SORTED BY YTD WAGE"
	END SELECT

	TITLE$(6%) = ""

	!
	! Column headings
	!
	TITLE$(7%) = "Emp #      Name                           SSN" + &
		"               YTD Wages      Deduction      Employers"

	IF TERMINATED$ = "Y" OR TERMINATED$ = "B"
	THEN
		TITLE$(7%) = TITLE$(7%) + "    TermDate"
	END IF

	TITLE$(8%) ="."
	TITLE$(9%) =""

	!
	! Line layouts
	!
	LYT_LINE$ = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM.EMP$ = ""
		THEN
			RESET #PR_EMP_MASTER.CH%, KEY #0%
		ELSE
			FIND #PR_EMP_MASTER.CH%, KEY #0% GE FROM.EMP$, REGARDLESS
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
		CONTINUE 17400 IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_EMP_MASTER::EMPNUM > TO.EMP$) AND (TO.EMP$ <> "")
	NOTTERM% = (PR_EMP_MASTER::TERMDAY <= "00000000")

	SELECT TERMINATED$
	CASE "Y"
		GOTO 17020 IF NOTTERM% <> 0%
	CASE "N"
		GOTO 17020 IF NOTTERM% = 0%
	END SELECT

	GOTO GetNextRec IF COMP_STRING(EDIT$(PR_EMP_MASTER::EMPNUM, -1%), &
		WLDCRD$) = 0% &
		AND (WLDCRD$ <> "")

	YTD_TOTAL = 0.0
	DED_TOTAL = 0.0

17060	WHEN ERROR IN
		FIND #PR_REG_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

17070	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 11%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	IF PR_REG_ERNDED::EMPNUM = PR_EMP_MASTER::EMPNUM
	THEN
		GOTO 17070 IF PR_REG_ERNDED::ETYPE <> "P" AND &
			PR_REG_ERNDED::ETYPE <> "D" AND &
			PR_REG_ERNDED::ETYPE <> "F"

		!
		! Calculate YTD
		!
		YTD_TOTAL = FUNC_ROUND(YTD_TOTAL + &
			PR_REG_ERNDED::QTR_DOLL(LOOP%), 2%) &
			FOR LOOP% = FROM_QTR% TO TO_QTR% &
			IF PR_REG_ERNDED::ETYPE = "P"

		!
		! Calculate Deductions
		!
		DED_TOTAL = FUNC_ROUND(DED_TOTAL + &
			PR_REG_ERNDED::QTR_DOLL(LOOP%), 2%) &
			FOR LOOP% = FROM_QTR% TO TO_QTR% &
			IF (PR_REG_ERNDED::ETYPE = "D" OR &
				PR_REG_ERNDED::ETYPE = "F") AND &
			(PR_REG_ERNDED::CODE = DED_CODE$)

		GOTO 17070
	END IF

	%PAGE

17100	!*******************************************************************
	! Put employee record into temp file, if not blank.
	!*******************************************************************

	IF (DED_TOTAL <> 0.0) OR (YTD_TOTAL <> 0.0)
	THEN
		PR_TEMP::EMPNUM = PR_EMP_MASTER::EMPNUM
		PR_TEMP::EMPNAME = PR_EMP_MASTER::EMPNAME
		PR_TEMP::SSN = PR_EMP_MASTER::SSN
		PR_TEMP::TERMDAY = PR_EMP_MASTER::TERMDAY
		PR_TEMP::YTDWAGE = -YTD_TOTAL
		PR_TEMP::DYTDWAGE = -YTD_TOTAL
		PR_TEMP::DEDUCT = -DED_TOTAL

		EMPRMAX = FUNC_ROUND(YTD_TOTAL * EPR_LIMIT, 2%)
		EMPRAMT = FUNC_ROUND(DED_TOTAL * EPR_RATE, 2%)
		IF EMPRAMT < EMPRMAX
		THEN
			PR_TEMP::EPR_DEDUCT = -EMPRAMT
		ELSE
			PR_TEMP::EPR_DEDUCT = -EMPRMAX
		END IF

		PUT #PR_TEMP.CH%
	END IF

17350	!
	! Try for next record
	!
	GOTO 17020

	%PAGE

17400	!*******************************************************************
	! Print out the Temporary file
	!*******************************************************************

	TOTAL_WAGES = 0.0
	TOTAL_DEDUCTION = 0.0

	RESET #PR_TEMP.CH%

17410	WHEN ERROR IN
		GET #PR_TEMP.CH%
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	TEXT$ = PR_TEMP::EMPNUM + " " + &
		PR_TEMP::EMPNAME + " " + &
		PRNT_SSN(PR_TEMP::SSN, 0%) + " " + &
		FORMAT$(-PR_TEMP::YTDWAGE, "###,###,###.##") + " " + &
		FORMAT$(-PR_TEMP::DEDUCT, "###,###,###.##") + " " + &
		FORMAT$(-PR_TEMP::EPR_DEDUCT, "###,###,###.##")

	IF TERMINATED$ = "Y" OR TERMINATED$ = "B"
	THEN
		TEXT$ = TEXT$ + "   " + PRNT_DATE(PR_TEMP::TERMDAY, 8%)
	END IF

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TOTAL_WAGES = TOTAL_WAGES + PR_TEMP::YTDWAGE
	TOTAL_DEDUCTION = TOTAL_DEDUCTION + PR_TEMP::DEDUCT
	TOTAL_EPR = TOTAL_EPR + PR_TEMP::EPR_DEDUCT

	GOTO 17410

 ExitTotal:
	!
	! Handle end of report
	!
	TEXT$ = "           " + " " + &
		"GRAND TOTAL                   " + " " + &
		"           " + " " + &
		FORMAT$(-TOTAL_WAGES, "###,###,###.##") + " " + &
		FORMAT$(-TOTAL_DEDUCTION, "###,###,###.##") + " " + &
		FORMAT$(-TOTAL_EPR, "###,###,###.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -2%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

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
