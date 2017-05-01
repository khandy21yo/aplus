1	%TITLE "Payroll Labor Recap Report"
	%SBTTL "PR_RPRT_LABORRECAP"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:PR015
	!
	! Abstract:HELP
	!	.p
	!	This program prints out the ^*Payroll Labor Recap Report\* for a specified time
	!	period for specific or all items.
	!	.b
	!	.lm +12
	!	.list 0,"o"
	!	.le
	!	Product Percent
	!	.le
	!	Employee Worked
	!	.le
	!	Sales Amount
	!	.le
	!	Labor Hours
	!	.le
	!	Gross Earnings
	!	.le
	!	PR_PERC
	!	.le
	!	Advance Rate
	!	.le
	!	Regular Hours
	!	.le
	!	Overtime Hours
	!	.le
	!	Class Hours
	!	.le
	!	Vacation Hours
	!	.le
	!	Management Hours
	!	.le
	!	Other Hours
	!	.end list
	!
	! Index:
	!	.x Payroll Labor Recap>Report
	!	.x Report>Payroll Labor Recap
	!	.x Payroll Labor Recap>Report
	!	.x Report>Payroll Labor Recap
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_LABORRECAP/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_LABORRECAP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_LABORRECAP.OBJ;*
	!
	! Author:
	!
	!	02/27/89 - Kevin Handy
	!
	! Modification history:
	!
	!	03/08/89 - Kevin Handy
	!		Fixed bug with printing by departments.
	!
	!	03/27/89 - Kevin Handy
	!		Fixed problem where an employee with two checks
	!		was treated as though he had three.
	!
	!	04/03/89 - Kevin Handy
	!		Made several formatting changes.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/25/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE				UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)		PR_TRN_PAY_CDD		PR_TRN_PAY
	MAP	(PR_HIS_PAY)		PR_TRN_PAY_CDD		PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_SALES.HB"
	MAP	(PR_SALES)		PR_SALES_CDD		PR_SALES

	!
	! Dimension
	!
	RECORD SUMM_RECORD
		INTEGER EMP_WORKED
		REAL SALES_AMOUNT
		REAL LABOR_HOURS
		REAL GROSS_EARNINGS
		REAL VAC_HOURS
		REAL REG_HOURS
		REAL OVT_HOURS
		REAL CLASS_HOURS
		REAL MGT_HOURS
		REAL OTHER_HOURS
	END RECORD

	DECLARE SUMM_RECORD BLANK_TOTAL
	DECLARE SUMM_RECORD GRAND_TOTAL
	DECLARE SUMM_RECORD EMP_TOTAL
	DECLARE SUMM_RECORD DEPT_TOTAL
	DECLARE SUMM_RECORD LOC_TOTAL

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:FLD01
	!	^*(01) Payroll Date\*
	!	.p
	!	The ^*Payroll Date\* field enters the date of the particular
	!	payroll which is to be printed.
	!	.p
	!	This field requires an entry. The format if MMDDYYYYor MMDDYY.
	!
	! Index:
	!	.x Payroll Date>Labor Recap Report
	!	.x Labor Recap Report>Payroll Date
	!
	!--
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)
	YYYY$ = LEFT(BATCH_NO$, 4%)
	BEGIN_DATE$ = DATE_INVDCODE(DATE_DAYCODE(BATCH_NO$) - 6%)

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.p
	!	The ^*From Item\* field causes the printing
	!	to begin with a particular item.
	!	.p
	!	A blank field will cause the report to start with the first item is the
	!	file.
	!
	! Index:
	!	.x From Item>Labor Recap Report
	!	.x Labor Recap Report>From Item
	!
	!--
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To Item\* field causes the printing
	!	to end with a particular item.
	!	.b
	!	A blank field causes the report to end with the last item in the file.
	!
	! Index:
	!	.x To Item>Labor Recap Report
	!	.x Labor Recap Report>To Item
	!
	!--
	BY_DEPT$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	! Abstract:FLD04
	!	^*(04) Department\*
	!	.p
	!	The ^*Department\* field records a department, area,
	!	to which an employee is assigned.
	!
	! Index:
	!	.x Department>Labor Recap Report
	!	.x Labor Recap Report>Department
	!
	!--

	IF (BY_DEPT$ = "") OR &
		(INSTR(1%, BY_DEPT$, "?") <> 0%) OR &
		(INSTR(1%, BY_DEPT$, "%") <> 0%) OR &
		(INSTR(1%, BY_DEPT$, ",") <> 0%) OR &
		(INSTR(1%, BY_DEPT$, "*") <> 0%)
	THEN
		BY_DEPT% = -1%		! BY_DEPT$ may have several depts
	ELSE
		BY_DEPT% = 0%		! BY_DEPT$ is only one dept
	END IF

310	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		CONTINUE 315 IF ERR = 5%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	GOTO 320

315	!
	! Open Pay history if pay journal no found
	!
	USE_HISTORY% = -1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	PR_TRN_PAY.CH% = PR_HIS_PAY.CH%

320	!
	! Open sales info file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_SALES.OPN"
	USE
		FILENAME$ = "PR_SALES"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
360	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Labor Statistics Recap Report"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		MID(BATCH_NO$, 5%, 2%) + "/" + &
		MID(BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(BATCH_NO$, 4%)

	TITLE$(3%) = "."
	TITLE$(3%) = "For Department(s): " + BY_DEPT$ &
		IF BY_DEPT$ <> ""

	TITLE$(4%) = ""

	TITLE$(5%) = "              PROD   EMP     SALES     TOTAL " + &
		"   GROSS    P/R   ADV     REG      OVT   " + &
		"CLASS     VAC     MGT   OTHER"
	TITLE$(6%) = "LOCA DEPT     FACT  PAID       LOC LABOR HRS " + &
		"EARNINGS     %   RATE    HOURS   HOURS   " + &
		"HOURS   HOURS   HOURS   HOURS"
	TITLE$(7%) = "."

	BLANK_TOTAL::EMP_WORKED		= 0%
	BLANK_TOTAL::SALES_AMOUNT	= 0.0
	BLANK_TOTAL::LABOR_HOURS	= 0.0
	BLANK_TOTAL::GROSS_EARNINGS	= 0.0
	BLANK_TOTAL::VAC_HOURS		= 0.0
	BLANK_TOTAL::REG_HOURS		= 0.0
	BLANK_TOTAL::OVT_HOURS		= 0.0
	BLANK_TOTAL::CLASS_HOURS	= 0.0
	BLANK_TOTAL::MGT_HOURS		= 0.0
	BLANK_TOTAL::OTHER_HOURS	= 0.0

	GRAND_TOTAL = BLANK_TOTAL
	EMP_TOTAL = BLANK_TOTAL
	DEPT_TOTAL = BLANK_TOTAL
	LOC_TOTAL = BLANK_TOTAL

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_TRN_PAY.CH%, KEY #2%
		ELSE
			FIND #PR_TRN_PAY.CH%, KEY #2% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_TRN_PAY"
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
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! If history then set history map to journal
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	GOTO 17020 IF PR_TRN_PAY::PTYPE = "A"

	!
	! Skip if not in a valid department
	!
	IF BY_DEPT$ <> ""
	THEN
		GOTO 17020 IF COMP_STRING(PR_TRN_PAY::DEPT, BY_DEPT$) = 0%
	END IF

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_TRN_PAY::LOCATION > TO_ITEM$) AND &
		TO_ITEM$ <> ""

17110	SELECT PR_TRN_PAY::CODE

	CASE "VA"
		EMP_TOTAL::VAC_HOURS = PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR

	CASE "RG", "TX"
		EMP_TOTAL::REG_HOURS = PR_TRN_PAY::REG_HR
		EMP_TOTAL::OVT_HOURS = PR_TRN_PAY::OVT_HR

	CASE "CL"
		EMP_TOTAL::CLASS_HOURS = PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR

	CASE "MG"
		EMP_TOTAL::MGT_HOURS = PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR

	CASE ELSE
		EMP_TOTAL::OTHER_HOURS = PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR

	END SELECT

	EMP_TOTAL::LABOR_HOURS = PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR
	EMP_TOTAL::GROSS_EARNINGS = PR_TRN_PAY::GROSS

	!
	! Print department total if necessary
	!
	GOSUB PrintDeptTotal &
		IF ((THIS_LOC$ <> PR_TRN_PAY::LOCATION) OR &
		(THIS_DEPT$ <> PR_TRN_PAY::DEPT)) AND THIS_LOC$<>""

	!
	! Print location total if necessary
	!
	GOSUB PrintLocalTotal &
		IF (THIS_LOC$ <> PR_TRN_PAY::LOCATION) AND THIS_LOC$<>""

	!
	! Summarize everything for the location totals
	!

	CALL SUMMARIZE_TRN( 0%, &
		EMP_TOTAL, &
		DEPT_TOTAL &
	)

	DEPT_TOTAL::EMP_WORKED = DEPT_TOTAL::EMP_WORKED + 1% &
		IF PR_TRN_PAY::EMPNUM <> THIS_EMP$
	EMP_TOTAL = BLANK_TOTAL

	THIS_DEPT$ = PR_TRN_PAY::DEPT
	THIS_LOC$ = PR_TRN_PAY::LOCATION
	THIS_EMP$ = PR_TRN_PAY::EMPNUM

	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB PrintDeptTotal

	GOSUB PrintLocalTotal

	GOSUB PrintGrandTotal

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

 PrintGrandTotal:
18400	!********************************************************************
	! Print the grand totals
	!********************************************************************
	TEXT$ = "Grand Total"

	CALL PRINT_TRN( &
		UTL_REPORTX, &
		TITLE$(), &
		TEXT$, &
		GRAND_TOTAL &
	)

	RETURN

	%Page

 PrintLocalTotal:
18500	!********************************************************************
	! Print the LOC totals
	!********************************************************************

	IF BY_DEPT% = 0%
	THEN
		TEXT$ = THIS_LOC$ + " " + THIS_DEPT$
	ELSE
		TEXT$ = THIS_LOC$ + " Total "
	END IF

	CALL PRINT_TRN( &
		UTL_REPORTX, &
		TITLE$(), &
		TEXT$, &
		LOC_TOTAL &
	)

	CALL SUMMARIZE_TRN( 1%, &
		LOC_TOTAL, &
		GRAND_TOTAL &
	)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	LOC_TOTAL = BLANK_TOTAL
	RETURN

	%Page

 PrintDeptTotal:
18600	!********************************************************************
	! Print the LOC totals
	!********************************************************************

	GOTO 18690 IF DEPT_TOTAL = BLANK_TOTAL

18610	!
	! Get all sales for this (location) department
	!
	WHEN ERROR IN
		GET #PR_SALES.CH%, &
			KEY #0% EQ THIS_LOC$, &
			REGARDLESS !+ THIS_DEPT$ + BEGIN_DATE$
	USE
		CONTINUE 18620
	END WHEN

	WHILE (THIS_LOC$ = PR_SALES::LOCATION)

		IF (PR_SALES::SALEDATE >= BEGIN_DATE$) AND &
			(PR_SALES::SALEDATE <= BATCH_NO$)
		THEN
			DEPT_TOTAL::SALES_AMOUNT = DEPT_TOTAL::SALES_AMOUNT + &
				PR_SALES::AMOUNT
		END IF

		WHEN ERROR IN
			GET #PR_SALES.CH%, REGARDLESS
		USE
			CONTINUE 18620
		END WHEN

	NEXT

18620	GOTO 18690 IF (BY_DEPT% = 0%)

	TEXT$ = THIS_LOC$ + " " + THIS_DEPT$

	CALL PRINT_TRN( &
		UTL_REPORTX, &
		TITLE$(), &
		TEXT$, &
		DEPT_TOTAL &
	)

18690	CALL SUMMARIZE_TRN( 0%, &
		DEPT_TOTAL, &
		LOC_TOTAL &
	)

	!
	! Initilize for start of this new location
	!
	DEPT_TOTAL = BLANK_TOTAL
	RETURN

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

	END

20000	!*******************************************************************
	! Subroutine used to summarize all of an employees data into
	! the grand totals.
	!*******************************************************************

	SUB SUMMARIZE_TRN( LONG FLAG, &
		SUMM_RECORD DEPT_TOTAL, &
		SUMM_RECORD SUMM_TOTAL &
	)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	RECORD SUMM_RECORD
		INTEGER EMP_WORKED
		REAL SALES_AMOUNT
		REAL LABOR_HOURS
		REAL GROSS_EARNINGS
		REAL VAC_HOURS
		REAL REG_HOURS
		REAL OVT_HOURS
		REAL CLASS_HOURS
		REAL MGT_HOURS
		REAL OTHER_HOURS
	END RECORD

	SUMM_TOTAL::EMP_WORKED = SUMM_TOTAL::EMP_WORKED + DEPT_TOTAL::EMP_WORKED
	SUMM_TOTAL::LABOR_HOURS = SUMM_TOTAL::LABOR_HOURS + DEPT_TOTAL::LABOR_HOURS
	SUMM_TOTAL::GROSS_EARNINGS = SUMM_TOTAL::GROSS_EARNINGS + DEPT_TOTAL::GROSS_EARNINGS
	SUMM_TOTAL::VAC_HOURS = SUMM_TOTAL::VAC_HOURS + DEPT_TOTAL::VAC_HOURS
	SUMM_TOTAL::REG_HOURS = SUMM_TOTAL::REG_HOURS + DEPT_TOTAL::REG_HOURS
	SUMM_TOTAL::OVT_HOURS = SUMM_TOTAL::OVT_HOURS + DEPT_TOTAL::OVT_HOURS
	SUMM_TOTAL::CLASS_HOURS = SUMM_TOTAL::CLASS_HOURS + DEPT_TOTAL::CLASS_HOURS
	SUMM_TOTAL::MGT_HOURS = SUMM_TOTAL::MGT_HOURS + DEPT_TOTAL::MGT_HOURS
	SUMM_TOTAL::OTHER_HOURS = SUMM_TOTAL::OTHER_HOURS + DEPT_TOTAL::OTHER_HOURS

	IF FLAG = 0%
	THEN
		SUMM_TOTAL::SALES_AMOUNT = DEPT_TOTAL::SALES_AMOUNT
	ELSE
		SUMM_TOTAL::SALES_AMOUNT = SUMM_TOTAL::SALES_AMOUNT + DEPT_TOTAL::SALES_AMOUNT
	END IF

	END SUB

21000	!*******************************************************************
	! Print out an employee, local total, grand total, ...
	!*******************************************************************

	SUB PRINT_TRN( &
		UTL_REPORTX_CDD UTL_REPORTX, &
		STRING TITLE(), &
		STRING TEXT, &
		SUMM_RECORD SUMM_TOTAL &
	)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! Dimensions
	!
	RECORD SUMM_RECORD
		INTEGER EMP_WORKED
		REAL SALES_AMOUNT
		REAL LABOR_HOURS
		REAL GROSS_EARNINGS
		REAL VAC_HOURS
		REAL REG_HOURS
		REAL OVT_HOURS
		REAL CLASS_HOURS
		REAL MGT_HOURS
		REAL OTHER_HOURS
	END RECORD

	!
	! Set up
	!
	IF SUMM_TOTAL::LABOR_HOURS = 0.0
	THEN
		PROD_PERC = 0.0
	ELSE
		PROD_PERC = (SUMM_TOTAL::SALES_AMOUNT / SUMM_TOTAL::LABOR_HOURS)
	END IF

	IF SUMM_TOTAL::SALES_AMOUNT = 0.0
	THEN
		PR_PERC = 0.0
	ELSE
		PR_PERC = (SUMM_TOTAL::GROSS_EARNINGS / SUMM_TOTAL::SALES_AMOUNT) * 100.0
	END IF

	IF SUMM_TOTAL::LABOR_HOURS = 0.0
	THEN
		ADV_RATE = 0.0
	ELSE
		ADV_RATE = (SUMM_TOTAL::GROSS_EARNINGS / SUMM_TOTAL::LABOR_HOURS)
	END IF

	TEXT$ = TEXT + &
		FORMAT$(PROD_PERC, "####.##  ") + &
		FORMAT$(SUMM_TOTAL::EMP_WORKED, "#### ") + &
		FORMAT$(SUMM_TOTAL::SALES_AMOUNT, "######.##  ") + &
		FORMAT$(SUMM_TOTAL::LABOR_HOURS, "#####.## ") + &
		FORMAT$(SUMM_TOTAL::GROSS_EARNINGS, "#####.## ") + &
		FORMAT$(PR_PERC, "###.## ") + &
		FORMAT$(ADV_RATE, "##.## ") + &
		FORMAT$(SUMM_TOTAL::REG_HOURS, "<%>####.## ") + &
		FORMAT$(SUMM_TOTAL::OVT_HOURS, "<%>###.## ") + &
		FORMAT$(SUMM_TOTAL::CLASS_HOURS, "<%>###.## ") + &
		FORMAT$(SUMM_TOTAL::VAC_HOURS, "<%>###.## ") + &
		FORMAT$(SUMM_TOTAL::MGT_HOURS, "<%>###.## ") + &
		FORMAT$(SUMM_TOTAL::OTHER_HOURS, "<%>###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	END SUB
