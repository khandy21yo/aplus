1	%TITLE "Employee Overtime Report"
	%SBTTL "PR_RPRT_AUDT_ACCRUAL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! Computer Management Center, Inc..
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:PR070
	!
	! Abstract:HELP
	!	.p
	!	The ^*Employee Overtime report\* lists all employees who have earned overtime
	!	rates in the defined period and for defined parameters. Included in this
	!	report are the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Location
	!	.le
	!	Department
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Minor Status
	!	.le
	!	Overtime Hours
	!	.le
	!	Overtime Amount
	!	.els
	!
	! Index:
	!	.x Employee Overtime>Report
	!	.x Report>Employee Overtime
	!	.x Overtime>Report
	!	.x Report>Overtime
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_AUDT_ACCRUAL/LINE/NOOPT
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_AUDT_ACCRUAL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_AUDT_ACCRUAL.OBJ;*
	!
	! Author:
	!
	!	04/21/92 - Kevin Handy
	!
	! Modification history:
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	05/04/93 - Kevin Handy
	!		Trap error at 17270 (lookup into EMP_ACCRUAL file).
	!
	!	05/26/93 - Kevin Handy
	!		Added Subtotal by employee.
	!
	!	12/02/94 - Kevin Handy
	!		Added wildcard for accrual type
	!
	!	01/11/95 - Kevin Handy
	!		Added wildcard for employee number
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/11/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)	PR_TRN_PAY_CDD		PR_TRN_PAY
	MAP	(PR_HIS_PAY)	PR_TRN_PAY_CDD		PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	MAP	(PR_EMP_ACCRUAL) PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	RECORD PR_TEMP_RECORD
		STRING	CODE = 2%
		STRING	EMPNUM = 10%
		STRING  PDATE = 8%
		GFLOAT	ACC_HR
		GFLOAT	ACC_DOL
		GFLOAT	USE_HR
		GFLOAT	USE_DOL
	END RECORD

	MAP (PR_TEMP) PR_TEMP_RECORD PR_TEMP

	!
	! Dimension Statements
	!
	DIM DATA_FILE$(2000%)

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)

	!++
	! Abstract:FLD01
	!	^*(01) Start Payroll Date\*
	!	.p
	!	The ^*Start Payroll Date\* field specifies the date of
	!	the payroll folder with which the report will begin printing.
	!	A blank field will cause the report to start
	!	with the first payroll folder date in the file.
	!
	! Index:
	!	.x Start Payroll Date>Employee Check Audit Report
	!	.x Employee Check Audit Report>Start Payroll Date
	!
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

	!++
	! Abstract:FLD02
	!	^*(02) End Payroll Date\*
	!	.p
	!	The ^*End Payroll Date\* field specifies the date of
	!	the payroll folder with which the report is to end printing.
	!	A blank field will cause the report to end
	!	with the last payroll folder date in the file.
	!
	! Index:
	!	.x End Payroll Date>Employee Check Audit Report
	!	.x Employee Check Audit Report>End Payroll Date
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	!
	! Abstract:FLD03
	!	^*(02) From Location _#\*
	!	.p
	!	The ^*From Location _#\* field specifies the printing
	!	to begin with a particular location.
	!	.p
	!	A blank field will cause the report to start with the first location in the
	!	file.
	!
	! Index:
	!	.x From Location>Overtime Report
	!	.x Overtime Report>From Location
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	!
	! Abstract:FLD04
	!	^*(03) To Location _#\*
	!	.p
	!	The ^*To Location _#\* fiels specifies the printing
	!	to end with a particular location.
	!	.p
	!	A blank field will cause the report to end with the last location in the file.
	!
	! Index:
	!	.x To Location>Overtime Report
	!	.x Overtime Report>To Location
	!
	!--

	DEPT$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)
	!++
	!
	! Abstract:FLD05
	!	^*(04) Department\*
	!	.p
	!	The ^*Department\* field specifies which department is to be examined in
	!	finding the overtime hours by inserting the department number. If all
	!	departments are to be examined, leave the field blank or insert an _*.
	!
	! Index:
	!	.x Department>Overtime Report
	!	.x Overtime Report>Department
	!
	!--

	WCENTER$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)
	!++
	!
	! Abstract:FLD06
	!	^*(05) Workcenter\*
	!	.p
	!	The ^*Workcenter\* is a smaller division within the department. It may be
	!	left blank to get a report including all workcenters or individual centers
	!	may be identified.
	!
	! Index:
	!	.x Workcenter>Overtime Report
	!	.x Overtime Report>Workcenter
	!
	! Datatype:TEXT
	! Size:6
	!--

	ONLY_TOTAL$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)
	!++
	!
	! Abstract:FLD07
	!	^*(07) Print Totals Only_?\*
	!	.p
	!	The ^*Print Totals\* command specifies if only totals, or if the entire
	!	report to be printed. If ^*Y\* is entered, only the report totals will be
	!	printed. If ^*N\* is entered, the entire report will be printed.
	!
	! Index:
	!	.x Print Totals>Overtime Report
	!	.x Overtime Report>Print Totals
	!
	!--

	WILD_ACC$ = TRM$(UTL_REPORTX::OPTDEF(7%))
	!++
	!
	! Abstract:FLD08
	!	^*(08) Wildcard Accrual\*
	!
	! Index:
	!	.x Wildcard Accrual>Overtime Report
	!	.x Overtime Report>Wildcard Accrual
	!
	! Datatype:TEXT
	! Size:1
	! Required:Y
	! Valid Input: Y,N
	!--

	WILD_EMP$ = TRM$(UTL_REPORTX::OPTDEF(8%))
	!++
	!
	! Abstract:FLD09
	!	^*(08) Wildcard Employee\*
	!
	! Index:
	!	.x Wildcard Employee>Overtime Report
	!	.x Overtime Report>Wildcard Employee
	!
	! Datatype:TEXT
	! Size:1
	! Required:Y
	! Valid Input: Y,N
	!--

	RECALC$ = LEFT$(UTL_REPORTX::OPTDEF(9%), 1%)
	!++
	!
	! Abstract:FLD10
	!	^*(10) Wildcard Employee\*
	!
	! Index:
	!	.x Recalculate rates>Overtime Report
	!	.x Overtime Report>Recalculate rates
	!
	! Datatype:TEXT
	! Size:1
	! Required:Y
	! Valid Input: Y,N
	!--

	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

290	!
	! Create a temporary file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)
	OPEN "PR_TEMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		TEMPORARY, &
		BUFFER 32%, &
		MAP PR_TEMP, &
		PRIMARY KEY (PR_TEMP::CODE, &
			PR_TEMP::EMPNUM, &
			PR_TEMP::PDATE) DUPLICATES, &
		ACCESS MODIFY, &
		ALLOW NONE

300	!
	! Employee Master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Accrual definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.OPN"
	USE
		FILENAME$ = "PR_EMP_ACCRUAL"
		CONTINUE HelpError
	END WHEN

320	!
	! Ernded defintions
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Quarterly Accrual Report"
	TITLE$(2%) = "From " + PRNT_DATE(FROM_BATCH_NO$, 8%) + " to " + &
		PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = "Payroll System"
	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "ED                                          " + &
		"           ------ AccruaL -------  -------- Use " + &
		"------------  ------ Change -------"
	TITLE$(6%) = "CD  Description      EmpNum      EmpName    " + &
		"              Hours      Dollars      Hours     " + &
		" Dollars         Hours      Dollars"
	TITLE$(7%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$Location:004,$Dept:011,$WorkCenter:016," + &
		"$EmpNum:027,$EmpName:058,$Date:67,$MinorStat:070," + &
		"VOvLimit:079,VOvAmt:088"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	FOR PR_LOOP% = 1% TO DATA_FILE%

		BATCH_NO$ = DATA_FILE$(PR_LOOP%)
		CALL ENTR_3MESSAGE(SCOPE, "Processing: " + BATCH_NO$, 1%)

		!
		! Open Pay folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 17010 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%
		USE_HISTORY% = 0%

		GOTO 17015

17010		!
		! Open pay history folder if journal not there
		!
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		PR_TRN_PAY.CH% = 0%

		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%
		USE_HISTORY% = -1%

17015		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_TMP_PAY.CH%, KEY #2%
		ELSE
			FIND #PR_TMP_PAY.CH%, KEY #2% GE FROM_ITEM$, REGARDLESS
		END IF

 GetNextRec:
17020		!
		! Main loop starts here
		!
		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Get next record
		!
		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			CONTINUE 17190 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		IF USE_HISTORY%
		THEN
			PR_TRN_PAY = PR_HIS_PAY
		END IF

		!
		! Check current record
		!
		GOTO ExitLoop IF (PR_TRN_PAY::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec &
			IF COMP_STRING(PR_TRN_PAY::DEPT, DEPT$) = 0% AND &
			DEPT$ <> ""
		GOTO GetNextRec &
			IF COMP_STRING(PR_TRN_PAY::WORK_CENTER, &
			WCENTER$) = 0% AND &
			WCENTER$ <> ""

17100		!
		! Handle accrual
		!
		GOTO 17020 &
			IF COMP_STRING(PR_TRN_PAY::CODE, WILD_ACC$) = 0% AND &
			WILD_ACC$ <> ""

		GOTO 17020 &
			IF COMP_STRING(PR_TRN_PAY::EMPNUM, WILD_EMP$) = 0% AND &
			WILD_EMP$ <> ""

		IF PR_TRN_PAY::PTYPE = "A"
		THEN
			PR_TEMP::CODE	= PR_TRN_PAY::CODE
			PR_TEMP::EMPNUM	= PR_TRN_PAY::EMPNUM
			PR_TEMP::PDATE  = BATCH_NO$

			PR_TEMP::ACC_HR = PR_TRN_PAY::REG_HR + &
				PR_TRN_PAY::OVT_HR
			PR_TEMP::ACC_DOL = PR_TRN_PAY::GROSS
			PR_TEMP::USE_HR = 0.0
			PR_TEMP::USE_DOL = 0.0

			PUT #PR_TEMP.CH%

			GOTO 17020
		END IF

17110		!
		! Handle use of accrual
		!
		GOTO 17020 UNLESS PR_TRN_PAY::PTYPE = "P"

		WHEN ERROR IN
			FIND #PR_EMP_ACCRUAL.CH%, &
				KEY #0% EQ PR_TRN_PAY::EMPNUM + &
				PR_TRN_PAY::CODE, &
				REGARDLESS
		USE
			CONTINUE 17020
		END WHEN

17120		WHEN ERROR IN
			GET #PR_TEMP.CH%, &
				KEY #0% EQ PR_TRN_PAY::CODE + &
				PR_TRN_PAY::EMPNUM + &
				BATCH_NO$
		USE
			CONTINUE 17130
		END WHEN

		PR_TEMP::USE_HR = PR_TEMP::USE_HR + PR_TRN_PAY::REG_HR + &
			PR_TRN_PAY::OVT_HR
		PR_TEMP::USE_DOL = PR_TEMP::USE_DOL + PR_TRN_PAY::GROSS

		UPDATE #PR_TEMP.CH%

		GOTO 17020

17130		PR_TEMP::CODE	= PR_TRN_PAY::CODE
		PR_TEMP::EMPNUM	= PR_TRN_PAY::EMPNUM
		PR_TEMP::PDATE  = BATCH_NO$

		PR_TEMP::ACC_HR = 0.0
		PR_TEMP::ACC_DOL = 0.0
		PR_TEMP::USE_HR = PR_TRN_PAY::REG_HR + &
			PR_TRN_PAY::OVT_HR
		PR_TEMP::USE_DOL = PR_TRN_PAY::GROSS

		PUT #PR_TEMP.CH%

		GOTO 17020

 ExitLoop:
17190		CLOSE #PR_TMP_PAY.CH%
		CALL ASSG_FREECHANNEL(PR_TMP_PAY.CH%)
		PR_TMP_PAY.CH% = 0%
		PR_TRN_PAY.CH% = 0%
		PR_HIS_PAY.CH% = 0%
	NEXT PR_LOOP%

17200	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	COUNT% = 0%
	EMPCOUNT% = 0%
	TEST_EMP$ = ""

	RESET #PR_TEMP.CH%

17220	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	IF TEST_CODE$ <> PR_TEMP::CODE
	THEN
		GOSUB EmpSummary
		GOSUB Summary
		TEST_CODE$ = PR_TEMP::CODE
		TEST_EMP$ = PR_TEMP::EMPNUM
	END IF

	IF TEST_EMP$ <> PR_TEMP::EMPNUM
	THEN
		GOSUB EMPSummary
		TEST_EMP$ = PR_TEMP::EMPNUM
	END IF

	PR_EMP_MASTER::EMPNUM	= PR_TEMP::EMPNUM
	PR_EMP_MASTER::EMPNAME	= "???????????????????????????????"
	PR_EMP_MASTER::SSN	= "???-??-????"
	PR_EMP_MASTER::HIREDAY	= "00000000"

17260	GET #PR_EMP_MASTER.CH%, KEY #0% EQ PR_TEMP::EMPNUM, REGARDLESS

17270	IF (PR_ERNDED_DEF::CODE <> PR_TEMP::CODE)
	THEN
		WHEN ERROR IN
			GET #PR_ERNDED_DEF.CH%, &
				KEY #0% EQ "P" + PR_TEMP::CODE, &
				REGARDLESS
		USE
			PR_ERNDED_DEF::ETYPE = "P"
			PR_ERNDED_DEF::CODE  = PR_TEMP::CODE
			PR_ERNDED_DEF::DESCR = "????????????????????"

			CONTINUE 17300
		END WHEN
	END IF

17300	!
	! If the recalculate flag has been set, then determine the
	! proper rate to use.
	!
	IF RECALC$ = "Y"
	THEN
		RATE_TYPE$ = "H"
		RATE_CODE$ = PR_TEMP::CODE

		!
		! Try for a specific rate based on the pay code first
		!
		CALL PR_READ_RATE_CODE(PR_EMP_MASTER::EMPNUM, &
			PR_EMP_MASTER::OPER, &
			PR_TEMP::PDATE, &
			RATE_TYPE$, &
			RATE_CODE$, &
			HOUR_RATE, &
			PIECE_RATE, &
			FACTOR%, &
			STDEFF, &
			EVALDATE$, &
			EFF_DATE$)

		!
		! Try for the standard employee rate if a specific rate isn't found
		!
		IF HOUR_RATE = 0.0
		THEN
			CALL PR_READ_RATE_CODE(PR_EMP_MASTER::EMPNUM, &
				PR_EMP_MASTER::OPER, &
				PR_TEMP::PDATE, &
				PR_EMP_MASTER::RATE_TYPE, &
				PR_EMP_MASTER::RATE_CDE, &
				HOUR_RATE, &
				PIECE_RATE, &
				FACTOR%, &
				STDEFF, &
				EVALDATE$, &
				EFF_DATE$)
		END IF

		!
		! Try for a generic rate if a no other rate is found
		!
		IF HOUR_RATE = 0.0
		THEN
			CALL PR_READ_RATE(PR_EMP_MASTER::EMPNUM, &
				PR_EMP_MASTER::OPER, &
				PR_TEMP::PDATE, &
				RATE_TYPE$, &
				RATE_CDE$, &
				HOUR_RATE, &
				PIECE_RATE, &
				FACTOR%, &
				STDEFF, &
				EVALDATE$, &
				EFF_DATE$)
		END IF

		PR_TEMP::ACC_DOL = FUNC_ROUND(PR_TEMP::ACC_HR * HOUR_RATE, 2%)
	END IF

	!
	! Print total for employee
	!
	TEXT$ = PR_TEMP::CODE + " " + &
		LEFT(PR_ERNDED_DEF::DESCR, 9%) + " " + &
		PRNT_DATE(PR_TEMP::PDATE, 6%) + " " + &
		PR_TEMP::EMPNUM + " " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 20%) + " " + &
		FORMAT$(PR_TEMP::ACC_HR, "#,###,###.##") + &
		FORMAT$(PR_TEMP::ACC_DOL, "#,###,###.##") + &
		FORMAT$(PR_TEMP::USE_HR, "#,###,###.##") + &
		FORMAT$(PR_TEMP::USE_DOL, "#,###,###.##") + &
		FORMAT$(PR_TEMP::ACC_HR - PR_TEMP::USE_HR, "#,###,###.##") + &
		FORMAT$(PR_TEMP::ACC_DOL - PR_TEMP::USE_DOL, "#,###,###.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	SUMM_QTR_ACC_DOL = SUMM_QTR_ACC_DOL + PR_TEMP::ACC_DOL
	SUMM_QTR_ACC_HR = SUMM_QTR_ACC_HR + PR_TEMP::ACC_HR
	SUMM_QTR_USE_DOL = SUMM_QTR_USE_DOL + PR_TEMP::USE_DOL
	SUMM_QTR_USE_HR = SUMM_QTR_USE_HR + PR_TEMP::USE_HR

	EMP_QTR_ACC_DOL = EMP_QTR_ACC_DOL + PR_TEMP::ACC_DOL
	EMP_QTR_ACC_HR = EMP_QTR_ACC_HR + PR_TEMP::ACC_HR
	EMP_QTR_USE_DOL = EMP_QTR_USE_DOL + PR_TEMP::USE_DOL
	EMP_QTR_USE_HR = EMP_QTR_USE_HR + PR_TEMP::USE_HR

	COUNT% = COUNT% + 1%
	EMPCOUNT% = EMPCOUNT% + 1%

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO 17220

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB EmpSummary
	GOSUB Summary

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

	%Page

 Summary:
	!******************************************************************
	! Look up state profile record
	!******************************************************************

	IF COUNT% <> 0%
	THEN
		TEXT$ = "                    " + &
			"       Total                      " + &
			FORMAT$(SUMM_QTR_ACC_HR, "#,###,###.##") + &
			FORMAT$(SUMM_QTR_ACC_DOL, "#,###,###.##") + &
			FORMAT$(SUMM_QTR_USE_HR, "#,###,###.##") + &
			FORMAT$(SUMM_QTR_USE_DOL, "#,###,###.##") + &
			FORMAT$(SUMM_QTR_ACC_HR - SUMM_QTR_USE_HR, "#,###,###.##") + &
			FORMAT$(SUMM_QTR_ACC_DOL - SUMM_QTR_USE_DOL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	SUMM_QTR_ACC_DOL = 0.0
	SUMM_QTR_ACC_HR = 0.0
	SUMM_QTR_USE_DOL = 0.0
	SUMM_QTR_USE_HR = 0.0

	COUNT% = 0%

	RETURN

 EmpSummary:
	!******************************************************************
	! Look up state profile record
	!******************************************************************

	IF EMPCOUNT% <> 0%
	THEN
		TEXT$ = "                    " + &
			"       Employee Total             " + &
			FORMAT$(EMP_QTR_ACC_HR, "#,###,###.##") + &
			FORMAT$(EMP_QTR_ACC_DOL, "#,###,###.##") + &
			FORMAT$(EMP_QTR_USE_HR, "#,###,###.##") + &
			FORMAT$(EMP_QTR_USE_DOL, "#,###,###.##") + &
			FORMAT$(EMP_QTR_ACC_HR - EMP_QTR_USE_HR, "#,###,###.##") + &
			FORMAT$(EMP_QTR_ACC_DOL - EMP_QTR_USE_DOL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	EMP_QTR_ACC_DOL = 0.0
	EMP_QTR_ACC_HR = 0.0
	EMP_QTR_USE_DOL = 0.0
	EMP_QTR_USE_HR = 0.0

	EMPCOUNT% = 0%

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

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
