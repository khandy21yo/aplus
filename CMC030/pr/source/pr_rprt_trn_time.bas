1	%TITLE "Payroll Time/Unit Report"
	%SBTTL "PR_RPRT_TRN_TIME"
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
	! ID:PR001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Time Journal Report\* option lists data
	!	relative to a specific payroll file. The primary purpose of the
	!	report is to review and balance the data entered
	!	in the Timekeeper routine. The user has the option to print the
	!	report in a variety of sequences.
	!	.b
	!	It is recommended that the final version for each
	!	payroll folder be filed permanently.
	!	.b
	!	The data fields include:
	!	.table 3,25
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name
	!	.te
	!	Default Location
	!	.te
	!	Period End Date
	!	.te
	!	Location Worked
	!	.te
	!	Type
	!	.te
	!	Union
	!	.te
	!	Department
	!	.te
	!	General Ledger Account
	!	.te
	!	Earnings Code
	!	.te
	!	Payment Type
	!	.te
	!	Rate Code
	!	.te
	!	Rate
	!	.te
	!	Regular Hours
	!	.te
	!	Overtime Hours
	!	.te
	!	Overtime Factor
	!	.te
	!	Gross Earnings
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Time Journal>Report
	!	.x Report>Time Journal
	!	.x Time JOurnal Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_TIME/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_TIME, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_TIME.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	05/23/89 - Kevin Handy
	!		Fixed titles ("Default location" instead of "subacct",
	!		"End date" instead of "Oper").
	!
	!	06/25/89 - J. Shad Rydalch
	!		Added sort field "LN" by creating a temporary file
	!		of PR_EMP_MASTER and make it look like it is going
	!		through PR_TRN_PAY. When is really going through
	!		the temporary file.
	!
	!	07/06/90 - Kevin Handy
	!		Fixed handling of HIS files.
	!
	!	07/30/91 - Kevin Handy
	!		Fixed bug in sortby so that 'LO' goes by
	!		location instead of Name, and added SN so that
	!		this report prints in the same orders as the
	!		worksheet.
	!
	!	08/02/91 - Kevin Handy
	!		Many mods to make sortby work correctly.
	!
	!	02/17/92 - Kevin Handy
	!		Modified to ignore "A" records.
	!
	!	10/15/92 - Kevin Handy
	!		Fixed bug upon finding end of the pay file
	!		before the end of the-report.
	!
	!	11/16/92 - Kevin Handy
	!		Added ability to print subtotal by rate code
	!		as requested by King B
	!
	!	03/08/93 - Kevin Handy
	!		Modified to subtotal when payroll date changes on
	!		the employee, not only when employee number
	!		changes.
	!
	!	03/31/93 - Kevin Handy
	!		Modified to zero subtotal by pay type after it
	!		is printed, so doesn't accumulate between pay
	!		dates.
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS to several GET and FIND statements.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	01/30/2000 - Kevin Handy
	!		Add "SO" sort by option.
	!		Fix handling of several sort by options.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD    PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)	PR_TRN_PAY_CDD		PR_TRN_PAY
	MAP	(PR_HIS_PAY)	PR_TRN_PAY_CDD		PR_HIS_PAY

	RECORD SUBTOTAL_CDD
		STRING	CODE = 2%
		REAL	REG_HR
		REAL	OVT_HR
		REAL	PIECE
		REAL	GROSS
	END RECORD

	DIM SUBTOTAL_CDD EMP_SUBTOTAL(50%), SUBTOTAL(50%)

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
	!	.ts 55
	!	^*(01) Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Payroll Date\* field
	!	enters the payroll file date of the payroll which
	!	is to be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Time Journal Report>Payroll Date
	!	.x Payroll Date>Time Journal Report
	!	.x Date>Time Journal Report
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item _#\*
	!	.b
	!	.lm +5
	!	The ^*From Item _#\* field begins the report with a
	!	selected location or employee number. Any value in this field must be
	!	in agreement with the value in the Sort field (05).
	!	For example, if field (05) has a value of ^*NU\*, this field
	!	must contain an employee number or be left blank. If field (05) has a value of
	!	^*LN\* or ^*LO\*, this field must contain a location code or be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Time Journal Report>From Item
	!	.x From Item>Time Journal Report
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item _#\*
	!	.b
	!	.lm +5
	!	The ^*To Item _#\* field
	!	ends the report with a selected location or employee
	!	number. Any value in this field must be in agreement with the value in the
	!	Sort field (05). For example, if field (05) has a value of ^*NU\*, this field
	!	must contain an employee number or be left blank. If field (05) has a value of
	!	^*LN\* or ^*LO\*, this field must contain a location code or be left blank.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Time Journal Report
	!	.x Time Journal Report>To Item
	!
	!--

	BATCH_ENTRY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Entry Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*Entry Batch _#\* field
	!	selections a specific batch number that has previously been
	!	assigned to be printed.
	!	.b
	!	If this field is left blank, the Time Journal Report will include all batches
	!	for a specific payroll file folder.
	!	.lm -5
	!
	! Index:
	!	.x Time Journal Report>Entry Batch
	!	.x Entry Batch>Time Journal Report
	!	.x Batch>Time Journal Report
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.ts 55
	!	^*(05) Sort	LN,LO,NU,NA,SN,SO\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field
	!	selects the order in which the report will be printed. Valid
	!	selections are:
	!	.table 3,25
	!	.te
	!	^*NU\*	Number
	!	.te
	!	^*NA\*	Name
	!	.te
	!	^*SO\*	Sort name
	!	.te
	!	^*SN\*	Social Security Number
	!	.te
	!	^*LO\*	Location, Alpha sort
	!	.te
	!	^*LN\*	Location, Department, Number
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Sort>Time Journal Report
	!	.x Time Journal Report>Sort
	!
	!--

	DEPT$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Department\*
	!	.b
	!	.lm +5
	!	The ^*Department\* field recordings the department, area,
	!	to which the employee is assigned.
	!	.lm -5
	!
	! Index:
	!	.x Department>Time Journal Report
	!	.x Time Journal Report>Department
	!
	!--

	XCODE$ = LEFT(UTL_REPORTX::OPTDEF(6%), 1%)

	!++
	! Abstract:FLD07
	!	^*(07) Code Subtotal\*
	!
	! Index:
	!	.x Code Subtotal>Time Journal Report
	!	.x Time Journal Report>Code Subtotal
	!
	!--

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Pay folder
	!
	USE_HISTORY% = 0%
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		CONTINUE 315 IF ERR = 5%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	GOTO 320

315	!
	! Open Pay History folder if regular folder not found
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

	%PAGE

	SELECT SORTBY$
	CASE "NU"
		K_NUM% = 0%

	CASE "NA"
		K_NUM% = 1%

	CASE "SO"
		K_NUM% = 2%

	CASE "SN"
		K_NUM% = 3%

	CASE "LO"
		K_NUM% = 4%

	CASE "LN"
550		!
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

560		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, REGARDLESS
		USE
			CONTINUE 565 IF ERR = 11%	! end of file
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

		!
		! Check to see if it is a record we really want,
		! if there is not an errorTHEN we want it.
		!
		WHEN ERROR IN
			FIND #PR_TRN_PAY.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
				REGARDLESS
			PUT #PR_EMP_MASTER_TEMP.CH%
		USE
			CONTINUE 560 IF ERR = 155%	! record not found
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

		GOTO 560

565		K_NUM% = 0%
		PR_EMP_MASTER.CH% = PR_EMP_MASTER_TEMP.CH%

	END SELECT

 ReportTitle:
	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Payroll Time/Unit Report"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		MID(BATCH_NO$, 5%, 2%) + "/" + &
		MID(BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(BATCH_NO$, 4%)

	TITLE$(3%) = ""

	TITLE$(4%) = "                                       Default   " + &
		"  End                --------------Hour-------------  " + &
		"-------Units-------"

	TITLE$(5%) = "Loc    Ty Un Dept   Account           Location   " + &
		" Date     EC  PT  RC     Rate  Reg Hrs  Ovt Hrs Fac   " + &
		"     Rate       Qty     Gross"

	TITLE$(6%) = ""

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
		FILENAME$ = "PR_EMP_MASTER_TEMP"
		CONTINUE HelpError
	END WHEN

	SUB_COUNT% = 0%
	EMP_LINE_COUNTER% = 0%
	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%
	SUBTOTAL% = 0%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	EMP_SUBTOTAL% = 0%

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


	GOTO 17020 IF COMP_STRING(PR_EMP_MASTER::DEPT, DEPT$) = 0% &
		AND DEPT$ <> ""

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "LN"
		GOTO ExitTotal IF (PR_EMP_MASTER::LOCATION > &
			TO_ITEM$) AND TO_ITEM$ <> ""
		GOSUB SubTotal IF TEST_LOCATION$ <> "" AND &
			TEST_LOCATION$ <> PR_EMP_MASTER::LOCATION
		TEST_LOCATION$ = PR_EMP_MASTER::LOCATION

	CASE "NU"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "LO"
		GOTO ExitTotal IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOSUB SubTotal IF TEST_LOCATION$ <> "" AND &
			TEST_LOCATION$ <> PR_EMP_MASTER::LOCATION
		TEST_LOCATION$ = PR_EMP_MASTER::LOCATION

	CASE "NA"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "SO"
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "SN"
		GOTO ExitTotal IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	END SELECT

17030	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17020
	END WHEN

	!
	! If history flag is setTHEN set history into journal map
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

17040	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		PR_TRN_PAY::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + " " + &
		PR_EMP_MASTER::LOCATION + " " + &
		PRNT_DATE(PR_TRN_PAY::PR_END_DATE, 6%), 0%)

	TEST_END_DATE$ = PR_TRN_PAY::PR_END_DATE + ""

17100	!
	! Quit if employee number changes
	!
	IF (PR_TRN_PAY::EMPNUM <> PR_EMP_MASTER::EMPNUM)
	THEN
		GOSUB EmployeeTotal
		GOTO 17020
	END IF

	!
	! Subtotal if pay date changes
	!
	IF (PR_TRN_PAY::PR_END_DATE <> TEST_END_DATE$)
	THEN
		GOSUB EmployeeTotal
		GOTO 17040
	END IF

	GOTO 17350 IF PR_TRN_PAY::PTYPE = "A"

	!
	! Print employee record
	!
	GOTO 17350 IF BATCH_ENTRY$ <> "" AND &
		BATCH_ENTRY$ <> PR_TRN_PAY::BATCH_ENTRY

	W$ = " "
	W$ = "*" IF PR_TRN_PAY::LOCATION <> PR_EMP_MASTER::LOCATION

	TEXT$ = PR_TRN_PAY::LOCATION + " " + W$ + " " + &
		PR_TRN_PAY::EMP_GRADE + " " + &
		PR_TRN_PAY::UNION + " " + &
		PR_TRN_PAY::DEPT + " " + &
		PR_TRN_PAY::ACCT + " " + &
		PR_TRN_PAY::SUBACC + " " + &
		PR_TRN_PAY::OPER + " " + &
		PR_TRN_PAY::CODE + "  " + &
		PR_TRN_PAY::PTYPE + "   " + &
		PR_TRN_PAY::RTYPE + "  " + &
		FORMAT$(PR_TRN_PAY::HOUR_RATE, "####.### ") + &
		FORMAT$(PR_TRN_PAY::REG_HR, "#####.## ") + &
		FORMAT$(PR_TRN_PAY::OVT_HR, "#####.## ") + &
		FORMAT$(PR_TRN_PAY::FACTOR, "###%  ") + &
		FORMAT$(PR_TRN_PAY::PIECE_RATE, "####.#### ") + &
		FORMAT$(PR_TRN_PAY::PIECE, "#####.### ") + &
		FORMAT$(PR_TRN_PAY::GROSS, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	EMP_LINE_COUNTER% = EMP_LINE_COUNTER% + 1%
	SUB_COUNT% = SUB_COUNT% + 1%

	GOTO ExitProgram IF UTL_REPORTX::STAT

	EMP_TOTAL(1%) = EMP_TOTAL(1%) + PR_TRN_PAY::REG_HR
	EMP_TOTAL(2%) = EMP_TOTAL(2%) + PR_TRN_PAY::OVT_HR
	EMP_TOTAL(3%) = EMP_TOTAL(3%) + PR_TRN_PAY::PIECE
	EMP_TOTAL(4%) = EMP_TOTAL(4%) + PR_TRN_PAY::GROSS

	!
	! Summarize by rate type
	!
	FOR LOOP% = 1% TO EMP_SUBTOTAL%
		IF EMP_SUBTOTAL(LOOP%)::CODE = PR_TRN_PAY::CODE
		THEN
			EMP_SUBTOTAL(LOOP%)::REG_HR = &
				FUNC_ROUND(EMP_SUBTOTAL(LOOP%)::REG_HR + &
				PR_TRN_PAY::REG_HR, 2%)
			EMP_SUBTOTAL(LOOP%)::OVT_HR = &
				FUNC_ROUND(EMP_SUBTOTAL(LOOP%)::OVT_HR + &
				PR_TRN_PAY::OVT_HR, 2%)
			EMP_SUBTOTAL(LOOP%)::PIECE = &
				FUNC_ROUND(EMP_SUBTOTAL(LOOP%)::PIECE + &
				PR_TRN_PAY::PIECE, 2%)
			EMP_SUBTOTAL(LOOP%)::GROSS = &
				FUNC_ROUND(EMP_SUBTOTAL(LOOP%)::GROSS + &
				PR_TRN_PAY::GROSS, 2%)
			GOTO 17120
		END IF
	NEXT LOOP%

	EMP_SUBTOTAL%, LOOP% = EMP_SUBTOTAL% + 1%
	EMP_SUBTOTAL(LOOP%)::CODE = PR_TRN_PAY::CODE
	EMP_SUBTOTAL(LOOP%)::REG_HR = PR_TRN_PAY::REG_HR
	EMP_SUBTOTAL(LOOP%)::OVT_HR = PR_TRN_PAY::OVT_HR
	EMP_SUBTOTAL(LOOP%)::PIECE = PR_TRN_PAY::PIECE
	EMP_SUBTOTAL(LOOP%)::GROSS = PR_TRN_PAY::GROSS

17120	!
	! Summarize by rate type
	!
	FOR LOOP% = 1% TO SUBTOTAL%
		IF SUBTOTAL(LOOP%)::CODE = PR_TRN_PAY::CODE
		THEN
			SUBTOTAL(LOOP%)::REG_HR = &
				FUNC_ROUND(SUBTOTAL(LOOP%)::REG_HR + &
				PR_TRN_PAY::REG_HR, 2%)
			SUBTOTAL(LOOP%)::OVT_HR = &
				FUNC_ROUND(SUBTOTAL(LOOP%)::OVT_HR + &
				PR_TRN_PAY::OVT_HR, 2%)
			SUBTOTAL(LOOP%)::PIECE = &
				FUNC_ROUND(SUBTOTAL(LOOP%)::PIECE + &
				PR_TRN_PAY::PIECE, 2%)
			SUBTOTAL(LOOP%)::GROSS = &
				FUNC_ROUND(SUBTOTAL(LOOP%)::GROSS + &
				PR_TRN_PAY::GROSS, 2%)
			GOTO 17140
		END IF
	NEXT LOOP%

	SUBTOTAL%, LOOP% = SUBTOTAL% + 1%
	SUBTOTAL(LOOP%)::CODE = PR_TRN_PAY::CODE
	SUBTOTAL(LOOP%)::REG_HR = PR_TRN_PAY::REG_HR
	SUBTOTAL(LOOP%)::OVT_HR = PR_TRN_PAY::OVT_HR
	SUBTOTAL(LOOP%)::PIECE = PR_TRN_PAY::PIECE
	SUBTOTAL(LOOP%)::GROSS = PR_TRN_PAY::GROSS


17140	!

17350	!
	! Try for next record
	!
	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 17390 IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	GOTO 17100

17390	GOSUB EmployeeTotal
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB EmployeeTotal

	SELECT SORTBY$
	CASE "SA", "LN", "LO", "LN"
		GOSUB SubTotal
	END SELECT

	IF (SUBTOTAL% > 1%) AND (XCODE$ = "Y")
	THEN
		FOR LOOP% = 1% TO SUBTOTAL%
			TEXT$ = SPACE$(44%) + &
				"Code Subtotal  " + &
				SUBTOTAL(LOOP%)::CODE + &
				SPACE$(18%) + &
				FORMAT$(SUBTOTAL(LOOP%)::REG_HR, "#####.## ") + &
				FORMAT$(SUBTOTAL(LOOP%)::OVT_HR, "#####.## ") + &
				"      " + &
				"          " + &
				FORMAT$(SUBTOTAL(LOOP%)::PIECE, "#####.### ") + &
				FORMAT$(SUBTOTAL(LOOP%)::GROSS, "######.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		NEXT LOOP%
	END IF

	TEXT$ = SPACE$(55%) + "Grand Total             " + &
		FORMAT$(GRAND_TOTAL(1%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#####.##                 ") + &
		FORMAT$(GRAND_TOTAL(3%), "#####.### ") + &
		FORMAT$(GRAND_TOTAL(4%), "######.##")

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

 EmployeeTotal:
	!******************************************************************
	! Print Employee total
	!******************************************************************

	IF EMP_LINE_COUNTER%
	THEN
		IF (EMP_SUBTOTAL% > 1%) AND (XCODE$ = "Y")
		THEN
			FOR LOOP% = 1% TO EMP_SUBTOTAL%
				TEXT$ = SPACE$(44%) + &
					"Code Subtotal  " + &
					EMP_SUBTOTAL(LOOP%)::CODE + &
					SPACE$(18%) + &
					FORMAT$(EMP_SUBTOTAL(LOOP%)::REG_HR, "#####.## ") + &
					FORMAT$(EMP_SUBTOTAL(LOOP%)::OVT_HR, "#####.## ") + &
					"      " + &
					"          " + &
					FORMAT$(EMP_SUBTOTAL(LOOP%)::PIECE, "#####.### ") + &
					FORMAT$(EMP_SUBTOTAL(LOOP%)::GROSS, "######.##")

				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			NEXT LOOP%
		END IF

		TEXT$ = SPACE$(55%) + "Employee Total          " + &
			FORMAT$(EMP_TOTAL(1%), "#####.## ") + &
			FORMAT$(EMP_TOTAL(2%), "#####.##                 ") + &
			FORMAT$(EMP_TOTAL(3%), "#####.### ") + &
			FORMAT$(EMP_TOTAL(4%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	GRAND_TOTAL(I%) = GRAND_TOTAL(I%) + EMP_TOTAL(I%) &
		FOR I% = 1% TO 4%

	SUB_TOTAL(I%) = SUB_TOTAL(I%) + EMP_TOTAL(I%) &
		FOR I% = 1% TO 4%

	EMP_LINE_COUNTER% = 0%
	EMP_SUBTOTAL%  = 0%
	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%

	RETURN

 SubTotal:
	!******************************************************************
	! Print Sub total
	!******************************************************************

	IF SUB_COUNT%
	THEN
		TEXT$ = SPACE$(55%) + "Sub Total               " + &
			FORMAT$(SUB_TOTAL(1%), "#####.## ") + &
			FORMAT$(SUB_TOTAL(2%), "#####.##                 ") + &
			FORMAT$(SUB_TOTAL(3%), "#####.### ") + &
			FORMAT$(SUB_TOTAL(4%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		IF (SORTBY$ = "LN") OR (SORTBY$ = "LO")
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)
		ELSE
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		END IF

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	SUB_TOTAL(I%) = 0.0 FOR I% = 1% TO 4%
	SUB_COUNT% = 0%

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
