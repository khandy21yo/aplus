1	%TITLE "Payroll Time/Piece Report"
	%SBTTL "PR_RPRT_TRN_TIME_02"
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
	! ID:PR029
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Time Journal Report\* option lists data
	!	relative to a specific payroll file. The primary purpose of the
	!	report is to provide a means to review and balance the data entered
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
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_TIME_02/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_TIME_02, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_TIME_02.OBJ;*
	!
	! Author:
	!
	!	05/22/89 - Kevin Handy
	!
	! Modification history:
	!
	!	05/22/89 - Kevin Handy
	!		Modified to remove units.
	!
	!	05/23/89 - Kevin Handy
	!		Fixed titles ("Location" instead of "SubAccount",
	!		"End Date" instead of "Oper").
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/02/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/13/2000 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)	PR_TRN_PAY_CDD		PR_TRN_PAY
	MAP	(PR_HIS_PAY)	PR_TRN_PAY_CDD		PR_HIS_PAY

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
	!	.x Time Journal>Payroll Date
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item _#\*
	!	.b
	!	.lm +5
	!	The ^*From Item _#\* field begins the report with
	!	a selected location or employee number, depending upon the ^*Sort\*
	!	selection. Any value in this field must be in agreement with the
	!	value in field (05). If field (05) has a value of ^*NU\*, this
	!	field must contain an employee number or be left blank.  If field (05)
	!	has a value of ^*LN\* or ^*SA\*, this field must contain a location
	!	code or be left blank.
	!	.lm -5
	!
	! Index:
	!	.x From Item Number>Time Journal Report
	!	.x Time Journal Report>From Item Number
	!	.x Item Number>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item _#\*
	!	.b
	!	.lm +5
	!	The ^*To Item _#\* field ends the
	!	report with a selected location or employee number, depending
	!	upon the ^*Sort\* selection. Any value in this field
	!	must be in agreement with the value in field (05). If field (05) has a
	!	value of ^*NU\*, this field must contain an employee number or be left blank.
	!	If field (05) has a value of ^*LN\* or ^*SA\*, this field must contain
	!	a location code or be left blank.
	!	.lm -5
	!
	! Index:
	!	.x To Item Number>Time Journal Report
	!	.x Time Journal Report>To Item Number
	!	.x Item Number>To
	!
	!--

	BATCH_ENTRY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Entry Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*Entry Batch _#\* field selects
	!	a specific batch number which was assigned
	!	in a Timekeeping routine to be printed.
	!	.b
	!	If this field is left blank, the Time Journal Report will include all
	!	batches for a specific payroll file folder.
	!	.lm -5
	!
	! Index:
	!	.x Time Journal Report>Entry Batch _#
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.ts 55
	!	^*(05) Sort	LN,NU,SA\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field selects the order
	!	in which the report will be printed.
	!	.b
	!	Valid selections are:
	!	.table 3,25
	!	.te
	!	^*LN\*	Location, Department, Number
	!	.te
	!	^*NU\*	Number
	!	.te
	!	^*SA\*	Subaccount
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
	!	The ^*Department\* field enters the department which
	!	is to be printed.
	!	.b
	!	If the field is left blank, all departments
	!	will be included.
	!	.lm -5
	!
	! Index:
	!	.x Department
	!
	!--

	SELECT SORTBY$
	CASE "NU"
		K_NUM% = 0%

	CASE "SA"
		K_NUM% = 1%

	CASE "LN"
		K_NUM% = 2%
	END SELECT

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
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		CONTINUE 315 IF ERR = 5%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	GOTO ReportTitle

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

	%PAGE

 ReportTitle:
	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Time Journal Report"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		MID(BATCH_NO$, 5%, 2%) + "/" + &
		MID(BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(BATCH_NO$, 4%)

	TITLE$(3%) = ""

	TITLE$(4%) = "                                       Default   " + &
		" End                 --------------Hour-------------  "

	TITLE$(5%) = "Loc    Ty Un Dept   Account           Location   " + &
		"Date      EC  PT  RC     Rate  Reg Hrs  Ovt Hrs Fac   " + &
		"    Gross"

	TITLE$(6%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_TRN_PAY.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_TRN_PAY.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
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
	! If history flag is set then set history into journal map
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	GOTO 17020 IF PR_TRN_PAY::PTYPE = "A"

	GOTO 17020 IF COMP_STRING(PR_TRN_PAY::DEPT, DEPT$) = 0% &
		AND DEPT$ <> ""

	GOTO 17020 IF BATCH_ENTRY$ <> "" AND &
		BATCH_ENTRY$ <> PR_TRN_PAY::BATCH_ENTRY

	GOTO 17100 &
		IF TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM + PR_TRN_PAY::PR_END_DATE

	IF TEST_EMPNUM$ <> ""
	THEN
		GOSUB EmployeeTotal
		EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%
		EMP_LINE_COUNTER% = 0%
	END IF

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "NU"
		GOTO ExitTotal IF (PR_TRN_PAY::EMPNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "SA"
		GOTO ExitTotal IF (PR_TRN_PAY::SUBACC > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOSUB SubTotal IF TEST_SUBACC$<>"" AND &
			TEST_SUBACC$ <> PR_TRN_PAY::SUBACC
		TEST_SUBACC$ = PR_TRN_PAY::SUBACC

	CASE "LN"
		GOTO ExitTotal IF (PR_TRN_PAY::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOSUB SubTotal IF TEST_LOCATION$<>"" AND &
			TEST_LOCATION$ <> PR_TRN_PAY::LOCATION
		TEST_LOCATION$ = PR_TRN_PAY::LOCATION
	END SELECT

17040	PR_EMP_MASTER::EMPNAME = "????????????????????????????????????"

	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, &
			KEY #0% EQ PR_TRN_PAY::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17050 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

17050	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		PR_TRN_PAY::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + " " + &
		PR_EMP_MASTER::LOCATION + " " + &
		PRNT_DATE(PR_TRN_PAY::PR_END_DATE, 6%), 0%)

17100	!
	! Print employee record
	!
	TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM + PR_TRN_PAY::PR_END_DATE

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
		FORMAT$(PR_TRN_PAY::GROSS, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	EMP_LINE_COUNTER% = EMP_LINE_COUNTER% + 1%

	GOTO ExitProgram IF UTL_REPORTX::STAT

	EMP_TOTAL(1%) = EMP_TOTAL(1%) + PR_TRN_PAY::REG_HR
	EMP_TOTAL(2%) = EMP_TOTAL(2%) + PR_TRN_PAY::OVT_HR
	EMP_TOTAL(3%) = EMP_TOTAL(3%) + PR_TRN_PAY::PIECE
	EMP_TOTAL(4%) = EMP_TOTAL(4%) + PR_TRN_PAY::GROSS

17350	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB EmployeeTotal

	SELECT SORTBY$
	CASE "SA", "LN"
		GOSUB SubTotal
	END SELECT

	TEXT$ = SPACE$(55%) + "Grand Total             " + &
		FORMAT$(GRAND_TOTAL(1%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#####.##       ") + &
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
		TEXT$ = SPACE$(55%) + "Employee Total          " + &
			FORMAT$(EMP_TOTAL(1%), "#####.## ") + &
			FORMAT$(EMP_TOTAL(2%), "#####.##       ") + &
			FORMAT$(EMP_TOTAL(4%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	GRAND_TOTAL(I%) = GRAND_TOTAL(I%) + EMP_TOTAL(I%) &
		FOR I% = 1% TO 4%

	SUB_TOTAL(I%) = SUB_TOTAL(I%) + EMP_TOTAL(I%) &
		FOR I% = 1% TO 4%

	RETURN

 SubTotal:
	!******************************************************************
	! Print Sub total
	!******************************************************************
	TEXT$ = SPACE$(55%) + "Sub Total               " + &
			FORMAT$(SUB_TOTAL(1%), "#####.## ") + &
			FORMAT$(SUB_TOTAL(2%), "#####.##       ") + &
			FORMAT$(SUB_TOTAL(4%), "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	IF (SORTBY$ = "LN")
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)
	ELSE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SUB_TOTAL(I%) = 0.0 FOR I% = 1% TO 4%

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
