1	%TITLE "Payroll Labor Distribution Report"
	%SBTTL "PR_RPRT_TRN_LABDIS"
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
	! Computer Management Center, Inc..
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:PR002
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Labor Distribution Report\* option prints
	!	a report listing detailed payroll costs and subtotals in the following
	!	hierarchal order:
	!	.table 3,25
	!	.te
	!	Major	Sub-account
	!	.te
	!	Intermediate	Operation
	!	.te
	!	Minor	General Ledger Account
	!	.end table
	!	The fields include the following:
	!	.table 3,25
	!	.te
	!	Sub-account
	!	.te
	!	Operation
	!	.te
	!	Account
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name
	!	.te
	!	Hourly Rate
	!	.te
	!	Regular Hours
	!	.te
	!	Overtime Hours
	!	.te
	!	Overtime Factor
	!	.te
	!	Unit Rate
	!	.te
	!	Unit Quantity
	!	.te
	!	Gross Payroll Cost
	!	.end table
	!
	! Index:
	!	.x Labor Distribution>Report
	!	.x Report>Labor Distribution
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_LABDIS/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_LABDIS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_LABDIS.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	06/19/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	07/15/94 - Kevin Handy
	!		Added Direct and Indirect grans totals.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Fix a possible bug with undefined employee #'s
	!
	!	08/14/2000 - Kevin Handy
	!		Add a "TOTALS ONLY" option (LL)
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

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Payroll Date\* field causes the printing
	!	for this particular payroll date.
	!	.b
	!	This field requires an entry.
	!	.lm -5
	!
	! Index:
	!	.x Payroll Date>Labor Distribution Report
	!	.x Labor Distribution Report>Payroll Date
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)

	!++
	! Abstract:FLD05
	!	^*(05) Entry Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*Entry Batch _#\* enters the batch number
	!	which is to be printed.
	!	.b
	!	A blank field allows for all batch numbers to be
	!	printed.
	!	.lm -5
	!
	! Index:
	!	.x Entry Batch>Labor Distribution Report
	!	.x Batch>Labor Distribution Report
	!	.x Labor Distribution Report>Entry Batch
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Sub Account _#\*
	!	.b
	!	.lm +5
	!	The ^*Sub Account _#\* field cause the
	!	report to print beginning with this particular
	!	Account _#.
	!	.b
	!	A blank in this field will cause the report
	!	to begin with the first account number in file.
	!
	! Index:
	!	.x From Sub Account>Labor Distribution Report
	!	.x Sub Account>Labor Distribution Report
	!	.x Labor Distribution Report>From Sub Account
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Sub Account _#\*
	!	.b
	!	.lm +5
	!	The ^*Sub Account _#\* field causes the report to print
	!	ending with this particular Account _#.
	!	.b
	!	A blank in this field will cause the
	!	report to end with the last number in the file.
	!
	! Index:
	!	.x To Sub Account>Labor Distribution Report
	!	.x Sub Account>Labor Distribution Report
	!	.x Labor Distribution Report>To Sub Account
	!
	!--

	PAGE_IT$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	! Abstract:FLD04
	!	.ts 55
	!	^*(04) Page After Sub Account	Y or N\*
	!	.b
	!	.lm +5
	!	The ^*Page After Sub Account\* field causes a page break
	!	to occur after printing each Sub Account.
	!	.table 3,25
	!	.te
	!	^*Y\*	Yes page break
	!	.te
	!	^*N\*	No page break
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Page After>Labor Distribution Report
	!	.x Labor Distribution Report>Page After
	!
	!--

	BATCH_ENTRY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)
	!++
	! Abstract:FLD05
	!	^*(05) Entry Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*Entry Batch _#\* enters the batch number
	!	which is to be printed.
	!	.b
	!	A blank field allows for all batch numbers to be
	!	printed.
	!	.lm -5
	!
	! Index:
	!	.x Entry Batch>Labor Distribution Report
	!	.x Batch>Labor Distribution Report
	!	.x Labor Distribution Report>Entry Batch
	!
	!--


	TOTALS_ONLY$ = LEFT$(UTL_REPORTX::OPTDEF(5%), 1%)
	!++
	! Abstract:FLD06
	!	^*(06) Totals Only\*
	!	.b
	!	.lm +5
	!	Allows for only the final totals to be printed, when the
	!	detail is not wanted.
	!	.lm -5
	!
	! Index:
	!	.x Entry Batch>Labor Distribution Report
	!	.x Batch>Labor Distribution Report
	!	.x Labor Distribution Report>Entry Batch
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
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		CONTINUE 315 IF ERR = 5%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	GOTO ReportTitle

315	!
	! Open Pay folder
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
	! Set up titles
	!
	TITLE$(1%) = "Payroll Labor Distribution Report"
	TITLE$(2%) = "For the Payroll Folder Dated:  " + &
		PRNT_DATE(BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = SPACE$(39%) + &
		SPACE$(30%) + &
		"--------------Hour-------------  -------Piece-------"
	TITLE$(5%) = "SubAcct   Oper      Account            " + &
		"EmpNum     EmpName            " + &
		"    Rate   RegHrs   OvtHrs Fac        Rate       Qty     Gross"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$SubAcct:010,$Oper:019,$Account:038,$EmpNum:049," + &
		"EmpName::068,VHourRate:077,VRegHrs:086,VOvtHrs:095," + &
		"VFactor:099,VUnitsRate:111,VUnitsQty:121,VGross:131"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	FIRST_PASS% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_TRN_PAY.CH%, KEY #1%
		ELSE
			FIND #PR_TRN_PAY.CH%, KEY #1% GE FROM_ITEM$, REGARDLESS
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
	! If history flag is set then use set history map to journal map
	!
	IF USE_HISTORY% = -1%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_TRN_PAY::SUBACC > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	GOTO 17020 IF PR_TRN_PAY::PTYPE = "A"

	GOTO 17020 IF BATCH_ENTRY$ <> "" AND &
		BATCH_ENTRY$ <> PR_TRN_PAY::BATCH_ENTRY

17040	IF TEST_EMPNUM$ <> PR_TRN_PAY::EMPNUM
	THEN
		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_TRN_PAY::EMPNUM, &
				REGARDLESS
		USE
			PR_TRN_PAY::EMPNUM = TEST_EMPNUM$
			PR_EMP_MASTER::EMPNAME = "????????????????????????????????????"

			CONTINUE 17100 IF ERR = 155%
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

	END IF

17100	TEXT$ = ""

	IF TEST_SUBACC$ <> PR_TRN_PAY::SUBACC
	THEN
		IF FIRST_PASS%
		THEN
			GOSUB AccountTotal

			GOTO ExitProgram IF UTL_REPORTX::STAT

			GOSUB OperTotal

			GOTO ExitProgram IF UTL_REPORTX::STAT

			GOSUB SubaccTotal

			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		TEST_ACCOUNT$ = PR_TRN_PAY::ACCT
		TEST_OPER$ = PR_TRN_PAY::OPER
		TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM

		TEXT$ = PR_TRN_PAY::SUBACC + " " + &
			PR_TRN_PAY::OPER + " " + &
			PR_TRN_PAY::ACCT + " " + &
			PR_TRN_PAY::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 19%) + " "

	END IF

	IF TEST_OPER$ <> PR_TRN_PAY::OPER
	THEN
		IF FIRST_PASS%
		THEN
			GOSUB AccountTotal

			GOTO ExitProgram IF UTL_REPORTX::STAT

			GOSUB OperTotal

			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		TEST_ACCOUNT$ = PR_TRN_PAY::ACCT
		TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM

		TEXT$ = SPACE$(11%) + &
			PR_TRN_PAY::OPER + " " + &
			PR_TRN_PAY::ACCT + " " + &
			PR_TRN_PAY::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 19%) + " "
	END IF

	IF TEST_ACCOUNT$ <> PR_TRN_PAY::ACCT
	THEN
		IF FIRST_PASS%
		THEN
			GOSUB AccountTotal

			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM

		TEXT$ = SPACE$(20%) + &
			PR_TRN_PAY::ACCT + " " + &
			PR_TRN_PAY::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 19%) + " "

	END IF

	IF TEST_EMPNUM$ <> PR_TRN_PAY::EMPNUM
	THEN
		TEXT$ = SPACE$(39%) + &
			PR_TRN_PAY::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 19%) + " "
	END IF

	!
	! Print employee record
	!
	TEST_SUBACC$ = PR_TRN_PAY::SUBACC
	TEST_ACCOUNT$ = PR_TRN_PAY::ACCT
	TEST_OPER$ = PR_TRN_PAY::OPER
	TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM

	TEXT$ = LEFT(TEXT$ + SPACE$(69%), 69%) + &
		FORMAT$(PR_TRN_PAY::HOUR_RATE, "####.### ") + &
		FORMAT$(PR_TRN_PAY::REG_HR, "#####.## ") + &
		FORMAT$(PR_TRN_PAY::OVT_HR, "#####.## ") + &
		FORMAT$(PR_TRN_PAY::FACTOR, "###%  ") + &
		FORMAT$(PR_TRN_PAY::PIECE_RATE, "####.#### ") + &
		FORMAT$(PR_TRN_PAY::PIECE, "#####.### ") + &
		FORMAT$(PR_TRN_PAY::GROSS, "######.##")

	IF TOTALS_ONLY$ <> "Y"
	THEN
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	TOTAL(1%) = PR_TRN_PAY::REG_HR
	TOTAL(2%) = PR_TRN_PAY::OVT_HR
	TOTAL(3%) = PR_TRN_PAY::PIECE
	TOTAL(4%) = PR_TRN_PAY::GROSS

	IF PR_TRN_PAY::SUBACC = ""
	THEN
		INDIRECT_TOTAL(I%) = INDIRECT_TOTAL(I%) + TOTAL(I%) &
			FOR I% = 1% TO 4%
	ELSE
		DIRECT_TOTAL(I%) = DIRECT_TOTAL(I%) + TOTAL(I%) &
			FOR I% = 1% TO 4%
	END IF

	FOR LOOP% = 1% TO 4%
		ACCOUNT_TOTAL(LOOP%) = ACCOUNT_TOTAL(LOOP%) + TOTAL(LOOP%)
		OPER_TOTAL(LOOP%) = OPER_TOTAL(LOOP%) + TOTAL(LOOP%)
		SUBACC_TOTAL(LOOP%) = SUBACC_TOTAL(LOOP%) + TOTAL(LOOP%)
		GRAND_TOTAL(LOOP%) = GRAND_TOTAL(LOOP%) + TOTAL(LOOP%)
	NEXT LOOP%

	!
	! Set first pass to no
	!
	FIRST_PASS% = -1%

	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	IF FIRST_PASS%
	THEN
		GOSUB AccountTotal

		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOSUB OperTotal

		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOSUB SubaccTotal

		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

	!
	! Displat direct/indirect subtotals
	!
	TEXT$ = SPACE$(65%) + "Indirect Total"
	TEXT$ = LEFT(TEXT$ + SPACE$(78%), 78%) + &
		FORMAT$(INDIRECT_TOTAL(1%), "#####.## ") + &
		FORMAT$(INDIRECT_TOTAL(2%), "#####.##                 ") + &
		FORMAT$(INDIRECT_TOTAL(3%), "#####.### ") + &
		FORMAT$(INDIRECT_TOTAL(4%), "######.##*")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(65%) + "Direct Total"
	TEXT$ = LEFT(TEXT$ + SPACE$(78%), 78%) + &
		FORMAT$(DIRECT_TOTAL(1%), "#####.## ") + &
		FORMAT$(DIRECT_TOTAL(2%), "#####.##                 ") + &
		FORMAT$(DIRECT_TOTAL(3%), "#####.### ") + &
		FORMAT$(DIRECT_TOTAL(4%), "######.##*")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(65%) + "Grand Total"
	TEXT$ = LEFT(TEXT$ + SPACE$(78%), 78%) + &
		FORMAT$(GRAND_TOTAL(1%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#####.##                 ") + &
		FORMAT$(GRAND_TOTAL(3%), "#####.### ") + &
		FORMAT$(GRAND_TOTAL(4%), "######.##*")

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

 AccountTotal:
	!******************************************************************
	! Print Employee total
	!******************************************************************
	TEXT$ = SPACE$(50%) + "Account Total"

	TEXT$ = LEFT(TEXT$ + SPACE$(78%), 78%) + &
		FORMAT$(ACCOUNT_TOTAL(1%), "#####.## ") + &
		FORMAT$(ACCOUNT_TOTAL(2%), "#####.##                 ") + &
		FORMAT$(ACCOUNT_TOTAL(3%), "#####.### ") + &
		FORMAT$(ACCOUNT_TOTAL(4%), "######.##*")

	IF TOTALS_ONLY$ <> "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	ACCOUNT_TOTAL(I%) = 0.0 FOR I% = 1% TO 4%

	RETURN

 OperTotal:
	!******************************************************************
	! Print Employee total
	!******************************************************************
	TEXT$ = SPACE$(55%) + "Operation Total"

	TEXT$ = LEFT(TEXT$ + SPACE$(78%), 78%) + &
		FORMAT$(OPER_TOTAL(1%), "#####.## ") + &
		FORMAT$(OPER_TOTAL(2%), "#####.##                 ") + &
		FORMAT$(OPER_TOTAL(3%), "#####.### ") + &
		FORMAT$(OPER_TOTAL(4%), "######.##*")

	IF TOTALS_ONLY$ <> "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	OPER_TOTAL(I%) = 0.0 FOR I% = 1% TO 4%

	RETURN

 SubaccTotal:
	!******************************************************************
	! Print Employee total
	!******************************************************************
	TEXT$ = SPACE$(60%) + "Sub Account Total"

	TEXT$ = LEFT(TEXT$ + SPACE$(78%), 78%) + &
		FORMAT$(SUBACC_TOTAL(1%), "#####.## ") + &
		FORMAT$(SUBACC_TOTAL(2%), "#####.##                 ") + &
		FORMAT$(SUBACC_TOTAL(3%), "#####.### ") + &
		FORMAT$(SUBACC_TOTAL(4%), "######.##*")

	IF TOTALS_ONLY$ <> "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		IF PAGE_IT$ = "Y"
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)
		END IF
	END IF

	SUBACC_TOTAL(I%) = 0.0 FOR I% = 1% TO 4%

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
