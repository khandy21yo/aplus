1	%TITLE "Payroll Subaccount Audit Report"
	%SBTTL "PR_RPRT_AUDT_SUBACC"
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
	! ID:PR051
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Subaccount Audit Report\* prints a listing of each Subaccout with the
	!	employees and their time worked for each account.
	!	.table 3,25
	!	.te
	!	Subaccount
	!	.te
	!	Operation
	!	.te
	!	Account
	!	.te
	!	Employee Number
	!	.te
	!	Name
	!	.te
	!	Regular Time
	!	.te
	!	Overtime
	!	.te
	!	Pieces
	!	.te
	!	Gross
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>SubAccount Audit
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_AUDT_SUBACC/LINE/NOOPT
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_AUDT_SUBACC, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_AUDT_SUBACC.OBJ;*
	!
	! Author:
	!
	!	03/27/88 - Robert Peterson
	!
	! Modification history:
	!
	!	06/23/89 - Kevin Handy
	!		Opened PR_TEMP as a TEMPORARY file instead of
	!		trying to remember to kill it when done.
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to a spreadsheet or a DIF file.
	!
	!	06/03/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" in pay file.
	!
	!	08/10/93 - Kevin Handy
	!		Increased dimension for file names.
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Removed unsolicited_input stuff.
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/20/2000 - Kevin Handy
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	MAP	(PR_TEMP)	PR_TEMP.SUBACC$ = 10%, &
				PR_TEMP.OPER$ = 8%, &
				PR_TEMP.ACCT$ = 18%, &
				PR_TEMP.EMPNUM$ = 10%, &
				PR_TEMP.PR_END_DATE$ = 08%, &
				PR_TEMP.PTYPE$ = 01%, &
				PR_TEMP.RTYPE$ = 01%, &
				PR_TEMP.CODE$ = 02%, &
				PR_TEMP.HOUR_RATE, &
				PR_TEMP.REG_HR, &
				PR_TEMP.OVT_HR, &
				PR_TEMP.PIECE, &
				PR_TEMP.FACTOR%, &
				PR_TEMP.GROSS

	!
	! Dimension
	!
	DIM DATA_FILE$(2000%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) From Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*From Payroll Date\* field specifies the
	!	first folder date included in this report.
	!	.b
	!	A blank field causes the report to begin with the
	!	first date in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)
	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	.ts 55
	!	^*(02) To Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*To Payroll Date\* field specifies the
	!	ending folder date included in this report.
	!	.b
	!	A blank field causes the report to end with the last
	!	payroll date in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	! Abstract:FLD03
	!	^*(03) From Subaccount\*
	!	.b
	!	.lm +5
	!	The ^*From Subaccount\* field specifies the subaccount with
	!	which the report will begin printing.
	!	.b
	!	A blank field causes the report to
	!	begin with the first subaccount in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Subaccount
	!
	!--


	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	! Abstract:FLD04
	!	^*(04) To Subaccount\*
	!	.b
	!	.lm +5
	!	The ^*To Subaccount\* field specifies the subaccount with
	!	which the report will end printing.
	!	.b
	!	A blank field causes the report to end
	!	with the last subaccount in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Subaccount
	!
	!--

	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)
	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating work file.  Reading Pay folder.", 1%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY ( &
				PR_TEMP.SUBACC$, &
				PR_TEMP.OPER$, &
				PR_TEMP.ACCT$, &
				PR_TEMP.EMPNUM$, &
				PR_TEMP.PR_END_DATE$) DUPLICATES, &
			ACCESS MODIFY, &
			ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Open all payroll folder files
	!
	FOR PR_LOOP% = 1% TO DATA_FILE%
		BATCH_NO$ = DATA_FILE$(PR_LOOP%)

320		!
		! Open Pay folder
		!
		USE_HISTORY% = 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 325 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%

		GOTO 330

325		!
		! Open pay history folder if journal not there
		!
		USE_HISTORY% = -1%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%

330		!
		! Add to audit file
		!
		WHEN ERROR IN
			RESET #PR_TMP_PAY.CH%, KEY #0%
		USE
			CONTINUE 350
		END WHEN

340		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			CONTINUE 350 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		!
		! If journal net there the set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_PAY = PR_HIS_PAY
		END IF

		GOTO 340 IF PR_TRN_PAY::PTYPE = "A"

		PR_TEMP.SUBACC$	= PR_TRN_PAY::SUBACC
		PR_TEMP.OPER$	= PR_TRN_PAY::OPER
		PR_TEMP.ACCT$	= PR_TRN_PAY::ACCT
		PR_TEMP.EMPNUM$	= PR_TRN_PAY::EMPNUM
		PR_TEMP.PR_END_DATE$	= PR_TRN_PAY::PR_END_DATE
		PR_TEMP.PTYPE$	= PR_TRN_PAY::PTYPE
		PR_TEMP.RTYPE$	= PR_TRN_PAY::RTYPE
		PR_TEMP.CODE$	= PR_TRN_PAY::CODE
		PR_TEMP.HOUR_RATE	= PR_TRN_PAY::HOUR_RATE
		PR_TEMP.REG_HR	= PR_TRN_PAY::REG_HR
		PR_TEMP.OVT_HR	= PR_TRN_PAY::OVT_HR
		PR_TEMP.PIECE	= PR_TRN_PAY::PIECE
		PR_TEMP.FACTOR%	= PR_TRN_PAY::FACTOR
		PR_TEMP.GROSS	= PR_TRN_PAY::GROSS

		PUT #PR_TEMP.CH%

		GOTO 340

350		CLOSE PR_TMP_PAY.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		CALL ASSG_FREECHANNEL(PR_HIS_PAY.CH%)
	!
	! Get next payroll
	!
	NEXT PR_LOOP%

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Sub Account Audit Report"
	TITLE$(2%) = "For the Payroll Folders Dated From: " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + " To: " + &
		PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Set headings
	!
	TITLE$(4%) = "SubAcct     Oper      Account             " + &
		"Emp #       Name                      Reg Time      " + &
		"Overtime          Pieces         Gross"
	TITLE$(5%) = ""

	!
	! Define line layouts
	!
	LYT_LINE$ = "$SubAcct:010,$Oper:020,$Acct:040,$EmpNum:052," + &
		"$EmpName:074,VRegTime:088,VOvrTime:102,VPieces:118,VGross:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	IF FROM_ITEM$ = ""
	THEN
		RESET #PR_TEMP.CH%, KEY #0%
	ELSE
		FIND #PR_TEMP.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
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
		GET #PR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_TEMP.SUBACC$ > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO 17060 IF PR_TEMP.EMPNUM$ = TEST_EMPNUM$ AND &
		PR_TEMP.ACCT$ = TEST_ACCT$ AND PR_TEMP.OPER$ = TEST_OPER$ AND &
		PR_TEMP.SUBACC$ = TEST_SUBACC$ OR PASS_1% = 0% &

	GOSUB EmpTotal

	GOTO ExitProgram IF UTL_REPORTX::STAT

17060	IF PR_TEMP.ACCT$ <> TEST_ACCT$ OR PR_TEMP.OPER$ <> TEST_OPER$ OR &
		PR_TEMP.SUBACC$ <> TEST_SUBACC$
	THEN
		IF PASS_1%
		THEN
			GOSUB AcctTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		WORK_ACCT$ = PR_TEMP.ACCT$
	END IF

	IF PR_TEMP.OPER$ <> TEST_OPER$ OR PR_TEMP.SUBACC$ <> TEST_SUBACC$
	THEN
		IF PASS_1%
		THEN
			GOSUB OperTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		WORK_OPER$ = PR_TEMP.OPER$
	END IF

	IF PR_TEMP.SUBACC$ <> TEST_SUBACC$
	THEN
		IF PASS_1%
		THEN
			GOSUB SubAccTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		WORK_SUBACC$ = PR_TEMP.SUBACC$
	END IF

	IF PRINT_LINE%
	THEN
		!
		! Print grand total
		!
		TEXT$ = SPACE$(75%) + &
			"------------- ------------- --------------- " + &
			"-------------"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	PRINT_LINE% = 0%

	!
	! Set test values
	!
	TEST_SUBACC$ = PR_TEMP.SUBACC$
	TEST_OPER$ = PR_TEMP.OPER$
	TEST_ACCT$ = PR_TEMP.ACCT$
	TEST_EMPNUM$ = PR_TEMP.EMPNUM$

	PASS_1% = -1%

	EMP_TOTAL(1%) = EMP_TOTAL(1%) + PR_TEMP.REG_HR
	EMP_TOTAL(2%) = EMP_TOTAL(2%) + PR_TEMP.OVT_HR
	EMP_TOTAL(3%) = EMP_TOTAL(3%) + PR_TEMP.PIECE
	EMP_TOTAL(4%) = EMP_TOTAL(4%) + PR_TEMP.GROSS

	!
	! Go to next record
	!
	GOTO 17020


 ExitTotal:
	!
	! Handle end of report
	!
	IF PASS_1%
	THEN
		GOSUB EmpTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	GOSUB AcctTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB OperTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB SubAccTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print grand total
	!
	TEXT$ = SPACE$(75%) + &
		"------------- ------------- --------------- " + &
		"-------------"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = SPACE$(54%) + "      Grand Total    " + &
		FORMAT$(GRAND_TOTAL(1%), "##,###,###.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "##,###,###.##  ") + &
		FORMAT$(GRAND_TOTAL(3%), "##,###,###.### ") + &
		FORMAT$(GRAND_TOTAL(4%), "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(75%) + &
		"------------- ------------- --------------- " + &
		"-------------"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

 ExitProgram:
17500	!
	! Kill temp file
	!
	CLOSE PR_TEMP.CH%

17510	CALL OUTP_FINISH(UTL_REPORTX)

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

 EmpTotal:
17700	!******************************************************************
	! Print employee total
	!******************************************************************
	!
	! Look up employee name
	!
	WORK_EMPNAME$ = STRING$(LEN(PR_EMP_MASTER::EMPNAME), 63%)
	WORK_EMPNUM$ = TEST_EMPNUM$

	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, KEY #0% GE WORK_EMPNUM$, REGARDLESS
	USE
		CONTINUE 17710 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	WORK_EMPNAME$ = PR_EMP_MASTER::EMPNAME

17710	SUM = 0.0
	SUM = FUNC_ROUND(SUM + EMP_TOTAL(LOOP%), 2%) FOR LOOP% = 1% TO 4%

	IF SUM <> 0.0
	THEN
		TEXT$ = LEFT(WORK_SUBACC$ + SPACE$(10%), 10%) + "  " + &
			LEFT(WORK_OPER$ + SPACE$(8%), 8%) + "  " + &
			LEFT(WORK_ACCT$ + SPACE$(18%), 18%) + "  " + &
			LEFT(WORK_EMPNUM$ + SPACE$(10%), 10%) + "  " + &
			LEFT(WORK_EMPNAME$ + SPACE$(20%), 20%) + " " + &
			FORMAT$(EMP_TOTAL(1%), "##,###,###.## ") + &
			FORMAT$(EMP_TOTAL(2%), "##,###,###.##  ") + &
			FORMAT$(EMP_TOTAL(3%), "##,###,###.### ") + &
			FORMAT$(EMP_TOTAL(4%), "##,###,###.##")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		WORK_SUBACC$, WORK_OPER$, WORK_ACCT$, &
			WORK_EMPNUM$, WORK_EMPNAME$ = ""

	END IF

	ACCT_TOTAL(LOOP%) = ACCT_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 4%

	EMP_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 4%

	RETURN

	%Page

 AcctTotal:
	!******************************************************************
	! Print Total for Account
	!******************************************************************
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + ACCT_TOTAL(LOOP%), 2%) FOR LOOP% = 1% TO 4%

	IF SUM <> 0.0
	THEN
		TEXT$ = SPACE$(75%) + &
			"------------- ------------- --------------- " + &
			"-------------"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		TEXT$ = SPACE$(54%) + "Acct Total           " + &
			FORMAT$(ACCT_TOTAL(1%), "##,###,###.## ") + &
			FORMAT$(ACCT_TOTAL(2%), "##,###,###.##  ") + &
			FORMAT$(ACCT_TOTAL(3%), "##,###,###.### ") + &
			FORMAT$(ACCT_TOTAL(4%), "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		PRINT_LINE% = -1%
	END IF

	OPER_TOTAL(LOOP%) = OPER_TOTAL(LOOP%) + ACCT_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 4%

	ACCT_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 4%

	RETURN

	%Page

 OperTotal:
	!******************************************************************
	! Print Total for Operation
	!******************************************************************
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + OPER_TOTAL(LOOP%), 2%) FOR LOOP% = 1% TO 4%

	IF SUM <> 0.0
	THEN
		TEXT$ = SPACE$(75%) + &
			"------------- ------------- --------------- " + &
			"-------------"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		TEXT$ = SPACE$(54%) + "  Oper Total         " + &
			FORMAT$(OPER_TOTAL(1%), "##,###,###.## ") + &
			FORMAT$(OPER_TOTAL(2%), "##,###,###.##  ") + &
			FORMAT$(OPER_TOTAL(3%), "##,###,###.### ") + &
			FORMAT$(OPER_TOTAL(4%), "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		PRINT_LINE% = -1%
	END IF

	SUBACC_TOTAL(LOOP%) = SUBACC_TOTAL(LOOP%) + OPER_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 4%

	OPER_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 4%

	RETURN

	%Page

 SubAccTotal:
	!******************************************************************
	! Print Total for SubAcc
	!******************************************************************
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + SUBACC_TOTAL(LOOP%), 2%) FOR LOOP% = 1% TO 4%

	IF SUM <> 0.0
	THEN
		TEXT$ = SPACE$(75%) + &
			"------------- ------------- --------------- " + &
			"-------------"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		TEXT$ = SPACE$(54%) + "    SubAcc Total     " + &
			FORMAT$(SUBACC_TOTAL(1%), "##,###,###.## ") + &
			FORMAT$(SUBACC_TOTAL(2%), "##,###,###.##  ") + &
			FORMAT$(SUBACC_TOTAL(3%), "##,###,###.### ") + &
			FORMAT$(SUBACC_TOTAL(4%), "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		PRINT_LINE% = -1%
	END IF

	GRAND_TOTAL(LOOP%) = GRAND_TOTAL(LOOP%) + SUBACC_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 4%

	SUBACC_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 4%

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
