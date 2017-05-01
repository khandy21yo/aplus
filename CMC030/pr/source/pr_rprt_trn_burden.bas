1	%TITLE "Payroll Labor/Burden Distribution Report"
	%SBTTL "PR_RPRT_TRN_BURDEN"
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
	! ID:PR003
	!
	! Abstract:HELP
	!	.p
	!	The ^*Burden Distribution\* report option prints
	!	a report listing summarized payroll costs and subtotals along with
	!	related burden application amounts in the following hierarchal order:
	!	.b
	!	.lm +15
	!	.list 0,"*"
	!	.le
	!	Major#########- Sub-account
	!	.le
	!	Intermediate##- Operation
	!	.le
	!	Minor#########- General Ledger Account
	!	.els
	!	.lm -15
	!	.p
	!	The fields include the following:
	!	.b
	!	.lm +15
	!	.list 0,"*"
	!	.le
	!	Sub-account
	!	.le
	!	Operation
	!	.le
	!	Account
	!	.le
	!	Hours
	!	.le
	!	Regular Earnings
	!	.le
	!	Premium Earnings
	!	.le
	!	Units
	!	.le
	!	Gross (Direct Labor Costs)
	!	.le
	!	Overhead Applied
	!	.le
	!	Workmens Compensation/Liability
	!	Insurance Costs Applied
	!	.le
	!	Union/Pension Costs Applied
	!	.le
	!	Total Burden Applied
	!	.els
	!	.lm -15
	!
	! Index:
	!	.x Report>Burden Distribution
	!	.x Burden Distribution>Report
	!	.x Burden Distribution Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_BURDEN/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_BURDEN, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_BURDEN.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	01/25/90 - Kevin Handy
	!		Changed FACTOR in calculation of SUBJECT_PREM
	!		to PR_TRN_PAY::FACTOR.
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent either to a spreadsheet or to a DIF file.
	!
	!	10/25/90 - Kevin Handy
	!		Added pay type "X" (eXcess).
	!
	!	01/09/91 - Kevin Handy
	!		Removed PR_WC_DEFINITION file.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.  Removed junk code.
	!
	!	07/14/91 - Kevin Handy
	!		Modified to use new form of PR_READ_BURDEN.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	10/13/92 - Kevin Handy
	!		Modified calculation of SUBJECT_PREM to use
	!		(FACTOR - 1) instead of (1 - FACTOR).
	!
	!	04/21/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/24/94 - Kevin Handy
	!		Added summary by account for those items with
	!		a sub-account, and those without.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
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
	!		Fix possible undefined employee bug
	!
	!	08/14/2000 - Kevin Handy
	!		Add TOTALS ONLY options.
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP (PR_TAX_PKG)	PR_TAX_PKG_CDD	PR_TAX_PKG

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DEF.HB"
	MAP (PR_OVERHEAD_DEF)	PR_OVERHEAD_DEF_CDD	PR_OVERHEAD_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DESC.HB"
	MAP (PR_OVERHEAD_DESC)	PR_OVERHEAD_DESC_CDD	PR_OVERHEAD_DESC

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.HB"
	MAP (PR_WC_DESCR)	PR_WC_DESCR_CDD	PR_WC_DESCR

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.HB"
	MAP (PR_WC_INSURANCE)	PR_WC_INSURANCE_CDD	PR_WC_INSURANCE

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	RECORD ACT_TOTAL_STRUCT
		STRING	ACCOUNT = 18%
		REAL	TOTAL(9%)
	END RECORD

	DIM ACT_TOTAL_STRUCT NSB_TOTAL(50%)	! No sub-account total
	DIM ACT_TOTAL_STRUCT SBA_TOTAL(50%)	! Subaccount Total

	!
	! Dimension arrays
	!
	DIM UP_BURDEN(30%), UP_LIA$(30%)

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
	!	The ^*Payroll Date\* field enters the date of
	!	the payroll which is to be printed.
	!	.b
	!	This field requires an entry.
	!	.lm -5
	!
	! Index:
	!	.X Payroll Date>Burden Distribution Report
	!	.x Burden Distribution Report>Payroll Date
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Sub Account _#\*
	!	.b
	!	.lm +5
	!	The ^*Sub Account _#\* field causes the
	!	report to print beginning with this particular
	!	Account _#.
	!	.b
	!	A blank in this field will cause the report to
	!	begin with the first account number in file.
	!	.lm -5
	!
	! Index:
	!	.x From Sub Account>Burden Distribution Report
	!	.x Sub Account>Burden Distribution Report
	!	.x Burden Distribution Report>From Sub Account
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Sub Account _#\*
	!	.b
	!	.lm +5
	!	The ^*Sub Account _#\* field causes the
	!	report to end with this particular Account
	!	_#.
	!	.b
	!	A blank in this field will cause the report to end with the
	!	last number in file.
	!	.lm -5
	!
	! Index:
	!	.x To Sub Account>Burden Distribution Report
	!	.x Sub Account>Burden Distribution Report
	!	.x Burden Distribution Report>To Sub Account
	!
	!--

	BATCH_ENTRY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Entry Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*Entry Batch _#\* field enters
	!	the batch number which is to be printed.
	!	.b
	!	A blank field allows all batch
	!	numbers to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Entry Batch>Burden Distribution Report
	!	.x Burden Distribution Report>Entry Batch
	!
	!--

	SUMMARY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)
	SUMMARY$ = "Y" IF SUMMARY$ <> "N" AND SUMMARY$ <> "O"

	!++
	! Abstract:FLD05
	!	^*(05) Summary Y/N\*
	!	.b
	!	.lm +5
	!	The ^*Summary Y/N\* field decides if the summary figures
	!	are to be printed. A ^*Y\* entry would allow the summary to be printed,
	!	while a ^*N\* entry would not.
	!	.lm -5
	!
	! Index:
	!	.x Summary>Burden Distribution Report
	!	.x Burden Distribution Report>Summary
	!
	!--

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		CONTINUE 305 IF ERR = 5%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	GOTO 310

305	!
	! Open history if journal file is not found
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

310	!
	! Open Employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

320	!
	! Open ERNDED Definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Tax package
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

340	!
	! Open the overhead description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DESC.OPN"
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "PR_OVERHEAD_DESC"
		CONTINUE HelpError
	END WHEN

350	!
	! Open the overhead definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DEF.OPN"
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "PR_OVERHEAD_DEF"
		CONTINUE HelpError
	END WHEN

360	!
	! Open the WC description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.OPN"
	USE
		CONTINUE 380 IF ERR = 5%
		FILENAME$ = "PR_WC_DESCR"
		CONTINUE HelpError
	END WHEN

380	!
	! Open the WC insurance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PR_WC_INSURANCE"
		CONTINUE HelpError
	END WHEN

	%Page

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Labor/Burden Distribution Report"
	TITLE$(2%) = "For the Payroll Folder Dated:  " + &
		PRNT_DATE(BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "                                                 " + &
		"-------------------Pay------------------ | " + &
		"-----------------Burden-----------------"
	TITLE$(5%) = "SubAcct    Oper     Account                Hours " + &
		"      Reg      Prem      Unit      Gross | " + &
		"      O/H     WC/LI   Unn/Pen      Total"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$SubAcct:010,$Oper:019,$EmpNum:030,$EmpName:039," + &
		"VRegPay:048,VHours:058,VPrem:068,VUnits:078,VGross:089," + &
		"VBurdenO/H:101,VBurdenWC/LI:111,VBurdenUnn/Pen:121,VTotal:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	FIRST_SUBACCT%, FIRST_OPER%, FIRST_ACCT% = 0%

	SBA_TOTAL% = 0%
	NSB_TOTAL% = 0%

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
	! Set history map to journal map is history flag is set
	!
	IF USE_HISTORY%
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

	IF TEST_SUBACC$ <> PR_TRN_PAY::SUBACC
	THEN
		IF FIRST_SUBACCT%
		THEN
			GOSUB AccountTotal

			GOTO ExitProgram IF UTL_REPORTX::STAT

			GOSUB OperTotal

			GOTO ExitProgram IF UTL_REPORTX::STAT

			GOSUB SubaccTotal

			GOTO ExitProgram IF UTL_REPORTX::STAT

			TEST_ACCOUNT$ = PR_TRN_PAY::ACCT
			TEST_OPER$ = PR_TRN_PAY::OPER
		END IF

		TEXT$ = PR_TRN_PAY::SUBACC + " " + &
			PR_TRN_PAY::OPER + " " + &
			PR_TRN_PAY::ACCT + " "

	END IF

	IF TEST_OPER$ <> PR_TRN_PAY::OPER
	THEN
		IF FIRST_OPER%
		THEN
			GOSUB AccountTotal

			GOTO ExitProgram IF UTL_REPORTX::STAT

			GOSUB OperTotal

			GOTO ExitProgram IF UTL_REPORTX::STAT

			TEST_ACCOUNT$ = PR_TRN_PAY::ACCT
		END IF

		TEXT$ = LEFT(TEXT$ + SPACE$(11%), 11%) + &
			PR_TRN_PAY::OPER + " " + &
			PR_TRN_PAY::ACCT + " "

	END IF

	IF TEST_ACCOUNT$ <> PR_TRN_PAY::ACCT
	THEN
		IF FIRST_ACCT%
		THEN
			GOSUB AccountTotal

			GOTO ExitProgram IF UTL_REPORTX::STAT

		END IF

		TEXT$ = LEFT(TEXT$ + SPACE$(20%), 20%) + &
			PR_TRN_PAY::ACCT + " "

	END IF

17030	IF TEST_EMPNUM$ <> PR_TRN_PAY::EMPNUM
	THEN
		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_TRN_PAY::EMPNUM, &
				REGARDLESS
		USE
			PR_TRN_PAY::EMPNUM = TEST_EMPNUM$
			PR_EMP_MASTER::EMPNAME	= &
				STRING$(LEN(PR_EMP_MASTER::EMPNAME), 68%)
			PR_EMP_MASTER::WC	= ""

			CONTINUE 17100 IF ERR = 155%
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN
	END IF

17100	TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM

	SUBJECT_PREM = 0.0

	IF PR_TRN_PAY::RTYPE = "H" OR PR_TRN_PAY::RTYPE = "S" OR &
		PR_TRN_PAY::RTYPE = "X"
	THEN
		SUBJECT_PREM = FUNC_ROUND(PR_TRN_PAY::OVT_HR * ( &
			((PR_TRN_PAY::FACTOR / 100.0) - 1.0) * &
			PR_TRN_PAY::HOUR_RATE), 2%)
		SUBJECT_PREM = 0.0 IF SUBJECT_PREM < 0.0
	END IF

	CALL PR_READ_BURDEN(PR_OVERHEAD_DEF.CH%, &
		PR_OVERHEAD_DESC.CH%, &
		PR_WC_DESCR.CH%, &
		PR_WC_INSURANCE.CH%, &
		PR_TAX_PKG.CH%, &
		PR_ERNDED_DEF.CH%, &
		PR_TRN_PAY::ACCT, &
		PR_TRN_PAY::OPER, &
		PR_TRN_PAY::CODE, &
		PR_TRN_PAY::TAX_PKG, &
		BATCH_NO$, &
		PR_TRN_PAY::GROSS, &
		PR_TRN_PAY::OVT_HR + PR_TRN_PAY::REG_HR, &
		SUBJECT_PREM, &
		PR_EMP_MASTER::WC, &
		WC_BURDEN, &
		WC_EXP$, &
		WC_LIA$, &
		OH_BURDEN, &
		OH_EXP$, &
		OH_APP$, &
		UP_BURDEN(), &
		UP_EXP$, &
		UP_LIA$(), &
		PREM_ACC$, &
		PREM_AMT)

	!
	! Print employee record
	!
	TEST_SUBACC$ = PR_TRN_PAY::SUBACC
	TEST_ACCOUNT$ = PR_TRN_PAY::ACCT
	TEST_OPER$ = PR_TRN_PAY::OPER

	FIRST_SUBACCT%, FIRST_OPER%, FIRST_ACCT% = -1%

	SUBJECT_PREM = FUNC_ROUND(SUBJECT_PREM, 2%)
	PREM_AMT = FUNC_ROUND(PREM_AMT, 2%)

	EMP(I%) = 0.0 FOR I% = 1% TO 9%

	EMP(1%) = FUNC_ROUND(PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR, 2%)

	IF PR_TRN_PAY::RTYPE = "S" OR PR_TRN_PAY::RTYPE = "H" OR &
		PR_TRN_PAY::RTYPE = "X"
	THEN
		EMP(2%) = PR_TRN_PAY::GROSS - SUBJECT_PREM
		IF PREM_AMT = 0.0
		THEN
			EMP(3%) = SUBJECT_PREM
			EMP(5%) = PR_TRN_PAY::GROSS
		ELSE
			EMP(3%) = FUNC_ROUND(SUBJECT_PREM - PREM_AMT, 2%)
			PREM_TOTAL(3%)= PREM_TOTAL(3%) + PREM_AMT
			PREM_TOTAL(5%)= PREM_TOTAL(5%) + PREM_AMT
			EMP(5%) = FUNC_ROUND(PR_TRN_PAY::GROSS - PREM_AMT, 2%)
		END IF
	ELSE
		EMP(4%) = PR_TRN_PAY::GROSS
		EMP(5%) = PR_TRN_PAY::GROSS
	END IF


	EMP(6%) = FUNC_ROUND(OH_BURDEN, 2%)
	EMP(7%) = FUNC_ROUND(WC_BURDEN, 2%)
	EMP(8%) = EMP(8%) + FUNC_ROUND(UP_BURDEN(LOOP%), 2%) &
		FOR LOOP% = 1% TO 10%
	EMP(9%) = FUNC_ROUND(EMP(9%) + EMP(LOOP%), 2%) &
		FOR LOOP% = 6% TO 8%

	IF SUMMARY$ = "N"
	THEN
		TEXT1$ = LEFT(SPACE$(20%) + PR_TRN_PAY::EMPNUM + " " + &
			PR_EMP_MASTER::EMPNAME, 39%) + &
			FORMAT$(EMP(1%), "######.## ") + &
			FORMAT$(EMP(2%), "######.## ") + &
			FORMAT$(EMP(3%), "######.## ") + &
			FORMAT$(EMP(4%), "######.## ") + &
			FORMAT$(EMP(5%), "#######.## | ") + &
			FORMAT$(EMP(6%), "######.## ") + &
			FORMAT$(EMP(7%), "######.## ") + &
			FORMAT$(EMP(8%), "######.## ") + &
			FORMAT$(EMP(9%), "#######.##")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT1$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Summarize this employee
	!
	ACCOUNT_TOTAL(LOOP%) = ACCOUNT_TOTAL(LOOP%) + EMP(LOOP%) &
		FOR LOOP% = 1% TO 9%

	!
	! Summarize by account
	!
	IF PR_TRN_PAY::SUBACC = ""
	THEN
		ITM% = 0%
		ITM% = LOOP% IF NSB_TOTAL(LOOP%)::ACCOUNT = PR_TRN_PAY::ACCT &
			FOR LOOP% = 1% TO NSB_TOTAL%
		IF ITM% = 0%
		THEN
			NSB_TOTAL%, ITM% = NSB_TOTAL% + 1%
			NSB_TOTAL(ITM%)::ACCOUNT = PR_TRN_PAY::ACCT
			NSB_TOTAL(ITM%)::TOTAL(LOOP%) = EMP(LOOP%) &
				FOR LOOP% = 1% TO 9%
		ELSE
			NSB_TOTAL(ITM%)::TOTAL(LOOP%) = &
				NSB_TOTAL(ITM%)::TOTAL(LOOP%) + &
				EMP(LOOP%) &
				FOR LOOP% = 1% TO 9%
		END IF
	ELSE
		ITM% = 0%
		ITM% = LOOP% IF SBA_TOTAL(LOOP%)::ACCOUNT = PR_TRN_PAY::ACCT &
			FOR LOOP% = 1% TO SBA_TOTAL%
		IF ITM% = 0%
		THEN
			SBA_TOTAL%, ITM% = SBA_TOTAL% + 1%
			SBA_TOTAL(ITM%)::ACCOUNT = PR_TRN_PAY::ACCT
			SBA_TOTAL(ITM%)::TOTAL(LOOP%) = EMP(LOOP%) &
				FOR LOOP% = 1% TO 9%
		ELSE
			SBA_TOTAL(ITM%)::TOTAL(LOOP%) = &
				SBA_TOTAL(ITM%)::TOTAL(LOOP%) + &
				EMP(LOOP%) &
				FOR LOOP% = 1% TO 9%
		END IF
	END IF

	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	IF FIRST_SUBACCT%
	THEN
		GOSUB AccountTotal

		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOSUB OperTotal

		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOSUB SubaccTotal

		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

	!
	! Is there premium pay that was not include in burden distribution
	!
	IF PREM_TOTAL(3%) <> 0.0
	THEN
		TEXT$ = "Overtime Premium Not Included in burden"

		TEXT$ = LEFT(TEXT$ + SPACE$(39%), 39%) + &
			FORMAT$(PREM_TOTAL(1%), "######.## ") + &
			FORMAT$(PREM_TOTAL(2%), "######.## ") + &
			FORMAT$(PREM_TOTAL(3%), "######.## ") + &
			FORMAT$(PREM_TOTAL(4%), "######.## ") + &
			FORMAT$(PREM_TOTAL(5%), "#######.## | ") + &
			FORMAT$(PREM_TOTAL(6%), "######.## ") + &
			FORMAT$(PREM_TOTAL(7%), "######.## ") + &
			FORMAT$(PREM_TOTAL(8%), "######.## ") + &
			FORMAT$(PREM_TOTAL(9%), "#######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GRAND_TOTAL(LOOP%) = GRAND_TOTAL(LOOP%) + PREM_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 9%
	END IF


	TEXT$ = SPACE$(15%) + "Grand Total"

	TEXT$ = LEFT(TEXT$ + SPACE$(39%), 39%) + &
		FORMAT$(GRAND_TOTAL(1%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(3%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(4%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(5%), "#######.## | ") + &
		FORMAT$(GRAND_TOTAL(6%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(7%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(8%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(9%), "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	!
	! Output Non-account totals
	!
	IF NSB_TOTAL% <> 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 5%)

		TEXT$ = "Total for Non-SubAccount"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		EMP(LOOP%) = 0.0 FOR LOOP% = 1% TO 9%

		FOR I% = 1% TO NSB_TOTAL%
			TEXT$ = SPACE$(15%) + NSB_TOTAL(I%)::ACCOUNT

			TEXT$ = LEFT(TEXT$ + SPACE$(39%), 39%) + &
				FORMAT$(NSB_TOTAL(I%)::TOTAL(1%), &
				"######.## ") + &
				FORMAT$(NSB_TOTAL(I%)::TOTAL(2%), &
				"######.## ") + &
				FORMAT$(NSB_TOTAL(I%)::TOTAL(3%), &
				"######.## ") + &
				FORMAT$(NSB_TOTAL(I%)::TOTAL(4%), &
				"######.## ") + &
				FORMAT$(NSB_TOTAL(I%)::TOTAL(5%), &
				"#######.## | ") + &
				FORMAT$(NSB_TOTAL(I%)::TOTAL(6%), &
				"######.## ") + &
				FORMAT$(NSB_TOTAL(I%)::TOTAL(7%), &
				"######.## ") + &
				FORMAT$(NSB_TOTAL(I%)::TOTAL(8%), &
				"######.## ") + &
				FORMAT$(NSB_TOTAL(I%)::TOTAL(9%), "#######.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			EMP(LOOP%) = EMP(LOOP%) + NSB_TOTAL(I%)::TOTAL(LOOP%) &
				FOR LOOP% = 1% TO 9%

		NEXT I%

		TEXT$ = SPACE$(15%) + "Total"

		TEXT$ = LEFT(TEXT$ + SPACE$(39%), 39%) + &
			FORMAT$(EMP(1%), "######.## ") + &
			FORMAT$(EMP(2%), "######.## ") + &
			FORMAT$(EMP(3%), "######.## ") + &
			FORMAT$(EMP(4%), "######.## ") + &
			FORMAT$(EMP(5%), "#######.## | ") + &
			FORMAT$(EMP(6%), "######.## ") + &
			FORMAT$(EMP(7%), "######.## ") + &
			FORMAT$(EMP(8%), "######.## ") + &
			FORMAT$(EMP(9%), "#######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	END IF

	!
	! Output Non-account totals
	!
	IF SBA_TOTAL% <> 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 5%)

		TEXT$ = "Total for SubAccount"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		EMP(LOOP%) = 0.0 FOR LOOP% = 1% TO 9%

		FOR I% = 1% TO SBA_TOTAL%
			TEXT$ = SPACE$(15%) + SBA_TOTAL(I%)::ACCOUNT

			TEXT$ = LEFT(TEXT$ + SPACE$(39%), 39%) + &
				FORMAT$(SBA_TOTAL(I%)::TOTAL(1%), &
				"######.## ") + &
				FORMAT$(SBA_TOTAL(I%)::TOTAL(2%), &
				"######.## ") + &
				FORMAT$(SBA_TOTAL(I%)::TOTAL(3%), &
				"######.## ") + &
				FORMAT$(SBA_TOTAL(I%)::TOTAL(4%), &
				"######.## ") + &
				FORMAT$(SBA_TOTAL(I%)::TOTAL(5%), &
				"#######.## | ") + &
				FORMAT$(SBA_TOTAL(I%)::TOTAL(6%), &
				"######.## ") + &
				FORMAT$(SBA_TOTAL(I%)::TOTAL(7%), &
				"######.## ") + &
				FORMAT$(SBA_TOTAL(I%)::TOTAL(8%), &
				"######.## ") + &
				FORMAT$(SBA_TOTAL(I%)::TOTAL(9%), "#######.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			EMP(LOOP%) = EMP(LOOP%) + SBA_TOTAL(I%)::TOTAL(LOOP%) &
				FOR LOOP% = 1% TO 9%

		NEXT I%

		TEXT$ = SPACE$(15%) + "Total"

		TEXT$ = LEFT(TEXT$ + SPACE$(39%), 39%) + &
			FORMAT$(EMP(1%), "######.## ") + &
			FORMAT$(EMP(2%), "######.## ") + &
			FORMAT$(EMP(3%), "######.## ") + &
			FORMAT$(EMP(4%), "######.## ") + &
			FORMAT$(EMP(5%), "#######.## | ") + &
			FORMAT$(EMP(6%), "######.## ") + &
			FORMAT$(EMP(7%), "######.## ") + &
			FORMAT$(EMP(8%), "######.## ") + &
			FORMAT$(EMP(9%), "#######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	END IF

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
	IF SUMMARY$ = "N"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			SPACE$(39%) + &
			"--------- " + &
			"--------- " + &
			"--------- " + &
			"--------- " + &
			"---------- | " + &
			"--------- " + &
			"--------- " + &
			"--------- " + &
			"----------", 3%)
	END IF

	IF SUMMARY$ <> "O"
	THEN
		TEXT$ = LEFT(TEXT$ + SPACE$(39%), 39%) + &
			FORMAT$(ACCOUNT_TOTAL(1%), "######.## ") + &
			FORMAT$(ACCOUNT_TOTAL(2%), "######.## ") + &
			FORMAT$(ACCOUNT_TOTAL(3%), "######.## ") + &
			FORMAT$(ACCOUNT_TOTAL(4%), "######.## ") + &
			FORMAT$(ACCOUNT_TOTAL(5%), "#######.## | ") + &
			FORMAT$(ACCOUNT_TOTAL(6%), "######.## ") + &
			FORMAT$(ACCOUNT_TOTAL(7%), "######.## ") + &
			FORMAT$(ACCOUNT_TOTAL(8%), "######.## ") + &
			FORMAT$(ACCOUNT_TOTAL(9%), "#######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	IF SUMMARY$ = "N"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			SPACE$(39%) + &
			"--------- " + &
			"--------- " + &
			"--------- " + &
			"--------- " + &
			"---------- | " + &
			"--------- " + &
			"--------- " + &
			"--------- " + &
			"----------", 0%)
	END IF

	OPER_TOTAL(I%) = OPER_TOTAL(I%) + ACCOUNT_TOTAL(I%) &
		FOR I% = 1% TO 9%

	ACCOUNT_TOTAL(I%) = 0.0 FOR I% = 1% TO 9%

	TEXT$ = ""

	RETURN

 OperTotal:
	!******************************************************************
	! Print Employee total
	!******************************************************************
	IF SUMMARY$ <> "O"
	THEN
		TEXT$ = SPACE$(5%) + "Operation Total"

		TEXT$ = LEFT(TEXT$ + SPACE$(39%), 39%) + &
			FORMAT$(OPER_TOTAL(1%), "######.## ") + &
			FORMAT$(OPER_TOTAL(2%), "######.## ") + &
			FORMAT$(OPER_TOTAL(3%), "######.## ") + &
			FORMAT$(OPER_TOTAL(4%), "######.## ") + &
			FORMAT$(OPER_TOTAL(5%), "#######.## | ") + &
			FORMAT$(OPER_TOTAL(6%), "######.## ") + &
			FORMAT$(OPER_TOTAL(7%), "######.## ") + &
			FORMAT$(OPER_TOTAL(8%), "######.## ") + &
			FORMAT$(OPER_TOTAL(9%), "#######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	SUBACC_TOTAL(I%) = SUBACC_TOTAL(I%) + OPER_TOTAL(I%) &
		FOR I% = 1% TO 9%

	OPER_TOTAL(I%) = 0.0 FOR I% = 1% TO 9%

	TEXT$ = ""

	RETURN

 SubaccTotal:
	!******************************************************************
	! Print Subaccount total
	!******************************************************************
	IF SUMMARY$ <> "O"
	THEN
		TEXT$ = SPACE$(10%) + "Sub Account Total"

		TEXT$ = LEFT(TEXT$ + SPACE$(39%), 39%) + &
			FORMAT$(SUBACC_TOTAL(1%), "######.## ") + &
			FORMAT$(SUBACC_TOTAL(2%), "######.## ") + &
			FORMAT$(SUBACC_TOTAL(3%), "######.## ") + &
			FORMAT$(SUBACC_TOTAL(4%), "######.## ") + &
			FORMAT$(SUBACC_TOTAL(5%), "#######.## | ") + &
			FORMAT$(SUBACC_TOTAL(6%), "######.## ") + &
			FORMAT$(SUBACC_TOTAL(7%), "######.## ") + &
			FORMAT$(SUBACC_TOTAL(8%), "######.## ") + &
			FORMAT$(SUBACC_TOTAL(9%), "#######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	GRAND_TOTAL(I%) = GRAND_TOTAL(I%) + SUBACC_TOTAL(I%) &
		FOR I% = 1% TO 9%

	SUBACC_TOTAL(I%) = 0.0 FOR I% = 1% TO 9%

	TEXT$ = ""

	RETURN

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
