1	%TITLE "Post Final Payroll Program"
	%SBTTL "PR_RPRT_FINALPOST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1995 BY
	! Software Solutions
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
	! Software Solutions.
	!
	! Software Solutions assumes no responsibility for the use or
	! reliability of its software on equipment which is not supported
	! by Software Solutions.
	!
	!++
	! ID:PRFINL
	!
	! Abstract:HELP
	!	.p
	!	The ^*Post Cash/Expenses/Employer Tax\* option
	!	executes a payroll posting option which
	!	may be executed exclusively or in conjunction with the "Accrual"
	!	posting option.
	!	.p
	!	If a payroll file folder contained labor charges which should be
	!	posted into the same accounting period as the payroll checks are to be
	!	written, the payroll accrual process could be bypassed. The "Final"
	!	posting routine would be the only posting routine necessary.
	!	.p
	!	Otherwise, the "Accrual" payroll posting routine would be executed
	!	in order to post the labor and burden charges into one accounting
	!	period and the "Final" payroll posting routine would be executed to
	!	debit the accrued payroll and payroll tax expense accounts and credit
	!	the payroll cash account, the various tax liability accounts and
	!	appropriate accounts for payroll deductions.
	!
	! Index:
	!	.x Post
	!	.x Post>Final
	!	.x Final>Post
	!	.x Post>Cash
	!	.x Cash>Post
	!	.x Expense>Post
	!	.x Post>Expense
	!	.x Employer Tax>Post
	!	.x Post>Employer Tax
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_FINALPOST
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_FINALPOST, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_FINALPOST.OBJ;*
	!
	! Author:
	!
	!	05/11/95 - Kevin Handy
	!		Copied from PR_POST_FINAL
	!
	! Modification history:
	!
	!	05/12/95 - Kevin Handy
	!		Modified to initialize arrays of structures before
	!		adding to whatever is in there.
	!
	!	05/18/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/30/95 - Kevin Handy
	!		Modified to sort by account number.
	!
	!	05/30/95 - Kevin Handy
	!		Added flag to FI description to specify if the
	!		information cones from the control file or the
	!		distribution file.
	!
	!	09/10/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/19/97 - Kevin Handy
	!		Handle FH code.
	!
	!	05/12/97 - Kevin Handy
	!		Lose UTL_WORK.DEV$ variable
	!		Use one more digit for fica rates.
	!
	!	07/24/97 - Kevin Handy
	!		Allow old FICA rates to work until I can get
	!		everyone converted.
	!
	!	05/29/98 - Kevin Handy
	!		Handle new 'F' final deduction code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/17/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add a pile of REGARDLESS
	!
	!	03/28/2001 - Kevin Handy
	!		Add exit on f10 code to outp_line calls.
	!
	!	03/28/2001 - Kevin Handy
	!		Added grand totals
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	MAP (UTL_REPORTX)	UTL_REPORTX_CDD	UTL_REPORTX

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD		GL_PERIOD

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.HB"
	MAP (PR_CONTROL)	PR_CONTROL_CDD		PR_CONTROL

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD		PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD		PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK

	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.HB"
	MAP (PR_HIS_PAY)	PR_HIS_PAY_CDD		PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.HB"
	MAP (PR_HIS_DED)	PR_HIS_DED_CDD		PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.HB"
	MAP (PR_HIS_CHECK)	PR_HIS_CHECK_CDD	PR_HIS_CHECK

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP (PR_TAX_TABLE)	PR_TAX_TABLE_CDD	PR_TAX_TABLE

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP (PR_TAX_PKG)	PR_TAX_PKG_CDD		PR_TAX_PKG

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_FRI.HB"
	MAP (PR_TAX_PROFILE_FRI) PR_TAX_PROFILE_FRI_CDD	PR_TAX_PROFILE_FRI

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_C.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_C_CDD	PR_TAX_PROFILE_C

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_E.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_E_CDD	PR_TAX_PROFILE_E

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_D.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_D_CDD	PR_TAX_PROFILE_D

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DEF.HB"
	MAP (PR_OVERHEAD_DEF)	PR_OVERHEAD_DEF_CDD	PR_OVERHEAD_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DESC.HB"
	MAP (PR_OVERHEAD_DESC)	PR_OVERHEAD_DESC_CDD	PR_OVERHEAD_DESC

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.HB"
	MAP (PR_WC_DESCR)	PR_WC_DESCR_CDD		PR_WC_DESCR

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.HB"
	MAP (PR_WC_INSURANCE)	PR_WC_INSURANCE_CDD	PR_WC_INSURANCE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	MAP (PR_EMP_ACCRUAL)	PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL

	!
	! Need to include _F version so that variable length record
	! business will work.
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	RECORD PR_TEMP_CDD
		STRING	ACCT = 18%
		STRING	SUBACC = 10%
		STRING	OPER = 8%
		REAL	AMOUNT
		REAL	UNITS
		REAL	HOURS
		STRING	DSCR = 30%
	END RECORD

	DECLARE LONG CONSTANT MAX_GL_SUMMARY = 300%
	DIM PR_TEMP_CDD GL_SUMMARY(MAX_GL_SUMMARY)
	DIM PR_TEMP_CDD GL_TOTAL(MAX_GL_SUMMARY)
	DECLARE PR_TEMP_CDD GL_SUMMARY_TEMP

	RECORD ACCRUAL_RECORD
		STRING ACCOUNT = 18%
		REAL   AMOUNT
	END RECORD

	DIM ACCRUAL_RECORD ACCRUAL(50%)

	!
	! External functions
	!
	EXTERNAL	LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! This dimension is used for current fui,fica,sui calculation
	!
	DIM EMP_WH_CODE%(5%), &
		EMP_NONTAX_CUR(5%), &
		EMP_WH_CODE$(5%, 10%), &
		EMP_SUBJ_CUR(5%, 10%)

	DIM TAX_CODE$(10%), &
		TAXABLE(10%, 4%)

	DIM PKG_WH_CODE$(10%)

	!
	! This dimension is used to accumulate the indirect tax expense
	!
	RECORD FICA_RECORD
		STRING	EX_ACCT = 18%
		REAL	EX_AMT
		REAL    HI_AMT
		STRING	SOURCE_FLAG = 20%
	END RECORD

	DIM FICA_RECORD FICA_TAX(400%)

	RECORD FUI_RECORD
		STRING	EX_ACCT = 18%
		REAL	EX_AMT
	END RECORD

	DIM FUI_RECORD FUI_TAX(300%)

	RECORD SUI_RECORD
		STRING	EX_ACCT = 18%
		STRING	LIA_ACCT = 18%
		STRING	CODE = 2%
		REAL	PCT
		REAL	EX_AMT
	END RECORD

	DIM SUI_RECORD SUI_TAX(300%)

	RECORD OST_RECORD
		STRING	EX_ACCT = 18%
		STRING	LIA_ACCT = 18%
		STRING	CODE = 2%
		REAL	PCT
		REAL	EX_AMT
	END RECORD

	DIM OST_RECORD OST_TAX(800%)

	!
	! This dimension is used to accrue employee deductions
	!
	RECORD EMPDED_RECORD
		STRING	XTYPE = 5%
		STRING	ACCOUNT = 18%
		REAL	AMOUNT
	END RECORD

	DIM EMPDED_RECORD EMPDED(500%)

	DIM UP_BURDEN(100%), &
		UP_LIA$(100%)

	%PAGE

	ON ERROR GOTO 19000

	SCOPE::PRG_PROGRAM = READ_SYSPN

	!
	! Other Variables
	!
	SUBJECT_TYPE_TABLE$ = "FIR$FUI$SUI$OST$"
	GL_SUMMARY% = 0%
	GL_TOTAL% = 0%

	!SUBJECT_TYPE_TABLE$ = 'FIR$FWH$SWH$OST$CWH$DWH$EWH$SUI$SWC$'

	TAX_TYPE_TABLE$ = "FI!FH!FW!SW!SX!CW!DW!EW!SU!SI!"

 Init:	!==============================================================
	! OPEN THE PRINT CONTROL FILE
	!==============================================================

	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	!
	! Abstract:FLD02
	!	^*(02) Payroll Date\*
	!	.p
	!	The ^*Payroll Date\* field
	!	enters the payroll file folder date for
	!	the payroll period which is to be posted. The date is to be entered
	!	in MMDDYY format.
	!
	! Index:
	!	.x Payroll Date>Final
	!	.x Final>Payroll Date
	!	.x Post>Payroll Date
	!	.x Payroll Date>Post
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)
	YYYY$, PR_YYYY$ = LEFT(BATCH_NO$, 4%)

	POST_DATE$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	!
	! Abstract:FLD03
	!
	!	^*(03) Post Date\*
	!
	! Index:
	! Datatype:TEXT
	! Size:0
	! Required:
	!--

	POST_DATE$ = DATE_STOREDATE(POST_DATE$)

	!
	! This is only for testing
	!
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(8%), 132%)
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

300	!
	! Get GL_PERIOD file info
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

320	!
	! Open chart of accounts
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_MASTER"
		CONTINUE HelpError
	END WHEN

340	!
	! Open Payroll control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.OPN"

		GET #PR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "PR_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF PR_CONTROL::CLOSEFLAG = "2"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "PR Reset in process", &
			"ERR", "PR_RESET", &
			"ERROR_RESET")
		GOTO ExitProgram
	END IF

	IF PR_CONTROL::CLOSEFLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "PR update in process", &
			"ERR", "PR_UPDATE", &
			"ERROR_UPDATE")
		GOTO ExitProgram
	END IF

350	!
	! Open Pay folder
	!
	HISTORY% = 0%
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		CONTINUE 355
	END WHEN

	GOTO 360

355	HISTORY% = -1%
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	PR_TRN_PAY.CH% = PR_HIS_PAY.CH%

360	!
	! Open Deduction folder
	!
	IF HISTORY%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"
		USE
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		PR_TRN_DED.CH% = PR_HIS_DED.CH%
	ELSE
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
		USE
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN
	END IF

370	!
	! Open Check folder
	!
	WHEN ERROR IN
		IF HISTORY%
		THEN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.OPN"
			PR_TRN_CHECK.CH% = PR_HIS_CHECK.CH%
		ELSE
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.OPN"
		END IF
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

380	!
	! Open ERNDED Definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

410	!
	! Open Tax Table file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"
		GET #PR_TAX_TABLE.CH%, KEY #0% EQ "F", REGARDLESS
	USE
		FILENAME$ = "PR_TAX_TABLE"
		CONTINUE HelpError
	END WHEN

	FICA_EMPR_PCT = (PR_TAX_TABLE::FICA_EMPR_PCT) / 10000.0
	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT
	FICA_EMPR_PCT_HI = (PR_TAX_TABLE::FICA_EMPR_PCT_HI) / 10000.0
	FICA_LIMIT_HI = PR_TAX_TABLE::FICA_LIMIT_HI

	!
	! Allow for an additional digit in FICA
	!
	IF FICA_EMPR_PCT > 0.100
	THEN
		FICA_EMPR_PCT = FICA_EMPR_PCT / 10.0
		FICA_EMPR_PCT_HI = FICA_EMPR_PCT_HI / 10.0
	END IF

	CLOSE PR_TAX_TABLE.CH%

420	!
	! Open Profile
	!
	FUI_PCT, FUI_MAX = 0.0

	CASH_ACCT$ = STRING$(LEN(PR_TAX_PROFILE_F::CASH_ACCT), 63%)
	PR_ACCRUAL_ACCT$ = STRING$(LEN(PR_TAX_PROFILE_F::PR_ACCRUAL_ACCT), 63%)
	WH_ACCT$ = STRING$(LEN(PR_TAX_PROFILE_F::WH_ACCT), 63%)
	FICA_EX_ACCT$ = STRING$(LEN(PR_TAX_PROFILE_F::FICA_EX_ACCT), 63%)
	FICA_LIA_ACCT_EMPR$ = STRING$( &
		LEN(PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPR), 63%)
	FICA_LIA_ACCT_EMPE$ = STRING$( &
		LEN(PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPE), 63%)
	FUI_EX_ACCT$ = STRING$(LEN(PR_TAX_PROFILE_F::FUI_EX_ACCT), 63%)
	FUI_LIA_ACCT$ = STRING$(LEN(PR_TAX_PROFILE_F::FUI_LIA_ACCT), 63%)

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"

	GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "F  ", REGARDLESS

	FUI_PCT = PR_TAX_PROFILE_F::FUI_PCT/100.
	FUI_MAX = PR_TAX_PROFILE_F::FUI_MAX

	CASH_ACCT$ = PR_TAX_PROFILE_F::CASH_ACCT
	PR_ACCRUAL_ACCT$ = PR_TAX_PROFILE_F::PR_ACCRUAL_ACCT
	WH_ACCT$ = PR_TAX_PROFILE_F::WH_ACCT
	FICA_EX_ACCT$ = PR_TAX_PROFILE_F::FICA_EX_ACCT
	FICA_LIA_ACCT_EMPR$ = PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPR
	FICA_LIA_ACCT_EMPE$ = PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPE
	FUI_EX_ACCT$ = PR_TAX_PROFILE_F::FUI_EX_ACCT
	FUI_LIA_ACCT$ = PR_TAX_PROFILE_F::FUI_LIA_ACCT

430	!
	! Open Tax package
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
	USE
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

440	!
	! Open Tax Profile Fringe account table
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_FRI.OPN"
	USE
		CONTINUE 470 IF ERR = 5%
			! FICA
		FILENAME$ = "PR_TAX_PROFILE_FRI"
		CONTINUE HelpError
	END WHEN

470	!
	! Open the overhead description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DESC.OPN"
	USE
		CONTINUE 480 IF ERR = 5%
		FILENAME$ = "PR_OVERHEAD_DESC"
		CONTINUE HelpError
	END WHEN

480	!
	! Open the overhead definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DEF.OPN"
	USE
		CONTINUE 490 IF ERR = 5%
		FILENAME$ = "PR_OVERHEAD_DEF"
		CONTINUE HelpError
	END WHEN

490	!
	! Open the WC description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.OPN"
	USE
		CONTINUE 510 IF ERR = 5%
		FILENAME$ = "PR_WC_DESCR"
		CONTINUE HelpError
	END WHEN

510	!
	! Open the WC insurance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.OPN"
	USE
		CONTINUE 520 IF ERR = 5%
		FILENAME$ = "PR_WC_INSURANCE"
		CONTINUE HelpError
	END WHEN

520	!
	! Open Employee Accrual definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.OPN"
	USE
		IF ERR = 5%
		THEN
			CALL ASSG_FREECHANNEL(PR_EMP_ACCRUAL.CH%)
			PR_EMP_ACCRUAL.CH% = 0%
			CONTINUE 530
		END IF
		FILENAME$ = "PR_EMP_ACCRUAL"
		CONTINUE HelpError
	END WHEN

530	!

	%PAGE

	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Final Post Audit Report"
	TITLE$(2%) = "For the Payroll Folder: " + PRNT_DATE(BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Define headers
	!
	TITLE$(4%) = "GL Account         SubAcct    Oper        Amount" + &
		"  Units  Hours   Description"
	TITLE$(5%) = ""


	%PAGE

3000	!******************************************************************
	! Open up batch control file and get a batch number
	!******************************************************************

	!
	! Initilize for all passes
	!
	POST_STATUS% = 0%

	IF FROM_ITEM$ = ""
	THEN
		RESET #PR_TRN_PAY.CH%
	ELSE
		FIND #PR_TRN_PAY.CH%, KEY #0% GE FROM_ITEM$
	END IF

	!
	! Set finish flag to zero
	! Set net check to zero
	!
	EOF% = 0%
	NET_CHECK = 0.0
	TEST_EMPNUM$ = ""

3010	!
	! Read in one record
	!
	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		IF ERR = 11%
		THEN
			EOF% = -1%
			CONTINUE 4000
		END IF
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	IF HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	!
	! Use only for testing
	!
	IF PR_TRN_PAY::EMPNUM > TO_ITEM$ AND TO_ITEM$ <> ""
	THEN
		EOF% = -1%
		GOTO 4000
	END IF

	!
	! Goto to the deduction file read if not the same employee or
	! the payroll dates do not match
	!
	IF PR_TRN_PAY::EMPNUM + PR_TRN_PAY::PR_END_DATE <> &
		TEST_EMPNUM$ + TEST_PR_END_DATE$ AND &
		TEST_EMPNUM$ <> ""
	THEN
		GOTO 4000
	END IF

	!
	! Handle "A" records
	!
	IF PR_TRN_PAY::PTYPE = "A"
	THEN
		GOSUB ReadAccrual
		GOTO 3010
	END IF

3020	!
	! This is where the process resumes back after it is
	! finished with one employee
	!
	NET_CHECK = FUNC_ROUND(NET_CHECK - PR_TRN_PAY::GROSS, 2%)

	!
	! Calculate the burden
	!
	SUBJECT_PREM = 0.0

	IF PR_TRN_PAY::RTYPE = "H" OR PR_TRN_PAY::RTYPE = "S" OR &
		PR_TRN_PAY::RTYPE = "X"
	THEN
		SUBJECT_PREM = FUNC_ROUND(PR_TRN_PAY::OVT_HR * ( &
			((PR_TRN_PAY::FACTOR/100.) - 1.0) * &
			PR_TRN_PAY::HOUR_RATE), 2%)
		SUBJECT_PREM = 0.0 IF SUBJECT_PREM < 0.0
	END IF

	IF TEST_EMPNUM$ <> PR_TRN_PAY::EMPNUM
	THEN
		GOSUB DumpEmployee

		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_TRN_PAY::EMPNUM, &
				REGARDLESS
		USE
			PR_EMP_MASTER::ACCT	= PR_TRN_PAY::ACCT
			PR_EMP_MASTER::WC	= ""
			PR_EMP_MASTER::SUI_SW	= "??"
			PR_EMP_MASTER::EMPNAME	= &
				"????????????????????????????????????"

			CONTINUE 3030 IF ERR = 155%
			FILENAME$ = "PR_MASTER"
			CONTINUE HelpError
		END WHEN

	END IF

3030	!
	! Get burden
	!
	EXP_ACCT$ = PR_TRN_PAY::ACCT
	!
	! If this is departmental accounting then set the expense
	! account number to the account number in the master file.
	!
	IF PR_CONTROL::OH_APPLY_FLAG = "D"
	THEN
		EXP_ACCT$ = PR_EMP_MASTER::ACCT
	END IF

	CALL PR_READ_BURDEN(PR_OVERHEAD_DEF.CH%, &
		PR_OVERHEAD_DESC.CH%, &
		PR_WC_DESCR.CH%, &
		PR_WC_INSURANCE.CH%, &
		PR_TAX_PKG.CH%, &
		PR_ERNDED_DEF.CH%, &
		EXP_ACCT$, &
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

	SI_LIA$ = WC_LIA$ IF WC_LIA$<>""

3040	TEST_EMPNUM$  = PR_TRN_PAY::EMPNUM
	TEST_PR_END_DATE$ = PR_TRN_PAY::PR_END_DATE

	CALL ENTR_3MESSAGE(SCOPE, &
		"Posting Employee: " + TEST_EMPNUM$, 1%)

	!***************************************************************
	! This section accumulates wages to determine indirect tax
	! expense allocations
	!
	! Accumulate amount for fui, fica, sui, ost
	!
	PKG_WH_CODE$(3%) = "??"
	PKG_WH_CODE$(4%) = "??"
	WHEN ERROR IN
		FIND #PR_TAX_PKG.CH%, &
			KEY #0% EQ PR_TRN_PAY::TAX_PKG + "S", &
			REGARDLESS
	USE
		CONTINUE 3045 IF ERR = 155%
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

3043	WHEN ERROR IN
		GET #PR_TAX_PKG.CH%, REGARDLESS
	USE
		CONTINUE 3045 IF ERR = 11%
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

	GOTO 3045 IF PR_TAX_PKG::TAX_PKG <> PR_TRN_PAY::TAX_PKG

	SELECT PR_TAX_PKG::STTYPE

	CASE "SU"
		PKG_WH_CODE$(3%) = PR_TAX_PKG::CODE

	CASE "SW"
		PKG_WH_CODE$(3%) = PR_TAX_PKG::CODE &
			IF PKG_WH_CODE$(3%) = "??"
		PKG_WH_CODE$(4%) = PR_TAX_PKG::CODE

	CASE "SX"
		PKG_WH_CODE$(4%) = PR_TAX_PKG::CODE

	END SELECT

	GOTO 3043

3045	PKG_WH_CODE$(3%) = EDIT$(PR_EMP_MASTER::SUI_SW, -1%) &
		IF EDIT$(PR_EMP_MASTER::SUI_SW, -1%) <> ""

	!
	! Last change, let's try the employees state.
	!
	PKG_WH_CODE$(3%) = EDIT$(PR_EMP_MASTER::STATE, -1%) &
		IF PKG_WH_CODE$(3%) = "??" AND &
		TRM$(PR_EMP_MASTER::STATE) <> ""

	PKG_WH_CODE$(4%) = EDIT$(PR_EMP_MASTER::STATE, -1%) &
		IF PKG_WH_CODE$(4%) = "??" AND &
		TRM$(PR_EMP_MASTER::STATE) <> ""

3050	FOR TAX_TYPE% = 1% TO LEN(SUBJECT_TYPE_TABLE$) / 4%
		!
		! See if taxable
		!
		SUBJECT_CODE$ = MID(SUBJECT_TYPE_TABLE$, &
			(TAX_TYPE% - 1%) * 4% + 1%, 3%)

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			SUBJECT_CODE$, &
			"P", &
			PR_TRN_PAY::CODE, &
			TAXABLE%, &
			REPORTABLE%)

		GOTO 3090 IF TAXABLE%

		WH_LOOP% = 1%

		IF TAX_TYPE% > 3%
		THEN
			GOTO 3060 IF PKG_WH_CODE$(TAX_TYPE%) = &
				EMP_WH_CODE$(TAX_TYPE%,WH_LOOP%) &
				FOR WH_LOOP% = 1% TO &
					EMP_WH_CODE%(TAX_TYPE%)

			EMP_WH_CODE%(TAX_TYPE%), WH_LOOP% = &
				EMP_WH_CODE%(TAX_TYPE%) + 1%
			EMP_WH_CODE$(TAX_TYPE%, WH_LOOP%) = &
				PKG_WH_CODE$(TAX_TYPE%)
		END IF

3060		IF TAX_TYPE% < 4% OR PKG_WH_CODE$(TAX_TYPE%) <> ""
		THEN
			EMP_SUBJ_CUR(TAX_TYPE%, WH_LOOP%) = &
				FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, &
				WH_LOOP%) + &
				PR_TRN_PAY::GROSS, 2%)
		END IF

		EMP_SUBJ_CUR(TAX_TYPE%, 0%) = &
			FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, 0%) + &
			PR_TRN_PAY::GROSS, 2%)

3090	NEXT TAX_TYPE%

	!**************************************************************
	! This is the end of the indirect tax accumulation section
	! for pay.
	!**************************************************************

3100	!*********************************************************
	! Add Workmen comp to payroll temporary work file
	!*********************************************************

	! Jump over wc if wc = 0.0
	GOTO 3200 IF WC_BURDEN = 0.0

	!
	! Add wc burden expense
	!
	GL_SUMMARY% = GL_SUMMARY% + 1%

	GL_SUMMARY(GL_SUMMARY%)::ACCT	= WC_EXP$
	GL_SUMMARY(GL_SUMMARY%)::SUBACC	= PR_TRN_PAY::SUBACC
	GL_SUMMARY(GL_SUMMARY%)::OPER	= PR_TRN_PAY::OPER
	GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= FUNC_ROUND(WC_BURDEN, 2%)
	GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::DSCR	= "WC EXPENSE"

3150	!
	! Add wc burden liability
	!
	GL_SUMMARY% = GL_SUMMARY% + 1%

	GL_SUMMARY(GL_SUMMARY%)::ACCT	= WC_LIA$
	GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
	GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
	GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= FUNC_ROUND(-WC_BURDEN, 2%)
	GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::DSCR	= "WC LIABILITY"

3200	!*********************************************************
	! Add Overhead to payroll temporary work file
	!*********************************************************
	! Jump over OH if OH = 0.0
	GOTO 3300 IF OH_BURDEN = 0.0

	!
	! Add OH burden expense
	!
	GL_SUMMARY% = GL_SUMMARY% + 1%

	GL_SUMMARY(GL_SUMMARY%)::ACCT	= OH_EXP$
	GL_SUMMARY(GL_SUMMARY%)::SUBACC	= PR_TRN_PAY::SUBACC
	GL_SUMMARY(GL_SUMMARY%)::OPER	= PR_TRN_PAY::OPER
	GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= FUNC_ROUND(OH_BURDEN, 2%)
	GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::DSCR	= "OH EXPENSE"

3250	!
	! Add OH burden applied
	!
	GL_SUMMARY% = GL_SUMMARY% + 1%

	GL_SUMMARY(GL_SUMMARY%)::ACCT	= OH_APP$
	GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
	GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
	GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= FUNC_ROUND(-OH_BURDEN, 2%)
	GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::DSCR	= "OH APPLIED"

3300	!*********************************************************
	! Add Union and pension benefits to payroll temporary work file
	!*********************************************************
	TEMP = 0.0
	TEMP = FUNC_ROUND(TEMP + UP_BURDEN(LOOP%), 2%) &
		FOR LOOP% = 1% TO 10%

	! Jump over UP if UP = 0.0
	GOTO 3400 IF TEMP = 0.0
	!
	! Add union pension burden
	!
	GL_SUMMARY% = GL_SUMMARY% + 1%

	GL_SUMMARY(GL_SUMMARY%)::ACCT	= UP_EXP$
	GL_SUMMARY(GL_SUMMARY%)::SUBACC	= PR_TRN_PAY::SUBACC
	GL_SUMMARY(GL_SUMMARY%)::OPER	= PR_TRN_PAY::OPER
	GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= FUNC_ROUND(TEMP, 2%)
	GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::DSCR	= "PENSION"

3350	!
	! Add wc union/pension liability
	!
	FOR LOOP% = 1% TO 10%

		GOTO 3390 IF UP_BURDEN(LOOP%)  = 0.0

		GL_SUMMARY% = GL_SUMMARY% + 1%

		GL_SUMMARY(GL_SUMMARY%)::ACCT	= UP_LIA$(LOOP%)
		GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
		GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
		GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= FUNC_ROUND(-WC_BURDEN, 2%)
		GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
		GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
		GL_SUMMARY(GL_SUMMARY%)::DSCR	= "PENSION LIABILITY"

3390	NEXT LOOP%

3400	!*********************************************************
	! Add Premium pay to payroll temporary work file
	!*********************************************************
	! Jump over PREM if PREM = 0.0
	GOTO 3500 IF PREM_AMT = 0.0
	!
	! Add overtime prem to prem account
	!
	GL_SUMMARY% = GL_SUMMARY% + 1%

	GL_SUMMARY(GL_SUMMARY%)::ACCT	= PREM_ACC$
	GL_SUMMARY(GL_SUMMARY%)::SUBACC	= PR_TRN_PAY::SUBACC
	GL_SUMMARY(GL_SUMMARY%)::OPER	= PR_TRN_PAY::OPER
	GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= FUNC_ROUND(PREM_AMT, 2%)
	GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::DSCR	= "OVERTIME PREMIUM"

3500	! Resume point

3700	!**********************************************************
	! Generate a GL for payroll expense record to pass through
	! to the post function
	!**********************************************************
	GL_SUMMARY% = GL_SUMMARY% + 1%

	GL_SUMMARY(GL_SUMMARY%)::ACCT	= PR_TRN_PAY::ACCT
	GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= FUNC_ROUND(PR_TRN_PAY::GROSS - &
		PREM_AMT, 2%)
	GL_SUMMARY(GL_SUMMARY%)::SUBACC	= PR_TRN_PAY::SUBACC
	GL_SUMMARY(GL_SUMMARY%)::OPER	= PR_TRN_PAY::OPER
	GL_SUMMARY(GL_SUMMARY%)::UNITS	= PR_TRN_PAY::PIECE
	GL_SUMMARY(GL_SUMMARY%)::HOURS	= PR_TRN_PAY::REG_HR + &
		PR_TRN_PAY::OVT_HR
	GL_SUMMARY(GL_SUMMARY%)::DSCR	= "Pay Record"

	!
	! Loop back and get next payroll pay record
	!
	GOTO 3010

4000	!
	! If the variable TEST_EMPNUM$ is equal to null
	! then notify the user that there is nothing to post
	! and jump to exit program
	!
	IF TEST_EMPNUM$ = ""
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find transaction to post", 0%)

		GOTO Aborted

	END IF

	!
	! Now we do the deduction journal
	!
	!
	! Look up deduction
	!
	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, KEY #0% EQ TEST_EMPNUM$ + &
			TEST_PR_END_DATE$, REGARDLESS
	USE
		CONTINUE 4100 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

4020	!
	! Get deduction record
	!
	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE 4100 IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	IF HISTORY%
	THEN
		PR_TRN_DED = PR_HIS_DED
	END IF

	!
	! Go to the check file if the employee number or the payroll
	! date do not match.
	!
	GOTO 4100 IF PR_TRN_DED::EMPNUM + PR_TRN_DED::PR_END_DATE <> &
		TEST_EMPNUM$ + TEST_PR_END_DATE$

	!
	! See if this is a tax
	!
	GOTO 4090 IF INSTR(1%, TAX_TYPE_TABLE$, PR_TRN_DED::CODE)

	!
	! Check Deductions
	!
	GOTO 4090 IF PR_TRN_DED::DTYPE = "T" OR PR_TRN_DED::DTYPE = "M"

	!***************************************************************
	! This section accumulates wages to determine indirect tax
	! expense allocations
	!
	FOR TAX_TYPE% = 1% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

		!
		! See if taxable and reportable
		!
		SUBJECT_CODE$ = MID(SUBJECT_TYPE_TABLE$, &
			(TAX_TYPE% - 1%) * 4% + 1%, 3%)

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			SUBJECT_CODE$, &
			PR_TRN_DED::DTYPE, &
			PR_TRN_DED::CODE, &
			TAXABLE%, &
			REPORTABLE%)

		IF (TAXABLE% = 0% AND &
			PR_TRN_DED::DTYPE <> "D" AND &
			PR_TRN_DED::DTYPE <> "F") OR &
			(TAXABLE% <> 0% AND &
			(PR_TRN_DED::DTYPE = "D" OR PR_TRN_DED::DTYPE = "F"))
		THEN
			!
			! This item is non-taxable.
			!
			! Nontax deduction is always handled in final,
			! because it is never handled in accrual.
			!
			IF TAX_TYPE% < 4%
			THEN
				EMP_SUBJ_CUR(TAX_TYPE%, 1%) = &
					FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, 1%) - &
					PR_TRN_DED::AMOUNT, 2%)
			ELSE
				EMP_NONTAX_CUR(TAX_TYPE%) = &
					FUNC_ROUND(EMP_NONTAX_CUR(TAX_TYPE%) + &
					PR_TRN_DED::AMOUNT, 2%)
			END IF
		END IF

	NEXT TAX_TYPE%

4090	!
	! Get pay deduction summary flag
	!
	! See if this is a tax, If it is not then read the
	! pay deduction definition file for the deduction account
	! number.
	!
	IF INSTR(1%, TAX_TYPE_TABLE$, PR_TRN_DED::CODE) = 0%
	THEN
		!
		! Try to save disk accesses. Look only if not
		! already here.
		!
		IF (PR_ERNDED_DEF::ETYPE <> PR_TRN_DED::DTYPE) OR &
			(PR_ERNDED_DEF::CODE <> PR_TRN_DED::CODE)
		THEN
			PR_ERNDED_DEF::DRCR_ACCT = STRING$(LEN( &
				PR_ERNDED_DEF::DRCR_ACCT), 63%)
			PR_ERNDED_DEF::SUMMARY = "Y"

			WHEN ERROR IN
				GET #PR_ERNDED_DEF.CH%, &
					KEY #0% EQ PR_TRN_DED::DTYPE + &
					PR_TRN_DED::CODE, REGARDLESS
			USE
				IF ERR = 155%
				THEN
					CALL ENTR_3MESSAGE(SCOPE, "Deduction code " + &
						TRM$(PR_TRN_DED::CODE) + &
						" for employee # " + &
						TRM$(TEST_EMPNUM$) + &
						" is undefined", 0%)
					CONTINUE 4095
				END IF

				FILENAME$ = "PR_ERNDED_DEF"
				CONTINUE HelpError
			END WHEN
		END IF
	ELSE
		PR_ERNDED_DEF::ETYPE = "~"
		PR_ERNDED_DEF::CODE = "~~"
		PR_ERNDED_DEF::SUMMARY = "Y"
	END IF

4095	IF PR_TRN_DED::DTYPE <> "T" AND PR_TRN_DED::DTYPE <> "M"
	THEN
		!
		! Total for net check
		!
		NET_CHECK = FUNC_ROUND(NET_CHECK + &
			PR_TRN_DED::AMOUNT, 2%)

		!
		! Post detail of deduction if summary flag is "N"
		!
		IF PR_ERNDED_DEF::SUMMARY = "N"
		THEN
			GL_SUMMARY% = GL_SUMMARY% + 1%

			GL_SUMMARY(GL_SUMMARY%)::ACCT	= PR_ERNDED_DEF::DRCR_ACCT
			GL_SUMMARY(GL_SUMMARY%)::SUBACC	= PR_TRN_DED::EMPNUM
			GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
			GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= FUNC_ROUND(-PR_TRN_DED::AMOUNT, 2%)
			GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::DSCR	= "Deduction " + &
				PR_TRN_DED::CODE

		!
		! Accumulate totals in array
		!
		ELSE
			!
			! If this is the second pass then accumulate
			! in array otherwise skip this step.
			!
			GOSUB AccPayDed
		END IF

	!
	! Loop back for next deduction record
	!
	GOTO 4020

4100	!***********************************************************
	! Now post the check to the cash account
	!

	!
	! Post checks
	!
	WHEN ERROR IN
		GET #PR_TRN_CHECK.CH%, &
			KEY #0% EQ TEST_EMPNUM$ + TEST_PR_END_DATE$, &
			REGARDLESS
	USE
		CONTINUE 4300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	IF HISTORY%
	THEN
		PR_TRN_CHECK = PR_HIS_CHECK
	END IF

	GL_SUMMARY% = GL_SUMMARY% + 1%

	GL_SUMMARY(GL_SUMMARY%)::ACCT	= CASH_ACCT$
	GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
	GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
	GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= FUNC_ROUND(NET_CHECK, 2%)
	GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::DSCR	= "Check"

4300	!****************************************************************
	! Look up fica tax and taxable wages
	!****************************************************************

	CODE_LOOP% = 0%

	CALL PR_READ_SUBJWAGE(TEST_EMPNUM$, &
		PR_YYYY$, &
		"FIR", &
		TAX_CODE$(), &
		TAXABLE(,), &
		CODE_LOOP%)

	EMP_SUBJ_YTD = 0.0
	EMP_SUBJ_YTD = FUNC_ROUND(EMP_SUBJ_YTD + &
		TAXABLE(1%, QTR%), 2%) &
		FOR QTR% = 1% TO 4%

	!--------------------------------------------------------------------
	! Determine FICA Taxes
	!--------------------------------------------------------------------

	FICA_WAGES = 0.0

	EMP_SUBJ_YTD = FUNC_ROUND(EMP_SUBJ_YTD + &
		EMP_SUBJ_CUR(1%, 1%), 2%)

	IF EMP_SUBJ_YTD - EMP_SUBJ_CUR(1%, 1%) < FICA_LIMIT_HI
	THEN
		FICA_EXCESS_HI = FUNC_ROUND(EMP_SUBJ_YTD - &
			FICA_LIMIT_HI, 2%)
		FICA_EXCESS_HI = 0.0 IF FICA_EXCESS_HI < 0.0
		FICA_WAGES_HI = FUNC_ROUND(EMP_SUBJ_CUR(1%, 1%) - &
			FICA_EXCESS_HI, 2%)
	END IF

	IF EMP_SUBJ_YTD - EMP_SUBJ_CUR(1%, 1%) < FICA_LIMIT
	THEN
		FICA_EXCESS = FUNC_ROUND(EMP_SUBJ_YTD - FICA_LIMIT, 2%)
		FICA_EXCESS = 0.0 IF FICA_EXCESS < 0.0
		FICA_WAGES = FUNC_ROUND(EMP_SUBJ_CUR(1%, 1%) - &
			FICA_EXCESS, 2%)
	END IF

	!-------------------------------------------------------------
	! Now figure out what account number to debit
	!-------------------------------------------------------------
	CALL GL_ASSG_ACCMASK(FICA_EX_ACCT$, &
		PR_EMP_MASTER::ACCT, &
		TEMP_FICA_EX_ACCT$)

	SOURCE_FLAG$ = "(Ctl)" + PR_EMP_MASTER::ACCT

	GOTO 4390 IF PR_CONTROL::OH_APPLY_FLAG <> "D"

4310	!
	! Look up first fica distribution labor account
	!
	WHEN ERROR IN
		FIND #PR_TAX_PROFILE_FRI.CH%, KEY #0% GE "FI", REGARDLESS
	USE
		CONTINUE 4390 IF ERR = 155% OR ERR = 9%
			! FUI
		FILENAME$ = "PR_TAX_PROFILE_FRI"
		CONTINUE HelpError
	END WHEN

4320	WHEN ERROR IN
		GET #PR_TAX_PROFILE_FRI.CH%, REGARDLESS
	USE
		CONTINUE 4390 IF ERR = 11%
			! FUI
		FILENAME$ = "PR_TAX_PROFILE_FRI"
		CONTINUE HelpError
	END WHEN

	GOTO 4390 IF PR_TAX_PROFILE_FRI::TAX_TYPE <> "FI"

	IF COMP_STRING(TRM$(PR_EMP_MASTER::ACCT), &
		PR_TAX_PROFILE_FRI::LABOR_ACCT)
	THEN
		CALL GL_ASSG_ACCMASK(PR_TAX_PROFILE_FRI::FRI_EX_ACCT, &
			PR_EMP_MASTER::ACCT, &
			TEMP_FICA_EX_ACCT$)

		SOURCE_FLAG$ = "(Distrib)"

		GOTO 4390
	END IF

	GOTO 4320

4390	!
	! Add fica expense to fica expense array
	!
	GOTO 4395 IF TEMP_FICA_EX_ACCT$ = FICA_TAX(LOOP%)::EX_ACCT &
		FOR LOOP% = 1% TO FICA_EX_LOOP%

	FICA_EX_LOOP%, LOOP% = FICA_EX_LOOP% + 1%
	FICA_TAX(LOOP%)::EX_ACCT = TEMP_FICA_EX_ACCT$
	FICA_TAX(LOOP%)::EX_AMT = 0.0
	FICA_TAX(LOOP%)::HI_AMT = 0.0
	FICA_TAX(LOOP%)::SOURCE_FLAG = SOURCE_FLAG$

4395	FICA_TAX(LOOP%)::EX_AMT = FUNC_ROUND(FICA_TAX(LOOP%)::EX_AMT + &
		FICA_WAGES, 2%)
	FICA_TAX(LOOP%)::HI_AMT = FUNC_ROUND(FICA_TAX(LOOP%)::HI_AMT + &
		FICA_WAGES_HI, 2%)

4400	!*************************************************************
	! Look up fui taxable wages
	!*************************************************************

	CODE_LOOP% = 0%

	CALL PR_READ_SUBJWAGE(TEST_EMPNUM$, &
		PR_YYYY$, &
		"FUI", &
		TAX_CODE$(), &
		TAXABLE(,), &
		CODE_LOOP%)

	EMP_SUBJ_YTD = 0.0
	EMP_SUBJ_YTD = FUNC_ROUND(EMP_SUBJ_YTD + &
		TAXABLE(1%, QTR%), 2%) &
		FOR QTR% = 1% TO 4%

	!-------------------------------------------------------------
	! Determine FUI Taxes
	!-------------------------------------------------------------

	EMP_SUBJ_YTD = FUNC_ROUND(EMP_SUBJ_YTD + &
		EMP_SUBJ_CUR(2%, 1%), 2%)

	IF EMP_SUBJ_YTD - EMP_SUBJ_CUR(2%, 1%) < FUI_MAX OR FUI_MAX = 0.0
	THEN
		FUI_EXCESS = FUNC_ROUND(EMP_SUBJ_YTD - FUI_MAX, 2%)
		FUI_EXCESS = 0.0 IF FUI_EXCESS < 0.0 OR &
			FUI_MAX = 0.0
		FUI_WAGES = FUNC_ROUND(EMP_SUBJ_CUR(2%, 1%) - &
			FUI_EXCESS, 2%)
	ELSE
		FUI_WAGES = 0.0
	END IF

	!-------------------------------------------------------------
	! Now figure out what account number to debit
	!-------------------------------------------------------------
	CALL GL_ASSG_ACCMASK(FUI_EX_ACCT$, &
		PR_EMP_MASTER::ACCT, &
		TEMP_FUI_EX_ACCT$)

	GOTO 4490 IF PR_CONTROL::OH_APPLY_FLAG <> "D"

4410	!
	! Look up first FUI distribution labor account
	!
	WHEN ERROR IN
		FIND #PR_TAX_PROFILE_FRI.CH%, KEY #0% GE "FU", REGARDLESS
	USE
		CONTINUE 4490 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TAX_PROFILE_FRI"
		CONTINUE HelpError
	END WHEN

4420	WHEN ERROR IN
		GET #PR_TAX_PROFILE_FRI.CH%, REGARDLESS
	USE
		CONTINUE 4490 IF ERR = 11%
			! SUI
		FILENAME$ = "PR_TAX_PROFILE_FRI"
		CONTINUE HelpError
	END WHEN

	GOTO 4490 IF PR_TAX_PROFILE_FRI::TAX_TYPE <> "FU"

	IF COMP_STRING(TRM$(PR_EMP_MASTER::ACCT), &
		PR_TAX_PROFILE_FRI::LABOR_ACCT)
	THEN
		CALL GL_ASSG_ACCMASK(PR_TAX_PROFILE_FRI::FRI_EX_ACCT, &
			PR_EMP_MASTER::ACCT, &
			TEMP_FUI_EX_ACCT$)

		GOTO 4490
	END IF

	GOTO 4420

4490	!
	! Add FUI expense to FUI expense array
	!
	GOTO 4495 IF TEMP_FUI_EX_ACCT$ = FUI_TAX(LOOP%)::EX_ACCT &
		FOR LOOP% = 1% TO FUI_EX_LOOP%

	FUI_EX_LOOP%, LOOP% = FUI_EX_LOOP% + 1%
	FUI_TAX(LOOP%)::EX_ACCT = TEMP_FUI_EX_ACCT$
	FUI_TAX(LOOP%)::EX_AMT = 0.0

4495	FUI_TAX(LOOP%)::EX_AMT = FUNC_ROUND(FUI_TAX(LOOP%)::EX_AMT + &
		FUI_WAGES, 2%)

4500	!*************************************************************
	! Look up sui taxable wages
	!*************************************************************

	CODE_LOOP% = EMP_WH_CODE%(3%)

	!
	! If we have some non-taxable amount to handle, but all
	! pay records have been accrued, then we need to fake up
	! a pay record to post the negitive SUTA amount. (HACK)
	!
	IF (CODE_LOOP% = 0%) AND (EMP_NONTAX_CUR(3%) <> 0%)
	THEN
		CODE_LOOP% = 1%
		EMP_SUBJ_CUR(3%, 0%) = 0%
		EMP_SUBJ_CUR(3%, 1%) = 0%
		EMP_WH_CODE$(3%, 1%) = PKG_WH_CODE$(3%)
	END IF

	TAX_CODE$(I%) = EMP_WH_CODE$(3%, I%) FOR I% = 1% TO CODE_LOOP%

	CALL PR_READ_SUBJWAGE(TEST_EMPNUM$, &
		PR_YYYY$, &
		"SUI", &
		TAX_CODE$(), &
		TAXABLE(,), &
		CODE_LOOP%)

	!
	! Allocate Taxable wages for the current period
	!
	TAX_TYPE% = 3%

	FACTOR = 1.0
	FACTOR = 1.0 - (EMP_NONTAX_CUR(TAX_TYPE%) / &
		EMP_SUBJ_CUR(TAX_TYPE%, 0%)) &
		IF EMP_SUBJ_CUR(TAX_TYPE%, 0%) <> 0.0

	TOTAL_TO_DIST = FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, 0%) - &
		EMP_NONTAX_CUR(TAX_TYPE%), 2%)

	FOR LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%) - 1%
		EMP_SUBJ_CUR(TAX_TYPE%, LOOP%) = &
			FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, LOOP%) * &
			FACTOR, 2%)

		TOTAL_TO_DIST = FUNC_ROUND(TOTAL_TO_DIST - &
			EMP_SUBJ_CUR(TAX_TYPE%, LOOP%), 2%)
	NEXT LOOP%

	EMP_SUBJ_CUR(TAX_TYPE%, EMP_WH_CODE%(TAX_TYPE%)) = &
		FUNC_ROUND(TOTAL_TO_DIST, 2%)


4520	!-------------------------------------------------------------------
	! Detemine SUI taxes
	!-------------------------------------------------------------------

	FOR LOOP% = 1% TO EMP_WH_CODE%(3%)

		!
		! Lookup tax rates, if we don't already have them
		!
		IF TEST_SUI_CODE$ <> EMP_WH_CODE$(3%, LOOP%)
		THEN
			TEST_SUI_CODE$ = EMP_WH_CODE$(3%, LOOP%)
			SUI_PCT = 0.0
			SUI_MAX = 0.0

			SUI_EX_ACCT$ = STRING$(LEN( &
				PR_TAX_PROFILE_S::SUI_EX_ACCT), 63%)
			SUI_LIA_ACCT$ = STRING$(LEN( &
				PR_TAX_PROFILE_S::SUI_LIA_ACCT), 63%)

			IF (PR_TAX_PROFILE_S::AUTH <> "S") OR &
				(PR_TAX_PROFILE_S::CODE <> TEST_SUI_CODE$)
			THEN
				WHEN ERROR IN
					GET #PR_TAX_PROFILE.CH%, &
						KEY #0% EQ "S" + TEST_SUI_CODE$, &
						REGARDLESS
				USE
					CONTINUE 4540 IF ERR = 155% OR ERR = 9%
					FILENAME$ = "PR_TAX_PROFILE"
					CONTINUE HelpError
				END WHEN
			END IF

			SUI_PCT = PR_TAX_PROFILE_S::SUI_PCT / 100.0
			SUI_MAX = PR_TAX_PROFILE_S::SUI_MAX

			SUI_EX_ACCT$ = PR_TAX_PROFILE_S::SUI_EX_ACCT
			SUI_LIA_ACCT$ = PR_TAX_PROFILE_S::SUI_LIA_ACCT
		END IF

4540		EMP_SUBJ_YTD = 0.0
		EMP_SUBJ_YTD = FUNC_ROUND(EMP_SUBJ_YTD + &
			TAXABLE(LOOP%, QTR%), 2%) &
			FOR QTR% = 1% TO 4%

		EMP_SUBJ_YTD = FUNC_ROUND(EMP_SUBJ_YTD + &
			EMP_SUBJ_CUR(3%,LOOP%), 2%)

		IF EMP_SUBJ_YTD - EMP_SUBJ_CUR(3%, LOOP%) < SUI_MAX &
			OR SUI_MAX = 0.0
		THEN
			SUI_EXCESS = FUNC_ROUND(EMP_SUBJ_YTD - &
				SUI_MAX, 2%)
			SUI_EXCESS = 0.0 IF SUI_EXCESS < 0.0 OR &
				SUI_MAX = 0.0
			SUI_WAGES = FUNC_ROUND(EMP_SUBJ_CUR(3%, &
				LOOP%) - SUI_EXCESS, 2%)
		ELSE
			SUI_WAGES = 0.0
		END IF

		!-------------------------------------------------------------
		! Now figure out what account number to debit
		!-------------------------------------------------------------
		CALL GL_ASSG_ACCMASK(SUI_EX_ACCT$, &
			PR_EMP_MASTER::ACCT, &
			TEMP_SUI_EX_ACCT$)

		CALL GL_ASSG_ACCMASK(SUI_LIA_ACCT$, &
			PR_EMP_MASTER::ACCT, &
			TEMP_SUI_LIA_ACCT$)

		GOTO 4590 IF PR_CONTROL::OH_APPLY_FLAG <> "D"

4550		!
		! Look up first SUI distribution labor account
		!
		WHEN ERROR IN
			FIND #PR_TAX_PROFILE_FRI.CH%, KEY #0% GE "SU", REGARDLESS
		USE
			CONTINUE 4590 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TAX_PROFILE_FRI"
			CONTINUE HelpError
		END WHEN

4560		WHEN ERROR IN
			GET #PR_TAX_PROFILE_FRI.CH%, REGARDLESS
		USE
			CONTINUE 4590 IF ERR = 11%
			FILENAME$ = "PR_TAX_PROFILE_FRI"
			CONTINUE HelpError
		END WHEN

		GOTO 4590 IF PR_TAX_PROFILE_FRI::TAX_TYPE <> "SU"

		IF COMP_STRING(TRM$(PR_EMP_MASTER::ACCT), &
			PR_TAX_PROFILE_FRI::LABOR_ACCT)
		THEN
			CALL GL_ASSG_ACCMASK(PR_TAX_PROFILE_FRI::FRI_EX_ACCT, &
				PR_EMP_MASTER::ACCT, &
				TEMP_SUI_EX_ACCT$)

			GOTO 4590
		END IF

		GOTO 4560

4590		!
		! Add SUI expense to SUI expense array
		!
		GOTO 4595 IF TEMP_SUI_EX_ACCT$ + TEMP_SUI_LIA_ACCT$ = &
			SUI_TAX(X%)::EX_ACCT + SUI_TAX(X%)::LIA_ACCT &
			FOR X% = 1% TO SUI_EX_COUNT%

		SUI_EX_COUNT%, X% = SUI_EX_COUNT% + 1%
		SUI_TAX(X%)::EX_ACCT = TEMP_SUI_EX_ACCT$
		SUI_TAX(X%)::LIA_ACCT = TEMP_SUI_LIA_ACCT$
		SUI_TAX(X%)::CODE = TEST_SUI_CODE$
		SUI_TAX(X%)::PCT = SUI_PCT
		SUI_TAX(X%)::EX_AMT = 0.0

4595		SUI_TAX(X%)::EX_AMT = FUNC_ROUND(SUI_TAX(X%)::EX_AMT + &
			SUI_WAGES, 2%)

4599	NEXT LOOP%

4600	!*************************************************************
	! Look up OST taxable wages
	!*************************************************************

	CODE_LOOP% = EMP_WH_CODE%(4%)

	!
	! If we have some non-taxable amount to handle, but all
	! pay records have been accrued, then we need to fake up
	! a pay record to post the negitive SUTA amount. (HACK)
	!
	IF (CODE_LOOP% = 0%) AND (EMP_NONTAX_CUR(4%) <> 0%)
	THEN
		CODE_LOOP% = 1%
		EMP_SUBJ_CUR(4%, 0%) = 0%
		EMP_SUBJ_CUR(4%, 1%) = 0%
		EMP_WH_CODE$(4%, 1%) = PKG_WH_CODE$(4%)
	END IF

	TAX_CODE$(I%) = EMP_WH_CODE$(4%, I%) FOR I% = 1% TO CODE_LOOP%

	CALL PR_READ_SUBJWAGE(TEST_EMPNUM$, &
		PR_YYYY$, &
		"OST", &
		TAX_CODE$(), &
		TAXABLE(,), &
		CODE_LOOP%)

	!
	! Allocate Taxable wages for the current period
	!
	TAX_TYPE% = 4%

	FACTOR = 1.0
	FACTOR = 1.0 - (EMP_NONTAX_CUR(TAX_TYPE%) / &
		EMP_SUBJ_CUR(TAX_TYPE%, 0%)) &
		IF EMP_SUBJ_CUR(TAX_TYPE%, 0%) <> 0.0

	TOTAL_TO_DIST = FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, 0%) - &
		EMP_NONTAX_CUR(TAX_TYPE%), 2%)

	FOR LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%) - 1%

		EMP_SUBJ_CUR(TAX_TYPE%, LOOP%) = &
			FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, LOOP%) * &
			FACTOR, 2%)

		TOTAL_TO_DIST = FUNC_ROUND(TOTAL_TO_DIST - &
			EMP_SUBJ_CUR(TAX_TYPE%, LOOP%), 2%)

	NEXT LOOP%

	EMP_SUBJ_CUR(TAX_TYPE%, EMP_WH_CODE%(TAX_TYPE%)) = &
		FUNC_ROUND(TOTAL_TO_DIST, 2%)

	!-------------------------------------------------------------
	! Detemine OST taxes
	!-------------------------------------------------------------

	FOR LOOP% = 1% TO EMP_WH_CODE%(4%)

4630		IF TEST_OST_CODE$ <> EMP_WH_CODE$(4%, LOOP%)
		THEN
			TEST_OST_CODE$ = EMP_WH_CODE$(4%, LOOP%)
			OST_PCT = 0.0
			OST_MAX = 0.0
			OST_DEDMAX = 0.0

			OST_EX_ACCT$ = STRING$(LEN( &
				PR_TAX_PROFILE_S::OST_EX_ACCT), 63%)
			OST_LIA_ACCT$ = STRING$(LEN( &
				PR_TAX_PROFILE_S::OST_LIA_ACCT), 63%)

			IF (PR_TAX_PROFILE_S::AUTH <> "S") OR &
				(PR_TAX_PROFILE_S::CODE <> &
				TEST_OST_CODE$)
			THEN
				WHEN ERROR IN
					GET #PR_TAX_PROFILE.CH%, &
						KEY #0% EQ "S" + &
						TEST_OST_CODE$, REGARDLESS
				USE
					CONTINUE 4640
				END WHEN
			END IF

			OST_PCT = PR_TAX_PROFILE_S::OST_PCT / 100.0
			OST_MAX = PR_TAX_PROFILE_S::OST_MAX
			OST_DEDMAX = PR_TAX_PROFILE_S::OST_DEDMAX / &
				PR_EMP_MASTER::PAYFREQ

			OST_EX_ACCT$ = PR_TAX_PROFILE_S::OST_EX_ACCT
			OST_LIA_ACCT$ = PR_TAX_PROFILE_S::OST_LIA_ACCT
		END IF

4640		OST_WAGES = 0.0

		EMP_SUBJ_YTD = 0.0
		EMP_SUBJ_YTD = FUNC_ROUND(EMP_SUBJ_YTD + &
			TAXABLE(LOOP%, QTR%), 2%) &
			FOR QTR% = 1% TO 4%

		IF OST_MAX <> 0.
		THEN
			EMP_SUBJ_YTD = FUNC_ROUND(EMP_SUBJ_YTD + &
				EMP_SUBJ_CUR(4%,LOOP%), 2%)

			IF EMP_SUBJ_YTD - EMP_SUBJ_CUR(4%, LOOP%) < &
				OST_MAX
			THEN
				OST_EXCESS = FUNC_ROUND(EMP_SUBJ_YTD - &
					OST_MAX, 2%)
				OST_EXCESS = 0.0 IF OST_EXCESS < 0.0
				OST_WAGES = FUNC_ROUND(EMP_SUBJ_CUR(4%, &
					LOOP%) - OST_EXCESS, 2%)
			END IF
		ELSE
			EMP_SUBJ_DED = FUNC_ROUND(EMP_SUBJ_DED + &
				EMP_SUBJ_CUR(4%,LOOP%), 2%)

			IF (EMP_SUBJ_DED - EMP_SUBJ_CUR(4%, LOOP%) < &
				OST_DEDMAX) AND (OST_DEDMAX <> 0.0)
			THEN
				OST_EXCESS = FUNC_ROUND(EMP_SUBJ_DED - &
					OST_DEDMAX, 2%)
				OST_EXCESS = 0.0 IF OST_EXCESS < 0.0
				OST_WAGES = FUNC_ROUND(EMP_SUBJ_CUR(4%, &
					LOOP%) - OST_EXCESS, 2%)
			END IF
		END IF

		!-------------------------------------------------------------
		! Now figure out what account number to debit
		!-------------------------------------------------------------
		CALL GL_ASSG_ACCMASK(OST_EX_ACCT$, &
			PR_EMP_MASTER::ACCT, &
			TEMP_OST_EX_ACCT$)

		GOTO 4690 IF PR_CONTROL::OH_APPLY_FLAG <> "D"

4650		!
		! Look up first OST distribution labor account
		!
		WHEN ERROR IN
			FIND #PR_TAX_PROFILE_FRI.CH%, KEY #0% GE "SX", REGARDLESS
		USE
			CONTINUE 4690 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TAX_PROFILE_FRI"
			CONTINUE HelpError
		END WHEN

4660		WHEN ERROR IN
			GET #PR_TAX_PROFILE_FRI.CH%, REGARDLESS
		USE
			CONTINUE 4690 IF ERR = 11%
			FILENAME$ = "PR_TAX_PROFILE_FRI"
			CONTINUE HelpError
		END WHEN

		GOTO 4690 IF PR_TAX_PROFILE_FRI::TAX_TYPE <> "SX"

		IF COMP_STRING(TRM$(PR_EMP_MASTER::ACCT), &
			PR_TAX_PROFILE_FRI::LABOR_ACCT)
		THEN
			CALL GL_ASSG_ACCMASK(PR_TAX_PROFILE_FRI::FRI_EX_ACCT, &
				PR_EMP_MASTER::ACCT, &
				TEMP_OST_EX_ACCT$)

			GOTO 4690
		END IF

		GOTO 4660

4690		!
		! Add OST expense to OST expense array
		!
		GOTO 4695 IF TEST_OST_CODE$ = OST_TAX(X%)::CODE &
			FOR X% = 1% TO OST_EX_LOOP%

		OST_EX_LOOP%, X% = OST_EX_LOOP% + 1%
		OST_TAX(X%)::EX_ACCT = TEMP_OST_EX_ACCT$
		OST_TAX(X%)::LIA_ACCT = OST_LIA_ACCT$
		OST_TAX(X%)::CODE = TEST_OST_CODE$
		OST_TAX(X%)::PCT = OST_PCT
		OST_TAX(X%)::EX_AMT = 0.0

4695		OST_TAX(X%)::EX_AMT = FUNC_ROUND(OST_TAX(X%)::EX_AMT + &
			OST_WAGES, 2%)

	NEXT LOOP%

4990	!
	! Zero employee array in preparation for next employee
	!
	FOR I% = 0% TO 5%
		EMP_WH_CODE%(I%) = 0%
		EMP_NONTAX_CUR(I%) = 0.0

		FOR J% = 0% TO 10%
			EMP_SUBJ_CUR(I%,J%) = 0.0
			EMP_WH_CODE$(I%,J%) = ""
		NEXT J%
	NEXT I%

	EMP_SUBJ_DED = 0.0

	!
	! Set up for fica and fui
	!
	EMP_WH_CODE%(1%) = 1%
	EMP_WH_CODE%(2%) = 1%

	IF EOF% = 0%
	THEN
		NET_CHECK = 0.0
		GOTO 3020
	END IF

	GOTO 6200

	!************************************************************
	! Here starts the processing after looping through all
	! of the folder.
	!************************************************************

5000	!
	! Post deductions from array of deductions
	! This will post both non tax and tax deductions.
	!
	FOR LOOP% = 1% TO ED_TYPECODE_LOOP%
		LIA_ACCT$ = STRING$(18%, 63%)

		TAX_TYPE% = (INSTR(1%, TAX_TYPE_TABLE$, &
			MID(EMPDED(LOOP%)::XTYPE, 2%, 2%)) + 2%) /3%

		IF TAX_TYPE%
		THEN
			TEST_CODE$ = RIGHT(EMPDED(LOOP%)::XTYPE, 2%)

			!
			! Look up account number if not federal
			!
			IF LEFT(TEST_CODE$, 1%) <> "F"
			THEN
				WHEN ERROR IN
					GET #PR_TAX_PROFILE.CH%, KEY #0% EQ &
						LEFT(TEST_CODE$, 1%) + &
						TRM$(MID(TEST_CODE$, 3%, 2%)), &
						REGARDLESS
				USE
					CONTINUE 5010 IF ERR = 155%
					FILENAME$ = "PR_ERNDED_DEF"
					CONTINUE HelpError
				END WHEN
			END IF

			SELECT LEFT(TEST_CODE$, 2%)

			CASE "FW"
				CALL GL_ASSG_ACCMASK(WH_ACCT$, &
					EMPDED(LOOP%)::ACCOUNT, &
					LIA_ACCT$)

			CASE "FI", "FH"
				CALL GL_ASSG_ACCMASK(FICA_LIA_ACCT_EMPE$, &
					EMPDED(LOOP%)::ACCOUNT, &
					LIA_ACCT$)

			CASE "SW"
				CALL GL_ASSG_ACCMASK(PR_TAX_PROFILE_S::WH_ACCT, &
					EMPDED(LOOP%)::ACCOUNT, &
					LIA_ACCT$)

			CASE "SX"
				CALL GL_ASSG_ACCMASK(PR_TAX_PROFILE_S::OST_LIA_ACCT, &
					EMPDED(LOOP%)::ACCOUNT, &
					LIA_ACCT$)

			CASE "CW"
				CALL GL_ASSG_ACCMASK(PR_TAX_PROFILE_C::CITY_LIA_ACCT, &
					EMPDED(LOOP%)::ACCOUNT, &
					LIA_ACCT$)

			CASE "DW"
				CALL GL_ASSG_ACCMASK(PR_TAX_PROFILE_D::COU_LIA_ACCT, &
					EMPDED(LOOP%)::ACCOUNT, &
					LIA_ACCT$)

			CASE "EW"
				CALL GL_ASSG_ACCMASK(PR_TAX_PROFILE_E::SCH_LIA_ACCT, &
					EMPDED(LOOP%)::ACCOUNT, &
					LIA_ACCT$)

			CASE "SU"
				CALL GL_ASSG_ACCMASK(PR_TAX_PROFILE_S::SUI_LIA_ACCT, &
					EMPDED(LOOP%)::ACCOUNT, &
					LIA_ACCT$)

			CASE "SI"
				CALL GL_ASSG_ACCMASK(SI_LIA$, &
					EMPDED(LOOP%)::ACCOUNT, &
					LIA_ACCT$)
			END SELECT

		ELSE
			!
			! Try to save disk accesses
			!
			IF (PR_ERNDED_DEF::ETYPE + &
				PR_ERNDED_DEF::CODE <> &
				LEFT(EMPDED(LOOP%)::XTYPE, 3%))
			THEN
				GET #PR_ERNDED_DEF.CH%, KEY #0% EQ &
					LEFT(EMPDED(LOOP%)::XTYPE, 3%), &
					REGARDLESS
			END IF

			LIA_ACCT$ = PR_ERNDED_DEF::DRCR_ACCT
		END IF

5010		LIA_ACCT$ = STRING$(18%, 63%) &
			IF EDIT$(LIA_ACCT$, -1%) = ""

		IF FUNC_ROUND(EMPDED(LOOP%)::AMOUNT, 2%) <> 0.0
		THEN
			GL_SUMMARY% = GL_SUMMARY% + 1%

			GL_SUMMARY(GL_SUMMARY%)::ACCT	= LIA_ACCT$
			GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
			GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
			GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= FUNC_ROUND(EMPDED(LOOP%)::AMOUNT, 2%)
			GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::DSCR	= "DED = " + RIGHT( &
				EMPDED(LOOP%)::XTYPE, 2%)

		END IF

	NEXT LOOP%

	ED_TYPECODE_LOOP% = 0%

5100	!
	! If accrual is not equal to zero the post gl
	!
	FOR LOOP1% = 1% TO ACCRUAL%
		GL_SUMMARY% = GL_SUMMARY% + 1%

		GL_SUMMARY(GL_SUMMARY%)::ACCT	= ACCRUAL(LOOP1%)::ACCOUNT
		GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
		GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
		GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= &
			FUNC_ROUND(ACCRUAL(LOOP1%)::AMOUNT, 2%)
		GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
		GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
		GL_SUMMARY(GL_SUMMARY%)::DSCR	= "PAYROLL ACCRUAL"

	NEXT LOOP1%

	ACCRUAL% = 0%

5200	!*******************************************************************
	! Read Payroll temp file and post to gl
	!*******************************************************************

5300	!
	! Post indirect employer payroll tax expenses
	!

	!
	! Loop for fica indirect calculation
	!
	FOR LOOP% = 1% TO FICA_EX_LOOP%
		!
		! Add in fica expense and liability
		!
		X_AMOUNT = FUNC_ROUND( &
			FICA_EMPR_PCT * FICA_TAX(LOOP%)::EX_AMT + &
			FICA_EMPR_PCT_HI * FICA_TAX(LOOP%)::HI_AMT, &
			2%)

		! Expense
		IF X_AMOUNT <> 0.0
		THEN
			GL_SUMMARY% = GL_SUMMARY% + 1%

			GL_SUMMARY(GL_SUMMARY%)::ACCT	= FICA_TAX(LOOP%)::EX_ACCT
			GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
			GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
			GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= X_AMOUNT
			GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::DSCR	= "IND FICA " + &
				FICA_TAX(LOOP%)::SOURCE_FLAG

			!
			! Now post the liability
			!
			CALL GL_ASSG_ACCMASK(FICA_LIA_ACCT_EMPR$, &
				FICA_TAX(LOOP%)::EX_ACCT, &
				TEMP_ACCT$)

			GL_SUMMARY% = GL_SUMMARY% + 1%

			GL_SUMMARY(GL_SUMMARY%)::ACCT	= TEMP_ACCT$
			GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
			GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
			GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= -X_AMOUNT
			GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::DSCR	= "INDIRECT FICA"

		END IF

	NEXT LOOP%

	FICA_EX_LOOP% = 0%

	!
	! Loop for fui indirect calculation
	!
	FOR LOOP% = 1% TO FUI_EX_LOOP%

		!
		! Post fui expense
		!
		X_AMOUNT = FUNC_ROUND(FUI_PCT * &
			FUI_TAX(LOOP%)::EX_AMT, 2%)

		! Expense
		IF X_AMOUNT <> 0.0
		THEN
			GL_SUMMARY% = GL_SUMMARY% + 1%

			GL_SUMMARY(GL_SUMMARY%)::ACCT	= FUI_TAX(LOOP%)::EX_ACCT
			GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
			GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
			GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= X_AMOUNT
			GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::DSCR	= "INDIRECT FUI"

			!
			! Now post fui liability
			!
			CALL GL_ASSG_ACCMASK(FUI_LIA_ACCT$, &
				FUI_TAX(LOOP%)::EX_ACCT, &
				TEMP_ACCT$)

			GL_SUMMARY% = GL_SUMMARY% + 1%

			GL_SUMMARY(GL_SUMMARY%)::ACCT	= TEMP_ACCT$
			GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
			GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
			GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= -X_AMOUNT
			GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::DSCR	= "INDIRECT FUI"
		END IF

	NEXT LOOP%

	FUI_EX_LOOP% = 0%

	!
	! Post sui expense and liability
	!
	FOR LOOP% = 1% TO SUI_EX_COUNT%

		X_AMOUNT = FUNC_ROUND(SUI_TAX(LOOP%)::PCT * &
			SUI_TAX(LOOP%)::EX_AMT, 2%)

		IF X_AMOUNT <> 0.0
		THEN
			!********
			! Expense
			!********
			GL_SUMMARY% = GL_SUMMARY% + 1%

			GL_SUMMARY(GL_SUMMARY%)::ACCT	= SUI_TAX(LOOP%)::EX_ACCT
			GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
			GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
			GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= X_AMOUNT
			GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::DSCR	= "INDIRECT SUI FOR " + &
				SUI_TAX(LOOP%)::CODE

			!**********
			! Liability
			!**********

			GL_SUMMARY% = GL_SUMMARY% + 1%

			GL_SUMMARY(GL_SUMMARY%)::ACCT	= SUI_TAX(LOOP%)::LIA_ACCT
			GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
			GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
			GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= -X_AMOUNT
			GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::DSCR	= "INDIRECT SUI FOR " + &
				SUI_TAX(LOOP%)::CODE

		END IF
	NEXT LOOP%

	SUI_EX_COUNT% = 0%

	!
	! Post ost expense and liability
	!
	FOR LOOP% = 1% TO OST_EX_LOOP%

		X_AMOUNT = FUNC_ROUND(OST_TAX(LOOP%)::PCT * &
			OST_TAX(LOOP%)::EX_AMT, 2%)

		!
		! Expense
		!
		IF X_AMOUNT <> 0.0
		THEN
			!
			! Call the post function
			!
			GL_SUMMARY% = GL_SUMMARY% + 1%

			GL_SUMMARY(GL_SUMMARY%)::ACCT	= OST_TAX(LOOP%)::EX_ACCT
			GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
			GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
			GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= X_AMOUNT
			GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::DSCR	= "INDIRECT OST FOR " + &
				OST_TAX(LOOP%)::CODE

			!
			! Liability
			!
			GL_SUMMARY% = GL_SUMMARY% + 1%

			GL_SUMMARY(GL_SUMMARY%)::ACCT	= OST_TAX(LOOP%)::LIA_ACCT
			GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
			GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
			GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= -X_AMOUNT
			GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::HOURS	= 0.0
			GL_SUMMARY(GL_SUMMARY%)::DSCR	= "INDIRECT OST FOR " + &
				OST_TAX(LOOP%)::CODE
		END IF
	NEXT LOOP%

	OST_EX_LOOP% = 0%

5990	!
	RETURN

6200	!
	!

	%PAGE

 ExitTotal:
10000	!******************************************************************
	! Exit normally
	!******************************************************************

	GOSUB DumpEmployee

	!
	! Sort the accounts (Slow bubble sort, but shouldn't be too many
	! accounts for one employee)
	!
	FOR XLOOP% = 1% TO GL_TOTAL%

		FOR YLOOP% = 1% TO GL_TOTAL% - XLOOP%

			IF GL_TOTAL(YLOOP%)::ACCT > &
				GL_TOTAL(YLOOP% + 1%)::ACCT
			THEN
				GL_SUMMARY_TEMP = GL_TOTAL(YLOOP%)
				GL_TOTAL(YLOOP%) = GL_TOTAL(YLOOP% + 1%)
				GL_TOTAL(YLOOP% + 1%) = GL_SUMMARY_TEMP
			END IF

		NEXT YLOOP%

	NEXT XLOOP%

	TEXT$ = "Grand Totals"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Print info
	!
	FOR XLOOP% = 1% TO GL_TOTAL%

		TEXT$ = &
			GL_TOTAL(XLOOP%)::ACCT + " " + &
			GL_TOTAL(XLOOP%)::SUBACC + " " + &
			GL_TOTAL(XLOOP%)::OPER + " " + &
			FORMAT$(GL_TOTAL(XLOOP%)::AMOUNT, "######.##") + &
			FORMAT$(GL_TOTAL(XLOOP%)::UNITS, "####.##") + &
			FORMAT$(GL_TOTAL(XLOOP%)::HOURS, "####.## ") + &
			GL_TOTAL(XLOOP%)::DSCR

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NEXT XLOOP%


 ExitProgram:
10010	CALL OUTP_FINISH(UTL_REPORTX)

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

 Aborted:
10100	!******************************************************************
	! Abort
	!******************************************************************

 Aborted1:
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%PAGE

 AccPayDed:
	!*******************************************************************
	! Accumulate payded into array
	!******************************************************************
	GOTO AccPayDed1 IF (EMPDED(LOOP%)::XTYPE = PR_TRN_DED::DTYPE + &
		PR_TRN_DED::CODE + PR_TRN_DED::TAX_CODE) AND &
		(EMPDED(LOOP%)::ACCOUNT = PR_EMP_MASTER::ACCT) &
		FOR LOOP% = 1% TO ED_TYPECODE_LOOP%

	ED_TYPECODE_LOOP%, LOOP% = ED_TYPECODE_LOOP% + 1%

	EMPDED(LOOP%)::XTYPE = PR_TRN_DED::DTYPE + PR_TRN_DED::CODE + &
		PR_TRN_DED::TAX_CODE
	EMPDED(LOOP%)::ACCOUNT = PR_EMP_MASTER::ACCT
	EMPDED(LOOP%)::AMOUNT = 0.0

 AccPayDed1:
	EMPDED(LOOP%)::AMOUNT = &
		FUNC_ROUND(EMPDED(LOOP%)::AMOUNT - PR_TRN_DED::AMOUNT, 2%)

	RETURN

	%Page

 ReadAccrual:
	!*******************************************************************
	! Handle "A" records (Accrual)
	!*******************************************************************

18000	IF (PR_ERNDED_DEF::ETYPE <> "P") OR &
		(PR_ERNDED_DEF::CODE <> PR_TRN_PAY::CODE)
	THEN
		!
		! Look up ernded definition file
		!
		PR_ERNDED_DEF::ETYPE = "P"
		PR_ERNDED_DEF::CODE = PR_TRN_PAY::CODE

		PR_ERNDED_DEF::DESCR = STRING$(LEN(PR_ERNDED_DEF::DESCR), 63%)
		PR_ERNDED_DEF::DRCR_ACCT = &
			STRING$(LEN(PR_ERNDED_DEF::DRCR_ACCT), 63%)
		PR_ERNDED_DEF::ACCRUAL_ACCT = &
			STRING$(LEN(PR_ERNDED_DEF::ACCRUAL_ACCT), 63%)

		WHEN ERROR IN
			GET #PR_ERNDED_DEF.CH%, &
				KEY #0% EQ "P" + PR_TRN_PAY::CODE, &
				REGARDLESS
		USE
			CONTINUE 18010 IF ERR = 155%
			FILENAME$ = "PR_ERNDED_DEF"
			CONTINUE HelpError
		END WHEN
	END IF

18005	IF (PR_EMP_ACCRUAL.CH% <> 0%)
	THEN
		WHEN ERROR IN
			GET #PR_EMP_ACCRUAL.CH%, &
				KEY #0% EQ PR_TRN_PAY::EMPNUM + PR_TRN_PAY::CODE, &
				REGARDLESS
		USE
			CONTINUE 18010 IF ERR = 155%
			FILENAME$ = "PR_EMP_ACCRUAL"
			CONTINUE HelpError
		END WHEN
	END IF

	RETURN IF PR_EMP_ACCRUAL::GLFLAG = "N"

18010	!
	! Expense account
	!
	GL_SUMMARY% = GL_SUMMARY% + 1%

	GL_SUMMARY(GL_SUMMARY%)::ACCT	= PR_TRN_PAY::ACCT
	GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
	GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
	GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= (PR_TRN_PAY::GROSS)
	GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::HOURS	= (PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR)
	GL_SUMMARY(GL_SUMMARY%)::DSCR	= "ACCRUAL " + PR_TRN_PAY::CODE

	CALL GL_ASSG_ACCMASK(PR_ERNDED_DEF::ACCRUAL_ACCT, &
		PR_EMP_MASTER::ACCT, &
		ACCT_NUM$)

	GL_SUMMARY% = GL_SUMMARY% + 1%

	GL_SUMMARY(GL_SUMMARY%)::ACCT	= ACCT_NUM$
	GL_SUMMARY(GL_SUMMARY%)::SUBACC	= ""
	GL_SUMMARY(GL_SUMMARY%)::OPER	= ""
	GL_SUMMARY(GL_SUMMARY%)::AMOUNT	= -(PR_TRN_PAY::GROSS)
	GL_SUMMARY(GL_SUMMARY%)::UNITS	= 0.0
	GL_SUMMARY(GL_SUMMARY%)::HOURS	= -(PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR)
	GL_SUMMARY(GL_SUMMARY%)::DSCR	= "ACCRUAL" + PR_TRN_PAY::CODE

	RETURN

	%PAGE

18300	!*******************************************************************
	! Dump out info for one employee
	!*******************************************************************
 DumpEmployee:
	GOSUB 5000

	!
	! Title
	!
	IF GL_SUMMARY% <> 0%
	THEN
		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			PR_EMP_MASTER::EMPNAME

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Sort the accounts (Slow bubble sort, but shouldn't be too many
	! accounts for one employee)
	!
	FOR XLOOP% = 1% TO GL_SUMMARY%

		FOR YLOOP% = 1% TO GL_SUMMARY% - XLOOP%

			IF GL_SUMMARY(YLOOP%)::ACCT > &
				GL_SUMMARY(YLOOP% + 1%)::ACCT
			THEN
				GL_SUMMARY_TEMP = GL_SUMMARY(YLOOP%)
				GL_SUMMARY(YLOOP%) = GL_SUMMARY(YLOOP% + 1%)
				GL_SUMMARY(YLOOP% + 1%) = GL_SUMMARY_TEMP
			END IF

		NEXT YLOOP%

	NEXT XLOOP%

	!
	! Print info
	!
	FOR XLOOP% = 1% TO GL_SUMMARY%

		TEXT$ = &
			GL_SUMMARY(XLOOP%)::ACCT + " " + &
			GL_SUMMARY(XLOOP%)::SUBACC + " " + &
			GL_SUMMARY(XLOOP%)::OPER + " " + &
			FORMAT$(GL_SUMMARY(XLOOP%)::AMOUNT, "######.##") + &
			FORMAT$(GL_SUMMARY(XLOOP%)::UNITS, "####.##") + &
			FORMAT$(GL_SUMMARY(XLOOP%)::HOURS, "####.## ") + &
			GL_SUMMARY(XLOOP%)::DSCR

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		FOR TLOOP% = 1% TO GL_TOTAL%
			IF GL_TOTAL(TLOOP%)::ACCT = GL_SUMMARY(XLOOP%)::ACCT
			THEN
				GL_TOTAL(TLOOP%)::AMOUNT = &
					GL_TOTAL(TLOOP%)::AMOUNT + &
					GL_SUMMARY(XLOOP%)::AMOUNT
				GL_TOTAL(TLOOP%)::UNITS = &
					GL_TOTAL(TLOOP%)::UNITS + &
					GL_SUMMARY(XLOOP%)::UNITS
				GL_TOTAL(TLOOP%)::HOURS = &
					GL_TOTAL(TLOOP%)::HOURS + &
					GL_SUMMARY(XLOOP%)::HOURS

				GOTO SkipOutT
			END IF

		NEXT TLOOP%

		GL_TOTAL% = GL_TOTAL% + 1%

		GL_TOTAL(GL_TOTAL%)::ACCT = GL_SUMMARY(XLOOP%)::ACCT
		GL_TOTAL(GL_TOTAL%)::SUBACC = ""
		GL_TOTAL(GL_TOTAL%)::OPER = ""
		GL_TOTAL(GL_TOTAL%)::AMOUNT = GL_SUMMARY(XLOOP%)::AMOUNT
		GL_TOTAL(GL_TOTAL%)::UNITS = GL_SUMMARY(XLOOP%)::UNITS
		GL_TOTAL(GL_TOTAL%)::HOURS = GL_SUMMARY(XLOOP%)::HOURS
		GL_TOTAL(GL_TOTAL%)::DSCR = GL_SUMMARY(XLOOP%)::DSCR

 SkipOutT:
	NEXT XLOOP%

	IF GL_SUMMARY% <> 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	GL_SUMMARY% = 0%

	RETURN

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%

	SCOPE::PRG_IDENT = "ERR"
	SCOPE::PRG_ITEM = "INTERRUPT"
	SCOPE::PRG_PROGRAM = "POST"

	CALL ENTR_3MESSAGE(SCOPE, &
		"Posting has been interrupted.  Correct error and restart", 0%)

	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
