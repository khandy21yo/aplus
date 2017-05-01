1	%TITLE "Post Payroll Accrual Program"
	%SBTTL "PR_POST_ACCRUAL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:PRACCR
	!
	! Abstract:HELP
	!	.p
	!	^*ACCRUE - Post Accrued Labor/Burden\*
	!	.p
	!	The ^*Post Accrued Labor/Burden\* option
	!	executes a posting routine which will debit
	!	labor and burden expense accounts and credit the accrued labor account.
	!	.p
	!	If the labor expenses in an entire payroll folder are to be posted
	!	in one accounting period, but the checks written for the folder are
	!	written in a subsequent accounting period, this option must be executed
	!	prior to the execution of the "Final" post option.
	!	.p
	!	When the Post Accrued Labor/Burden option is executed, the user
	!	must determine if the payroll folder date is to be changed. This allows
	!	the user the opportunity to enter and balance smaller files -- perhaps
	!	on a daily basis -- which can be posted to different accounting periods
	!	if necessary. As the smaller files are accrued by executing the subject
	!	option, each of the file folder dates can be renamed to coincide with a
	!	master file folder date. This procedure effectively transfers the data
	!	from several smaller file folders into a single larger folder. The
	!	smaller file folders are thereby deleted.
	!
	! Index:
	!	.x Post>Accrued>Labor
	!	.x Post>Accrued>Burden
	!	.x Post>Labor>Accrued
	!	.x Post>Burden>Accrued
	!	.x Accrued>Labor Post
	!	.x Accrued>Burden Post
	!	.x Labor>Post
	!	.x Burden>Post
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_POST_ACCRUAL
	!	$ LINK/EXECUTABLE=PR_EXE: PR_POST_ACCRUAL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_POST_ACCRUAL.OBJ;*
	!
	! Author:
	!
	!	12/20/87 - Robert Peterson
	!
	! Modification history:
	!
	!	03/04/89 - Frank Starman
	!		Add OST
	!
	!	03/27/89 - Kevin Handy
	!		Many modifications to fix minor problems
	!		discovered by looking through a cross reference
	!		listing, and searching for odd variables.
	!		(Only used once, only assigned, only read, ...)
	!
	!	04/06/89 - Kevin Handy
	!		More modifications.  Removed internal summing
	!		of YTD amounts, which was already being read
	!		by PR_READ_SUBJWAGE(), and removal of several
	!		arrays and their code that were loaded but
	!		never used (EMP_NT_CUR, EMP_NT_YTD, EMP_SUBJ_YTD).
	!
	!	04/10/89 - Kevin Handy
	!		Fixed problem in OST_DEDMAX calculation where
	!		it was not dividing by the number of periods.
	!
	!	06/23/89 - Kevin Handy
	!		Opened PR_TEMP as TEMPORARY instead of trying
	!		to remember to delete it.
	!
	!	07/07/89 - Kevin Handy
	!		Fixed problem where it wasn't determining the
	!		correct code for the SX and SU taxes.
	!
	!	09/21/89 - Frank Starman
	!		Set convention. If FUI_MAX (SUI_MAX) is equal
	!		zero then the is no maximum
	!
	!	01/25/90 - Kevin Handy
	!		Modified calculation of SUBJECT_PREM to use the
	!		variable PR_TRN_PAY::FACTOR instead of FACTOR.
	!
	!	04/10/90 - Kevin Handy
	!		Modified to handle more accounts using
	!		overlay masks.
	!
	!	04/18/90 - Kevin Handy
	!		Fixed some problems that were found with the
	!		modifications done on 04/10/90.
	!
	!	04/19/90 - Kevin Handy
	!		Fixed problems where typeing ahead in the confirm
	!		display could cause the program to cresh because
	!		unsolicited input was left on.
	!
	!	10/25/90 - Kevin Handy
	!		Added code for "X" (eXcess).
	!
	!	12/26/90 - Kevin Handy
	!		Modifications for splitup of FICA into OASDI and HI.
	!
	!	01/09/91 - Kevin Handy
	!		Removed the PR_WC_DERFINITION file.
	!
	!	07/13/91 - Kevin Handy
	!		Removed the COM area PR_READ_SUBJWAGE, and
	!		changed PR_READ_SUBJWAGE to pass PR_EMP_TAXES.CH%
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_REG_ERNDED file which is no longer
	!		used in this program.
	!
	!	07/13/91 - Kevin Handy
	!		Remove commented out code.
	!
	!	07/25/91 - Kevin Handy
	!		Modified so that if there is no state in the SUI
	!		field in the master file, it will use the employees
	!		state, or his SW state, so that ?? is less likely to
	!		come up.  If none is found, trap the error in the
	!		SUI calculation and use a rate of 0% to be clearer.
	!
	!	01/13/92 - Kevin Handy
	!		Modified to ignore "A" records.
	!
	!	02/10/92 - Kevin Handy
	!		Added folder date to summary report.
	!
	!	10/13/92 - Kevin Handy
	!		Fixed bug in calculation of SUBJECT_PREM
	!		to use (FACTOR-1) instead of (1-FACTOR).
	!
	!	01/06/93 - Kevin Handy
	!		Fixed bug when accruing an accrued folder caused
	!		records to be lost.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/01/93 - Kevin Handy
	!		Commented out code that zeroed several arrays
	!		before calling PR_READ_SUBJWAGE, which then
	!		zeroed them again.
	!
	!	07/01/93 - Kevin Handy
	!		Modified call to PR_READ_SUBJWAGE to lose
	!		parameters that were not used for anything:
	!		REPORTABLE(), WKWRK(), and TAXWH().
	!
	!	10/18/93 - Kevin Handy
	!		Modifications in the SUI calculation to make it
	!		closely match how the final post calculates it,
	!		which comes out correctly.
	!
	!	02/03/94 - Kevin Handy
	!		Fix problem with losing accrual records, and
	!		previously accrued records.
	!
	!	02/04/94 - Kevin Handy
	!		Reformatted many lines to be less than 80 characters
	!		wide so that I could see what I'm editing.
	!
	!	02/04/94 - Kevin Handy
	!		Removed INSTR search around GL_ASSG_ACCTMASK because
	!		it turns out it slows it down more than it speeds
	!		it up.
	!
	!	03/14/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/03/94 - Kevin Handy
	!		Changed parameters to PR_READ_SUBJWAGE (lost
	!		PR_REG_TAXES.CH%). Removed all references to
	!		PR_REG_TAXES in main program. This in prep for
	!		fix to a SUTA/FUTA problem.
	!
	!	01/19/95 - Kevin Handy
	!		Don't post check record if there isn't a
	!		check number attached.
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Removed SUBJECT% parameter on PR_READ_SUBJTAX.
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	10/24/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/12/97 - Kevin Handy
	!		Use one more digit for fica rates
	!
	!	07/24/97 - Kevin Handy
	!		Allow old number of digits for FICA till we get
	!		everyone switched.
	!
	!	08/25/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	10/05/99 - Kevin Handy
	!		Fix TITLE$() parameter to GL_TRAN_POST so Alpha
	!		won't crash
	!
	!	05/08/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add several REGARDLESS clauses
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	04/18/2001 - Kevin Handy
	!		Add error trap for missing tax table.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	MAP (UTL_REPORTX)	UTL_REPORTX_CDD	UTL_REPORTX

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.HB"
	MAP (PR_CONTROL)	PR_CONTROL_CDD	PR_CONTROL

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP (PR_TAX_TABLE) PR_TAX_TABLE_CDD PR_TAX_TABLE

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP (PR_TAX_PKG)	PR_TAX_PKG_CDD	PR_TAX_PKG

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_FRI.HB"
	MAP (PR_TAX_PROFILE_FRI) PR_TAX_PROFILE_FRI_CDD	PR_TAX_PROFILE_FRI

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

	!
	! Need to include _F version so that variable length record
	! business will work.
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DEF.HB"
	MAP (PR_OVERHEAD_DEF)	PR_OVERHEAD_DEF_CDD	PR_OVERHEAD_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DESC.HB"
	MAP (PR_OVERHEAD_DESC)	PR_OVERHEAD_DESC_CDD	PR_OVERHEAD_DESC

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.HB"
	MAP (PR_WC_DESCR)	PR_WC_DESCR_CDD	PR_WC_DESCR

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.HB"
	MAP (PR_WC_INSURANCE)	PR_WC_INSURANCE_CDD	PR_WC_INSURANCE

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	MAP (PR_TEMP) &
		STRING	PR_TEMP.ACCT, &
		STRING	PR_TEMP.SUBACC, &
		STRING	PR_TEMP.OPER, &
		REAL	PR_TEMP.AMOUNT, &
		REAL	PR_TEMP.UNITS, &
		REAL	PR_TEMP.HOURS

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! Storage record structures
	!
	RECORD ACCRUAL_RECORD
		STRING ACCOUNT = 18%
		REAL   AMOUNT
	END RECORD

	DIM ACCRUAL_RECORD ACCRUAL(20%)

	!
	! External functions
	!
	EXTERNAL	LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! This dimension is used for current fui,fica,sui,ost calculation
	!
	DIM EMP_WH_CODE$(10%, 7%), &
		EMP_WH_CODE%(4%)

	DIM TAX_CODE$(10%), &
		TAXABLE(10%, 4%)

	!
	! This dimension is used to accumulate the indirect tax expense
	!
	DIM	FUI_TAX_EX_ACCT$(400%), &
		FUI_TAX_EX(400%), &
		SUI_TAX_EX_ACCT$(800%), &
		SUI_TAX_LIA_ACCT$(800%), &
		SUI_TAX_CODE$(800%), &
		SUI_TAX_PCT(800%), &
		SUI_TAX_EX(800%), &
		OST_TAX_EX_ACCT$(800%), &
		OST_TAX_LIA_ACCT$(800%), &
		OST_TAX_CODE$(800%), &
		OST_TAX_PCT(800%), &
		OST_TAX_EX(800%)


	RECORD FICA_TAX_CDD
		STRING ACCOUNT = 18%
		REAL   OASDI_WAGE
		REAL   HI_WAGE
	END RECORD

	DIM FICA_TAX_CDD FICA_TAX(400%)

	%PAGE

	ON ERROR GOTO 19000

	SCOPE::PRG_PROGRAM = READ_SYSPN

	!
	! Other Variables
	!
	SUBJECT_TYPE_TABLE$ = "FIR!FUI!SUI!OST!"

 Init:	!==============================================================
	! OPEN THE PRINT CONTROL FILE
	!==============================================================

	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	YYYY_PP$ = LEFT(UTL_REPORTX::OPTDEF(0%), 4%) + "_" + &
		TRM$(RIGHT(UTL_REPORTX::OPTDEF(0%), 5%))
	!++
	!
	! Abstract:FLD01
	!	^*(01) Post Period (YYYYPP)\*
	!	.p
	!	The ^*Post Period\* field specifies
	!	the accounting period into which
	!	payroll expenses are to be accrued.
	!	.p
	!	The period must be entered in YYYYPP format.
	!
	! Index:
	!	.x Post>Period
	!	.x Period>Post
	!
	!--


	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	!
	! Abstract:FLD02
	!	^*(02) Accrual Payroll Date\*
	!	.p
	!	The ^*Accrual Payroll Date\* field specifies the payroll file
	!	folder date which is to be accrued. The date is to be entered in
	!	MMDDYY format.
	!
	! Index:
	!	.x Accrual>Payroll Date>Post
	!	.x Post>Accrual>Payroll Date
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)
	TEMP_BATCH_NO$ = BATCH_NO$
	YYYY$ = LEFT(BATCH_NO$, 4%)

	FINAL_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	!
	! Abstract:FLD03
	!	^*(03) Final Payroll Date\*
	!	.p
	!	The ^*Final Payroll Date\* field specifies
	!	the payroll file folder date
	!	which may be equal or unequal to the Accrual Payroll Date.
	!	.p
	!	Several Accrual Payroll Dates may be rolled into a single payroll
	!	file folder by designating a single Final Payroll [File Folder] Date as
	!	several payroll folders are processed with the execution of the payroll
	!	accrual posting routine.
	!	.p
	!	Payroll data relating to a single payroll period only should be
	!	rolled into a Final Payroll Folder File Date.
	!
	! Index:
	!	.x Final>Payroll Date>Post
	!	.x Payroll Date>Final>Post
	!	.x Post>Payroll Date>Final
	!
	!--

	FINAL_BATCH_NO$ = DATE_STOREDATE(FINAL_BATCH_NO$)
		! Reformat to (YYYYMMDD)

	! Set update flag of accrued post
	!	1 - Updated to Register
	!	2 - Accrued Post
	!	4 - Final Post
	UPDATE_FLAG% = 2%

	!
	! Look up device
	!
	CALL  READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	!
	! Get GL_PERIOD file info
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		GET #GL_PERIOD.CH%, RECORD 1%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	CLOSE GL_PERIOD.CH%

	!
	! Check format, year, and period
	!
	IF LEN(YYYY_PP$) <> 7%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			LEFT(YYYY_PP$, 4%) + RIGHT(YYYY_PP$, 6%) + &
			" must have the following format YYYYPP", 0%)
		GOTO ExitProgram
	END IF

	IF RIGHT(YYYY_PP$, 6%) > FORMAT$(GL_PERIOD::FPFY, "<0>#") &
		OR RIGHT(YYYY_PP$, 6%) < "01"
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"The period must be within the range  of '01' to '" + &
			FORMAT$(GL_PERIOD::FPFY, "<0>#") + "'", 0%)
		GOTO ExitProgram
	END IF

	IF LEN(XLATE(LEFT(YYYY_PP$, 4%), STRING$(48%, 0%) + "0123456789")) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			LEFT(YYYY_PP$, 4%) + &
			" must have the following format YYYY", 0%)
		GOTO ExitProgram
	END IF

	IF YYYY_PP$ <= GL_PERIOD::YEAR + "_" + &
		FORMAT$(GL_PERIOD::LASTPERCLO, "<0>#")
	THEN
		CALL ENTR_3MESSAGE(SCOPE, YYYY_PP$ + " has been closed!", 0%)
		GOTO ExitProgram
	END IF

	!
	! Determine start and end date for payroll post
	!
	TEMP% = VAL%(MID(YYYY_PP$, 6%, 2%))
	END_DATE$ = GL_PERIOD::ENDDATE(TEMP%)
	TEMP% = TEMP% - 1%
	TEMP% = GL_PERIOD::FPFY IF TEMP% < 1%
	START_DATE$ = GL_PERIOD::ENDDATE(TEMP%)

310	!
	! Open GL period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.CRE"
	USE
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

320	!
	! Open chart of accounts
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.MOD"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.MOD"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

340	!
	! Open Payroll control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.MOD"

		GET #PR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "PR_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF PR_CONTROL::CLOSEFLAG = "2"
	THEN
		CALL HELP_3MESSAGE(SCOPE, &
			"PR Reset in process", "ERR", "PR_RESET", "ERROR_RESET")
		GOTO ExitProgram
	END IF

	IF PR_CONTROL::CLOSEFLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, &
			"PR update in process", "ERR", "PR_UPDATE", &
			"ERROR_UPDATE")
		GOTO ExitProgram
	END IF

350	!
	! Open Pay Accrual folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.UPD"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	PR_TRN_PAY_ACCRUAL.CH% = PR_TRN_PAY.CH%

360	!
	! Open Pay Final folder
	!
	IF BATCH_NO$ <> FINAL_BATCH_NO$ AND FINAL_BATCH_NO$ <> ""
	THEN
		BATCH_NO$ = FINAL_BATCH_NO$
		PR_TRN_PAY.CH% = 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.CRE"
		USE
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TRN_PAY_FINAL.CH% = PR_TRN_PAY.CH%
	END IF

370	!
	! Open Deduction Accrual folder
	!
	BATCH_NO$ = TEMP_BATCH_NO$

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.UPD"
	USE
		CONTINUE 390 IF ERR = 5%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	PR_TRN_DED_ACCRUAL.CH% = PR_TRN_DED.CH%

380	!
	! Open Deduction Final folder
	!
	IF BATCH_NO$ <> FINAL_BATCH_NO$ AND FINAL_BATCH_NO$ <> ""
	THEN
		BATCH_NO$ = FINAL_BATCH_NO$
		PR_TRN_DED.CH% = 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.CRE"
		USE
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		PR_TRN_DED_FINAL.CH% = PR_TRN_DED.CH%
	END IF

390	!
	! Open Check Accrual folder
	!
	BATCH_NO$ = TEMP_BATCH_NO$

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.UPD"
	USE
		CONTINUE 430 IF ERR = 5%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	PR_TRN_CHECK_ACCRUAL.CH% = PR_TRN_CHECK.CH%

400	!
	! Open Check Final folder
	!
	IF BATCH_NO$ <> FINAL_BATCH_NO$ AND FINAL_BATCH_NO$ <> ""
	THEN
		BATCH_NO$ = FINAL_BATCH_NO$
		PR_TRN_CHECK.CH% = 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.CRE"
		USE
			FILENAME$ = "PR_TRN_CHECK"
			CONTINUE HelpError
		END WHEN

		PR_TRN_CHECK_FINAL.CH% = PR_TRN_CHECK.CH%
	END IF

430	!
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
	! Handle new number of digits for percentage
	!
	IF FICA_EMPR_PCT > 0.100
	THEN
		FICA_EMPR_PCT = FICA_EMPR_PCT / 10.0
		FICA_EMPR_PCT_HI = FICA_EMPR_PCT_HI / 10.0
	END IF

	CLOSE PR_TAX_TABLE.CH%

440	!
	! Open Profile
	!
 !	FUI_PCT, FUI_MAX = 0.0
 !
 !	PR_ACCRUAL_ACCT$= STRING$(LEN(PR_TAX_PROFILE_F::PR_ACCRUAL_ACCT), 63%)
 !	FICA_EX_ACCT$ = STRING$(LEN(PR_TAX_PROFILE_F::FICA_EX_ACCT), 63%)
 !	FICA_LIA_ACCT_EMPR$ = &
 !		STRING$(LEN(PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPR), 63%)
 !	FUI_EX_ACCT$ = STRING$(LEN(PR_TAX_PROFILE_F::FUI_EX_ACCT), 63%)
 !	FUI_LIA_ACCT$ = STRING$(LEN(PR_TAX_PROFILE_F::FUI_LIA_ACCT), 63%)

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"

		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "F  ", REGARDLESS
	USE
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	FUI_PCT = PR_TAX_PROFILE_F::FUI_PCT / 100.0
	FUI_MAX = PR_TAX_PROFILE_F::FUI_MAX

	PR_ACCRUAL_ACCT$ = PR_TAX_PROFILE_F::PR_ACCRUAL_ACCT
	FICA_EX_ACCT$ = PR_TAX_PROFILE_F::FICA_EX_ACCT
	FICA_LIA_ACCT_EMPR$ = PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPR
	FUI_EX_ACCT$ = PR_TAX_PROFILE_F::FUI_EX_ACCT
	FUI_LIA_ACCT$ = PR_TAX_PROFILE_F::FUI_LIA_ACCT

450	!
	! Open Tax package
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
	USE
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

460	!
	! Open Tax Profile Fringe account table
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_FRI.OPN"
	USE
		CONTINUE 465 IF ERR = 5%
		FILENAME$ = "PR_TAX_PROFILE_FRI"
		CONTINUE HelpError
	END WHEN

465	!
	! Create temp payroll work file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY &
			( &
				PR_TEMP.ACCT, &
				PR_TEMP.SUBACC, &
				PR_TEMP.OPER), &
			ACCESS MODIFY, &
			ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
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
	! Open ERNDED Definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		CONTINUE 3000 IF ERR = 5%
		FILENAME$ = "PR_ERNDED_DEFINTION"
		CONTINUE HelpError
	END WHEN

	%PAGE


3000	!
	! Open up batch control file and get a batch number
	!
	CALL ASSG_CHANNEL(UTL_BATCH.CH%, STAT%)

	CALL ASSG_BATCH(UTL_BATCH.CH%, &
		UTL_WORK.DEV$, &
		"Initilize", &
		BATCH.NUMBER$, &
		"Folder Date " + BATCH_NO$, &
		"Payroll Accrual Journal Post", &
		"A", &
		YYYY_PP$, &
		"", &
		EXIT_STATUS%)

	IF (EXIT_STATUS% AND 2%)
	THEN
		CALL HELP_3MESSAGE(SCOPE, &
			"Batch Number Missing", "ERR", "UTL_BATCH", &
			"ERROR_MISBAT")
		GOTO Aborted1
	END IF

	IF (EXIT_STATUS% AND 4%)
	THEN
		GOTO Aborted1
	END IF

	!
	! Initilize for all passes
	!
	POST_STATUS% = 0%

	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	!
	! DO ALL PASSES
	!
	FOR PASS% = 1% TO 5%

		SELECT PASS%

		CASE 1%
			!
			! Post Process was interrupted
			!
			IF EXIT_STATUS% = 1%
			THEN
				SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

				CALL ENTR_3MESSAGE(SCOPE, &
					"Process was interrupted.  Restarting", 1%)

				!
				! Call the GL post function and remove
				! partial post
				!
				CALL GL_TRAN_POST(PASS%, &
					POST_STATUS%, &
					BATCH.NUMBER$, &
					UTL_BATCH.CH%, &
					GL_YYYY_PP.CH%, &
					GL_CHART.CH%, &
					START_DATE$, &
					END_DATE$, &
					TITLE$(), &
					"")

				GOTO Aborted1 IF (POST_STATUS% AND 4%)

				!
				! Call the PR post function for PAY
				! and remove partial post
				!
				CALL PR_TRAN_POST(PASS%, &
					POST_STATUS%, &
					UPDATE_FLAG%, &
					"PAY", &
					BATCH.NUMBER$, &
					UTL_BATCH.CH%, &
					PR_TRN_PAY_ACCRUAL.CH%, &
					PR_TRN_PAY_FINAL.CH%, &
					PR_EMP_MASTER.CH%)

				GOTO Aborted1 IF (POST_STATUS% AND 4%)

				!
				! Call the PR post function for DED
				! and remove partial post
				!
				CALL PR_TRAN_POST(PASS%, &
					POST_STATUS%, &
					UPDATE_FLAG%, &
					"DED", &
					BATCH.NUMBER$, &
					UTL_BATCH.CH%, &
					PR_TRN_DED_ACCRUAL.CH%, &
					PR_TRN_DED_FINAL.CH%, &
					PR_EMP_MASTER.CH%)

				GOTO Aborted1 IF (POST_STATUS% AND 4%)

				!
				! Call the PR post function for CHECK
				! and remove partial post
				!
				CALL PR_TRAN_POST(PASS%, &
					POST_STATUS%, &
					UPDATE_FLAG%, &
					"CHECK", &
					BATCH.NUMBER$, &
					UTL_BATCH.CH%, &
					PR_TRN_CHECK_ACCRUAL.CH%, &
					PR_TRN_CHECK_FINAL.CH%, &
					PR_EMP_MASTER.CH%)

				GOTO Aborted1 IF (POST_STATUS% AND 4%)

				SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
					LOC(OUTP_XUNSOL) BY VALUE, &
					LOC(SCOPE::SMG_KBID) BY VALUE)

			END IF

			GOTO 5990

		CASE 2%
			SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

			!
			! Create transmittal
			!
			CALL ENTR_3MESSAGE(SCOPE, &
				"Creating posting transmittals" + &
				". . . Posting Period " + YYYY_PP$, 1%)

			!
			! Set up to trap interrupt
			!
			SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
				LOC(OUTP_XUNSOL) BY VALUE, &
				LOC(SCOPE::SMG_KBID) BY VALUE)

			RRR_FLAG% = 0%

			RESET #PR_TRN_PAY_ACCRUAL.CH%

		CASE 3%
			!
			! Call the post function to Display transmittal
			!
			SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

			CALL GL_TRAN_POST(PASS%, &
				POST_STATUS%, &
				BATCH.NUMBER$, &
				UTL_BATCH.CH%, &
				GL_YYYY_PP.CH%, &
				GL_CHART.CH%, &
				START_DATE$, &
				END_DATE$, &
				TITLE$(), &
				"")

			GOTO Aborted IF POST_STATUS%

			SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
				LOC(OUTP_XUNSOL) BY VALUE, &
				LOC(SCOPE::SMG_KBID) BY VALUE)

			RRR_FLAG% = 0%

			GOTO 5990

		CASE 4%
			SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

			!
			! Post to gl and pr final
			!
			CALL ENTR_3MESSAGE(SCOPE, &
				"Posting to GL . . . Posting Period " + &
				YYYY_PP$, 1%)

			!
			! Set up to trap interrupt
			!
			SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
				LOC(OUTP_XUNSOL) BY VALUE, &
				LOC(SCOPE::SMG_KBID) BY VALUE)

			RRR_FLAG% = 0%

			RESET #PR_TRN_PAY_ACCRUAL.CH%

		CASE 5%
			!
			! Print transmittal and remove batch
			!
			!
			! Now, handle the finish up of the pass
			!
			TITLE$(1%) = &
				"Payroll Accrual Journal Transmittal - batch #" + &
				BATCH.NUMBER$
			TITLE$(2%) = "Payroll Folder Date " + &
				PRNT_DATE(BATCH_NO$, 8%)
			TITLE$(3%) = "posted to " + YYYY_PP$
			TITLE$(4%) = ""
			TITLE$(5%) = ""

			!
			! Call GL to print transmittal
			!
			CALL GL_TRAN_POST(5%, POST_STATUS%, &
				BATCH.NUMBER$, UTL_BATCH.CH%, &
				GL_YYYY_PP.CH%, GL_CHART.CH%, &
				"", "", TITLE$(), &
				"Payroll Accrual Journal Post")

			!
			! Complete process and remove batch control
			!
			CALL ASSG_BATCH(UTL_BATCH.CH%, &
				UTL_WORK.DEV$, &
				"Finished", &
				BATCH.NUMBER$, &
				"Folder Date " + BATCH_NO$, &
				"Payroll Accrual Journal Post", &
				"X", &
				YYYY_PP$, &
				"", &
				EXIT_STATUS%)

			IF EXIT_STATUS%
			THEN
				CALL HELP_3MESSAGE(SCOPE, &
					"Fatal error! PARTIAL POSTING Done only!", &
					"ERR", SCOPE::PRG_PROGRAM, "INTERRUPT")
				UTL_REPORTX::STAT = -1%
				GOTO ExitProgram
			END IF

			GOTO 5990

		END SELECT

		EOF% = 0%
		TEST_EMPNUM$ = ""

3010		!
		! Handle any special junk in RRR_FLAG%
		!
		GOSUB Interupt IF RRR_FLAG%

		!
		! Read in one record
		!
		WHEN ERROR IN
			GET #PR_TRN_PAY_ACCRUAL.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			IF ERR = 11%
			THEN
				EOF% = -1%
				CONTINUE 4000
			END IF
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		IF (PR_TRN_PAY::UPDATE_FLAG AND 4%) OR &
			(PR_TRN_PAY::UPDATE_FLAG AND 2%)
		THEN
			!
			! Call the PR post function for PAY
			!
			CALL PR_TRAN_POST(PASS%, &
				POST_STATUS%, &
				UPDATE_FLAG%, &
				"PAY", &
				BATCH.NUMBER$, &
				UTL_BATCH.CH%, &
				PR_TRN_PAY_ACCRUAL.CH%, &
				PR_TRN_PAY_FINAL.CH%, &
				PR_EMP_MASTER.CH%)

			GOTO Aborted1 IF (POST_STATUS% AND 4%) AND PASS% = 4%
			GOTO Aborted1 IF (POST_STATUS% AND 4%)

			GOTO 3010
		END IF

		!
		! Skip Accrual Accounts
		!
		IF PR_TRN_PAY::PTYPE = "A"
		THEN
			!
			! Call the PR post function for PAY
			!
			CALL PR_TRAN_POST(PASS%, &
				POST_STATUS%, &
				UPDATE_FLAG%, &
				"PAY", &
				BATCH.NUMBER$, &
				UTL_BATCH.CH%, &
				PR_TRN_PAY_ACCRUAL.CH%, &
				PR_TRN_PAY_FINAL.CH%, &
				PR_EMP_MASTER.CH%)

			GOTO Aborted1 IF (POST_STATUS% AND 4%) AND PASS% = 4%
			GOTO Aborted1 IF (POST_STATUS% AND 4%)

			GOTO 3010
		END IF

		!
		! Goto indirect tax calculation if not the same employee
		!
		GOTO 4000 IF PR_TRN_PAY::EMPNUM <> TEST_EMPNUM$ AND &
			TEST_EMPNUM$ <> ""

3020		SUBJECT_PREM = 0.0

		IF PR_TRN_PAY::RTYPE = "H" OR PR_TRN_PAY::RTYPE = "S" OR &
			PR_TRN_PAY::RTYPE = "X"
		THEN
			SUBJECT_PREM = FUNC_ROUND(PR_TRN_PAY::OVT_HR * ( &
				((PR_TRN_PAY::FACTOR / 100.0) - 1.0) * &
				PR_TRN_PAY::HOUR_RATE), 2%)
			SUBJECT_PREM = 0.0 IF SUBJECT_PREM < 0.0
		END IF

		!
		! Look Up new employee
		!
		IF TEST_EMPNUM$ <> PR_TRN_PAY::EMPNUM
		THEN
			PR_EMP_MASTER::ACCT	= PR_TRN_PAY::ACCT
			PR_EMP_MASTER::WC	= ""
			PR_EMP_MASTER::SUI_SW	= "??"
			PR_EMP_MASTER::EMPNAME	= &
				"????????????????????????????????????"

			WHEN ERROR IN
				GET #PR_EMP_MASTER.CH%, &
					KEY #0% EQ PR_TRN_PAY::EMPNUM, &
					REGARDLESS
			USE
				CONTINUE 3030 IF ERR = 155%
				FILENAME$ = "PR_EMP_MASTER"
				CONTINUE HelpError
			END WHEN
		END IF

3030		!
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
			FINAL_BATCH_NO$, &
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

3040		!
		! Set test values
		!
		TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM

		GOTO 3700 IF PASS% <> 2%

		!***************************************************************
		! This section accumulates wages to determine indirect tax
		! expense allocations
		!
		! Accumulate amount for fui, fica, sui, ost
		!
		PKG_WH_CODE$(4%) = "??"
		PKG_WH_CODE$(3%) = "??"
		WHEN ERROR IN
			FIND #PR_TAX_PKG.CH%, &
				KEY #0% EQ PR_TRN_PAY::TAX_PKG + "S", &
				REGARDLESS
		USE
			CONTINUE 3045 IF ERR = 155%
			FILENAME$ = "PR_TAX_PKG"
			CONTINUE HelpError
		END WHEN

3043		WHEN ERROR IN
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

3045		PKG_WH_CODE$(3%) = EDIT$(PR_EMP_MASTER::SUI_SW, -1%) &
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

3050		FOR TAX_TYPE% = 1% TO LEN(SUBJECT_TYPE_TABLE$) / 4%
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

			IF TAX_TYPE% > 2%
			THEN
				GOTO 3060 IF PKG_WH_CODE$(TAX_TYPE%) = &
					EMP_WH_CODE$(TAX_TYPE%, WH_LOOP%) &
					FOR WH_LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%)

				EMP_WH_CODE%(TAX_TYPE%), WH_LOOP% = &
					EMP_WH_CODE%(TAX_TYPE%) + 1%
				EMP_WH_CODE$(TAX_TYPE%, WH_LOOP%) = &
					PKG_WH_CODE$(TAX_TYPE%)

			END IF

3060			IF TAX_TYPE% < 3% OR PKG_WH_CODE$(TAX_TYPE%) <> ""
			THEN
				EMP_SUBJ_CUR(TAX_TYPE%, WH_LOOP%) = &
					FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, &
					WH_LOOP%) + &
					PR_TRN_PAY::GROSS, 2%)
			END IF

3090		NEXT TAX_TYPE%
		!
		! This is the end of the indirect tax accumulation section
		! for pay.
		!**************************************************************
		!
		! Get next record if this record has already been accrued
		!
		GOTO 3010 IF (PR_TRN_PAY::UPDATE_FLAG AND 2%)

		!
		! Accrue payroll
		!
		CALL GL_ASSG_ACCMASK(PR_ACCRUAL_ACCT$, &
			PR_EMP_MASTER::ACCT, &
			TEMP_ACCT$)

		FOR I% = 1% TO ACCRUAL%
			IF ACCRUAL(I%)::ACCOUNT = TEMP_ACCT$
			THEN
				ACCRUAL(I%)::AMOUNT = ACCRUAL(I%)::AMOUNT - &
					PR_TRN_PAY::GROSS
				GOTO 3100
			END IF
		NEXT I%

		ACCRUAL% = ACCRUAL% + 1%
		ACCRUAL(ACCRUAL%)::ACCOUNT = TEMP_ACCT$
		ACCRUAL(ACCRUAL%)::AMOUNT = -PR_TRN_PAY::GROSS

3100		! Jump over wc if wc = 0.0
		GOTO 3200 IF WC_BURDEN = 0.0

		!
		! Add wc burden expense
		!
		PR_TEMP.ACCT = WC_EXP$
		PR_TEMP.SUBACC = PR_TRN_PAY::SUBACC
		PR_TEMP.OPER = PR_TRN_PAY::OPER

		WHEN ERROR IN
			GET #PR_TEMP.CH%, KEY #0% EQ PR_TEMP.ACCT + &
				PR_TEMP.SUBACC + &
				PR_TEMP.OPER

			PR_TEMP.AMOUNT = PR_TEMP.AMOUNT + WC_BURDEN

			UPDATE #PR_TEMP.CH%
		USE
			CONTINUE 3110 IF ERR = 155%
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

		GOTO 3150

3110		! Can not find expense record

		PR_TEMP.ACCT = WC_EXP$
		PR_TEMP.SUBACC = PR_TRN_PAY::SUBACC
		PR_TEMP.OPER = PR_TRN_PAY::OPER
		PR_TEMP.AMOUNT = WC_BURDEN
		PR_TEMP.UNITS = 0.0
		PR_TEMP.HOURS = 0.0

		WHEN ERROR IN
			PUT #PR_TEMP.CH%
		USE
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

3150		!
		! Add wc burden liability
		!
		PR_TEMP.ACCT = WC_LIA$
		PR_TEMP.SUBACC = ""
		PR_TEMP.OPER = ""

		WHEN ERROR IN
			GET #PR_TEMP.CH%, KEY #0% EQ PR_TEMP.ACCT + &
				PR_TEMP.SUBACC + &
				PR_TEMP.OPER

			PR_TEMP.AMOUNT = PR_TEMP.AMOUNT - WC_BURDEN

			UPDATE #PR_TEMP.CH%
		USE
			CONTINUE 3160 IF ERR = 155%
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

		GOTO 3200

3160		! Can not find liability record

		PR_TEMP.ACCT = WC_LIA$
		PR_TEMP.SUBACC = ""
		PR_TEMP.OPER = ""
		PR_TEMP.AMOUNT = -WC_BURDEN
		PR_TEMP.UNITS = 0.0
		PR_TEMP.HOURS = 0.0

		WHEN ERROR IN
			PUT #PR_TEMP.CH%
		USE
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

3200		! Jump over OH if OH = 0.0
		GOTO 3300 IF OH_BURDEN = 0.0

		!
		! Add OH burden expense
		!
		PR_TEMP.ACCT = OH_EXP$
		PR_TEMP.SUBACC = PR_TRN_PAY::SUBACC
		PR_TEMP.OPER = PR_TRN_PAY::OPER

		WHEN ERROR IN
			GET #PR_TEMP.CH%, KEY #0% EQ PR_TEMP.ACCT + &
				PR_TEMP.SUBACC + &
				PR_TEMP.OPER

			PR_TEMP.AMOUNT = PR_TEMP.AMOUNT + OH_BURDEN

			UPDATE #PR_TEMP.CH%
		USE
			CONTINUE 3210 IF ERR = 155%
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

		GOTO 3250

3210		! Can not find expense record

		PR_TEMP.ACCT = OH_EXP$
		PR_TEMP.SUBACC = PR_TRN_PAY::SUBACC
		PR_TEMP.OPER = PR_TRN_PAY::OPER
		PR_TEMP.AMOUNT = OH_BURDEN
		PR_TEMP.UNITS = 0.0
		PR_TEMP.HOURS = 0.0

		WHEN ERROR IN
			PUT #PR_TEMP.CH%
		USE
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

3250		!
		! Add OH burden applied
		!
		PR_TEMP.ACCT = OH_APP$
		PR_TEMP.SUBACC = ""
		PR_TEMP.OPER = ""

		WHEN ERROR IN
			GET #PR_TEMP.CH%, KEY #0% EQ PR_TEMP.ACCT + &
				PR_TEMP.SUBACC + &
				PR_TEMP.OPER

			PR_TEMP.AMOUNT = PR_TEMP.AMOUNT - OH_BURDEN

			UPDATE #PR_TEMP.CH%
		USE
			CONTINUE 3260 IF ERR = 155%
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

		GOTO 3300

3260		! Can not find applied record

		PR_TEMP.ACCT = OH_APP$
		PR_TEMP.SUBACC = ""
		PR_TEMP.OPER = ""
		PR_TEMP.AMOUNT = -OH_BURDEN
		PR_TEMP.UNITS = 0.0
		PR_TEMP.HOURS = 0.0

		WHEN ERROR IN
			PUT #PR_TEMP.CH%
		USE
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

3300		TEMP = 0.0
		TEMP = TEMP + UP_BURDEN(LOOP%) FOR LOOP% = 1% TO 10%
		! Jump over UP if UP = 0.0
		GOTO 3400 IF TEMP = 0.0
		!
		! Add union pension burden
		!
		PR_TEMP.ACCT = UP_EXP$
		PR_TEMP.SUBACC = PR_TRN_PAY::SUBACC
		PR_TEMP.OPER = PR_TRN_PAY::OPER

		WHEN ERROR IN
			GET #PR_TEMP.CH%, KEY #0% EQ PR_TEMP.ACCT + &
				PR_TEMP.SUBACC + &
				PR_TEMP.OPER

			PR_TEMP.AMOUNT = PR_TEMP.AMOUNT + TEMP

			UPDATE #PR_TEMP.CH%
		USE
			CONTINUE 3310 IF ERR = 155%
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

		GOTO 3350

3310		! Can not find expense record

		PR_TEMP.ACCT = UP_EXP$
		PR_TEMP.SUBACC = PR_TRN_PAY::SUBACC
		PR_TEMP.OPER = PR_TRN_PAY::OPER
		PR_TEMP.AMOUNT = TEMP
		PR_TEMP.UNITS = 0.0
		PR_TEMP.HOURS = 0.0

		WHEN ERROR IN
			PUT #PR_TEMP.CH%
		USE
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

3350		!
		! Add wc union/pension liability
		!
		FOR LOOP% = 1% TO 10%

			GOTO 3390 IF UP_BURDEN(LOOP%)  = 0.0

			PR_TEMP.ACCT = UP_LIA$(LOOP%)
			PR_TEMP.SUBACC = ""
			PR_TEMP.OPER = ""

			WHEN ERROR IN
				GET #PR_TEMP.CH%, KEY #0% EQ PR_TEMP.ACCT + &
					PR_TEMP.SUBACC + &
					PR_TEMP.OPER

				PR_TEMP.AMOUNT = PR_TEMP.AMOUNT - UP_BURDEN(LOOP%)

				UPDATE #PR_TEMP.CH%
			USE
				CONTINUE 3360 IF ERR = 155%
				FILENAME$ = "PR_TEMP"
				CONTINUE HelpError
			END WHEN

			GOTO 3390

3360			! Can not find liability record

			PR_TEMP.ACCT = UP_LIA$(LOOP%)
			PR_TEMP.SUBACC = ""
			PR_TEMP.OPER = ""
			PR_TEMP.AMOUNT = -WC_BURDEN
			PR_TEMP.UNITS = 0.0
			PR_TEMP.HOURS = 0.0

			WHEN ERROR IN
				PUT #PR_TEMP.CH%
			USE
				FILENAME$ = "PR_TEMP"
				CONTINUE HelpError
			END WHEN

3390		NEXT LOOP%

3400		! Jump over PREM if PREM = 0.0
		GOTO 3500 IF PREM_AMT = 0.0
		!
		! Add overtime prem to prem account
		!
		PR_TEMP.ACCT = PREM_ACC$
		PR_TEMP.SUBACC = PR_TRN_PAY::SUBACC
		PR_TEMP.OPER = PR_TRN_PAY::OPER

		WHEN ERROR IN
			GET #PR_TEMP.CH%, KEY #0% EQ PR_TEMP.ACCT + &
				PR_TEMP.SUBACC + &
				PR_TEMP.OPER

			PR_TEMP.AMOUNT = PR_TEMP.AMOUNT + PREM_AMT

			UPDATE #PR_TEMP.CH%
		USE
			CONTINUE 3410 IF ERR = 155%
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

		GOTO 3500

3410		! Can not find premium record

		PR_TEMP.ACCT = PREM_ACC$
		PR_TEMP.SUBACC = PR_TRN_PAY::SUBACC
		PR_TEMP.OPER = PR_TRN_PAY::OPER
		PR_TEMP.AMOUNT = PREM_AMT
		PR_TEMP.UNITS = 0.0
		PR_TEMP.HOURS = 0.0

		WHEN ERROR IN
			PUT #PR_TEMP.CH%
		USE
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

3500		!
		! Resume
		!

3700		!*********************************************************
		! Generate a GL for payroll expense record to pass through
		! to the post function
		!*********************************************************
		GL_YYYY_PP::ACCT	= PR_TRN_PAY::ACCT
		GL_YYYY_PP::SOURCE	= "PR"
		GL_YYYY_PP::REFNO	= "PAYROLL"
		GL_YYYY_PP::TRANDAT	= TEMP_BATCH_NO$
		GL_YYYY_PP::DESCR	= PR_EMP_MASTER::EMPNAME
		GL_YYYY_PP::AMOUNT	= FUNC_ROUND(PR_TRN_PAY::GROSS - &
			PREM_AMT, 2%)
		GL_YYYY_PP::XREFNO	= PR_TRN_PAY::EMPNUM
		GL_YYYY_PP::POSTIM	= TIME_NOW
		GL_YYYY_PP::POSDAT	= DATE_TODAY
		GL_YYYY_PP::CKNO	= ""
		GL_YYYY_PP::TRANKEY	= ""
		GL_YYYY_PP::SUBACC	= PR_TRN_PAY::SUBACC
		GL_YYYY_PP::OPERATION	= PR_TRN_PAY::OPER
		GL_YYYY_PP::UNITS	= PR_TRN_PAY::PIECE
		GL_YYYY_PP::HOURS	= PR_TRN_PAY::REG_HR + &
			PR_TRN_PAY::OVT_HR
		GL_YYYY_PP::UPDSTA	= ""
		GL_YYYY_PP::BTHNUM	= BATCH.NUMBER$

		!
		! Call the post function
		!
		CALL GL_TRAN_POST(PASS%, &
			POST_STATUS%, &
			BATCH.NUMBER$, &
			UTL_BATCH.CH%, &
			GL_YYYY_PP.CH%, &
			GL_CHART.CH%, &
			START_DATE$, &
			END_DATE$, &
			TITLE$(), &
			"")

		GOTO Aborted1 IF (POST_STATUS% AND 4%) AND PASS% = 4%
		GOTO Aborted IF (POST_STATUS% AND 4%)

		!
		! Call the PR post function for PAY
		!
		CALL PR_TRAN_POST(PASS%, &
			POST_STATUS%, &
			UPDATE_FLAG%, &
			"PAY", &
			BATCH.NUMBER$, &
			UTL_BATCH.CH%, &
			PR_TRN_PAY_ACCRUAL.CH%, &
			PR_TRN_PAY_FINAL.CH%, &
			PR_EMP_MASTER.CH%)

		GOTO Aborted1 IF (POST_STATUS% AND 4%) AND PASS% = 4%
		GOTO Aborted1 IF (POST_STATUS% AND 4%)

		GOTO 3010

4000		!
		! Error resume point
		! This line number is here to be consistent with the
		! final post process
		!

		!
		! Jump over calculation of sui, fui, fica
		! if this is not pass 2
		!
		GOTO 4990 IF PASS% <> 2%

4300		!*************************************************************
		! Look up fica tax and taxable wages
		!*************************************************************

		CODE_LOOP% = 0%

		CALL PR_READ_SUBJWAGE(TEST_EMPNUM$, &
			YYYY$, &
			"FIR", &
			TAX_CODE$(), &
			TAXABLE(,), &
			CODE_LOOP%)

		EMP_SUBJ_YTD = 0.0
		EMP_SUBJ_YTD = FUNC_ROUND(EMP_SUBJ_YTD + &
			TAXABLE(1%, QTR%), 2%) &
			FOR QTR% = 1% TO 4%

		!--------------------------------------------------------------
		! Determine FICA Taxes
		!--------------------------------------------------------------

		FICA_WAGES, FICA_WAGES_HI = 0.0

		IF EMP_SUBJ_YTD < FICA_LIMIT_HI
		THEN
			EMP_SUBJ_YTD_HI = FUNC_ROUND(EMP_SUBJ_YTD + &
				EMP_SUBJ_CUR(1%, 1%), 2%)
			FICA_EXCESS_HI = FUNC_ROUND(EMP_SUBJ_YTD_HI - &
				FICA_LIMIT_HI, 2%)
			FICA_EXCESS_HI = 0.0 IF FICA_EXCESS_HI < 0.0
			FICA_WAGES_HI = FUNC_ROUND(EMP_SUBJ_CUR(1%, 1%) - &
				FICA_EXCESS_HI, 2%)
		END IF

		IF EMP_SUBJ_YTD < FICA_LIMIT
		THEN
			EMP_SUBJ_YTD = FUNC_ROUND(EMP_SUBJ_YTD + &
				EMP_SUBJ_CUR(1%, 1%), 2%)
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

		GOTO 4390 IF PR_CONTROL::OH_APPLY_FLAG <> "D"

4310		!
		! Look up first fica distribution labor account
		!
		WHEN ERROR IN
			FIND #PR_TAX_PROFILE_FRI.CH%, &
				KEY #0% GE "FI", &
				REGARDLESS
		USE
			CONTINUE 4390 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TAX_PROFILE_FRI"
			CONTINUE HelpError
		END WHEN

4320		WHEN ERROR IN
			GET #PR_TAX_PROFILE_FRI.CH%, REGARDLESS
		USE
			CONTINUE 4390 IF ERR = 11%
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

			GOTO 4390
		END IF

		GOTO 4320

4390		!
		! Add fica expense to fica expense array
		!
		GOTO 4395 IF TEMP_FICA_EX_ACCT$ = FICA_TAX(LOOP%)::ACCOUNT &
			FOR LOOP% = 1% TO FICA_EX_LOOP%

		FICA_EX_LOOP%, LOOP% = FICA_EX_LOOP% + 1%
		FICA_TAX(LOOP%)::ACCOUNT = TEMP_FICA_EX_ACCT$

4395		FICA_TAX(LOOP%)::OASDI_WAGE = &
			FUNC_ROUND(FICA_TAX(LOOP%)::OASDI_WAGE + FICA_WAGES, 2%)
		FICA_TAX(LOOP%)::HI_WAGE = &
			FUNC_ROUND(FICA_TAX(LOOP%)::HI_WAGE + FICA_WAGES_HI, 2%)

4400		!*************************************************************
		! Look up fui taxable wages
		!*************************************************************

		CODE_LOOP% = 0%

		CALL PR_READ_SUBJWAGE(TEST_EMPNUM$, &
			YYYY$, &
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

		FUI_WAGES = 0.0


		IF EMP_SUBJ_YTD < FUI_MAX OR FUI_MAX = 0.0
		THEN
			EMP_SUBJ_YTD = FUNC_ROUND(EMP_SUBJ_YTD + &
				EMP_SUBJ_CUR(2%, 1%), 2%)
			FUI_EXCESS = FUNC_ROUND(EMP_SUBJ_YTD - FUI_MAX, 2%)
			FUI_EXCESS = 0.0 IF FUI_EXCESS < 0.0 OR &
				FUI_MAX = 0.0
			FUI_WAGES = FUNC_ROUND(EMP_SUBJ_CUR(2%, 1%) - &
				FUI_EXCESS, 2%)
		END IF

		!-------------------------------------------------------------
		! Now figure out what account number to debit
		!-------------------------------------------------------------
		CALL GL_ASSG_ACCMASK(FUI_EX_ACCT$, &
			PR_EMP_MASTER::ACCT, &
			TEMP_FUI_EX_ACCT$)

		GOTO 4490 IF PR_CONTROL::OH_APPLY_FLAG <> "D"

4410		!
		! Look up first FUI distribution labor account
		!
		WHEN ERROR IN
			FIND #PR_TAX_PROFILE_FRI.CH%, &
				KEY #0% GE "FU", &
				REGARDLESS
		USE
			CONTINUE 4490 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TAX_PROFILE_FRI"
			CONTINUE HelpError
		END WHEN

4420		WHEN ERROR IN
			GET #PR_TAX_PROFILE_FRI.CH%, REGARDLESS
		USE
			CONTINUE 4490 IF ERR = 11%
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

4490		!
		! Add FUI expense to FUI expense array
		!
		GOTO 4495 IF TEMP_FUI_EX_ACCT$ = FUI_TAX_EX_ACCT$(LOOP%) &
			FOR LOOP% = 1% TO FUI_EX_LOOP%

		FUI_EX_LOOP%, LOOP% = FUI_EX_LOOP% + 1%
		FUI_TAX_EX_ACCT$(LOOP%) = TEMP_FUI_EX_ACCT$

4495		FUI_TAX_EX(LOOP%) = FUNC_ROUND(FUI_TAX_EX(LOOP%) + &
			FUI_WAGES, 2%)

4500		!*************************************************************
		! Look up sui taxable wages
		!*************************************************************

		CODE_LOOP% = EMP_WH_CODE%(3%)

		TAX_CODE$(I%) = EMP_WH_CODE$(3%, I%) FOR I% = 1% TO CODE_LOOP%

		CALL PR_READ_SUBJWAGE(TEST_EMPNUM$, &
			YYYY$, &
			"SUI", &
			TAX_CODE$(), &
			TAXABLE(,), &
			CODE_LOOP%)

		!-------------------------------------------------------------------
		! Detemine SUI taxes
		!-------------------------------------------------------------------

		FOR LOOP% = 1% TO EMP_WH_CODE%(3%)
			SUI_WAGES = 0.0
			IF TEST_SUI_CODE$ <> EMP_WH_CODE$(3%, LOOP%)
			THEN
				TEST_SUI_CODE$ = EMP_WH_CODE$(3%, LOOP%)
				SUI_PCT = 0.0
				SUI_MAX = 0.0

				SUI_EX_ACCT$ = STRING$(LEN( &
					PR_TAX_PROFILE_S::SUI_EX_ACCT), 63%)
				SUI_LIA_ACCT$ = STRING$(LEN( &
					PR_TAX_PROFILE_S::SUI_LIA_ACCT), 63%)

				WHEN ERROR IN
					GET #PR_TAX_PROFILE.CH%, &
						KEY #0% EQ "S" + EMP_WH_CODE$(3%, LOOP%), &
						REGARDLESS
				USE
					CONTINUE 4595 IF ERR = 155% OR ERR = 9%
					FILENAME$ = "PR_TAX_PROFILE"
					CONTINUE HelpError
				END WHEN

				SUI_PCT = PR_TAX_PROFILE_S::SUI_PCT / 100.0
				SUI_MAX = PR_TAX_PROFILE_S::SUI_MAX

				SUI_EX_ACCT$ = PR_TAX_PROFILE_S::SUI_EX_ACCT
				SUI_LIA_ACCT$ = PR_TAX_PROFILE_S::SUI_LIA_ACCT
			END IF

4540			EMP_SUBJ_YTD = 0.0
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
				SUI_EXCESS = 0.0 IF SUI_EXCESS < 0.0  OR &
						SUI_MAX = 0.0
				SUI_WAGES = FUNC_ROUND(EMP_SUBJ_CUR(3%, &
					LOOP%) - SUI_EXCESS, 2%)
			END IF

			!-------------------------------------------------------
			! Now figure out what account number to debit
			!-------------------------------------------------------
			CALL GL_ASSG_ACCMASK(SUI_EX_ACCT$, &
				PR_EMP_MASTER::ACCT, &
				TEMP_SUI_EX_ACCT$)

			CALL GL_ASSG_ACCMASK(SUI_LIA_ACCT$, &
				PR_EMP_MASTER::ACCT, &
				TEMP_SUI_LIA_ACCT$)

			GOTO 4590 IF PR_CONTROL::OH_APPLY_FLAG <> "D"

4550			!
			! Look up first SUI distribution labor account
			!
			WHEN ERROR IN
				FIND #PR_TAX_PROFILE_FRI.CH%, &
					KEY #0% GE "SU", &
					REGARDLESS
			USE
				CONTINUE 4590 IF ERR = 155% OR ERR = 9%
				FILENAME$ = "PR_TAX_PROFILE_FRI"
				CONTINUE HelpError
			END WHEN

4560			WHEN ERROR IN
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

4590			!
			! Add SUI expense to SUI expense array
			!
			GOTO 4595 IF TEMP_SUI_EX_ACCT$ + TEMP_SUI_LIA_ACCT$ = &
				SUI_TAX_EX_ACCT$(X%) + SUI_TAX_LIA_ACCT$(X%) &
				FOR X% = 1% TO SUI_EX_LOOP%

			SUI_EX_LOOP%, X% = SUI_EX_LOOP% + 1%
			SUI_TAX_EX_ACCT$(X%) = TEMP_SUI_EX_ACCT$
			SUI_TAX_LIA_ACCT$(X%) = TEMP_SUI_LIA_ACCT$
			SUI_TAX_CODE$(X%) = TEST_SUI_CODE$
			SUI_TAX_PCT(X%) = SUI_PCT
			SUI_TAX_EX(X%) = 0.0

4595			SUI_TAX_EX(X%) = FUNC_ROUND(SUI_TAX_EX(X%) + &
				SUI_WAGES, 2%)

		NEXT LOOP%

4600		!*************************************************************
		! Look up OST taxable wages
		!*************************************************************

		CODE_LOOP% = EMP_WH_CODE%(4%)

		TAX_CODE$(I%) = EMP_WH_CODE$(4%, I%) FOR I% = 1% TO CODE_LOOP%

		CALL PR_READ_SUBJWAGE(TEST_EMPNUM$, &
			YYYY$, &
			"OST", &
			TAX_CODE$(), &
			TAXABLE(,), &
			CODE_LOOP%)

		!-------------------------------------------------------------
		! Detemine OST taxes
		!-------------------------------------------------------------

		FOR LOOP% = 1% TO EMP_WH_CODE%(4%)
			OST_WAGES = 0.0
			IF TEST_OST_CODE$ <> EMP_WH_CODE$(4%, LOOP%)
			THEN
				TEST_OST_CODE$ = EMP_WH_CODE$(4%, LOOP%)
				OST_PCT = 0.0
				OST_MAX = 0.0
				OST_DEDMAX = 0.0

				OST_EX_ACCT$ = STRING$(LEN( &
					PR_TAX_PROFILE_S::OST_EX_ACCT), 63%)
				OST_LIA_ACCT$ = STRING$(LEN( &
					PR_TAX_PROFILE_S::OST_LIA_ACCT), 63%)

				WHEN ERROR IN
					GET #PR_TAX_PROFILE.CH%, &
						KEY #0% EQ "S" + EMP_WH_CODE$(4%, LOOP%), &
						REGARDLESS
				USE
					CONTINUE 4695 IF ERR = 155%
					FILENAME$ = "PR_TAX_PROFILE"
					CONTINUE HelpError
				END WHEN

				OST_PCT = PR_TAX_PROFILE_S::OST_PCT / 100.0
				OST_MAX = PR_TAX_PROFILE_S::OST_MAX
				OST_DEDMAX = PR_TAX_PROFILE_S::OST_DEDMAX / &
					PR_EMP_MASTER::PAYFREQ

				OST_EX_ACCT$ = PR_TAX_PROFILE_S::OST_EX_ACCT
				OST_LIA_ACCT$ = PR_TAX_PROFILE_S::OST_LIA_ACCT
			END IF

4640			EMP_SUBJ_YTD = 0.0
			EMP_SUBJ_YTD = FUNC_ROUND(EMP_SUBJ_YTD + &
				TAXABLE(LOOP%, QTR%), 2%) &
				FOR QTR% = 1% TO 4%

			IF OST_MAX <> 0.0
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
					EMP_SUBJ_CUR(4%, LOOP%), 2%)

				IF EMP_SUBJ_DED - EMP_SUBJ_CUR(4%, LOOP%) < &
					OST_DEDMAX
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

4650			!
			! Look up first OST distribution labor account
			!
			WHEN ERROR IN
				FIND #PR_TAX_PROFILE_FRI.CH%, &
					KEY #0% GE "SX", &
					REGARDLESS
			USE
				CONTINUE 4690 IF ERR = 155% OR ERR = 9%
				FILENAME$ = "PR_TAX_PROFILE_FRI"
				CONTINUE HelpError
			END WHEN

4660			WHEN ERROR IN
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

4690			!
			! Add OST expense to OST expense array
			!
			GOTO 4695 IF TEMP_OST_EX_ACCT$ + OST_LIA_ACCT$ = &
				OST_TAX_EX_ACCT$(X%) + OST_TAX_LIA_ACCT$(X%) &
				FOR X% = 1% TO OST_EX_LOOP%

			OST_EX_LOOP%, X% = OST_EX_LOOP% + 1%
			OST_TAX_EX_ACCT$(X%) = TEMP_OST_EX_ACCT$
			OST_TAX_LIA_ACCT$(X%) = OST_LIA_ACCT$
			OST_TAX_CODE$(X%) = TEST_OST_CODE$
			OST_TAX_PCT(X%) = OST_PCT
			OST_TAX_EX(X%) = 0.0

4695			OST_TAX_EX(X%) = FUNC_ROUND(OST_TAX_EX(X%) + &
				OST_WAGES, 2%)

		NEXT LOOP%

4990		!
		! Zero employee array in preparation for next employee
		!
		EMP_SUBJ_CUR(I%,J%) = 0.0 FOR J% = 0% TO 4% FOR I% = 1% TO 10%
		EMP_WH_CODE$(I%,J%) = "" FOR J% = 1% TO 4% FOR I% = 1% TO 10%
		EMP_SUBJ_DED = 0.0
		EMP_WH_CODE%(I%) = 0% FOR I% = 1% TO 4%

		!
		! Set up for fica and fui
		!
		EMP_WH_CODE%(1%) = 1%
		EMP_WH_CODE%(2%) = 1%


		IF EOF% = 0%
		THEN
			GOTO 3020
		END IF

5100		!
		! Post the accrual amount to the GL
		!
		GL_YYYY_PP::SOURCE	= "PR"
		GL_YYYY_PP::REFNO	= "ACCRUAL"
		GL_YYYY_PP::TRANDAT	= TEMP_BATCH_NO$
		GL_YYYY_PP::DESCR	= "PAYROLL ACCRUAL"
		GL_YYYY_PP::XREFNO	= ""
		GL_YYYY_PP::POSTIM	= TIME_NOW
		GL_YYYY_PP::POSDAT	= DATE_TODAY
		GL_YYYY_PP::CKNO	= ""
		GL_YYYY_PP::TRANKEY	= ""
		GL_YYYY_PP::SUBACC	= ""
		GL_YYYY_PP::OPERATION	= ""
		GL_YYYY_PP::UNITS	= 0.0
		GL_YYYY_PP::HOURS	= 0.0

		GL_YYYY_PP::UPDSTA	= ""
		GL_YYYY_PP::BTHNUM	= BATCH.NUMBER$

		!
		! Call the post function
		!
		FOR LOOP% = 1% TO ACCRUAL%

			GL_YYYY_PP::ACCT	= ACCRUAL(LOOP%)::ACCOUNT
			GL_YYYY_PP::AMOUNT	= ACCRUAL(LOOP%)::AMOUNT

			CALL GL_TRAN_POST(PASS%, &
				POST_STATUS%, &
				BATCH.NUMBER$, &
				UTL_BATCH.CH%, &
				GL_YYYY_PP.CH%, &
				GL_CHART.CH%, &
				START_DATE$, &
				END_DATE$, &
				TITLE$(), &
				"")

			GOTO Aborted1 IF (POST_STATUS% AND 4%) AND PASS% = 4%
			GOTO Aborted IF (POST_STATUS% AND 4%)
		NEXT LOOP%

5200		!
		! Read Payroll temp file and post to gl
		!
		WHEN ERROR IN
			RESET #PR_TEMP.CH%
		USE
			CONTINUE 5300
		END WHEN

5220		!
		! Look for interrupt
		!
		GOSUB Interupt IF RRR_FLAG%

		WHEN ERROR IN
			GET #PR_TEMP.CH%
		USE
			CONTINUE 5300 IF ERR = 11%
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

		GL_YYYY_PP::ACCT	= PR_TEMP.ACCT
		GL_YYYY_PP::SOURCE	= "PR"
		GL_YYYY_PP::REFNO	= "BURDEN"
		GL_YYYY_PP::TRANDAT	= TEMP_BATCH_NO$
		GL_YYYY_PP::DESCR	= "PAYROLL BURDEN"
		GL_YYYY_PP::AMOUNT	= PR_TEMP.AMOUNT
		GL_YYYY_PP::XREFNO	= ""
		GL_YYYY_PP::POSTIM	= TIME_NOW
		GL_YYYY_PP::POSDAT	= DATE_TODAY
		GL_YYYY_PP::CKNO	= ""
		GL_YYYY_PP::TRANKEY	= ""
		GL_YYYY_PP::SUBACC	= PR_TEMP.SUBACC
		GL_YYYY_PP::OPERATION	= PR_TEMP.OPER
		GL_YYYY_PP::UNITS	= PR_TEMP.UNITS
		GL_YYYY_PP::HOURS	= PR_TEMP.HOURS

		GL_YYYY_PP::UPDSTA	= ""
		GL_YYYY_PP::BTHNUM	= BATCH.NUMBER$

		!
		! Call the post function
		!
		CALL GL_TRAN_POST(PASS%, &
			POST_STATUS%, &
			BATCH.NUMBER$, &
			UTL_BATCH.CH%, &
			GL_YYYY_PP.CH%, &
			GL_CHART.CH%, &
			START_DATE$, &
			END_DATE$, &
			TITLE$(), &
			"")

		GOTO Aborted1 IF (POST_STATUS% AND 4%) AND PASS% = 4%
		GOTO Aborted IF (POST_STATUS% AND 4%)

		GOTO 5220

5300		!
		! Post indirect employer payroll tax expenses
		!

		!
		! Default gl fields
		!
		GL_YYYY_PP::SOURCE	= "PR"
		GL_YYYY_PP::REFNO	= "INDIRECT TAX"
		GL_YYYY_PP::TRANDAT	= BATCH_NO$
		GL_YYYY_PP::DESCR	= "INDIRECT TAX EXPENSES"
		GL_YYYY_PP::XREFNO	= ""
		GL_YYYY_PP::POSTIM	= TIME_NOW
		GL_YYYY_PP::POSDAT	= DATE_TODAY
		GL_YYYY_PP::CKNO	= ""
		GL_YYYY_PP::TRANKEY	= ""
		GL_YYYY_PP::SUBACC	= ""
		GL_YYYY_PP::OPERATION	= ""
		GL_YYYY_PP::UNITS	= 0.0
		GL_YYYY_PP::HOURS	= 0.0

		GL_YYYY_PP::UPDSTA	= ""
		GL_YYYY_PP::BTHNUM	= BATCH.NUMBER$

		!
		! Loop for fica indirect calculation
		!
		FOR LOOP% = 1% TO FICA_EX_LOOP%
			!
			! Add in fica expense and liability
			!
			GL_YYYY_PP::AMOUNT = FUNC_ROUND( &
				FICA_EMPR_PCT * FICA_TAX(LOOP%)::OASDI_WAGE + &
				FICA_EMPR_PCT_HI * FICA_TAX(LOOP%)::HI_WAGE, &
				2%)

			GL_YYYY_PP::ACCT = FICA_TAX(LOOP%)::ACCOUNT

			! Expense
			IF GL_YYYY_PP::AMOUNT <> 0.0
			THEN
				!
				! Call the post function
				!
				CALL GL_TRAN_POST(PASS%, &
					POST_STATUS%, &
					BATCH.NUMBER$, &
					UTL_BATCH.CH%, &
					GL_YYYY_PP.CH%, &
					GL_CHART.CH%, &
					START_DATE$, &
					END_DATE$, &
					TITLE$(), &
					"")

				GOTO Aborted1 &
					IF (POST_STATUS% AND 4%) AND PASS% = 4%
				GOTO Aborted IF (POST_STATUS% AND 4%)

				!
				! Now post the liability
				!
				CALL GL_ASSG_ACCMASK(FICA_LIA_ACCT_EMPR$, &
					FICA_TAX(LOOP%)::ACCOUNT, &
					TEMP_FICA_ACCT$)

				GL_YYYY_PP::ACCT = TEMP_FICA_ACCT$

				GL_YYYY_PP::AMOUNT = -GL_YYYY_PP::AMOUNT

				!
				! Call the post function
				!
				CALL GL_TRAN_POST(PASS%, &
					POST_STATUS%, &
					BATCH.NUMBER$, &
					UTL_BATCH.CH%, &
					GL_YYYY_PP.CH%, &
					GL_CHART.CH%, &
					START_DATE$, &
					END_DATE$, &
					TITLE$(), &
					"")

				GOTO Aborted1 &
					IF (POST_STATUS% AND 4%) AND PASS% = 4%
				GOTO Aborted IF (POST_STATUS% AND 4%)
			END IF
		NEXT LOOP%

		!
		! Loop for fui indirect calculation
		!
		FOR LOOP% = 1% TO FUI_EX_LOOP%
			!
			! Post fui expense
			!
			GL_YYYY_PP::AMOUNT = FUNC_ROUND(FUI_PCT * &
					FUI_TAX_EX(LOOP%), 2%)

			GL_YYYY_PP::ACCT = FUI_TAX_EX_ACCT$(LOOP%)

			! Expense
			IF GL_YYYY_PP::AMOUNT <> 0.0
			THEN
				!
				! Call the post function
				!
				CALL GL_TRAN_POST(PASS%, &
					POST_STATUS%, &
					BATCH.NUMBER$, &
					UTL_BATCH.CH%, &
					GL_YYYY_PP.CH%, &
					GL_CHART.CH%, &
					START_DATE$, &
					END_DATE$, &
					TITLE$(), &
					"")

				GOTO Aborted1 &
					IF (POST_STATUS% AND 4%) AND PASS% = 4%
				GOTO Aborted IF (POST_STATUS% AND 4%)


				!
				! Now post fui liability
				!
				CALL GL_ASSG_ACCMASK(FUI_LIA_ACCT$, &
					FUI_TAX_EX_ACCT$(LOOP%), &
					TEMP_FUI_ACCT$)

				GL_YYYY_PP::ACCT = TEMP_FUI_ACCT$

				GL_YYYY_PP::AMOUNT = -GL_YYYY_PP::AMOUNT

				!
				! Call the post function
				!
				CALL GL_TRAN_POST(PASS%, &
					POST_STATUS%, &
					BATCH.NUMBER$, &
					UTL_BATCH.CH%, &
					GL_YYYY_PP.CH%, &
					GL_CHART.CH%, &
					START_DATE$, &
					END_DATE$, &
					TITLE$(), &
					"")

				GOTO Aborted1 &
					IF (POST_STATUS% AND 4%) AND PASS% = 4%
				GOTO Aborted IF (POST_STATUS% AND 4%)

			END IF

		NEXT LOOP%

		!
		! Post sui expense and liability
		!
		FOR LOOP% = 1% TO SUI_EX_LOOP%

			GL_YYYY_PP::DESCR = "INDIRECT TAX EXPENSES FOR " + &
				SUI_TAX_CODE$(LOOP%)

			GL_YYYY_PP::AMOUNT = FUNC_ROUND(SUI_TAX_PCT(LOOP%) * &
					SUI_TAX_EX(LOOP%), 2%)

			GL_YYYY_PP::ACCT = SUI_TAX_EX_ACCT$(LOOP%)

			! Expense
			IF GL_YYYY_PP::AMOUNT <> 0.0
			THEN
				!
				! Call the post function
				!
				CALL GL_TRAN_POST(PASS%, &
					POST_STATUS%, &
					BATCH.NUMBER$, &
					UTL_BATCH.CH%, &
					GL_YYYY_PP.CH%, &
					GL_CHART.CH%, &
					START_DATE$, &
					END_DATE$, &
					TITLE$(), &
					"")

				GOTO Aborted1 &
					IF (POST_STATUS% AND 4%) AND PASS% = 4%
				GOTO Aborted IF (POST_STATUS% AND 4%)
			END IF

			GL_YYYY_PP::ACCT = SUI_TAX_LIA_ACCT$(LOOP%)

			! Liability
			IF GL_YYYY_PP::AMOUNT <> 0.0
			THEN
				GL_YYYY_PP::AMOUNT = &
					FUNC_ROUND(-GL_YYYY_PP::AMOUNT, 2%)

				!
				! Call the post function
				!
				CALL GL_TRAN_POST(PASS%, &
					POST_STATUS%, &
					BATCH.NUMBER$, &
					UTL_BATCH.CH%, &
					GL_YYYY_PP.CH%, &
					GL_CHART.CH%, &
					START_DATE$, &
					END_DATE$, &
					TITLE$(), &
					"")

				GOTO Aborted1 &
					IF (POST_STATUS% AND 4%) AND PASS% = 4%
				GOTO Aborted IF (POST_STATUS% AND 4%)
			END IF

		NEXT LOOP%

		!
		! Post ost expense and liability
		!
		FOR LOOP% = 1% TO OST_EX_LOOP%

			GL_YYYY_PP::DESCR = "INDIRECT TAX EXPENSES FOR " + &
				OST_TAX_CODE$(LOOP%)

			GL_YYYY_PP::AMOUNT = FUNC_ROUND(OST_TAX_PCT(LOOP%) * &
					OST_TAX_EX(LOOP%), 2%)

			GL_YYYY_PP::ACCT = OST_TAX_EX_ACCT$(LOOP%)

			! Expense
			IF GL_YYYY_PP::AMOUNT <> 0.0
			THEN
				!
				! Call the post function
				!
				CALL GL_TRAN_POST(PASS%, &
					POST_STATUS%, &
					BATCH.NUMBER$, &
					UTL_BATCH.CH%, &
					GL_YYYY_PP.CH%, &
					GL_CHART.CH%, &
					START_DATE$, &
					END_DATE$, &
					TITLE$(), &
					"")

				GOTO Aborted1 &
					IF (POST_STATUS% AND 4%) AND PASS% = 4%
				GOTO Aborted IF (POST_STATUS% AND 4%)
			END IF

			GL_YYYY_PP::ACCT = OST_TAX_LIA_ACCT$(LOOP%)

			! Liability
			IF GL_YYYY_PP::AMOUNT <> 0.0
			THEN
				GL_YYYY_PP::AMOUNT = &
					FUNC_ROUND(-GL_YYYY_PP::AMOUNT, 2%)

				!
				! Call the post function
				!
				CALL GL_TRAN_POST(PASS%, &
					POST_STATUS%, &
					BATCH.NUMBER$, &
					UTL_BATCH.CH%, &
					GL_YYYY_PP.CH%, &
					GL_CHART.CH%, &
					START_DATE$, &
					END_DATE$, &
					TITLE$(), &
					"")

				GOTO Aborted1 &
					IF (POST_STATUS% AND 4%) AND PASS% = 4%
				GOTO Aborted IF (POST_STATUS% AND 4%)
			END IF

		NEXT LOOP%

5400		!
		! Jump over post to ded and check if not pass 4
		!
		GOTO 5990 IF PASS% <> 4%

		WHEN ERROR IN
			RESET #PR_TRN_DED_ACCRUAL.CH%
		USE
			CONTINUE 5500 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

5420		!
		! Get deduction record
		!
		WHEN ERROR IN
			GET #PR_TRN_DED_ACCRUAL.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 5500 IF ERR = 11%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		!
		! Call the PR post function for DED
		!
		CALL PR_TRAN_POST(PASS%, &
			POST_STATUS%, &
			UPDATE_FLAG%, &
			"DED", &
			BATCH.NUMBER$, &
			UTL_BATCH.CH%, &
			PR_TRN_DED_ACCRUAL.CH%, &
			PR_TRN_DED_FINAL.CH%, &
			PR_EMP_MASTER.CH%)

		GOTO Aborted1 IF (POST_STATUS% AND 4%) AND PASS% = 4%
		GOTO Aborted1 IF (POST_STATUS% AND 4%)

		GOTO 5420

5500		!
		! Post checks
		!
		WHEN ERROR IN
			RESET #PR_TRN_CHECK_ACCRUAL.CH%
		USE
			CONTINUE 5990 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TRN_CHECK"
			CONTINUE HelpError
		END WHEN

5520		!
		! Get check record
		!
		WHEN ERROR IN
			GET #PR_TRN_CHECK_ACCRUAL.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 5990 IF ERR = 11%
			FILENAME$ = "PR_TRN_CHECK"
			CONTINUE HelpError
		END WHEN

		! Don't post check if there isn't any check number

		IF PR_TRN_CHECK::CHECK <> ""
		THEN
			!
			! Call the PR post function for CHECK
			!
			CALL PR_TRAN_POST(PASS%, &
				POST_STATUS%, &
				UPDATE_FLAG%, &
				"CHECK", &
				BATCH.NUMBER$, &
				UTL_BATCH.CH%, &
				PR_TRN_CHECK_ACCRUAL.CH%, &
				PR_TRN_CHECK_FINAL.CH%, &
				PR_EMP_MASTER.CH%)

			GOTO Aborted1 IF (POST_STATUS% AND 4%) AND PASS% = 4%
			GOTO Aborted1 IF (POST_STATUS% AND 4%)
		END IF

		GOTO 5520

5990	NEXT PASS%

6200	!
	! Jump of removal if PR_TRN_PAY_FINAL.CH% = 0%
	!
	GOTO 6250 IF PR_TRN_PAY_FINAL.CH% = 0%

	!
	! Remove file
	!
	CLOSE PR_TRN_PAY_ACCRUAL.CH%
 !	WHEN ERROR IN
 !		KILL PR_TRN_PAY.DEV$ + "PR_TRN_PAY_" + TEMP_BATCH_NO$ + ".JRL" &
 !			FOR LOOP% = 1% TO 5%
 !	USE
 !		CONTINUE 6210
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(PR_TRN_PAY.DEV$ + &
		"PR_TRN_PAY_" + TEMP_BATCH_NO$ + ".JRL;*")

6210	CLOSE PR_TRN_DED_ACCRUAL.CH%
 !	WHEN ERROR IN
 !		KILL PR_TRN_DED.DEV$ + "PR_TRN_DED_" + TEMP_BATCH_NO$ + ".JRL" &
 !			FOR LOOP% = 1% TO 5%
 !	USE
 !		CONTINUE 6220
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(PR_TRN_DED.DEV$ + &
		"PR_TRN_DED_" + TEMP_BATCH_NO$ + ".JRL;*")

6220	CLOSE PR_TRN_CHECK_ACCRUAL.CH%
 !	WHEN ERROR IN
 !		KILL PR_TRN_CHECK.DEV$ + "PR_TRN_CHECK_" + TEMP_BATCH_NO$ + ".JRL" &
 !			FOR LOOP% = 1% TO 5%
 !	USE
 !		CONTINUE 6250
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(PR_TRN_CHECK.DEV$ + &
		"PR_TRN_CHECK_" + TEMP_BATCH_NO$ + ".JRL;*")

6250	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	IF UTL_REPORTX::PRINTTO <> 1%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Process complete ", 4%)
	END IF

	%PAGE

7300	!
	! Update payroll control file
	!
	WHEN ERROR IN
		GET #PR_CONTROL.CH%, RECORD 1%

		PR_CONTROL::POST_DATE = BATCH_NO$

		UPDATE #PR_CONTROL.CH%

		CLOSE PR_CONTROL.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		FILENAME$ = "PR_CONTROL"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ExitProgram:
10000	!******************************************************************
	! Exit normally
	!******************************************************************

	!
	! Kill work file
	!
	CLOSE PR_TEMP.CH%


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

	CALL ASSG_BATCH(UTL_BATCH.CH%, &
		UTL_WORK.DEV$, &
		"Finish/Abort", &
		BATCH.NUMBER$, &
		"Folder Date " + BATCH_NO$, &
		"Payroll Accrual Journal Post", &
		"X", &
		YYYY_PP$, &
		"", &
		EXIT_STATUS%)

 Aborted1:
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%PAGE

 Interupt:
	!********************************************************************
	! Handle any special junk in RRR_FLAG%
	!********************************************************************
	!
	! Handle any special junk in RRR_FLAG%
	!
	SELECT RRR_FLAG%

	!
	! Repaint screen
	!
	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Help
	!
	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	END SELECT

	RRR_FLAG% = 0%

	RETURN

	%PAGE

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

	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

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
