1	%TITLE "Post Payroll Employee Accrual Program"
	%SBTTL "PR_POST_EMPACCRUAL"
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
	! ID:PREMP
	!
	! Abstract:HELP
	!	.p
	!	The ^*Post Employee Accruals\* option
	!	posts the accruals to the payroll files and,
	!	on a selective basis, to the General Ledger.
	!	.p
	!	For example, if vacation and sick pay benefits are to be accrued
	!	for each employee, it is possible to accrue the dollar value of
	!	vacation accruals only in the General Ledger, but post both vacation
	!	and sick pay hours accrued to the payroll files.
	!	.note
	!	The dollar value of the accruals posted to the
	!	General Ledger is based on current rates for the
	!	period. Employee pay rate changes, of course, affect
	!	the value of previously posted accruals. Accordingly
	!	a journal entry is required to adjust the dollar value
	!	of accruals whenever pay rate changes are made. The
	!	total dollar value displayed for a benefit on the
	!	Employee Accruals Report which coincides with the end
	!	of an accounting period should agree with the related
	!	liability account in the General Ledger.
	!	.end note
	!
	! Index:
	!	.x Post>Employee Accrual
	!	.x Accrual>Post
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_POST_EMPACCRUAL
	!	$ LINK/EXECUTABLE=PR_EXE: PR_POST_EMPACCRUAL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_POST_EMPACCRUAL.OBJ;*
	!
	! Author:
	!
	!	12/20/87 - Robert Peterson
	!
	! Modification history:
	!
	!	03/18/91 - Kevin Handy
	!		Added eval_date field to pr_read_rate.
	!
	!	05/22/91 - Kevin Handy
	!		Changed "ER" to "ERL" in test (then unwound).
	!		Unwound error trapping.
	!
	!	05/22/91 - Kevin Handy
	!		Modified so that someone with a termination date
	!		of "00/00/0000" would still be processed.
	!
	!	12/18/91 - Kevin Handy
	!		Removed unused map POST_TO_GL.
	!
	!	02/10/92 - Kevin Handy
	!		Added folder date to title on summary.
	!
	!	12/22/92 - Kevin Handy
	!		Modified so that AddAcct will ignore zero's.
	!
	!	06/24/93 - Kevin Handy
	!		Removed "IF INSTR(..."?")" around "ASSG_ACCMASK()"
	!		since it no longer creates massive speedup.
	!
	!	06/24/93 - Kevin Handy
	!		Added test in AddAcct: to check for blank
	!		GL number, and display error message.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/18/95 - Kevin Handy
	!		Lose extra include of CONSTANTS.INC
	!		Refoemat source closer to 80 columns.
	!		Lose unecessary definitions.
	!
	!	12/15/95 - Kevin Handy
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	08/14/96 - Kevin Handy
	!		Lost extra '&' before 'than'.
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
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	10/05/99 - Fis TITLE$() to GL_TRAN_POST call so Alpha
	!		wont crash.
	!
	!	03/22/2000 - Kevin Handy
	!		Add EFF_DATE parameter to PR_READ_RATE
	!
	!	09/05/2000 - Kevin Handy
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
	MAP (UTL_REPORTX)	UTL_REPORTX_CDD	UTL_REPORTX

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART
	MAP (GL_CHART_EXAM)	GL_CHART_CDD	GL_CHART_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP	(PR_EMP_STD_ERNDED)	PR_EMP_STD_ERNDED_CDD	PR_EMP_STD_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	!
	! Array to hold totals
	!
	RECORD ACCT_RECORD
		STRING	ACCT = 18
		DOUBLE	DEBIT
		DOUBLE	CREDIT
		DOUBLE	HOURS
	END RECORD

	DECLARE INTEGER CONSTANT MAX_ACCT = 1000

	DIM ACCT_RECORD ACCT_TOTAL(MAX_ACCT)

	%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.HB"
	MAP (PR_CONTROL)	PR_CONTROL_CDD	PR_CONTROL

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION GL_EXAM_CHART
	EXTERNAL LONG OUTP_XUNSOL ! (It's really an AST routine)

	%PAGE

	ON ERROR GOTO 19000

	SCOPE::PRG_PROGRAM = READ_SYSPN

	USEDACCT$ = ""

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
	!	^*(01) Post Period\*
	!	.p
	!	The ^*Post Period\* field specifies
	!	the General Ledger accounting period that the
	!	records are to be accrued to.
	!	.p
	!	The format for the ^*Period\* must by (YYYYPP).
	!
	! Index:
	!	.x Post Period>Accrual Post
	!	.x Accrual Post>Post Period
	!
	!--

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	!
	! Abstract:FLD02
	!	^*(02) Accrual Date\*
	!	.p
	!	The ^*Accrual Date\* field specifies the effective date of
	!	the employee benefits accrual execution.
	!
	! Index:
	!	.x Accrual Date>Accrual Post
	!	.x Accrual Post>Accrual Date
	!
	!--
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)

	DEFAULT_RATE = VAL(EDIT$(UTL_REPORTX::OPTDEF(2%), -1%))
	!++
	!
	! Abstract:FLD03
	!	^*(03) Default Pay Rate\*
	!	.p
	!	The ^*Default Pay Rate\* field specifies a default pay rate which
	!	will be used in the event an employee has no individual default pay
	!	rate in the Employee Master File Rate record.
	!	.p
	!	This field should agree with the Default Pay Rate field in the
	!	Print Employee Accruals report setting screen.
	!
	! Index:
	!	.x Default Pay Rate>Accrual Post
	!	.x Pay Rate>Accrual Post
	!	.x Accrual Post>Default Pay Rate
	!
	!--

	CODE_NOT_POSTED$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)
	!++
	!
	! Abstract:FLD04
	!	^*(04) Accrual Not Posted\*
	!	.p
	!	The ^*Accrual Not Posted\* field specifies any specific benefits
	!	which are to be posted to the payroll files only and not to the
	!	General Ledger. For example, if payroll benefits to be accrued for
	!	employees include vacation, sick pay and personal leave, but vacation
	!	benefits only are to be posted to the General Ledger,
	!	this field would include the codes for sick pay and personal leave
	!	separated by a comma.
	!
	! Index:
	!	.x Accrual Not Posted>Accrual Post
	!	.x Accrual Post>Accrual Not Posted
	!
	!--

	YEAR_END$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	!
	! Abstract:FLD05
	!	^*(05) Year End Date\*
	!	.p
	!	The ^*Year End Date\* field specifies a date which determines
	!	the end of the "payroll benefits year" universally for all
	!	employees. If this field is left blank, the "payroll benefits
	!	year" will be equal to each employee's employment year.
	!
	! Index:
	!	.x Year End Date>Accrual Post
	!	.x Accrual Post>Year End Date
	!
	!--

	YEAR_END$ = DATE_STOREDATE(YEAR_END$) ! Reformat to (YYYYMMDD)


	CALL READ_DEVICE("PR_EMP_STD_ERNDED", JOUR_DEV_NAME$, STAT%)

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

	IF LEN(XLATE(LEFT(YYYY_PP$, 4%), STRING$(48%, 0%) + &
			"0123456789")) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			LEFT(YYYY_PP$, 4%) + " must have the following " + &
			"format YYYY", 0%)
		GOTO ExitProgram
	END IF

	IF YYYY_PP$ <= GL_PERIOD::YEAR + "_" + FORMAT$( &
		GL_PERIOD::LASTPERCLO, "<0>#")
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
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_MASTER"
		CONTINUE HelpError
	END WHEN

340	!
	! Open Payroll control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.MOD"
	USE
		CONTINUE 345 IF ERR = 5%
		FILENAME$ = "PR_CONTROL"
		CONTINUE HelpError
	END WHEN

	GOTO 347

345	!
	! Create control file
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.CRE"

	PR_CONTROL::YEAR = DATE_TODAY
	PR_CONTROL::POST_DATE = ""
	PR_CONTROL::UR_DATE = ""
	PR_CONTROL::UR_COUNT = 0%
	PR_CONTROL::CLOSEFLAG = "0"

	PUT #PR_CONTROL.CH%, RECORD 1%

347	GET #PR_CONTROL.CH%, RECORD 1%

350	!
	! Open Pay Deduction definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

360	!
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.MOD"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

3000	!
	! Open up batch control file and get a batch number
	!
	CALL ASSG_BATCH(UTL_BATCH.CH%, &
			JOUR_DEV_NAME$, &
			"Initilize", &
			BATCH.NUMBER$, &
			"Accrual Date " + BATCH_NO$, &
			"Payroll Employee Accrual Journal Post", &
			"A", &
			YYYY_PP$, &
			"", &
			EXIT_STATUS%)

	IF (EXIT_STATUS% AND 2%)
	THEN
		CALL HELP_3MESSAGE(SCOPE, "Batch Number Missing", &
			"ERR", "UTL_BATCH", &
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
				CALL ENTR_3MESSAGE(SCOPE, &
					"Process was interrupted.  Restarting",1%)

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

			END IF

			GOTO 3990

		CASE 2%
			!
			! Create transmittal
			!
			CALL ENTR_3MESSAGE(SCOPE, &
				"Creating posting transmittals" + &
				". . . Posting Period " + YYYY_PP$, 1%)

			!
			! Set up to trap interrupt
			!
			SMG_STATUS% = &
				SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
					LOC(OUTP_XUNSOL) BY VALUE, &
					LOC(SCOPE::SMG_KBID) BY VALUE)

			RRR_FLAG% = 0%

			RESET #PR_EMP_MASTER.CH%

		CASE 3%
			!
			! Call the post function to Display transmittal
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

			GOTO Aborted IF POST_STATUS%

			GOTO 3990

		CASE 4%
			!
			! Post to gl and pr final
			!
			CALL ENTR_3MESSAGE(SCOPE, "Posting to GL . . ." + &
				" Posting Period " + YYYY_PP$, 1%)

			!
			! Set up to trap interrupt
			!
			SMG_STATUS% = &
				SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
					LOC(OUTP_XUNSOL) BY VALUE, &
					LOC(SCOPE::SMG_KBID) BY VALUE)

			RRR_FLAG% = 0%

			RESET #PR_EMP_MASTER.CH%

		CASE 5%
			!
			! Print transmittal and remove batch
			!
			!
			! Now, handle the finish up of the pass
			!
			TITLE$(1%) = "Payroll Employee Accrual Journal Transmittal - batch #" + &
				BATCH.NUMBER$
			TITLE$(2%) = "Folder date " + PRNT_DATE(BATCH_NO$, 8%)
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
				JOUR_DEV_NAME$, &
				"Finished", &
				BATCH.NUMBER$, &
				"Accrual Date " + BATCH_NO$, &
				"Payroll Employee Accrual Journal Post", &
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

			GOTO 3990

		END SELECT

3010		!
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
			SMG_STATUS% = &
				SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

			CALL MENU_3INTERRUPT(SCOPE)

			SMG_STATUS% = &
				SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
					LOC(OUTP_XUNSOL) BY VALUE, &
					LOC(SCOPE::SMG_KBID) BY VALUE)

		!
		! Exit Not allowed
		!
		END SELECT

		RRR_FLAG% = 0%

		!
		! Read in one record
		!
		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%
		USE
			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 3800 IF ERR = 11%
			FILENAME$ = "PR_MASTER"
			CONTINUE HelpError
		END WHEN

		!
		! If the employee has been terminated before the accrual date
		! then accruals will not be calculated for this employee....
		!
		GOTO 3500 IF PR_EMP_MASTER::TERMDAY <= BATCH_NO$ AND &
			PR_EMP_MASTER::TERMDAY > "00000000"

3100		!
		! Look up standard earnings and deductions
		!
		WHEN ERROR IN
			FIND #PR_EMP_STD_ERNDED.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM
		USE
			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 3500 IF ERR = 155%
			FILENAME$ = "PR_EMP_STD_ERNDED"
			CONTINUE HelpError
		END WHEN

3120		WHEN ERROR IN
			GET #PR_EMP_STD_ERNDED.CH%
		USE
			!
			! Locked block
			!
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 3500 IF ERR = 11%
			FILENAME$ = "PR_EMP_STD_ERNDED"
			CONTINUE HelpError
		END WHEN

		GOTO 3500 IF PR_EMP_STD_ERNDED::EMPNUM <> PR_EMP_MASTER::EMPNUM

		GOTO 3120 IF PR_EMP_STD_ERNDED::RTYPE <> "A"

3130		!
		! Look up ernded definition file
		!
		PR_ERNDED_DEF::DESCR = STRING$(LEN(PR_ERNDED_DEF::DESCR), 63%)
		PR_ERNDED_DEF::DRCR_ACCT = &
			STRING$(LEN(PR_ERNDED_DEF::DRCR_ACCT), 63%)
		PR_ERNDED_DEF::ACCRUAL_ACCT = &
			STRING$(LEN(PR_ERNDED_DEF::ACCRUAL_ACCT), 63%)

		WHEN ERROR IN
			GET #PR_ERNDED_DEF.CH%, &
				KEY #0% EQ "P" + PR_EMP_STD_ERNDED::CODE, &
				REGARDLESS
		USE
			CONTINUE 3140 IF ERR = 155%
			FILENAME$ = "PR_ERNDED_DEF"
			CONTINUE HelpError
		END WHEN

3140		!
		! Calculate accrual
		!
		CUR_HRS, CUR_DOL = 0.0

		CUR_HRS = PR_EMP_STD_ERNDED::RATE

		IF PR_EMP_STD_ERNDED::LIMIT <> 0.0
		THEN
			IF (CUR_HRS + PR_EMP_STD_ERNDED::ACCRUED - &
				PR_EMP_STD_ERNDED::CTDBAL) > &
				PR_EMP_STD_ERNDED::LIMIT
			THEN
				CUR_HRS = PR_EMP_STD_ERNDED::LIMIT  - &
					(PR_EMP_STD_ERNDED::ACCRUED - &
					PR_EMP_STD_ERNDED::CTDBAL)
			END IF
		END IF

		!
		! Employee rate
		!
		CALL PR_READ_RATE(PR_EMP_MASTER::EMPNUM, &
			PR_EMP_MASTER::OPER, &
			BATCH_NO$, &
			PR_EMP_MASTER::RATE_TYPE, &
			PR_EMP_MASTER::RATE_CDE, &
			HOUR_RATE, &
			PIECE_RATE, &
			FACTOR%, &
			STDEFF, &
			EVALDATE$, &
			EFF_DATE$)

		IF HOUR_RATE = 0.0
		THEN
			HOUR_RATE = DEFAULT_RATE
		END IF

		CUR_DOL = FUNC_ROUND(CUR_HRS * HOUR_RATE, 2%)

		!
		! Check to see if current dollars is zero
		! if it is then skip it
		!
		GOTO 3120 IF CUR_DOL = 0.0 AND CUR_HRS = 0.0

		IF PASS% = 2%
		THEN
			!
			! Create posting transmittal for accruals
			! to be posted to the general ledger
			!
			IF COMP_STRING(EDIT$(PR_EMP_STD_ERNDED::CODE, -1%), &
				CODE_NOT_POSTED$) = 0% &
				OR EDIT$(CODE_NOT_POSTED$, -1%) = ""
			THEN
				!
				! Expense account
				!
				CALL GL_ASSG_ACCMASK( &
					PR_ERNDED_DEF::ACCRUAL_ACCT, &
					PR_EMP_MASTER::ACCT, &
					ACCT_NUM$)

				ACCT_TOTAL = CUR_DOL
				ACCT_HOURS = CUR_HRS
				GOSUB AddAcct

				ACCT_TOTAL = -CUR_DOL
				ACCT_HOURS = -CUR_HRS
				ACCT_NUM$ = PR_ERNDED_DEF::DRCR_ACCT
				GOSUB AddAcct
			END IF
		END IF

3190		!
		! Update std ernded record if pass is 4
		!
		IF PASS% = 4%
		THEN
			TEST% = 0%

			IF EDIT$(YEAR_END$, -1%) <> ""
			THEN
				IF MID(YEAR_END$, 5%, 2%) = &
					MID(BATCH_NO$, 5%, 2%)
				THEN
					TEST% = -1%
				END IF
			ELSE
				IF MID(PR_EMP_MASTER::HIREDAY, 5%, 2%) = &
					MID(BATCH_NO$, 5%, 2%)
				THEN
					TEST% = -1%
				END IF
			END IF

			IF TEST%
			THEN
				PR_EMP_STD_ERNDED::ACCRUED = &
					PR_EMP_STD_ERNDED::ACCRUED - &
					PR_EMP_STD_ERNDED::CTDBAL
				PR_EMP_STD_ERNDED::CTDBAL = 0.0
			END IF

			PR_EMP_STD_ERNDED::ACCRUED = &
				PR_EMP_STD_ERNDED::ACCRUED + CUR_HRS

			WHEN ERROR IN
				UPDATE #PR_EMP_STD_ERNDED.CH%
			USE
				FILENAME$ = "PR_ERNDED_DEF"
				CONTINUE HelpError
			END WHEN

		END IF

		GOTO 3120

3500		!
		! Try for next record
		!
		GOTO 3010

3800		!
		! Post summary of employee accruals
		!
		FOR I% = 1% TO TOTAL_ACCT%
			IF ACCT_TOTAL(I%)::DEBIT <> 0.0 OR &
				ACCT_TOTAL(I%)::CREDIT <> 0.0 OR &
				ACCT_TOTAL(I%)::HOURS <> 0.0
			THEN
				GL_YYYY_PP::ACCT	= ACCT_TOTAL(I%)::ACCT
				GL_YYYY_PP::SOURCE	= "PR"
				GL_YYYY_PP::REFNO	= "EMP ACCRUAL"
				GL_YYYY_PP::TRANDAT	= BATCH_NO$
				GL_YYYY_PP::DESCR	= "EMPLOYEE ACCRUAL"
				GL_YYYY_PP::AMOUNT	= ACCT_TOTAL(I%)::DEBIT + &
					ACCT_TOTAL(I%)::CREDIT
				GL_YYYY_PP::XREFNO	= ""
				GL_YYYY_PP::POSTIM	= TIME_NOW
				GL_YYYY_PP::POSDAT	= DATE_TODAY
				GL_YYYY_PP::CKNO	= ""
				GL_YYYY_PP::TRANKEY	= ""
				GL_YYYY_PP::SUBACC	= ""
				GL_YYYY_PP::OPERATION	= ""
				GL_YYYY_PP::UNITS	= 0.0
				GL_YYYY_PP::HOURS	= ACCT_TOTAL(I%)::HOURS

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

				GOTO Aborted1 &
					IF (POST_STATUS% AND 4%) AND PASS% = 4%
				GOTO Aborted IF (POST_STATUS% AND 4%)
			END IF
		NEXT I%


3990	NEXT PASS%

4200	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	IF UTL_REPORTX::PRINTTO <> 1%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Process complete ", 4%)
	END IF

	%PAGE

 ExitProgram:
10000	!******************************************************************
	! Exit normally
	!******************************************************************

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

 Aborted:
10100	!******************************************************************
	! Abort
	!******************************************************************

	CALL ASSG_BATCH(UTL_BATCH.CH%, &
		JOUR_DEV_NAME$, &
		"Finish/Abort", &
		BATCH.NUMBER$, &
		"Accrual Date " + BATCH_NO$, &
		"Payroll Employee Accrual Journal Post", &
		"X", &
		YYYY_PP$, &
		"", &
		EXIT_STATUS%)

 Aborted1:
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%PAGE

 AddAcct:
	!*******************************************************************
	! Search Account balance list for currently existing account
	!*******************************************************************

	GOTO AddAcct2 IF ACCT_TOTAL = 0.0

	IF TRM$(ACCT_NUM$) = ""
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Employee " + TRM$(PR_EMP_MASTER::EMPNUM) + &
			" has blank GL account", &
			0%)
	ELSE
		IF INSTR(1%, USEDACCT$, ACCT_NUM$) = 0%
		THEN
			USEDACCT$ = USEDACCT$ + "," + ACCT_NUM$

			IF GL_EXAM_CHART(ACCT_NUM$, GL_CHART_EXAM) <> &
				CMC$_NORMAL
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Employee " + &
					TRM$(PR_EMP_MASTER::EMPNUM) + &
					" has undefined GL account", &
					0%)
			END IF

		END IF
	END IF

	GOTO AddAcct1 &
		IF (ACCT_TOTAL(I%)::ACCT = ACCT_NUM$) &
			FOR I% = 1% TO TOTAL_ACCT%

	!
	! Item not found, create it
	!
	I%, TOTAL_ACCT% = TOTAL_ACCT% + 1%

	WHILE (I% > 1%) AND (ACCT_TOTAL(I% - 1%)::ACCT > ACCT_NUM$)
		ACCT_TOTAL(I%) = ACCT_TOTAL(I% - 1%)
		I% = I% - 1%
	NEXT

	ACCT_TOTAL(I%)::ACCT = ACCT_NUM$
	ACCT_TOTAL(I%)::CREDIT = 0.0
	ACCT_TOTAL(I%)::DEBIT = 0.0
	ACCT_TOTAL(I%)::HOURS = 0.0

 AddAcct1:
	!
	! Add credit/debit amounts
	!
	IF ACCT_TOTAL > 0.0
	THEN
		ACCT_TOTAL(I%)::DEBIT = ACCT_TOTAL(I%)::DEBIT + ACCT_TOTAL
	ELSE
		ACCT_TOTAL(I%)::CREDIT = ACCT_TOTAL(I%)::CREDIT + ACCT_TOTAL
	END IF

	ACCT_TOTAL(I%)::HOURS = ACCT_TOTAL(I%)::HOURS + ACCT_HOURS

 AddAcct2:
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
