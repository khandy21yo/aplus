1	%TITLE "POST - Post Adjustments"
	%SBTTL "PR_POST_ACCRUALADJUST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:PRADPS
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Post Adjustments\* option
	!	posts to the General Ledger any adjustments to the accrued benefit
	!	liability account(s) and related expense account(s) required as a result of
	!	employee rate changes which have occurred since the previous final payroll
	!	post or accrual adjustment post was executed.
	!	.lm -5
	!
	! Index:
	!	.x Post>Accrual Adjustments
	!	.x Accrual Adjustments>Post
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_POST_ACCRUALADJUST/LINE
	!	$ LINK/EXE=PR_EXE: PR_POST_ACCRUALADJUST, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_POST_ACCRUALADJUST.OBJ;*
	!
	! Author:
	!
	!	12/15/92 - Kevin Handy
	!
	! Modification history:
	!
	!	12/29/92 - Kevin Handy
	!		Modified to handle PR_EMP_ACCRUAL::GLFLAG for sick
	!		pay type of accruals that don't hit the GL.
	!
	!	01/21/93 - Kevin Handy
	!		Fixed bug where it was checking account numbers
	!		even if it was not going to use them because of
	!		the ::GLFLAG.
	!
	!	02/02/93 - Kevin Handy
	!		Added line number 3000
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/04/93 - Kevin Handy
	!		Clean up problems with accrual code going in
	!		blank sometimes.
	!
	!	07/28/93 - Kevin Handy
	!		Modified so that would not die when no employee
	!		record existed for accrual record.  Just skip
	!		over it.
	!
	!	01/06/93 - Kevin Handy
	!		Modified to shift unavailable to available on
	!		annaversary (sic) date.
	!
	!	01/06/93 - Kevin Handy
	!		Fixed shift from unavailable to available to ignore
	!		year, and only look at the month.
	!
	!	05/05/94 - Kevin Handy
	!		Fixed place where AVAILDATE was typed GLFLAG.
	!
	!	05/05/94 - Kevin Handy
	!		Added error trap for 3050.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/10/95 - Kevin Handy
	!		Fix closer to 80 columns.
	!		Don't post zero's to GL, payroll quarterly
	!		register or payroll journal.
	!
	!	01/15/96 - Kevin Handy
	!		Modified to shift unavailable to avaiable if
	!		the rate code is a "1", which can be caused
	!		by changing the code from "2" to "1". (LL)
	!
	!	01/15/96 - Kevin Handy
	!		Create new accrual type "3", which accures as
	!		unavailable up to the anneversary date, then
	!		everything is available after that.
	!
	!	09/10/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/24/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/22/2000 - Kevin Handy
	!		Add EFF_DATE parameter to PR_READ_RATE
	!
	!	09/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	07/16/2001 - Kevin Handy
	!		Use specific rate if possible, then fall back to
	!		a generic rate. (marco)
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include constants and and some functions
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	MAP	(PR_EMP_ACCRUAL)	PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.HB"
	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.HB"
	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.HB"
	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	PR_TRAN_POSTHIS
	EXTERNAL LONG	FUNCTION	PR_TRAN_POSTREG
	EXTERNAL LONG	FUNCTION	PR_TRAN_POSTACCRUAL
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES

	!
	! Declare internal variables
	!
	DECLARE	GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE PR_HIS_PAY_CDD		PR_HIS_PAY
	DECLARE PR_HIS_DED_CDD		PR_HIS_DED
	DECLARE PR_HIS_CHECK_CDD	PR_HIS_CHECK
	DECLARE PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL_DIST
	DECLARE PR_REG_ERNDED_CDD	PR_REG_ERNDED
	DECLARE PR_REG_TAXES_CDD	PR_REG_TAXES

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE	LONG			PRNT_SUMMARY
	DECLARE LONG			CHECK_PERIOD

	DECLARE	STRING			BATCH_NUMBER
	DECLARE	STRING			GLPERIOD
	DECLARE	STRING			PR.INTER.PERIOD
	DECLARE	STRING			GL.INTER.PERIOD
	DECLARE	STRING			POSTDATE
	DECLARE	STRING			POSTTIME
	DECLARE	STRING			TEXT
	DECLARE	STRING			TITLE(10%)

	%PAGE

	!**************************************************************
	! Get some stuff done before we start
	!**************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Title
	!
	TITLE(1%) = "ACCRUAL ADJUSTMENT POSTING PROTOCOL"
	TITLE(2%) = "Payroll System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	%PAGE

	!**************************************************************
	! Process `from user' input
	!**************************************************************

	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GLPERIOD = TRM$(UTL_REPORTX::OPTDEF(0%))
	!++
	! Abstract:FLD01
	!	^*(01) GL Period (YYYYPP)\*
	!	.lm +5
	!	.b
	!	The ^*GL Period (YYYYPP)\* field specifies
	!	the General Ledger Period into which
	!	any Accrual Adjustments will be posted.
	!
	! Index:
	!	.x Accrual Adjustment>Post>GL Period
	!	.x Post>Accrual Adjustment>GL Period
	!	.x GL Period>Post>Accrual Adjustment
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(1%))

	!++
	! Abstract:FLD02
	!	^*Fldr Date (MMDDYYYY)\*
	!	.lm +5
	!	.b
	!	The ^* Fldr Date (MMDDYYYY)\* field specifies the
	!	folder date of the file which
	!	is to be posted.  The folder date must not be a duplicate of any other
	!	folder date which has already been created or which will be created.
	!	.lm -5
	!
	! Index:
	!	.x Accrual Adjustment>Post>Folder Date
	!	.x Folder Date>Accrual Adjustment>Post
	!	.x Post>Accrual Adjustment>Folder Date
	!
	!--
	CHECK_DATE$ = LEFT(UTL_REPORTX::OPTDEF(2%), 1%)
	!++
	! Abstract:FLD03
	!	^*(03) Check Dates (Y/N)\*
	!	.lm +5
	!	.b
	!	The ^*Check Dates\* field specifies if the post should
	!	compare the folder date with the General Ledger
	!	period into which the folder will be posted.  If this
	!	field is a "Y", the post will be aborted if the folder date is not in
	!	agreement with the General Ledger Period.  An "N" value will ignore
	!	any comparison.
	!	.lm -5
	!
	! Index:
	!	.x Check Dates>Accrual Adjustment>Post
	!	.x Post>Accrual Adjustment>Check Dates
	!	.x Accrual Adjustment>Post>Check Dates
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF (CHECK_DATE$ = "Y")

	QTR% = VAL%(UTL_REPORTX::OPTDEF(3%)) - 1%
	!++
	! Abstract:FLD04
	!	^*(04) Quarter (1,2,3,4)\*
	!	.lm +5
	!	.b
	!	The ^*Quarter (1,2,3,4)\* field specifies
	!	the calendar quarter to which
	!	adjustments are to be reflected in the payroll files.
	!
	! Index:
	!	.x Accrual Adjustment>Post>Quarter
	!	.x Post>Accrual Adjustment>Quarter
	!	.x Quarter>Accrual Adjustment>Post
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
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.OPN"
	USE
		FILENAME$ = "PR_EMP_ACCRUAL"
		CONTINUE HelpError
	END WHEN

320	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

	%PAGE

	!******************************************************************
	!	1) See if the posting process has been interrupted
	!	2) If not interrupted, go on
	!	3) If interrupted, delete the superceded ledger records
	!		and then continue
	!******************************************************************

	!******************************************************************
	! Check if posting process has been interrupted
	!******************************************************************

	!
	! Open up batch control file and check if interrupted
	!
	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "PR_ACCRUAL", BATCH_NO$, &
		GL.INTER.PERIOD, PR.INTER.PERIOD)

	SELECT INTR_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Process was interrupted
	!
	CASE CMC$_WARNING

		IF TRM$(GL.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				GL_TRAN_POSTGL(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"", "", GL.INTER.PERIOD) <> CMC$_NORMAL
		END IF

		IF TRM$(PR.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				PR_TRAN_POSTHIS(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"", "", "", PR.INTER.PERIOD) <> &
				CMC$_NORMAL

			GOTO Aborted IF &
				PR_TRAN_POSTACCRUAL(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"", PR.INTER.PERIOD) <> CMC$_NORMAL

			GOTO Aborted IF &
				PR_TRAN_POSTREG(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"", "", PR.INTER.PERIOD) <> CMC$_NORMAL

		END IF

	!
	! Abort post process
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	%PAGE

 AssignBatch:
	!******************************************************************
	!	1) Assign a batch number
	!	2) Make sure no legitimate records in the ledger already
	!		have this batch number; if any records do have
	!		this newly assigned number, go back to (1) and
	!		get a new one.
	!******************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "PR_ACCRUAL", BATCH_NO$, &
		GLPERIOD, "") <> CMC$_NORMAL

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", GLPERIOD)

	SELECT EXIT_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something's definitely wrong here
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	EXIT_STATUS = PR_TRAN_POSTHIS(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", "", BATCH_NO$)

	SELECT EXIT_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something's definitely wrong here
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	EXIT_STATUS = PR_TRAN_POSTREG(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", LEFT(BATCH_NO$, 4%))

	SELECT EXIT_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something's definitely wrong here
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	EXIT_STATUS = PR_TRAN_POSTACCRUAL(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", GLPERIOD)

	SELECT EXIT_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something's definitely wrong here
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	%PAGE

	!******************************************************************
	!	1) List the debits/credits transmittal for the user while
	!		also putting the journal data in temporary files
	!	2) If confirmation, then go on
	!******************************************************************

	!******************************************************************
	! Create transmittal
	!******************************************************************
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "PR_ACCRUAL", BATCH_NO$, &
		"", "") <> CMC$_NORMAL

3000	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	WHEN ERROR IN
		RESET #PR_EMP_ACCRUAL.CH%, KEY #K_NUM%
	USE
		CONTINUE Confirm
	END WHEN

 ReadHeader:
3020	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_EMP_ACCRUAL.CH%, REGARDLESS
	USE
		CONTINUE Confirm
	END WHEN

3025	!
	! Lookup employee master file record
	!
	IF PR_EMP_ACCRUAL::EMPNUM <> PR_EMP_MASTER::EMPNUM
	THEN
		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_EMP_ACCRUAL::EMPNUM, REGARDLESS
		USE
			CONTINUE 3020 IF ERR = 155%	! Master record gone
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Look up current rate
	!
	!
	! Try for a specific rate based on the pay code first
	!
	CALL PR_READ_RATE_CODE(PR_EMP_MASTER::EMPNUM, &
		PR_EMP_MASTER::OPER, &
		BATCH_NO$, &
		RATE_TYPE$, &
		PR_EMP_ACCRUAL::ATYPE, &
		HOUR_RATE, &
		PIECE_RATE, &
		FACTOR%, &
		STDEFF, &
		EVALDATE$, &
		EFF_DATE$)

	!
	! Try for a generic rate if a specific rate isn't found
	!
	IF HOUR_RATE = 0.0
	THEN
		CALL PR_READ_RATE(PR_EMP_MASTER::EMPNUM, &
			PR_EMP_MASTER::OPER, &
			BATCH_NO$, &
			RATE_TYPE$, &
			RATE_CDE$, &
			HOUR_RATE, &
			PIECE_RATE, &
			FACTOR%, &
			STDEFF, &
			EVALDATE$, &
			EFF_DATE$)
	END IF

	!
	! Do we need to roll the hours from unavailable to available?
	!
	IF (PR_EMP_ACCRUAL::AVAILFLAG = "1") OR &
		((PR_EMP_ACCRUAL::AVAILFLAG = "2") AND &
			(MID(PR_EMP_ACCRUAL::AVAILDATE, 5%, 2%) = &
			MID(BATCH_NO$, 5%, 2%))) OR &
		((PR_EMP_ACCRUAL::AVAILFLAG = "3") AND &
			(BATCH_NO$ >= PR_EMP_ACCRUAL::AVAILDATE))
	THEN
		SHIFT_AVA = PR_EMP_ACCRUAL::HOURSUNA
		SHIFT_UNA = -SHIFT_AVA
	ELSE
		SHIFT_AVA = 0.0
		SHIFT_UNA = 0.0
	END IF

	!
	! Calculate any changes
	!
	CHANGE_UNA = FUNC_ROUND((PR_EMP_ACCRUAL::HOURSUNA + SHIFT_UNA) * &
		HOUR_RATE, 2%) - &
		PR_EMP_ACCRUAL::DOLLARUNA
	CHANGE_AVA = FUNC_ROUND((PR_EMP_ACCRUAL::HOURSAVA + SHIFT_AVA) * &
		HOUR_RATE, 2%) - &
		PR_EMP_ACCRUAL::DOLLARAVA

	IF (CHANGE_UNA = 0.0) AND (CHANGE_AVA = 0.0)
	THEN
		GOTO 3020
	END IF

3050	IF (PR_ERNDED_DEF::ETYPE <> "P") OR &
		(PR_ERNDED_DEF::CODE <> PR_EMP_ACCRUAL::ATYPE)
	THEN
		!
		! Look up ernded definition file
		!
		WHEN ERROR IN
			GET #PR_ERNDED_DEF.CH%, &
				KEY #0% EQ "P" + PR_EMP_ACCRUAL::ATYPE, &
				REGARDLESS
		USE
			PR_ERNDED_DEF::ETYPE = "P"
			PR_ERNDED_DEF::CODE = PR_EMP_ACCRUAL::ATYPE

			PR_ERNDED_DEF::DESCR = STRING$(LEN(PR_ERNDED_DEF::DESCR), 63%)
			PR_ERNDED_DEF::DRCR_ACCT = &
				STRING$(LEN(PR_ERNDED_DEF::DRCR_ACCT), 63%)
			PR_ERNDED_DEF::ACCRUAL_ACCT = &
				STRING$(LEN(PR_ERNDED_DEF::ACCRUAL_ACCT), 63%)

			CONTINUE 3060 IF ERR = 155%
			FILENAME$ = "PR_ERNDED_DEF"
			CONTINUE HelpError
		END WHEN
	END IF

3060	!
	! Calculate debit/credit accounts
	!
	CALL GL_ASSG_ACCMASK(PR_ERNDED_DEF::ACCRUAL_ACCT, &
		PR_EMP_MASTER::ACCT, &
		CREDIT_ACCOUNT$)


	CALL GL_ASSG_ACCMASK(PR_ERNDED_DEF::DRCR_ACCT, &
		PR_EMP_MASTER::ACCT, &
		DEBIT_ACCOUNT$)

	IF (PR_EMP_ACCRUAL::GLFLAG <> "N")
	THEN
		!
		! validate credit/debit accounts
		!
		EXIT_STATUS = GL_EXAM_CHART(CREDIT_ACCOUNT$, GL_CHART_EXAM)

		SELECT EXIT_STATUS
		!
		! Success; go on
		!
		CASE CMC$_NORMAL

		!
		! Undefined; set a flag and go on
		!
		CASE CMC$_UNDEFINED
			ACCTNUM$ = "*"

		!
		! SNAFU:  (Situation Normal - it's All Fouled Up)
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		!
		! See if AR Account number is defined
		!
		EXIT_STATUS = GL_EXAM_CHART(DEBIT_ACCOUNT$, GL_CHART_EXAM)

		SELECT EXIT_STATUS
		!
		! Success; go on
		!
		CASE CMC$_NORMAL

		!
		! Undefined; set a flag and go on
		!
		CASE CMC$_UNDEFINED
			ACCTNUM$ = "*"

		!
		! SNAFU:  (Situation Normal - it's All Fouled Up)
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		IF (CHANGE_UNA + CHANGE_AVA) <> 0.0
		THEN
			!
			! Generate a GL record to pass through to the post function
			!
			GL_YYYY_PP::ACCT	= DEBIT_ACCOUNT$
			GL_YYYY_PP::SOURCE	= "PR"
			GL_YYYY_PP::REFNO	= ""
			GL_YYYY_PP::TRANDAT	= BATCH_NO$
			GL_YYYY_PP::DESCR	= "Accrual Adjustment"
			GL_YYYY_PP::AMOUNT	= CHANGE_UNA + CHANGE_AVA
			GL_YYYY_PP::XREFNO	= PR_EMP_MASTER::EMPNUM
			GL_YYYY_PP::POSTIM	= POSTTIME
			GL_YYYY_PP::POSDAT	= POSTDATE
			GL_YYYY_PP::CKNO	= ""
			GL_YYYY_PP::TRANKEY	= ""
			GL_YYYY_PP::SUBACC	= ""
			GL_YYYY_PP::OPERATION	= ""
			GL_YYYY_PP::UNITS	= 0.0
			GL_YYYY_PP::HOURS	= 0.0
			GL_YYYY_PP::UPDSTA	= ""
			GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

			!
			! Put the record into the temporary file
			!
			EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
				CHECK_PERIOD, "", TITLE(), &
				UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, GLPERIOD)

			!
			! Check the date; is it out of range?
			!
			SELECT EXIT_STATUS
			!
			! Date is correct; go on
			!
			CASE CMC$_NORMAL

			!
			! Date is not in the correct period
			!
			CASE CMC$_DATEOUT
				TRANDAT$ = "*"

			!
			! Something else going wrong
			!
			CASE ELSE
				GOTO Aborted

			END SELECT

			!
			! Generate a GL record to pass through to the post function
			!
			GL_YYYY_PP::ACCT	= CREDIT_ACCOUNT$
			GL_YYYY_PP::SOURCE	= "PR"
			GL_YYYY_PP::REFNO	= ""
			GL_YYYY_PP::TRANDAT	= BATCH_NO$
			GL_YYYY_PP::DESCR	= "Accrual Adjustment"
			GL_YYYY_PP::AMOUNT	= -(CHANGE_UNA + CHANGE_AVA)
			GL_YYYY_PP::XREFNO	= PR_EMP_MASTER::EMPNUM
			GL_YYYY_PP::POSTIM	= POSTTIME
			GL_YYYY_PP::POSDAT	= POSTDATE
			GL_YYYY_PP::CKNO	= ""
			GL_YYYY_PP::TRANKEY	= ""
			GL_YYYY_PP::SUBACC	= ""
			GL_YYYY_PP::OPERATION	= ""
			GL_YYYY_PP::UNITS	= 0.0
			GL_YYYY_PP::HOURS	= 0.0
			GL_YYYY_PP::UPDSTA	= ""
			GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

			!
			! Put the record into the temporary file
			!
			EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
				CHECK_PERIOD, "", TITLE(), &
				UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, GLPERIOD)

			!
			! Check the date; is it out of range?
			!
			SELECT EXIT_STATUS
			!
			! Date is correct; go on
			!
			CASE CMC$_NORMAL

			!
			! Date is not in the correct period
			!
			CASE CMC$_DATEOUT
				TRANDAT$ = "*"

			!
			! Something else going wrong
			!
			CASE ELSE
				GOTO Aborted

			END SELECT
		END IF

	END IF

	IF (CHANGE_UNA + CHANGE_AVA) <> 0.0
	THEN
		!
		! Generate a PR_HIS_DED record to pass through to the
		! post function
		!
		PR_HIS_PAY::EMPNUM		= PR_EMP_MASTER::EMPNUM
		PR_HIS_PAY::PR_END_DATE		= BATCH_NO$
		PR_HIS_PAY::EMP_SKILL		= ""
		PR_HIS_PAY::EMP_GRADE		= ""
		PR_HIS_PAY::ACCT		= DEBIT_ACCOUNT$
		PR_HIS_PAY::SUBACC		= ""
		PR_HIS_PAY::OPER		= ""
		PR_HIS_PAY::LOCATION		= ""
		PR_HIS_PAY::DEPT		= ""
		PR_HIS_PAY::WORK_CENTER		= ""
		PR_HIS_PAY::UNION		= ""
		PR_HIS_PAY::PTYPE		= "A"
		PR_HIS_PAY::RTYPE		= "O"
		PR_HIS_PAY::CODE		= PR_EMP_ACCRUAL::ATYPE
		PR_HIS_PAY::PIECE_RATE		= 0.0
		PR_HIS_PAY::HOUR_RATE		= 0.0
		PR_HIS_PAY::REG_HR		= 0.0
		PR_HIS_PAY::OVT_HR		= 0.0
		PR_HIS_PAY::PIECE		= 0.0
		PR_HIS_PAY::FACTOR		= 100%
		PR_HIS_PAY::GROSS		= CHANGE_UNA + CHANGE_AVA
		PR_HIS_PAY::TAX_PKG		= PR_EMP_MASTER::TAX_PKG
		PR_HIS_PAY::BATCH_ENTRY		= ""
		PR_HIS_PAY::UPDATE_FLAG		= -1%
		PR_HIS_PAY::SEQNUM		= ""
		PR_HIS_PAY::BATCH		= BATCH_NUMBER
		PR_HIS_PAY::WORKDATE		= ""
		FOR I% = 0% TO 6%
			PR_HIS_PAY::REGULAR(I%) = 0.0
			PR_HIS_PAY::OVERTIME(I%) = 0.0
		NEXT I%
		PR_HIS_PAY::EQUIPMENT		= ""
		PR_HIS_PAY::EQUIPHOUR		= 0.0

		!
		! Call the post function
		!
		GOTO Aborted IF PR_TRAN_POSTHIS(OPT_ADDREC, SUBOPT_REGISTER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			PR_HIS_PAY, PR_HIS_DED, PR_HIS_CHECK, BATCH_NO$) <> CMC$_NORMAL

		!
		! Generate a PR_HIS_CHECK record to pass through to the
		! post function
		!
		PR_HIS_CHECK::EMPNUM		= PR_EMP_MASTER::EMPNUM
		PR_HIS_CHECK::PR_END_DATE	= BATCH_NO$
		PR_HIS_CHECK::CHECK		= "999999"
		PR_HIS_CHECK::CHECK_DATE	= BATCH_NO$
		PR_HIS_CHECK::PAYFREQ		= PR_EMP_MASTER::PAYFREQ
		PR_HIS_CHECK::UPDATE_FLAG	= -1%
		PR_HIS_CHECK::BATCH		= BATCH_NUMBER

		!
		! Call the post function
		!
		GOTO Aborted IF PR_TRAN_POSTHIS(OPT_ADDREC, SUBOPT_LEDGER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			PR_HIS_PAY, PR_HIS_DED, PR_HIS_CHECK, BATCH_NO$) <> CMC$_NORMAL
	END IF

	!
	! Generate a PR_EMP_ACCRUAL record to pass through to the
	! post function
	!
	PR_EMP_ACCRUAL_DIST = PR_EMP_ACCRUAL
	PR_EMP_ACCRUAL_DIST::HOURSUNA	= SHIFT_UNA
	PR_EMP_ACCRUAL_DIST::HOURSAVA	= SHIFT_AVA
	PR_EMP_ACCRUAL_DIST::DOLLARUNA	= CHANGE_UNA
	PR_EMP_ACCRUAL_DIST::DOLLARAVA	= CHANGE_AVA
	PR_EMP_ACCRUAL_DIST::BATCH	= BATCH_NUMBER

	!
	! Call the post function
	!
	GOTO Aborted IF PR_TRAN_POSTACCRUAL(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		PR_EMP_ACCRUAL_DIST, GLPERIOD) <> CMC$_NORMAL

	IF (CHANGE_UNA + CHANGE_AVA) <> 0.0
	THEN
		!
		! Generate a PR_HIS_DED record to pass through to the
		! post function
		!
		PR_REG_ERNDED::EMPNUM		= PR_EMP_MASTER::EMPNUM
		PR_REG_ERNDED::ETYPE		= "A"
		PR_REG_ERNDED::CODE		= PR_EMP_ACCRUAL::ATYPE
		FOR I% = 0% TO 3%
			PR_REG_ERNDED::QTR_DOLL(I%)	= 0.0
			PR_REG_ERNDED::REG_HRS(I%)	= 0.0
			PR_REG_ERNDED::PRE_HRS(I%)	= 0.0
			PR_REG_ERNDED::UNITS(I%)	= 0.0
		NEXT I%
		PR_REG_ERNDED::QTR_DOLL(QTR%)	= CHANGE_UNA + CHANGE_AVA
		PR_REG_ERNDED::UPDATE_COUNTER	= 0%

		!
		! Call the post function
		!
		GOTO Aborted IF PR_TRAN_POSTREG(OPT_ADDREC, SUBOPT_REGISTER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			PR_REG_ERNDED, PR_REG_TAXWH, LEFT(BATCH_NO$, 4%)) <> &
			CMC$_NORMAL
	END IF

 GoToNext:
	!
	! Was the account number undefined?
	!
	IF (ACCTNUM$ = "*")
	THEN
		!
		! Put the invalid stuff in TEXT to send to OUTP_UNDEFCODES
		!
		TEXT = "  Emp# " + PR_EMP_ACCRUAL::EMPNUM + " " + &
			PR_EMP_ACCRUAL::ATYPE + " " + &
			ACCTNUM$ + DEBIT_ACCOUNT$ + " " + &
			CREDIT_ACCOUNT$

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, &
				TEXT) <> CMC$_NORMAL

		!
		! Blank flags
		!
		ACCTNUM$ = " "

	END IF

	GOTO 3020

	%PAGE

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", GLPERIOD)

	EXIT_STATUS = EXIT_STATUS AND OUTP_UNDEFCODES(OPT_CONFIRM, TITLE(), &
		UTL_REPORTX, "")

	GOTO Aborted IF EXIT_STATUS <> CMC$_NORMAL

	%PAGE

	!******************************************************************
	!	1) Since everything's correct and we have the user's
	!		permission, transfer all data from the temporary
	!		files into the ledger
	!******************************************************************
	!
	! Begin posting
	!
	GOTO Interrupt IF &
		GL_TRAN_POSTGL(OPT_POSTFILE, SUBOPT_DETAIL, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", GLPERIOD) <> CMC$_NORMAL

	!
	! Post the AR Sales Journal header
	!
	GOTO Interrupt IF &
		PR_TRAN_POSTHIS(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", BATCH_NO$) <> &
			CMC$_NORMAL

	!
	! Continue by posting the Sales Journal line items
	!
	GOTO Interrupt IF &
		PR_TRAN_POSTHIS(OPT_POSTFILE, SUBOPT_LINEITEM, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", BATCH_NO$) <> &
			CMC$_NORMAL

	!
	! Post any Sales Tax items
	!
	GOTO Interrupt IF &
		PR_TRAN_POSTHIS(OPT_POSTFILE, SUBOPT_LEDGER, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", BATCH_NO$) <> &
			CMC$_NORMAL

	!
	! Post any Sales Tax items
	!
	GOTO Interrupt IF &
		PR_TRAN_POSTACCRUAL(OPT_POSTFILE, &
			SUBOPT_REGISTER, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", GLPERIOD) <> CMC$_NORMAL

	!
	! Post the AR Sales Journal header
	!
	GOTO Interrupt IF &
		PR_TRAN_POSTREG(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", LEFT(BATCH_NO$, 4%)) <> &
			CMC$_NORMAL

	%PAGE

	!
	! Remove files
	!
5000

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "PR_ACCRUAL", BATCH_NO$, "", "")

	PRNT_SUMMARY = SUBOPT_FINAL

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************
	!
	! Print credit and debit transmittals
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_SUMMARY, PRNT_SUMMARY, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	!
	! Print undefined codes (if any)
	!
	TEXT = "Item   Invoice CustomerNum AR AccountNum       TransDate SubAcct"

	EXIT_STATUS = OUTP_UNDEFCODES(OPT_SUMMARY, TITLE(), UTL_REPORTX, TEXT)

	!
	! Finish up the transmittal
	!
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

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************
	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "PR_ACCRUAL", &
			BATCH_NO$, "", "")
		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "PR_ACCRUAL", BATCH_NO$, "", "")

	GOTO ExitProgram

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
	! Error trapping
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32000	!******************************************************************
	! End of posting program PR_POST_ACCRUALADJUST
	!******************************************************************
	END
