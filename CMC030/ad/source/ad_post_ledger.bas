1	%TITLE "Asset Ledger Posting Program"
	%SBTTL "AD_POST_LEDGER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
	! Computer Management Center
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
	! ID:AD018
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Post Ledger to General Ledger\* option posts
	!	the Asset Period Ledger to the General Ledger. Only
	!	the object which has been designated as "Book" will
	!	be posted. This object must be assigned in the Asset
	!	Control File. Upon execution of a Posting routine,
	!	a transmittal report will be displayed on the screen at
	!	the completion of which the user has the option to either
	!	abort or continue the posting process. If there are any
	!	undefined account numbers in a journal or if the journal
	!	is not in balance, the posting routine will automatically
	!	be aborted. When the option to continue is elected, a
	!	hard copy of the transmittal will be printed.
	!	.lm -5
	!
	! Index:
	!	.x Post>Asset Ledger
	!	.x Asset Ledger>Post
	!	.x General Ledger
	!	.x Asset>Controlling File
	!	.x Controlling File
	!	.x Posting Routine
	!	.x Account number
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_POST_LEDGER
	!	$ LINK/EXE=AD_EXE: AD_POST_LEDGER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_POST_LEDGER.OBJ;*
	!
	! Author:
	!
	!	06/20/89 - Aaron Redd
	!
	! Modification history:
	!
	!	08/15/91 - Frank F. Starman
	!		Continue and show undefined accounts if AD_READ_ACCOUNT
	!		function doesn't return CMC$_NORMAL.
	!
	!	09/20/91 - Frank F. Starman
	!		Return END_DATE ot the posting period and use it
	!		when posting to GL.
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	08/29/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.HB"
	MAP	(AD_CALCULATION)	AD_CALCULATION_CDD	AD_CALCULATION

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[AD.OPEN]AD_ACCOUNT.HB"
	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	AD_TRAN_POSTAD
	EXTERNAL LONG	FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG	FUNCTION	GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION	AD_EXAM_ASSET
	EXTERNAL LONG	FUNCTION	AD_READ_ACCOUNT
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES

	!
	! Declare internal variables
	!
	DECLARE	AD_ACCOUNT_CDD		AD_ACCOUNT_EXAM
	DECLARE	AD_35ASSET_CDD		AD_35ASSET_EXAM
	DECLARE	GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE	LONG			PRNT_SUMMARY

	DECLARE	STRING			BATCH_NUMBER
	DECLARE	STRING			GLPERIOD
	DECLARE	STRING			ADPERIOD
	DECLARE	STRING			GL.INTER.PERIOD
	DECLARE	STRING			POSTDATE
	DECLARE	STRING			POSTTIME
	DECLARE	STRING			END_DATE
	DECLARE	STRING			TEXT
	DECLARE	STRING			TITLE(10%)

	%PAGE

	!**********************************************************************
	! Get some stuff done before we start
	!**********************************************************************
	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Title
	!
	TITLE(1%) = "ASSET  LEDGER  POSTING  PROTOCOL"
	TITLE(2%) = "Asset Depreciation System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	%PAGE

	!**********************************************************************
	! Process `from user' input
	!**********************************************************************
	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	!++
	!
	! Abstract:FLD01
	!	^*(01) Post Period\*
	!	.b
	!	.lm +5
	!	The accounting period to which the General Journal(s)
	!	is(are) to be posted must be entered in the
	!	^*Post Period\* field.
	!	.b
	!	A blank value here will cause the period to math the AD
	!	period being posted.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--
	GLXPERIOD$ = TRM$(UTL_REPORTX::OPTDEF(0%))


320	!
	! Open AD ledger file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CALCULATION.UPD"
	USE
		IF ERR = 138%
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "AD_CALCULATION"
		CONTINUE HelpError
	END WHEN

	%PAGE

	!**********************************************************************
	!	1) See if the posting process has been interrupted
	!	2) If not interrupted, go on
	!	3) If interrupted, delete the superceded ledger records
	!		and then continue
	!**********************************************************************

	!**********************************************************************
	! Check if posting process has been interrupted
	!**********************************************************************
	!
	! Open up batch control file and check if interrupted
	!
	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AD_CALCULATION", BATCH_NO$, &
		GL.INTER.PERIOD, "")

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
				GL_TRAN_POSTGL(OPT_RESTART, &
					SUBOPT_NOOPT, BATCH_NUMBER, &
					TITLE(), UTL_REPORTX, "", "", &
					GL.INTER.PERIOD) <> CMC$_NORMAL
		END IF

	!
	! Abort post process
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	%PAGE

	!**********************************************************************
	!	1) Assign a batch number
	!	2) Make sure no legitimate records in the ledger already
	!		have this batch number; if any records do have
	!		this newly assigned number, go back to (1) and
	!		get a new one.
	!**********************************************************************
	!
	! Get the period from the AD Control File
	!
	GOTO Aborted IF &
		AD_TRAN_POSTAD(OPT_CHECK, SUBOPT_LEDGER, "", TITLE(), &
		UTL_REPORTX, AD_CALCULATION, "", ADPERIOD) <> CMC$_NORMAL

	END_DATE = RIGHT(ADPERIOD, 7%)
	ADPERIOD = LEFT(ADPERIOD, 6%)
	IF GLXPERIOD$ = ""
	THEN
		GLPERIOD = ADPERIOD
	ELSE
		GLPERIOD = GLXPERIOD$
	END IF

 AssignBatch:
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AD_CALCULATION", &
		BATCH_NO$, ADPERIOD, "") <> CMC$_NORMAL

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CHECK, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD)

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

	!**********************************************************************
	!	1) List the debits/credits transmittal for the user while
	!		also putting the journal data in temporary files
	!	2) If confirmation, then go on
	!**********************************************************************

	!**********************************************************************
	! Create transmittal
	!**********************************************************************
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", BATCH_NO$, "", "") <> CMC$_NORMAL

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	!
	! Get ready to begin
	!
	RESET #AD_CALCULATION.CH%

	!
	! Blank flags
	!
	CALC_ASSET$, EXPACCT$, DEPACCT$ = " "

 NextRec:
	!
	! Get the (next) AD Calculation ledger record
	!
1000	WHEN ERROR IN
		GET #AD_CALCULATION.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE Confirm IF ERR = 11%
		FILENAME$ = "AD_CALCULATION"
		CONTINUE HelpError
	END WHEN

	!
	! Check if we should post this record
	!
	EXIT_STATUS = AD_TRAN_POSTAD(OPT_ADDREC, SUBOPT_LEDGER, "", TITLE(), &
		UTL_REPORTX, AD_CALCULATION, "", ADPERIOD)

	SELECT EXIT_STATUS
	!
	! OK to post the record; continue
	!
	CASE CMC$_NORMAL

	!
	! Don't post this one
	!
	CASE CMC$_WARNING
		GOTO NextRec

	!
	! Something's very wrong
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Is the asset number defined?
	!
	EXIT_STATUS = AD_EXAM_ASSET(AD_CALCULATION::ASSET_NUM, AD_35ASSET_EXAM)

	SELECT EXIT_STATUS
	!
	! Code found; continue
	!
	CASE CMC$_NORMAL

	!
	! Asset number was undefined; set flag and continue
	!
	CASE CMC$_UNDEFINED
		CALC_ASSET$ = "*"

	!
	! Strange Days
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Are there account numbers associated with this asset number?
	!
	V% = AD_READ_ACCOUNT(AD_35ASSET_EXAM::LOCATION, &
		AD_35ASSET_EXAM::ASSET_TYPE, AD_ACCOUNT_EXAM)

	!**********************************************************************
	! Create two GL_YYYY_PP records from the AD_CALCULATION record
	!**********************************************************************

	!
	! Generate a (debit) GL record to pass through to the post function
	!
	GL_YYYY_PP::ACCT	= AD_ACCOUNT_EXAM::EXP_ACCT
	GL_YYYY_PP::SOURCE	= "AD"
	GL_YYYY_PP::REFNO	= AD_CALCULATION::ASSET_NUM
	GL_YYYY_PP::TRANDAT	= END_DATE
	GL_YYYY_PP::DESCR	= AD_35ASSET_EXAM::DESCRIPTION
	GL_YYYY_PP::AMOUNT	= AD_CALCULATION::AMOUNT_CUR
	GL_YYYY_PP::XREFNO	= ""
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= ""
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= AD_CALCULATION::UNIT_CUR
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	! Is the Expense account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(AD_ACCOUNT_EXAM::EXP_ACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set a flag and go on
	!
	CASE CMC$_UNDEFINED
		EXPACCT$ = "*"

	!
	! SNAFU:  (Situation Normal - it's All Fouled Up)
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Put the record into the temporary file
	!
	GOTO Aborted IF &
		GL_TRAN_POSTGL(OPT_ADDREC, SUBOPT_NOOPT, "", TITLE(), &
		UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, &
		GLPERIOD) <> CMC$_NORMAL

	!
	! Generate another (credit) GL record to pass through the post function
	!
	GL_YYYY_PP::ACCT	= AD_ACCOUNT_EXAM::DEP_ACCT
	GL_YYYY_PP::SOURCE	= "AD"
	GL_YYYY_PP::REFNO	= AD_CALCULATION::ASSET_NUM
	GL_YYYY_PP::TRANDAT	= END_DATE
	GL_YYYY_PP::DESCR	= AD_35ASSET_EXAM::DESCRIPTION
	GL_YYYY_PP::AMOUNT	= -AD_CALCULATION::AMOUNT_CUR
	GL_YYYY_PP::XREFNO	= ""
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= ""
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= -AD_CALCULATION::UNIT_CUR
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	! Is the Depreciation account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(AD_ACCOUNT_EXAM::DEP_ACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Depreciation account number undefined; set a flag and go on
	!
	CASE CMC$_UNDEFINED
		DEPACCT$ = "*"

	!
	! SNAFU:  (Situation Normal - it's All Fouled Up)
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Put the record into the temporary file
	!
	GOTO Aborted IF &
		GL_TRAN_POSTGL(OPT_ADDREC, SUBOPT_NOOPT, "", TITLE(), &
		UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, &
		GLPERIOD) <> CMC$_NORMAL

	!
	! Was anything undefined?
	!
	IF INSTR(1%, EXPACCT$ + DEPACCT$ + CALC_ASSET$, "*")
	THEN
		!
		! Put the invalid stuff in TEXT to send to OUTP_UNDEFCODES
		!
		TEXT = CALC_ASSET$ + AD_CALCULATION::ASSET_NUM + " " + &
			EXPACCT$ + AD_ACCOUNT_EXAM::EXP_ACCT + " " + &
			DEPACCT$ + AD_ACCOUNT_EXAM::DEP_ACCT
		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
				UTL_REPORTX, TEXT) <> CMC$_NORMAL

		!
		! Blank flags
		!
		CALC_ASSET$, EXPACCT$, DEPACCT$ = " "

	END IF

	GOTO NextRec

 Confirm:
	!**********************************************************************
	! Confirm posting
	!**********************************************************************
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CONFIRM, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	EXIT_STATUS = EXIT_STATUS AND OUTP_UNDEFCODES(OPT_CONFIRM, TITLE(), &
		UTL_REPORTX, "")

	GOTO Aborted IF EXIT_STATUS <> CMC$_NORMAL

	%PAGE

	!**********************************************************************
	!	1) Since everything's correct and we have the user's
	!		permission, transfer all data from the temporary
	!		files into the ledger
	!**********************************************************************
	!
	! Begin posting
	!
	GOTO Interrupt IF &
		GL_TRAN_POSTGL(OPT_POSTFILE, SUBOPT_DETAIL, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD) <> CMC$_NORMAL

	!
	! Reset some flags
	!
	GOTO Interrupt IF &
		AD_TRAN_POSTAD(OPT_POSTFILE, SUBOPT_LEDGER, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", ADPERIOD) <> CMC$_NORMAL

	%PAGE

 Complete:
	!**********************************************************************
	!	1) Set the Batch Control file so that if we stopped now,
	!		the post would not show up as "interrupted"
	!		(after all, we're finished)
	!**********************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", BATCH_NO$, "", "")

	PRNT_SUMMARY = SUBOPT_FINAL

	%PAGE

 ExitProgram:
	!**********************************************************************
	! Exit normally
	!**********************************************************************
	!
	! Print credit and debit transmittal
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_SUMMARY, PRNT_SUMMARY, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	!
	! Print undefined codes (if any)
	!
	TEXT = " AssetNum    ExpenseAccount      DepreciationAccount "

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
	!**********************************************************************
	! Abort process
	!**********************************************************************
	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", &
			BATCH_NO$, "", "")
		GOTO ExitProgram
	END IF

 Interrupt:
	!**********************************************************************
	! Interrupt process
	!**********************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", BATCH_NO$, "", "")

	GOTO ExitProgram

 HelpError:
	!**********************************************************************
	! Help Message for an error
	!**********************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!**********************************************************************
	! Error trapping
	!**********************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32000	END
