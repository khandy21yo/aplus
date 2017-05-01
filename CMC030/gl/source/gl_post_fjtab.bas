1	%TITLE "POST - Post Journals to GL"
	%SBTTL "GL_POST_FJTAB"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2003 BY
	!
	! Software Solutions, Inc.
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
	!  Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by  Software Solutions, Inc.
	!
	!++
	! ID:GL030
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	When a batch of General Journals have been entered, printed and
	!	reviewed for accuracy and are accepted as correct, they are to
	!	be posted into the General Ledger. If an entry is not balanced,
	!	the system will indicate there is an imbalance, and the posting
	!	will be aborted. The posting will also abort if there
	!	is an invalid account number in a journal.
	!	.b
	!	If for any reason the posting needs to be aborted,
	!	type ^*Y\* for ^*YES\* when the message is shown on the screen
	!	to abort. If the information is correct, press ^*Do\* to
	!	continue the posting.
	!	.lm -5
	!
	! Index:
	!	.x Post>General Journal>Regular
	!	.x Post>General Journal>Recurring
	!	.x Post>General Journal>Reversing
	!	.x General Journal>Post
	!	.x Regular>Post
	!	.x Recurring>Post
	!	.x Reversing>Post
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_POST_FJTAB/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_POST_FJTAB, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_POST_FJTAB.OBJ;*
	!
	! Author:
	!
	!	07/17/2003 - Kevin Handy
	!
	! Modification history:
	!
	!	09/05/2003 - Kevin Handy
	!		Fix customer number posting to GL for credit card
	!		accounts.
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include constants and and some functions
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG	FUNCTION	GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES
	EXTERNAL LONG	FUNCTION	AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION	AR_TRAN_POSTAR

	!
	! Declare internal variables
	!
	DECLARE	GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM
	DECLARE AR_OPEN_CDD		AR_OPEN
	DECLARE AR_OPEN_DIST_CDD	AR_OPEN_DIST

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE	LONG			PRNT_SUMMARY
	DECLARE LONG			CHECK_PERIOD

	DECLARE	STRING			BATCH_NUMBER
	DECLARE	STRING			GLPERIOD
	DECLARE	STRING			GL_INTER_PERIOD
	DECLARE	STRING			REV_INTER_PERIOD
	DECLARE	STRING			POSTDATE
	DECLARE	STRING			POSTTIME
	DECLARE	STRING			TEXT
	DECLARE	STRING			TITLE(10%)
	DECLARE	STRING			AR.INTER.PERIOD
	DIM READ_BUFFER$(50%)

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
	TITLE(1%) = "FJ  TRANSFER  POSTING  PROTOCOL"
	TITLE(2%) = "General Ledger System"
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

	!
	! Get the user input into a usable form
	!

	!++
	!
	! Abstract:FLD01
	!	.x Journal>Name
	!	^*(01) Journal Name\*
	!	.b
	!	.lm +5
	!	The ^*Journal Name\* field specifies the name of the
	!	text file to read.
	!	An entry is required in this field.
	!	.lm -15
	!
	! Index:
	!	.x Type>Journal
	!
	!--
	JRL_NAME$ = TRM$(UTL_REPORTX::OPTDEF(0%))

	!++
	!
	! Abstract:FLD02
	!	^*(02) Post Period\*
	!	.b
	!	.lm +5
	!	The accounting period to which the General Journal(s)
	!	is(are) to be posted must be entered in the
	!	^*Post Period\* field.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--
	GLPERIOD = TRM$(UTL_REPORTX::OPTDEF(1%))

310	!
	! Open the General Journal file
	!
	WHEN ERROR IN
		JRL_NAME.CH% = 7%
		OPEN JRL_NAME$ FOR INPUT AS FILE JRL_NAME.CH%
	USE
		FILENAME$ = "TEXT.FILE"
		CONTINUE HelpError
	END WHEN

	%PAGE

	!******************************************************************
	!	1) See if the posting process has been interrupted
	!	2) If not interrupted, go on
	!	3) If interrupted, delete the superceded ledger records
	!		and continue
	!******************************************************************

	!******************************************************************
	! Check if posting process has been interrupted
	!******************************************************************

	!
	! Open up batch control file and check if interrupted
	!
	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "TEXT_FILE", JRL_NAME$, &
		GL_INTER_PERIOD, REV_INTER_PERIOD)

	SELECT INTR_STATUS

	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Process was interrupted
	!
	CASE CMC$_WARNING

		IF TRM$(GL_INTER_PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				GL_TRAN_POSTGL(OPT_RESTART, &
				SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", &
				GL_INTER_PERIOD) <> CMC$_NORMAL
		END IF

		IF TRM$(REV_INTER_PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				GL_TRAN_POSTGL(OPT_RESTART, &
				SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", &
				REV_INTER_PERIOD) <> CMC$_NORMAL
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

	!******************************************************************
	! Assign batch number and open control files
	!******************************************************************

	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "TEXT_FILE", JRL_NAME$, &
		GLPERIOD, GL_PERIOD_REV$) <> CMC$_NORMAL

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

	EXIT_STATUS = AR_TRAN_POSTAR(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", "", GLPERIOD)

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

 Transmittal:
	!******************************************************************
	!	1) List the debits/credits transmittal for the user while
	!		also putting the journal data in temporary files
	!	2) Ask the user to confirm whether or not he/she wants to
	!		continue with the posting process
	!	3) If the user gives confirmation, then go on
	!******************************************************************

	!******************************************************************
	! Create transmittal
	!******************************************************************
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", JRL_NAME$, "", "") <> CMC$_NORMAL

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	!
	! Blank flags
	!
	SUBACCT$, EXPACCT$, TRANDATE$ = " "

 NextRec:
1000	WHEN ERROR IN
		INPUT LINE #JRL_NAME.CH%, READ_LINE$
		READ_LINE$ = EDIT$(READ_LINE$, 1% + 4%)
		GOTO 1000 IF READ_LINE$ = ""
	USE
		CONTINUE Confirm IF ERR = 11%
		FILENAME$ = "TEXT_FILE"
		CONTINUE HelpError
	END WHEN

	READ_BUFFER% = 0%
	READ_BUFFER$(I%) = "" FOR I% = 1% TO 30%

	WHILE READ_LINE$ <> ""
		I% = INSTR(1%, READ_LINE$, "	")
		I% = LEN(READ_LINE$) + 1% IF I% = 0%
		READ_BUFFER% = READ_BUFFER% + 1%
		TEMP$ = LEFT(READ_LINE$, I% - 1%)
		IF LEFT(TEMP$, 1%) = '"'
		THEN
			TEMP$ = SEG$(TEMP$, 2%, LEN(TEMP$) - 1%)
		END IF
		READ_BUFFER$(READ_BUFFER%) = TEMP$
		READ_LINE$ = RIGHT(READ_LINE$, I% + 1%)
	NEXT

	!******************************************************************
	! Create a GL_YYYY_PP record from the GL_GJ_LINE record
	!******************************************************************
	ACCT$ = READ_BUFFER$(1%)
	IF INSTR(1%, ACCT$, ".") = 0%
	THEN
		ACCT$ = ACCT$ + "." +  READ_BUFFER$(12%)
	END IF

	!
	! Skip Blanks
	!
	AMOUNT = VAL(READ_BUFFER$(9%))
	SUBACCT$ = READ_BUFFER$(12%)

	SELECT TRM$(ACCT$)
	CASE "130.01"
		CUSNUM$ = "IFCHEV    "
	CASE "130.02"
		CUSNUM$ = "IFSINC    "
	CASE "130.03"
		CUSNUM$ = "IFPHIL    "
	CASE ELSE
		CUSNUM$ = READ_BUFFER$(6%)
	END SELECT

	!
	! Try to add in the units/Second line of data
	!
	! Hopefully this is enough to mage sure it is unique
	!
	IF (GL_YYYY_PP::ACCT = ACCT$) AND &
		(GL_YYYY_PP::REFNO = READ_BUFFER$(7%)) AND &
		(GL_YYYY_PP::XREFNO = CUSNUM$) AND &
		(GL_YYYY_PP::DESCR = READ_BUFFER$(8%)) AND &
		(GL_YYYY_PP::TRANKEY = READ_BUFFER$(3%))
	THEN
		GL_YYYY_PP::UNITS = FUNC_ROUND(GL_YYYY_PP::UNITS + &
			VAL(READ_BUFFER$(10%)), 2%)
		GL_YYYY_PP::AMOUNT = FUNC_ROUND(GL_YYYY_PP::AMOUNT + AMOUNT, 2%)

		GOTO NextRec
	END IF

	!
	! This code is used to delay actually writing GL record so that
	! units can be attached from the next record read.
	!
	IF GL_YYYY_PP::AMOUNT <> 0.0
	THEN
		GOSUB PostIt
		GL_YYYY_PP::AMOUNT = 0.0
	END IF

	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::ACCT	= ACCT$
	GL_YYYY_PP::SOURCE	= "TX"
	GL_YYYY_PP::REFNO	= READ_BUFFER$(7%)
	GL_YYYY_PP::TRANDAT	= RIGHT(READ_BUFFER$(2%), 5%) + &
		LEFT(READ_BUFFER$(2%), 4%)
	GL_YYYY_PP::DESCR	= READ_BUFFER$(8%)
	GL_YYYY_PP::AMOUNT	= AMOUNT
	GL_YYYY_PP::XREFNO	= CUSNUM$
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= READ_BUFFER$(3%)
	GL_YYYY_PP::SUBACC	= SUBACCT$
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= VAL(READ_BUFFER$(10%))
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= "  "
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	! See if GL Chart number is defined
	!
	EXIT_STATUS = GL_EXAM_CHART(GL_YYYY_PP::ACCT, GL_CHART_EXAM)

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

 GoToNext:
	!
	! Was anything undefined?
	!
	IF INSTR(1%, TRANDATE$ + EXPACCT$ + SUBACCT$, "*")
	THEN
		!
		! Put the invalid stuff in TEXT to send to OUTP_UNDEFCODES
		!
		TEXT = TRANDATE$ + PRNT_DATE(GL_YYYY_PP::TRANDAT, 8%) + " " + &
			EXPACCT$ + GL_YYYY_PP::ACCT + " " + &
			SUBACCT$ + GL_YYYY_PP::SUBACC

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, &
				TEXT) <> CMC$_NORMAL

		!
		! Blank flags
		!
		SUBACCT$, EXPACCT$, TRANDATE$ = " "

	END IF

	SELECT TRM$(ACCT$)
	CASE "130.01"
		CUSNUM$ = "IFCHEV    "
	CASE "130.02"
		CUSNUM$ = "IFSINC    "
	CASE "130.03"
		CUSNUM$ = "IFPHIL    "
	CASE ELSE
		GOTO EndAR
	END SELECT

	INVNUM$ = READ_BUFFER$(3%)
	TRADAT$ = RIGHT(READ_BUFFER$(2%), 5%) + &
		LEFT(READ_BUFFER$(2%), 4%)

	!
	! Set some initial variable values
	!
	TOTAL.DISCOUNT, TOTAL.OTHER = 0.0
	PRINTHEAD% = 0%

	!
	! Get the customer description
	!
	EXIT_STATUS = AR_EXAM_CUSTOM(CUSNUM$, AR_35CUSTOM_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		CUSTNUM$ = "*"

	!
	! SNAFU:  Situation Normal - All Fouled Up
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Generate a AR record to pass through to the post function
	!
	AR_OPEN::CUSNUM		= CUSNUM$
	AR_OPEN::INVNUM		= INVNUM$
	AR_OPEN::TRATYP		= "01"
	AR_OPEN::TRADAT		= TRADAT$
	AR_OPEN::SALAMT		= VAL(READ_BUFFER$(9%))
	AR_OPEN::DISAMT		= 0.0
	AR_OPEN::OTHCHG		= 0.0
	AR_OPEN::RECNUM		= ""
	AR_OPEN::CHKNUM		= ""
	AR_OPEN::ARACCT		= ACCT$
	AR_OPEN::SUBACC		= SUBACCT$
	AR_OPEN::SALNUM		= ""
	AR_OPEN::DESCR		= READ_BUFFER$(8%)
	AR_OPEN::BATCH		= BATCH_NUMBER
	AR_OPEN::UPDATED	= GLPERIOD + "00"
	AR_OPEN::CLOSEDATE	= ""
	AR_OPEN::DUEDATE	= RIGHT(READ_BUFFER$(2%), 5%) + &
		LEFT(READ_BUFFER$(2%), 4%)
	AR_OPEN::DISCOUNTDATE	= RIGHT(READ_BUFFER$(2%), 5%) + &
		LEFT(READ_BUFFER$(2%), 4%)

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_OPEN, AR_OPEN_DIST, AR_SALTAXLED, GLPERIOD) <> CMC$_NORMAL

	!
	! Was anything undefined?
	!
	IF INSTR(1%, CUSTNUM$ + ARACCT$ + TRANDAT$, "*") AND (PRINTHEAD% = 0%)
	THEN
		!
		! Put the invalid stuff in TEXT to send to OUTP_UNDEFCODES
		!
		TEXT = "HEADER " + INVNUM$ + " " + &
			CUSTNUM$ + CUSNUM$ + " " + &
			ARACCT$ + ACCT$ + " " + &
			TRANDAT$ + PRNT_DATE(TRADAT$, 8%)

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, &
				TEXT) <> CMC$_NORMAL

		!
		! Blank flags
		!
		CUSTNUM$, ARACCT$, TRANDAT$ = " "

	END IF


 EndAR:
	GOTO NextRec

	!*******************************************************************
	! Actually write record to GL
	!*******************************************************************
 PostIt:
	!
	! Put the record into the temporary files
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, "", TITLE(), &
		UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, GLPERIOD)

	!
	! Check the date; is it in the correct period?
	!
	SELECT EXIT_STATUS

	!
	! The date is correct; keep going
	!
	CASE CMC$_NORMAL, CMC$_WARNING

	!
	! The date's off; set a flag and go on
	!
	CASE CMC$_DATEOUT
		TRANDATE$ = "*"

	!
	! A definite problem of some sort
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	RETURN

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************
	IF GL_YYYY_PP::AMOUNT <> 0.0
	THEN
		GOSUB PostIt
		GL_YYYY_PP::AMOUNT = 0.0
	END IF

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CONFIRM, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD)

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
		AR_TRAN_POSTAR(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "", GLPERIOD) <> CMC$_NORMAL

	%PAGE

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", JRL_TYPE$, "", "")

	PRNT_SUMMARY = SUBOPT_FINAL

	GOTO ExitProgram

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************
	!
	! Print credit and debit transmittal
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_SUMMARY, PRNT_SUMMARY, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_SUMMARY, PRNT_SUMMARY + SUBOPT_REVERSE, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", GL_PERIOD_REV$) &
		IF (JRL_TYPE$ = "3") OR (JRL_TYPE$ = "4")

	!
	! Print undefined code if any
	!
	TEXT = " TransDate   Account             SubAccount"

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
			TITLE(), UTL_REPORTX, &
			"", JRL_TYPE$, "", "")
		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", JRL_TYPE$, "", "")

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
	! End of posting program GL_POST_FJTAB
	!******************************************************************
	END
