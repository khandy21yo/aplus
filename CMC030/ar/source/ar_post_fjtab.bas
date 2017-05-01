1	%TITLE "Accounts Receivable Sales Journal Post Program"
	%SBTTL "AR_POST_FJTAB"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2003 BY
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:SJPST
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Post Sales Journal\* option is provided to post entries
	!	which have been made in the Sales Journal to the General
	!	Ledger System and the Accounts Receivable Register.
	!	.b
	!	If the entries being posted are out of balance, the message,
	!	^*"Batch is OUT OF BALANCE - POSTING IS ABORTED. Hit any key
	!	to continue"\*, will appear on the screen. Any imbalance must
	!	be corrected before the posting can be executed. Corrections
	!	are made in the Sales Journal Maintenance routine.
	!	.b
	!	After the posting is completed, the system will return to the
	!	Sales Journal Menu screen.
	!	.lm -5
	!
	! Index:
	!	.x Post>Sales Journal
	!	.x Sales Journal>Post
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_POST_FJTAB/LINE
	!	$ LINK/EXE=AR_EXE: AR_POST_FJTAB, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_POST_FJTAB.OBJ;*
	!
	! Author:
	!
	!	07/17/2003 - Kevin Handy
	!
	! Modification history:
	!
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
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION	AR_TRAN_POSTAR
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES

	!
	! Declare internal variables
	!
	DECLARE AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM
	DECLARE AR_OPEN_CDD		AR_OPEN
	DECLARE AR_OPEN_DIST_CDD	AR_OPEN_DIST
	DECLARE AR_SALTAXLED_CDD	AR_SALTAXLED
	DECLARE	GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE	LONG			PRNT_SUMMARY
	DECLARE LONG			CHECK_PERIOD

	DECLARE	STRING			BATCH_NUMBER
	DECLARE	STRING			GLPERIOD
	DECLARE	STRING			AR.INTER.PERIOD
	DECLARE	STRING			GL.INTER.PERIOD
	DECLARE	STRING			POSTDATE
	DECLARE	STRING			POSTTIME
	DECLARE	STRING			TEXT
	DECLARE	STRING			TITLE(10%)
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
	TITLE(1%) = "FJ  TRANSMITTAL  JOURNAL  POSTING  PROTOCOL"
	TITLE(2%) = "Accounts Receivable System"
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
	!	.x Post Period>Post Sales Journal
	!	^*(01) Post Period\*
	!	.b
	!	.lm +5
	!	The ^*Post Period\* field determines
	!	the year and the accounting period corresponding to which the
	!	Sales Journal records are to be posted in the General Ledger.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Post Sales Journal>Post Period
	!
	!--

	FILE_NAME$ = TRM$(UTL_REPORTX::OPTDEF(1%))
	!++
	! Abstract:FLD02
	!	^*(02) Batch to Post\*
	!	.b
	!	.lm +5
	!	The ^*Batch to Post\* field allows for entry of the batch
	!	_# of the Sales Journal which is to be posted.
	!	.b
	!	Only one batch at a time may be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch to Post>Post Sales Journal
	!	.x Post Sales Journal>Batch to Post
	!
	!--

300	!
	! Open Accounts Receivable SJ header file
	!
	WHEN ERROR IN
		FILE_NAME.CH% = 7%
		OPEN FILE_NAME$ FOR INPUT AS FILE FILE_NAME.CH%
	USE
		FILENAME$ = "TEXT_FILE"
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
		UTL_REPORTX, "TEXT_FILE", FILE_NAME$, &
		GL.INTER.PERIOD, AR.INTER.PERIOD)

	SELECT INTR_STATUS

	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Process was interrupted
	!
	CASE CMC$_WARNING

		IF TRM$(AR.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				AR_TRAN_POSTAR(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"", "", "", AR.INTER.PERIOD) <> CMC$_NORMAL
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
		UTL_REPORTX, "TEXT_FILE", FILE_NAME$, &
		GLPERIOD, "") <> CMC$_NORMAL

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

	!******************************************************************
	!	1) List the debits/credits transmittal for the user while
	!		also putting the journal data in temporary files
	!	2) If confirmation, then go on
	!******************************************************************

	!******************************************************************
	! Create transmittal
	!******************************************************************
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "TEXT_FILE", FILE_NAME$, &
		"", "") <> CMC$_NORMAL

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	!
	! Blank flags
	!
	CUSTNUM$, ARACCT$, TRANDAT$ = " "

 ReadHeader:
	!
	! Read in one record from the header file
	!
3000	WHEN ERROR IN
		INPUT LINE #FILE_NAME.CH%, READ_LINE$
		READ_LINE$ = EDIT$(READ_LINE$, 1% + 4%)
		GOTO 3000 IF READ_LINE$ = ""
	USE
		CONTINUE Confirm
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

	CUSNUM$ = READ_BUFFER$(1%)
	INVNUM$ = READ_BUFFER$(3%)
	ACCT$ = "130.00"
	TRADAT$ = RIGHT(READ_BUFFER$(4%), 5%) + &
		LEFT(READ_BUFFER$(4%), 4%)

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
	AR_OPEN::SALAMT		= VAL(READ_BUFFER$(7%))
	AR_OPEN::DISAMT		= 0.0
	AR_OPEN::OTHCHG		= 0.0
	AR_OPEN::RECNUM		= ""
	AR_OPEN::CHKNUM		= ""
	AR_OPEN::ARACCT		= ACCT$
	AR_OPEN::SUBACC		= READ_BUFFER$(2%)
	AR_OPEN::SALNUM		= ""
	AR_OPEN::DESCR		= READ_BUFFER$(8%)
	AR_OPEN::BATCH		= BATCH_NUMBER
	AR_OPEN::UPDATED	= GLPERIOD + "00"
	AR_OPEN::CLOSEDATE	= ""
	AR_OPEN::DUEDATE	= RIGHT(READ_BUFFER$(5%), 5%) + &
		LEFT(READ_BUFFER$(5%), 4%)
	AR_OPEN::DISCOUNTDATE	= RIGHT(READ_BUFFER$(5%), 5%) + &
		LEFT(READ_BUFFER$(5%), 4%)

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

	GOTO ReadHeader

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************

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

	!
	! Post the AR Sales Journal header
	!
	GOTO Interrupt IF &
		AR_TRAN_POSTAR(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "", GLPERIOD) <> CMC$_NORMAL


	%PAGE

	!
	! Remove files
	!
5000	CLOSE FILE_NAME.CH%


 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "TEXT_FILE", FILE_NAME$, "", "")

	PRNT_SUMMARY = SUBOPT_FINAL

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************

	!
	! Print undefined codes (if any)
	!
	TEXT = "Item   Invoice CustomerNum AR AccountNum       " + &
		"TransDate SubAcct"

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
			TITLE(), UTL_REPORTX, "TEXT_FILE", &
			FILE_NAME$, "", "")
		GOTO ExitProgram
	END IF

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "TEXT_FILE", FILE_NAME$, "", "")

	GOTO ExitProgram

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
	! End of posting program AR_POST_FJTAB
	!******************************************************************
	END
