1	%TITLE "POST - Post Journals to GL"
	%SBTTL "GL_POST_GJPOST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:REVPST
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
	!	$ BAS GL_SOURCE:GL_POST_GJPOST/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_POST_GJPOST, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_POST_GJPOST.OBJ;*
	!
	! Author:
	!
	!	06/16/89 - Aaron Redd
	!
	! Modification history:
	!
	!	11/07/89 - Kevin Handy
	!		Fixed bug in handling single recurring
	!		journal item.  (Variable naming problem)
	!
	!	10/05/90 - Kevin Handy
	!		Added journal type "4", Recurring/Reversing.
	!
	!	05/06/91 - J. Shad Rydalch
	!		Removed all calls to SB_TRAN_POST since those were
	!		moved into GL_TRAN_POSTGL.
	!
	!	09/16/91 - Kevin Handy
	!		Modified to also zero SUBACCT$ at start of program.
	!
	!	09/16/91 - Kevin Handy
	!		Modified so that transaction date is not a fatal
	!		error.
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/21/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	07/15/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	10/02/2000 - Kevin Handy
	!		Modify to allow the "PRINT_ITEMS" to work for
	!		batches other than "2" and "4" (recurring) by
	!		deleting items that actually post instead of
	!		killing the entire file, (Pwells)
	!
	!	10/06/2000 - Kevin Handy
	!		Fix case when PRINT_ITEMS is blank so it will
	!		actually post something.
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
	! Map statements
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_GJ_LINE.HB"
	MAP	(GL_GJ_LINE)	GL_GJ_LINE_CDD	GL_GJ_LINE

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG	FUNCTION	GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES

	!
	! Declare internal variables
	!
	DECLARE	GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX

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
	TITLE(1%) = "  JOURNAL  POSTING  PROTOCOL"
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
	!	.x Journal>Type
	!	^*(01) Journal Type\*
	!	.b
	!	.lm +5
	!	The ^*Journal Type\* field is used to specify which journal
	!	type is to be posted.  Valid values are:
	!	.TABLE 3,25
	!	.te
	!	^*1\* - General
	!	.TE
	!	^*2\* - Recurring
	!	.TE
	!	^*3\* - Reversing
	!	.TE
	!	^*4\* - Recurring/Reversing
	!	.END TABLE
	!	An entry is required in this field.
	!	.lm -15
	!
	! Index:
	!	.x Type>Journal
	!
	!--
	JRL_TYPE$ = TRM$(UTL_REPORTX::OPTDEF(0%))

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

	!++
	!
	! Abstract:FLD03
	!	^*(03) Select Journal _#\*
	!	.b
	!	.lm +5
	!	This field selects specific Recurring
	!	General Journals to be posted.  If all Recurring journals are
	!	to be posted, the field is left blank.
	!	.b
	!	To select a single journal to print, type in the number of the
	!	journal.  To select a series of journals, type in the numbers
	!	separated by commas.  Up to twenty characters may be entered.
	!	.lm -5
	!
	! Index:
	!
	! Required:
	!--
	PRINT_ITEMS$ = TRM$(UTL_REPORTX::OPTDEF(2%))

	!++
	!
	! Abstract:FLD04
	!	^*(04) Reversal Period\*
	!	.b
	!	.lm +5
	!	The accounting period to which the General Journal(s)
	!	is(are) to be reversed must be entered in this
	!	field.  The format for entry is YYYYPP.
	!	.b
	!	Note: This field is used only when posting type 3 (reversing) and
	!	type 4 (Recurring/Reversing) journals.
	!	It is ignored for all others.
	!	.lm -5
	!
	! Index:
	!
	!--
	GL_PERIOD_REV$ = TRM$(UTL_REPORTX::OPTDEF(3%))

	!++
	!
	! Abstract:FLD05
	!	^*(05) Journal Date\*
	!	.b
	!	.lm +5
	!	It is appropriate to change the ^*Journal Date\* of
	!	all Recurring General Journals each accounting
	!	period in order that the Journal Date coincides
	!	with the date of the accounting period.  The format for entry is MMDDYYYY
	!	or MMDDYY.
	!	.b
	!	Note:  This field is only used for journal type 2 (Recurring)
	!	and type 4 (Recurring/Reversing).  It is ignored for all others.
	!	.lm -5
	!
	! Index:
	!
	!--
	REC_DATE$ = MID(UTL_REPORTX::OPTDEF(4%), 7%, 4%) + &
		LEFT(UTL_REPORTX::OPTDEF(4%), 2%) + &
		MID(UTL_REPORTX::OPTDEF(4%), 4%, 2%)
	!++
	! Abstract:FLD06
	!	^*(06) Check Dates\*
	!	.b
	!	.lm +5
	!	The ^*Check Dates\* field allows checking the transaction
	!	date before posting. If ^*Y (Yes)\* is entered, the date of the
	!	transaction will be checked to ensure it coincides with the specified posting
	!	period. If ^*N (No)\* is entered, the transaction date will not be checked and
	!	all transactions will be posted.
	!	.lm -5
	!
	! Index:
	!
	!--
	CHECK_DATE$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF (CHECK_DATE$ = "Y")

	!
	! Figure out which journal type GJ is and alter the title accordingly
	!
	SELECT JRL_TYPE$

	CASE "2"
		TITLE(1%) = "RECURRING" + TITLE(1%)

	CASE "3"
		TITLE(1%) = "REVERSING" + TITLE(1%)

	CASE "4"
		TITLE(1%) = "RECURRING/REVERSING" + TITLE(1%)

	CASE ELSE
		TITLE(1%) = "GENERAL" + TITLE(1%)
		JRL_TYPE$ = "1"

	END SELECT

310	!
	! Open the General Journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_GJ_LINE.UPD"
	USE
		IF ERR = 138%
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "GL_GJ_LINE.UPD"
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
		UTL_REPORTX, "GL_GJ_LINE", JRL_TYPE$, &
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
		UTL_REPORTX, "GL_GJ_LINE", JRL_TYPE$, &
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

	GOTO Transmittal IF (JRL_TYPE$ <> "3") AND (JRL_TYPE$ <> "4")

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", GL_PERIOD_REV$)

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
		UTL_REPORTX, "", JRL_TYPE$, "", "") <> CMC$_NORMAL

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	!
	! Blank flags
	!
	SUBACCT$, EXPACCT$, TRANDATE$ = " "

	RESET #GL_GJ_LINE.CH%

 NextRec:
1000	WHEN ERROR IN
		GET #GL_GJ_LINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE Confirm IF ERR = 11%
		FILENAME$ = "GL_GJ_LINE"
		CONTINUE HelpError
	END WHEN

	!
	! Handle the Recurring journal's selective setup
	!
	GOTO NextRec IF &
		(PRINT_ITEMS$ <> "") AND &
		(COMP_STRING(GL_GJ_LINE::JOURNAL, PRINT_ITEMS$) = 0%)

	!******************************************************************
	! Create a GL_YYYY_PP record from the GL_GJ_LINE record
	!******************************************************************

	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::ACCT	= GL_GJ_LINE::ACCT
	GL_YYYY_PP::SOURCE	= GL_GJ_LINE::SOURCE
	GL_YYYY_PP::REFNO	= GL_GJ_LINE::JOURNAL
	GL_YYYY_PP::TRANDAT	= GL_GJ_LINE::TRANDAT
	GL_YYYY_PP::TRANDAT	= REC_DATE$ &
		IF ((JRL_TYPE$ = "2") OR (JRL_TYPE$ = "4"))
	GL_YYYY_PP::DESCR	= GL_GJ_LINE::DESCR
	GL_YYYY_PP::AMOUNT	= GL_GJ_LINE::AMOUNT
	GL_YYYY_PP::XREFNO	= GL_GJ_LINE::XREFNO
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::CKNO	= GL_GJ_LINE::CKNO
	GL_YYYY_PP::TRANKEY	= GL_GJ_LINE::TRANKEY
	GL_YYYY_PP::SUBACC	= GL_GJ_LINE::SUBACC
	GL_YYYY_PP::OPERATION	= GL_GJ_LINE::OPERATION
	GL_YYYY_PP::UNITS	= GL_GJ_LINE::UNITS
	GL_YYYY_PP::HOURS	= GL_GJ_LINE::HOURS
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

 GoToNext:
	!
	! Was anything undefined?
	!
	IF INSTR(1%, TRANDATE$ + EXPACCT$ + SUBACCT$, "*")
	THEN
		!
		! Put the invalid stuff in TEXT to send to OUTP_UNDEFCODES
		!
		TEXT = TRANDATE$ + PRNT_DATE(GL_GJ_LINE::TRANDAT, 8%) + " " + &
			EXPACCT$ + GL_GJ_LINE::ACCT + " " + &
			SUBACCT$ + GL_GJ_LINE::SUBACC

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

	GOTO NextRec

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************
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
	! Post Reversed information
	!
	IF (JRL_TYPE$ = "3") OR (JRL_TYPE$ = "4")
	THEN
		GOTO Interrupt IF &
			GL_TRAN_POSTGL(OPT_POSTFILE, &
			SUBOPT_DETAIL + SUBOPT_REVERSE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", GL_PERIOD_REV$) <> &
			CMC$_NORMAL
	END IF

	%PAGE

1500	!******************************************************************
	!	Kill the now-useless journal
	!******************************************************************

	IF (JRL_TYPE$ <> "2") AND (JRL_TYPE$ <> "4")
	THEN
		IF  PRINT_ITEMS$ = ""
		THEN
			CLOSE GL_GJ_LINE.CH%
			SMG_STATUS% = LIB$DELETE_FILE(GL_GJ_LINE.DEV$ + &
				"GL_GJ_" + JRL_TYPE$ + ".JRL;*")
		ELSE
			GOSUB KillLines
			CLOSE GL_GJ_LINE.CH%
		END IF
	END IF

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

 KillLines:
	!*******************************************************************
	! Kill individual lines from the file instead of killing the entire
	! journal
	!*******************************************************************

12000	RESET #GL_GJ_LINE.CH%

12010	WHEN ERROR IN
		GET #GL_GJ_LINE.CH%
	USE
		CONTINUE 12090 IF ERR = 11%
		FILENAME$ = "GL_GJ_LINE"
		CONTINUE HelpError
	END WHEN

	!
	! Kill line if it was posted
	!
	IF COMP_STRING(GL_GJ_LINE::JOURNAL, PRINT_ITEMS$) <> 0%
	THEN
		DELETE #GL_GJ_LINE.CH%
	END IF

	GOTO 12010

12090	RETURN

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

	%PAGE

32000	!******************************************************************
	! End of posting program GL_POST_GJPOST
	!******************************************************************
	END
