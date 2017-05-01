1	%TITLE "Post Close Journal"
	%SBTTL "WP_POST_CLOSE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:WP0021
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Post Close Journal\* process transfers data from the Close Journal
	!	to the General Ledger.
	!	.lm -5
	!
	! Index:
	!	.x Close Journal>Post
	!	.x Post>Close Journal
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_POST_CLOSE/LINE
	!	$ LINK/EXE=WP_EXE: WP_POST_CLOSE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_POST_CLOSE.OBJ;*
	!
	! Author:
	!
	!	09/08/92 - Dan Perkins
	!		Modified from WP_POST_ISSUE.
	!
	! Modification history:
	!
	!	09/22/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/10/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/19/93 - Frank F. Starman
	!		Assign GLPERIOD while creating a new batch.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/18/95 - Kevin Handy
	!		Clean up comments.
	!		Remove unecessary extern statements.
	!
	!	08/13/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/19/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
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
	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.HB"
	MAP (WP_CLOSEJOUR)	WP_CLOSEJOUR_CDD	WP_CLOSEJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.HB"
	MAP (WP_CLOSELINE)	WP_CLOSELINE_CDD	WP_CLOSELINE

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	DECLARE			UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	DECLARE			GL_YYYY_PP_CDD		GL_YYYY_PP

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES
	EXTERNAL LONG   FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG   FUNCTION	GL_EXAM_CHART

	!
	! Declare internal variables
	!
	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE	LONG			CHECK_PERIOD
	DECLARE LONG			PRNT_SUMMARY

	DECLARE STRING			GLPERIOD
	DECLARE STRING			GL.INTER.PERIOD
	DECLARE	STRING			BATCH_NUMBER
	DECLARE STRING			CHECK_DATE
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
	TITLE(1%) = "CLOSE  JOURNAL  POSTING  PROTOCOL"
	TITLE(2%) = "Work In Process System"
	TITLE(3%) = ""
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

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^* (01) Batch No\*
	!	.lm +5
	!	.b
	!	The ^*Batch No\* field
	!	enters the two (2) character number of a batch to be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Post Close Journal
	!	.x Post>Close Journal>Batch Number
	!	.x Close Journal>Post>Batch Number
	!
	! Required:
	!--

	GLPERIOD = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) G[eneral] L[edger] Period\*
	!	.lm +5
	!	.b
	!	The ^*G[eneral] L[edger] Period\* field
	!	enters the General Ledger accounting period into
	!	which a batch will be posted.
	!	.b
	!	The format for this field is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger Period>Post>Close Journal
	!	.x Post>General Ledger>Close Journal
	!	.x Close Journal>Post>General Ledger
	!
	!--

	CHECK_DATE = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Check Date\*
	!	.lm +5
	!	.b
	!	The ^*Check Date\* field
	!	posts transactions with or without checking dates.
	!	A ^*Y\* value causes the dates in the journal records to be compared with the
	!	General Ledger period into which the journal is to be posted.  If any journal
	!	record date is outside the range of the period into which they are to be
	!	posted, the posting will be aborted.  An ^*N\* value will cause the system to
	!	not compare journal record dates with the period into which they are to be
	!	posted.
	!	.lm -5
	!
	! Index:
	!	.x Check Dates>Close Journal>Post
	!	.x Post>Close Journal>Check Dates
	!	.x Close Journal>Post>Check Dates
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF CHECK_DATE = "Y"

300	!
	! Open WIP CLOSEJOUR header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.UPD"
	USE
		FILENAME$ = "WP_CLOSEJOUR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open WIP CLOSELINE file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.UPD"
	USE
		FILENAME$ = "WP_CLOSELINE"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Subaccount file
	!
	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.CRE"

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
		UTL_REPORTX, "WP_CLOSEJOUR", BATCH_NO$, GL.INTER.PERIOD, "")

	SELECT INTR_STATUS

	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Process was interrupted
	!
	CASE CMC$_WARNING

		GOTO Aborted IF GL_TRAN_POSTGL(OPT_RESTART, &
			SUBOPT_NOOPT, BATCH_NUMBER, TITLE(), &
			UTL_REPORTX, "", "", &
			GL.INTER.PERIOD) <> CMC$_NORMAL

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
		UTL_REPORTX, "WP_CLOSEJOUR", BATCH_NO$, GLPERIOD, "") <> &
		CMC$_NORMAL

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CHECK, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	SELECT EXIT_STATUS
	!
	! Success
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something else wrong
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
		UTL_REPORTX, "WP_CLOSEJOUR", BATCH_NO$, "", "") <> CMC$_NORMAL

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	ACCOUNT$, TRANDATE$ = ""

	RESET #WP_CLOSEJOUR.CH%

 ReadHeader:
	!
	! Read in one record from the header file
	!
3000	WHEN ERROR IN
		GET #WP_CLOSEJOUR.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE Confirm IF ERR = 155% OR ERR = 11%
		FILENAME$ = "WP_CLOSEJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Find the first line item for the header
	!
3100	WHEN ERROR IN
		FIND #WP_CLOSELINE.CH%, KEY #0% EQ WP_CLOSEJOUR::JOB, REGARDLESS
	USE
		CONTINUE ProcessHeader IF ERR = 155%
		FILENAME$ = "WP_ISSUELINE"
		CONTINUE HelpError
	END WHEN

 LineItem:
	!
	! Get the (next) line item
	!
3200	WHEN ERROR IN
		GET #WP_CLOSELINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader IF ERR = 11%
		FILENAME$ = "WP_CLOSELINE"
		CONTINUE HelpError
	END WHEN

	!
	! Go to the next header if we're done with the line items
	!
	GOTO ProcessHeader IF WP_CLOSELINE::JOB <> WP_CLOSEJOUR::JOB

	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::SOURCE	= "WPCL"
	GL_YYYY_PP::REFNO	= ""
	GL_YYYY_PP::TRANDAT	= WP_CLOSEJOUR::CLOSEDATE
	GL_YYYY_PP::XREFNO	= ""
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= WP_CLOSEJOUR::JOB
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	! Is account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(WP_CLOSELINE::VACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS
	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		ACCOUNT$ = "*"

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Check for undefined codes
	!
	IF INSTR(1%, ACCOUNT$, "*")
	THEN
		TEXT$ = WP_CLOSEJOUR::JOB + " " + &
			ACCOUNT$ + WP_CLOSELINE::VACCT + " WIP ACCOUNT"

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		ACCOUNT$ = " "

	END IF

	!
	! Finish the record
	!
	GL_YYYY_PP::ACCT   = WP_CLOSELINE::VACCT
	GL_YYYY_PP::AMOUNT = FUNC_ROUND(WP_CLOSELINE::VAMOUNT, 2%)
	GL_YYYY_PP::UNITS  = 0.0
	GL_YYYY_PP::DESCR  = WP_CLOSELINE::VCLASS

	!
	! Post
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, GLPERIOD)

	!
	! Was the date out of the given range?
	!
	SELECT EXIT_STATUS
	!
	! Date OK; set flag and go on
	!
	CASE CMC$_NORMAL

	!
	! Date out; set flag (if not already set) and go on
	!
	CASE CMC$_DATEOUT
		TRANDATE$ = "*"

	!
	! Strange Days
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Check for undefined codes
	!
	IF INSTR(1%, TRANDATE$, "*")
	THEN
		TEXT$ = WP_CLOSEJOUR::JOB + " " + &
			TRANDATE$ + WP_CLOSEJOUR::CLOSEDATE + &
			" GL DATE OUT OF RANGE"

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		TRANDATE$ = " "

	END IF

	GOTO LineItem

 ProcessHeader:
	GOTO ReadHeader

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	EXIT_STATUS = EXIT_STATUS AND OUTP_UNDEFCODES(OPT_CONFIRM, TITLE(), &
		UTL_REPORTX, "")

	GOTO Aborted IF EXIT_STATUS <> CMC$_NORMAL

	RESET #WP_CLOSEJOUR.CH%

	!
	! Read in one record from the header file
	!
4000	WHEN ERROR IN
		GET #WP_CLOSEJOUR.CH%
		GET #SB_SUBACCOUNT.CH%, KEY #0% EQ "J" + WP_CLOSEJOUR::JOB
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE PostToGL IF ERR = 11% OR ERR = 155%
		FILENAME$ = "WP_CLOSEJOUR"
		CONTINUE HelpError
	END WHEN

	SB_SUBACCOUNT::SSTATUS = "C"
	SB_SUBACCOUNT::EDATE   = WP_CLOSEJOUR::CLOSEDATE

	WHEN ERROR IN
		UPDATE #SB_SUBACCOUNT.CH%
	USE
		FILENAME$ = "WP_CLOSEJOUR"
		CONTINUE HelpError
	END WHEN

	GOTO 4000

	!******************************************************************
	!	1) Since everything's correct and we have the user's
	!		permission, transfer all data from the temporary
	!		files into the ledger
	!******************************************************************
	!
	! Begin posting
	!
	! Post to GL
	!
 PostToGL:
	GOTO Interrupt IF GL_TRAN_POSTGL(OPT_POSTFILE, SUBOPT_DETAIL, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", GLPERIOD) <> CMC$_NORMAL

	%PAGE

	!
	! Remove files
	!
5000	CLOSE WP_CLOSEJOUR.CH%
	CLOSE WP_CLOSELINE.CH%

5010 !	WHEN ERROR IN
 !		KILL WP_CLOSEJOUR.DEV$ + "WP_CLOSEJOUR_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5020
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(WP_CLOSEJOUR.DEV$ + &
		"WP_CLOSEJOUR_" + BATCH_NO$ + ".JRL;*")

5020 !	WHEN ERROR IN
 !		KILL WP_CLOSELINE.DEV$ + "WP_CLOSELINE_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE Complete
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(WP_CLOSELINE.DEV$ + &
		"WP_CLOSELINE_" + BATCH_NO$ + ".JRL;*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "WP_CLOSEJOUR", BATCH_NO$, "", "")

	PRNT_SUMMARY = SUBOPT_FINAL

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************

	!
	! Print credit and debit transmittal
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_SUMMARY, PRNT_SUMMARY, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	!
	! Print undefined codes (if any)
	!
	TEXT = ""

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
			TITLE(), UTL_REPORTX, "WP_CLOSEJOUR", BATCH_NO$, "", "")

		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "WP_CLOSEJOUR", BATCH_NO$, "", "")

	GOTO ExitProgram

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), UTL_REPORTX, TITLE(), 0%)

	GOTO Aborted

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32000	!******************************************************************
	! End of posting program WP_POST_CLOSE
	!******************************************************************
	END
