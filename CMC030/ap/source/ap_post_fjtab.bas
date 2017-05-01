1	%TITLE "Post Purchases Journal"
	%SBTTL "AP_POST_FJTAB"
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
	! ID:APPJPO
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Post Purchases Journal\* option posts
	!	the Purchases Journal transactions into the General Ledger,
	!	the Accounts Payable Subsidiary Ledger,
	!	the Job Costing System,
	!	and the Purchase Order Register.
	!	.note RETENTION
	!	The Purchases Journal Posting Transmittal report
	!	should be permanently filed along with the related
	!	Purchases Journal report.
	!	.end note
	!	^*
	!	.note
	!	Both the Print Purchases Journal and Print Invoice
	!	Attachments routines must be executed prior to posting
	!	a Purchases Journal batch, since the batch file is
	!	deleted upon the completion of the posting routine.\*
	!	.end note
	!
	! Index:
	!	.x Purchase Journal>Post
	!	.x Post>Purchase Journal
	!	.Y PO
	!	.Y PURCHASEORDERSYSTEM
	!	.Y PURCHASEORDER
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_POST_FJTAB/LINE
	!	$ LINK/EXE=AP_EXE: AP_POST_FJTAB, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_POST_FJTAB.OBJ;*
	!
	! Author:
	!
	!	07/17/2003 - Kevin Handy
	!
	! Modification history:
	!
	!	05/12/2004 - Kevin Handy
	!		Allocate transaction numbers using control file
	!		instead of trying to base it on CStore number.
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
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD		AP_CONTROL

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	AP_EXAM_VENDOR
	EXTERNAL LONG	FUNCTION	AP_TRAN_POSTAP
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES

	!
	! Declare internal variables
	!
	DECLARE	AP_OPEN_CDD		AP_OPEN
	DECLARE	AP_OPEN_DIST_CDD	AP_OPEN_DIST
	DECLARE	AP_VENDOR_CDD		AP_VENDOR_EXAM
	DECLARE	GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE PO_REG_LINE_CDD		PO_REG_LINE
	DECLARE PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE

	!
	! Record for remembering account/amount distribution for one
	! pj header.
	!
	RECORD ACCT_SUMMARY_CDD
		STRING	ACCT = 18%
		GFLOAT	AMOUNT
	END RECORD

	!
	! Dimension arrays
	!
	DIM ACCT_SUMMARY_CDD ACCOUNT_LIST(100%), CASH_LIST(100%)

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE	LONG			PRNT_SUMMARY
	DECLARE LONG			CHECK_PERIOD

	DECLARE	STRING			BATCH_NUMBER
	DECLARE	STRING			GLPERIOD
	DECLARE	STRING			AP.INTER.PERIOD
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
	TITLE(1%) = "FJ  TRANSFER  JOURNAL  POSTING  PROTOCOL"
	TITLE(2%) = "Accounts Payable System"
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
	!	^*(01) YearPeriod (YYYYPP)\*
	!	.b
	!	.lm +5
	!	The ^*YearPeriod\* field
	!	posts a Purchases Journal
	!	into a specific accounting period.
	!	.b
	!	Enter the period into which the journal file is to be
	!	posted in YYYYPP format.
	!	.lm -5
	!
	! Index:
	!	.x YearPeriod>Post Purchase Journal
	!	.x Post Purchase Journal>YearPeriod
	!
	!--

	FILE_NAME$ = TRM$(UTL_REPORTX::OPTDEF(1%))

	!++
	! Abstract:FLD02
	!	^*(02) File Name\*
	!	.b
	!	.lm +5
	!	The name of the transfer file to read from.
	!	.lm -5
	!
	! Index:
	!	.x File Name>Post Purchase Journal
	!	.x Post Purchase Journal>File Name
	!
	!--

310	!
	! Open journal header file
	!
	WHEN ERROR IN
		FILE_NAME.CH% = 7%
		OPEN FILE_NAME$ FOR INPUT AS FILE FILE_NAME.CH%
	USE
		FILENAME$ = "TEXT.FILE"
		CONTINUE HelpError
	END WHEN

320	!
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.MOD"
	USE
		FILENAME$ = "AP_CONTROL"
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
		GL.INTER.PERIOD, AP.INTER.PERIOD)

	SELECT INTR_STATUS

	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Process was interrupted
	!
	CASE CMC$_WARNING

		IF TRM$(AP.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				AP_TRAN_POSTAP(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, "", "", &
				AP.INTER.PERIOD) <> CMC$_NORMAL
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

	EXIT_STATUS = AP_TRAN_POSTAP(OPT_CHECK, SUBOPT_NOOPT, &
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
		UTL_REPORTX, "TEXT_FILE", BATCH_NO$, "", "") <> CMC$_NORMAL

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	!
	! Blank flags
	!
	VENDNUM$, CASHACCT$, APACCT$, TRANDAT$, PONUM$ = " "

 ReadHeader:
	!
	! Read in one record from the header file
	!
500	WHEN ERROR IN
		INPUT LINE #FILE_NAME.CH%, READ_LINE$
		READ_LINE$ = EDIT$(READ_LINE$, 1% + 4%)
		GOTO 500 IF READ_LINE$ = ""
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

	VENNUM$ = READ_BUFFER$(1%)

	SELECT VENNUM$
	CASE "DONUTS"
		VENNUM$ = "MISC"
	END SELECT

 !	TRANKEY$ = READ_BUFFER$(9%)
	GOSUB GetTrans
	AP_ACCT$ = "301.00"

	!
	! Check the vendor number - is it defined?
	!
	EXIT_STATUS = AP_EXAM_VENDOR(VENNUM$, AP_VENDOR_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		VENDNUM$ = "*"

	!
	! SNAFU:  Situation Normal - All Fouled Up
	!
	CASE ELSE
		TEXT$ = "Transaction " + TRM$(TRANKEY$) + &
			" - " + TRM$(VENNUM$) + &
			"Invalid Vendor Number!"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted

	END SELECT

	!
	! See if AP Account number is defined
	!
	EXIT_STATUS = GL_EXAM_CHART(AP_ACCT$, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set a flag and go on
	!
	CASE CMC$_UNDEFINED
		APACCT$ = "*"

	!
	! SNAFU:  (Situation Normal - it's All Fouled Up)
	!
	CASE ELSE
		TEXT$ = "Transaction " + TRM$(TRANKEY$) + &
			" - " + TRM$(VENNUM$) + " " + &
			TRM$(AP_ACCT$) + &
			" Invalid GL Account!"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO Aborted

	END SELECT

	!
	! Set initial values for some variables
	!
	ACCOUNT_LIST% = 0%
	CASH_LIST% = 0%
	USE_AMT = 0.0
	PRINTHEAD% = 0%
	ACCTNUM$ = " "
	VARACCTNUM$ = " "
	SUBACCT$ = " "

 ProcessHeader:
	!
	! Process the header
	!
	! Generate a AP record to pass through to the post function
	!
	AP_OPEN::VENNUM		= VENNUM$
	AP_OPEN::TRANKEY	= TRANKEY$
	AP_OPEN::TRANKEY_DATE	= RIGHT(READ_BUFFER$(3%), 5%) + &
		LEFT(READ_BUFFER$(3%), 4%)
	AP_OPEN::INVNUM		= READ_BUFFER$(2%)
	AP_OPEN::INVDAT		= RIGHT(READ_BUFFER$(3%), 5%) + &
		LEFT(READ_BUFFER$(3%), 4%)
	AP_OPEN::INVAMT		= VAL(READ_BUFFER$(6%))
	AP_OPEN::CODE_1099	= ""
	AP_OPEN::AMT_1099	= 0.0
	AP_OPEN::USE_JOB_NUM	= ""
	AP_OPEN::USE_AMT	= 0.0
	AP_OPEN::DISCDAT	= RIGHT(READ_BUFFER$(4%), 5%) + &
		LEFT(READ_BUFFER$(4%), 4%)
	AP_OPEN::DISAMT		= 0.0
	AP_OPEN::DUEDAT		= RIGHT(READ_BUFFER$(4%), 5%) + &
		LEFT(READ_BUFFER$(4%), 4%)
	AP_OPEN::PONUM		= ""
	AP_OPEN::AP_ACCT	= AP_ACCT$
	AP_OPEN::CASH_ACCT	= ""
	AP_OPEN::CKNUM		= ""
	AP_OPEN::CKDAT		= ""
	AP_OPEN::CKDESC		= ""
	AP_OPEN::CKAMT		= 0.0
	AP_OPEN::UPDATED	= GLPERIOD + "00"
	AP_OPEN::CLOSEDATE	= ""
	AP_OPEN::SELECTED	= ""
	AP_OPEN::BATCH		= BATCH_NUMBER

	AP_OPEN::DUEDAT = AP_OPEN::TRANKEY_DATE IF AP_OPEN::DUEDAT = ""
	AP_OPEN::DISCDAT = AP_OPEN::TRANKEY_DATE IF AP_OPEN::DISCDAT = ""

	!
	! Put the AP Open record into the temporary file
	!
	GOTO Aborted IF AP_TRAN_POSTAP(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AP_OPEN, AP_OPEN_DIST, "") <> CMC$_NORMAL

	!
	! Was anything undefined?
	!
700	IF INSTR(1%, VENDNUM$ + CASHACCT$ + APACCT$ + TRANDAT$, "*") AND &
		(PRINTHEAD% = 0%)
	THEN
		!
		! Put the invalid stuff in TEXT to send to OUTP_UNDEFCODES
		!
		TEXT = "HEADER " + TRANKEY$ + " " + &
			VENDNUM$ + VENNUM$ + " " + &
			APACCT$ + AP_ACCT$ + " " + &
			TRANDAT$ + PRNT_DATE(RIGHT(READ_BUFFER$(3%), 5%) + &
				LEFT(READ_BUFFER$(3%), 4%), 8%)

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT) <> CMC$_NORMAL

		!
		! Blank flags
		!
		VENDNUM$, APACCT$, CASHACCT$, TRANDAT$ = " "

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
	! Post the Header items
	!
	GOTO Interrupt IF &
		AP_TRAN_POSTAP(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD) <> CMC$_NORMAL


	%PAGE

	!
	! Remove files
	!
5000	CLOSE FILE_NAME.CH%


 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "TEXT_FILE", BATCH_NO$, "", "")

	PRNT_SUMMARY = SUBOPT_FINAL

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************

	!
	! Print undefined codes (if any)
	!
	TEXT = "Item   Tran#   Vendor      AP Account          " + &
		"Cash Account        InvDate   SubAccount"

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

 GetTrans:
18000	!*******************************************************************
	! This subroutine will assign an Transaction number from the control
	! file AP_CONTROL.  It will make sure that the number it is trying
	! to assign does not already exist.
	!*******************************************************************

18020	!
	! Read in the control record
	!
	WHEN ERROR IN
		GET #AP_CONTROL.CH%, RECORD 1%
	USE
		!
		! Invoice Record not found in ap open file
		!
		CONTINUE ExitProgram
	END WHEN

 !	AP_PJH::AP_ACCT = AP_CONTROL::AP_ACCT

18060	!
	! We have a key to try now
	!
	TEMP = VAL(AP_CONTROL::LAST_TRANKEY) + 1.0

	IF TEMP > 1000000.0
	THEN
		AP_CONTROL::LAST_TRANKEY = "000000"
	ELSE
		AP_CONTROL::LAST_TRANKEY = FORMAT$(TEMP, "<0>#####")
	END IF

	TRANKEY$ = AP_CONTROL::LAST_TRANKEY

	UPDATE #AP_CONTROL.CH%

	RETURN

	%PAGE

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************
	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "TEXT_FILE", &
			BATCH_NO$, "", "")
		GOTO ExitProgram
	END IF

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "TEXT_FILE", BATCH_NO$, "", "")

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
	! End of posting program AP_POST_FJTAB
	!******************************************************************
	END
