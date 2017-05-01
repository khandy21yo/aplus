1	%TITLE "General Ledger Posting Program"
	%SBTTL "GL_TRAN_POST"
	%IDENT "V3.6a Calico"

	SUB GL_TRAN_POST(PASS%, &
		POST_STATUS%, &
		BATCHNO$, &
		UTL_BATCH.CH%, &
		GL_YYYY_PP.CH%, &
		GL_CHART.CH%, &
		START_DATE$, &
		END_DATE$, &
		TITLE$(), &
		DESCR$)

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
	!
	! ABSTRACT:HELP
	!	.p
	!	This subroutine is used to display the initial
	!	totals for the GL POSTs, and to POST the GL_CHART
	!	file for current totals.  It also does the remove
	!	post if necessary.
	!
	! Index:
	!	POST
	!
	! Option:
	!
	!
	! Parameters:
	!
	!	PASS%
	!		Tells which pass the process is in.
	!		Pass 0
	!			Open GL_CHART, PERIOD, GL_YYYY_PP Files.
	!			Set test for dates.
	!		Pass 1
	!			If process has been interrupted then remove
	!			period file records with current batch number.
	!		Pass 2
	!			Generate the totals for transmittal and check
	!			for undefined account numbers.
	!		Pass 3
	!			Display current totals and allow user to abort
	!			the POST if they so desire or abort the post
	!			if there are undefined account numbers.
	!		Pass 4
	!			Add records to the period file.
	!		Pass 5
	!			Print transmittal on paper, POST GL_CHART
	!			unless previous try to POST this batch has
	!			already done it.
	!
	!	POST_STATUS%
	!		.table
	!		1% = Undefined code
	!
	!		2% = Journal out of balance
	!
	!		4% = Abort
	!
	!		8% = Check period being posted to GL - test date
	!			is out of range
	!		.endtable
	!
	!		NOTE::  The POST_STATUS% flag can be added to but
	!			should not in any other way be altered.
	!
	!	BATCHNO$
	!		Batch number is used to determine what record has
	!		been posted.
	!
	!	UTL_BATCH.CH%
	!		Temporary file used to record the file being
	!		added to and the RFA.
	!
	!	GL_YYYY_PP.CH%
	!		Channel opened for the GL period file.
	!
	!	GL_CHART.CH%
	!		Channel opened for the GL chart file.
	!
	!	START_DATE$
	!		Date at first of period to check date range
	!		on transactions.
	!
	!	END_DATE$
	!		Date at end of period to check date range
	!		on transactions.
	!
	!	TITLE$()
	!		Title for the transmittal.
	!
	!	DESCR$
	!		Description for the GL_TRAN_POST window.
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_TRAN_POST
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_TRAN_POST
	!	$ DELETE GL_TRAN_POST.OBJ;*
	!
	! AUTHOR:
	!
	!	09/23/87 - B. Craig Larsen
	!
	! MODIFICATION HISTORY:
	!
	!	07/28/88 - Kevin Handy
	!		Increased array size to 10,000 so Costal Group
	!		could update their journal.
	!
	!	11/30/88 - Kevin Handy
	!		Changed ENTR_ENTRY to read an integer instead
	!		of a string.
	!
	!	03/25/93 - Kevin Handy
	!		Modified undefined account message slightly (added
	!		TRM$) so more info would be displayed on screen.
	!
	!	03/30/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/27/93 - Kevin Handy
	!		Modify to lose confirm message on "normal" posts.
	!
	!	05/27/93 - Kevin Handy
	!		Modify to lose conform of post month.
	!
	!	08/11/93 - Kevin Handy
	!		Re-enable confirm message for Robinsons.
	!
	!	02/04/94 - Kevin Handy
	!		Fix problem with pushing F10 to get out causing post
	!		to continue.
	!
	!	03/24/94 - Kevin Handy
	!		Modified so that an undefined account with a
	!		negitive value will not cause program to crash.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/20/95 - Kevin Handy
	!		Remove bad external definitions.
	!		Fix 1st parameter to entr_4entry.
	!		Lose extre entr_4entry in loop.
	!
	!	07/02/96 - Kevin Handy
	!		Lose awful example code.
	!		Reformat source code.
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Send LOC(AST) to SMG$ENABLE_UNSOLICITED_INPUT
	!
	!	12/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	05/04/2001 - Kevin Handy
	!		Improve handling of user typing 'N' to abort
	!		posting. Especially don't change OPT$ to "Y"
	!		immediately after asking them to input it.
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE SMG_SCROLL_CDD SMG_SCROLL

	RECORD	UTL_BATCH
		STRING	FILE_NAME = 6%
		RFA	RFA_RFA
	END RECORD UTL_BATCH

	MAP (UTL_BATCH)	UTL_BATCH	UTL_BATCH

	!
	! External functions
	!
	EXTERNAL INTEGER	FUNCTION	DSPL_SCROLL

	!
	! Trial balance record
	!
	DECLARE RFA	TEST_RFA

	DECLARE INTEGER CONSTANT TRIAL_MAX = 10000

	%INCLUDE "SOURCE:[GL.OPEN]POST_TO_GL.HB"
	MAP (POST_TO) &
		GLCOUNT%, &
		POST_TO_GL_CDD	POST_GL_TOTALS(TRIAL_MAX)

	!
	! Dimension
	!
	DIM VIEW_A$(TRIAL_MAX)

	%PAGE

	ON ERROR GOTO 19000

	SELECT PASS%
	!
	! Pass 1 goes through the period file, removing all batches
	! that match the current batch number(Thus the user had
	! better not mess around with the batch number in the
	! batch control file).
	!
	CASE 1%
1100		!*****************************************************
		! This subroutine deletes all of the records in the
		! GL_YYYY_PP file that has the same batch number as
		! the current batch.
		!*****************************************************
 KillLoop:
		WHEN ERROR IN
			GET #UTL_BATCH.CH%, KEY #0% EQ "YYYYPP"

			DELETE #UTL_BATCH.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1120 IF ERR = 155%
			EXIT HANDLER
		END WHEN

1110		GET #GL_YYYY_PP.CH%, RFA UTL_BATCH::RFA_RFA

		DELETE #GL_YYYY_PP.CH%

		GOTO KillLoop

1120		WHEN ERROR IN
			GET #GL_YYYY_PP.CH%, KEY #4% EQ BATCHNO$
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1180
		END WHEN

1130		!
		! Create the data display
		!
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 130%, &
			SMG_VIEW%, SMG$M_BORDER)

		SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 132%)

		SMG_STATUS% = SMG$LABEL_BORDER(SMG_VIEW%, " General Ledger" + &
			" Post - ERROR ")

		!
		! Paste on the data display
		!
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_VIEW%, SCOPE::SMG_PBID, &
			2%, 2%)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
			"Account            Source Tran Key Tran Date  " + &
			"Description               " + &
			"Post Date  " + &
			"        Amount", 9%, 1%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
			"Remove record from the transaction file (GL_YYYY_PP) ? ", &
			18%, 1%, , SMG$M_BOLD)

1140		SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
			GL_YYYY_PP::ACCT + " " + &
			GL_YYYY_PP::SOURCE + "   " + &
			GL_YYYY_PP::TRANKEY + "   " + &
			PRNT_DATE(GL_YYYY_PP::TRANDAT, 8%) + " " + &
			LEFT(GL_YYYY_PP::DESCR, 24%) + "  " + &
			PRNT_DATE(GL_YYYY_PP::POSDAT, 8%) + " " + &
			FORMAT$(GL_YYYY_PP::AMOUNT, "###,###,###.##"), &
			10%, 1%)

1150		TEMP_IDENT$ = SCOPE::PRG_IDENT
		TEMP_ITEM$ = SCOPE::PRG_ITEM

		SCOPE::PRG_IDENT = "POST"
		SCOPE::PRG_ITEM = "DELETE_ENTRY"

		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		INP$ = EDIT$(ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
			"Confirm Deletion of Entry - then press <Do> ", &
			"N", 0%, "", ""), -1%)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

		SCOPE::PRG_IDENT = TEMP_IDENT$
		SCOPE::PRG_ITEM = TEMP_ITEM$

		SELECT SCOPE::SCOPE_EXIT
		!
		! Exit
		!
		CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			POST_STATUS% = (POST_STATUS% OR 4%)
			SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY( &
				SMG_VIEW%, SCOPE::SMG_PBID)
			GOTO ExitSubroutine

		!
		! Normal key typed
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_F7

		!
		! Bad key typed
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 1150

		END SELECT

1160		IF INP$ = "Y"
		THEN
			DELETE #GL_YYYY_PP.CH%
		END IF

1170		WHEN ERROR IN
			GET #GL_YYYY_PP.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1180
		END WHEN

		GOTO 1140

1180		IF SMG_VIEW%
		THEN
			SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_VIEW%, &
				SCOPE::SMG_PBID)
		END IF

		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	!
	! Pass 2 will generate the transmittal and check for
	! undefined account numbers
	!
	CASE 2%
		AMOUNT = GL_YYYY_PP::AMOUNT

		POST_STATUS% = (POST_STATUS% OR 8%) &
			IF RIGHT(GL_YYYY_PP::TRANDAT, 5%) < START_DATE$ OR &
			RIGHT(GL_YYYY_PP::TRANDAT, 5%) > END_DATE$ &
			IF START_DATE$ <> ""

		!
		! Search trial balance list for currently existing account
		!
		GOTO 1225 IF (POST_GL_TOTALS(I%)::ACCT = GL_YYYY_PP::ACCT) &
			FOR I% = 1% TO GLCOUNT%

		!
		! Item not found, create it
		!
		I%, GLCOUNT% = GLCOUNT% + 1%

		WHILE (I% > 1%) AND &
			(POST_GL_TOTALS(I% - 1%)::ACCT > GL_YYYY_PP::ACCT)
			POST_GL_TOTALS(I%) = POST_GL_TOTALS(I% - 1%)
			I% = I% - 1%
		NEXT

		POST_GL_TOTALS(I%)::ACCT	= GL_YYYY_PP::ACCT
		POST_GL_TOTALS(I%)::DESCR	= STRING$(LEN(GL_CHART::DESCR), 63%)
		POST_GL_TOTALS(I%)::ACCTYPE	= "?"
		POST_GL_TOTALS(I%)::BEGBAL	= 0.0
		POST_GL_TOTALS(I%)::CREDIT	= 0.0
		POST_GL_TOTALS(I%)::DEBIT	= 0.0
		POST_GL_TOTALS(I%)::UNITS	= 0.0
		POST_GL_TOTALS(I%)::HOURS	= 0.0

1220		!
		! Set the undefined flag to yes.  If the account
		! is defined then it will be set to no.
		!
		POST_GL_TOTALS(I%)::UNDEFINED_ACCT = "Y"

		WHEN ERROR IN
			GET #GL_CHART.CH%, KEY #0% EQ GL_YYYY_PP::ACCT
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			IF ERR = 155%
			THEN
				POST_STATUS% = (POST_STATUS% OR 1%)
				CONTINUE 1225
			END IF
			EXIT HANDLER
		END WHEN

		POST_GL_TOTALS(I%)::DESCR	= GL_CHART::DESCR
		POST_GL_TOTALS(I%)::ACCTYPE	= GL_CHART::ACCTYPE
		POST_GL_TOTALS(I%)::BEGBAL	= GL_CHART::RUNDOL &
			IF INSTR(1%, "RE", GL_CHART::ACCTYPE) = 0%

		POST_GL_TOTALS(I%)::UNDEFINED_ACCT = "N"

1225		!
		! Skip undefined is account is really defined
		!
		IF POST_GL_TOTALS(I%)::UNDEFINED_ACCT = "Y"
		THEN
			SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

			TEMP_IDENT$	= SCOPE::PRG_IDENT
			TEMP_ITEM$	= SCOPE::PRG_ITEM

			SCOPE::PRG_IDENT = "POST"
			SCOPE::PRG_ITEM = "UNDEFINED_ACCOUNT"

			SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_MESSAGE, &
				"Undef. Acct " + &
				TRM$(GL_YYYY_PP::ACCT) + " Ref " + &
				TRM$(GL_YYYY_PP::REFNO) + " Xref " + &
				TRM$(GL_YYYY_PP::XREFNO) + " Tran " + &
				TRM$(GL_YYYY_PP::TRANKEY) + " Amt " + &
				NUM1$(GL_YYYY_PP::AMOUNT), &
				1%, 1%, 0%, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_MESSAGE, &
				FORMAT$(GL_YYYY_PP::AMOUNT, "###,###,###.##  ") + &
				GL_YYYY_PP::DESCR + &
				"  Press <RETURN> to continue, <EXIT> to ABORT!!!", &
				2%, 1%, 0%, SMG$M_BOLD)

			SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

			!
			! Get input
			!
			JUNK% = 0%
			WHILE JUNK% = 0%
				JUNK% = ENTR_4ENTRY(SCOPE, &
					SCOPE::SMG_MESSAGE BY VALUE, &
					0% BY VALUE)
				JUNK% = ENTR_4SPECIALKEYS(SCOPE, &
					SCOPE::SMG_MESSAGE BY VALUE, &
					0% BY VALUE, JUNK% BY VALUE)
			NEXT

			SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
				LOC(OUTP_XUNSOL) BY VALUE, &
				LOC(SCOPE::SMG_KBID) BY VALUE)

			SCOPE::PRG_IDENT = TEMP_IDENT$
			SCOPE::PRG_ITEM = TEMP_ITEM$

			SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

			SELECT JUNK%
			!
			! Exit
			!
			CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

				POST_STATUS% = (POST_STATUS% OR 4%)

			!
			! Normal key typed
			!
			CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_F7

			!
			! Bad key typed
			!
			CASE ELSE
				SCOPE::SCOPE_EXIT = JUNK%
				CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
				GOTO 1225

			END SELECT

			CALL ENTR_3MESSAGE(SCOPE, &
				"Creating posting transmittals", 1%)
		END IF

1230		!
		! Add credit/debit amounts
		!
		IF AMOUNT > 0.0
		THEN
			POST_GL_TOTALS(I%)::DEBIT = &
				POST_GL_TOTALS(I%)::DEBIT + AMOUNT
		ELSE
			POST_GL_TOTALS(I%)::CREDIT = &
				POST_GL_TOTALS(I%)::CREDIT + AMOUNT
		END IF

		POST_GL_TOTALS(I%)::UNITS = &
			POST_GL_TOTALS(I%)::UNITS + GL_YYYY_PP::UNITS

		POST_GL_TOTALS(I%)::HOURS = &
			POST_GL_TOTALS(I%)::HOURS + GL_YYYY_PP::HOURS

	!
	! Pass 3 will display the transmittal, notify the user
	! if there are any undefined account numbers, notify
	! the user if data is out of range, and ask to confirm
	! the post
	!
	CASE 3%
		!*****************************************************
		! Calculate grand total, and cause error if it does
		! not balance
		!*****************************************************

		!
		! Calculate total, Create display record
		!
		TOTAL, DEBIT_TOTAL, CREDIT_TOTAL = 0.0

		FOR LOOP% = 1% TO GLCOUNT%
			TOTAL = FUNC_ROUND(TOTAL + POST_GL_TOTALS(LOOP%)::CREDIT + &
				POST_GL_TOTALS(LOOP%)::DEBIT, 2%)
			CREDIT_TOTAL = FUNC_ROUND(CREDIT_TOTAL + POST_GL_TOTALS(LOOP%)::CREDIT, 2%)
			DEBIT_TOTAL = FUNC_ROUND(DEBIT_TOTAL + POST_GL_TOTALS(LOOP%)::DEBIT, 2%)

			VIEW_A$(LOOP%) = POST_GL_TOTALS(LOOP%)::ACCT + " " + &
				LEFT(POST_GL_TOTALS(LOOP%)::DESCR, 30%) + " " + &
				FORMAT$(POST_GL_TOTALS(LOOP%)::DEBIT, "<%>##,###,###.## ") + &
				FORMAT$(-POST_GL_TOTALS(LOOP%)::CREDIT, "<%>##,###,###.## ")

			IF INSTR(1%, "RE", &
				EDIT$(POST_GL_TOTALS(LOOP%)::ACCTYPE, -1%)) = 0%
			THEN
				VIEW_A$(LOOP%) = VIEW_A$(LOOP%) + &
					FORMAT$(POST_GL_TOTALS(LOOP%)::BEGBAL + &
					POST_GL_TOTALS(LOOP%)::DEBIT + &
					POST_GL_TOTALS(LOOP%)::CREDIT, "<%>##,###,###.## ")
			ELSE
				VIEW_A$(LOOP%) = VIEW_A$(LOOP%) + &
					FORMAT$(0.0, "<%>##,###,###.## ")
			END IF

			VIEW_A$(LOOP%) = VIEW_A$(LOOP%) + &
				FORMAT$(POST_GL_TOTALS(LOOP%)::UNITS, "###,###.## ") + &
				FORMAT$(POST_GL_TOTALS(LOOP%)::HOURS, "###,###.##")
		NEXT LOOP%

		POST_STATUS% = POST_STATUS% OR 2% IF (TOTAL <> 0.0)

		!
		! Create the data display
		!
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, &
			130%, SMG_VIEW%, SMG$M_BORDER)

		SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 132%)

		SMG_STATUS% = SMG$LABEL_BORDER(SMG_VIEW%, DESCR$)

		!
		! Paste on the data display
		!
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_VIEW%, &
			SCOPE::SMG_PBID, 2%, 2%)

		!
		! Create window definition
		!
		SMG_SCROLL::WINDOW	= SMG_VIEW%
		SMG_SCROLL::SCROLL_TOP	= 2%
		SMG_SCROLL::SCROLL_BOT	= 17%
		SMG_SCROLL::TOP_ARRAY	= 1%
		SMG_SCROLL::BOT_ARRAY	= GLCOUNT%
		SMG_SCROLL::CUR_LINE	= 1%
		SMG_SCROLL::CUR_W_COL	= 1%
		SMG_SCROLL::FIND_LINE	= 1%
		SMG_SCROLL::TOP_LINE	= 1%
		SMG_SCROLL::CUR_W_ROW	= 1%
		SMG_SCROLL::BEG_ELEMENT	= 1%
		SMG_SCROLL::END_ELEMENT	= GLCOUNT%
		SMG_SCROLL::SMG_FLAG	= 0%
		SMG_SCROLL::PROMPT	= ""
		SMG_SCROLL::NUM_COLM	= 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
			"Account            Description" + SPACE$(29%) + &
			"Debit         Credit        Balance      Units      Hours", &
			1%, 1%, , SMG$M_BOLD)

		TEMP$ = "Does not balance! " IF POST_STATUS% AND 2%
		TEMP$ = TEMP$ + "Undefined code!" IF POST_STATUS% AND 1%

		TEMP$ = TEMP$ + SPACE$(50% - LEN(TEMP$)) + &
			FORMAT$(DEBIT_TOTAL, "###,###,###.## ") + &
			FORMAT$(-CREDIT_TOTAL, "###,###,###.## ")

		SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, TEMP$, 18%, &
			1%, , SMG$M_BOLD)

		!
		! Display first page
		!
		SMG_STATUS% = DSPL_SCROLL(SMG_SCROLL, VIEW_A$(), 0%, "PAINT")

 TestPostStatus:
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

 TestPostStatusA:
		!
		! Warning that date test is out of range
		!
		IF (POST_STATUS% AND 8%)
		THEN
			TEMP_IDENT$ = SCOPE::PRG_IDENT
			TEMP_ITEM$ = SCOPE::PRG_ITEM

			SCOPE::PRG_IDENT = "ERR"
			SCOPE::PRG_ITEM = "DATE_OUT_OF_RANGE"

			OPT$ = EDIT$(ENTR_3YESNO(SCOPE,  &
				SCOPE::SMG_OPTION, "", &
				"Is this journal being posted to the " + &
				"correct accounting period?  Confirm " + &
				"then press <Do> ", "N", 0%, "", ""), -1%)

			SCOPE::PRG_IDENT = TEMP_IDENT$
			SCOPE::PRG_ITEM = TEMP_ITEM$

			SELECT SCOPE::SCOPE_EXIT
			!
			! Exit
			!
			CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

				POST_STATUS% = (POST_STATUS% OR 4%)
				SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY( &
					SMG_VIEW%, SCOPE::SMG_PBID)
				OPT$ = "N"

			!
			! Normal key typed
			!
			CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_F7

			!
			! Bad key typed
			!
			CASE ELSE
				CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
				GOTO TestPostStatusA

			END SELECT

			IF OPT$ = "Y"
			THEN
				POST_STATUS% = POST_STATUS% AND NOT 8%
				OPT$ = ""
				SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
				GOTO TestPostStatus
			ELSE
				POST_STATUS% = POST_STATUS% OR 4%
			END IF

		!
		! May be out of balance or undefined account number
		!
		ELSE
			IF POST_STATUS%
			THEN
				TEMP_IDENT$ = SCOPE::PRG_IDENT
				TEMP_ITEM$ = SCOPE::PRG_ITEM

				SCOPE::PRG_IDENT = "ERR"
				SCOPE::PRG_ITEM = "UNKNOWN"
				SCOPE::PRG_ITEM = "INVALID_CODE" IF POST_STATUS% AND 1%
				SCOPE::PRG_ITEM = "OUT_OF_BALANCE" IF POST_STATUS% AND 2%

				CALL ENTR_3MESSAGE(SCOPE, "POST aborted! Error " + SCOPE::PRG_ITEM, 0%)

				SCOPE::PRG_IDENT = TEMP_IDENT$
				SCOPE::PRG_ITEM = TEMP_ITEM$

				OPT$ = "ABORT"
			ELSE
				TEMP_IDENT$ = SCOPE::PRG_IDENT
				TEMP_ITEM$ = SCOPE::PRG_ITEM

				SCOPE::PRG_IDENT = "POST"
				SCOPE::PRG_ITEM = "CONFIRM"

				OPT$ = EDIT$(ENTR_3YESNO(SCOPE, &
					SCOPE::SMG_OPTION, "", &
					"Confirm posting - then press <Do> ", &
					"N", 0%, "", ""), -1%)

				SCOPE::PRG_IDENT = TEMP_IDENT$
				SCOPE::PRG_ITEM = TEMP_ITEM$

				IF OPT$ <> "Y"
				THEN
					POST_STATUS% = POST_STATUS% OR 4%
				END IF

			END IF
		END IF

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

		SELECT SCOPE::SCOPE_EXIT

		!
		! Control/C
		!
		CASE 3%
			GOTO TestPostStatus

		!
		! Scroll screen
		!
		CASE SMG$K_TRM_UP, &
			SMG$K_TRM_DOWN, &
			SMG$K_TRM_PREV_SCREEN, &
			SMG$K_TRM_NEXT_SCREEN, &
			SMG$K_TRM_F18, &
			SMG$K_TRM_F19

			SMG_STATUS% = DSPL_SCROLL(SMG_SCROLL, VIEW_A$(), &
				SCOPE::SCOPE_EXIT, "PAINT")
			GOTO TestPostStatus

		!
		! Exit
		!
		CASE SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
				SPACE$(132%), 1%, 1%)
			SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_VIEW%)
			POST_STATUS% = (POST_STATUS% OR 4%)
			GOTO ExitSubroutine

		!
		! Normal key typed
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_F7

		!
		! Bad key typed
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO TestPostStatus

		END SELECT

		!
		! Handle user options
		!
		SELECT OPT$

		!
		! Abort
		!
		CASE "N", "ABORT"
			SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
				SPACE$(132%), 1%, 1%)
			SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_VIEW%)
			SCOPE::SCOPE_EXIT = SMG$K_TRM_F10
			POST_STATUS% = (POST_STATUS% OR 4%)
			GOTO ExitSubroutine

		!
		! Go
		!
		CASE "Y"
			SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
				SPACE$(132%), 1%, 1%)
			SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_VIEW%)
			GOTO ExitSubroutine
		END SELECT

		GOTO TestPostStatus

	!
	! Pass 4 will add the record to the general ledger folder
	!
	CASE 4%
1400		!
		! Put record into GL_YYYY_PP file
		!
		PUT #GL_YYYY_PP.CH%

1450		!
		! Put record into UTL_BATCH file in case of crash
		!
		UTL_BATCH::FILE_NAME	= "YYYYPP"
		UTL_BATCH::RFA_RFA	= GETRFA(GL_YYYY_PP.CH%)

		PUT #UTL_BATCH.CH%

	!
	! Pass 5 goes through the GL_TRAN_POST structure, and
	! updates GL_CHART accounts.  Pass 5 then prints a
	! transmittal(summary).
	!
	CASE 5%
		!*****************************************************
		! Pass 5, POST chart, create report
		!*****************************************************

		!
		! Append title to bottom of TITLE$()
		!
		I% = 1%
		I% = I% + 1% UNTIL TITLE$(I%) = ""

		TITLE$(I% + 1%) = "Account            Description" + SPACE$(29%) + &
			"Debit         Credit        Balance      Units      Hours"

		TITLE$(I% + 2%) = ""

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TOTAL, CREDIT_TOTAL, DEBIT_TOTAL = 0.0

1500		WHEN ERROR IN
			GET #UTL_BATCH.CH%, KEY #0% EQ "CHART", REGARDLESS
		USE
			CONTINUE 1505
		END WHEN

		INTERRUPTED% = -1%

1505		!
		! Print out list
		!
		FOR LOOP% = 1% TO GLCOUNT%

1510			GET #GL_CHART.CH%, &
				KEY #0% EQ POST_GL_TOTALS(LOOP%)::ACCT

			!
			! Post the chart of accounts, unless this is a restarted
			! post and the account has already been posted, as
			! shown by the fact that the batch number is the same
			! as the current batch number being POSTed.
			!

			!
			! Skip if normal posting
			!

			GOTO 1530 IF INTERRUPTED% = 0%

			TEST_RFA = GETRFA(GL_CHART.CH%)

1520			WHEN ERROR IN
				GET #UTL_BATCH.CH%, &
					KEY #0% EQ "CHART", &
					REGARDLESS
			USE
				CONTINUE 1530 IF ERR = 155% OR ERR = 11%
				EXIT HANDLER
			END WHEN

			WHILE UTL_BATCH::FILE_NAME  = "CHART"

				GOTO 1550 IF UTL_BATCH::RFA_RFA = TEST_RFA

				WHEN ERROR IN
					GET #UTL_BATCH.CH%, REGARDLESS
				USE
					CONTINUE 1530 IF ERR = 155% OR ERR = 11%
					EXIT HANDLER
				END WHEN
			NEXT

1530			!
			! POST the running totals
			!
			GL_CHART::RUNDOL = GL_CHART::RUNDOL + &
				POST_GL_TOTALS(LOOP%)::CREDIT + &
				POST_GL_TOTALS(LOOP%)::DEBIT
			GL_CHART::RUNUNIT = GL_CHART::RUNUNIT + &
				POST_GL_TOTALS(LOOP%)::UNITS
			GL_CHART::RUNHOUR = GL_CHART::RUNHOUR + &
				POST_GL_TOTALS(LOOP%)::HOURS
			GL_CHART::BATCH = BATCHNO$

			!
			! Update the GL_CHART file
			!
			UPDATE #GL_CHART.CH%

1540			!
			! Put record into UTL_BATCH file in case of crash
			!
			UTL_BATCH::FILE_NAME	= "CHART"
			UTL_BATCH::RFA_RFA	= GETRFA(GL_CHART.CH%)
			PUT #UTL_BATCH.CH%

1550			!
			! Output a line to the report
			!
			TEXT$ = POST_GL_TOTALS(LOOP%)::ACCT + " " + &
				LEFT(POST_GL_TOTALS(LOOP%)::DESCR, 30%) + " " + &
				FORMAT$(POST_GL_TOTALS(LOOP%)::DEBIT, "<%>##,###,###.## ") + &
				FORMAT$(-POST_GL_TOTALS(LOOP%)::CREDIT, "<%>##,###,###.## ")

			IF INSTR(1%, "RE", EDIT$( &
				POST_GL_TOTALS(LOOP%)::ACCTYPE, -1%)) = 0%
			THEN
				TEXT$ = TEXT$ + &
					FORMAT$(POST_GL_TOTALS(LOOP%)::BEGBAL + &
					POST_GL_TOTALS(LOOP%)::DEBIT + &
					POST_GL_TOTALS(LOOP%)::CREDIT, &
					"<%>##,###,###.## ")
			ELSE
				TEXT$ = TEXT$ + FORMAT$(0.0, "<%>##,###,###.## ")
			END IF

			TEXT$ = TEXT$ + &
				FORMAT$(POST_GL_TOTALS(LOOP%)::UNITS, "###,###.## ") + &
				FORMAT$(POST_GL_TOTALS(LOOP%)::HOURS, "###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			!
			! Calculate totals
			!
			CREDIT_TOTAL = FUNC_ROUND(CREDIT_TOTAL + &
				POST_GL_TOTALS(LOOP%)::CREDIT, 2%)
			DEBIT_TOTAL = FUNC_ROUND(DEBIT_TOTAL + &
				POST_GL_TOTALS(LOOP%)::DEBIT, 2%)

		NEXT LOOP%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		TEMP$ = "TOTAL"
		TEXT$ = TEMP$ + SPACE$(50% - LEN(TEMP$)) + &
			FORMAT$(DEBIT_TOTAL, "###,###,###.## ") + &
			FORMAT$(-CREDIT_TOTAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GLCOUNT% = 0%

	END SELECT

 ExitSubroutine:

	EXIT SUB

	%PAGE

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	ON ERROR GO BACK

32767	END SUB
	!+-+-+
	!++
	! Abstract:HELP
	!	.b
	!	^*POST\*
	!	.b
	!	.lm +5
	!	This subroutine is used to display the initial
	!	totals for the GL POST, and to POST the GL_CHART
	!	file for current totals. It also does the remove
	!	post if necessary.
	!	.lm -5
	!
	! Index:
	!
	!--
