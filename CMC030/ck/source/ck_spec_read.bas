1	%TITLE "Read Checks from GL"
	%SBTTL "CK_SPEC_READ"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	! ID:CKREAD
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Read Current Data from GL\* option reads all
	!	cash transactions in the General Ledger including checks, deposits, and
	!	any adjustments for each cash account defined in the Check Reconciliation
	!	Control File. It will then write those transactions into the Check
	!	Reconciliation file.
	!	.b
	!	If cash transactions are added to the General Ledger after this option has
	!	been executed, the Read option can be executed again in order to include the
	!	additional transactions in the Check Reconciliation files.
	!	.b
	!	When this option is accessed, a message will be displayed indicating that
	!	data will be read from a specified accounting period. The user has the option
	!	of confirming the reading of the indicated period. When all records have been
	!	read from the subject period, and if there is data to be read in the subsequent
	!	accounting period, a message will be displayed indicating that the system is
	!	ready to read the next period. Reading will continue for each subsequent period
	!	for which there is any data unless the user responds with a "No" to the
	!	confirmation question.
	!	.b
	!	When the cash files have been read and copied into the Check Reconciliation
	!	file, a brief report will print which includes the following information for
	!	each cash account defined in the Check Reconciliation utility file:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Bank Code
	!	.le
	!	Deposit
	!	.le
	!	Check
	!	.le
	!	Adjustment
	!	.le
	!	Total
	!	.els
	!	.lm -15
	!
	! Index:
	!	.x Copy>Cash Transactions from General Ledger
	!	.x Copy>Cash Transactions to Check Reconciliation System
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_SPEC_READ
	!	$ LINK/EXEC=CK_EXE:*.EXE CK_SPEC_READ, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE CK_SPEC_READ.OBJ;*
	!
	! Author:
	!
	!	04/19/88 - Kevin Handy
	!
	! Modification history:
	!
	!	05/24/91 - Kevin Handy
	!		Modified to close GL_PERIOD file as soon as possible.
	!
	!	11/07/91 - Kevin Handy
	!		Modified to handle PJ like CD.
	!
	!	03/12/92 - Kevin Handy
	!		Unrolled error trapping.
	!
	!	09/02/92 - Kevin Handy
	!		Modifications to make all records from one post
	!		contain the same source code so that the audit
	!		by source code will balance.
	!
	!	08/18/93 - Kevin Handy
	!		Added "SJ" to list of things that are deposits.
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Lose extra parameter to ASSG_FREECHANNEL
	!		Change SMG_BLANK to SMG_BLANK%
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code.
	!
	!	04/30/97 - Kevin Handy
	!		Use OUTP_INITFORM function.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/12/98 - Kevin Handy
	!		Change "SCOPE::PRG_PROGRAM" to "FILENAME$"
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/14/99 - Kevin Handy
	!		Information coming from PS will be a check, not
	!		an adjustment.
	!		Use WHEN ERROR IN
	!		Add in a couple of REGARDLESS clauses
	!
	!	05/13/99 - Kevin Handy
	!		Added OIJ as a deposit type
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Includes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Maps
	!
	%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROL.HB"
	MAP (CK_CONTROL)	CK_CONTROL_CDD		CK_CONTROL

	%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROLACC.HB"
	MAP (CK_CONTROLACC)	CK_CONTROLACC_CDD	CK_CONTROLACC

	%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.HB"
	MAP (CK_CKMNT)		CK_CKMNT_CDD		CK_CKMNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	!
	! Functions
	!
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	!
	! Dimension statements
	!
	RECORD GTOTAL_CDD
		STRING	BANK_ACCT = 6%
		REAL	DEPOSIT
		REAL	CHECK
		REAL	ADJUST
	END RECORD

	DIM GTOTAL_CDD BANK_TOTAL(200%)

	%PAGE

	!*******************************************************************
	! Initilization
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	REPORT$ = "CKREAD"

305	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"

		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE #GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)

310	!
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROL.OPN"

		GET #CK_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "CK_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF CK_CONTROL::FLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "CK Purge in process", &
			"ERR", "CK_PURGE", "ERROR_PURGE")
		GOTO ExitProgram
	END IF

	CUR_PERIOD% = CK_CONTROL::PERIOD + 1%
	YEAR$ = CK_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")
	YYYYPP$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

320	!
	! Open control account file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROLACC.OPN"
	USE
		FILENAME$ = "CK_CONTROLACC"
		CONTINUE HelpError
	END WHEN

330	!
	! Open reconciliation file (Create it if necessary)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.CRE"
	USE
		FILENAME$ = "CK_CKMNT"
		CONTINUE HelpError
	END WHEN

	%PAGE

700	!******************************************************************
	! Ask for the Period to be updated.
	!******************************************************************
	!
	! Paint the background
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_SCREEN_DATA%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_SCREEN_DATA%, &
		"Read General Ledger Data " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Reading Data from Period " + YYYY_PP$ + &
		" " + GL_PERIOD::PERIOD(CUR_PERIOD%), 4%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

	SCOPE::PRG_ITEM = "CONFIRM"

	!++
	! Abstract:CONFIRM
	!	^*Confirm Reading Period\*
	!	.b
	!	.lm +5
	!	The ^*Confirm Reading Period\* message
	!	confirms the reading of the General Ledger file
	!	related to the period indicated and the copying of the cash transactions in
	!	that file into the Check Reconciliation file.  The execution of this option
	!	will automatically begin with the first period in which there is data in any
	!	defined cash account which has not been previously read and copied into the
	!	Check Reconciliation file.
	!	.lm -5
	!
	! Index:
	!	.x Copy>Cash Transactions
	!	.x Read>Cash Transactions
	!
	!--

	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm Reading period " + &
		YYYY_PP$ + " - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, SPACE$(78%), 4%, 5%)

	%PAGE

2000	!
	! Open the period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.MOD"
	USE
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

5000	!*******************************************************************
	! Purge out any information which already exists for this
	! period in the check reconciliation file.
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Clearing previous versions", 1%)

	WHEN ERROR IN
		RESET #CK_CKMNT.CH%
	USE
		CONTINUE 6000
	END WHEN

5010	!
	! Pull up one record
	!
	WHEN ERROR IN
		GET #CK_CKMNT.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF
		CONTINUE 6000 IF ERR = 11%
		FILENAME$ = "CK_CKMNT"
		CONTINUE HelpError
	END WHEN

	!
	! Skip if if not right type, or right period
	!
	GOTO 5010 IF (CK_CKMNT::STYPE <> "G") OR &
		(CK_CKMNT::GLDATE <> YYYYPP$)

5020	!
	! Remove this record
	!
	DELETE #CK_CKMNT.CH%

	GOTO 5010

	%PAGE

6000	!*******************************************************************
	! Scan through GL_YYYY_PP file, and add info to CK file.
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Reading Bank Accounts", 1%)

	!
	! No banks have been totaled yet
	!
	BANK_TOTAL% = 0%

	!
	! Try pulling up the CONTROLACC record for this account number
	!
	WHEN ERROR IN
		RESET #CK_CONTROLACC.CH%, KEY #0
	USE
		CONTINUE ExitProgram
	END WHEN

6020	!
	! Get the control account record
	!
	WHEN ERROR IN
		GET #CK_CONTROLACC.CH%, REGARDLESS
	USE
		CONTINUE 7000 IF ERR = 11%
		FILENAME$ = "CK_CONTROLACC"
		CONTINUE HelpError
	END WHEN

6030	!
	! Find start of cash account
	!
	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, KEY #0% EQ CK_CONTROLACC::ACCOUNT
	USE
		CONTINUE 6020 IF ERR = 155%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

6040	!
	! Pull up one record
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF
		CONTINUE 6020 IF ERR = 11%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

	!
	! Get next control record if gl account number changes
	!
	GOTO 6020 IF GL_YYYY_PP::ACCT <> CK_CONTROLACC::ACCOUNT

	!
	! Handle the check number first, since we need it throughout
	! the rest of the code.
	!
	RSET CK_CKMNT::CKNUM = EDIT$(GL_YYYY_PP::CKNO, 8% + 128%)

	!
	! If the check/deposit number id null then ask for the
	! check/deposit number otherwise skip this section.
	!
	GOTO 6090 IF EDIT$(CK_CKMNT::CKNUM, -1%) <> ""

	!
	! If this is the first time a null check/deposit number has
	! been encountered then paint the screen.
	!
	IF FIRST_NULL_CKNUM% = 0%
	THEN
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Account #", 1%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Source", 2%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Reference", 3%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Tran Date", 4%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Description", 5%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Amount", 6%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Xref", 7%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Post Time", 8%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Post Date", 9%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Chk/Dep", 10%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Voucher #", 11%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Sub account", 12%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Operation", 13%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Units", 14%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Hours", 15%, 2%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Batch #", 16%, 2%)

		FIRST_NULL_CKNUM% = -1%

	END IF

	!
	! Print record that needs thee check/deposit number.
	!
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_YYYY_PP::ACCT, 1%, 21%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::SOURCE, 2%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::REFNO, 3%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PRNT_DATE(GL_YYYY_PP::TRANDAT, 8%), 4%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::DESCR, 5%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(GL_YYYY_PP::AMOUNT, "###,###,###.##"), 6%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::XREFNO, 7%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PRNT_TIME(GL_YYYY_PP::POSTIM, 4%), 8%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PRNT_DATE(GL_YYYY_PP::POSDAT, 8%), 9%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::CKNO, 10%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::TRANKEY, 11%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::SUBACC, 12%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::OPERATION, 13%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(GL_YYYY_PP::UNITS, "###,###,###.##"), 14%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(GL_YYYY_PP::HOURS, "###,###,###.##"), 15%, 21%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		GL_YYYY_PP::BTHNUM, 16%, 21%)

6050	!
	! Ask for the new check/deposit number
	!
	SCOPE::PRG_ITEM = "FLD01CKNO"

	!++
	! Abstract:FLD01CKNO
	!	^*(01) GL Period\*
	!	.b
	!	.lm +5
	!	The ^*General Ledger Period\* enters a General Ledger
	!	Period which identifies the accounting period from which all cash transactions
	!	will be read. The transactions will then be written into the Check
	!	Reconciliation file.
	!	.lm -5
	!
	! Index:
	!	.x GL Period
	!
	!--

	GL_YYYY_PP::CKNO = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"10;21", "New Check Number", GL_YYYY_PP::CKNO, 0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control c
	!
	CASE 3%
		GOTO 6050

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 6050

	END SELECT

	!
	! If the check/deposit number is still null then
	! ask for the number again.
	!
	IF EDIT$(GL_YYYY_PP::CKNO, -1%) = ""
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Check/Deposit number must have a value", 0%)
		GOTO 6050
	END IF

6060	UPDATE #GL_YYYY_PP.CH%

	RSET CK_CKMNT::CKNUM = EDIT$(GL_YYYY_PP::CKNO, 8% + 128%)

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	CALL ENTR_3MESSAGE(SCOPE, "Reading Bank Accounts", 1%)

6090	!
	! Figure out what flag to put on this record in the
	! ck_ckmnt file.
	!
	SELECT GL_YYYY_PP::SOURCE

	CASE "CD  ", "PJ  ", "PR  "
		THIS_FLAG$ = "C"

	CASE "CR  ", "CRJ ", "SJ  ", "PS  ", "OIJ "
		THIS_FLAG$ = "D"

	CASE ELSE
		IF (CK_CONTROLACC::STARTCK <= CK_CKMNT::CKNUM) AND &
			(CK_CONTROLACC::ENDCK >= CK_CKMNT::CKNUM)
		THEN
			THIS_FLAG$ = "C"
		ELSE
			THIS_FLAG$ = "A"
		END IF
	END SELECT

	!
	! Create grand totals for final output
	!
	TOTAL_CHECK = TOTAL_CHECK + GL_YYYY_PP::AMOUNT

	!
	! Summarize for grand total
	!
	FOR I% = 1% TO BANK_TOTAL%

		GOTO 6100 &
			IF BANK_TOTAL(I%)::BANK_ACCT = CK_CONTROLACC::BANK_ACCT

		IF BANK_TOTAL(I%)::BANK_ACCT > CK_CONTROLACC::BANK_ACCT
		THEN
			FOR J% = BANK_TOTAL% TO I% STEP -1%
				BANK_TOTAL(J% + 1%) = BANK_TOTAL(J%)
			NEXT J%
			BANK_TOTAL% = BANK_TOTAL% + 1%
			BANK_TOTAL(I%)::BANK_ACCT = &
				CK_CONTROLACC::BANK_ACCT + ""
			BANK_TOTAL(I%)::DEPOSIT = 0.0
			BANK_TOTAL(I%)::CHECK = 0.0
			BANK_TOTAL(I%)::ADJUST = 0.0
			GOTO 6100
		END IF
	NEXT I%

	I%, BANK_TOTAL% = BANK_TOTAL% + 1%
	BANK_TOTAL(I%)::BANK_ACCT = CK_CONTROLACC::BANK_ACCT + ""
	BANK_TOTAL(I%)::DEPOSIT = 0.0
	BANK_TOTAL(I%)::CHECK = 0.0
	BANK_TOTAL(I%)::ADJUST = 0.0

6100	SELECT THIS_FLAG$

	CASE "D"
		BANK_TOTAL(I%)::DEPOSIT = &
			BANK_TOTAL(I%)::DEPOSIT - GL_YYYY_PP::AMOUNT

	CASE "C"
		BANK_TOTAL(I%)::CHECK = &
			BANK_TOTAL(I%)::CHECK - GL_YYYY_PP::AMOUNT

	CASE ELSE
		BANK_TOTAL(I%)::ADJUST = &
			BANK_TOTAL(I%)::ADJUST - GL_YYYY_PP::AMOUNT

	END SELECT

6200	!
	! Create record for this one.
	!
	CK_CKMNT::BANK_ACCT = CK_CONTROLACC::BANK_ACCT
	CK_CKMNT::ETYPE = THIS_FLAG$
	CK_CKMNT::STYPE = "G"
	CK_CKMNT::CKDAT = GL_YYYY_PP::TRANDAT
	CK_CKMNT::CKAMT = -GL_YYYY_PP::AMOUNT
	CK_CKMNT::GLDATE = YYYYPP$

	WHEN ERROR IN
		PUT #CK_CKMNT.CH%
	USE
		FILENAME$ = "CK_CKMNT"
		CONTINUE HelpError
	END WHEN

	GOTO 6040

	%PAGE

7000	!******************************************************************
	! Set up the report settings screen
	!******************************************************************

	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	!*******************************************************************
	! Print summary of the update.
	!*******************************************************************

	TITLE$(1%) = "Read General Ledger Data"
	TITLE$(2%) = "For the GL Period " + YYYY_PP$
	TITLE$(3%) = "CK System"
	TITLE$(4%) = ""

	TITLE$(5%) = "Bank Code      Deposit         " + &
		"Check        Adjust         Total"
	TITLE$(6%) = ""

	!
	! Init totals
	!
	TOTAL_DEPOSIT = 0.0
	TOTAL_CHECK = 0.0
	TOTAL_ADJUST = 0.0

	!
	! Print out summary
	!
	FOR I% = 1% TO BANK_TOTAL%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			BANK_TOTAL(I%)::BANK_ACCT + "  " + &
			FORMAT$(BANK_TOTAL(I%)::DEPOSIT, "###,###,###.##") + &
			FORMAT$(BANK_TOTAL(I%)::CHECK, "###,###,###.##") + &
			FORMAT$(BANK_TOTAL(I%)::ADJUST, "###,###,###.##") + &
			FORMAT$(BANK_TOTAL(I%)::DEPOSIT + &
				BANK_TOTAL(I%)::CHECK + &
				BANK_TOTAL(I%)::ADJUST, "###,###,###.##"), &
			0%)

			TOTAL_DEPOSIT = TOTAL_DEPOSIT + BANK_TOTAL(I%)::DEPOSIT
			TOTAL_CHECK = TOTAL_CHECK + BANK_TOTAL(I%)::CHECK
			TOTAL_ADJUST = TOTAL_ADJUST + BANK_TOTAL(I%)::ADJUST
	NEXT I%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Print out Grand total
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"Total   " + &
		FORMAT$(TOTAL_DEPOSIT, "###,###,###.##") + &
		FORMAT$(TOTAL_CHECK, "###,###,###.##") + &
		FORMAT$(TOTAL_ADJUST, "###,###,###.##") + &
		FORMAT$(TOTAL_DEPOSIT + TOTAL_CHECK + &
			TOTAL_ADJUST, "###,###,###.##"), &
		0%)

	!
	! Finish up report
	!
	CALL OUTP_FINISH(UTL_REPORTX)

	GOTO ExitProgram

	%PAGE

	!*******************************************************************
	! Exit the program
	!*******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

32767	END
