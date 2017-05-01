1	%TITLE "Accounts Payable Closing Program"
	%SBTTL "AP_CLOS_CLOSE"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Close Accounts Payable Ledger to History\* option
	!	transfers to the Closed History file from the Accounts
	!	Open file all items which have been paid and, therefore,
	!	have zero balances.
	!	.b
	!	This option should be executed for each accounting period after
	!	it has been determined that the Accounts
	!	Payable Cut-off Subsidiary
	!	Ledger balance agrees with the Accounts Payable
	!	control account in the General Ledger.
	!	.lm -5
	!
	! Index:
	!	.x Accounts Payable>Close Routine
	!
	! Option:
	!
	!	AP_CLOS_CLOSE$CONFIRM
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_CLOS_CLOSE/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_CLOS_CLOSE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_CLOS_CLOSE.OBJ;*
	!
	! Author:
	!
	!	12/23/86 - Kevin Handy
	!
	! Modification history:
	!
	!	10/21/87 - Robert Peterson
	!		Add interrupt menu during create of work file
	!
	!	11/14/87 - Robert Peterson
	!		Modify close to include only 1099 history if
	!		1099 history only flag has been set.  Store
	!		no history if the retention period is 0
	!
	!	05/17/88 - Lance Williams
	!		Modified the header.
	!
	!	09/08/88 - Kevin Handy
	!		Modified close to use .NEW file open routines
	!		instead of hard coding opens into the source
	!		code.  Fixed handling of AP_CLOSE file.
	!
	!	05/05/89 - Kevin Handy
	!		Modified to leave distribution items in
	!		AP_OPEN_DIST so that the reverse would
	!		be completely reversable.
	!
	!	07/20/90 - Kevin Handy
	!		Modified to open AP_CLOSE in update mode in an
	!		attempt to speed up the close a little bit more.
	!
	!	09/24/90 - Kevin Handy
	!		Trapped undefined vendor number error at 1130.
	!
	!	06/05/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	07/15/91 - Craig Tanner
	!		Further untangled error trapping.
	!
	!	11/02/92 - Dan Perkins
	!		Open AP_OPEN and AP_CLOSED files with extension
	!		.PST.
	!
	!	12/06/93 - Kevin Handy
	!		Modified to use COPY_COPYRECORDS in order to speed
	!		this puppy up.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/14/95 - Kevin Handy
	!		Format source closer to 80 columns.
	!		Use FORMAT$() instead of RIGHT(NUM1$(...))
	!
	!	07/02/96 - Kevin Handy
	!		Removed lots of commented out code.
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/97 - Kevin Handy
	!		Change 'VAL(' to 'VAL%('
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/16/99 - Kevin Handy
	!		Use COPY_COPYFILE instead of COPY_COPYRECORDS so
	!		that we are more compatible with Alpha (and faster
	!		too)
	!
	!	05/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/13/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD and Map statements
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP (AP_CLOSE)		AP_CLOSE_CDD	AP_CLOSE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL

	!
	! Declare variables
	!
	DECLARE INTEGER CONSTANT RECORD_ARRAY = 100
	DECLARE RFA RECORD_RFA(RECORD_ARRAY), RECORD_RFA

	ON ERROR GOTO 19000

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	!
	! Open AP open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.PST"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

320	!
	! Open AP close file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.PST"
	USE
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

330	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%
		CLOSE GL_PERIOD.CH%
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

340	!
	! Open Vendor file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

350	!
	! Open ap Control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.MOD"

		GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	!
	! Resetting of the file
	!
	IF AP_CONTROL::CLOSEFLAG = "2"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AP Reset in process", &
			"ERR", "AP_RESET", "ERROR_RESET")
		GOTO ExitProgram
	END IF

	!
	! Purging of the file
	!
	IF AP_CONTROL::CLOSEFLAG = "3"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AP Purge in process", &
			"ERR", "AP_PURGE", "ERROR_PURGE")
		GOTO ExitProgram
	END IF

	CUR_PERIOD% = AP_CONTROL::LASTPERCLOSE + 1%
	YEAR$ = AP_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	YYYY_PP$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

360	!
	! New open item file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.NEW"
	USE
		FILENAME$ = "AP_TEMP_OPEN"
		CONTINUE HelpError
	END WHEN

500	!
	! Paint the background, and confirm close
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
		"Accounts Payable Close for " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	!
	! Put the data on the screen
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "CLOSING " + &
		LEFT(YYYY_PP$, 4%) + "_" + RIGHT(YYYY_PP$, 5%) + &
		" " + GL_PERIOD::PERIOD(CUR_PERIOD%), 4%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Vendor # ", 6%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "End Balance", 8%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Close Balance", 10%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	SCOPE::PRG_ITEM = "CONFIRM"
	!++
	! Abstract:CONFIRM
	!	^*Confirm\*
	!	.b
	!	.lm +5
	!	^*Confirm\* asks for user confirmation for the period which will be closed to
	!	the history account. This gives the user an opportunity to change the closing
	!	decision.
	!	.lm -5
	!
	! Index:
	!	.x Confirm>Close to History Account
	!
	!--
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Confirm closing - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	%PAGE

1000	!******************************************************************
	! Check to see if close had crashed??
	!******************************************************************
	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%


1050	!*******************************************************************
	! Copy closed file information over
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Creating copy of closed data", 1%)

	CLOSE AP_CLOSE.CH%

	AP_CLOSE_NEW.NAME$ = AP_CLOSE.NAME$ + "_NEW"
 !	STATUS% = COPY_COPYRECORDS(AP_CLOSE.NAME$, AP_CLOSE_NEW.NAME$)
	STATUS% = COPY_COPYFILE(AP_CLOSE.NAME$, AP_CLOSE_NEW.NAME$)
	IF (STATUS AND 1%)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to create copy of closed file!", 0%)
 !		KILL AP_CLOSE_NEW.NAME$
		SMG_STATUS% = LIB$DELETE_FILE(AP_CLOSE_NEW.NAME$ + ";*")

		GOTO AbortExit
	END IF

1060	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.NEU"

1100	!******************************************************************
	! Close Accounts payable
	!******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Processing open data", 1%)

	WHEN ERROR IN
		RESET #AP_OPEN.CH%
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

1115	!
	! Set close flag in control file
	!
	WHEN ERROR IN
		GET #AP_CONTROL.CH%, RECORD 1%

		AP_CONTROL::CLOSEFLAG = "1"

		UPDATE #AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

1120	!
	! Main loop starts here
	!
	!
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

		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	END SELECT

	RRR_FLAG% = 0%

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_OPEN.CH%
	USE
		CONTINUE 1200 IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	RECORD_RFA = GETRFA(AP_OPEN.CH%)

1130	!
	! Find the vendor
	!
	IF TEST_VENNUM$ <> AP_OPEN::VENNUM
	THEN
		AP_VENDOR::FLG1099 = "?"

		WHEN ERROR IN
			GET #AP_VENDOR.CH%, KEY #0% EQ AP_OPEN::VENNUM, REGARDLESS
		USE
			CONTINUE 1140
		END WHEN
	END IF

1140	IF TEST_VENNUM$ <> AP_OPEN::VENNUM
	THEN
		GOSUB PutRecord
		GET #AP_OPEN.CH%, RFA RECORD_RFA
		RECORD_LOOP% = 0%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_OPEN::VENNUM, &
			6%, 23%)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			FORMAT$(END_BAL_DUE, "##,###,###.##"), &
			8%, 23%)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			FORMAT$(CLOSE_BAL_DUE, "##,###,###.##"), &
			10%, 23%)

		!
		! Assign something impossible
		!
		TRANKEY$ = "ZZZZZZZZZZZ"
		CURRENT_FLAG%, FIRST_PASS% = 0%

		BAL_DUE = 0.0
	END IF

	TEST_VENNUM$ = AP_OPEN::VENNUM

	!
	! Test for the pass
	!
	IF AP_OPEN::TRANKEY <> TRANKEY$ AND FIRST_PASS%
	THEN
		GOSUB PutRecord
		GET #AP_OPEN.CH%, RFA RECORD_RFA
		RECORD_LOOP% = 0%

		BAL_DUE = 0.0
		CURRENT_FLAG% = 0%
	END IF

	BAL_DUE = FUNC_ROUND(BAL_DUE + (AP_OPEN::INVAMT - AP_OPEN::DISAMT) - &
		AP_OPEN::CKAMT, 2%)

	END_BAL_DUE = FUNC_ROUND(END_BAL_DUE + &
		(AP_OPEN::INVAMT - AP_OPEN::DISAMT) - &
		AP_OPEN::CKAMT, 2%)

	CLOSE_BAL_DUE = FUNC_ROUND(CLOSE_BAL_DUE + (AP_OPEN::INVAMT - &
		AP_OPEN::DISAMT), 2%) &
		IF YYYY_PP$ >= LEFT(AP_OPEN::UPDATED, 6%)

	CLOSE_BAL_DUE = FUNC_ROUND(CLOSE_BAL_DUE - AP_OPEN::CKAMT, 2%) &
		IF YYYY_PP$ >= LEFT(AP_OPEN::UPDATED, 6%)

	CURRENT_FLAG% = -1% IF YYYY_PP$ < LEFT(AP_OPEN::UPDATED, 6%)

	TRANKEY$ = AP_OPEN::TRANKEY
	FIRST_PASS% = -1%

	RECORD_LOOP% = RECORD_LOOP% + 1%
	RECORD_RFA(RECORD_LOOP%) = GETRFA(AP_OPEN.CH%)

	GOTO 1120

	%PAGE

1200	!
	! Complete Initail process
	!
	GOSUB PutRecord

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_OPEN::VENNUM, &
		6%, 23%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(END_BAL_DUE, "##,###,###.##"), &
		8%, 23%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(CLOSE_BAL_DUE, "##,###,###.##"), &
		10%, 23%)

	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM = "BALANC"
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Are balances correct?  confirm - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO AbortExit
	END IF

	%PAGE

1210	!
	! Add closed records to closed file
	!
1300	!
	! Kill ap_open file and rename temp file to open
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Renaming open temp file to open file. . .", 1%)

	CLOSE AP_OPEN.CH%, AP_OPEN.CH_NEW%, AP_CLOSE.CH%, AP_CLOSE.CH_NEW%

 !	WHEN ERROR IN
 !		KILL AP_OPEN.NAME$ FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 1310
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AP_OPEN.NAME$ + ";*")

1310	WHEN ERROR IN
		NAME AP_OPEN.NAME_NEW$ AS AP_OPEN.NAME$
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

1320 !	WHEN ERROR IN
 !		KILL AP_CLOSE.NAME$ FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 1330
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AP_CLOSE.NAME$ + ";*")

1330	NAME AP_CLOSE.NAME_NEW$ AS AP_CLOSE.NAME$

1390	!
	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Closing completed", 0%)

1400	!
	! Update Control file
	!
	WHEN ERROR IN
		GET #AP_CONTROL.CH%, RECORD 1%

		AP_CONTROL::LASTPERCLOSE = CUR_PERIOD%
		AP_CONTROL::YEAR = YEAR$

		UPDATE #AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

 AbortExit:
	GET #AP_CONTROL.CH%, RECORD 1%

	AP_CONTROL::CLOSEFLAG = "0"

	UPDATE #AP_CONTROL.CH%

	CLOSE AP_OPEN.CH%, AP_CLOSE.CH%, AP_CONTROL.CH%, &
		AP_CLOSE.CH_NEW%, AP_OPEN.CH_NEW%

1500	!
	! Kill temp files
	!
 !	WHEN ERROR IN
 !		KILL AP_OPEN.NAME_NEW$ FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 1510
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AP_OPEN.NAME_NEW$ + ";*")

1510 !	WHEN ERROR IN
 !		KILL AP_CLOSE.NAME_NEW$ FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 1520
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AP_CLOSE.NAME_NEW$ + ";*")

1520	!
	! Exit program
	!
 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 PutRecord:
18000	!*******************************************************************
	! Put records in temporary file
	!*******************************************************************
	GOTO 18090 IF RECORD_LOOP% = 0%

	GOTO 18020 IF FUNC_ROUND(BAL_DUE, 2%) <> 0.0 OR CURRENT_FLAG% <> 0%

	!
	! Check to see if storing only 1099 history
	!
	IF AP_CONTROL::RETAIN_1099_ONLY = "Y"
	THEN
		IF AP_VENDOR::FLG1099 <> "Y"
		THEN
			GOTO 18090
		END IF
	END IF

	!
	! Skip storing history if the retention period is 0
	!
	IF AP_CONTROL::RETAIN <= 0%
	THEN
		GOTO 18090
	END IF

	FOR LOOP% = 1% TO RECORD_LOOP%

		WHEN ERROR IN
			GET #AP_OPEN.CH%, RFA RECORD_RFA(LOOP%)
		USE
			FILENAME$ = "AP_OPEN"
			CONTINUE HelpError
		END WHEN

		AP_CLOSE::VENNUM	= AP_OPEN::VENNUM
		AP_CLOSE::TRANKEY	= AP_OPEN::TRANKEY
		AP_CLOSE::TRANKEY_DATE	= AP_OPEN::TRANKEY_DATE
		AP_CLOSE::INVNUM	= AP_OPEN::INVNUM
		AP_CLOSE::INVDAT	= AP_OPEN::INVDAT
		AP_CLOSE::INVAMT	= AP_OPEN::INVAMT
		AP_CLOSE::CODE_1099	= AP_OPEN::CODE_1099
		AP_CLOSE::AMT_1099	= AP_OPEN::AMT_1099
		AP_CLOSE::USE_JOB_NUM	= AP_OPEN::USE_JOB_NUM
		AP_CLOSE::USE_AMT	= AP_OPEN::USE_AMT
		AP_CLOSE::DISCDAT	= AP_OPEN::DISCDAT
		AP_CLOSE::DISAMT	= AP_OPEN::DISAMT
		AP_CLOSE::DUEDAT	= AP_OPEN::DUEDAT
		AP_CLOSE::PONUM		= AP_OPEN::PONUM
		AP_CLOSE::AP_ACCT	= AP_OPEN::AP_ACCT
		AP_CLOSE::CASH_ACCT	= AP_OPEN::CASH_ACCT
		AP_CLOSE::CKNUM		= AP_OPEN::CKNUM
		AP_CLOSE::CKDAT		= AP_OPEN::CKDAT
		AP_CLOSE::CKDESC	= AP_OPEN::CKDESC
		AP_CLOSE::CKAMT		= AP_OPEN::CKAMT
		AP_CLOSE::UPDATED	= AP_OPEN::UPDATED
		AP_CLOSE::SELECTED	= AP_OPEN::SELECTED
		AP_CLOSE::BATCH		= AP_OPEN::BATCH
		AP_CLOSE::CLOSEDATE	= YYYY_PP$

18010		WHEN ERROR IN
			PUT #AP_CLOSE.CH_NEW%
		USE
			FILENAME$ = "AP_TEMP_CLOSE"
			CONTINUE HelpError
		END WHEN

	NEXT LOOP%

	GOTO 18090

18020	FOR LOOP% = 1% TO RECORD_LOOP%

		WHEN ERROR IN
			GET #AP_OPEN.CH%, RFA RECORD_RFA(LOOP%)
		USE
			FILENAME$ = "AP_OPEN"
			CONTINUE HelpError
		END WHEN

18030		WHEN ERROR IN
			PUT #AP_OPEN.CH_NEW%
		USE
			FILENAME$ = "AP_TEMP_OPEN"
			CONTINUE HelpError
		END WHEN

	NEXT LOOP%

18090	RETURN

	%PAGE

 HelpError:
	!**********************************************************
	! Help Message for an error
	!**********************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
