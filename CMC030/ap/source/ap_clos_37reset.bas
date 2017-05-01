1	%TITLE "Accounts Payable Reset Close Program"
	%SBTTL "AP_CLOS_37RESE"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 2000 BY
	!	Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Reset Accounts Payable Ledger from History\* option
	!	reverses an Accounts Payable Close.
	!	.b
	!	^*Note:\* This option does not recover Accounts Payable detail, only
	!	register information is restored. One period at a time should be reset.
	!	Otherwise, two periods will be reset, and both will be closed into the
	!	same period.
	!	.lm -5
	!
	! Index:
	!	.x Reset>Close
	!	.x Close>Reset
	!
	! Option:
	!
	!	AP_CLOS_37RESET$BALANC
	!	AP_CLOS_37RESET$CONFIRM
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_CLOS_37RESET/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_CLOS_37RESET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_CLOS_37RESET.OBJ;*
	!
	! Author:
	!
	!	06/30/2000 - Kevin Handy
	!		New version using multiple close files instead
	!		of a large monolithic one.
	!
	! Modification history:
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

	%INCLUDE "SOURCE:[AP.OPEN]AP_37CLOSE.HB"
	MAP (AP_37CLOSE)	AP_37CLOSE_CDD	AP_37CLOSE

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
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Declare variables
	!
	DECLARE INTEGER CONSTANT RECORD_ARRAY = 100%

	ON ERROR GOTO 19000

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	!
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
	IF AP_CONTROL::CLOSEFLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AP Close in process", &
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

	CUR_PERIOD% = AP_CONTROL::LASTPERCLOSE
	YEAR$ = AP_CONTROL::YEAR

	YYYY_PP$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

310	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

320	!
	! Open AP open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.UPD"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

330	!
	! Open AP close file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_37CLOSE.OPN"
	USE
		FILENAME$ = "AP_37CLOSE"
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
		"Accounts Payable Reset for " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	!
	! Put the data on the screen
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "RESETING " + &
		LEFT(YYYY_PP$, 4%) + "_" + RIGHT(YYYY_PP$, 5%) + &
		" " + GL_PERIOD::PERIOD(CUR_PERIOD%), 4%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Vendor #", &
		6%, 5%)

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
	!	The ^*Confirm\* asks for user confirmation of the accounts to be reset from
	!	the closed status. This gives the user the opportunity to change the reset
	!	decision.
	!	.lm -5
	!
	! Index:
	!	.x Confirm>Accounts Payable Reset
	!
	!--
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Confirm reseting - then press <Do> ", "N", 0%, "", "")

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


1100	!******************************************************************
	! Close Accounts payable
	!******************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Reseting", 1%)

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
		AP_CONTROL::CLOSEFLAG = "2"
		UPDATE #AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	LAST_VENDOR$ = ""

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
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	END SELECT

	RRR_FLAG% = 0%

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_37CLOSE.CH%
	USE
		CONTINUE 1200 IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	IF (LAST_VENDOR$ <> AP_37CLOSE::VENNUM)
	THEN
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			AP_37CLOSE::VENNUM, 6%, 23%)

		LAST_VENDOR$ = AP_37CLOSE::VENNUM + ""
	END IF

1130	!
	! Find the vendor
	!
	AP_OPEN::VENNUM		= AP_37CLOSE::VENNUM
	AP_OPEN::TRANKEY	= AP_37CLOSE::TRANKEY
	AP_OPEN::TRANKEY_DATE	= AP_37CLOSE::TRANKEY_DATE
	AP_OPEN::INVNUM		= AP_37CLOSE::INVNUM
	AP_OPEN::INVDAT		= AP_37CLOSE::INVDAT
	AP_OPEN::INVAMT		= AP_37CLOSE::INVAMT
	AP_OPEN::CODE_1099	= AP_37CLOSE::CODE_1099
	AP_OPEN::AMT_1099	= AP_37CLOSE::AMT_1099
	AP_OPEN::USE_JOB_NUM	= AP_37CLOSE::USE_JOB_NUM
	AP_OPEN::USE_AMT	= AP_37CLOSE::USE_AMT
	AP_OPEN::DISCDAT	= AP_37CLOSE::DISCDAT
	AP_OPEN::DISAMT		= AP_37CLOSE::DISAMT
	AP_OPEN::DUEDAT		= AP_37CLOSE::DUEDAT
	AP_OPEN::PONUM		= AP_37CLOSE::PONUM
	AP_OPEN::AP_ACCT	= AP_37CLOSE::AP_ACCT
	AP_OPEN::CASH_ACCT	= AP_37CLOSE::CASH_ACCT
	AP_OPEN::CKNUM		= AP_37CLOSE::CKNUM
	AP_OPEN::CKDAT		= AP_37CLOSE::CKDAT
	AP_OPEN::CKDESC		= AP_37CLOSE::CKDESC
	AP_OPEN::CKAMT		= AP_37CLOSE::CKAMT
	AP_OPEN::UPDATED	= AP_37CLOSE::UPDATED
	AP_OPEN::SELECTED	= AP_37CLOSE::SELECTED
	AP_OPEN::BATCH		= AP_37CLOSE::BATCH

	PUT #AP_OPEN.CH_NEW%

	GOTO 1120

	%PAGE

1200	!
	! Complete Initail process
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_OPEN::VENNUM, &
		6%, 23%)

	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM = "BALANC"
	!++
	! Abstract:BALANC
	!	^*Reset Correct\*
	!	.b
	!	.lm +5
	!	The ^*Reset Correct\* again asks for user confirmation as to whether the reset
	!	files are correct. This re-affirms the users decision to reset or allows
	!	for escape from the process.
	!	.lm -5
	!
	! Index:
	!	.x Reset Correct>Reset Accounts Payable Ledger
	!	.x Reset Accounts Payable Ledger>Reset Correct
	!
	!--
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Is reset correct?  confirm - then press <Do> ", &
		"N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO AbortExit
	END IF

	%PAGE

1210	!
	! Add open records to closed file
	!
	RESET #AP_OPEN.CH%

	CALL ENTR_3MESSAGE(SCOPE, &
		"Adding closed records to closed file. . .", 1%)

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

1220	!
	! Main loop starts here
	!

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_OPEN.CH%
	USE
		CONTINUE 1300 IF ERR = 11%
		FILENAME$ = "AP_TEMP_CLOSE"
		CONTINUE HelpError
	END WHEN

	IF (LAST_VENDOR$ <> AP_OPEN::VENNUM)
	THEN
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_OPEN::VENNUM, &
			6%, 23%)

		LAST_VENDOR$ = AP_OPEN::VENNUM + ""
	END IF

1230	WHEN ERROR IN
		PUT #AP_OPEN.CH_NEW%
	USE
		FILENAME$ = "AP_37CLOSE"
		CONTINUE HelpError
	END WHEN

1290	GOTO 1220

1300	!
	! Kill ap_open file and rename temp file to open
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Renaming open temp file to open file. . .", 1%)

	CLOSE AP_OPEN.CH%, AP_OPEN.CH_NEW%, AP_37CLOSE.CH%, AP_37CLOSE.CH_NEW%

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
 !		KILL AP_37CLOSE.NAME$ FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 1330
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AP_37CLOSE.NAME$ + ";*")

1330	!

1390	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Reseting completed", 0%)

1400	!
	! Update Control file
	!
	WHEN ERROR IN
		GET #AP_CONTROL.CH%, RECORD 1%

		CUR_PERIOD% = CUR_PERIOD% - 1%
		IF CUR_PERIOD% < 1%
		THEN
			CUR_PERIOD% = GL_PERIOD::FPFY
			YEAR$ = FORMAT$(VAL%(YEAR$) - 1%, "<0>###")
		END IF

		AP_CONTROL::LASTPERCLOSE = CUR_PERIOD%
		AP_CONTROL::YEAR = YEAR$

		UPDATE #AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

 AbortExit:
	WHEN ERROR IN
		GET #AP_CONTROL.CH%, RECORD 1%
		AP_CONTROL::CLOSEFLAG = "0"
		UPDATE #AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	CLOSE AP_OPEN.CH%, AP_37CLOSE.CH%, AP_CONTROL.CH%, AP_OPEN.CH_NEW%

1500	!
	! Kill temp files
	!
	WHEN ERROR IN
		KILL AP_OPEN.NAME_NEW$ FOR LOOP% = 1% TO 10%
	USE
		CONTINUE 1520
	END WHEN

1520	GOTO ExitProgram

	!
	! Exit program
	!
 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

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
