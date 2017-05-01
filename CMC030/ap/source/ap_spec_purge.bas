1	%TITLE "Purge AP Closed File and Vendor File"
	%SBTTL "AP_SPEC_PURGE"
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
	!	The ^*Purge Accounts Payable Files\* option
	!	purges both Vendor Master files
	!	and History files.
	!	.b
	!	The execution of the ^*Purge\* option will purge all records in the
	!	Accounts Payable History file with a "post period" older than the
	!	"number of periods in retention cycle" recorded in the Accounts
	!	Payable Control Record.  In addition, any Vendor Master record will
	!	be purged ^&if\& the purge flag in the master record has been set to "Y"
	!	^&and\& there are no related records in either the Accounts Payable Open
	!	or Accounts Payable Closed file.
	!	.lm -5
	!
	! Index:
	!	.x Accounts Payable>Purge Files
	!	.x Purge Files>Accounts Payable
	!
	! Option:
	!
	!	AP_SPEC_PURGE$CONFIRM
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_PURGE/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_SPEC_PURGE,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_PURGE.OBJ;*
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
	!	06/06/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	03/12/92 - Kevin Handy
	!		Removed duplicate error trapping (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source code closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	09/18/96 - Kevin Handy
	!		Create new close file on AP_CLOSE.DEV$ instead
	!		of UTL_WORK.DEV$
	!		Clean up source code.
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/20/99 - Kevin Handy
	!		Fix calls to unsolicited input
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
	! Map statements
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP (AP_CLOSE)		AP_CLOSE_CDD	AP_CLOSE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	! External functions
	!
	EXTERNAL LONG		OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Declare variables
	!
	DECLARE INTEGER CONSTANT RECORD_ARRAY = 100

	DECLARE RFA RECORD_RFA(RECORD_ARRAY), RECORD_RFA

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(AP_TEMP_CLOSE.CH%, STAT%)

	!
	! Look up device
	!
	CALL READ_DEVICE("AP_CLOSE", AP_CLOSE.DEV$, STAT%)

300	!
	! Open ap open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

320	!
	! Open ap close file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.UPD"
	USE
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Vendor file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.MOD"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

340	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

350	!
	! Open ap Control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.MOD"

		GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "GL_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF AP_CONTROL::CLOSEFLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AP Close in process", &
			"ERR", "AP_CLOSE", "ERROR_CLOSE")
		GOTO ExitProgram
	END IF

	IF AP_CONTROL::CLOSEFLAG = "2"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AP Reset in process", &
			"ERR", "AP_RESET", "ERROR_RESET")
		GOTO ExitProgram
	END IF

	CUR_PERIOD% = AP_CONTROL::LASTPERCLOSE
	RETENTION% = AP_CONTROL::RETAIN

	YEAR_OFF% = RETENTION% / GL_PERIOD::FPFY

	PERIOD_OFF% = RETENTION% - YEAR_OFF% * GL_PERIOD::FPFY

	PERIOD_OFF% = CUR_PERIOD% - PERIOD_OFF%

	IF PERIOD_OFF% < 1%
	THEN
		YEAR_OFF% = YEAR_OFF% + 1%
		PERIOD_OFF% = GL_PERIOD::FPFY + PERIOD_OFF%
	END IF

	YEAR$ = FORMAT$((VAL%(AP_CONTROL::YEAR) - YEAR_OFF%), "<0>###")

	YYYY_PP$ = YEAR$ + "_" + FORMAT$(PERIOD_OFF%, "<0>#")

360	!======================================================================
	! AP_CLOSE file (create, open read/write)
	!======================================================================

	CALL WRIT_CURPROTECTION(AP_CLOSE.PRO$, ST%)

	WHEN ERROR IN
		OPEN AP_CLOSE.DEV$ + "AP_TEMP_CLOSE.LED" FOR OUTPUT AS FILE AP_TEMP_CLOSE.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP AP_CLOSE, &
			PRIMARY KEY (AP_CLOSE::VENNUM, AP_CLOSE::TRANKEY) DUPLICATES, &
			ALTERNATE KEY (AP_CLOSE::VENNUM, AP_CLOSE::INVNUM) DUPLICATES CHANGES, &
			ALTERNATE KEY AP_CLOSE::BATCH DUPLICATES CHANGES, &
			ACCESS MODIFY, ALLOW MODIFY
	USE
		FILENAME$ = "AP_TEMP_CLOSE"
		CONTINUE HelpError
	END WHEN

	CALL WRIT_CURPROTECTION(OLD_PROT$, ST%)

500	!
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
		"Purge Accounts Payable " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "All transaction dated before " + &
			YYYY_PP$ + " will be purged", 4%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Vendor # ", 6%, 5%)

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
	!	^*Confirm\*
	!	.b
	!	.lm +5
	!	^*Confirm\* asks for user confirmation to the Purging Process for the
	!	specified period.
	!	.lm -5
	!
	! Index:
	!	.x Confirm>Purge
	!	.x Purge>Confirm
	!
	!--
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm purge process - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	!
	! Set help message
	!
	SCOPE::PRG_ITEM = "HELP"

	%PAGE

1000	!******************************************************************
	! Purge ap closed file
	!******************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Purging closed file", 1%)

	WHEN ERROR IN
		RESET #AP_CLOSE.CH%
	USE
		CONTINUE 2000
	END WHEN

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

1015	!
	! Set close flag in control file
	!
	WHEN ERROR IN
		GET #AP_CONTROL.CH%, RECORD 1%

		AP_CONTROL::CLOSEFLAG = "3"

		UPDATE #AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

1020	!
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
		GET #AP_CLOSE.CH%
	USE
		CONTINUE 2000 IF ERR = 11%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

	RECORD_RFA = GETRFA(AP_CLOSE.CH%)

	IF TEST_VENNUM$ <> AP_CLOSE::VENNUM
	THEN
		GOSUB PutRecord
		GET #AP_CLOSE.CH%, RFA RECORD_RFA
		RECORD_LOOP% = 0%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			AP_CLOSE::VENNUM, 6%, 23%)

		TRANKEY$ = "ZZZZZZZZZZZZZZZ"
		CURRENT_FLAG%, FIRST_PASS% = 0%

	END IF

	TEST_VENNUM$ = AP_CLOSE::VENNUM

	IF AP_CLOSE::TRANKEY <> TRANKEY$ AND FIRST_PASS%
	THEN
		GOSUB PutRecord
		GET #AP_CLOSE.CH%, RFA RECORD_RFA
		RECORD_LOOP% = 0%

		CURRENT_FLAG% = 0%
	END IF

	CURRENT_FLAG% = -1% IF YYYY_PP$ <= LEFT(AP_CLOSE::UPDATED, 4%) + &
		"_" + MID(AP_CLOSE::UPDATED, 5%, 2%)

	TRANKEY$ = AP_CLOSE::TRANKEY
	FIRST_PASS% = -1%

	RECORD_LOOP% = RECORD_LOOP% + 1%
	RECORD_RFA(RECORD_LOOP%) = GETRFA(AP_CLOSE.CH%)

	GOTO 1020

	%PAGE

2000	!
	! Put last record out to the close file
	!
	IF RECORD_LOOP% <> 0%
	THEN
		GOSUB PutRecord
	END IF

	!******************************************************************
	! Purge ap vendor file
	!******************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Purging vendor file file", 1%)

	WHEN ERROR IN
		RESET #AP_VENDOR.CH%
	USE
		CONTINUE 3000
	END WHEN

2020	!
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
		GET #AP_VENDOR.CH%
	USE
		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_VENDOR::VENNUM, &
		6%, 23%)

2030	WHEN ERROR IN
		FIND #AP_OPEN.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE 2040 IF ERR = 155%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 2060

2040	WHEN ERROR IN
		FIND #AP_TEMP_CLOSE.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE 2050 IF ERR = 155%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

	GOTO 2060

2050	IF AP_VENDOR::PURGE = "Y"
	THEN
		WHEN ERROR IN
			DELETE #AP_VENDOR.CH%
		USE
			FILENAME$ = "AP_VENDOR"
			CONTINUE HelpError
		END WHEN
	END IF

2060	GOTO 2020

3000	!
	! Kill ap_close file and rename temp file to close
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Renaming close temp file to close file. . .", 1%)

	CLOSE AP_CLOSE.CH%, AP_TEMP_CLOSE.CH%

 !	WHEN ERROR IN
 !		KILL AP_CLOSE.DEV$ + "AP_CLOSE.LED" FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 3010
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AP_CLOSE.DEV$ + "AP_CLOSE.LED;*")

3010	NAME AP_CLOSE.DEV$ + "AP_TEMP_CLOSE.LED" AS &
		AP_CLOSE.DEV$ + "AP_CLOSE.LED"

3090	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Purge completed", 0%)

4000	!
	! Update Control file
	!
	WHEN ERROR IN
		GET #AP_CONTROL.CH%, RECORD 1%

		AP_CONTROL::CLOSEFLAG = "0"

		UPDATE #AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	CLOSE AP_CLOSE.CH%, AP_CONTROL.CH%, AP_TEMP_CLOSE.CH%

5000	!
	! Kill temp files
	!
 !	WHEN ERROR IN
 !		KILL AP_CLOSE.DEV$ + "AP_TEMP_CLOSE.LED" FOR LOOP% = 1% TO 10%
 !	USE
 !		CONTINUE 5010
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AP_CLOSE.DEV$ + "AP_TEMP_CLOSE.LED;*")

5010	!
	! Exit program
	!
 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 PutRecord:
18000	!*******************************************************************
	! Put records in temporary file
	!*******************************************************************
	RETURN IF RECORD_LOOP% = 0% OR CURRENT_FLAG% = 0%

	FOR LOOP% = 1% TO RECORD_LOOP%

		WHEN ERROR IN
			GET #AP_CLOSE.CH%, RFA RECORD_RFA(LOOP%)
		USE
			FILENAME$ = "AP_CLOSE"
			CONTINUE HelpError
		END WHEN

18010		WHEN ERROR IN
			PUT #AP_TEMP_CLOSE.CH%
		USE
			FILENAME$ = "AP_TEMP_CLOSE"
			CONTINUE HelpError
		END WHEN

	NEXT LOOP%

18090	RETURN

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
