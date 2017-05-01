1	%TITLE "Test for Invalid Vendor Numbers"
	%SBTTL "AP_SPEC_TESTACCT"
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Test For Undefined Vendor Numbers\* option
	!	reads all transactions
	!	in both the Accounts Payable Open and Account Payable Closed files
	!	in order to determine if there is are any transactions for a
	!	vendor for which there is no record in the Vendor Master file.
	!	.b
	!	If for some extremely unusual reason an orphan record should be
	!	found, the screen will display the following data contained in the
	!	transaction record:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Vendor Number
	!	.le
	!	Transaction Number
	!	.le
	!	Transaction Date
	!	.le
	!	Invoice Number
	!	.le
	!	Invoice Date
	!	.le
	!	Invoice Amount
	!	.end list
	!	.lm -5
	!	A message is also displayed indicating the subject vendor
	!	number is undefined.
	!
	! Index:
	!	.x Test for Undefined Vendor Numbers
	!
	! Option:
	!
	!	AP_SPEC_TESTACCT$CONFIRM
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_TESTACCT/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_SPEC_TESTACCT,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_TESTACCT.OBJ;*
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
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	06/06/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
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

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	! External functions
	!
	EXTERNAL LONG		OUTP_XUNSOL ! (It's really an AST routine)

	%PAGE

	ON ERROR GOTO 19000

	CALL READ_INITIALIZE

310	!
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
		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.OPN"
	USE
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Vendor file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

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
		"Check for invalid Vendor numbers " + &
		TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Vendor Number", 2%, 4%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Trans. Number", 4%, 4%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Trans. Date", 5%, 4%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Invoice Number", 7%, 4%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Invoice Date", 8%, 4%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Invoice Amount", 9%, 4%)

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
	!	^*Confirm Testing\*
	!	.b
	!	.lm +5
	!	^*Confirm Testing\* asks for user confirmation to the testing for
	!	invalid vendor numbers.
	!	.lm -5
	!
	! Index:
	!	.x Confirm Testing>Invalid Vendor Numbers
	!	.x Invalid Vendor Numbers>Confirm Testing
	!
	!--

	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", "Confirm testing " + &
		" - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	SCOPE::PRG_ITEM = "HELP"

	%PAGE

2000	!******************************************************************
	! Test for undefined vendor number in the open file
	!******************************************************************

	WHEN ERROR IN
		RESET #AP_OPEN.CH%
	USE
		CONTINUE 3000
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Testing for undefined vendor number", 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Vendor # in Open file", 12%, 4%)

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

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

	!
	! Exit keys
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	END SELECT

	RRR_FLAG% = 0%

	WHEN ERROR IN
		GET #AP_OPEN.CH%
	USE
		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

2030	IF TEST_VENNUM$ <> AP_OPEN::VENNUM
	THEN
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_OPEN::VENNUM, &
			12%, 26%)

		WHEN ERROR IN
			FIND #AP_VENDOR.CH%, &
				KEY #0% EQ AP_OPEN::VENNUM, &
				REGARDLESS
		USE
			CONTINUE 2040 IF ERR = 155%
			FILENAME$ = "AP_VENDOR"
			CONTINUE HelpError
		END WHEN

		GOTO 2050
	END IF

	GOTO 2050

2040	GOSUB 18000	! Undefined vendor #

2050	TEST_VENNUM$ = AP_OPEN::VENNUM
	GOTO 2020

	%PAGE

3000	!********************************************************************
	! Test for undefined vendor number in the closed file
	!********************************************************************

	WHEN ERROR IN
		RESET #AP_CLOSE.CH%
	USE
		CONTINUE 5000
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Vendor # in Closed file      ", 12%, 4%)

	!
	! Set flag
	!
	RRR_FLAG% = 0%

3020	!
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

	!
	! Exit keys
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	END SELECT

	RRR_FLAG% = 0%

	WHEN ERROR IN
		GET #AP_CLOSE.CH%
	USE
		CONTINUE 5000 IF ERR = 11%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

3030	IF TEST_VENNUM$ <> AP_CLOSE::VENNUM
	THEN
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			AP_CLOSE::VENNUM, 12%, 28%)

		WHEN ERROR IN
			FIND #AP_VENDOR.CH%, &
				KEY #0% EQ AP_CLOSE::VENNUM, &
				REGARDLESS
		USE
			CONTINUE 3040 IF ERR = 155%
			FILENAME$ = "AP_VENDOR"
			CONTINUE HelpError
		END WHEN

		GOTO 3050
	END IF

	GOTO 3050

3040	GOSUB 18100	! Undefined vendor #

3050	TEST_VENNUM$ = AP_CLOSE::VENNUM
	GOTO 3020

	%PAGE

5000	!********************************************************************
	! End process
	!********************************************************************
	CLOSE AP_OPEN.CH%, AP_CLOSE.CH%, AP_VENDOR.CH%

	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Process completed", 0%)

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

18000	!*****************************************************************
	! Display undefined vendor number in open file
	!*****************************************************************
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_OPEN::VENNUM, 2%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_OPEN::TRANKEY, 4%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PRNT_DATE(AP_OPEN::TRANKEY_DATE, 8%), 5%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_OPEN::INVNUM, 7%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PRNT_DATE(AP_OPEN::INVDAT, 8%), 8%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, FORMAT$(AP_OPEN::INVAMT, &
		"##,###,###.##"), 9%, 20%)

	SCOPE::PRG_ITEM = "UNDEF"

	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	CALL ENTR_3MESSAGE(SCOPE, TRM$(AP_OPEN::VENNUM) + " is undefined.", 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control c
	!
	CASE 3%
		GOTO 18000

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good characters
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_F7

	!
	! Bad characters
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 18000

	END SELECT

	CALL ENTR_3MESSAGE(SCOPE, "Testing for undefined vendor number", 1%)

	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RETURN

	%Page

18100	!*****************************************************************
	! Display undefined vendor number in closed file
	!*****************************************************************
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_CLOSE::VENNUM, 2%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_CLOSE::TRANKEY, 4%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PRNT_DATE(AP_CLOSE::TRANKEY_DATE, 8%), 5%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_CLOSE::INVNUM, 7%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PRNT_DATE(AP_CLOSE::INVDAT, 8%), 8%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(AP_CLOSE::INVAMT, &
		"##,###,###.##"), 9%, 20%)

	SCOPE::PRG_ITEM = "UNDEF"

	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	CALL ENTR_3MESSAGE(SCOPE, TRM$(AP_CLOSE::VENNUM) + " is undefined.", 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control c
	!
	CASE 3%
		GOTO 18100

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good characters
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_F7

	!
	! Bad characters
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 18100

	END SELECT

	CALL ENTR_3MESSAGE(SCOPE, "Testing for undefined vendor number", 1%)

	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RETURN

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
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
