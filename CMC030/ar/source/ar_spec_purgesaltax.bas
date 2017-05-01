1	%TITLE "Purge AR Closed and Customer Files"
	%SBTTL "AR_SPEC_PURGESALTAX"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1995 BY
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
	! or reliability of its software on equipment which is not supported
	! by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Purges the sales tax register up to a given date.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_SPEC_PURGESALTAX/LINE
	!	$ LINK/EXECUTABLE=AR_EXE: AR_SPEC_PURGESALTAX,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_SPEC_PURGESALTAX.OBJ;*
	!
	! Author:
	!
	!	07/06/95 - Kevin Handy
	!
	! Modification history:
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/20/99 - Kevin Handy
	!		Fix unsolicited input (remove)
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.HB"
	MAP	(AR_SALTAXLED)	AR_SALTAXLED_CDD	AR_SALTAXLED

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Open Accounts Receivable Open Item file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.MOD"
	USE
		FILENAME$ = "AR_SALTAXLED"
		CONTINUE HelpError
	END WHEN

	YYYY_PP$ = "19500101"

500	!
	! Paint the background
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, &
		SMG_SCREEN_DATA%, SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		" Purge Sales Tax Register" + &
		TRM$(SCOPE::PRG_COMPANY) + " ", SMG$K_TOP)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"All transaction dated before " + &
		PRNT_DATE(YYYY_PP$, 8%) + " will be purged", 4%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Customer # ", 6%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 2%, 2%)

	%PAGE

510	!
	! Confirm whether or not to purge
	!
	SCOPE::PRG_ITEM = "TODATE"
	YYYY_PP$ = ENTR_3DATE(SCOPE, SCOPE::SMG_OPTION, "", &
		"Purge up to date ", YYYY_PP$, 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"All transaction dated before " + &
		PRNT_DATE(YYYY_PP$, 8%) + " will be purged", 4%, 5%)

	!
	! Confirm whether or not to purge
	!
	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", "Confirm purge process " + &
		" - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "") IF INP$ <> "Y"

	!
	! Set help message
	!
	SCOPE::PRG_ITEM = "HELP"

	%PAGE

1000	!******************************************************************
	! Purge AR closed file
	!******************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Purging Sales tax Register", 1%)

	WHEN ERROR IN
		RESET #AR_SALTAXLED.CH%
	USE
		FILENAME$ = "AR_SALTAXLED"
		CONTINUE HelpError
	END WHEN

	TOTAL_REMOVED% = 0%

1020	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_SALTAXLED.CH%
	USE
		CONTINUE 5000 IF ERR = 11%
		FILENAME$ = "AR_SALTAXLED"
		CONTINUE HelpError
	END WHEN

1030	!
	! Check against purge date
	!
	IF AR_SALTAXLED::TRADAT < YYYY_PP$
	THEN
		WHEN ERROR IN
			DELETE #AR_SALTAXLED.CH%
		USE
			FILENAME$ = "AR_SALTAXLED"
			CONTINUE HelpError
		END WHEN

		TOTAL_REMOVED% = TOTAL_REMOVED% + 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Removed " + &
			NUM1$(TOTAL_REMOVED%) + " records", 8%, 5%)

	END IF

	GOTO 1020

	%PAGE

5000	!
	! Exit program
	!
	CALL ENTR_3MESSAGE(SCOPE, "Purge Completed.", 0%)

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************

	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of AR_SPEC_PURGESALTAX
	!******************************************************************
	END
