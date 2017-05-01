1	%TITLE "SETFLG - Set Update Flags in Folder"
	%SBTTL "PR_SPEC_SETFLAG"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program is used to force the update flags in a payroll folder
	!	to be a specific value.
	!	This program is designed to be used only as a last resort, and
	!	after changes have been made in all relevant history.
	!	.note DANGER
	!	This program should only be used as a last resort, and when you
	!	know absolutely what you are doing. Getting the update flags
	!	wrong can cause doubling in the general ledger, or the quarterly
	!	totals.
	!	.end note
	!	.lm -5
	!
	! Index:
	!	.x Status>Set Flags
	!	.x Set Flags>Status
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_SETFLAG/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_SPEC_SETFLAG, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_SETFLAG.OBJ;*
	!
	! Author:
	!
	!	03/08/89 - Kevin Handy
	!
	! Modification history:
	!
	!	07/28/89 - Kevin Handy
	!		Dimensioned the two arrays DATE_FILE$ and
	!		REVERSE_LIST$ so that more than 10 files
	!		may show up on the list.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	08/06/93 - Kevin Handy
	!		Modified to allow individual files to not exist.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last parameter to entr_3choice.
	!
	!	08/02/95 - Kevin Handy
	!		Modify so that you can only enter on digit into
	!		the status field.
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/20/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK

	DIM DATE_FILE$(100%), REVERSE_LIST$(100%)

	%PAGE

	ON ERROR GOTO 19000

	!
	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************
	CALL READ_INITIALIZE

	!
	! Create display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

100	!******************************************************************
	! Get date for file name
	!******************************************************************

	!
	! Look up device
	!
	CALL  READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL  READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

	CALL FIND_FILE(PR_TRN_PAY.DEV$ + "PR_TRN_PAY_*.JRL", DATE_FILE$(), &
		16%, "", "")

	DATE_FILE% = VAL%(DATE_FILE$(0%))

	IF DATE_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "No payroll folder found", 0%)
		GOTO ExitProgram
	END IF

	REVERSE_LIST$(DATE_FILE% - LOOP% + 1%) = &
		MID(DATE_FILE$(LOOP%), 16%, 2%) + "/" + &
		MID(DATE_FILE$(LOOP%), 18%, 2%) + "/" + &
		MID(DATE_FILE$(LOOP%), 12%, 4%) &
		FOR LOOP% = DATE_FILE% TO 1% STEP -1%

110	!
	! Ask for the payroll folder date
	!
	TEMP$ = "Payroll Folder Dates"

	X% = ENTR_3CHOICE(SCOPE, "", "", REVERSE_LIST$(), &
		"", 0%, TEMP$, "", 0%)

	IF X% > 0%
	THEN
		BATCH_NO$ = RIGHT(REVERSE_LIST$(X%), 7%) + &
			LEFT(REVERSE_LIST$(X%), 2%) + &
			MID(REVERSE_LIST$(X%), 4%, 2%)
		GOTO 300
	END IF

	SELECT SCOPE::SCOPE_EXIT
		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
			GOTO ExitProgram

	END SELECT

	GOTO 110

	%Page

300	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.UPD"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Deduction folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.UPD"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Check folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.UPD"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

340	!

500	!
	! Ask for state to change post flag
	!

	!
	! Set help message
	!
	SCOPE::PRG_ITEM = "POST_STATE"

	!++
	! Abstract:POST_STATE
	!	^*New Status\*
	!	.p
	!	The ^*New Status\* entry is used to tell what status is to be
	!	forced into the folder.
	!	.p
	!	Valid values are the sum of the following:
	!	.b
	!	.TABLE 3,32
	!	.Te
	!	1	UPDATE
	!	.Te
	!	2	ACCRUAL
	!	.Te
	!	4	FINAL
	!	.eND TABLE
	!
	! Index:
	!	.x Set Flag>New Status
	!	.x New Status>Set Flag
	!
	!--
	STATE% = ENTR_3NUMBER(SCOPE,  SMG_SCREEN_DATA%, "", &
		"Enter the Post flag", 0.0, 0%, "#", &
		"")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 500
	END SELECT

	SCOPE::PRG_ITEM = "CONFIRM"

	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm Payroll SETFLAG process " + &
		" - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF


	!
	! Set help message
	!
	SCOPE::PRG_ITEM = "HELP"

1000	CALL ENTR_3MESSAGE(SCOPE, "Processing the pay file", 1%)
	WHEN ERROR IN
		RESET #PR_TRN_PAY.CH%, KEY #0%
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

1010	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 2000 IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	PR_TRN_PAY::UPDATE_FLAG = STATE%

	UPDATE #PR_TRN_PAY.CH%

	GOTO 1010

2000	CALL ENTR_3MESSAGE(SCOPE, "Processing the deduction file", 1%)
	WHEN ERROR IN
		RESET #PR_TRN_DED.CH%
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

2010	WHEN ERROR IN
		GET #PR_TRN_DED.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	PR_TRN_DED::UPDATE_FLAG = STATE%

	UPDATE #PR_TRN_DED.CH%

	GOTO 2010

3000	CALL ENTR_3MESSAGE(SCOPE, "Processing the check file", 1%)
	WHEN ERROR IN
		RESET #PR_TRN_CHECK.CH%
	USE
		CONTINUE 4000 IF ERR = 9%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

3010	WHEN ERROR IN
		GET #PR_TRN_CHECK.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 4000 IF ERR = 11%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	PR_TRN_CHECK::UPDATE_FLAG = STATE%

	UPDATE #PR_TRN_CHECK.CH%

	GOTO 3010

4000	!

 ExitProgram:
	CLOSE PR_TRN_CHECK.CH%, PR_TRN_DED.CH%, PR_TRN_PAY.CH%
	!
	! Exit to next program or menu
	!
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
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
