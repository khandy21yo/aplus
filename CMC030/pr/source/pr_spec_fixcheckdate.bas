1	%TITLE "Fix Payroll End Date in Folder."
	%SBTTL "PR_SPEC_FIXCHECKDATE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1996 BY
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
	! or reliability of its software on equipment which is not =
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.p
	!	This program is q quick and dirty to fix the check date
	!	in a payroll folder for when checks were printed with
	!	the wrong date
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_FIXCHECKDATE
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_FIXCHECKDATE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_FIXCHECKDATE.OBJ;*
	!
	! Author:
	!
	!	01/25/96 - Kevin Handy
	!		Based upon PR_SPEC_FIXDATE. (LL)
	!
	! Modification history:
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	10/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Define options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL READ_DEVICE("PR_TRN_CHECK", PR_TRN_CHECK.DEV$, STAT%)

	!
	! Ask for the date for the Check Journal
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY( &
		SMG_SCREEN_DATA%, SCOPE::SMG_PBID, 1%, 1%)

	BATCH_NO$ = DATE_TODAY

100	PR_TRN_CHECK_DATE$ = ENTR_3DATE(SCOPE, SMG_SCREEN_DATA%, "", &
		"Enter Journal Date (MMDDYYYY)", BATCH_NO$, 64%, "8", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 100

	END SELECT

	PR_TRN_CHECK_DATE$ = EDIT$(PR_TRN_CHECK_DATE$, -1%)

	IF LEN(EDIT$(PR_TRN_CHECK_DATE$, -1%)) <> 8%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the journal date in (MMDDYYYY) format", 0%)
		GOTO 100
	END IF

	BATCH_NO$ = PR_TRN_CHECK_DATE$

	END_DATE$ = ENTR_3DATE(SCOPE,  SMG_SCREEN_DATA%, "", &
		"Enter Date For Checks (MMDDYYYY)", &
		PR_TRN_CHECK_DATE$, 64%, "8", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 100

	END SELECT

190	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	%PAGE

	!***************************************************************
	! Open all of the files
	!***************************************************************

	!
	! Open Payroll Check information file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.UPD"
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE NoFile
	END WHEN

	!***************************************************************
	! Begin actually doing something
	!***************************************************************

	!
	! Get the first check to be blanked
	!
500	WHEN ERROR IN
		RESET #PR_TRN_CHECK.CH%
		GET #PR_TRN_CHECK.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

550	!
	! Set the date
	!
	IF PR_TRN_CHECK::CHECK_DATE <> END_DATE$
	THEN
		PR_TRN_CHECK::CHECK_DATE = END_DATE$

		!
		! Replace the check record
		!
		UPDATE #PR_TRN_CHECK.CH%

		CALL ENTR_3MESSAGE(SCOPE, PR_TRN_CHECK::EMPNUM, 1%)

	END IF

	!
	! Get the next check
	!
600	WHEN ERROR IN
		GET #PR_TRN_CHECK.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 700 IF (ERR = 11%)
		FILENAME$ = "PR_TRN_FIXCHECKDATE"
		CONTINUE NoFile
	END WHEN

	!
	! Go up to examine the record (and blank it if necessary)
	!
	GOTO 550

	%PAGE

700	!

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

 HelpError:
19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	!
	! Handle the cases where a file couldn't be opened
	!
 NoFile:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	END
