1	%TITLE "BLANK - Blank Checks Numbers"
	%SBTTL "PR_SPEC_ZORCHCHECK"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 2003 BY
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
	!	The ^*Blank Check Numbers\* routine can be used to reset all
	!	check numbers to blank, so they can be re-printed.
	!	.b
	!	The same effect can be accomplished by going into the
	!	detail maintenance, but if a large range of checks are to be
	!	blanked, this routine will be faster.
	!	.lm -5
	!
	! Index:
	!	.x Check numbers>Blank
	!	.x Blank>Check numbers
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_ZORCHCHECK
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_ZORCHCHECK, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_ZORCHCHECK.OBJ;*
	!
	! Author:
	!
	!	03/20/2003 - Kevin Handy
	!
	! Modification history:
	!
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

	!
	! Declare variables
	!
	DECLARE STRING	FIRST_CHECK, LAST_CHECK, DUMMY_EMP

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initialize variables
	!
	FIRST_CHECK = "      "
	LAST_CHECK = "      "
	DUMMY_EMP = "          "

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

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

	BATCH_NO$ = DATE_TODAY

100	SCOPE::PRG_ITEM = "FLD01CDATE"

	!++
	! Abstract:FLD01CDATE
	!	.ts 55
	!	^*Check Journal Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Check Journal Date\* field refers to the date of the Check Journal
	!	which is to be reset.
	!	.lm -5
	!
	! Index:
	!	.x Check Journal Date>Blank Check Numbers
	!	.x Blank Check Numbers>Check Journal Date
	!
	!--

	PR_TRN_CHECK_DATE$ = ENTR_3DATE(SCOPE,  SMG_SCREEN_DATA%, "", &
		"Enter Check Journal Date (MMDDYYYY)", BATCH_NO$, 64%, "8", "")

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
			"Please enter the journal date in (MMDDYYYY) format", &
			0%)
		GOTO 100
	END IF

	BATCH_NO$ = PR_TRN_CHECK_DATE$

190	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	%PAGE

	!
	! Draw the screen
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, &
		SMG_SCREEN_DATA%, SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		" Removing Empty Checks for Journal Dated " + BATCH_NO$ + " ", &
		SMG$K_TOP)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 2%, 2%)

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
		CONTINUE HelpError
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
		CONTINUE ExitProgram IF (ERR = 155%)
		CONTINUE HelpError
	END WHEN

	!
	! See if we've reached the end of the checks to blank
	!

550	GOTO 600 IF (PR_TRN_CHECK::CHECK <> "      ")

	!
	! Replace the check record
	!
	WHEN ERROR IN
		DELETE #PR_TRN_CHECK.CH%
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	!
	! Get the next check
	!
600	WHEN ERROR IN
		GET #PR_TRN_CHECK.CH%
	USE
		CONTINUE ExitProgram IF (ERR = 11%)
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	!
	! Go up to examine the record (and blank it if necessary)
	!
	GOTO 550

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	!
	! Close the file
	!
	CLOSE PR_TRN_CHECK.CH%

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

	END
