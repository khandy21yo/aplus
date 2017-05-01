1	%TITLE "INCHIS - Increment Historical Check Numbers"
	%SBTTL "PR_SPEC_INCREMENTCHECKHIS"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1988, 1989 BY
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
	!	The ^*Increment Historical Check Numbers\* routine is a special routine that
	!	can fix the check numbers (before any posting) when payroll
	!	checks have been run with the wrong starting check number.
	!	.lm -5
	!
	! Index:
	!	.x Increment>Check numbers
	!	.x Check numbers>Increment
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_INCREMENTCHECKHIS
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_INCREMENTCHECKHIS, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_INCREMENTCHECKHIS.OBJ;*
	!
	! Author:
	!
	!	06/01/89 - Aaron Redd
	!
	!	03/25/91 - Kevin Handy
	!
	! Modification history:
	!
	!	03/25/91 - Kevin Handy
	!		Taken from PR_SPEC_INCREMENTCHECK and modified
	!		to increment a history file and the general
	!		ledger check numbers.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	10/11/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.HB"
	MAP (PR_HIS_CHECK)	PR_HIS_CHECK_CDD	PR_HIS_CHECK

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	!
	! Declare variables
	!
	DECLARE STRING	FIRST.CHECK, LAST.CHECK

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initialize variables
	!
	FIRST.CHECK = "      "
	LAST.CHECK = "      "
	INCREMENT = 0
	YYYY_PP$ = "XXXX_XX"

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL READ_DEVICE("PR_HIS_CHECK", PR_HIS_CHECK.DEV$, STAT%)

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
	!	^*Check Journal Date\*
	!	.p
	!	The ^*Check Journal Date\* field refers to the date of the Check Journal
	!	which is desired to be reset.
	!
	! Index:
	!	.x Check Journal Date>Blank Check Numbers
	!	.x Blank Check Numbers>Check Journal Date
	!
	!--

	PR_HIS_CHECK_DATE$ = ENTR_3DATE(SCOPE,  SMG_SCREEN_DATA%, "", &
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

	PR_HIS_CHECK_DATE$ = EDIT$(PR_HIS_CHECK_DATE$, -1%)

	IF LEN(EDIT$(PR_HIS_CHECK_DATE$, -1%)) <> 8%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the journal date in (MMDDYYYY) format", &
			0%)
		GOTO 100
	END IF

	BATCH_NO$ = PR_HIS_CHECK_DATE$

190	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	%PAGE

	!
	! Draw the screen
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, &
		SMG_SCREEN_DATA%, SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		" Blanking Checks for Journal Dated " + BATCH_NO$ + " ", &
		SMG$K_TOP)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"From Check Number", 9%, 20%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"To Check Number", 11%, 20%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Increment by", 13%, 20%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"GL Period", 15%, 20%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 2%, 2%)

	!
	! Ask for beginning check number
	!
200	SCOPE::PRG_ITEM = "FLD02FCHK"

	!++
	! Abstract:FLD02FCHK"
	!	^*From Check Number\*
	!	.p
	!	The ^*From Check Number\* entered in this field will cause the check numbers
	!	to be revised from a particular number.
	!	.p
	!	A blank field will cause the revising to start with the first item in the file.
	!
	! Index:
	!	.x From Check Number>Increment Check Numbers
	!	.x Increment Check Numbers>From Check Number
	!
	!--

	FIRST.CHECK = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "9;40", &
		"", FIRST.CHECK, 32%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 200

	CASE SMG$K_TRM_DOWN
		! Go to other feild

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 200

	END SELECT

	!
	! Ask for ending check number
	!
250	SCOPE::PRG_ITEM = "FLD03TCHK"

	!++
	! Abstract:FLD03TCHK"
	!	^*To Check Number\*
	!	.p
	!	The ^*To Check Number\* entered in this field will cause the revising of the
	!	check numbers to end with a particular item.
	!	.p
	!	A blank field causes the revising to end with the last check number.
	!
	! Index:
	!	.x To Check Number>Increment Check Numbers
	!	.x Increment Check Numbers>To Check Number
	!
	!--

	LAST.CHECK = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "11;40", &
		"", LAST.CHECK, 32%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 200

	CASE SMG$K_TRM_DOWN
		! Go to other feild

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 250

	END SELECT

260	SCOPE::PRG_ITEM = "FLD04INC"

	!++
	! Abstract:FLD04INC"
	!	^*Increment By\*
	!	.p
	!	The ^*Increment By\* field allows for entry of the check incremental number.
	!	Any number may be designated by the user.
	!
	! Index:
	!	.x Increment By>Increment Check Numbers
	!	.x Increment Check Numbers>Increment By
	!
	!--

	INCREMENT = ENTR_3NUMBER(SCOPE,  SMG_SCREEN_DATA%, "13;40", &
		"", INCREMENT, 0%, "######", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 250

	CASE SMG$K_TRM_DOWN
		! Go to other feild
		GOTO 270

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 250

	END SELECT


270	SCOPE::PRG_ITEM = "FLD05GLP"

	!++
	! Abstract:FLD05GLP"
	!	^*GL Period\*
	!	.p
	!	General Ledger Period where the check numbers have
	!	been posted.
	!
	! Index:
	!	.x GL Period>Increment Check Numbers
	!	.x Increment Check Numbers>GL Period
	!
	!--

	YYYY_PP$ = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "15;40", &
		"", YYYY_PP$, 0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 260

	CASE SMG$K_TRM_DOWN
		! Go to other feild
		GOTO 270

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 250

	END SELECT



	!***************************************************************
	! Open all of the files
	!***************************************************************

	!
	! Open Payroll Check information file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.UPD"
	USE
		FILENAME$ = "PR_HIS_CHECK"
		CONTINUE NoFile
	END WHEN

	!***************************************************************
	! Begin actually doing something
	!***************************************************************

310	!
	! Open GL period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.UPD"
	USE
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE NoFile
	END WHEN

	%PAGE

1000	!*******************************************************************
	! Increment GL file
	!*******************************************************************

	RESET #GL_YYYY_PP.CH%, KEY #0%

1010	!
	! Pull in next check number
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%
	USE
		CONTINUE 2000 IF ERR = 11%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

	GOTO 1010 IF (GL_YYYY_PP::CKNO > LAST.CHECK) OR &
		(GL_YYYY_PP::CKNO < FIRST.CHECK)

1020	!
	! Make sure it is a PR check
	!
	WHEN ERROR IN
		GET #PR_HIS_CHECK.CH%, KEY #1% EQ GL_YYYY_PP::CKNO, REGARDLESS
	USE
		CONTINUE 1010 IF ERR = 155%
		FILENAME$ = "PR_HIS_CHECK"
		CONTINUE HelpError
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "GL Check " + GL_YYYY_PP::CKNO, 1%)

1030	!
	! Increment check number
	!
	GL_YYYY_PP::CKNO = NUM1$(VAL(GL_YYYY_PP::CKNO) + INCREMENT)

	WHEN ERROR IN
		UPDATE #GL_YYYY_PP.CH%
	USE
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

	GOTO 1010

	%PAGE

	!*******************************************************************
	! Process Payroll Checks
	!*******************************************************************

	!
	! Get the first check to be blanked
	!
2000	WHEN ERROR IN
		RESET #PR_HIS_CHECK.CH%, KEY #0%
	USE
		FILENAME$ = "PR_HIS_CHECK"
		CONTINUE HelpError
	END WHEN

2010	WHEN ERROR IN
		GET #PR_HIS_CHECK.CH%
	USE
		CONTINUE ExitProgram IF (ERR = 11%)
		FILENAME$ = "PR_HIS_CHECK"
		CONTINUE NoFile
	END WHEN

	!
	! See if we've reached the end of the checks to blank
	!
2020	GOTO 2010 IF (PR_HIS_CHECK::CHECK > LAST.CHECK) OR &
		(PR_HIS_CHECK::CHECK < FIRST.CHECK)

	CALL ENTR_3MESSAGE(SCOPE, "PR Check " + PR_HIS_CHECK::CHECK, 1%)

	!
	! Change the check
	!
	PR_HIS_CHECK::CHECK = NUM1$(VAL(PR_HIS_CHECK::CHECK) + INCREMENT)

	!
	! Replace the check record
	!
	WHEN ERROR IN
		UPDATE #PR_HIS_CHECK.CH%
	USE
		FILENAME$ = "PR_HIS_CHECK"
		CONTINUE HelpError
	END WHEN

	!
	! Go up to examine the record (and blank it if necessary)
	!
	GOTO 2010

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	!
	! Close the file
	!
	CLOSE PR_HIS_CHECK.CH%

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
	RESUME 19990

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
