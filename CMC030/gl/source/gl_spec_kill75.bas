1	%TITLE "INCRE - Increment Check Numbers"
	%SBTTL "GL_SPEC_KILL75"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2003 BY
	! Software Solutions, Inc.
	! Idaho Falls, Idaho
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
	!	Kill all of the information except/for gl account *.75
	!	. lm -5
	!
	! Index:
	!	.x Increment Check numbers
	!	.x Check numbers>Increment
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_KILL75
	!	$ LINK/EXECUTABLE=GL_EXE:*.EXE GL_SPEC_KILL75, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_KILL75.OBJ;*
	!
	! Author:
	!
	!	08/27/2003 - Kevin Handy
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initialize variables
	!
	PATTERN$ = ".75"
	KEEP$ = "N"

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Ask for the date for the Check Journal
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

	YYYY_PPX$ = LEFT(DATE_TODAY, 6%)
	YYYY_PP$ = LEFT(YYYY_PPX$, 4%) + "_" + RIGHT(YYYY_PPX$, 5%)

100	SCOPE::PRG_ITEM = "FLD01CDATE"

	!++
	! Abstract:FLD01CDATE
	!	^*GL Period\*
	!	.p
	!	The general ledger period to process
	!
	! Index:
	!	.x GL Period>Blank Check Numbers
	!	.x Blank Check Numbers>GL Period
	!
	!--

	GL_FOLDER_PERIOD$ = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "", &
		"Enter GL PERIOD (YYYYPP)", YYYY_PPX$, 64%, "8", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 100

	END SELECT

	GL_FOLDER_PERIOD$ = EDIT$(GL_FOLDER_PERIOD$, -1%)

	IF LEN(EDIT$(GL_FOLDER_PERIOD$, -1%)) <> 6%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please enter the PERIOD in " + &
			"(YYYYPP) format", 0%)
		GOTO 100
	END IF

	YYYY_PPX$ = GL_FOLDER_PERIOD$
	YYYY_PP$ = LEFT(YYYY_PPX$, 4%) + "_" + RIGHT(YYYY_PPX$, 5%)

190	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	%PAGE

	!
	! Draw the screen
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, &
		SMG_SCREEN_DATA%, SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		" Blanking Checks for GL Period " + YYYY_PP$ + " ", &
		SMG$K_TOP)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Keep(Y) or kill(N)", 7%, 20%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Search string", 9%, 20%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 2%, 2%)

	!
	! Ask for beginning check number
	!
195	SCOPE::PRG_ITEM = "FLD02BTCH"

	!++
	! Abstract:FLD02BTCH"
	!	^*Batch Number\*
	!	.p
	!	The ^*Batch Number\* entered in this field will cause the check numbers
	!	to be revised from a particular number.
	!	.p
	!	A blank field will cause the revising to start with the first item in the file.
	!
	! Index:
	!	.x Batch Number>Increment Check Numbers
	!	.x Increment Check Numbers>Batch Number
	!
	!--

	KEEP$ = ENTR_3YESNO(SCOPE,  SMG_SCREEN_DATA%, "7;40", &
		"", KEEP$, 32%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 195

	CASE SMG$K_TRM_DOWN
		! Go to other feild

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 195

	END SELECT

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

	PATTERN$ = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "9;40", &
		"", PATTERN$, 32%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 195

	CASE SMG$K_TRM_DOWN
		! Go to other feild

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 200

	END SELECT

	!
	! Ask for ending check number
	!

	!***************************************************************
	! Open all of the files
	!***************************************************************

	!
	! Open Payroll Check information file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.UPD"
	USE
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE NoFile
	END WHEN

	!***************************************************************
	! Begin actually doing something
	!***************************************************************

	!
	! Get the first check to be blanked
	!
500	RESET #GL_YYYY_PP.CH%
	GET #GL_YYYY_PP.CH%

	!
	! See if we've reached the end of the checks to blank
	!
550
	!
	! Change the check
	!
	IF INSTR(1%, GL_YYYY_PP::ACCT, PATTERN$) <> 0%
	THEN
		IF KEEP$ = "Y"
		THEN
			DELETE #GL_YYYY_PP.CH%
		END IF
	ELSE
		IF KEEP$ = "N"
		THEN
			DELETE #GL_YYYY_PP.CH%
		END IF
	END IF

	!
	! Get the next check
	!
600	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%
	USE
		CONTINUE ExitProgram IF (ERR = 11%)
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE NoFile
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
	CLOSE GL_YYYY_PP.CH%

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
