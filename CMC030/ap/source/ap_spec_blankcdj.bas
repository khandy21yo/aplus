1	%TITLE "Blank Out a Series of Checks by Number"
	%SBTTL "AP_SPEC_BLANKCDJ"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	!	The ^*Blank Check Numbers\* routine can be used to reset all
	!	check numbers to blank so they can be re-printed.
	!	.b
	!	The same effect can be accomplished by going into the
	!	detail maintenance, but if a large range of numbers needs to be
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
	!	$ BAS AP_SOURCE:AP_SPEC_BLANKCDJ
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_SPEC_BLANKCDJ, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_BLANKCDJ.OBJ;*
	!
	! Author:
	!
	!	05/30/89 - Kevin Handy
	!
	!	11/05/91 - Kevin Handy
	!		Rewrote based on PR_SPEC_BLANKCHECK so users
	!		could use instead of being a special program
	!		that only I could use.
	!
	! Modification history:
	!
	!	02/11/93 - Kevin Handy
	!		Modified to go through entire file, instead of
	!		going in check number order, because that caused
	!		many wierd things to happen when it saw the
	!		same check number over and over and over.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/04/96 - Kevin Handy
	!		Reformat source code.
	!		Added batch number to AP_CDJ.
	!		Change input flag from 32% to 0%
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	09/26/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.HB"
	MAP (AP_CDJ)	AP_CDJ_CDD	AP_CDJ

	!
	! Declare variables
	!
	DECLARE STRING	FIRST_CHECK, LAST_CHECK

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initialize variables
	!
	CDJ_BATCH$ = "01"
	FIRST_CHECK = "      "
	LAST_CHECK = "      "

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

190	%PAGE

	!
	! Draw the screen
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, &
		SMG_SCREEN_DATA%, SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		" Blanking Checks for AP Cash Disp.", SMG$K_TOP)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Batch Number", 7%, 20%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"From Check Number", 9%, 20%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"To Check Number", 11%, 20%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 2%, 2%)

	!
	! Ask for beginning check number
	!
195	SCOPE::PRG_ITEM = "FLD01BTH"

	!++
	! Abstract:FLD01BTH
	!	^*Batch Number\*
	!	.b
	!	This field specifies which batch to
	!	blank the check numbers in.
	!
	! Index:
	!	.x Batch Number>Blank Check Numbers
	!	.x Blank Check Numbers>Batch Number
	!
	!--

	CDJ_BATCH$ = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "7;40", &
		"", CDJ_BATCH$, 0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 195

	CASE SMG$K_TRM_DOWN
		! Go to next field

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 200

	END SELECT

	!
	! Ask for beginning check number
	!
200	SCOPE::PRG_ITEM = "FLD02FCHK"

	!++
	! Abstract:FLD02FCHK
	!	^*From Check Number\*
	!	.b
	!	.lm +5
	!	The ^*From Check Number\* entered in this field will cause the resetting
	!	of the check numbers to begin with a particular check number.
	!	.b
	!	A blank field will cause the resetting to start with the first check in the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x From Check Number>Blank Check Numbers
	!	.x Blank Check Numbers>From Check Number
	!
	!--

	FIRST_CHECK = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "9;40", &
		"", FIRST_CHECK, 0%, "'E", "")

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
250	SCOPE::PRG_ITEM = "FLD03TCHK"

	!++
	! Abstract:FLD03TCHK
	!	^*To Check Number\*
	!	.b
	!	.lm +5
	!	The ^*To Check Number\* entered in this field will cause the resetting of
	!	the checks to end with a particular check number.
	!	.b
	!	A blank field causes the resetting to end with the last check number in the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x To Check Number>Blank Check Numbers
	!	.x Blank Check Numbers>To Check Number
	!
	!--
	LAST_CHECK = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "11;40", &
		"", LAST_CHECK, 0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		! Go to other feild
		GOTO 200

	CASE SMG$K_TRM_DOWN
		! Go to other feild
		GOTO 250

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
		%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.UPD"
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	!***************************************************************
	! Begin actually doing something
	!***************************************************************

	!
	! Get the first check to be blanked
	!
500	RESET #AP_CDJ.CH%
	GET #AP_CDJ.CH%

	!
	! See if we've reached the end of the checks to blank
	!
550	GOTO 600 IF (AP_CDJ::CKNUM > LAST_CHECK) OR &
		(AP_CDJ::CKNUM < FIRST_CHECK)

	!
	! Blank the check
	!
	AP_CDJ::CKNUM = ""
	AP_CDJ::CKDAT = ""

	!
	! Replace the check record
	!
	WHEN ERROR IN
		UPDATE #AP_CDJ.CH%
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	!
	! Get the next check
	!
600	WHEN ERROR IN
		GET #AP_CDJ.CH%
	USE
		CONTINUE ExitProgram IF (ERR = 11%)
		FILENAME$ = "AP_CDJ"
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
	CLOSE AP_CDJ.CH%

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
