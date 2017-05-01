1	%TITLE "Blank Out a Series of Checks by Number"
	%SBTTL "WP_SPEC_BLANKREQJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1997 BY
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
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
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
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_SPEC_BLANKREQJOUR
	!	$ LINK/EXECUTABLE=WP_EXE:*.EXE WP_SPEC_BLANKREQJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_SPEC_BLANKREQJOUR.OBJ;*
	!
	! Author:
	!
	!	04/01/97 - Kevin Handy
	!		Based upon AP_SPEC_BLANKCDJ
	!
	! Modification history:
	!
	!	05/19/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE
	!
	!	04/26/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.HB"
	MAP (WP_REQLINE)	WP_REQLINE_CDD	WP_REQLINE

	!
	! Declare variables
	!
	DECLARE STRING	FIRST_REQNUM, LAST_REQNUM

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initialize variables
	!
	BATCH_NO$ = "01      "
	FIRST_REQNUM = "          "
	LAST_REQNUM = "          "

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

190	!
	! Draw the screen
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, &
		SMG_SCREEN_DATA%, SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		" Blanking Req Numbers for WP Req Journal", SMG$K_TOP)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Batch Number", 7%, 20%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"From Req Number", 9%, 20%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"To Req Number", 11%, 20%)

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

	BATCH_NO$ = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "7;40", &
		"", BATCH_NO$, 0%, "'E", "")

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
200	SCOPE::PRG_ITEM = "FLD02FREQ"

	!++
	! Abstract:FLD02FREQ
	!	^*From Req Number\*
	!
	! Index:
	!
	!--

	FIRST_REQNUM = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "9;40", &
		"", FIRST_REQNUM, 0%, "'E", "")

	RSET FIRST_REQNUM = TRM$(FIRST_REQNUM)

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
250	SCOPE::PRG_ITEM = "FLD03TREQ"

	!++
	! Abstract:FLD03TREQ
	!	^*To Check Number\*
	!
	! Index:
	!
	!--
	LAST_REQNUM = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "11;40", &
		"", LAST_REQNUM, 0%, "'E", "")

	RSET LAST_REQNUM = TRM$(LAST_REQNUM)

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
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.UPD"
	USE
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

	!***************************************************************
	! Begin actually doing something
	!***************************************************************

	!
	! Get the first check to be blanked
	!
500	RESET #WP_REQLINE.CH%
	GET #WP_REQLINE.CH%

	!
	! See if we've reached the end of the checks to blank
	!
550	GOTO 600 IF (WP_REQLINE::REQNUM > LAST_REQNUM) OR &
		(WP_REQLINE::REQNUM < FIRST_REQNUM)

	!
	! Blank the check
	!
	WP_REQLINE::REQNUM = ""

	!
	! Replace the check record
	!
	WHEN ERROR IN
		UPDATE #WP_REQLINE.CH%
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Get the next check
	!
600	WHEN ERROR IN
		GET #WP_REQLINE.CH%
	USE
		CONTINUE ExitProgram IF (ERR = 11%)
		FILENAME$ = "WP_REQLINE"
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
	CLOSE WP_REQLINE.CH%

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
	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	END
