1	%TITLE "Batch Monitor Function"
	%SBTTL "UTL_FUNC_MTRBATCH"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_FUNC_MTRBATCH

	!
	! COPYRIGHT (C) 1989 BY
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
	!	This function will put up a screen containing all
	!	of the pertinent information in the UTL Batch Control file
	!	(including the Post Status), and update this information
	!	regularly (about every 10 seconds). Thus, a user will be
	!	able to monitor the progress of all processes that are
	!	assigned a batch number (including Posts and certain
	!	Special routines).
	!
	! Index:
	!	.x Bactch>Monitor
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_FUNC_MTRBATCH
	!	$ LIB FUNC_LIB:CMCFUN/REP UTL_FUNC_MTRBATCH
	!	$ DELETE UTL_FUNC_MTRBATCH.OBJ;*
	!
	! Author:
	!
	!	07/26/89 - Aaron Redd
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/20/95 - Kevin Handy
	!		Lose bad external definitions.
	!		Fix 1st parameter to entr_4entry and entr_4specialkeys.
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Change field 'PROGRAM' to 'PROGRAMNAME'
	!
	!	09/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	!++
	! Abstract:COMMAND
	!	^*BATCH/MONITOR\*
	!	.b
	!	.lm +5
	!	Displays information about the Batch Control file
	!	(including the Post Status), and update this information
	!	regularly (about every 10 seconds). Thus, a user will be
	!	able to monitor the progress of all processes that are
	!	assigned a batch number (including Posts and certain
	!	Special routines).
	!	.b
	!	^*Format: BATCH/MONITOR\*
	!	.b
	!	^*Example:
	!	.literal
	!	Menu Command Level: /BATCH/MONITOR
	!	.end literal
	!
	! Index:
	!	.x BATCH/MONITOR
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

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_BATCH_CONTROL.HB"
	MAP (UTL_BATCH_CONTROL)	UTL_BATCH_CONTROL_CDD	UTL_BATCH_CONTROL

	!
	! Declare variables and/or constants (including arrays)
	!
	DECLARE	LONG	SCR.ROW, OLD.TIMEOUT, OLD_OPTION, OLD_MESSAGE, &
			CUR_OPT, OLD_OPT
	DECLARE	STRING	TEMPTIM, TEMPDAT, PRNT_FLAG

	DECLARE	LONG	OPT_POS(5%)
	DECLARE	STRING	OPT_TEXT(5%)

	!
	! Initialize variables
	!
	OLD.TIMEOUT = SCOPE::SCOPE_TIMEOUT	! Store old timeout value
	SCOPE::SCOPE_TIMEOUT = 10%		! Reset timeout to 10 seconds
	PRNT_FLAG = "EXE"		! Monitor just executing processes
	CUR_OPT = 2%
	OLD_OPT = 2%

	OPT_POS(1%) = 12%
	OPT_POS(2%) = 26%
	OPT_POS(3%) = 46%
	OPT_POS(4%) = 69%
	OPT_POS(5%) = 74%
	OPT_TEXT(1%) = "All_processes"
	OPT_TEXT(2%) = "Executing_processes"
	OPT_TEXT(3%) = "Nonexecuting_processes"
	OPT_TEXT(4%) = "Help"
	OPT_TEXT(5%) = "eXit"

	!
	! Set up stuff for help
	!
	SCOPE::PRG_IDENT = "PROG"
	SCOPE::PRG_PROGRAM = "UTL_FUNC_MTRBATCH"

	!*******************************************************************
	! Initialize
	!*******************************************************************

	!
	! Open the UTL Batch Control File
	!
100	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_BATCH_CONTROL.OPN"
	USE
		CONTINUE 32000
	END WHEN

	!
	! Save the screen as it is at the moment
	!
	SMG_STATUS% = SMG$SAVE_PHYSICAL_SCREEN(SCOPE::SMG_PBID, SAVE_PBID%)

	!
	! Create a display, and label the border
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 130%, &
		MTR_DISPLAY%, SMG$M_BORDER)
	SMG_STATUS% = SMG$LABEL_BORDER(MTR_DISPLAY%, &
		" Batch Process Monitoring Utility ", SMG$K_TOP)

	!
	! Erase the screen so that the width change is hidden;
	! Paste the display onto the screen
	!
	SMG_STATUS% = SMG$ERASE_PASTEBOARD(SCOPE::SMG_PBID)
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 132%, 80%)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(MTR_DISPLAY%, &
		SCOPE::SMG_PBID, 2%, 2%)

	!
	! Save and recreate the OPTION virtual display
	!
	OLD_OPTION = SCOPE::SMG_OPTION
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
		SCOPE::SMG_PBID, 21%, 1%)

	!
	! Save and recreate the MESSAGE virtual display
	!
	OLD_MESSAGE = SCOPE::SMG_MESSAGE
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SCOPE::SMG_MESSAGE, &
		SCOPE::SMG_PBID, 23%, 1%)

	!
	! List options available
	!
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
		"COMMANDS:  All_processes " + &
		"Executing_processes Nonexecuting_processes Help eXit", 1%, 1%)

	!
	! Put the initial option in bold characters
	!
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, OPT_TEXT(CUR_OPT), 1%, &
		OPT_POS(CUR_OPT), , SMG$M_BOLD)

	%PAGE

 DrawScreen:
	!*******************************************************************
	! Draw the Batch Process Monitor Screen
	!*******************************************************************
	!
	! Put a Current Date/Time line and Heading on the screen
	!
	TEXT$ = "             Current Date:            Current Time:"
	HDR$ = "BatchNum  ProcessName" + SPACE$(30%) + "UserBatch  " + &
		"Start Date/Time   Stat  Description"

	SMG_STATUS% = SMG$PUT_CHARS(MTR_DISPLAY%, TEXT$, 2%, 29%)
	SMG_STATUS% = SMG$PUT_CHARS(MTR_DISPLAY%, HDR$, 4%, 14%)

 DrawInfo:
	!
	! Reset at the beginninng of the Batch Control File
	!
	RESET #UTL_BATCH_CONTROL.CH%
	SCR.ROW = 6%

	!
	! Get the (next) record
	!
200	WHEN ERROR IN
		GET #UTL_BATCH_CONTROL.CH%, REGARDLESS
	USE
		CONTINUE FinishScreen IF (ERR = 11%)
	END WHEN

	!
	! Draw the Batch Control Data
	!
	TEXT$ = UTL_BATCH_CONTROL::BATCH + "    " + &
		UTL_BATCH_CONTROL::PROGRAMNAME + " " + &
		UTL_BATCH_CONTROL::BFILE + "   " + &
		PRNT_DATE(UTL_BATCH_CONTROL::DSTART, 0%) + " " + &
		PRNT_TIME(UTL_BATCH_CONTROL::TSTART, 0%) + " " + &
		UTL_BATCH_CONTROL::USTATUS + "     " + &
		UTL_BATCH_CONTROL::DESCR

	!
	! Should we print out the line?
	!
	SELECT PRNT_FLAG
	!
	! Print out all lines
	!
	CASE "ALL"
		SMG_STATUS% = SMG$PUT_CHARS(MTR_DISPLAY%, TEXT$, &
			SCR.ROW, 14%, , SMG$M_BOLD)
		SCR.ROW = SCR.ROW + 1%

	!
	! Print out just lines pertaining to executing processes
	!
	CASE "EXE"
		IF (UTL_BATCH_CONTROL::USTATUS <> "") AND &
			(UTL_BATCH_CONTROL::USTATUS <> "N")
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(MTR_DISPLAY%, &
				TEXT$, SCR.ROW, 14%, , SMG$M_BOLD)
			SCR.ROW = SCR.ROW + 1%
		END IF

	!
	! List nonexecuting processes (interrupted, etc.)
	!
	CASE "NON"
		IF (UTL_BATCH_CONTROL::USTATUS = "") OR &
			(UTL_BATCH_CONTROL::USTATUS = "N")
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(MTR_DISPLAY%, &
				TEXT$, SCR.ROW, 14%, , SMG$M_BOLD)
			SCR.ROW = SCR.ROW + 1%
		END IF

	END SELECT

	!
	! Will the next line be too far down?
	!
	GOTO FinishScreen IF SCR.ROW = 19%

	!
	! Go back to get the next record
	!
	GOTO 200

 FinishScreen:
	!
	! Erase the rest of the display
	!
	SMG_STATUS% = SMG$ERASE_LINE(MTR_DISPLAY%, I%, 1%) &
			FOR I% = SCR.ROW to 18%

	!
	! Put on Current Date/Time
	!
	TEMPDAT = DATE_TODAY
	SMG_STATUS% = SMG$PUT_CHARS(MTR_DISPLAY%, PRNT_DATE(TEMPDAT, 0%), &
		2%, 56%, , SMG$M_BOLD)
	TEMPTIM = TIME_NOW
	SMG_STATUS% = SMG$PUT_CHARS(MTR_DISPLAY%, PRNT_TIME(TEMPTIM, 0%), &
		2%, 81%, , SMG$M_BOLD)

	%PAGE

 InputPause:
	!*******************************************************************
	! Pause and wait for timeout to expire in ENTR_ENTRY
	!*******************************************************************
	!
	! Ask for input
	!
	CHAR% = 0%
	WHILE CHAR% = 0%
		CHAR% = ENTR_4ENTRY(SCOPE, MTR_DISPLAY% BY VALUE, 8% BY VALUE)
		CHAR% = ENTR_4SPECIALKEYS(SCOPE, MTR_DISPLAY% BY VALUE, &
			8% BY VALUE, CHAR% BY VALUE)
	NEXT

	!
	! Reset old option value
	!
	OLD_OPT = CUR_OPT

	!
	! Check for special keys
	!
	SELECT CHAR%
	!
	! Timeout; update information
	!
	CASE SMG$K_TRM_TIMEOUT
		GOTO DrawInfo

	!
	! Right arrow (->), Down arrow
	!
	CASE SMG$K_TRM_RIGHT, SMG$K_TRM_DOWN
		CUR_OPT = CUR_OPT + 1%
		CUR_OPT = 1% IF (CUR_OPT = 6%)

	!
	! Left arrow (<-), Up arrow
	!
	CASE SMG$K_TRM_LEFT, SMG$K_TRM_UP
		CUR_OPT = CUR_OPT - 1%
		CUR_OPT = 5% IF (CUR_OPT = 0%)

	!
	! Return; choose current option
	!
	CASE SMG$K_TRM_CR

		SELECT CUR_OPT

		CASE 1%, 2%, 3%
			PRNT_FLAG = LEFT(EDIT$(OPT_TEXT(CUR_OPT), -1%), 3%)
			GOTO DrawInfo

		CASE 4%
			CALL HELP_3MESSAGE(SCOPE, "", "PROG", &
				SCOPE::PRG_PROGRAM, "HELP")

		CASE 5%
			GOTO ExitFunction

		END SELECT

	!
	! Exit will get you out
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitFunction

	!
	! Ignore all other special keys; check for characters
	!
	CASE ELSE
		CHAR$ = EDIT$(CHR$(CHAR%), -1%)

		SELECT CHAR$

		CASE "A", "N", "E"
			CUR_OPT = I% IF &
				CHAR$ = LEFT(OPT_TEXT(I%), 1%) &
					FOR I% = 1% to 3%
			PRNT_FLAG = LEFT(EDIT$(OPT_TEXT(CUR_OPT), -1%), 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
				OPT_TEXT(OLD_OPT), 1%, OPT_POS(OLD_OPT))

			SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
				OPT_TEXT(CUR_OPT), 1%, &
				OPT_POS(CUR_OPT), , SMG$M_BOLD)

			GOTO DrawInfo

		CASE "H"
			CUR_OPT = 4%
			SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
				OPT_TEXT(OLD_OPT), 1%, OPT_POS(OLD_OPT))

			SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
				"Help", 1%, 69%, , SMG$M_BOLD)

			CALL HELP_3MESSAGE(SCOPE, "", "PROG", &
				SCOPE::PRG_PROGRAM, "HELP")

			GOTO InputPause

		CASE "X"
			SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
				OPT_TEXT(OLD_OPT), 1%, OPT_POS(OLD_OPT))

			SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
				"eXit", 1%, 74%, , SMG$M_BOLD)

			GOTO ExitFunction

		END SELECT

	END SELECT

	!
	! Print out current option in bold characters
	!
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, OPT_TEXT(CUR_OPT), 1%, &
		OPT_POS(CUR_OPT), , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, OPT_TEXT(OLD_OPT), 1%, &
		OPT_POS(OLD_OPT)) IF (OLD_OPT <> CUR_OPT)

	GOTO InputPause

	%PAGE

 ExitFunction:
	!*******************************************************************
	! Exit this function (go back to MENU_3INTERRUPT(SCOPE)
	!*******************************************************************
	!
	! Erase Displays
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(MTR_DISPLAY%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Clear the monitor display off of the screen
	!
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
		SCOPE::SMG_PBID)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(MTR_DISPLAY%)
	SMG_STATUS% = SMG$ERASE_PASTEBOARD(SCOPE::SMG_PBID)
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 80%)

	!
	! Restore the OPTION and MESSAGE virtual display numbers
	!
	SCOPE::SMG_OPTION = OLD_OPTION
	SCOPE::SMG_MESSAGE = OLD_MESSAGE

	!
	! Restore the saved screen characteristics
	!
	SMG_STATUS% = SMG$RESTORE_PHYSICAL_SCREEN(SCOPE::SMG_PBID, SAVE_PBID%)

	!
	! Close the file
	!
	CLOSE #UTL_BATCH_CONTROL.CH%

	!
	! Restore old timeout value
	!
	SCOPE::SCOPE_TIMEOUT = OLD.TIMEOUT

	EXIT FUNCTION

	%PAGE

32000	!*******************************************************************
	! End of function UTL_FUNC_MTRBATCH
	!*******************************************************************
	END FUNCTION
