1	%TITLE "Timeout Setup"
	%SBTTL "UTL_MAST_TIMEOUT"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
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
	!	.P
	!	^*Time out\* maintains the length of time a maintenance program will
	!	^*sit\* with a record locked without user input.
	!	.P
	!	When one user has a record locked (which occurs in the change
	!	and blank options), no other user may read that record.  By limiting
	!	the amount of time a user may lock a record, one user may not lock a
	!	record, leaving other users to wait for it to free.
	!	.P
	!	A value of zero (*0) causes it to wait indefinitely. Timeout is
	!	measured in seconds.  If the user wishes to timeout after 30 seconds,
	!	the timeout value would be 30.  If the user wishes to timeout after 2
	!	minutes, the timeout value would be 120.
	!	.P
	!	After the new timeout value has been entered, exit this
	!	process by pressing the exit key.  No other options or commands are
	!	available in this program.
	!
	! Index:
	!	.x Utility>Time Out
	!	.x Time Out
	!	.x Set Time Out
	!	.x Time Out>Maintenance
	!	.x Maintain>Time Out
	!
	! Option:
	!
	!	UTL_MAIN_TIMEOUT$HELP
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAST_TIMEOUT/LINE
	!	$ LINK/EXECUTABLE=UTL_EXE: UTL_MAST_TIMEOUT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_MAST_TIMEOUT.OBJ;*
	!
	! Author:
	!
	!	09/11/87 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/19/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Change field 'PROGRAM' to 'PROGRAMNAME'
	!
	!	11/28/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!++
	! Abstract:COMMAND
	!	^*TIMEOUT\*
	!	.P
	!	^*Time out\* maintains the length of time a maintenance program will
	!	^*sit\* with a record locked without user input.
	!	.P
	!	When one user has a record locked (which occurs in the change
	!	and blank options), no other user may read that record. By limiting
	!	the amount of time a user may lock a record, one user may not lock a
	!	record, leaving other users to wait for it to free.
	!	.P
	!	A value of zero (*0) causes it to wait indefinitely. Timeout is
	!	measured in seconds. If the user wishes to timeout after 30 seconds,
	!	the timeout value would be 30. If the user wishes to timeout after 2
	!	minutes, the timeout value would be 120.
	!	.P
	!	After the new timeout value has been entered, exit this
	!	process by pressing the exit key. No other options or commands are
	!	available in this program.
	!	.P
	!	^*Format: TIMEOUT\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level> /TIMEOUT
	!	.end literal
	!
	! Index:
	!	.x TIMEOUT
	!	.x Utility>Time Out
	!	.x Time Out
	!	.x Set Time Out
	!	.x Time Out>Maintenance
	!	.x Maintain>Time Out
	!
	! Option:
	!
	!	UTL_MAIN_TIMEOUT$HELP
	!
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map stuff
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.HB"
	MAP (UTL_SET)		UTL_SET_CDD	UTL_SET

	COM (CH_UTL_SET) &
		UTL_SET.CH%

	!
	! External functions
	!
	EXTERNAL STRING  FUNCTION READ_SET

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize the keyboard
	!
	CALL READ_INITIALIZE

200	!
	! Open the set file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.CRE"
	USE
		FILENAME$ = "UTL_SET"
		CONTINUE HelpError
	END WHEN

400	!
	! Set up for title and such
	!
	SCOPE::PRG_ITEM	= "TIMEOUT"

	TEMP1$ = TRM$(SCOPE::PRG_COMPANY)
	TOP_TITLE$ = " Timeout Maintainence "
	TOP_TITLE$ = TOP_TITLE$ + "for " + TEMP1$ + " " IF TEMP1$ <> ""

	!
	! Create display for this file
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		TIME_WINDOW%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER(TIME_WINDOW%, LEFT(TOP_TITLE$, 78%))

	SMG_STATUS% = SMG$PUT_CHARS(TIME_WINDOW%, "Timeout Value", 7%, 33%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		TIME_WINDOW%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

1000	!
	! Display the current value
	!
	WHEN ERROR IN
		TIMEOUT% = VAL%(READ_SET("TIME", "OUT"))
	USE
		TIMEOUT% = 0%
	END WHEN

1100	!
	! Ask for a changed value
	!
	NEW_TIMEOUT$ = NUM1$(ENTR_3NUMBER(SCOPE, TIME_WINDOW%, &
		"9;38", "New timeout value", &
		TIMEOUT% * 1.0, 0%, "#####", ""))

	SELECT SCOPE::SCOPE_EXIT

	!
	! Please change value
	!
	CASE SMG$K_TRM_DO, 10%, 12%, 13%

		GOTO 1200

	!
	! Let me out of here
	!
	CASE SMG$K_TRM_F8, SMG$K_TRM_F10

		GOTO ExitProgram

	!
	! Randomly banged on the keys
	!
	CASE ELSE

		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)

	END SELECT

	GOTO 1000

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

1200	!*******************************************************************
	! Update the record if it exists, create it if it doesnt.
	!*******************************************************************

	UTL_SET::PROGRAMNAME = "TIME"
	UTL_SET::ITEM = "OUT"

	WHEN ERROR IN
		GET #UTL_SET.CH%, &
			KEY #0% EQ UTL_SET::PROGRAMNAME + UTL_SET::ITEM

		UTL_SET::SDATA = EDIT$(NEW_TIMEOUT$, -1%)

		UPDATE #UTL_SET.CH%
	USE
		CONTINUE 1400 IF ERR = 155%
		FILENAME$ = "UTL_SET"
		CONTINUE HelpError
	END WHEN

	GOTO 1000

1400	!
	! Trap non existant error to here, add the record
	!
	UTL_SET::HARD = ""
	UTL_SET::SDATA = EDIT$(NEW_TIMEOUT$, -1%)

	PUT #UTL_SET.CH%

	GOTO 1000

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
