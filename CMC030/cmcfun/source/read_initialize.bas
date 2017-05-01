1	%TITLE "Maintenance Initilization"
	%SBTTL "READ_INITIALIZE"
	%IDENT "V3.6a Calico"

	SUB READ_INITIALIZE

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This subroutine initializes the maintenance programs.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	This subroutine initializes the maintainence programs.
	!
	! Example:
	!
	!	CALL READ_INITIALIZE
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:READ_INITIALIZE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP READ_INITIALIZE
	!	$ DELETE READ_INITIALIZE.OBJ;*
	!
	! Author:
	!
	!	02/23/87 - Kevin Handy
	!
	! Modification history:
	!
	!	05/31/90 - Frank F. Starman
	!		Initialize all three variables for help key.
	!
	!	02/21/92 - Kevin Handy
	!		Inserted KEYBOARD.OPN into source instead
	!		of "%include"ing it.  This is the only place
	!		it is now used.
	!
	!	02/21/92 - Kevin Handy
	!		Modified to change terminal to harwware formfeed,
	!		so that formfeeds can pass through printer port.
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	02/27/92 - Kevin Handy
	!		Modified to turn off WRAP on terminal.
	!
	!	04/03/92 - Frank F. Starman
	!		Modify use of the READ_35SET function.
	!
	!	04/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/29/92 - Kevin Handy
	!		Changed map for UTL_SET_READ to a define.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	09/15/97 - Kevin Handy
	!		Lose commented out code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Send SCOPE as LOC(SCOPE) to AST routine.
	!
	!	04/13/99 - Kevin Handy
	!		Use BASIC$STARLET for TT$
	!
	!	06/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "$TTDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Maps
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE) UTL_PROFILE_CDD UTL_PROFILE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.HB"
	DECLARE UTL_SET_CDD UTL_SET_READ

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION READ_35SET
	EXTERNAL LONG READ_3BROADCAST

	%PAGE

	!
	! Get the program name
	!
	SCOPE::PRG_PROGRAM = READ_SYSPN
	SCOPE::PRG_IDENT = "H"
	SCOPE::PRG_ITEM = "HELP"

60	!******************************************************************
	! Keyboard open routine
	!******************************************************************

	SCOPE::SCREEN_WIDTH = 80% IF SCOPE::SCREEN_WIDTH < 80%
	SCOPE::SCREEN_WIDTH = 132% IF SCOPE::SCREEN_WIDTH > 80%

	!
	! Create the pasteboard
	!
	SMG_STATUS% = SMG$CREATE_PASTEBOARD(SCOPE::SMG_PBID, , , &
		SMG_ROWS%, SMG_COLS%)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Change width if necessary
	!
	IF SCOPE::SCREEN_WIDTH <> SMG_COLS%
	THEN
		SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, &
			SCOPE::SCREEN_WIDTH, SMG_COLS%,, SMG_ROWS%)
	END IF

	!
	! Create the Message display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		2%,			! 2 Rows &
		132%,			! Columns &
		SCOPE::SMG_MESSAGE,	! Identifier &
		,			! No border &
		,			! No attributes &
					! Default character set &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SCOPE::SMG_MESSAGE,	! Message display &
		SCOPE::SMG_PBID,	! Pasteboard &
		23%,			! Row to start in &
		1%,			! Column to start in &
					! Don't need top-disp &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Create the option display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		2%,			! 2 Rows &
		132%,			! Columns &
		SCOPE::SMG_OPTION,	! Identifier &
		,			! No border &
		,			! No attributes &
					! Default character set &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SCOPE::SMG_OPTION,	! Option Display &
		SCOPE::SMG_PBID,	! Pasteboard &
		21%,			! Row to start in &
		1%,			! Column to start in &
					! Don't need top-disp &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%


	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_KEYBOARD(SCOPE::SMG_KBID)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Remove the cursor
	!
	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Set broadcast trapping
	!
	! (READ_BROADCAST is actually a function, but...)
	!
	SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
		LOC(READ_3BROADCAST), LOC(SCOPE))

	!
	! Set up hardware form feed so reports will page correctly
	! through printer port.
	!
	I% = SMG$SET_TERM_CHARACTERISTICS(SCOPE::SMG_PBID, &
		TT$M_MECHFORM, 0%, TT$M_WRAP, 0%)

100	!
	! Load in company name
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
		GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
		CLOSE #UTL_PROFILE.CH%
		CALL ASSG_FREECHANNEL(UTL_PROFILE.CH%)
	USE
		UTL_PROFILE::MENU_NAME = ""
		UTL_PROFILE::REP_NAME = ""
		UTL_PROFILE::MAINLOCATION = ""
		UTL_PROFILE::DEFLOCATION = ""
	END WHEN

200	SCOPE::PRG_COMPANY = UTL_PROFILE::MENU_NAME

	!
	! Turn on timeout if desired
	!
	V% = READ_35SET("TIME", "OUT", UTL_SET_READ)
	JUNK$ = EDIT$(UTL_SET_READ::SDATA, 2% + 4%)
	SCOPE::SCOPE_TIMEOUT = VAL%(JUNK$) IF JUNK$ <> ""

32767	END SUB
