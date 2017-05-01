1	%TITLE "Give Help on Keyboard"
	%SBTTL "HELP_KEYBOARD"
	%IDENT "V3.6a Calico"

	SUB HELP_KEYBOARD(DEF_DIR$, LIB_NAME$, KEYBOARD$)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	!	.p
	! Parameters:
	!
	!	DEF_DIR$
	!		The passed string that holds the directory.
	!
	!	LIB_NAME$
	!		The passed string that holds the terminal library.
	!
	!	KEYBOARD$
	!		The passed string that holds the keyboard name.
	!
	!
	!	Returned value
	!		Gives the user information on the keyboard
	!		he or she is working on, or on some other keyboard.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:HELP_KEYBOARD/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP HELP_KEYBOARD
	!	$ DELETE HELP_KEYBOARD.OBJ;*
	!
	! AUTHOR:
	!
	!	06/29/87 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	03/13/92 - Kevin Handy
	!		Clean up vars (checkvar)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/29/2000 - Kevin Handy
	!		Drop useless error trap
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"

	!
	! Dimension statements
	!
	DIM TEXT$(500%)			! Source text
	DIM GROUP$(32%), GROUP%(32%)		! Group pointers

	!
	! Define vars
	!
	DECLARE LONG SMG_KEYBOARD

	RECORD KMAP_MAP
		LONG XPOS
		LONG YPOS
		LONG XLEN
		LONG XFLAG
		STRING SNAME = 8%
		STRING LNAME = 20%
	END RECORD

	DIM KMAP_MAP KMAP(100%)

	DECLARE RFA TXRFA

	%PAGE

	!
	! Prepare for help
	!
	OLD_IDENT$	= SCOPE::PRG_IDENT
	OLD_PROGRAM$	= SCOPE::PRG_PROGRAM
	OLD_ITEM$	= SCOPE::PRG_ITEM

	SCOPE::PRG_IDENT	= "KYBD"
	SCOPE::PRG_PROGRAM	= "HELP_KEYBOARD"
	SCOPE::PRG_ITEM	= "INITILIZATION"

1000	!
	! Open up keyboard information file
	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(LR.INDEX%, LBR$C_READ)

	IF (ST% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Unable to initilize library", 0%)
		EXIT SUB
	END IF

	!
	! Open the library function
	!

	ST% = LBR$OPEN(LR.INDEX%, LIB_NAME$, , DEF_DIR$ + ".TLB")

	IF (ST% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to open library " + LIB_NAME$, 0%)
		EXIT SUB
	END IF

	!
	! Search for key in file
	!
	ST% = LBR$LOOKUP_KEY(LR.INDEX%, KEYBOARD$, TXRFA)

	IF (ST% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Unable to find " + KEYBOARD$, 0%)
		EXIT SUB
	END IF

	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		20%,		! 20 Rows &
		132%,		! 80 Columns &
		SMG_KEYBOARD	! Identifier &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_KEYBOARD,	! Data pasteboard &
		SCOPE::SMG_PBID,	! Pasetboard &
		1%,		! Row to start in &
		1%,		! Column to start in &
				! Don't need top-disp &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Remove message
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	!
	! Change width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, , &
		C_WIDTH%)
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 132%)

1100	!
	! Read entire file into memory
	!
	TEXT$ = SPACE$(132%)
	ST% = LBR$GET_RECORD(LR.INDEX%, TEXT$)

	IF NOT(ST% AND 1%) = 1%
	THEN
		GOTO 1200
	END IF

	TEXT$ = EDIT$(TEXT$, 4% + 8% + 16% + 128%)
	GOTO 1100 IF (TEXT$ = "") OR (LEFT(TEXT$, 1%) = "!")

	IF LEFT(TEXT$, 2%) = "##"
	THEN
		!
		! Keep track of groups
		!
		GROUP$(KEYBOARD.GROUPS%) = RIGHT(TEXT$, 3%)
		GROUP%(KEYBOARD.GROUPS%) = KEYBOARD.MAX%
		KEYBOARD.GROUPS% = KEYBOARD.GROUPS% + 1%
	END IF

	TEXT$(KEYBOARD.MAX%) = TEXT$
	KEYBOARD.MAX% = KEYBOARD.MAX% + 1%
	GOTO 1100

1200	!
	! Display default keyboard
	!
	!
	! Close library file
	!
	ST% = LBR$CLOSE(LR.INDEX%)

	GOSUB LoadKeyboard
	GOSUB DisplayKeyboard
	GOSUB HighLight

	%PAGE

2000	!*******************************************************************
	! Commands
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, &
		"Press action key <Next-Screen>,<Previous-Screen>," + &
		"<Up, Down, Right, or Left Arrow>,<Exit>, or <Help>", 8%)

	!
	! Handle function keys
	!
 Mloop:
	SELECT SCOPE::SCOPE_EXIT

	!
	! Next Screen
	!
	CASE SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_F7
		KEYBOARD.GROUP% = KEYBOARD.GROUP% + 1%
		KEYBOARD.GROUP% = 0% &
			IF KEYBOARD.GROUP% >= KEYBOARD.GROUPS%
		GOSUB LoadKeyboard
		GOSUB DisplayKeyboard
		GOSUB HighLight
		GOTO 2000

	!
	! Prevoius Screen
	!
	CASE SMG$K_TRM_PREV_SCREEN
		KEYBOARD.GROUP% = KEYBOARD.GROUP% - 1%
		KEYBOARD.GROUP% = KEYBOARD.GROUPS% - 1% &
			IF KEYBOARD.GROUP% = -1%
		GOSUB LoadKeyboard
		GOSUB DisplayKeyboard
		GOSUB HighLight
		GOTO 2000

	!
	! Left arrow
	!
	CASE SMG$K_TRM_LEFT
		CURRENT.KEY% = CURRENT.KEY% - 1%
		CURRENT.KEY% = KMAP.TOTAL% - 1% &
			IF CURRENT.KEY% = -1%
		GOTO Mloop IF KMAP(CURRENT.KEY%)::SNAME = ""
		GOSUB HighLight
		GOTO 2000

	!
	! Right arrow
	!
	CASE SMG$K_TRM_RIGHT
		CURRENT.KEY% = CURRENT.KEY% + 1%
		CURRENT.KEY% = 0% &
			IF CURRENT.KEY% >= KMAP.TOTAL%
		GOTO Mloop IF KMAP(CURRENT.KEY%)::SNAME = ""
		GOSUB HighLight
		GOTO 2000

	!
	! Down arrow
	!
	CASE SMG$K_TRM_DOWN
		TEMP.CENTER% = KMAP(CURRENT.KEY%)::XPOS + &
			KMAP(CURRENT.KEY%)::XLEN / 2%
		TEMP% = CURRENT.KEY% + 1%

		TEMP% = TEMP% + 1% &
			UNTIL (TEMP% >= KMAP.TOTAL%) OR &
				((KMAP(TEMP%)::XPOS <= TEMP.CENTER%) AND &
				(KMAP(TEMP%)::XPOS + KMAP(TEMP%)::XLEN >= &
				TEMP.CENTER%) AND &
				(TRM$(KMAP(TEMP%)::SNAME) <> ""))

		GOTO 2000 IF (TEMP% >= KMAP.TOTAL%)

		CURRENT.KEY% = TEMP%
		GOSUB HighLight
		GOTO 2000

	!
	! Up arrow
	!
	CASE SMG$K_TRM_UP
		TEMP.CENTER% = KMAP(CURRENT.KEY%)::XPOS + &
			KMAP(CURRENT.KEY%)::XLEN / 2%
		TEMP% = CURRENT.KEY%

 XUloop:	TEMP% = TEMP% - 1%
		GOTO 2000 IF TEMP% < 0%

		IF ((KMAP(TEMP%)::XPOS <= TEMP.CENTER%) AND &
			(KMAP(TEMP%)::XPOS + KMAP(TEMP%)::XLEN >= &
				TEMP.CENTER%) AND &
			(TRM$(KMAP(TEMP%)::SNAME) <> ""))
		THEN
			CURRENT.KEY% = TEMP%
			GOSUB HighLight
			GOTO 2000
		END IF

		GOTO XUloop

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8

		GOTO 10000

	END SELECT

	CALL ENTR_3MESSAGE(SCOPE, "Undefined key", 0%)

	GOTO 2000

	%PAGE

10000	!*******************************************************************
	! Exit from subroutine
	!*******************************************************************

	!
	! Blank screen
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_KEYBOARD)

	!
	! Reset width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, C_WIDTH%)

	!
	! Restore old screen
	!
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_KEYBOARD)

	SCOPE::PRG_IDENT	= OLD_IDENT$
	SCOPE::PRG_PROGRAM	= OLD_PROGRAM$
	SCOPE::PRG_ITEM	= OLD_ITEM$

	EXIT SUB

	%PAGE

11000	!*******************************************************************
	! Load the current keyboard
	!*******************************************************************
 LoadKeyboard:

	!
	! Set up temp pointer (Skip ## definition)
	!
	TEMP.POINTER% = GROUP%(KEYBOARD.GROUP%)

	CURRENT.KEY% = 0%
	PREVIOUS.KEY% = 0%

	X.POINTER% = 1%
	Y.POINTER% = 1%

	KMAP.TOTAL% = 0%
	X.MAX% = 0%

11100	!
	! Point to next line
	!
	TEMP.POINTER% = TEMP.POINTER% + 1%

	!
	! Exit when done
	!
	GOTO 11200 IF (TEMP.POINTER% >= KEYBOARD.MAX%) OR &
		(LEFT(TEXT$(TEMP.POINTER%), 2%) = "##")

	TEXT$ = TEXT$(TEMP.POINTER%)

	!
	! Handle #NextLine
	!
	IF LEFT(TEXT$, 2%) = "#N"
	THEN
		X.POINTER% = 1%
		Y.POINTER% = Y.POINTER% + 2%
		GOTO 11100
	END IF

	!
	! Shred text information
	!
	I% = INSTR(1%, TEXT$, " ")

	J% = INSTR(I% + 1%, TEXT$, " ")
	J% = LEN(TEXT$) + 1% UNLESS J%

	K% = INSTR(J% + 1%, TEXT$, " ")
	K% = LEN(TEXT$) + 1% UNLESS K%

	X.LEN% = VAL%(LEFT(TEXT$, I%))
	X.FLAG% = VAL%(SEG$(TEXT$, I%, J%))

	IF (X.FLAG% AND 1%) = 0%
	THEN
		TEMP% = X.POINTER% + X.LEN%
		X.MAX% = TEMP% IF TEMP% > X.MAX%
		KMAP(KMAP.TOTAL%)::XPOS = X.POINTER%
		KMAP(KMAP.TOTAL%)::YPOS = Y.POINTER%
		KMAP(KMAP.TOTAL%)::XLEN = X.LEN%
		KMAP(KMAP.TOTAL%)::XFLAG = X.FLAG%
		KMAP(KMAP.TOTAL%)::SNAME = SEG$(TEXT$, J% + 1%, K% - 1%)
		KMAP(KMAP.TOTAL%)::LNAME = RIGHT(TEXT$, K% + 1%)
		KMAP.TOTAL% = KMAP.TOTAL% + 1%
	END IF

	X.POINTER% = X.POINTER% + X.LEN% + 1%

	GOTO 11100

11200	!
	! Adjust to center
	!
	X.MAX% = (132% - X.MAX%) / 2%
	KMAP(I%)::XPOS = KMAP(I%)::XPOS + X.MAX% &
		FOR I% = 0% TO KMAP.TOTAL% - 1%

	RETURN

	%PAGE

12000	!*******************************************************************
	! Display the current keyboard information
	!*******************************************************************
 DisplayKeyboard:

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_KEYBOARD)

	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_KEYBOARD)

	SMG_STATUS% = SMG$PUT_CHARS_WIDE(SMG_KEYBOARD, &
		GROUP$(KEYBOARD.GROUP%) + " Keyboard", 20%, 1%)

	FOR I% = 0% TO KMAP.TOTAL% - 1%

		!
		! Left line
		!
		SMG_STATUS% = SMG$DRAW_LINE(SMG_KEYBOARD, &
			KMAP(I%)::YPOS, KMAP(I%)::XPOS, &
			KMAP(I%)::YPOS + 2%, KMAP(I%)::XPOS)

		!
		! Right line
		!
		SMG_STATUS% = SMG$DRAW_LINE(SMG_KEYBOARD, &
			KMAP(I%)::YPOS, KMAP(I%)::XPOS + KMAP(I%)::XLEN + 1%, &
			KMAP(I%)::YPOS + 2%, &
			KMAP(I%)::XPOS + KMAP(I%)::XLEN + 1%)

		!
		! Top line
		!
		SMG_STATUS% = SMG$DRAW_LINE(SMG_KEYBOARD, &
			KMAP(I%)::YPOS, KMAP(I%)::XPOS, &
			KMAP(I%)::YPOS, KMAP(I%)::XPOS + KMAP(I%)::XLEN + 1%) &
			UNLESS (KMAP(I%)::XFLAG AND 4%)

		!
		! Bottom line
		!
		SMG_STATUS% = SMG$DRAW_LINE(SMG_KEYBOARD, &
			KMAP(I%)::YPOS + 2%, KMAP(I%)::XPOS, &
			KMAP(I%)::YPOS + 2%, &
			KMAP(I%)::XPOS + KMAP(I%)::XLEN + 1%) &
			UNLESS (KMAP(I%)::XFLAG AND 2%)

		!
		! Text
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_KEYBOARD, &
			FORMAT$(TRM$(KMAP(I%)::SNAME), "'" + &
				STRING$(KMAP(I%)::XLEN - 1%, A"C"B)), &
			KMAP(I%)::YPOS + 1%, KMAP(I%)::XPOS + 1%)
	NEXT I%

	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_KEYBOARD)

	RETURN

	%PAGE

13000	!*******************************************************************
	! Unbold previous key, bold current key
	!*******************************************************************
 Highlight:

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_KEYBOARD)

	!
	! Unbold previous key
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_KEYBOARD, &
		FORMAT$(TRM$(KMAP(PREVIOUS.KEY%)::SNAME), "'" + &
			STRING$(KMAP(PREVIOUS.KEY%)::XLEN - 1%, A"C"B)), &
		KMAP(PREVIOUS.KEY%)::YPOS + 1%, KMAP(PREVIOUS.KEY%)::XPOS + 1%)

	!
	! Bold current key
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_KEYBOARD, &
		FORMAT$(TRM$(KMAP(CURRENT.KEY%)::SNAME), "'" + &
			STRING$(KMAP(CURRENT.KEY%)::XLEN - 1%, A"C"B)), &
		KMAP(CURRENT.KEY%)::YPOS + 1%, KMAP(CURRENT.KEY%)::XPOS + 1%, , &
		SMG$M_REVERSE)

	!
	! Display key name
	!
	TEMP$ = SPACE$(20%)
	RSET TEMP$ = TRM$(KMAP(CURRENT.KEY%)::LNAME)

	SMG_STATUS% = SMG$PUT_CHARS_WIDE(SMG_KEYBOARD, &
		TEMP$, 20%, 132% - 40%)

	!
	! Finish up
	!
	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_KEYBOARD)

	PREVIOUS.KEY% = CURRENT.KEY%
	SCOPE::PRG_ITEM = KMAP(CURRENT.KEY%)::LNAME

	RETURN

32767	END SUB
