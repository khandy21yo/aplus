1	%TITLE "Cut a Section Off the Screen Onto a Clipboard"
	%SBTTL "SUBR_3CLIPBOARDCUT"
	%IDENT "V3.6a Calico"

	SUB SUBR_3CLIPBOARDCUT(SCOPE_STRUCT SCOPE, STRING THE_FILE)

	!
	! COPYRIGHT (C) 1987 BY
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
	!	.p
	!	Allows user to select a section
	!	of the screen to clip off, and will then clip it off
	!	the main screen and put it to the given file name.
	!	.p
	!	User uses arrow keys to position the cursor.
	!	The ^*<SELECT>\* key allows the user to mark the area to clip.
	!	.p
	!	User uses arrow keys to mark the area to remove.
	!	The ^*<REMOVE>\* key removes the marked(highlighted) area to
	!	the clipboard file.
	!	.p
	!	The user can also use the arrow keys to select the
	!	direction(the last selected arrow key is the current
	!	direction except when the program is first entered when
	!	the current direction is right).
	!	.p
	!	The ^*<TAB>\* key will move the cursor 7 spaces in the current
	!	selected direction.
	!	.p
	!	The ^*<PREVSCREEN>\* key will move the cursor to the TOP of the
	!	screen if the current direction is up/down or the TOP left
	!	corner/TOP right corner if the current direction is left/right.
	!	.p
	!	The ^*<NEXTSCREEN>\* key will move the cursor to the BOTTOM of the
	!	screen if the current direction is up/down or the BOTTOM left
	!	corner/BOTTOM right corner if the current direction is
	!	left/right.
	!	.p
	!	The ^*<LISTCHOICES>\* key lists several options:
	!	.list 0,"*"
	!	.le
	!	Exit.
	!	.le
	!	Append to the clip board file(default).
	!	.le
	!	Create a new clip board  file.
	!	.le
	!	Select the FULL screen.
	!	.els
	!
	! Index:
	!	.x CLIPBOARD
	!	.x SUBR_3CLIPBOARDCUT
	!	.x CLIP
	!	.x CLIP BOARD
	!
	! Parameters:
	!
	!	THE_FILE
	!		Passed name of file to put the clipped data to.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:SUBR_3CLIPBOARDCUT/NOLINE/NOOPT
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP SUBR_3CLIPBOARDCUT
	!	$ DELETE SUBR_3CLIPBOARDCUT.OBJ;*
	!
	! Author:
	!
	!	10/22/87 - B. Craig Larsen
	!
	!	11/27/89 - Kevin Handy
	!		Taken from SUBR_CLIPBOARDCUT.
	!
	! Modification history:
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/19/95 - Kevin Handy
	!		Lose bad external def's that caused errors.
	!		Fix 1st parameter to entr_4entry and
	!		entr_4specailkeys.
	!
	!	09/04/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ routines
	!
	!	06/30/99 - Kevin Handy
	!		Compile /NOOPT to lose problems on Alpha
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Map statements
	!
	RECORD SCREENCAPTURE_STRUCT
		LONG LINES
		STRING SCREEN(24%) = 132%
	END RECORD

	DECLARE SCREENCAPTURE_STRUCT SCREENCAPTURE

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION            DSPL_SCREENCAPTURE(STRING, LONG)

	%PAGE

	!
	! Save initial information
	!
	OLD_IDENT$	= SCOPE::PRG_IDENT + ""
	OLD_PROGRAM$	= SCOPE::PRG_PROGRAM + ""
	OLD_ITEM$	= SCOPE::PRG_ITEM + ""

	SCOPE::PRG_IDENT	= "H"
	SCOPE::PRG_PROGRAM	= "SUBR_3CLIPBOARDCUT"
	SCOPE::PRG_ITEM		= "HELP"

	!
	! Set up for entchoice
	!
	EC$(0%) = "4"
	EC$(1%) = "Exit                                   "
	EC$(2%) = "Append to the clip board file(default) "
	EC$(3%) = "Create a new clip board file           "
	EC$(4%) = "Select the FULL screen                 "

	!
	! The direction of the TAB R:right,L:left,U:up,D:down
	!
	DIRECTION$ = "R"

	!
	! Determine the size of the screen
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, , &
		PB_WIDTH%, , PB_HEIGHT%)

	!
	! Create a virtual display for the cursor
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(1%, 1%, CLIPBOARD_WINDOW%)

	!
	! Define initial position of the cursor
	!
	CUR_COL% = PB_WIDTH% / 2%
	CUR_ROW% = PB_HEIGHT% / 2%

	!
	! Put the cursor on screen
	!
	SMG_STATUS% = SMG$SET_CURSOR_ABS(CLIPBOARD_WINDOW%, 1%, 1%)

	!
	! Cursor ON
	!
	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(CLIPBOARD_WINDOW%, &
		SCOPE::SMG_PBID, CUR_ROW%, CUR_COL%)

	%PAGE

 Enter:
	SMG_STATUS% = SMG$BEGIN_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	IF NOT SELECTED%
	THEN
		!
		! ON
		!
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
		SMG_STATUS% = SMG$MOVE_VIRTUAL_DISPLAY(CLIPBOARD_WINDOW%, &
			SCOPE::SMG_PBID, CUR_ROW%, CUR_COL%)
	ELSE
		!
		! OFF
		!
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		SELECT CUR_ROW%
		CASE >= PB_ROW%
			CLIPBOARD_UL_ROW% = PB_ROW%
			CLIPBOARD_LR_ROW% = CUR_ROW%

		CASE < PB_ROW%
			CLIPBOARD_UL_ROW% = CUR_ROW%
			CLIPBOARD_LR_ROW% = PB_ROW%

		END SELECT

		SELECT CUR_COL%
		CASE >= PB_COL%
			CLIPBOARD_UL_COL% = PB_COL%
			CLIPBOARD_LR_COL% = CUR_COL%

		CASE < PB_COL%
			CLIPBOARD_UL_COL% = CUR_COL%
			CLIPBOARD_LR_COL% = PB_COL%

		END SELECT

		SMG_STATUS% = SMG$CHANGE_VIRTUAL_DISPLAY(CLIPBOARD_WINDOW%, &
			CLIPBOARD_LR_ROW% - CLIPBOARD_UL_ROW% + 1%, &
			CLIPBOARD_LR_COL% - CLIPBOARD_UL_COL% + 1%)

		!
		! Slurp in all of the display stuff.
		!
		SCREENCAPTURE::LINES = 1%
		SCREENCAPTURE::SCREEN(X%) = "" FOR X% = 0% TO 24%

		SMG_STATUS% = SMG$PUT_PASTEBOARD(SCOPE::SMG_PBID, &
			DSPL_SCREENCAPTURE, LOC(SCREENCAPTURE), 0%)

		FOR I% = CLIPBOARD_UL_ROW% TO CLIPBOARD_LR_ROW%

			SMG_STATUS% = SMG$PUT_CHARS(CLIPBOARD_WINDOW%, &
				RIGHT$(SCREENCAPTURE::SCREEN(I%), &
				CLIPBOARD_UL_COL%), &
				I% - CLIPBOARD_UL_ROW% + 1%, 1%)

		NEXT I%

		SMG_STATUS% = SMG$MOVE_VIRTUAL_DISPLAY(CLIPBOARD_WINDOW%, &
			SCOPE::SMG_PBID, CLIPBOARD_UL_ROW%, CLIPBOARD_UL_COL%)
	END IF

	SMG_STATUS% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	A% = ENTR_4ENTRY(SCOPE, CLIPBOARD_WINDOW% BY VALUE, AFLAG% BY VALUE)
	A% = ENTR_4SPECIALKEYS(SCOPE, CLIPBOARD_WINDOW% BY VALUE, &
		AFLAG% BY VALUE, A% BY VALUE)

	SELECT A%
	CASE SMG$K_TRM_UP

		DIRECTION$ = "U"
		CUR_ROW% = CUR_ROW% - 1% IF CUR_ROW% > 1%

	CASE SMG$K_TRM_DOWN

		DIRECTION$ = "D"
		CUR_ROW% = CUR_ROW% + 1% IF CUR_ROW% < PB_HEIGHT%

	CASE SMG$K_TRM_LEFT

		DIRECTION$ = "L"
		CUR_COL% = CUR_COL% - 1% IF CUR_COL% > 1%

	CASE SMG$K_TRM_RIGHT

		DIRECTION$ = "R"
		CUR_COL% = CUR_COL% + 1% IF CUR_COL% < PB_WIDTH%

	CASE SMG$K_TRM_HT, SMG$K_TRM_CTRLI

		SELECT DIRECTION$
		CASE "R"
			IF CUR_COL% < PB_WIDTH%
			THEN
				CUR_COL% = CUR_COL% + 7%
				CUR_COL% = PB_WIDTH% &
					IF CUR_COL% > PB_WIDTH%
			END IF

		CASE "L"
			IF CUR_COL% > 1%
			THEN
				CUR_COL% = CUR_COL% - 7%
				CUR_COL% = 1% IF CUR_COL% < 1%
			END IF

		CASE "U"
			IF CUR_ROW% > 1%
			THEN
				CUR_ROW% = CUR_ROW% - 7%
				CUR_ROW% = 1% IF CUR_ROW% < 1%
			END IF

		CASE "D"

			IF CUR_ROW% < PB_HEIGHT%
			THEN
				CUR_ROW% = CUR_ROW% + 7%
				CUR_ROW% = PB_HEIGHT% &
					IF CUR_ROW% > PB_HEIGHT%
			END IF

		END SELECT

	CASE SMG$K_TRM_PREV_SCREEN

		SELECT DIRECTION$
		CASE "R"
			CUR_ROW% = 1%
			CUR_COL% = PB_WIDTH%

		CASE "L"
			CUR_ROW% = 1%
			CUR_COL% = 1%

		CASE "U", "D"
			CUR_ROW% = 1%

		END SELECT

	CASE SMG$K_TRM_NEXT_SCREEN

		SELECT DIRECTION$
		CASE "R"
			CUR_ROW% = PB_HEIGHT%
			CUR_COL% = PB_WIDTH%

		CASE "L"
			CUR_ROW% = PB_HEIGHT%
			CUR_COL% = 1%

		CASE "D", "U"
			CUR_ROW% = PB_HEIGHT%

		END SELECT

	CASE SMG$K_TRM_REMOVE

		IF SELECTED%
		THEN
			SELECTED% = 0%

			SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(CLIPBOARD_WINDOW%, &
				SCOPE::SMG_PBID)

			!
			! Open the clipboard file
			!
			SMG_STATUS% = LIB$GET_LUN(CLIPBOARD_CLIP_CH%)

			IF CREATE%
			THEN
				NAME$ = READ_SYSLOG("CMC$CLIPBOARD")
				SMG_STATUS% = LIB$DELETE_FILE( &
					NAME$ + ";*")
				OPEN THE_FILE FOR OUTPUT AS FILE &
					CLIPBOARD_CLIP_CH%, RECORDSIZE 250%
			ELSE
				OPEN THE_FILE AS FILE CLIPBOARD_CLIP_CH%, &
					RECORDSIZE 250%, ACCESS APPEND
			END IF

			!
			! Slurp in all of the display stuff.
			!
			SCREENCAPTURE::LINES = 1%
			SCREENCAPTURE::SCREEN(X%) = "" FOR X% = 0% TO 24%

			SMG_STATUS%    = SMG$PUT_PASTEBOARD(SCOPE::SMG_PBID, &
				DSPL_SCREENCAPTURE, LOC(SCREENCAPTURE), 0%)

			PRINT #CLIPBOARD_CLIP_CH%, &
				TRM$(SEG$(SCREENCAPTURE::SCREEN(I%), &
				CLIPBOARD_UL_COL%, CLIPBOARD_LR_COL%)) &
				FOR I% = CLIPBOARD_UL_ROW% TO CLIPBOARD_LR_ROW%

			!
			! Close the clipboard
			!
			CLOSE #CLIPBOARD_CLIP_CH%
			SMG_STATUS% = LIB$FREE_LUN(CLIPBOARD_CLIP_CH%)

			GOTO ExitFromSub
		ELSE
			CALL ENTR_3MESSAGENEWWINDOW(SCOPE, &
				"Nothing has been selected !!!", &
				0%)
		END IF

	CASE SMG$K_TRM_F10, SMG$K_TRM_F8

		GOTO ExitFromSub

	CASE SMG$K_TRM_SELECT

		GOSUB SelectIt

	CASE SMG$K_TRM_F14

		X% = ENTR_3CHOICE(SCOPE, "", "", EC$(), "", 128%, &
			" List of Choices ", "", 0%)

		SELECT X%
		!
		! Exit
		!
		CASE 1%
			GOTO ExitFromSub

		!
		! Append
		!
		CASE 2%
			CREATE% =  0%

		!
		! Overwrite
		!
		CASE 3%
			CREATE% = -1%

		!
		! Full Screen Select
		!
		CASE 4%
			IF NOT SELECTED%
			THEN
				SMG_STATUS% = SMG$CHANGE_VIRTUAL_DISPLAY( &
					CLIPBOARD_WINDOW%, 1%, 1%,, &
					SMG$M_REVERSE OR SMG$M_BOLD)
				!
				! Cursor OFF
				!
				SMG_STATUS% = SMG$SET_CURSOR_MODE( &
					SCOPE::SMG_PBID, 1%)
				SELECTED% = -1%
			END IF

			PB_ROW% = 1%
			PB_COL% = 1%

			CUR_ROW% = PB_HEIGHT%
			CUR_COL% = PB_WIDTH%

		END SELECT

	END SELECT

	GOTO Enter

	%PAGE

 SelectIt:
	IF SELECTED%
	THEN
		CALL ENTR_3MESSAGENEWWINDOW(SCOPE, &
			"Select is already ON !!!", 0%)
	ELSE
		SELECTED% = -1%

		PB_ROW% = CUR_ROW%
		PB_COL% = CUR_COL%

		SMG_STATUS% = SMG$CHANGE_VIRTUAL_DISPLAY(CLIPBOARD_WINDOW%, &
			1%, 1%, , SMG$M_REVERSE OR SMG$M_BOLD)

		!
		! Cursor OFF
		!
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
	END IF

	RETURN

	!
	! Exit fron subroutine
	!
 ExitFromSub:
	!
	! Cursor OFF
	!
	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(CLIPBOARD_WINDOW%, &
		SCOPE::SMG_PBID)

	SCOPE::PRG_IDENT = OLD_IDENT$
	SCOPE::PRG_PROGRAM = OLD_PROGRAM$
	SCOPE::PRG_ITEM = OLD_ITEM$

	END SUB
