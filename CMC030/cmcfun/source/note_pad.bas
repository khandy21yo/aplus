1	%TITLE "Enter and Maintain Text"
	%SBTTL "NOTE_PAD"
	%IDENT "V3.6a Calico"

	SUB NOTE_PAD(STRING WPOSITION, STRING WSIZE, &
		STRING TEXT, LONG FLAG, &
		STRING TITLE, LONG BORDER_POS)

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
	!	This call is used to enter and maintain text
	!	data in it's own window.  The edited text data
	!	is then passed back to the call program to be
	!	used accordingly.
	!
	!	              PCOL
	!			|
	!			v
	!	 PROW --------->+--------BORDER TITLE-----------+
	!			|	INTERNAL TITLE		| -----
	!			+-------------------------------+   ^
	!			|				|   |
	!			|				|  WROWS
	!			|              TEXT             |   |
	!			|				|   v
	!			|				| -----
	!			+-------------------------------+
	!		         |<-----------WCOLS----------->|
	!
	! Parameters:
	!
	!	The values of FLAG are as follows:
	!	.table
	!		FLAG		MEANING
	!		 0		Normal.
	!		 2		Draw lines.
	!		 8		No begin/end updates on scrolling.
	!		 16		Border Title.
	!	.endtable
	!
	!	WPOSITION
	!		The passed position at the begining.
	!
	!	WSIZE
	!		The passed size of the display section.
	!
	!	TEXT
	!		The passed text data the user enters to maintain.
	!
	!	TITLE
	!		The passed title for the text in the window.
	!
	!	BORDER_POS
	!		Draws the border position.
	!
	!
	!	Returned value
	!		This function maintains the text data in it's own
	!		window.  It is edited and passed back to the call
	!		program.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:NOTE_PAD/LINE/NOOPT
	!	$ LIB FUNC_LIB:CMCFUN/REP NOTE_PAD
	!	$ DELETE NOTE_PAD.OBJ;*
	!
	! Author:
	!
	!	01/01/87 - Kevin Handy
	!
	! Modification history:
	!
	!	02/10/89 - Kevin Handy
	!		Modified for changes in ENTR_NOLSTRING
	!
	!	09/22/89 - Kevin Handy
	!		Modified to use ENTR_ENTER instead of
	!		ENTR_NOLSTRING.
	!
	!	07/19/93 - Kevin Handy
	!		Modified to pass SMG$K_TRM_FIND through DSPL_SCROLL
	!		function instead of "FIND".
	!
	!	10/04/93 - Kevin Handy
	!		Changed "=>" for ">=".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/19/95 - Kevin Handy
	!		Lose bas external def's that caused errors.
	!		Fix 1st parameter to entr_4entry and
	!		entr_4specialkeys.
	!
	!	09/04/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/23/99 - Kevin Handy
	!		Lose useless error trapping
	!
	!	04/08/99 - Kevin Handy
	!		Fixed several places where SMG_SCROLL was passed
	!		when SMG_SCROLL::WINDOW should have been.
	!
	!	06/30/99 - Kevin Handy
	!		Compile /NOOPT to lose problems on Alpha
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE	SMG_SCROLL_CDD	SMG_SCROLL

	EXTERNAL INTEGER FUNCTION DSPL_SCROLL

	DECLARE LONG SMG_TEXT, WROWS, WCOLS, PROW, PCOL, COL, &
		VIDEO_ATTRIBUTE, WORK_ATTRIBUTE

	DIM TEXT$(1000%), RET%(1000%)

	%PAGE

	OLD_IDENT$ = SCOPE::PRG_IDENT		! Save the COMMON Ident
	OLD_PROGRAM$ = SCOPE::PRG_PROGRAM	! Save the COMMON Program
	OLD_ITEM$ = SCOPE::PRG_ITEM		! Save the COMMON item

	SCOPE::PRG_IDENT = "FUNC"
	SCOPE::PRG_PROGRAM = "NOTE_PAD"
	SCOPE::PRG_ITEM = "NOTEPAD"

	!
	! Determine the window size
	!
	CALL DSPL_SPLITCURSOR(WSIZE,     WROWS, WCOLS)
	CALL DSPL_SPLITCURSOR(WPOSITION, PROW,  PCOL)

	WCOLS = 78% IF WCOLS > 78% OR WCOLS < 1%

	WROWS = WROWS + 2% IF (FLAG AND 16%) = 0%
	WROWS = 14% IF WROWS > 14% OR WROWS < 1%

	TEXT_LOOP% = 1%
	GOSUB ParseText

50	!
	! Create the window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(WROWS, WCOLS, SMG_TEXT, &
		SMG$M_BORDER)

	OLD_OPTION = SCOPE::SMG_OPTION	! Save the OPTION  virtual display
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 80%, SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
		SCOPE::SMG_PBID, 21%, 1%)

	!
	! Output the title and title line
	!
	IF (FLAG AND 16%)
	THEN
		SMG_STATUS% = SMG$LABEL_BORDER(SMG_TEXT, TITLE,, BORDER_POS)
	ELSE
		SMG_STATUS% = SMG$PUT_CHARS(SMG_TEXT, TITLE, 1%, 1%)
		SMG_STATUS% = SMG$DRAW_LINE(SMG_TEXT, 2%, 1%, 2%, WCOLS)
	END IF

	SMG_SCROLL::WINDOW	= SMG_TEXT
	SMG_SCROLL::SCROLL_TOP	= 3%
	SMG_SCROLL::SCROLL_TOP	= 1% IF (FLAG AND 16%)
	SMG_SCROLL::SCROLL_BOT	= WROWS

	SMG_SCROLL::TOP_ARRAY	= 1%
	SMG_SCROLL::TOP_LINE	= 1%
	SMG_SCROLL::BEG_ELEMENT	= 1%
	SMG_SCROLL::CUR_LINE, SMG_SCROLL::FIND_LINE = 1%

	SMG_SCROLL::CUR_W_COL	= 1%
	SMG_SCROLL::CUR_W_ROW	= 1%

	SMG_SCROLL::PROMPT	= ""
	SMG_SCROLL::DRAW_COLS	= ""
	SMG_SCROLL::SMG_FLAG	= 8% + 128%

	SMG_SCROLL::VIDEO_SET	= SMG$M_REVERSE

	V% = DSPL_SCROLL(SMG_SCROLL, TEXT$(), 0%, "PAINT")

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_TEXT, SCOPE::SMG_PBID, &
		PROW, PCOL)

 L1000:
	PLACE_CUR_W_COL% = SMG_SCROLL::CUR_W_COL

 L1010:
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL::WINDOW,	! Window &
		"", &
		SMG_SCROLL::CUR_W_ROW,	! Line &
		SMG_SCROLL::CUR_W_COL,	! Column &
		0%,			! Erase screen &
		VIDEO_ATTRIBUTE)	! Videoattribute

	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)

 L1020:
	!
	! Normal entry
	!
	WORK% = 0%
	WHILE (WORK% = 0%)
		WORK% = ENTR_4ENTRY(SCOPE, SMG_TEXT BY VALUE, 0% BY VALUE)
		WORK% = ENTR_4SPECIALKEYS(SCOPE, SMG_TEXT BY VALUE, 0% BY VALUE, WORK% BY VALUE)
	NEXT
	WORK$ = CHR$(WORK%)

	IF CLEAR_BOT%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "", 1%)
	END IF

	CLEAR_BOT% = 0%

	IF ((WORK% >= 32%) AND (WORK% <= 126%)) OR ((WORK% >= 160%) AND (WORK% <= 254%))
	THEN
		IF SELECT_START_ROW%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Adding text is not allowed when <select> key is active", 1%)
			CLEAR_BOT% = -1%
			GOTO L1020
		END IF

		IF LEN(RIGHT(TEXT$(SMG_SCROLL::FIND_LINE), SMG_SCROLL::CUR_W_COL))
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL::WINDOW,	! Window &
				WORK$ + RIGHT(TEXT$(SMG_SCROLL::FIND_LINE), &
				SMG_SCROLL::CUR_W_COL), &
				SMG_SCROLL::CUR_W_ROW,	! Line &
				SMG_SCROLL::CUR_W_COL,	! Column &
				0%,			! Erase screen &
				WORK_ATTRIBUTE)		! Videoattribute
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL::WINDOW,	! Window &
			WORK$, &
			SMG_SCROLL::CUR_W_ROW,	! Line &
			SMG_SCROLL::CUR_W_COL,	! Column &
			0%,			! Erase screen &
			VIDEO_ATTRIBUTE)	! Videoattribute

		TEXT$(SMG_SCROLL::FIND_LINE) = &
			LEFT(TEXT$(SMG_SCROLL::FIND_LINE), &
			SMG_SCROLL::CUR_W_COL - 1%) + &
			WORK$ + RIGHT(TEXT$(SMG_SCROLL::FIND_LINE), &
			SMG_SCROLL::CUR_W_COL)

		PLACE_CUR_W_COL%, SMG_SCROLL::CUR_W_COL = &
			SMG_SCROLL::CUR_W_COL + 1%

		IF LEN(TEXT$(SMG_SCROLL::FIND_LINE)) > WCOLS - 1% AND SMG_SCROLL::CUR_W_COL > WCOLS - 1%
		THEN
			TEXT_LOOP% = SMG_SCROLL::FIND_LINE
			GOSUB CreateTextString
			GOSUB ParseText
			PLACE_CUR_W_COL%, SMG_SCROLL::CUR_W_COL = (SMG_SCROLL::CUR_W_COL - &
				LEN(TEXT$(SMG_SCROLL::FIND_LINE)))
			SMG_SCROLL::FIND_LINE = SMG_SCROLL::FIND_LINE + 1%

			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

			TEMP = DSPL_SCROLL(SMG_SCROLL, &
				TEXT$(), &
				0%, &
				"PAINT")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL::WINDOW,	! Window &
				"", &
				SMG_SCROLL::CUR_W_ROW,	! Line &
				SMG_SCROLL::CUR_W_COL,	! Column &
				0%,			! Erase screen &
				VIDEO_ATTRIBUTE)	! Videoattribute

			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
		END IF

		GOTO L1020
	END IF

	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!*****************************************************************
	! Determine position of cursor and grab any
	! special characters
	!*****************************************************************
	SCOPE::SCOPE_EXIT = WORK%

	SELECT WORK%
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitSub

	!
	! Handle BS and TAB (beep and ignore them)
	!
	CASE SMG$K_TRM_CTRLH, SMG$K_TRM_CTRLI
		SMG_STATUS% = SMG$RING_BELL(SMG_SCROLL::WINDOW)
		GOTO L1010

	!
	! Next line, previous line
	!
	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, SMG$K_TRM_PREV_SCREEN, &
		SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_F18, &
		SMG$K_TRM_F19

		IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_DOWN AND SMG_SCROLL::FIND_LINE = SMG_SCROLL::BOT_ARRAY) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_UP  AND SMG_SCROLL::FIND_LINE = 1%) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_NEXT_SCREEN AND SMG_SCROLL::FIND_LINE = SMG_SCROLL::BOT_ARRAY) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_PREV_SCREEN  AND SMG_SCROLL::FIND_LINE = 1%) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_F19 AND SMG_SCROLL::FIND_LINE = SMG_SCROLL::BOT_ARRAY) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_F18  AND SMG_SCROLL::FIND_LINE = 1%)
		THEN
			SMG_STATUS% = SMG$RING_BELL(SMG_SCROLL::WINDOW)
			GOTO L1010
		END IF

		IF LEN(TEXT$(SMG_SCROLL::FIND_LINE)) > WCOLS - 1%
		THEN
			TEXT_LOOP% = SMG_SCROLL::FIND_LINE
			GOSUB Common2

			TEMP = DSPL_SCROLL(SMG_SCROLL, &
				TEXT$(), &
				0%, &
				"PAINT")
		END IF

		TEMP = DSPL_SCROLL(SMG_SCROLL, &
			TEXT$(), &
			SCOPE::SCOPE_EXIT, &
			"")

		SMG_SCROLL::CUR_W_COL, PLACE_CUR_W_COL% = 1% IF &
			SCOPE::SCOPE_EXIT = SMG$K_TRM_PREV_SCREEN OR &
			SCOPE::SCOPE_EXIT = SMG$K_TRM_NEXT_SCREEN

		TEMP% = LEN(TEXT$(SMG_SCROLL::FIND_LINE)) + 1%

		IF SMG_SCROLL::CUR_W_COL < PLACE_CUR_W_COL%
		THEN
			SMG_SCROLL::CUR_W_COL = PLACE_CUR_W_COL%
		END IF

		IF SMG_SCROLL::CUR_W_COL > TEMP%
		THEN
			SMG_SCROLL::CUR_W_COL = TEMP%
		END IF

		GOTO L1010

	!
	! Test for left arrow
	!
	CASE SMG$K_TRM_LEFT
		WORK_ATTRIBUTE = 0%
		WORK_ATTRIBUTE = VIDEO_ATTRIBUTE IF SMG_SCROLL::FIND_LINE < SELECT_START_ROW% OR &
			SMG_SCROLL::FIND_LINE = SELECT_START_ROW% AND SMG_SCROLL::CUR_W_COL <= SELECT_START_COL%

		IF VIDEO_ATTRIBUTE
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL::WINDOW,	! Window &
				MID(TEXT$(SMG_SCROLL::FIND_LINE), SMG_SCROLL::CUR_W_COL, 1%), &
				SMG_SCROLL::CUR_W_ROW,	! Line &
				SMG_SCROLL::CUR_W_COL,	! Column &
				0%,			! Erase screen &
				WORK_ATTRIBUTE)		! Videoattribute
		END IF

		SMG_SCROLL::CUR_W_COL = SMG_SCROLL::CUR_W_COL - 1%
		IF SMG_SCROLL::CUR_W_COL <= 0%
		THEN
			IF SMG_SCROLL::FIND_LINE > 1%
			THEN
				SMG_SCROLL::FIND_LINE = SMG_SCROLL::FIND_LINE -	1%

				TEMP = DSPL_SCROLL(SMG_SCROLL, &
					TEXT$(), &
					SMG$K_TRM_UP, &
					"")

				SMG_SCROLL::CUR_W_COL = LEN(TEXT$(SMG_SCROLL::FIND_LINE)) + 1%
			ELSE
				SMG_STATUS% = SMG$RING_BELL(SMG_SCROLL::WINDOW)
				SMG_SCROLL::CUR_W_COL = 1%
			END IF
		END IF

		GOTO L1000

	!
	! Test for right arrow
	!
	CASE SMG$K_TRM_RIGHT
		WORK_ATTRIBUTE = 0%
		WORK_ATTRIBUTE =  VIDEO_ATTRIBUTE IF SMG_SCROLL::FIND_LINE > SELECT_START_ROW% OR &
			SMG_SCROLL::FIND_LINE = SELECT_START_ROW% AND SMG_SCROLL::CUR_W_COL >= SELECT_START_COL%

		IF VIDEO_ATTRIBUTE
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL::WINDOW,	! Window &
				MID(TEXT$(SMG_SCROLL::FIND_LINE), SMG_SCROLL::CUR_W_COL, 1%), &
				SMG_SCROLL::CUR_W_ROW,	! Line &
				SMG_SCROLL::CUR_W_COL,	! Column &
				0%,			! Erase screen &
				WORK_ATTRIBUTE)	! Videoattribute
		END IF

		SMG_SCROLL::CUR_W_COL = SMG_SCROLL::CUR_W_COL + 1%
		IF SMG_SCROLL::CUR_W_COL > LEN(TEXT$(SMG_SCROLL::FIND_LINE)) + 1%
		THEN
			IF SMG_SCROLL::FIND_LINE < SMG_SCROLL::BOT_ARRAY
			THEN
				SMG_SCROLL::FIND_LINE = SMG_SCROLL::FIND_LINE + 1%
				TEMP = DSPL_SCROLL(SMG_SCROLL, &
					TEXT$(), &
					SMG$K_TRM_DOWN, &
					"")
				SMG_SCROLL::CUR_W_COL = 1%
			ELSE
				SMG_STATUS% = SMG$RING_BELL(SMG_SCROLL::WINDOW)
				SMG_SCROLL::CUR_W_COL = LEN(TEXT$(SMG_SCROLL::FIND_LINE)) + 1%
			END IF
		END IF

		GOTO L1000

	!
	! Delete
	!
	CASE SMG$K_TRM_DELETE
		IF SMG_SCROLL::CUR_W_COL = 1%
		THEN
			IF SMG_SCROLL::FIND_LINE <= 1%
			THEN
				SMG_STATUS% = SMG$RING_BELL(SMG_SCROLL::WINDOW)
				GOTO L1000
			ELSE
				GOSUB Common1
			END IF
		END IF

		TEXT_LOOP% = SMG_SCROLL::FIND_LINE
		TEXT$(TEXT_LOOP%) = LEFT(TEXT$(TEXT_LOOP%), SMG_SCROLL::CUR_W_COL - 2%) + &
			'255'C + RIGHT(TEXT$(TEXT_LOOP%), SMG_SCROLL::CUR_W_COL)

		SMG_SCROLL::CUR_W_COL = SMG_SCROLL::CUR_W_COL - 1%

		GOSUB Common2

	!
	! Rub Line
	!
	CASE SMG$K_TRM_F12, SMG$K_TRM_CTRLU
		IF SMG_SCROLL::CUR_W_COL = 1%
		THEN
			IF SMG_SCROLL::FIND_LINE <= 1%
			THEN
				SMG_STATUS% = SMG$RING_BELL(SMG_SCROLL::WINDOW)
				GOTO L1000
			END IF
		END IF

		TEXT_LOOP% = SMG_SCROLL::FIND_LINE
		TEXT$(TEXT_LOOP%) = RIGHT(TEXT$(TEXT_LOOP%), SMG_SCROLL::CUR_W_COL)
		SMG_SCROLL::CUR_W_COL = 1%

		IF SMG_SCROLL::FIND_LINE > 1%
		THEN
			GOSUB Common1
		END IF

		TEXT_LOOP% = SMG_SCROLL::FIND_LINE
		GOSUB Common2

	!
	! Rub Word
	!
	CASE SMG$K_TRM_F13
		IF SMG_SCROLL::CUR_W_COL = 1%
		THEN
			IF SMG_SCROLL::FIND_LINE <= 1%
			THEN
				SMG_STATUS% = SMG$RING_BELL(SMG_SCROLL::WINDOW)
				GOTO L1000
			ELSE
				GOSUB Common1
			END IF
		END IF

		TEMP$ = TEXT$(SMG_SCROLL::FIND_LINE)
		FIND% = SMG_SCROLL::CUR_W_COL

		UNTIL FIND% < 1% OR MID(TEMP$, FIND%, 1%) <> " "
			FIND% = FIND% - 1%
		NEXT

		MARK% = FIND% + 1%

		UNTIL FIND% < 1% OR MID(TEMP$, FIND%, 1%) = " "
			FIND% = FIND% - 1%
		NEXT

		UNTIL FIND% < 1% OR MID(TEMP$, FIND%, 1%) <> " "
			FIND% = FIND% - 1%
		NEXT

		TEXT_LOOP% = SMG_SCROLL::FIND_LINE
		TEXT$(TEXT_LOOP%) = LEFT(TEMP$, FIND%) + '255'C + &
			RIGHT(TEMP$, MARK%)

		SMG_SCROLL::CUR_W_COL = FIND% + 1%

		GOSUB Common2

	!
	! Find Key
	!
	CASE SMG$K_TRM_FIND
 Find1:		!
		! Search for certain keyword
		!
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, "Search for: ", &
			1%, 1%, 1%)

		FINP$ = SPACE$(67%)

		TEMP% = ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
			1%, 13%, FINP$, -1%, 16% + 4096%)
		FINP$ = EDIT$(FINP$, 4% + 8% + 128% + 256%)
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

		SELECT SCOPE::SCOPE_EXIT
			! ^C, ^Z, Exit
		CASE SMG$K_TRM_CTRLC, &
			SMG$K_TRM_F8, SMG$K_TRM_F10, &
			SMG$K_TRM_CTRLZ

			GOTO L1000

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, &
			SMG$K_TRM_F7 ! Good keys

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO Find1
		END SELECT

		GOTO L1000 IF FINP$ = ""

		COL = SMG_SCROLL::CUR_W_COL - 1%

		FOR F% = SMG_SCROLL::FIND_LINE TO SMG_SCROLL::END_ELEMENT

 Find2:			COL = INSTR((COL + 1%), TEXT$(F%), FINP$)

			IF COL
			THEN
				SMG_SCROLL::FIND_LINE = F%
				TEMP = DSPL_SCROLL(SMG_SCROLL, &
					TEXT$(), &
					SMG$K_TRM_FIND, &
					"FIND")

				SMG_SCROLL::CUR_W_COL = COL

				SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL::WINDOW, &
					MID(TEXT$(SMG_SCROLL::FIND_LINE), SMG_SCROLL::CUR_W_COL, 1%), &
					SMG_SCROLL::CUR_W_ROW,	! Line &
					SMG_SCROLL::CUR_W_COL,, &
					SMG$M_REVERSE + SMG$M_BLINK)

 Find3:				CALL ENTR_3MESSAGE(SCOPE, &
					"Press <EXIT> to exit from this search or", 0%)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL::WINDOW, &
					MID(TEXT$(SMG_SCROLL::FIND_LINE), SMG_SCROLL::CUR_W_COL, 1%), &
					SMG_SCROLL::CUR_W_ROW,	! Line &
					SMG_SCROLL::CUR_W_COL,, &
					VIDEO_ATTRIBUTE)

				SELECT SCOPE::SCOPE_EXIT
				! ^C, ^Z, Exit
				CASE SMG$K_TRM_CTRLC, &
					SMG$K_TRM_F8, SMG$K_TRM_F10, &
					SMG$K_TRM_CTRLZ

					GOTO FindEnd

				CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, &
					SMG$K_TRM_F7	! Good keys

				CASE ELSE
					CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
					GOTO Find3
				END SELECT

				ELEN = DSPL_SCROLL(SMG_SCROLL, TEXT$(), 0%, &
					"PAINT")

				GOTO Find2
			END IF
		NEXT F%

		CALL ENTR_3MESSAGE(SCOPE, "Unable to find string!!!", 0%)
 FindEnd:
	!
	! Select key
	!
	CASE SMG$K_TRM_SELECT
		IF SELECT_START_ROW%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Select range is already active", 1%)
			CLEAR_BOT% = -1%
		ELSE
			CUT_TEXT$ = ""
			SELECT_START_ROW% = SMG_SCROLL::FIND_LINE
			SELECT_START_COL% = SMG_SCROLL::CUR_W_COL
			VIDEO_ATTRIBUTE = SMG$M_REVERSE
		END IF

	!
	! Remove key
	!
	CASE SMG$K_TRM_REMOVE
		IF SELECT_START_ROW% = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "No select range active", 1%)
			CLEAR_BOT% = -1%
		ELSE
			VIDEO_ATTRIBUTE = 0%
			IF SELECT_START_ROW% = SMG_SCROLL::FIND_LINE
			THEN
				IF SELECT_START_COL% >= SMG_SCROLL::CUR_W_COL
				THEN
					CUT_TEXT$ = MID(TEXT$(SMG_SCROLL::FIND_LINE), &
						SMG_SCROLL::CUR_W_COL, SELECT_START_COL% - (SMG_SCROLL::CUR_W_COL))
					TEXT$(SMG_SCROLL::FIND_LINE) = LEFT(TEXT$(SMG_SCROLL::FIND_LINE), &
						SMG_SCROLL::CUR_W_COL - 1%) + &
						RIGHT(TEXT$(SMG_SCROLL::FIND_LINE), SELECT_START_COL%)
					TEXT_LOOP% = SELECT_START_ROW%
				ELSE
					CUT_TEXT$ = MID(TEXT$(SMG_SCROLL::FIND_LINE), &
						SELECT_START_COL%, SMG_SCROLL::CUR_W_COL - (SELECT_START_COL%))
					TEXT$(SMG_SCROLL::FIND_LINE) = LEFT(TEXT$(SMG_SCROLL::FIND_LINE), &
						SELECT_START_COL% - 1%) + &
						RIGHT(TEXT$(SMG_SCROLL::FIND_LINE), SMG_SCROLL::CUR_W_COL)
					SMG_SCROLL::CUR_W_COL = SELECT_START_COL%
					TEXT_LOOP% = SELECT_START_ROW%
				END IF
			ELSE
				IF SELECT_START_ROW% > SMG_SCROLL::FIND_LINE
				THEN
					CUT_TEXT$ = RIGHT(TEXT$(SMG_SCROLL::FIND_LINE), SMG_SCROLL::CUR_W_COL + 1%)
					TEXT$(SMG_SCROLL::FIND_LINE) = LEFT(TEXT$(SMG_SCROLL::FIND_LINE), &
						SMG_SCROLL::CUR_W_COL)
					FOR LOOP% = SMG_SCROLL::FIND_LINE + 1% TO SELECT_START_ROW% - 1%
						CUT_TEXT$ = CUT_TEXT$ + TEXT$(LOOP%)
						TEXT$(LOOP%) = ""
					NEXT LOOP%
					CUT_TEXT$ = CUT_TEXT$ + LEFT(TEXT$(SELECT_START_ROW%), SELECT_START_COL%)
					TEXT$(SELECT_START_ROW%) =  RIGHT(TEXT$(SELECT_START_ROW%), SELECT_START_COL% + 1%)
					TEXT_LOOP% = SMG_SCROLL::FIND_LINE
				ELSE
					CUT_TEXT$ = RIGHT(TEXT$(SELECT_START_ROW%), SELECT_START_COL%)
					TEXT$(SELECT_START_ROW%) =  LEFT(TEXT$(SELECT_START_ROW%), &
						SELECT_START_COL% - 1%)
					FOR LOOP% = SELECT_START_ROW% + 1% TO SMG_SCROLL::FIND_LINE - 1%
						CUT_TEXT$ = CUT_TEXT$ + TEXT$(LOOP%)
						TEXT$(LOOP%) = ""
					NEXT LOOP%
					CUT_TEXT$ = CUT_TEXT$ + LEFT(TEXT$(SMG_SCROLL::FIND_LINE), &
						SMG_SCROLL::CUR_W_COL - 1%)
					TEXT$(SMG_SCROLL::FIND_LINE) = RIGHT(TEXT$(SMG_SCROLL::FIND_LINE), &
						SMG_SCROLL::CUR_W_COL)
					TEXT_LOOP% = SELECT_START_ROW%
					SMG_SCROLL::CUR_W_COL = SELECT_START_COL%
					SMG_SCROLL::FIND_LINE = SELECT_START_ROW%
				END IF
			END IF
			GOSUB Common2
			TEMP = DSPL_SCROLL(SMG_SCROLL, &
				TEXT$(), &
				0%, &
				"PAINT")

			SELECT_START_ROW%, SELECT_START_COL% = 0%
		END IF

	!
	! Insert key
	!
	CASE SMG$K_TRM_INSERT_HERE
		TEXT_LOOP% = SMG_SCROLL::FIND_LINE
		TEXT$(TEXT_LOOP%) = LEFT(TEXT$(TEXT_LOOP%), SMG_SCROLL::CUR_W_COL - 1%) + &
			CUT_TEXT$ + '255'C + RIGHT(TEXT$(TEXT_LOOP%), SMG_SCROLL::CUR_W_COL)
		GOSUB Common2
		TEMP = DSPL_SCROLL(SMG_SCROLL, &
			TEXT$(), &
			0%, &
			"PAINT")

	!
	! Carriage return
	!
	CASE 13%
		RET%(SMG_SCROLL::FIND_LINE) = 13%
		TEXT_LOOP%, SMG_SCROLL::FIND_LINE = &
			SMG_SCROLL::FIND_LINE + 1%
		TEXT$(TEXT_LOOP%) = '255'C + &
			RIGHT(TEXT$(TEXT_LOOP% - 1%), SMG_SCROLL::CUR_W_COL) + &
			TEXT$(TEXT_LOOP%)
		TEXT$(TEXT_LOOP% - 1%) = LEFT(TEXT$(TEXT_LOOP% - 1%), SMG_SCROLL::CUR_W_COL - 1%)

		SMG_SCROLL::BOT_ARRAY,	SMG_SCROLL::END_ELEMENT = &
			SMG_SCROLL::BOT_ARRAY + 1%
		GOSUB Common2

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)

	END SELECT

	TEMP = DSPL_SCROLL(SMG_SCROLL, &
		TEXT$(), &
		0%, &
		"PAINT")

	GOTO L1000

 ExitSub:
	!
	! Delete all displays
	!
	TEXT_LOOP% = 1%
	GOSUB CreateTextString

	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
		SCOPE::SMG_PBID)

	SCOPE::SMG_OPTION = OLD_OPTION	! Restore OPTION  virtual display

	SCOPE::PRG_IDENT = OLD_IDENT$		! Restore the COMMON Ident
	SCOPE::PRG_PROGRAM = OLD_PROGRAM$	! Restore the COMMON Program
	SCOPE::PRG_ITEM = OLD_ITEM$		! Restore the COMMON item

	EXIT SUB

	%PAGE

 ParseText:
	!*******************************************************************
	! Create text array for window
	!
	! This subroutine takes a text string (TEXT), and converts it into
	! an array of strings (TEXT$) with a array of return flags (RET%).
	!*******************************************************************

	!
	! While there is still text to be had
	!
	WHILE TEXT <> ""

		RET%(TEXT_LOOP%) = 0%

		!
		! Search for a return
		!
		FIND% = INSTR(1%, LEFT(TEXT, WCOLS), '13'C)
		IF FIND%
		THEN
			RET%(TEXT_LOOP%) = 13%
			!
			! Handle the return
			!
			TEXT$(TEXT_LOOP%) = LEFT(TEXT, FIND% - 1%)
			TEXT = RIGHT(TEXT, FIND% + 1%)
			GOSUB FindCursor
			TEXT_LOOP% = TEXT_LOOP% + 1%
		ELSE
			!
			! No return, so search for a break
			!
			IF LEN(TEXT) < WCOLS
			THEN
				!
				! Rest of test will fit on screen
				!
				TEXT$(TEXT_LOOP%) = TEXT
				TEXT = ""
				GOSUB FindCursor
				TEXT_LOOP% = TEXT_LOOP% + 1%
			ELSE
				!
				! Search for a break between words
				!
				FIND% = WCOLS
				UNTIL FIND% < 1% OR MID(TEXT, FIND%, 1%) <> " "
					FIND% = FIND% - 1%
				NEXT

				UNTIL FIND% < 1% OR MID(TEXT, FIND%, 1%) = " "
					FIND% = FIND% - 1%
				NEXT

				FIND% = WCOLS IF FIND% < 1%

				!
				! Stash away the text
				!
				TEXT$(TEXT_LOOP%) = LEFT(TEXT, FIND%)
				TEXT = RIGHT(TEXT, FIND% + 1%)
				GOSUB FindCursor
				TEXT_LOOP% = TEXT_LOOP% + 1%
			END IF
		END IF
	NEXT

	TEXT$(TEXT_LOOP%) = ""
	SMG_SCROLL::BOT_ARRAY,	SMG_SCROLL::END_ELEMENT = TEXT_LOOP%

	RETURN

	%Page

 CreateTextString:
	!
	! Take text from window and put into text string
	!
	TEXT = ""

	FOR LOOP% = TEXT_LOOP% TO SMG_SCROLL::BOT_ARRAY
		TEXT = TEXT + TEXT$(LOOP%) IF RET%(LOOP%) = 0%
		TEXT = TEXT + TEXT$(LOOP%) + CHR$(RET%(LOOP%)) IF RET%(LOOP%)
	NEXT LOOP%

	RETURN

	%Page

 FindCursor:
	!
	! Find the cursor token to determine row and column to position
	! cursor on
	!
	TEMP% = INSTR(1%, TEXT$(TEXT_LOOP%), '255'C)

	IF TEMP%
	THEN
		SMG_SCROLL::CUR_W_COL = TEMP%
		SMG_SCROLL::FIND_LINE = TEXT_LOOP%
		TEXT$(TEXT_LOOP%) = LEFT(TEXT$(TEXT_LOOP%),TEMP% - 1%) + &
			RIGHT(TEXT$(TEXT_LOOP%), TEMP% + 1%)
	END IF

	RETURN

	%Page

 Common1:
	SMG_SCROLL::FIND_LINE = SMG_SCROLL::FIND_LINE - 1%
	SMG_SCROLL::FIND_LINE = 1% IF SMG_SCROLL::FIND_LINE < 1%
	TEXT$(SMG_SCROLL::FIND_LINE) = TEXT$(SMG_SCROLL::FIND_LINE) + " " &
			IF RET%(SMG_SCROLL::FIND_LINE)
	SMG_SCROLL::CUR_W_COL = LEN(TEXT$(SMG_SCROLL::FIND_LINE)) + 1%
	RET%(SMG_SCROLL::FIND_LINE) = 0%

	TEMP = DSPL_SCROLL(SMG_SCROLL, &
		TEXT$(), &
		SMG$K_TRM_UP, &
		"")

	RETURN

	%Page

 Common2:
	IF TEXT_LOOP% > 1%
	THEN
		IF  SMG_SCROLL::CUR_W_COL + LEN(TEXT$(TEXT_LOOP% - 1%)) < WCOLS AND RET%(TEXT_LOOP% - 1%) = 0%
		THEN
			TEXT_LOOP%, SMG_SCROLL::FIND_LINE = SMG_SCROLL::FIND_LINE - 1% &
				IF SMG_SCROLL::FIND_LINE > 1%
			TEMP = DSPL_SCROLL(SMG_SCROLL, &
				TEXT$(), &
				SMG$K_TRM_UP, &
				"")
		END IF
	END IF

	LINE_ONE_RET$, LINE_TWO_RET$ = ""
	LINE_ONE_RET$ = CHR$(RET%(TEXT_LOOP%)) IF RET%(TEXT_LOOP%)
	LINE_TWO_RET$ = CHR$(RET%(TEXT_LOOP% + 1%)) IF RET%(TEXT_LOOP% + 1%)
	RET%(TEXT_LOOP%), RET%(TEXT_LOOP% + 1%) = 0%
	TEXT$(TEXT_LOOP%) = TEXT$(TEXT_LOOP%) + LINE_ONE_RET$ + &
		TEXT$(TEXT_LOOP% + 1%) + LINE_TWO_RET$
	TEXT$(TEXT_LOOP% + 1%) = ""
	GOSUB CreateTextString
	GOSUB ParseText

	RETURN

	END SUB
