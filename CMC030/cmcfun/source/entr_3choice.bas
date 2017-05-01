1	%TITLE "Select a Choice from a List"
	%SBTTL "ENTR_3CHOICE"
	%IDENT "V3.6a Calico"

	FUNCTION INTEGER ENTR_3CHOICE(SCOPE_STRUCT SCOPE, STRING WPOSITION, &
		STRING WSIZE, STRING TEXT(), STRING SELECTED, &
		LONG FLAG, STRING TITLE, STRING DRAW_COLS, &
		LONG BORDER_POS)

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
	!	.B
	!	Point the arrow at the item that you wish to select, and press the
	!	Select key.
	!	...LM +5
	!	..This function is used to display a selection list
	!	..in it's own window, and allow the user to select an
	!	..item from the selection list. The function returns
	!	..a -1% if no line was selected otherwise it returns
	!	..a number greater than -1% equal to the array element
	!	..number.
	!	...literal
	!	..PCOL
	!	..|
	!	..v
	!	..PROW --------->+--------BORDER TITLE-----------+
	!	..###############|#######INTERNAL TITLE##########| -----
	!	..###############+-------------------------------+ ^
	!	..###############|###############################| |
	!	..###############|###############################| WROWS
	!	..###############|###############################| |
	!	..###############|###############################| v
	!	..###############|###############################| -----
	!	..###############+-------------------------------+
	!	..###############|<------------WCOLS------------>|
	!	...end literal
	!	...B
	!	..The values of FLAG are as follows:
	!	...TABLE 3,25
	!	...TE
	!	..0 Normal.
	!	...Te
	!	..1 Encode the lines.
	!	...Te
	!	..2 Draw lines.
	!	...Te
	!	..8 No begin/end updates on scrolling.
	!	...Te
	!	..16 Border Title.
	!	...Te
	!	..32 Returns only on EXIT with all selected
	!	...Te
	!	..values in the parameter SELECTED as
	!	...Te
	!	.."1,2,3,5,8,4,".
	!	...Te
	!	..64 Reverse window	no border.
	!	...Te
	!	..128 Reverse window	border.
	!	...Te
	!	..256 Dont repaint
	!	...end table
	!	...lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	WPOSITION
	!		The passed position at the begining.
	!
	!	WSIZE
	!		The passed size of the display section.
	!
	!	TEXT()
	!		Used to check against tables.
	!
	!	FLAG
	!
	!		.table
	!		0 Normal.
	!
	!		1 Encode the lines.
	!
	!		2 Draw lines.
	!
	!		8 No begin/end updates on scrolling.
	!
	!		16 Border Title.
	!
	!		32 Returns only on EXIT with all selected
	!		values in the parameter SELECTED as
	!		"1,2,3,5,8,4,".
	!
	!		64 Reverse window - no border.
	!
	!		128 Reverse window - border.
	!
	!		256 Dont repaint
	!
	!		512 Display only
	!		.end table
	!
	!	TITLE
	!		The passed title for the choice menu.
	!
	!	DRAW_COLS
	!		Used to draw the columns of the menu.
	!
	!	BORDER_POS
	!		Draws the border position.
	!
	!	SELECTED
	!		Is the choice the user selects.
	!
	!	Returned values
	!	A number greater that -1% equal to the array element.
	!
	! Example:
	!
	!	X% = ENTR_3CHOICE('', '', VTEXT$(), GETS$, 138%, &
	!		"Example", DRAW_C$, '0'L)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_3CHOICE/LINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_3CHOICE
	!	$ DELETE ENTR_3CHOICE.OBJ;*
	!
	! Author:
	!
	!	02/05/86 - Kevin Handy and B. Craig Larsen
	!
	! Modification history:
	!
	!	02/10/89 - Kevin Handy
	!		Modified for changes in ENTR_NOLSTRING.
	!
	!	09/22/89 - Kevin Handy
	!		Modified to use ENTR_ENTER instead of
	!		ENTR_NOLSTRING.
	!
	!	05/08/90 - Frank F. Starman
	!		Find also lowercase characters. Set blinking
	!		video for matching strings. Replace ENTR_3MESSAGE
	!		function with HELP_3MESSAGE. Check for FIND if
	!		already in FIND string.
	!
	!	02/05/92 - Frank F. Starman
	!		Add flag 512
	!
	!	02/20/92 - Frank F. Starman
	!		Allow exit with the Magic key.
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/14/92 - Kevin Handy
	!		Clean up vars (checkvar)
	!
	!	03/24/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/19/93 - Kevin Handy
	!		Modified to pass SMG$K_TRM_FIND through DSPL_SCROLL
	!		instead of "FIND"
	!
	!	03/06/95 - Kevin Handy
	!		Modified to look for the number of elements
	!		passed through in TEXT(0%), so that blank
	!		entries will be allowed.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!		Fix a lot of goofy formatting
	!
	!	08/19/98 - Kevin Handy
	!		Trap control/Z like a F10.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/08/99 - Kevin Handy
	!		Use 'WHEN ERROR IN'
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE	SMG_SCROLL_CDD	SMG_SCROLL

	DECLARE LONG SMG_CHOICE, OLD_OPTION, OLD_MESSAGE, BEG_ELEM, &
		END_ELEM, PROW, PCOL, WROWS, WCOLS, COLS, ELEN, &
		WIDTH, HEIGHT

	EXTERNAL LONG FUNCTION DSPL_SCROLL

5	OLD_OPTION = SCOPE::SMG_OPTION	! Save the OPTION  virtual display
	OLD_MESSAGE= SCOPE::SMG_MESSAGE ! Save the MESSAGE virtual display

	IF (FLAG AND 512%) = 0%
	THEN
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, &
			SCOPE::SMG_OPTION)
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
			SCOPE::SMG_PBID, 21%, 1%)

		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, &
			SCOPE::SMG_MESSAGE)
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SCOPE::SMG_MESSAGE, &
			SCOPE::SMG_PBID, 23%, 1%)
	END IF

10	ENTR_3CHOICE = -1%

	OLD_IDENT$ = SCOPE::PRG_IDENT		! Save the COMMON Ident
	OLD_PROGRAM$ = SCOPE::PRG_PROGRAM	! Save the COMMON Program
	OLD_ITEM$ = SCOPE::PRG_ITEM		! Save the COMMON item

	SCOPE::PRG_IDENT	= "H"
	SCOPE::PRG_PROGRAM	= "ENTR_3CHOICE"
	SCOPE::PRG_ITEM		= "HELP"

	CALL DSPL_SPLITCURSOR(WSIZE,     WROWS, WCOLS)
	CALL DSPL_SPLITCURSOR(WPOSITION, PROW,  PCOL)

	BEG_ELEM = 1%

	IF (TEXT(0%) = "")
	THEN
		I% = 1%
		WHILE EDIT$(TEXT(I%), 4% + 128%) <> ""
			I% = I% + 1%
		NEXT
	ELSE
		WHEN ERROR IN
			I% = VAL%(TEXT(0%)) + 1%
		USE
			CONTINUE 20
		END WHEN
	END IF

20	END_ELEM = I% - 1%

	GOTO ExitFunction IF END_ELEM = 0%

	!
	! Determine the window size
	!
	AL% = LEN(EDIT$(TEXT(I%), 4% + 128%)) + 2% &
		IF LEN(EDIT$(TEXT(I%), 4% + 128%)) + 2% > AL% &
		FOR I% = 1% TO END_ELEM
	AL% = LEN(EDIT$(TITLE, 4%)) &
		IF LEN(EDIT$(TITLE, 4%)) > AL%

	WCOLS = AL% IF (AL% < WCOLS OR WCOLS < 1%) AND AL% > 0%
	WCOLS = 78% IF WCOLS > 78% OR WCOLS < 1%

	WROWS = END_ELEM - BEG_ELEM + 1% IF WROWS < 1%
	WROWS = WROWS + 2% IF (FLAG AND 16%) = 0%
	WROWS = 14% IF WROWS > 14% OR WROWS < 1%

	!
	! Determine the window position
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID,, &
		WIDTH,, HEIGHT)
	PROW = (HEIGHT / 2%) - (WROWS / 2%) + 2% &
		IF (PROW < 1% OR PROW > HEIGHT) AND HEIGHT > 0%
	PROW = 4% IF WROWS + PROW > 19%
	PCOL = (WIDTH  / 2%) - (WCOLS / 2%) + 1% &
		IF (PCOL < 1% OR PCOL > WIDTH)  AND WIDTH > 0%

50	!
	! Create the window
	!
	IF (FLAG AND 64%)
	THEN
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(WROWS, WCOLS, &
			SMG_CHOICE,, SMG$M_REVERSE)
	ELSE
		IF (FLAG AND 128%)
		THEN
			SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(WROWS, WCOLS, &
				SMG_CHOICE, SMG$M_BORDER, SMG$M_REVERSE)
		ELSE
			SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(WROWS, WCOLS, &
				SMG_CHOICE, SMG$M_BORDER)
		END IF
	END IF

	!
	! Output the title and title line
	!
	IF (FLAG AND 16%)
	THEN
		SMG_STATUS% = SMG$LABEL_BORDER(SMG_CHOICE, TITLE, BORDER_POS)
	ELSE
		SMG_STATUS% = SMG$PUT_CHARS(SMG_CHOICE, TITLE, 1%, 1%)
		SMG_STATUS% = SMG$DRAW_LINE(SMG_CHOICE, 2%, 1%, 2%, WCOLS)
	END IF

	SMG_SCROLL::WINDOW	= SMG_CHOICE
	SMG_SCROLL::SCROLL_TOP	= 3%
	SMG_SCROLL::SCROLL_TOP	= 1% IF (FLAG AND 16%)
	!SMG_SCROLL::SCROLL_BOT	= SMG_SCROLL::SCROLL_TOP + WROWS
	SMG_SCROLL::SCROLL_BOT	= WROWS

	SMG_SCROLL::TOP_ARRAY	= BEG_ELEM
	SMG_SCROLL::BOT_ARRAY	= END_ELEM
	SMG_SCROLL::TOP_LINE	= 1%
	SMG_SCROLL::BEG_ELEMENT	= BEG_ELEM
	SMG_SCROLL::END_ELEMENT	= END_ELEM

	EC_SELECT$ = EDIT$(SELECTED, 4% + 8% + 32% + 128% + 256%)
	SELECTED   = "" IF (FLAG AND 32%)
	IF DRAW_COLS = ""
	THEN
		SMG_SCROLL::CUR_LINE, SMG_SCROLL::FIND_LINE = I% &
			IF EC_SELECT$ = EDIT$(TEXT(I%), &
			4% + 8% + 32% + 128% + 256%) &
			FOR I% = 0% TO END_ELEM
	ELSE
		SMG_SCROLL::CUR_LINE, SMG_SCROLL::FIND_LINE = I% &
			IF EC_SELECT$ = EDIT$(LEFT(TEXT(I%), &
			VAL%(LEFT(DRAW_COLS, 3%)) - 3%), &
			4% + 8% + 32% + 128% + 256%) &
			FOR I% = 0% TO END_ELEM
	END IF
	SMG_SCROLL::CUR_LINE, SMG_SCROLL::FIND_LINE = SMG_SCROLL::TOP_LINE &
		IF SMG_SCROLL::CUR_LINE < 1%

	SMG_SCROLL::CUR_W_COL	= 1%
	SMG_SCROLL::CUR_W_ROW	= 1%

	SMG_SCROLL::PROMPT	= "->"
	SMG_SCROLL::PROMPT	= "  " IF (FLAG AND 512%)
	SMG_SCROLL::DRAW_COLS	= DRAW_COLS
	SMG_SCROLL::SMG_FLAG	= 0%
	SMG_SCROLL::SMG_FLAG	= SMG_SCROLL::SMG_FLAG OR 1% IF FLAG AND 1%
	SMG_SCROLL::SMG_FLAG	= SMG_SCROLL::SMG_FLAG OR 2% IF FLAG AND 2%
	SMG_SCROLL::SMG_FLAG	= SMG_SCROLL::SMG_FLAG OR 8% IF FLAG AND 8%

	V% = DSPL_SCROLL(SMG_SCROLL, TEXT(), 0%, "PAINT")

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_CHOICE, SCOPE::SMG_PBID, &
		PROW, PCOL)

 Menu:
	GOTO ExitFunction IF (FLAG AND 512%)
	IF (FLAG AND 32%)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"<FIND>,<SELECT - * indicates selected>,<Action Keys> or", 12%)
	ELSE
		CALL ENTR_3MESSAGE(SCOPE, &
			"<FIND>,<SELECT>,<Action Keys> or", 12%)
	END IF
 CheckKey:
	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_UP,		! Uparrow		&
		SMG$K_TRM_DOWN,		! Downarrow		&
		SMG$K_TRM_LEFT,		! Left arrow		&
		SMG$K_TRM_RIGHT,	! Right arrow		&
		SMG$K_TRM_PREV_SCREEN,	! Previous screen	&
		SMG$K_TRM_NEXT_SCREEN,	! Next screen		&
		SMG$K_TRM_F18,		! Top			&
		SMG$K_TRM_F19		! Bottom

		V% = DSPL_SCROLL(SMG_SCROLL, TEXT(), SCOPE::SCOPE_EXIT, "")

	!
	! Normal Exit
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_F7

		SCOPE::SCOPE_EXIT = SMG$K_TRM_DO
		GOTO ExitFunction

	!
	! Selected Exit
	!
	CASE SMG$K_TRM_SELECT

		ENTR_3CHOICE = SMG_SCROLL::CUR_LINE
		IF (FLAG AND 32%)
		THEN
			SELECTED = SELECTED + &
				NUM1$(SMG_SCROLL::CUR_LINE) + ","

			TEXT(SMG_SCROLL::CUR_LINE) = "* " + &
				RIGHT(TEXT(SMG_SCROLL::CUR_LINE), 3%)

			V% = DSPL_SCROLL(SMG_SCROLL, TEXT(), 0%, "PAINT")

			GOTO Menu
		END IF

		SCOPE::SCOPE_EXIT = SMG$K_TRM_DO
		GOTO ExitFunction

	!
	! Exit Key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		GOTO ExitFunction

	!
	! Magic Key, allow to do something magical
	!
	CASE SMG$K_TRM_F17

		GOTO ExitFunction

	!
	! Find Key
	!
	CASE SMG$K_TRM_FIND
 Find1:		!
		! Search for certain keyword
		!
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, "Search for: ", &
			1%, 1%, 1%)

		FINP$ = SPACE$(67%)

		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, 13%, &
			FINP$, -1%, 16% + 4096%)

		! ^C, ^Z, Exit
		CASE SMG$K_TRM_CTRLC, &
			SMG$K_TRM_F8,	SMG$K_TRM_F10, &
			SMG$K_TRM_CTRLZ, SMG$K_TIMEOUT

			GOTO FindEnd

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, &
			SMG$K_TRM_F7 ! Good keys

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO Find1
		END SELECT

		FINP$ = EDIT$(FINP$, 4% + 8% + 128% + 256%)

		GOTO Menu IF FINP$ = ""

		COLS = 0%

		FOR F% = SMG_SCROLL::BEG_ELEMENT TO SMG_SCROLL::END_ELEMENT

 Find2:			COLS = INSTR((COLS + 1%), EDIT$(TEXT(F%), 32%), FINP$)

			IF COLS
			THEN
				SMG_SCROLL::FIND_LINE = F%
				ELEN = DSPL_SCROLL(SMG_SCROLL, TEXT(), &
					SMG$K_TRM_FIND, "FIND")

				SMG_STATUS% = SMG$PUT_CHARS(SMG_CHOICE, &
					MID$(TEXT(F%), COLS, LEN(FINP$)), &
					SMG_SCROLL::CUR_W_ROW, &
					COLS + 2%,, SMG$M_BLINK)

 Find3:				CALL HELP_34MESSAGE(SCOPE, "string match", &
					"S", "ENTR_3CHOICE", "", "STRMATCH")
	!++
	! Success:STRMATCH
	!	^*String Match\*
	!	.b
	!	.lm +5
	!	Matching string has been found.
	!	.table 3,25
	!	.te
	!	<Exit> key terminates searching
	!	.te
	!	<FIND> key prompts for a new searching string
	!	.te
	!	<RESUME> key will continue for the next match.
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--

				SELECT SCOPE::SCOPE_EXIT
				! ^C, ^Z, Exit
				CASE SMG$K_TRM_CTRLC, &
					SMG$K_TRM_F8,	SMG$K_TRM_F10, &
					SMG$K_TRM_CTRLZ

					ELEN = DSPL_SCROLL(SMG_SCROLL, TEXT(), &
						0%, "PAINT")

					GOTO FindEnd

				CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, &
					SMG$K_TRM_F7	! Good keys

				!
				! Find Key
				!
				CASE SMG$K_TRM_FIND
					ELEN = DSPL_SCROLL(SMG_SCROLL, TEXT(), 0%, &
						"PAINT")
					GOTO CheckKey

				CASE ELSE
					CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
					GOTO Find3
				END SELECT

				ELEN = DSPL_SCROLL(SMG_SCROLL, TEXT(), 0%, &
					"PAINT")

				GOTO Find2
			END IF
		NEXT F%

		CALL HELP_34MESSAGE(SCOPE, "no string match", "I", &
			"ENTR_3CHOICE", "", "NOSTRMATCH")
	!++
	! Information:NOSTRMATCH
	!	^*No String Match\*
	!	.b
	!	.lm +5
	!	No matching string has been found.
	!	.b
	!	Press <RESUME> to continue.
	!	.lm -5
	!
	! Index:
	!
	!--

 FindEnd:
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	!
	! Otherwise, it is a bad key
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
	END SELECT

	GOTO Menu

 ExitFunction:
	!
	! Delete all displays
	!
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, SCOPE::SMG_PBID) &
		UNLESS (FLAG AND 512%)
	SCOPE::SMG_OPTION = OLD_OPTION	! Restore OPTION  virtual display
	SCOPE::SMG_MESSAGE = OLD_MESSAGE ! Restore MESSAGE virtual display

	SCOPE::PRG_IDENT	= OLD_IDENT$		! Restore the COMMON Ident
	SCOPE::PRG_PROGRAM	= OLD_PROGRAM$		! Restore the COMMON Program
	SCOPE::PRG_ITEM		= OLD_ITEM$		! Restore the COMMON item

	EXIT FUNCTION

	END FUNCTION
