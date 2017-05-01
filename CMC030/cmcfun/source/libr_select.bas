1	%TITLE "Select Item Out of Library, or Edit Library"
	%SBTTL "LIBR_SELECT"
	%IDENT "V3.6a Calico"

	FUNCTION STRING LIBR_SELECT( &
		LIB_NAME$, &
		LIB_TITLE$, &
		LIB_HELP$, &
		OPTLIST$)

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
	!	This function will select an item out of the library,
	!	edit an existing item into a library, or edit an
	!	item in the library.
	!
	! Parameter:
	!
	!	LIB_NAME$
	!		The passed name of the library file to edit.
	!
	!	LIB_TITLE$
	!		Passed title to display on select window.
	!
	!	LIB_HELP$
	!		Variable passed which is used for generating help messages.
	!
	!	OPTLIST$
	!		Application can select which options
	!		to make available to the user.  Valid
	!		options are :
	!		.table
	!			"S" - Select a record in library
	!
	!			"M" - Edit a record in library
	!
	!			"C" - Create a new record in library
	!
	!			"H" - Help message for user
	!
	!			"X" - Exit to exit from function
	!		.endtable
	!		OPTLIST$ will default to all options if
	!		value is blank.
	!
	!
	!	Returned value
	!		A selected item (Item with an arrow on it).
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:LIBR_SELECT/NOLINE/NOOPT
	!	$ LIB FUNC_LIB:CMCFUN/REP LIBR_SELECT
	!	$ DELETE LIBR_SELECT.OBJ;*
	!
	!
	! Author:
	!
	!	08/24/87 - Kevin Handy
	!
	! Modification history:
	!
	!	07/22/92 - Kevin Handy
	!		Modification to fix bug in scrolling routine.
	!		Told scroll function there was 18 lines when
	!		there was only 15.
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/13/93 - Kevin Handy
	!		Added Getmaster and Putmaster options.
	!
	!	02/01/93 - Kevin Handy
	!		Modified to show key when going into "M"aintain.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/24/97 - Kevin Handy
	!		Lose unecessary externals
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/16/99 - Kevin Handy
	!		Initialize LIBR_INDEX$(0) to lose Alpha Warning.
	!
	!	06/30/99 - Kevin Handy
	!		Compile /NOOPT to lose problems
	!
	!	03/25/2004 - Kevin Handy
	!		Make <select> go into the "Maintain" option to
	!		make things less suprising.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE  SMG_SCROLL_CDD SELECT_SCROLL

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION LIBR_MAINTNODSR
	EXTERNAL LONG    FUNCTION LIBR_EXTRACTVAR
	EXTERNAL LONG    FUNCTION LIBR_INSERTVAR
	EXTERNAL LONG    FUNCTION DSPL_SCROLL

	!
	! Declare variables
	!
	DIM LIBR_INDEX$(2000%), RFA LIB_RFA(2000%)

	LIB_STORE_PROGRAM$ = SCOPE::PRG_PROGRAM
	LIB_STORE_ITEM$ = SCOPE::PRG_ITEM
	SCOPE::PRG_PROGRAM = LIB_HELP$

	%PAGE

1000	!*******************************************************************
	! Step 1 - Get an index of the file
	!*******************************************************************

	OPT2% = 0%

	LIBR_INDEX$(0%) = "0"
	CALL LIBR_INDEX(LIB_NAME$, "*", LIBR_INDEX$(), LIB_RFA())

	SELECT_COUNT% = VAL%(LIBR_INDEX$(0%))

2000	!*******************************************************************
	! Step 2 - Create a scrolling region to display junk in
	!*******************************************************************

	WIDTH% = LEN(LIB_TITLE$) + 2%

	WIDTH% = 20% IF WIDTH% < 20%

	WIDTH% = 40% IF WIDTH% > 40%

	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		15%, &
		WIDTH%, &
		SMG_SELECT%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_SELECT%, &
		LIB_TITLE$ &
	)

	SELECT_SCROLL::WINDOW		= SMG_SELECT%
	SELECT_SCROLL::TOP_ARRAY	= 1%

	IF SELECT_COUNT% = 0%
	THEN
		SELECT_SCROLL::BOT_ARRAY	= 1%
	ELSE
		SELECT_SCROLL::BOT_ARRAY	= SELECT_COUNT%
	END IF

	SELECT_SCROLL::SCROLL_TOP	= 1%
	SELECT_SCROLL::SCROLL_BOT	= 15%
	SELECT_SCROLL::BEG_ELEMENT	= 1%
	SELECT_SCROLL::END_ELEMENT	= SELECT_SCROLL::BOT_ARRAY
	SELECT_SCROLL::TOP_LINE		= 1%
	SELECT_SCROLL::CUR_LINE		= 1%
	SELECT_SCROLL::CUR_W_ROW	= 1%
	SELECT_SCROLL::CUR_W_COL	= 1%
	SELECT_SCROLL::FIND_LINE	= 1%
	SELECT_SCROLL::SMG_FLAG		= 0%
	SELECT_SCROLL::PROMPT		= "->"
	SELECT_SCROLL::VIDEO_COMP	= 0%
	SELECT_SCROLL::CHARSET		= 0%
	SELECT_SCROLL::DRAW_COLS	= ""

	V% = DSPL_SCROLL(SELECT_SCROLL, LIBR_INDEX$(), 0%, "PAINT")

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SELECT%, &
		SCOPE::SMG_PBID, &
		4%, &
		43% &
	)

3000	!*******************************************************************
	! Now, give the user his options
	!*******************************************************************

	!
	! Ask for option
	!
	SCOPE::PRG_ITEM = ""
	OPTLIST$ = "Select Maintain Create Getmaster Putmaster Help eXit" &
		IF TRM$(OPTLIST$) = ""
	INP$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT2%, FLAGW%)

	!
	! Handle funny terminators
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Cursor movement
	!
	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
		SMG$K_TRM_F18, SMG$K_TRM_F19

		V% = DSPL_SCROLL(SELECT_SCROLL, LIBR_INDEX$(), &
			SCOPE::SCOPE_EXIT, "PAINT")

		GOTO 3000

	!
	! Exit characters
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		GOTO ExitProgram

	!
	! Select character
	!
	CASE SMG$K_TRM_SELECT
		INP$ = "M"
 !		GOTO ExitSelect

	!
	! Good characters
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad characters
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 3000

	END SELECT

4000	!
	! Process user options
	!
	SELECT INP$

	!
	! Select record
	!
	CASE "S"
		IF TRM$(LIBR_INDEX$(SELECT_SCROLL::CUR_LINE)) = ""
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"A form must be created before selecting it.", &
				0%)
			GOTO 3000
		END IF

		GOTO ExitSelect

	!
	! Edit current record
	!
	CASE "M"
		IF TRM$(LIBR_INDEX$(SELECT_SCROLL::CUR_LINE)) = ""
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"A form must be created before maintaining it.", 0%)
			GOTO 3000
		END IF

		LIB_KEY$ = LIBR_INDEX$(SELECT_SCROLL::CUR_LINE)

		V% = LIBR_MAINTNODSR(LIB_NAME$, LIB_KEY$, &
			LIB_TITLE$ + " - " + LIB_KEY$, 2%)

		GOSUB Reload

	!
	! Create new text file
	!
	CASE "C"

4100		LIB_KEY$ = ENTR_3STRING(SCOPE, SCOPE::SMG_OPTION, &
			"", "File to create", SPACE$(49%), 0%, "", "")

		!
		! Handle funny terminators
		!
		SELECT SCOPE::SCOPE_EXIT

		!
		! Exit characters
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOTO 3000

		!
		! Good characters
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad characters
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 4100

		END SELECT

		IF TRM$(LIB_NAME$) = ""
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"A form name must be given before process can continue", 0%)
			GOTO 3000
		END IF

		V% = LIBR_MAINTNODSR(LIB_NAME$, LIB_KEY$, LIB_TITLE$, 2%)

		GOSUB Reload

	!
	! Getmaster
	!
	CASE "G"
		!
		! Get name of system library
		!
		GOSUB GetMasterName

		!
		! Ask for item to get
		!
		FINAME$ = TRM$(ENTR_3STRING(SCOPE, SCOPE::SMG_OPTION, "", &
			"Form to get", SPACE$(32%), 0%, "", ""))

		SELECT SCOPE::SCOPE_EXIT

		!
		! Exit keys
		!
		CASE SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 3000

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 3000

		END SELECT

		!
		! Extract file
		!
		ST% = LIBR_EXTRACTVAR(TEMP$, TEMP_TEXT$, FINAME$)

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Error in extract " + NUM1$(ST%), 0%)
			GOTO 3000
		END IF

		!
		! Insert file
		!
		ST% = LIBR_INSERTVAR(LIB_NAME$, TEMP_TEXT$, FINAME$)

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Error in Insert " + NUM1$(ST%), 0%)
			GOTO 3000
		END IF

		GOSUB Reload

	!
	! Putmaster
	!
	CASE "P"
		!
		! Get name of system library
		!
		GOSUB GetMasterName

		!
		! Ask for item to get
		!
		FINAME$ = SPACE$(32%)
		LSET FINAME$ = LIBR_INDEX$(SELECT_SCROLL::CUR_LINE)
		FINAME$ = TRM$(ENTR_3STRING(SCOPE, SCOPE::SMG_OPTION, "", &
			"Form to put", FINAME$, 0%, "", ""))

		SELECT SCOPE::SCOPE_EXIT

		!
		! Exit keys
		!
		CASE SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 3000

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 3000

		END SELECT

		!
		! Extract file
		!
		ST% = LIBR_EXTRACTVAR(LIB_NAME$, TEMP_TEXT$, FINAME$)

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Error in extract " + NUM1$(ST%), 0%)
			GOTO 3000
		END IF

		!
		! Insert file
		!
		ST% = LIBR_INSERTVAR(TEMP$, TEMP_TEXT$, FINAME$)

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Error in Insert " + NUM1$(ST%), 0%)
			GOTO 3000
		END IF

		GOSUB Reload

	!
	! Help
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", &
			SCOPE::PRG_IDENT, SCOPE::PRG_PROGRAM, "HELP")

	!
	! Exit
	!
	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO 3000

	%PAGE

 Reload:
	!
	! Set lib index to blank
	!
	TEST_COUNT% = SELECT_COUNT%

	CALL LIBR_INDEX(LIB_NAME$, "*", LIBR_INDEX$(), LIB_RFA())

	SELECT_COUNT% = VAL%(LIBR_INDEX$(0%))

	LIBR_INDEX$(LOOP%) = SPACE$(10%) &
		FOR LOOP% = SELECT_COUNT% + 1% TO TEST_COUNT%

	IF TEST_COUNT% > SELECT_COUNT%
	THEN
		SELECT_SCROLL::FIND_LINE = 1%

		V% = DSPL_SCROLL(SELECT_SCROLL, LIBR_INDEX$(), 0%, "PAINT")

	END IF

	SELECT_SCROLL::BOT_ARRAY	= SELECT_COUNT%
	SELECT_SCROLL::BOT_ARRAY	= 1% IF SELECT_COUNT% = 0%

	SELECT_SCROLL::END_ELEMENT	= SELECT_SCROLL::BOT_ARRAY

	IF TEST_COUNT% <= SELECT_COUNT%
	THEN
		V% = DSPL_SCROLL(SELECT_SCROLL, LIBR_INDEX$(), 0%, "PAINT")
	END IF

	RETURN

	%PAGE

	!*******************************************************************
	! Figure out a good name for the master file.
	! Return as TEMP$.
	!*******************************************************************
 GetMasterName:

	TEMP$ = LIB_NAME$

	!
	! Strip off device names
	!
 GetMasterName1:
	V% = INSTR(1%, TEMP$, ":")
	IF V%
	THEN
		TEMP$ = RIGHT(TEMP$, V% + 1%)
		GOTO GetMasterName1
	END IF

	!
	! Strip off account names
	!
 GetMasterName2:
	V% = INSTR(1%, TEMP$, "]")
	IF V%
	THEN
		TEMP$ = RIGHT(TEMP$, V% + 1%)
		GOTO GetMasterName2
	END IF

	!
	! Strip off version numbers
	!
	V% = INSTR(1%, TEMP$, ";")
	IF V%
	THEN
		TEMP$ = LEFT(TEMP$, V% - 1%)
	END IF

	TEMP$ = "CMC:" + TEMP$

	RETURN

	%PAGE

	!*******************************************************************
	! Exit and select current record
	!*******************************************************************
 ExitSelect:
	LIBR_SELECT = LIBR_INDEX$(SELECT_SCROLL::CUR_LINE)


 ExitProgram:
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SELECT%)

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	SCOPE::PRG_PROGRAM = LIB_STORE_PROGRAM$
	SCOPE::PRG_ITEM = LIB_STORE_ITEM$

	END FUNCTION
