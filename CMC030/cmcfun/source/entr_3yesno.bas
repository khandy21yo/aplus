1	%TITLE "Function to Enter Yes/No Response"
	%SBTTL "ENTR_3YESNO"
	%IDENT "V3.6a Calico"

	FUNCTION STRING ENTR_3YESNO(SCOPE_STRUCT SCOPE, &
		LONG XX_VDID, STRING OP_CPOS, &
		STRING OP_PROMPT, STRING OP_DEFLT, LONG OP_FLAG, &
		STRING OP_XFORMAT, STRING OP_XDEFLT)

	!
	! COPYRIGHT (C) 1987 BY
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Enters Yes or No.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	OP_CPOS
	!		Position to display data.
	!		"" - Do not display on top area
	!
	!	OP_FLAG
	!	.table
	!		  1 - Print data without entry
	!		  4 - Keypunch mode
	!		  8 - Timeout
	!		 32 - Use default values
	!		 64 - Don't display
	!		128 - Return result in OP_XDEFLT
	!	.endtable
	!
	!	XX_VDID
	!		Passed variable that creates or deletes the window
	!		that holds the string.
	!
	!	OP_PROMPT
	!		The passed string used for the prompt and its initialization
	!
	!	OP_DEFLT
	!		One of the passed defaults for the data.
	!
	!	OP_XFORMAT
	!		The passed format for the data.
	!
	!	OP_XDEFLT
	!		The passed default form the data size.
	!
	!
	!	This function returns a yes or no value to the screen.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_3YESNO/NOLINE/NOOPT
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_3YESNO
	!	$ DELETE ENTR_3YESNO.OBJ;*
	!
	! AUTHOR:
	!
	!	07/09/85 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	01/26/86 - Kevin Handy
	!		Added default value stuff.
	!
	!	07/14/87 - Kevin Handy
	!		Modified to return value in OP_XDEFLT
	!
	!	07/28/87 - Kevin Handy
	!		Fixed problem of auto-exit not displaying
	!		the value entered.
	!
	!	12/11/89 - Kevin Handy
	!		Modified to fit in sharable library.
	!
	!	01/04/89 - Kevin Handy
	!		Modified so that undefined (Not Y or N)
	!		entrys will not display as "No".  Data in
	!		file looked OK, but it was this function
	!		hiding what was really there.
	!
	!	10/09/90 - Frank F. Starman
	!		Restore cursor after Help.
	!
	!	03/25/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/19/95 - Kevin Handy
	!		Lose bad external def's that caused errors.
	!		Fix first param to entr_4entry.
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/16/99 - Kevin Handy
	!		Dimension VTEXT$ array to lose Alpha Warning.
	!
	!	06/30/99 - Kevin Handy
	!		Use /NOOPT on compile to lose problems
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	DECLARE LONG XPOS, YPOS, Y1POS
	DIM VTEXT$(5%)

	!
	! Split out cursor positioning function
	!
	CALL DSPL_SPLITCURSOR(OP_CPOS, X%, Y%)
	XPOS = X%
	YPOS = Y%

	!
	! Decide which value to use
	!
	IF (OP_FLAG AND 32%)
	THEN
		GETS$ = OP_XDEFLT
	ELSE
		GETS$ = OP_DEFLT
	END IF

	!
	! Don't enter
	!
	GOTO 3000 IF (OP_FLAG AND 1%)

	!
	! Display original value
	!
	SMG_STATUS% = SMG$PUT_CHARS( &
		XX_VDID,	! Window &
		FORMAT$(FNYN$(GETS$), OP_XFORMAT),	! Data &
		XPOS,		! Line &
		YPOS,		! Column &
		0%,		! Erase flag &
		SMG$M_REVERSE)	! Attributes &
			IF (OP_CPOS <> "") AND ((OP_FLAG AND 64%) = 0%)

1000	!
	! Initilization/prompt
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS &
		(SCOPE::SMG_OPTION, OP_PROMPT + " <Yes/No>:", 1%, 1%, 1%)

	!
	! Unhide cursor
	!
	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
	Y1POS = LEN(OP_PROMPT) + 12%

1100	!
	! Normal entry
	!
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
		FNYN$(GETS$), 1%, Y1POS, 0%)
	SMG_STATUS% = SMG$SET_CURSOR_ABS(SCOPE::SMG_OPTION, 1%, Y1POS)

	JUNK$ = GETS$

	GETS% = ENTR_4ENTRY(SCOPE, &
		SCOPE::SMG_OPTION BY VALUE, 256% BY VALUE)
	GETS% = ENTR_4SPECIALKEYS(SCOPE, &
		SCOPE::SMG_OPTION BY VALUE, 256% BY VALUE, GETS% BY VALUE)

	GOTO 1100 IF GETS% = 0%

	IF (GETS% < 32%) OR (GETS% > 126%)
	THEN
		GETS$ = JUNK$
		SCOPE::SCOPE_EXIT = GETS%
	ELSE
		GETS$ = EDIT$(CHR$(GETS%), 32%)
		SCOPE::SCOPE_EXIT = 0%
	END IF
	GETS$ = "N" IF GETS$ <> "Y"

1150	!
	! Test Input
	!
	SELECT GETS%
	!
	! Normal character typed
	!
	CASE 32% TO 126%
		GOTO 1100 IF (OP_FLAG AND 4%) = 0%
		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, FNYN$(GETS$), &
			1%, Y1POS, 0%)
		SMG_STATUS% = SMG$SET_CURSOR_ABS(SCOPE::SMG_OPTION, &
			1%, Y1POS)

	!
	! List Choices
	!
	CASE SMG$K_TRM_F14
		VTEXT$(0%) = "2"
		VTEXT$(1%) = "Y    Yes"
		VTEXT$(2%) = "N    No"
		X% = ENTR_3CHOICE(SCOPE, "", "", VTEXT$(), GETS$, &
			138%, "Value  Description", "006", 0%)

		GETS$ = EDIT$(LEFT(VTEXT$(X%), 5% - 3%), &
			4% + 8% + 32% + 128% + 256%) IF X% > 0%
		GOTO 1100
	END SELECT

	IF (GETS$ <> "Y") AND (GETS$ <> "N")
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please enter 'Y' or 'N'", 1%)
		GOTO 1100
	END IF

3000	!
	! Exit function
	!
	SMG_STATUS% = SMG$PUT_CHARS( &
		XX_VDID,	! Window &
		FORMAT$(FNYN$(GETS$), OP_XFORMAT),	! Data &
		XPOS,		! Line &
		YPOS,		! Column &
		0%,		! Erase flag &
		SMG$M_BOLD	! Attributes &
	) IF (OP_CPOS <> "") AND ((OP_FLAG AND 64%) = 0%)

	SMG_STATUS% = SMG$FLUSH_BUFFER(SCOPE::SMG_PBID)

	!
	! Hide cursor
	!
	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	ENTR_3YESNO = GETS$

	OP_XDEFLT = GETS$ IF (OP_FLAG AND 128%)

	EXIT FUNCTION

	!
	! Function To Expand 'Y' and 'N' into three characters YES/NO
	!
	DEF FNYN$(STRX$)
		SELECT STRX$
		CASE "Y"
			FNYN$ = "Yes"
		CASE "N"
			FNYN$ = "No "
		CASE ELSE
			FNYN$ = STRX$ + "  "
		END SELECT
	END DEF

	END FUNCTION
