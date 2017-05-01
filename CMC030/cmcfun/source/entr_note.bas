1	%TITLE "Function to Enter a Note"
	%SBTTL "ENTR_NOTE"
	%IDENT "V3.6a Calico"

	FUNCTION STRING ENTR_NOTE(XX_VDID%, CPOS$, PROMPT$, &
		XSTART$, FLAG%, XFORMAT$, DEFLT$)

	!
	!		COPYRIGHT (C) 1984 BY
	!		Computer Management Center, Idaho Falls, Idaho.
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
	!	Enters/Displays a note on the screen. This function
	!	uses the NOTE_PAD function so all of it's editing features
	!	are available to the user.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	XX_VDID%
	!		The passed virtual display ID number.
	!
	!	POS$
	!		The  final position of the data on screen.
	!		Format as 'ROW;COL'.  If POS$='' (blank), data will
	!		be entered but not displayed above on screen.
	!
	!	PROMPT$
	!		Prompt string. (Will be followed by
	!		'ALPHA:', i.e. 'ADD' will generate 'ADD ALPHA:')
	!
	!	FLAG%
	!		An integer flag word.
	!		.table
	!			  1 - Don't enter data (display only?)
	!			 32 - Use default value
	!			 64 - Don't display
	!			128 - Return final value in default
	!		.endtable
	!
	!	FORMAT$
	!		A BASIC+2 print-using format for string.
	!
	!	DEFAULT$
	!		Default data value to use if <CR> is typed.
	!
	!
	!	Returns the string entered.
	!
	!	Returns DEFAULT$ if bit one is set in FLAG%.
	!
	! Example:
	!
	!	CN$ = ENTR_NOTE(SMG_SCREEN_DATA%, '3;19',"Customer Number", &
	!		CUSTOM.NUMBER$, FLAG%, "'E")
	!
	! Index:
	!	.X Note pad>Enter
	!	.x Enter>Note Pad
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_NOTE/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP ENTR_NOTE
	!	$ DELETE ENTR_NOTE.OBJ;*
	!
	! Author:
	!
	!	07/24/87 - Kevin Handy
	!
	! Modification history:
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update source to V3.6 standards.
	!		Change XX_VDID to XX_VDID%. Note that this changes
	!		the calling parameters of the function from float
	!		to long.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/16/98 - Kevin Handy
	!		Lose an excess number of %PAGE
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Includes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%PAGE

	!
	! Split out cursor positioning function
	!
	CALL DSPL_SPLITCURSOR(CPOS$, XPOS%, YPOS%)

	CALL DSPL_SPLITCURSOR(XFORMAT$, XSIZ%, YSIZ%)

	!
	! Decide between default value and normal value
	!
	IF (FLAG% AND 32%)
	THEN
		GETS$ = TRM$(DEFLT$)
	ELSE
		GETS$ = TRM$(XSTART$)
	END IF

	!
	! If display only
	!
	GOTO L3000 IF (FLAG% AND 1%)

	!
	! Initial display of item in reverse video
	!
	!	IF (CPOS$ <> "") AND ((FLAG% AND 64%) = 0%)
	!	THEN
	!		ATTR% = SMG$M_REVERSE
	!		GOSUB DisplayNote
	!	END IF

	!
	! Normal entry
	!
	JUNK$ = NUM1$(XPOS% - 1%) + ";" + NUM1$(YPOS% + 1%)
	CALL NOTE_PAD(JUNK$, XFORMAT$, GETS$, 0%, &
		"Note editor, press <EXIT> to exit.", 0%)
	SCOPE::SCOPE_EXIT = 12% IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLZ)

 L3000:	!
	! Exit function
	!

	!
	! Re-diaplay data on screen
	!
	IF (CPOS$ <> "") AND ((FLAG% AND 64%) = 0%)
	THEN
		ATTR% = SMG$M_BOLD
		GOSUB DisplayNote
	END IF

	!
	! Return value
	!
	ENTR_NOTE = GETS$

	!
	! Return in default (string format) if flag is set
	!
	DEFLT$ = GETS$ IF (FLAG% AND 128%)

	EXIT FUNCTION

	%PAGE

 DisplayNote:
	!*******************************************************************
	! Display a note on the screen
	!*******************************************************************

	JUNK$ = GETS$
	TEXT_LOOP% = 0%

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(XX_VDID%)

	SMG_STATUS% = SMG$ERASE_DISPLAY(XX_VDID%, &
		XPOS%, YPOS%, XPOS% + XSIZ% - 1%, YPOS% + YSIZ% - 1%)

	!
	! While there is still text to be had
	!
	WHILE JUNK$ <> ""

		!
		! Search for a return
		!
		FIND% = INSTR(1%, LEFT(JUNK$, YSIZ%), '13'C)
		IF FIND%
		THEN
			!
			! Handle the return
			!
			SMG_STATUS% = SMG$PUT_CHARS(XX_VDID%, &
				LEFT(JUNK$, FIND% - 1%), &
				XPOS% + TEXT_LOOP%, YPOS%, , ATTR%)
			JUNK$ = RIGHT(JUNK$, FIND% + 1%)
			TEXT_LOOP% = TEXT_LOOP% + 1%
		ELSE
			!
			! No return, so search for a break
			!
			IF LEN(JUNK$) < YSIZ%
			THEN
				!
				! Rest of test will fit on screen
				!
				SMG_STATUS% = SMG$PUT_CHARS(XX_VDID%, &
					JUNK$, XPOS% + TEXT_LOOP%, YPOS%, , ATTR%)
				JUNK$ = ""
				TEXT_LOOP% = TEXT_LOOP% + 1%
			ELSE
				!
				! Search for a break between words
				!
				FIND% = YSIZ%
				UNTIL (FIND% < 1%) OR (MID(JUNK$, FIND%, 1%) <> " ")
					FIND% = FIND% - 1%
				NEXT

				UNTIL (FIND% < 1%) OR (MID(JUNK$, FIND%, 1%) = " ")
					FIND% = FIND% - 1%
				NEXT

				FIND% = YSIZ% IF FIND% < 1%

				!
				! Stash away the text
				!
				SMG_STATUS% = SMG$PUT_CHARS(XX_VDID%, &
					LEFT(JUNK$, FIND%), &
					XPOS% + TEXT_LOOP%, YPOS%, , ATTR%)
				JUNK$ = RIGHT(JUNK$, FIND% + 1%)
				TEXT_LOOP% = TEXT_LOOP% + 1%
			END IF
		END IF
	NEXT

	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(XX_VDID%)

	RETURN

	END FUNCTION
