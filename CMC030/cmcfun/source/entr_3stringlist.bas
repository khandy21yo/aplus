1	%TITLE "Function to Enter a String with Table Verification"
	%SBTTL "ENTR_3STRINGLIST"
	%IDENT "V3.6a Calico"

	FUNCTION STRING ENTR_3STRINGLIST(SCOPE_STRUCT SCOPE, &
		LONG XX_VDID, STRING OP_CPOS, &
		STRING OP_PROMPT, STRING OP_XSTART, LONG OP_FLAG, &
		STRING OP_XFORMAT, STRING OP_XDEFLT, &
		STRING OP_VTEXT(), STRING OP_THETITLE, STRING OP_DRAWCOLS)

	!
	! COPYRIGHT (C) 1987 BY
	! Computer Management Center,
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
	!	Enters a string and matches against a list of valid
	!	strings.
	!	.b
	!	List choices allows selecting from the list of strings.
	!	.b
	!	This function requires that the length of the array
	!	list in the function be passed in the zero element of
	!	that array list. The array list is now labeled as
	!	OP_VTEXT(), the number of entries in that table would
	!	be entered in the OP_VTEXT(0%) element as a number.
	!	.lm -5
	!
	! Index:
	!
	! Parameter:
	!
	!	OP_CPOS
	!		Position to display string.
	!		"" - Do not display on top area
	!
	!	OP_FLAG
	!	.table
	!		  1 - Don't enter data (display only?)
	!
	!		  2 - Rset string instead of lset
	!
	!		  4 - Force keypunch input(no <CR> after input)
	!
	!		  8 - Indicates a timeout on input will occur
	!
	!		 16 - Convert from lowercase to uppercase
	!
	!		 32 - Use default value
	!
	!		 64 - Don't display
	!
	!		128 - Return value in OP_XDEFLT
	!
	!		2048 - Remove leading spaces in list
	!
	!		16384 - Allow up-arrow to exit
	!	.endtable
	!
	!	XX_VDID
	!		The passed variable that creates or deletes the
	!		window that holds the string.
	!
	!	OP_PROMPT
	!		The passed string used for the prompt and its initialization
	!
	!	OP_XSTART
	!		Passed variable used to decide between the default values
	!		and normal values.
	!
	!	OP_XFORMAT
	!		The passed format for the string.
	!
	!	OP_XDEFLT
	!		The default form the string size.
	!
	!	OP_VTEXT()
	!		Used to check against tables.
	!
	!	OP_THETITLE
	!		The title for the choice menu.
	!
	!	OP_DRAWCOLS
	!		Used to draw the columns of the menu.
	!
	!
	!	This function enters a string with a table verification.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_3STRINGLIST/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_3STRINGLIST
	!	$ DELETE ENTR_3STRINGLIST.OBJ;*
	!
	! AUTHOR:
	!
	!	06/24/85 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	01/23/87 - Kevin Handy
	!		Added OP_XDEFLT parameter, and OP_FLAG of 32, 64
	!
	!	02/03/87 - Kevin Handy
	!		Modified for SMG
	!
	!	07/07/88 - Kevin Handy
	!		Added flag of 16384% to exit on uparrow.
	!
	!	12/19/88 - Kevin Handy
	!		Modified the <exit> handling so that it would
	!		still return a value back.
	!
	!	02/09/89 - Kevin Handy
	!		Modified for change in ENTR_ENTER.
	!
	!	03/31/89 - Robert Peterson
	!		Added flag of 2048% to remove leading spaces
	!		from list of choices.
	!
	!	12/11/89 - Kevin Handy
	!		Modified for sharable library.
	!
	!	05/02/90 - Frank F. Starman
	!		Replace ENTR_3MESSAGE with HELP_34MESSAGE function.
	!		Handle TIMEOUT variable as the EXIT key.
	!
	!	07/06/93 - Kevin Handy
	!		Replaced VAL with VAL% which is a little bit faster.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	02/17/96 - Kevin Handy
	!		Allow control/Z to act line F10
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	DECLARE LONG XPOS, YPOS

	!
	! Split out cursor positioning function
	!
	CALL DSPL_SPLITCURSOR(OP_CPOS, X%, Y%)
	XPOS = X%
	YPOS = Y%

	!
	! Decide between default value and normal value
	!
	IF (OP_FLAG AND 32%)
	THEN
		GETS$ = SPACE$(LEN(OP_XSTART))
		LSET GETS$ = OP_XDEFLT
	ELSE
		GETS$ = OP_XSTART
	END IF

	!
	! If display only
	!
	GOTO L3200 IF (OP_FLAG AND 1%)

	!
	! Initial display of item in reverse video
	!
	IF (OP_CPOS <> "") AND ((OP_FLAG AND 64%) = 0%)
	THEN
		SMG_STATUS% = SMG$PUT_CHARS &
		( &
			XX_VDID,	! Window &
			FORMAT$(GETS$, OP_XFORMAT),	! Message to display &
			XPOS,	! Line &
			YPOS,	! Column &
			0%,		! Erase screen &
			SMG$M_REVERSE,	! Attributes &
		)
	END IF

	!
	! Initilization/prompt
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS &
	( &
		SCOPE::SMG_OPTION,	! Window &
		OP_PROMPT + " <alpha>:",	! Message to display &
		1%,		! Line &
		1%,		! Column &
		1%,		! Erase line &
		0%,		! Attributes &
	)
	Y1POS% = LEN(OP_PROMPT) + 11%

	!
	! Handle RSET string
	!
	LSET GETS$ = EDIT$(GETS$, 8%) IF (OP_FLAG AND 2%)

 L2000:	!
	! Normal entry
	!
	TEMP% = ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, Y1POS%, GETS$, -1%, OP_FLAG)

	!
	! Handle RSET string
	!
	RSET GETS$ = EDIT$(GETS$, 128%)	IF (OP_FLAG AND 2%)

 L3000:	!
	! Abort out on exits
	!
	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLC, SMG$K_TRM_TIMEOUT, &
		SMG$K_TRM_CTRLZ

		!
		! Let's make it easier on other programs
		! by forcing to s apecific return code.
		!
		SCOPE::SCOPE_EXIT = SMG$K_TRM_F10
		GOTO L3200

	CASE SMG$K_TRM_F14
		!
		! Attempt to enter through choice menu
		!
		DRAW_C$ = EDIT$(OP_DRAWCOLS, 2% + 4%)
		X% = ENTR_3CHOICE(SCOPE, "", "", OP_VTEXT(), GETS$, 138%, &
			OP_THETITLE, DRAW_C$, 0%)

		GOTO L2000 IF X% < 0%

		IF (OP_FLAG AND 2048%)
		THEN
			!
			! Remove leading spaces
			!
			GETS$ = EDIT$(LEFT(OP_VTEXT(X%), LEN(OP_XSTART)), 8%)
		ELSE
			GETS$ = LEFT(OP_VTEXT(X%), LEN(OP_XSTART))
		END IF

		GOTO L3000

	CASE SMG$K_TRM_UP
		GOTO L3200 IF OP_FLAG AND 16384%
	END SELECT

	!
	! Determine test segement length
	!
	TEST_SEG% = VAL%(LEFT(OP_DRAWCOLS, 3%)) - 3%
	TEST_SEG% = LEN(GETS$) IF TEST_SEG% <= 0% OR LEN(GETS$) < TEST_SEG%

	!
	! Check against tables
	!
	TT% = VAL%(OP_VTEXT(0%))

	!
	! If table element count is set then loop otherwise while next
	!
	IF TT%
	THEN
		FOR I% = 1% TO TT%
			IF OP_FLAG AND 2048%
			THEN
				GOTO L3200 IF LEFT(EDIT$(OP_VTEXT(I%), 8% + 128%), TEST_SEG%) = EDIT$(GETS$, 128%)
			ELSE
				GOTO L3200 IF EDIT$(LEFT(OP_VTEXT(I%), TEST_SEG%), 128%) = EDIT$(GETS$, 128%)
			END IF
		NEXT I%
	ELSE
		I% = 1%
		WHILE OP_VTEXT(I%) <> ""

			IF OP_FLAG AND 2048%
			THEN
				GOTO L3200 IF LEFT(EDIT$(OP_VTEXT(I%), 8% + 128%), TEST_SEG%) = EDIT$(GETS$, 128%)
			ELSE
				GOTO L3200 IF EDIT$(LEFT(OP_VTEXT(I%), TEST_SEG%), 128%) = EDIT$(GETS$, 128%)
			END IF
			I% = I% + 1%
		NEXT
	END IF

	CALL HELP_34MESSAGE(SCOPE, "invalid choice", &
			"W", "ENTR_3STRINGLIST", "", "INVCHOICE")
	!++
	! Warning:INVCHOICE
	!	^*Invalid Choice\*
	!	.b
	!	.lm +5
	!	^*Explanation\*
	!	.b
	!	An invalid input has been entered.
	!	.b
	!	^*User Action\*
	!	.b
	!	Press the ^*<List Choices>\* key or refer to the documentation
	!	manual for valid entries, and re-enter the input.
	!	.lm -5
	!
	! Index:
	!
	!--

	!SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
	GOTO L2000

 L3200:	!
	! Pad GETS$ with spaces if necessary
	!
	GETS$ = LEFT(GETS$ + SPACE$(LEN(OP_XSTART)), LEN(OP_XSTART))

	!
	! Re-display data on screen
	!
	IF (OP_CPOS <> "") AND ((OP_FLAG AND 64%) = 0%)
	THEN
		SMG_STATUS% = SMG$PUT_CHARS &
		( &
			XX_VDID,	! Window &
			FORMAT$(GETS$, OP_XFORMAT),	! Message to display &
			XPOS,	! Line &
			YPOS,	! Column &
			0%,		! Erase screen &
			SMG$M_BOLD,	! Attributes &
		)
	END IF

	!
	! Return value
	!
	ENTR_3STRINGLIST = GETS$

	!
	! Return value in OP_XDEFLT if supposed to
	!
	OP_XDEFLT = GETS$ IF (OP_FLAG AND 128%)

	END FUNCTION
