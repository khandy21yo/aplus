1	%TITLE "Function to Enter a String"
	%SBTTL "ENTR_3STRING"
	%IDENT "V3.6a Calico"

	FUNCTION STRING ENTR_3STRING(SCOPE_STRUCT SCOPE, &
		LONG XX_VDID, STRING OP_CPOS, STRING OP_PROMPT, &
		STRING OP_XSTART, LONG OP_FLAG, STRING OP_XFORMAT, &
		STRING OP_DEFLT)

	!
	!	COPYRIGHT (C) 1984 BY
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
	!	.b
	!	.lm +5
	!	Enters/Displays a string on the screen. This function
	!	uses the ENTRY function so all of it's editing features
	!	are available to the user.
	!	.lm -5
	!
	! Index:
	!
	! Parameter:
	!
	!	XX_VDID
	!		The virtual display ID number.
	!
	!	POS$
	!		The  passed final position of the data on screen.
	!		Format as 'ROW;COL'.  If POS$='' (blank), data will
	!		be entered but not displayed above on screen.
	!
	!	OP_PROMPT
	!		Passed prompt string. (Will be followed by
	!		'ALPHA:', i.e. 'ADD' will generate 'ADD ALPHA:')
	!
	!	OP_FLAG
	!		An integer flag word.
	!		.table
	!			1 - Don't enter data (display only?)
	!			2 - Rset string instead of lset
	!			4 - Force keypunch input(no <CR> after input)
	!			8 - Indicates a timeout on input will occur
	!			16 - Convert from lowercase to uppercase
	!			32 - Use default value
	!			64 - Don't display
	!			128 - Return final value in default
	!		.endtable
	!
	!	FORMAT$
	!		A passed BASIC+2 print-using format for string.
	!		It may also start with ~ to indicate a CMC
	!		format as follows:
	!		.table
	!			"~abx"
	!		where	~	indicates CMC format
	!			a	L or R for Pad to the left or right
	!			b	the pad character
	!			x	the normal BASIC print-using format
	!					as required.
	!		.endtable
	!
	!		Example:  "~LZ'E" means pad on the left side with
	!			the letter Z and use "'E" for the print-using.
	!
	!	DEFAULT$
	!		Returned default data value to use if <CR> is typed.
	!
	!
	!	Returns the string entered.
	!
	!	Returns DEFAULT$ if bit one is set in OP_FLAG.
	!
	! Example:
	!
	!	CN$ = ENTR_3STRING(SMG_SCREEN_DATA%, '3;19',"Customer Number", &
	!		CUSTOM.NUMBER$, OP_FLAG, "'E")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_3STRING/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_3STRING
	!	$ DELETE ENTR_3STRING.OBJ;*
	!
	! Author:
	!
	!	06/24/85 - Kevin Handy
	!
	! Modification history:
	!
	!	01/23/87 - Kevin Handy
	!		Added OP_DEFLT parameter, and OP_FLAG of 32, 64
	!
	!	02/03/87 - Kevin Handy
	!		Modified for SMG
	!
	!	02/29/88 - B. Craig Larsen
	!		Modified for CMC formats.
	!
	!	12/06/89 - Kevin Handy
	!		Made sharable version.
	!
	!	03/05/92 - Frank F. Starman
	!		Set RSET before return value if the flag is 2%.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/03/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/29/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Split out cursor positioning function
	!
	CALL DSPL_SPLITCURSOR(OP_CPOS, XPOS%, YPOS%)

	!
	! Decide between default value and normal value
	!
	IF (OP_FLAG AND 32%)
	THEN
		GETS$ = SPACE$(LEN(OP_XSTART))
		LSET GETS$ = OP_DEFLT
	ELSE
		GETS$ = OP_XSTART
	END IF

	IF LEFT(OP_XFORMAT, 1%) = "~"
	THEN
		T_FORMAT$ = RIGHT(OP_XFORMAT, 4%)
	ELSE
		T_FORMAT$ = OP_XFORMAT
	END IF

	!
	! If display only
	!
	GOTO L3000 IF (OP_FLAG AND 1%)

	!
	! Handle RSET string
	!
	LSET GETS$ = EDIT$(GETS$, 8%) IF (OP_FLAG AND 2%)

	!
	! Initial display of item in reverse video
	!
	SMG_STATUS% = SMG$PUT_CHARS(XX_VDID, FORMAT$(GETS$, T_FORMAT$), &
		XPOS%, YPOS%, 0%, SMG$M_REVERSE) &
		IF (OP_CPOS <> "") AND ((OP_FLAG AND 64%) = 0%)

	!
	! Initilization/prompt
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, OP_PROMPT + &
		" <alpha>:", 1%, 1%, 1%, 0%)

	Y1POS% = LEN(OP_PROMPT) + 11%

	!
	! Normal entry
	!
	TEMP% = ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, Y1POS%, &
		GETS$, -1%, OP_FLAG)

	!
	! Handle RSET string
	!
	RSET GETS$ = EDIT$(GETS$, 128%)	IF (OP_FLAG AND 2%)

 L3000:	!
	! Exit function
	!
	GETS$ = EDIT$(GETS$, 32%) IF (OP_FLAG AND 16%)	! Lower to uppercase

	!
	! Translate FORMAT$
	!
	IF LEFT(OP_XFORMAT, 1%) = "~"
	THEN
		!
		! Pad Character
		!
		PAD_C% = ASCII(MID(OP_XFORMAT, 3%, 1%))

		!
		! Pad Side
		!
		SELECT MID(OP_XFORMAT, 2%, 1%)
		CASE "L"

			ZPP$ = STRING$(LEN(GETS$), PAD_C%) + &
				EDIT$(GETS$, 8% + 128%)
			GETS$ = RIGHT(ZPP$, LEN(ZPP$) - LEN(GETS$) + 1%)

		CASE "R"

			GETS$ = LEFT(EDIT$(GETS$, 8% + 128%) + &
				STRING$(LEN(GETS$), PAD_C%), LEN(GETS$))

		CASE ELSE

		END SELECT
	END IF

	!
	! Re-display data on screen
	!
	SMG_STATUS% = SMG$PUT_CHARS(XX_VDID, FORMAT$(GETS$, T_FORMAT$), &
		XPOS%, YPOS%, 0%, SMG$M_BOLD) &
		IF (OP_CPOS <> "") AND ((OP_FLAG AND 64%) = 0%)

	!
	! Handle RSET string
	!
	RSET GETS$ = EDIT$(GETS$, 128%)	IF (OP_FLAG AND 2%)

	!
	! Return value
	!
	ENTR_3STRING = GETS$

	!
	! Return in default (string format) if flag is set
	!
	OP_DEFLT = GETS$ IF (OP_FLAG AND 128%)

	END FUNCTION
