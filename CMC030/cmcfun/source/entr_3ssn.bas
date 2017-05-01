1	%TITLE "Function to Enter a String"
	%SBTTL "ENTR_3SSN"
	%IDENT "V3.6a Calico"

	FUNCTION STRING ENTR_3SSN(SCOPE_STRUCT SCOPE, &
		LONG XX_VDID, STRING OPT_CPOS, STRING OPT_PROMPT, &
		STRING OPT_XSTART, LONG OPT_FLAG, STRING OPT_XFORMAT, &
		STRING OPT_DEFLT)

	!
	!	COPYRIGHT (C) 1984 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	! Parameters:
	!
	!	XX_VDID
	!		The passed virtual display ID number.
	!
	!	POS$
	!		The  final passed position of the data on screen.
	!		Format as 'ROW;COL'.  If POS$='' (blank), data will
	!		be entered but not displayed above on screen.
	!
	!	OPT_PROMPT
	!		Passed prompt string. (Will be followed by
	!		'ALPHA:', i.e. 'ADD' will generate 'ADD ALPHA:')
	!
	!	OPT_FLAG
	!		An integer flag word.
	!		.table
	!			1 - Don't enter data (display only?)
	!
	!			4 - Force keypunch input(no <CR> after input)
	!
	!			8 - Indicates a timeout on input will occur
	!
	!			32 - Use default value
	!
	!			64 - Don't display
	!
	!			128 - Return final value in default
	!		.endtable
	!
	!	FORMAT$
	!		A passed BASIC+2 print-using format for string.
	!
	!	DEFAULT$
	!		Passed Default data value to use if <CR> is typed.
	!
	!
	!	Returns the string entered.
	!
	!	Returns DEFAULT$ if bit one is set in OPT_FLAG.
	!
	! Example:
	!
	!	CN$ = ENTR_3SSN(SMG_SCREEN_DATA%, '3;19',"Customer Number", &
	!		CUSTOM.NUMBER$, OPT_FLAG, "'E")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_3SSN/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_3SSN
	!	$ DELETE ENTR_3SSN.OBJ;*
	!
	! Author:
	!
	!	06/24/85 - Kevin Handy
	!
	! Modification history:
	!
	!	01/23/87 - Kevin Handy
	!		Added OPT_DEFLT parameter, and OPT_FLAG of 32, 64
	!
	!	02/03/87 - Kevin Handy
	!		Modified for SMG
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changes in ENTR_ENTER.
	!
	!	12/20/89 - Kevin Handy
	!		Modified for sharable library.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
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
	CALL DSPL_SPLITCURSOR(OPT_CPOS, X%, Y%)
	XPOS = X%
	YPOS = Y%

	!
	! Decide between default value and normal value
	!
	GETS$ = SPACE$(12%)
	IF (OPT_FLAG AND 32%)
	THEN
		LSET GETS$ = OPT_DEFLT
	ELSE
		LSET GETS$ = OPT_XSTART
	END IF

	!
	! If display only
	!
	GOTO L3000 IF (OPT_FLAG AND 1%)

	!
	! Initial display of item in reverse video
	!
	IF (OPT_CPOS <> "") AND ((OPT_FLAG AND 64%) = 0%)
	THEN
		SMG_STATUS% = SMG$PUT_CHARS &
		( &
			XX_VDID,	! Window &
			GETS$, &
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
		OPT_PROMPT + " <ssn>:",	! Message to display &
		1%,		! Line &
		1%,		! Column &
		1%,		! Erase line &
		0%,		! Attributes &
	)
	Y1POS% = LEN(OPT_PROMPT) + 11%

	!
	! Normal entry
	!
	TEMP% = ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, Y1POS%, GETS$, -1%, OPT_FLAG)

	!
	! Reformat as necessary
	!
	IF INSTR(1%, GETS$, "-") = 0%
	THEN
		LSET GETS$ = LEFT(GETS$, 3%) + "-" + &
			MID(GETS$, 4%, 2%) + "-" + &
			RIGHT(GETS$, 6%)
	END IF

 L3000:	!
	! Exit function
	!

	!
	! Re-display data on screen
	!
	IF (OPT_CPOS <> "") AND ((OPT_FLAG AND 64%) = 0%)
	THEN
		SMG_STATUS% = SMG$PUT_CHARS &
		( &
			XX_VDID,	! Window &
			GETS$, &
			XPOS,	! Line &
			YPOS,	! Column &
			0%,		! Erase screen &
			SMG$M_BOLD,	! Attributes &
		)
	END IF

	!
	! Return value
	!
	ENTR_3SSN = GETS$

	!
	! Return in default (string format) if flag is set
	!
	OPT_DEFLT = GETS$ IF (OPT_FLAG AND 128%)

	END FUNCTION
