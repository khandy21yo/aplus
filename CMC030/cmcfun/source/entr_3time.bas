1	%TITLE "Function to Enter a Time"
	%SBTTL "ENTR_3TIME"
	%IDENT "V3.6a Calico"

	FUNCTION STRING ENTR_3TIME(SCOPE_STRUCT SCOPE, &
		LONG XX_VDID, STRING OPT_CPOS, &
		STRING OPT_PROMPT, STRING OPT_XDFLT, LONG OPT_FLAG, &
		STRING OPT_KIND, STRING OPT_DEFLT)

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
	!
	! Index:
	!
	! Parameters:
	!
	!	OPT_CPOS
	!		Position to display data.
	!		"" - Do not display on top area
	!
	!	OPT_FLAG
	!	.table
	!		  1 - Don't enter data (display only?)
	!
	!		  4 - Force keypunch input(no <CR> after input)
	!
	!		  8 - Indicates a timeout on input will occur
	!
	!		 32 - Use default value
	!
	!		 64 - Don't display
	!
	!		128 - Return value in OPT_DEFLT
	!
	!	       2048	Display only hours and minutes
	!
	!	       4096	Display only hours
	!	.endtable
	!
	!	XX_VDID
	!		Passed variable that creates or deletes the
	!		window that holds the string.
	!
	!	OPT_PROMPT
	!		The passed string used for the prompt and its initialization
	!
	!	OPT_XDFLT
	!		One of the passed defaults for the time format.
	!
	!	OPT_KIND
	!		The passed format for the time.
	!
	!	OPT_DEFLT
	!		The default form of the time size.
	!
	!
	!	This function enters a time on the screen in the user's format.
	!
	! Example:
	!
	! Index:
	!
	!	.x Enter>Time
	!	.x Time>Enter
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_3TIME/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_3TIME
	!	$ DELETE ENTR_3TIME.OBJ;*
	!
	! Author:
	!
	!	05/29/86 - B. Craig Larsen
	!
	! Modification history:
	!
	!	01/26/87 - Kevin Handy
	!
	!	04/23/87 - Kevin Handy
	!		Modified for verifying the date.  Must be numeric
	!		and each part must be in range.
	!
	!	07/14/87 - Kevin Handy
	!		Modified to return value in OPT_DEFLT
	!
	!	02/25/88 - B. Craig Larsen
	!		Modified to only display hours or hours and minutes.
	!
	!	12/26/89 - Kevin Handy
	!		Modified for sharable library.
	!
	!	05/02/90 - Frank F. Starman
	!		Replace ENTR_3MASSEGE with HELP_34MESSAGE function.
	!
	!	03/25/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 source code format.
	!		Fix OPT_KIND to be string instead of long. Note that
	!		this changes the calling parameters.
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Includes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	DECLARE LONG XPOS, YPOS

	%PAGE

	!
	! Split cursor position
	!
	CALL DSPL_SPLITCURSOR(OPT_CPOS, X%, Y%)
	XPOS = X%
	YPOS = Y%

	!
	!	Limit the OPT_XDFLT to 10 char
	!
	GETS$ = SPACE$(10%)

	IF OPT_FLAG AND 32%
	THEN
		LSET GETS$ = OPT_DEFLT
	ELSE
		LSET GETS$ = OPT_XDFLT
	END IF

	!
	!	Check for no input
	!
	GOTO L3000 IF OPT_FLAG AND 1%

	IF (OPT_CPOS <> "") AND ((OPT_FLAG AND 64%) = 0%)
	THEN
		SMG_STATUS% = SMG$PUT_CHARS &
		( &
			XX_VDID, &
			PRNT_TIME(GETS$, OPT_FLAG), &
			XPOS, &
			YPOS, &
			0%, &
			SMG$M_REVERSE &
		)
	END IF

 L1000:	!
	! Initialization/prompt
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS &
	( &
		SCOPE::SMG_OPTION,		! Window &
		OPT_PROMPT + " <time>:",	! Message to display &
		1%,		! Line &
		1%,		! Column &
		1%,		! Erase line &
		0%,		! Attributes &
	)
	Y1POS% = LEN(OPT_PROMPT) + 10%

	!
	! Hours minutes
	!
	GETS$ = LEFT(GETS$, 2%) IF OPT_FLAG AND 4096%
	GETS$ = LEFT(GETS$, 5%) IF OPT_FLAG AND 2048%

	!
	! Normal entry
	!
	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, Y1POS%, GETS$, -1%, OPT_FLAG)

	CASE SMG$K_TRM_CTRLC, SMG$K_TRM_F8, SMG$K_TRM_F10, &
		SMG$K_TRM_CTRLZ

		GETS$ = OPT_XDFLT
		GOTO L3000
	END SELECT

	GETS$ = EDIT$(GETS$, 2%)

	SELECT LEN(GETS$)

	CASE 1%, 2%, 3%, 4%, 5%, 6%, 7%, 8%, 9%, 10%	! Convert - its good
		GETS1$ = GETS$
		GETS$ = TIME_STORETIME(GETS$, OPT_KIND)
		IF GETS$ = ""
		THEN
			GETS$ = GETS1$ + SPACE$(10% - LEN(GETS1$))

			CALL HELP_34MESSAGE(SCOPE, "invalid time", "W", &
				"ENTR_3TIME", "", "INVTIME")
	!++
	! Warning:INVTIME
	!	^*Invalid Time\*
	!	.b
	!	.lm +5
	!	^*Explanation\*
	!	.b
	!	Invalid time or illegal time format.
	!	.b
	!	^*User Action\*
	!	.b
	!	Check for right time or format and re-enter input.
	!	.lm -5
	!
	! Index:
	!	.x Time
	!
	!--
			GOTO L1000
		END IF

	CASE ELSE		! Return blank string on C/R
		GETS$ = SPACE$(LEN(OPT_XDFLT))

	END SELECT

 L3000:	!
	! Exit function
	!
	IF (OPT_CPOS <> "") AND ((OPT_FLAG AND 64%) = 0%)
	THEN
		SMG_STATUS% = SMG$PUT_CHARS &
		( &
			XX_VDID,	! Window &
			PRNT_TIME(GETS$, OPT_FLAG),	! Message to display &
			XPOS,	! Line &
			YPOS,	! Column &
			0%,		! Erase screen &
			SMG$M_BOLD,	! Attributes &
		)
	END IF

	ENTR_3TIME = GETS$

	OPT_DEFLT = GETS$ IF (OPT_FLAG AND 128%)

	END FUNCTION
