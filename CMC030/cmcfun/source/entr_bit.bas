1	%TITLE "Function to Switch Bit Sign"
	%SBTTL "ENTR_BIT"
	%IDENT "V3.6a Calico"

	FUNCTION STRING ENTR_BIT(Xwindow%, Xcpos$, &
		Xprompt$, Xdflt$, Xflag%, &
		Xformat$, Xdeflt$, Xchar$, Xbits%, Xlength%)

	!
	! COPYRIGHT (C) 1987 BY
	!
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
	!	This function switch a bit sign
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	XCPOS$
	!		'' - Do not display on top area
	!
	!	XFLAG%
	!	.table
	!		1 - Don't enter data (display only?)
	!		4 - Force keypunch input(no <CR> after input)
	!		8 - Indicates a timeout on input will occur
	!		32 - Use default value
	!		64 - Don't display
	!		128 - Return value in XDEFLT$
	!	.endtable
	!
	!	Xwindow%
	!		The passed window for the origional display.
	!
	!	Xprompt$
	!		The passed string that holds the prompt information.
	!
	!	Xdflt$
	!		One of the passed defaults for the exit key.
	!
	!	Xformat$
	!		The passed format of the bits.
	!
	!	Xdeflt$
	!		The passed format default.
	!
	!	Xchar$
	!		Used to split up the characters.
	!
	!	Xbits%
	!		A bit sign to be switched.
	!
	!	Xlength%
	!		The length of the bit sign.
	!
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_BIT/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP ENTR_BIT
	!	$ DELETE ENTR_BIT.OBJ;*
	!
	! Author:
	!
	!	06/10/88 - Frank Starman
	!
	! Modification history:
	!
	!	02/09/89 - Kevin Handy
	!		Modified for change in ENTR_ENTRY.
	!
	!	03/14/92 - Kevin Handy
	!		Clean up vars (checkvar)
	!
	!	09/17/92 - Kevin Handy
	!		Fixed bug in name of function in error message from
	!		ENTR_3NUMBER to ENTR_BIT
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/19/95 - Kevin Handy
	!		Changed Xwindow to Xwindow%. This changes the
	!		calling parameters, but since we've been passing
	!		longs all this time no other program should change.
	!
	!	09/03/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/24/99 - Kevin Handy
	!		Use WHEN ERROR
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	EXTERNAL STRING FUNCTION FUNC_BITSTRING
	EXTERNAL STRING FUNCTION FUNC_BITSWITCH

	DECLARE LONG XPOS, YPOS

	%PAGE

	!
	! Seperate cursor position
	!
	CALL DSPL_SPLITCURSOR(XCPOS$, XPOS, YPOS)

	!
	! Choose the default to use
	!
	IF (XFLAG% AND 32%) = 0%
	THEN
		GETS$ = XDFLT$
	ELSE
		GETS$ = XDEFLT$
	END IF

	GETS$ = STRING$(LEN(XDFLT$), 0%) IF TRM$(GETS$) = ""

	!
	! Display only?
	!
	GOTO L3000 IF (XFLAG% AND 1%)

	JUNK% = INSTR(1%, XFORMAT$, ".")
	LEN2% = 0%
	LEN2% = LEN2% + 1% IF MID(XFORMAT$, I%, 1%) = "#" &
		FOR I% = JUNK% + 1% TO LEN(XFORMAT$) IF JUNK%

	GETS1$ = NUM1$(FUNC_ROUND(GETS% * 10.0 ^ LEN2%, 0%))
	GETS1$ = GETS1$ + SPACE$(LEN(XFORMAT$) - LEN(GETS1$))

 L1000: !
	! Display original
	!
	SMG_STATUS% = SMG$PUT_CHARS(Xwindow%, FUNC_BITSTRING(XBITS%, GETS$, &
		XLENGTH%,XCHAR$), XPOS, YPOS, 0%, SMG$M_REVERSE) &
		IF (XCPOS$ <> "") AND ((XFLAG% AND 64%) = 0%)

	SCOPE::SCOPE_EXIT = 0%

 L1005:	!
	! Initialization/prompt
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
		XPROMPT$ + " <number>:", &
		1%, 1%, 1%, 0%)
	Y1POS% = LEN(XPROMPT$) + 11%


	!
	! Normal entry
	!
 L1110:	GETS% = 0%
	TEMP_GETS$ = GETS1$ + ""

	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, Y1POS%, TEMP_GETS$, &
		-1%, XFLAG%)

	!
	! Pick out all of the exit keys
	!
	CASE SMG$K_TRM_CTRLC, SMG$K_TRM_F8, SMG$K_TRM_F10, &
		SMG$K_TRM_CTRLZ

		GETS$ = XDFLT$
		GOTO L3000
	END SELECT

	TEMP_GETS$ = EDIT$(TEMP_GETS$, 2%)

	!
	! Take value entered
	!
	WHEN ERROR IN
		GETS% = VAL%(TEMP_GETS$)
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Illegal number entered!", 1%)
		SELECT SCOPE::SCOPE_EXIT
		CASE 3%, 256%, 290%	! Cntrl C, PF1, Exit
			GETS% = XDEFLT
			CONTINUE L3000

		CASE ELSE
			CONTINUE L1005
		END SELECT
	END WHEN

	GETS$ = FUNC_BITSWITCH(XBITS%, GETS$, GETS%) &
		IF GETS% <> 0%

 L3000:	!
	! Display result
	!
	SMG_STATUS% = SMG$PUT_CHARS(Xwindow%, FUNC_BITSTRING(XBITS%, GETS$, &
		XLENGTH%,XCHAR$), XPOS, YPOS, 0%, SMG$M_BOLD) &
		IF (XCPOS$ <> "") AND ((XFLAG% AND 64%) = 0%)

	!
	! Return value
	!
	ENTR_BIT = GETS$

	!
	! Return value in XDEFLT$ if told to
	!
	XDEFLT$ = GETS$ IF (XFLAG% AND 128%)

	GOTO L1000 IF GETS% <> 0%

	END FUNCTION
