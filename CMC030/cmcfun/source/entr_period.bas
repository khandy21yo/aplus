1	%TITLE "Function to Enter a Period"
	%SBTTL "ENTR_PERIOD"
	%IDENT "V3.6a Calico"

	FUNCTION STRING ENTR_PERIOD(XX_VDID%, CPOS$, &
		PROMPT$, XDFLT$, FLAG%, &
		XFORMAT$, DEFLT$)

	!
	!	COPYRIGHT (C) 1986 BY
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
	!	This function enters a period
	!	CPOS$
	!	'' - Do not display on top area
	!	FLAG%
	!	.table 3,25
	!	.te
	!	1	Don't enter data (display only?)
	!	.te
	!	4	Force keypunch input(no <CR> after input)
	!	.te
	!	8	Indicates a timeout on input will occur
	!	.te
	!	32	Use default value
	!	.te
	!	64	Don't display
	!	.te
	!	128	Return fincal value in default
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	! Input:
	!
	!	XX_VDID%
	!		The passed variable that creates or deletes the
	!		window that holds the string.
	!
	!	PROMPT$
	!		The passed string used for the prompt and its initialization
	!
	!	XDFLT$
	!		One of the passed defaults for the period.
	!
	!	XFORMAT$
	!		The string used to examine the format for the
	!		period.
	!
	!	DEFLT$
	!		The default form of the period size.
	!
	!
	!	This function enters a period on the screen.
	!
	! Example:
	!
	! Index:
	!
	!	.x Enter>Period
	!	.x Period>Enter
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_PERIOD
	!	$ LIB FUNC_LIB:CMCFUN/REP ENTR_PERIOD
	!	$ DELETE ENTR_PERIOD.OBJ;*
	!
	! AUTHOR:
	!
	!	12/18/85 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	02/09/89 - Kevin Handy
	!		Modified for change in ENTR_ENTER.
	!
	!	04/05/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Standards.
	!		Changed XX_VDID to XX_VDID%
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope_exit%
	!
	!	09/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/29/2000 - Kevin Handy
	!		Lose useless error trapping
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	DECLARE LONG XPOS, YPOS

	%PAGE

	!
	! Seperate cursor position
	!
	CALL DSPL_SPLITCURSOR(CPOS$, X%, Y%)
	XPOS = X%
	YPOS = Y%

	!
	! Choose the default to use
	!
	IF (FLAG% AND 32%) = 0%
	THEN
		GETS$ = XDFLT$
	ELSE
		GETS$ = DEFLT$ + SPACE$(LEN(XDFLT$) - LEN(DEFLT$))
	END IF

	!
	! Display only?
	!
	GOTO L3000 IF (FLAG% AND 1%) <> 0%

	!
	! Display original
	!
	SMG_STATUS% = SMG$PUT_CHARS(XX_VDID%, GETS$, &
		XPOS, YPOS, 0%, SMG$M_REVERSE) &
		IF (CPOS$ <> "") AND ((FLAG% AND 64%) = 0%)

	SCOPE::SCOPE_EXIT = 0%

 L1000:	!
	! Initialization/prompt
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, PROMPT$ + "<YYYYPP>: ", &
		1%, 1%, 1%, 0%)
	Y1POS% = LEN(PROMPT$) + 10%

 L1110:	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
		1%, Y1POS%, GETS$, -1%, FLAG%)

	!
	! Pick out all of the exit keys
	!
	CASE SMG$K_TRM_CTRLC, SMG$K_TRM_F8, &
		SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		GOTO L3000
	END SELECT

	GETS$ = EDIT$(GETS$, 2%)

 L1200:
	SELECT LEN(GETS$)

	CASE 0%
		GETS$ = SPACE$(LEN(XDFLT$))
		GOTO L3000

	CASE 6%		! Period given in YYYYPP format
			! (Don't need to do anything)

	CASE ELSE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Invalid period: use YYYYPP format", 0%)
		GETS$ = GETS$ + SPACE$(LEN(XDFLT$) - LEN(GETS$))
		GOTO L1110
	END SELECT

2000	!
	! Examine period
	!
	IF READ_PERIOD("READ", XFORMAT$, GETS$, "", STAT$, "", "", 0%)
	THEN
		GOTO 3500
	END IF

 L3000:	!
	! Display result
	!
	SMG_STATUS% = SMG$PUT_CHARS(XX_VDID%, GETS$, &
		XPOS, YPOS, 0%, SMG$M_BOLD) &
		IF (CPOS$ <> "") AND ((FLAG% AND 64%) = 0%)

	ENTR_PERIOD = GETS$

	!
	! Return text in default if supposed to
	!
	DEFLT$ = GETS$ IF (FLAG% AND 128%)

	EXIT FUNCTION

3500	CALL ENTR_3MESSAGE(SCOPE, "Undefined period!", 0%)
	GOTO L1110

	END FUNCTION
