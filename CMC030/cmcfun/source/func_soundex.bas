1	%TITLE "Converts String to a Number"
	%SBTTL "FUNC_SOUNDEX"
	%IDENT "V3.6a Calico"

	FUNCTION REAL FUNC_SOUNDEX(SOUND$)

	!
	!	COPYRIGHT (C) 1985 BY
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
	!	This function converts string to a number to represent
	!	how a name (string) "sounds rather then how it is
	!	spelled
	!
	! Parameters:
	!
	!	SOUND$
	!		The passed name (string)
	!
	!	Returned value
	!		The number which evaluates name (string)
	!
	! Example:
	!
	!	SOUND = FUNC_SOUNDEX("SMITH")
	!
	! Compile
	!
	!	$ BAS FUNC_SOURCE:FUNC_SOUNDEX/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP FUNC_SOUNDEX
	!	$ DELETE FUNC_SOUNDEX.OBJ;*
	!
	! Author:
	!
	!	12/12/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	10/24/90 - Kevin Handy
	!		Modified to handle PS? as S?, MAC? as MC?,
	!		X? as Z?.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/26/93 - Kevin Handy
	!		X? and Z? already handled the same, so commented out
	!		that check.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	SOUNDNAME = 0.0

	!
	! Change lower case to upper case, trim junk off end
	!
	SOUNDNAME$ = EDIT$(SOUND$, 32% + 128%)

	!
	! Work out some special problems
	! (Psychology-Sychology, MacDonald-McDonald, Xerox-Zerox)
	!
	I% = INSTR(1%, " " + SOUNDNAME$, " PS")
	IF I%
	THEN
		SOUNDNAME$ = LEFT(SOUNDNAME$, I% - 1%) + &
			RIGHT(SOUNDNAME$, I% + 1%)
	END IF

	I% = INSTR(1%, " " + SOUNDNAME$, " MAC")
	IF I%
	THEN
		SOUNDNAME$ = LEFT(SOUNDNAME$, I%) + RIGHT(SOUNDNAME$, I% + 2%)
	END IF

	!
	! Work only with the first word
	!
	LENGTH% = INSTR(1%, SOUND$, " ")
	SOUNDNAME$ = LEFT(SOUNDNAME$, LENGTH%) IF LENGTH%

	LETTER$ = LEFT(SOUNDNAME$, 1%)
	SOUNDNAME = ASCII(LETTER$) * 1000.0
	SOUNDNAME$ = RIGHT(SOUNDNAME$, 2%)

	EXPO% = 3%
	LENGTH% = LEN(SOUNDNAME$)

	WHILE LENGTH% * EXPO% <> 0%
		IF LETTER$ <> LEFT(SOUNDNAME$, 1%)
		THEN
			POSI% = INSTR(1%, "BPFVVVVVCSKGJGXZDTTTTTTTLLLLLLLL" + &
				"MNNNNNNNR", LEFT(SOUNDNAME$, 1%))
			IF POSI% <> 0%
			THEN
				POSI% = INT((POSI% - 1%) / 8%) + 1%
				EXPO% = EXPO% - 1%
				SOUNDNAME = SOUNDNAME + POSI% * 10**EXPO%
			END IF
		END IF
		LETTER$ = LEFT(SOUNDNAME$, 1%)
		SOUNDNAME$ = RIGHT(SOUNDNAME$, 2%)
		LENGTH% = LENGTH% - 1%
	NEXT

	FUNC_SOUNDEX = SOUNDNAME

	END FUNCTION
