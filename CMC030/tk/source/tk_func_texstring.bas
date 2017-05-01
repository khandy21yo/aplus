1	%TITLE "Convert Text String into TeX Format"
	%SBTTL "TK_FUNC_TEXSTRING"
	%IDENT "V3.6a Calico"

	FUNCTION STRING TK_FUNC_TEXSTRING(INLINE$)

	!
	!	COPYRIGHT (C) 1988 BY
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
	!	.p
	!	This function converts a normal text string
	!	(one line only) into something that TeX can use.
	!
	! Index:
	!	.x Documentation
	!	.x TeX>String
	!	.x String>TeX
	!
	! Option:
	!
	!
	! Input:
	!
	!	INLINE$
	!		String to be converted.
	!
	! Output:
	!
	!	Converted String
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_FUNC_TEXSTRING
	!	$ LIB/REP FUNC_LIB:CMCFUN TK_FUNC_TEXSTRING
	!	$ DELETE TK_FUNC_TEXSTRING.OBJ;*
	!
	! Author:
	!
	!	07/22/88 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for STR$ routines
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Initilization
	!
	TEMP_IN$ = INLINE$ + ""
	TEMP_IN% = 0%

 TexLoop:
	!
	! Search for any character that may need conversion
	!
	TEMP_OLD% = TEMP_IN% + 1%
	TEMP_IN% = STR$FIND_FIRST_IN_SET(RIGHT(TEMP_IN$, TEMP_OLD%), &
		'#$%&_{}<>~^"')

	IF TEMP_IN%
	THEN
		!
		! Found character needing conversion, so convert it.
		!
		TEMP_IN% = TEMP_IN% + TEMP_OLD% - 1%

		SELECT MID(TEMP_IN$, TEMP_IN%, 1%)

		CASE "~", "^", "\"
			TEMP_IN$ = LEFT(TEMP_IN$, TEMP_IN% - 1%) + &
				"\verb+" + &
				MID(TEMP_IN$, TEMP_IN%, 1%) + "+" + &
				RIGHT(TEMP_IN$, TEMP_IN% + 1%)
			TEMP_IN% = TEMP_IN% + 7%

		CASE "<", ">"
			TEMP_IN$ = LEFT(TEMP_IN$, TEMP_IN% - 1%) + &
				"$" + &
				MID(TEMP_IN$, TEMP_IN%, 1%) + "$" + &
				RIGHT(TEMP_IN$, TEMP_IN% + 1%)
			TEMP_IN% = TEMP_IN% + 2%

		CASE '"'
			!
			! Alternate the quote marker between `` and ''.
			!
			IF QUOTE_FLAG%
			THEN
				TEMP_IN$ = LEFT(TEMP_IN$, TEMP_IN% - 1%) + &
					"''" + &
					RIGHT(TEMP_IN$, TEMP_IN% + 1%)
				QUOTE_FLAG% = 0%
			ELSE
				TEMP_IN$ = LEFT(TEMP_IN$, TEMP_IN% - 1%) + &
					"``" + &
					RIGHT(TEMP_IN$, TEMP_IN% + 1%)
				QUOTE_FLAG% = -1%
			END IF
			TEMP_IN% = TEMP_IN% + 1%

		CASE ELSE
			TEMP_IN$ = LEFT(TEMP_IN$, TEMP_IN% - 1%) + &
				"\" + &
				RIGHT(TEMP_IN$, TEMP_IN%)
			TEMP_IN% = TEMP_IN% + 1%
		END SELECT

		!
		! Look for another character needing conversion
		!
		GOTO TexLoop
	END IF

	!
	! Return converted string
	!
	TK_FUNC_TEXSTRING = TEMP_IN$

32767	END FUNCTION
