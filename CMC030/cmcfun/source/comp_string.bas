1	%TITLE "String Comparison Function"
	%SBTTL "COMP_STRING"
	%IDENT "V3.6a Calico"

	FUNCTION INTEGER COMP_STRING(TEST_STRING$, WILDCARD_STRING$)

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
	!	.b
	!	.lm +5
	!	This function compares a string to a pattern.
	!	It returns 0 if there is no match, and -1
	!	if there is.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	TEST_STRING$
	!		The passed string to be tested.
	!
	!	WILDCARD_STRING$
	!		The passed pattern to compare against.
	!		This value can be anything that is allowed in the
	!		"Wildcard" as defined in other manuels.
	!		('*'s, ",", "/", "?", ...)
	!
	!	Returned value
	!		An integer value containing the result of the
	!		comparison.
	!		(0 if match fails and -1 it succeeds.)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:COMP_STRING/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP COMP_STRING
	!	$ DELETE COMP_STRING.OBJ;*
	!
	! AUTHOR:
	!
	!	11/11/85 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	04/27/88 - Kevin Handy
	!		Modified to use the VAX function STR$MATCH_WILD
	!		which gives a much nicer range of matching
	!		(as well as being more consistant with the
	!		rest of VMS wildcards)
	!
	!	05/05/88 - Kevin Handy
	!		Modified to trim compare string, so that it will
	!		work like it did originally.
	!
	!	09/09/91 - Kevin Handy
	!		Modify documentation of function.
	!
	!	03/14/92 - Kevin Handy
	!		Clean up vars (checkvar)
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
	!
	!	10/22/98 - Kevin Handy
	!		A blank wildcard never matches, so skip
	!		out quickly. (Since this is such a common
	!		occurance, this should speed up some things)
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET fro STR$ routines
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$STRDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Trim the test string
	!
	TEST_STRINGA$ = TRM$(TEST_STRING$)

	IS_MATCH% = 0%			! No match
	GOTO L3000 IF WILDCARD_STRING$ == ""

	IS_MATCH% = -1% IF WILDCARD_STRING$ = "*"	! Yes match all
	STR2$ = TRM$(WILDCARD_STRING$) + ","

 L1000:	IF IS_MATCH% = 0%
	THEN
		!
		! Strip off one item to match
		!
		N_ITEM$ = LEFT(STR2$, INSTR(1%, STR2$, ",") - 1%)
		STR2$ = RIGHT(STR2$, LEN(N_ITEM$) + 2%)

		!
		! A slash is used for from-to, but if there is a wildcard
		! character in it, it is ignored.
		!
		SLASH% = INSTR(1%, N_ITEM$, "/")
		SLASH% = 0% IF INSTR(1%, N_ITEM$, "?") OR &
			INSTR(1%, N_ITEM$, "%") OR &
			INSTR(1%, N_ITEM$, "*")

		!
		! Handle as either from/to, or as a comparison string
		!
		IF SLASH%
		THEN
			!
			! From/to
			!
			IS_MATCH% = LEFT(N_ITEM$, SLASH% - 1%) <= TEST_STRINGA$ &
				AND TEST_STRINGA$ <= RIGHT(N_ITEM$, SLASH% + 1%)
		ELSE
			!
			! Change all question marks (?) to percent signes (%)
			! so that old type of comparison from RSTS/E will
			! still work.
			!
			FIXUP% = INSTR(1%, N_ITEM$, "?")
			WHILE FIXUP%
				N_ITEM$ = LEFT(N_ITEM$, FIXUP% - 1%) + "%" + &
					RIGHT(N_ITEM$, FIXUP% + 1%)
				FIXUP% = INSTR(1%, N_ITEM$, "?")
			NEXT

			IS_MATCH% = -1% &
			IF (STR$MATCH_WILD(TEST_STRINGA$, N_ITEM$) = STR$_MATCH)

		END IF
	END IF

 L2000:
	GOTO L1000 IF STR2$ <> "" AND IS_MATCH% = 0%

 L3000:
	COMP_STRING = IS_MATCH%

	END FUNCTION
