1	%TITLE "Search for Match Within String and Return Value"
	%SBTTL "FIND_STRING"
	%IDENT "V3.6a Calico"

	FUNCTION STRING FIND_STRING(STRING STRG, STRING MATCH)

	!
	! COPYRIGHT (C) 1986 BY
	! Computer Management Center
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
	!	This subroutine will search a string for a match
	!	and return the value for that match. The format is
	!	'<<match>value>'
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	STRG
	!		A passed string that is being searched for.
	!
	!	MATCH
	!		The passed match value for the string.
	!
	!
	!	The returning value is the value for the match.
	!
	! Example:
	!
	!	FIND_STRING('GOLD','OLD')
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:FIND_STRING/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP FIND_STRING
	!	$ DELETE FIND_STRING.OBJ;*
	!
	! AUTHOR:
	!
	!	09/01/87 - Robert Peterson
	!
	! MODIFICATION HISTORY:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	FIND_STRING = ""

	TEMP% = INSTR(1%, STRG, "<" + MATCH + ">")
	IF TEMP%
	THEN
		TEMP% = TEMP% + LEN(MATCH) + 2%
		TEMP1% = INSTR(TEMP%, STRG, ">")
		IF TEMP1%
		THEN
			FIND_STRING = EDIT$(SEG$(STRG, TEMP%, TEMP1% - 1%), 4%)
		END IF
	END IF

	END FUNCTION
