1	%TITLE "Array Comparison Function"
	%SBTTL "COMP_ARRAY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG COMP_ARRAY(TEST_STRING$, MATCH_ARRAY$)

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
	!	MATCH_ARRAY$
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
	!	$ BAS FUNC_SOURCE:COMP_ARRAY
	!	$ LIB FUNC_LIB:CMCFUN/REP COMP_ARRAY
	!	$ DELETE COMP_ARRAY.OBJ;*
	!
	! AUTHOR:
	!
	!	11/24/92 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Modified to include FUNCTION.HB.
	!
	!	11/20/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	DECLARE LONG EXIT_STATUS

	COM (CH_COMP_ARRAY) COMP_ARRAY.CH%

	EXIT_STATUS = 0%

1000	IF COMP_ARRAY.CH% <= 0%
	THEN
		!
		! Declare channels
		!
		CALL ASSG_CHANNEL(COMP_ARRAY.CH%, STAT%)

		!
		! Open input file
		!
		WHEN ERROR IN
			OPEN EDIT$(MATCH_ARRAY$, -1%) + ".WLD" &
				FOR INPUT AS FILE COMP_ARRAY.CH%, &
				ORGANIZATION SEQUENTIAL, &
				RECORDSIZE 128%
		USE
			CONTINUE SingleString
		END WHEN
	END IF

	WHEN ERROR IN
		RESET #COMP_ARRAY.CH%
	USE
		CALL ASSG_FREECHANNEL(COMP_ARRAY.CH%)
		COMP_ARRAY.CH% = 0%
		CONTINUE SingleString
	END WHEN

1010	WHEN ERROR IN
		INPUT LINE #COMP_ARRAY.CH%, ONELINE$
	USE
		CONTINUE SingleString
	END WHEN

	EXIT_STATUS = COMP_STRING(TEST_STRING$, EDIT$(ONELINE$, -1%))

	GOTO 1010 IF EXIT_STATUS <> -1%

 ExitFunction:
	COMP_ARRAY = EXIT_STATUS

	EXIT FUNCTION

 SingleString:
	EXIT_STATUS = COMP_STRING(TEST_STRING$, MATCH_ARRAY$)
	GOTO ExitFunction

	END FUNCTION
