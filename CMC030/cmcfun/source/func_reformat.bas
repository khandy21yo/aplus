1	%TITLE "Reformat a Number into a String"
	%SBTTL "FUNC_REFORMAT"
	%IDENT "V3.6a Calico"

	FUNCTION STRING FUNC_REFORMAT(REAL AMOUNT, &
		LONG STR_LEN, &
		LONG DEC)

	!
	!	COPYRIGHT (C) 1987 BY
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
	!	This function is used to reformat a number into a
	!	string with the decimal removed.  Note:  Amount is
	!	converted to a positive value.
	!
	! Index:
	!	.x Reformat
	!
	! Parameters:
	!
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:FUNC_REFORMAT
	!	$ LIB FUNC_LIB:CMCFUN/REP FUNC_REFORMAT
	!	$ DELETE FUNC_REFORMAT.OBJ;*
	!
	! Author:
	!
	!	12/29/88 - Robert Peterson
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/25/97 - Kevin Handy
	!		Clean up
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! External functions
	!
	EXTERNAL REAL	FUNCTION FUNC_ROUND

	TEMP = 10.^DEC * ABS(AMOUNT)

	AMOUNT$ = EDIT$(NUM1$(FUNC_ROUND(TEMP, 0%)), 128%)

	AMOUNT$ = "0" + AMOUNT$ UNTIL LEN(AMOUNT$) >= STR_LEN

	FUNC_REFORMAT = AMOUNT$

	END FUNCTION
