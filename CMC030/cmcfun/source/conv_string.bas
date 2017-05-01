1	%TITLE "Convert String to LSET or RSET"
	%SBTTL "CONV_STRING"
	%IDENT "V3.6a Calico"

	FUNCTION STRING CONV_STRING(STRING STRING_IN, LONG FLAG)

	!
	!		COPYRIGHT (C) 1992 BY
	!		Computer Management Center, Idaho Falls, Idaho.
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
	!	This function converts string input to LSET or RSET.
	!
	! Index:
	!
	! Parameters:
	!
	!	STRING_IN
	!		This passed string is the string the user wants to have
	!		fomatted.
	!
	!	Returned value
	!		This function outputs the string in its
	!		new format.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:CONV_STRING/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP CONV_STRING
	!	$ DELETE CONV_STRING.OBJ;*
	!
	! AUTHOR:
	!
	!	03/04/92 - Dan Perkins
	!
	! MODIFICATION HISTORY:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/18/95 - Kevin Handy
	!		Include FUNCTION.HB instead of CONSTANTS.INC
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	DECLARE STRING INTERNAL, SPAC

	!
	! Reformat as necessary
	!
	INTERNAL = EDIT$(STRING_IN, 8% + 128%)

	SPAC = SPACE$(LEN(STRING_IN) - LEN(INTERNAL))

	SELECT FLAG
		CASE CMC$_LEFT
			INTERNAL = INTERNAL + SPAC

		CASE CMC$_RIGHT
			INTERNAL = SPAC + INTERNAL

		CASE ELSE
			INTERNAL = STRING_IN

	END SELECT

	CONV_STRING = INTERNAL

	END FUNCTION
