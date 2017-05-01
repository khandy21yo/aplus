1	%TITLE "Function to Round Numbers"
	%SBTTL "FUNC_ROUNDLOW"
	%IDENT "V3.6a Calico"

	FUNCTION REAL FUNC_ROUNDLOW(XNUM, XPREC%)

	!
	!	COPYRIGHT (C) 2004 BY
	!	Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use or
	! reliability of its software on equipment which is not supported
	! by Software Solutions, Inc.
	!
	!++
	!
	! Abstract:HELP
	!	.p
	!	This function rounds off the numbers the user asks for.
	!	This version rounds downward at 1/2, where the regular
	!	FUNC_ROUND would round up.
	!
	! Parameters:
	!
	!	XNUM
	!		The passed number the user wants to have rounded off.
	!
	!	XPREC%
	!		The passed precision off the rounded off number.
	!
	!
	!	This function returns the number the user entered as it
	!	looks rounded off.
	!
	! Example:
	!
	!	ROUND = FUNC_ROUNDLOW( 3.14159, 3%)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:FUNC_ROUNDLOW/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP FUNC_ROUNDLOW
	!	$ DELETE FUNC_ROUNDLOW.OBJ;*
	!
	! AUTHOR:
	!
	!	08/10/2004 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	WHEN ERROR IN
		FUNC_ROUNDLOW = 10.0 ^ (-XPREC%) * &
			INT(ABS(XNUM) * &
			(10.0 ^ XPREC%) + 0.4999) * SGN(XNUM)
	USE
		!
		! Probibly an overflow error. Give them something
		! that they could possibly use anyway
		!
		FUNC_ROUNDLOW = XNUM
	END WHEN

	END FUNCTION
