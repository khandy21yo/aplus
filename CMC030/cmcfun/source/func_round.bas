1	%TITLE "Function to Round Numbers"
	%SBTTL "FUNC_ROUND"
	%IDENT "V3.6a Calico"

	FUNCTION REAL FUNC_ROUND(XNUM, XPREC%)

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
	!	This function rounds off the numbers the user asks for.
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
	!	ROUND = FUNC_ROUND( 3.14159, 3%)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:FUNC_ROUND/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP FUNC_ROUND
	!	$ DELETE FUNC_ROUND.OBJ;*
	!
	! AUTHOR:
	!
	!	08/19/85 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	08/17/90 - Kevin Handy
	!		Removed (XPREC% * 1.0) in two places to gain a little
	!		bit of speed.  The multiply doesn't seem to do
	!		anything usefull.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	03/16/99 - Kevin Handy
	!		Use WHEN ERROR
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	WHEN ERROR IN
		FUNC_ROUND = 10.0 ^ (-XPREC%) * &
			INT(ABS(XNUM) * &
			(10.0 ^ XPREC%) + 0.5001) * SGN(XNUM)
	USE
		!
		! Probibly an overflow error. Give them something
		! that they could possibly use anyway
		!
		FUNC_ROUND = XNUM
	END WHEN

	END FUNCTION
