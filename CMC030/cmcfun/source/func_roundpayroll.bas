1	%TITLE "Function to Round Numbers"
	%SBTTL "FUNC_ROUNDPAYROLL"
	%IDENT "V3.6a Calico"

	FUNCTION REAL FUNC_ROUNDPAYROLL(XNUM, METHOD$)

	!
	!	COPYRIGHT (C) 2000 BY
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
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
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
	!	METHOD
	!		The passed rounding method.
	!		"*U" Round up to whole Dollar.
	!		"*D" Round down to whole Dollar.
	!		"*N" Round to nearest whole Dollar.
	!		Otherwise use normal two digit rounding.
	!
	!	This function returns the number the user entered as it
	!	looks rounded off.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:FUNC_ROUNDPAYROLL/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP FUNC_ROUNDPAYROLL
	!	$ DELETE FUNC_ROUNDPAYROLL.OBJ;*
	!
	! AUTHOR:
	!
	!	05/16/2000 - Kevin Handy
	!		Loosely based on FUNC_ROUND code
	!
	! MODIFICATION HISTORY:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	WHEN ERROR IN
		SELECT METHOD$
		!
		! Round up to whole dollar
		!
		CASE "U"
			FUNC_ROUNDPAYROLL = &
				INT(ABS(XNUM) + 0.9999) * SGN(XNUM)

		!
		! Round Down to whole dollar
		!
		CASE "D"
			FUNC_ROUNDPAYROLL = &
				FIX(ABS(XNUM)) * SGN(XNUM)

		!
		! Round to nearest dollar
		!
		CASE "N"
			FUNC_ROUNDPAYROLL = &
				FIX(ABS(XNUM) + 0.5001) * SGN(XNUM)

		!
		! Normal two digit rounding
		!
		CASE ELSE
			FUNC_ROUNDPAYROLL = INT(ABS(XNUM) * &
				(100.0) + 0.5001) * SGN(XNUM) / 100.0
		END SELECT
	USE
		!
		! Probibly an overflow error. Give them something
		! that they could possibly use anyway
		!
		FUNC_ROUNDPAYROLL = XNUM
	END WHEN

	END FUNCTION
