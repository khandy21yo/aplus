1	%TITLE "Change a Bit in a String"
	%SBTTL "FUNC_BITSWITCH"
	%IDENT "V3.6a Calico"

	FUNCTION STRING FUNC_BITSWITCH(BITT%, BYTEE$, POSITION%)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center
	!	Idaho Falls, Idaho
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
	! Computer Management Center
	!
	! Computer Management Center assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Computer Management Center.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function changes a selected bit and returns a new string
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	BYTEE$
	!		A passed string holding the bytes.
	!
	!	POSITION%
	!		The passed position of bit in the string
	!
	!	BIT%
	!		Passed number of bits in ASCII code
	!
	!	Returned value
	!		Either 0 or 1.
	!
	! Example:
	!
	!	FB$ = FUNC_BITSWITCH$("GB", 8%, 8%)
	!	FB$='FB'
	!	'GB' is in 8-Bit ASCII code = '0100011101000010'
	!	'FB' is in 8-Bit ASCII code = '0100011001000010'
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:FUNC_BITSWITCH/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP FUNC_BITSWITCH
	!	$ DELETE FUNC_BITSWITCH.OBJ;*
	!
	! Author:
	!
	!	7/16/87 - Frank Starman
	!
	! Modification history:
	!
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

	!
	! Pull off position of a  byte
	!
	BYTEE% = INT((POSITION% - 1%) / BITT%) + 1%

	!
	! Pull off position of the bit
	!
	BIT.POS% = BITT% * BYTEE% - POSITION%
	CDE%     = ASCII(MID(BYTEE$, BYTEE%, 1%)) XOR 2%^BIT.POS%
	BYTEE$   = LEFT(BYTEE$, BYTEE% - 1%) + CHR$(CDE%) + &
		RIGHT(BYTEE$, BYTEE% + 1%)

	BYTEE$   = STRING$(LEN(BYTEE$), 0%) IF POSITION% = 0%

	FUNC_BITSWITCH = BYTEE$

	END FUNCTION
