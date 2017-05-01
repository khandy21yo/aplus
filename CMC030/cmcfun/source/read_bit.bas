1	%TITLE "Returns Which Bit Is Lit in a String"
	%SBTTL "READ_BIT"
	%IDENT "V3.6a Calico"

	FUNCTION INTEGER READ_BIT(BITT%, BYTEE$, POSITION%)

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
	!	.p
	!	This function returns bit values.
	!
	! Parameters:
	!
	!	BYTEE$
	!		A passed string holding the byte values.
	!
	!	POSITION%
	!		The passed position of bit in the string
	!
	!	BITT%
	!		Passed number of bits in ASCII code
	!
	!	Returned value
	!		Returns a value of 0 or 1.
	!
	! Example:
	!
	!	A% = READ_BIT%("AB", 10%, 8%)
	!	A% = 1% ("AB" is in 8-BIT ASCII code = '0100000101000010')
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:READ_BIT/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP READ_BIT
	!	$ DELETE READ_BIT.OBJ;*
	!
	! Author:
	!
	!	7/13/87 - Frank Starman
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
	CDE% = ASCII(MID(BYTEE$, BYTEE%, 1%)) AND 2% ^ BIT.POS%

	READ_BIT = SGN(CDE%)

	END FUNCTION
