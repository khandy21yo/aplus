1	%TITLE "Displays Position Where Bit Is 1%"
	%SBTTL "FUNC_BITSTRING"
	%IDENT "V3.6a Calico"

	FUNCTION STRING FUNC_BITSTRING(BITT%, BYTEE$, LENN%, XX$)

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
	!	This function displays selected character where bit is 1
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	BITT%
	!		Passed variable holding number of bits in ASCII code
	!
	!	BYTEE$
	!		A passed string holding the bytes.
	!
	!	LENN%
	!		The passed length of output string
	!
	!	XX$
	!		Passed displayed character.
	!
	!	Returned value
	!	 A string containing a character for position.
	!
	! Example:
	!
	!	A$ = FUNC_BITSTRING("AB", 10%, '1')
	!	A$ = (' 1     1 1    1 ')
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:FUNC_BITSTRING/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP FUNC_BITSTRING
	!	$ DELETE FUNC_BITSTRING.OBJ;*
	!
	! Author:
	!
	!	7/30/87 - Frank Starman
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

	EXTERNAL INTEGER FUNCTION READ_BIT

	XPRINT$ = ""

	FOR I% = 1% TO LENN%
		XBIT%    = READ_BIT(BITT%, BYTEE$, I%)
		XPRINT$  = XPRINT$ + XX$   IF XBIT% = 1%
		XPRINT$  = XPRINT$ + " "   IF XBIT% = 0%
	NEXT I%

	FUNC_BITSTRING = XPRINT$

	END FUNCTION
