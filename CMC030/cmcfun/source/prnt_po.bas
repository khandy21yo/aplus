1	%TITLE "Format Social Security Number"
	%SBTTL "PRNT_PO"
	%IDENT "V3.6a Calico"

	FUNCTION STRING PRNT_PO(PO_IN$, FLAG%)

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
	!	.b
	!	.lm +5
	!	This function formats a PO number.
	!	The flag is not used at this time.
	!	.b
	!	This function is obsolete and should not be used.
	!	It is only retained because it is included in the
	!	shared library.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	PO_IN$
	!		This passed string is the number the user wants to have
	!		fomatted.
	!
	!	Returned value
	!		This function outputs the PO number in its
	!		new format.
	!
	! Example:
	!
	!	PO$ = PRNT_PO("0000009901", 0%)
	!	-- PO$ = "99-01      "
	!
	!	PO% = PRNT_PO("0000009901", 1%)
	!	-- PO$ = "      99-01"
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:PRNT_PO/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP PRNT_PO
	!	$ DELETE PRNT_PO.OBJ;*
	!
	! AUTHOR:
	!
	!	07/05/90 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	02/05/92 - Dan Perkins
	!		Strip off leading zeros.
	!
	!	02/06/92 - Dan Perkins
	!		Use flag value of zero to LSET PO in field.
	!		Use flag value of one to RSET PO in field.
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
	! Reformat as necessary
	!
	IF PO_IN$ = ""
	THEN
		PO$ = PO_IN$ + " "
	ELSE
		PO$ = LEFT(PO_IN$, 8%) + "-" + &
			RIGHT(PO_IN$, 9%)

		WHILE LEFT(PO$, 1%) = "0"
			PO$ = RIGHT(PO$, 2%)
		NEXT

		SP$ = SPACE$(11% - LEN(PO$))

		SELECT FLAG%
			CASE 0%
				PO$ = PO$ + SP$

			CASE 1%
				PO$ = SP$ + PO$
		END SELECT

	END IF

	PRNT_PO = PO$

	END FUNCTION
