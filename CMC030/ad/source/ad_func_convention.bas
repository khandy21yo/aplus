1	%TITLE "Calculation Convention Coeff"
	%SBTTL "AD_FUNC_CONVENTION"
	%IDENT "V3.6a Calico"

	FUNCTION REAL AD_FUNC_CONVENTION( &
		STRING XCONVENTION, &
		INTEGER HALF.MONTH)

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
	! Computer Management Center, Inc.
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
	!	This subroutine reads the convention file.
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!	XCONVENTION = Depreciation convention code
	!
	! Output:
	!
	!	AD_FUNC_CONVENTION = Convention coefficient
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_FUNC_CONVENTION/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AD_FUNC_CONVENTION
	!	$ DELETE AD_FUNC_CONVENTION.OBJ;*
	!
	! Author:
	!
	!	12/19/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	05/28/91 - Frank F. Starman
	!		Change > to >= when testing case 'I'
	!
	!	03/14/92 - Kevin Handy
	!		Clean up vars (checkvar)
	!
	!	04/04/95 - Kevin Handy
	!		(V3.6)
	!		Update to version 3.6 level
	!
	!	08/27/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/19/97 - Kevin Handy
	!		Use integeres in #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/28/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (READ_AD_CONVENTION) AD_CONVENTION.CH%

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONVENTION.HB"
	MAP (AD_CONVENTION) AD_CONVENTION_CDD AD_CONVENTION

	DECLARE REAL	CONV, COEFF
	DECLARE INTEGER INDEX.MONTH

	CONV = HALF.MONTH

1000	IF AD_CONVENTION.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_CONVENTION.OPN"
		USE
			CONTINUE ExitFunc
		END WHEN
	END IF

2000	WHEN ERROR IN
		GET #AD_CONVENTION.CH%, KEY #0% EQ XCONVENTION, REGARDLESS
	USE
		CONTINUE ExitFunc
	END WHEN

	COEFF = 0.02 * AD_CONVENTION::COEFF

	GOTO ExitFunc IF COEFF = 0.0

	SELECT AD_CONVENTION::SPECIFIC

	CASE "B"
		!
		! Backward
		!
		CONV = COEFF * (INT(HALF.MONTH / COEFF) + 1%)

	CASE "F"
		!
		! Forward
		!
		CONV = COEFF * INT(HALF.MONTH / COEFF)

	CASE "I"
		!
		! Inward
		!
		INDEX.MONTH = HALF.MONTH
		WHILE INDEX.MONTH >= COEFF * 2%
			INDEX.MONTH = INDEX.MONTH - COEFF * 2%
		NEXT

		IF COEFF <= INDEX.MONTH
		THEN
			!
			! Forward
			!
			CONV = COEFF * INT(HALF.MONTH / COEFF)
		ELSE
			!
			! Backward
			!
			CONV = COEFF * (INT(HALF.MONTH / COEFF) + 1%)
		END IF

	CASE "O"
		!
		! Outward
		!
		INDEX.MONTH = HALF.MONTH
		WHILE INDEX.MONTH > COEFF * 2%
			INDEX.MONTH = INDEX.MONTH - COEFF * 2%
		NEXT

		IF COEFF <= INDEX.MONTH
		THEN
			!
			! Backward
			!
			CONV = COEFF * (INT(HALF.MONTH / COEFF) + 1%)
		ELSE
			!
			! Forward
			!
			CONV = COEFF * INT(HALF.MONTH / COEFF)
		END IF

	END SELECT

 ExitFunc:
	AD_FUNC_CONVENTION = CONV / 24.0

	END FUNCTION
