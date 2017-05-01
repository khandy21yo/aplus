1	%TITLE "Generate the Banner Code"
	%SBTTL "FUNC_BANNERCODE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG FUNC_BANNERCODE(STRING LICENSE)

	!
	! COPYRIGHT (C) 1989 BY
	! Computer Management Center
	!
	! Idaho Falls, Idaho.
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
	!	This function will generate a number using the given
	!	expiration date and license number for use in the Banner
	!	of the menu.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	DDATE$	- The passed expiration date.
	!
	!	LNUM$	- The passed license number.
	!
	!	The return is a long number.
	!
	! Example:
	!
	!	FUNC_BANNERCODE("19890811","1234")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:FUNC_BANNERCODE/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP FUNC_BANNERCODE
	!	$ DELETE FUNC_BANNERCODE.OBJ;*
	!
	! AUTHOR:
	!
	!	08/14/89 - B. Craig Larsen
	!
	! MODIFICATION HISTORY:
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
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
	!--
	%PAGE

	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	DECLARE STRING FIRST_HALF
	DECLARE STRING SECOND_HALF
	DECLARE WORD   HALF_LENGTH
	DECLARE LONG   CONTR_NUMBER

	HALF_LENGTH = LEN(EDIT$(LICENSE, -1%)) / 2%
	FIRST_HALF = LEFT(EDIT$(LICENSE, -1%), HALF_LENGTH) + &
		SPACE$(10% - HALF_LENGTH)
	SECOND_HALF = RIGHT(EDIT$(LICENSE, -1%), HALF_LENGTH + 1%) + &
		SPACE$(10% - HALF_LENGTH)

	!
	! Generate the Control Number
	!
	FOR I% = 1% TO 10%
		CONTR_NUMBER = CONTR_NUMBER + &
			ASCII(MID(FIRST_HALF, I%, 1%)) * 3%
		CONTR_NUMBER = CONTR_NUMBER + &
			ASCII(MID(SECOND_HALF, I%, 1%)) + 3%
		CONTR_NUMBER = CONTR_NUMBER * 3%
	NEXT I%

	FUNC_BANNERCODE = CONTR_NUMBER

	END FUNCTION
