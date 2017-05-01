1	%TITLE "Generate a date and a time given a batch number"
	%SBTTL "ASSG_UNMAKEBATCH"
	%IDENT "V3.6a Calico"

	SUB ASSG_UNMAKEBATCH(BATCH$, GIVEN_DATE$, GIVEN_TIME$)

	!
	! COPYRIGHT (C) 1994 BY
	! Computer Management Center
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
	!	This functon will generate
	!	a date and time
	!	when given a batch number.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	BATCH$
	!		This is the batch number to generate a date and time
	!		for.
	!
	!	GIVEN_DATE$
	!		This is the date returned from the batch number,
	!		in YYYYMMDD format.
	!
	!	GIVEN_TIME$
	!		This is the time to generate the batch number for,
	!		in HHMMSS format.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ASSG_UNMAKEBATCH
	!	$ LIB FUNC_LIB:CMCFUN/REP ASSG_UNMAKEBATCH
	!	$ DELETE ASSG_UNMAKEBATCH.OBJ;*
	!
	! Author:
	!
	!	09/28/94 - Kevin Handy
	!
	! Modification History:
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
	! External functions
	!
	EXTERNAL STRING	FUNCTION DATE_TODAY
	EXTERNAL LONG	FUNCTION DATE_DAYCODE
	EXTERNAL STRING	FUNCTION DATE_INVDCODE
	EXTERNAL STRING	FUNCTION TIME_INVCODE

	!
	! Define the RADIX and the characters used to print that RADIX.
	! Note that some characters are missing, because trying to
	! identify them can be hard from a printout, i.e. (0 and O,
	! 1 and I)
	!
	CHARACTER$ = "23456789ABCDEFGHJKLMNPQRSTUVWXYZ"
	CHAR% = LEN(CHARACTER$)

	!
	! Calculate the number of seconds in a single cycle
	!
	CYCLE = (CHAR% ^ 6.0)

	!
	! Convert the given batch number back into a numeric value
	!
	BUILDUP = 0.0

	FOR I% = 1% TO LEN(BATCH$)
		BUILDUP = BUILDUP * CHAR% + &
			INSTR(1%, CHARACTER$, MID(BATCH$, I%, 1%)) - 1%
	NEXT I%

	BUILDUP = BUILDUP - 1%

	!
	! Close approximation of a batch number that would be assigned
	! for tomorrow. Use tomorrow so can calculate on batches created
	! today.
	!
	TODAYBATCH = (DATE_DAYCODE(DATE_TODAY) + 1.0) * (24.0 * 60.0 * 60.0)

	!
	! Calculate which cycle it is likely to be in. We may guess one
	! cycle too far, so adjust back one cycle if necessary.
	!
	WHATCYCLE = FIX(TODAYBATCH / CYCLE + .0001)

	U1 = (BUILDUP + WHATCYCLE * CYCLE)

	IF (U1 > TODAYBATCH)
	THEN
		U1 = U1 - CYCLE
	END IF

	!
	! Generate a daycode and timecode value for that date
	!
	U1% = FIX(U1 / (24.0 * 60.0 * 60.0) + .00001)
	U2% = FIX(U1 - U1% * (24.0 * 60.0 * 60.0) + .5)

	!
	! Return values back
	!
	GIVEN_DATE$ = DATE_INVDCODE(U1%)
	GIVEN_TIME$ = TIME_INVCODE(U2%)

	END SUB
