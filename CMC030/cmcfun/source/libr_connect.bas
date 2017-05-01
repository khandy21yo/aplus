1	%TITLE "Insert a Text File into a Library"
	%SBTTL "LIBR_CONNECT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG LIBR_CONNECT(LIB_NAME$, NEW_KEY$, OLD_KEY$)

	!
	!	COPYRIGHT (C) 1987 BY
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
	!	This function will enter a new key into a library,
	!	making that key point to the same text as an
	!	existing key.
	!	The new Key must not exist in the file already.
	!	If it is, it will be ignored.
	!
	! Index:
	!
	! Parameters:
	!
	!	LIB_NAME$
	!		The passed name of the library file to use.
	!
	!	NEW_KEY$
	!		The passed key that is being added to the file.
	!
	!	OLD_KEY$
	!		The passed key that already exists.
	!
	!
	!	Returns a status code.
	!
	! Example:
	!
	!	ST% = LIBR_CONNECT("HELP_GL", "NEWKEY", "OLDKEY")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:LIBR_CONNECT/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP LIBR_CONNECT
	!	$ DELETE LIBR_CONNECT.OBJ;*
	!
	! Author:
	!
	!	01/07/87 - Kevin Handy
	!
	! Modification history:
	!
	!	07/19/88 - Kevin Handy
	!		Removed defaulting to REF:
	!
	!	11/30/88 - Kevin Handy
	!		Modified to close library if open fails.
	!
	!	06/16/89 - Kevin Handy
	!		Modified so would work in sharable library.
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
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"

	!
	! Declare variables
	!
	DECLARE RFA TEST.RFA

	%PAGE

	LIBR_CONNECT = 1%

100	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(L.INDEX%, LBR$C_UPDATE)

	IF (ST% AND 1%) = 0%
	THEN
 !		CALL ENTR_3MESSAGE(SCOPE, "Unable to initilize library", 0%)
		LIBR_CONNECT = ST%
		EXIT FUNCTION
	END IF

200	!
	! Open the library function
	!
	ST% = LBR$OPEN(L.INDEX%, LIB_NAME$, , ".TLB")

	IF (ST% AND 1%) = 0%
	THEN
 !		CALL ENTR_3MESSAGE(SCOPE, "Unable to open library", 0%)
		LIBR_EXTRACT = ST%
		GOTO 900
	END IF

300	!
	! Search for new key
	!
	ST% = LBR$LOOKUP_KEY(L.INDEX%, NEW_KEY$, TEST.RFA BY REF)

	IF (ST% AND 1%) = 1%
	THEN
		!
		! This key already exists
		!
		LIBR_CONNECT = ST% XOR 1%	! Force it to be an error
		GOTO 900
	END IF

	!
	! Search for old key
	!
	ST% = LBR$LOOKUP_KEY(L.INDEX%, OLD_KEY$, TEST.RFA BY REF)

	IF (ST% AND 1%) = 0%
	THEN
		!
		! Unable to find it
		!
		LIBR_CONNECT = ST%
		GOTO 900
	END IF

	!
	! Connect new key
	!
	ST% = LBR$INSERT_KEY(L.INDEX%, NEW_KEY$, TEST.RFA BY REF)

	LIBR_CONNECT = ST%

900	!
	! Close all files
	!
	JUNK% = LBR$CLOSE(L.INDEX%)

32767	END FUNCTION
