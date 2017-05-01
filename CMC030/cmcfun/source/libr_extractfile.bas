1	%TITLE "Extract a File from a Text Library"
	%SBTTL "LIBR_EXTRACTFILE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG LIBR_EXTRACTFILE(LIB_NAME$, &
		FILE_NAME.CH%, KEY_NAME$, FLAG%)

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
	!	This function pulls text out of a library, and puts
	!	it into a specified text file.
	!
	! Parameter:
	!
	!	LIB_NAME$
	!		Passed name of the library to pull text from.
	!
	!	FILE_NAME_CH%
	!		Passed channel of file to write text into.
	!
	!	KEY_NAME$
	!		Passed key for text to be pulled.
	!
	!	FLAG%
	!		1 - Drop first line that does not start with a dot (.)
	!
	!	Returns a status code.
	!
	! Example:
	!
	!	ST% = LIBR_EXTRACTFILE("HELP_GL", "TEXT.FILE", "ADDRESS")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:LIBR_EXTRACTFILE/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP LIBR_EXTRACTFILE
	!	$ DELETE LIBR_EXTRACTFILE.OBJ;*
	!
	! Author:
	!
	!	07/01/87 - Kevin Handy
	!
	! Modification history:
	!
	!	07/19/88 - Kevin Handy
	!		Removed defaulting to REF:
	!
	!	11/30/88 - Kevin Handy
	!		Modified to close library if open fails.
	!
	!	03/14/92 - Kevin Handy
	!		Clean vars (checkvar)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/14/95 - Kevin Handy
	!		Some clean up of the source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	DECLARE RFA TXRFA

	%PAGE

	LIBR_EXTRACTFILE = 1%

	IF FLAG% AND 1%
	THEN
		NONDOT.FLAG% = 0%
	ELSE
		NONDOT.FLAG% = -1%
	END IF

100	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(LR.INDEX%, LBR$C_READ)

	IF (ST% AND 1%) = 0%
	THEN
 !		CALL ENTR_3MESSAGE(SCOPE, "Unable to initilize library", 0%)
		LIBR_EXTRACTFILE = ST%
		EXIT FUNCTION
	END IF

200	!
	! Open the library function
	!
	ST% = LBR$OPEN(LR.INDEX%, LIB_NAME$, , ".TLB")

	IF (ST% AND 1%) = 0%
	THEN
		LIBR_EXTRACTFILE = ST%
		GOTO CloseLibrary
	END IF

300	!
	! Search for key in file
	!
	ST% = LBR$LOOKUP_KEY(LR.INDEX%, KEY_NAME$, TXRFA)

	IF (ST% AND 1%) = 0%
	THEN
		LIBR_EXTRACTFILE = ST%
		GOTO CloseLibrary
	END IF

500	!
	! Copy over text
	!
 Loop:
	TEXT$ = SPACE$(132%)
	ST% = LBR$GET_RECORD(LR.INDEX%, TEXT$)

	IF (ST% AND 1%) = 1%
	THEN
		!
		! Handle skipping runoff title
		!
		IF (NONDOT.FLAG% = 0%) AND (LEFT(TEXT$, 1%) <> ".")
		THEN
			!
			! Don't dump out line, and turn off skipping
			!
			NONDOT.FLAG% = -1%
		ELSE
			WHEN ERROR IN
				PRINT #FILE_NAME.CH%, TRM$(TEXT$)
			USE
				LIBR_EXTRACTFILE = -ERR
				CONTINUE CloseLibrary
			END WHEN
		END IF
		GOTO Loop
	END IF

1000	!
	! Close library file
	!
 CloseLibrary:
	ST% = LBR$CLOSE(LR.INDEX%)

	EXIT FUNCTION

	END FUNCTION
