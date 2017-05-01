1	%TITLE "Extract a File from a Text Library"
	%SBTTL "LIBR_EXTRACTVAR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG LIBR_EXTRACTVAR(LIB_NAME$, OUT_TEXT$, KEY_NAME$)

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
	!	it into a specified text string.  All text is run
	!	together into one string.
	!
	! Parameter:
	!
	!	LIB_NAME$
	!		Passed name of the library to pull text from.
	!
	!	OUT_TEXT$
	!		Outgoing text.
	!		Contains '13'C where a line break is required.
	!
	!	KEY_NAME$
	!		Passed key for text to be pulled.
	!
	!
	!	Returns a status code.
	!
	! Example:
	!
	!	ST% = LIBR_EXTRACTVAR("HELP_GL", TEXT$, "ADDRESS$NOTE")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:LIBR_EXTRACTVAR/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP LIBR_EXTRACTVAR
	!	$ DELETE LIBR_EXTRACTVAR.OBJ;*
	!
	! Author:
	!
	!	07/28/87 - Kevin Handy
	!
	! Modification history:
	!
	!	08/06/87 - Kevin Handy
	!		Modified to handle the "+" and "-" flags put
	!		in the library by LIN_INSERT_TEXT.
	!
	!	07/19/88 - Kevin Handy
	!		Removed defaulting to REF:
	!
	!	11/30/88 - Kevin Handy
	!		Modified to always goto CloseFile after the
	!		initilization function has executed.
	!
	!	06/16/89 - Kevin Handy
	!		Modified so would fit into sharable library.
	!
	!	03/13/92 - Kevin Handy
	!		Clean up vars (checkvar)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/15/99 - Kevin Handy
	!		Lose commented out code
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

	LIBR_EXTRACTVAR = 1%
	OUT_TEXT$ = ""

100	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(LR.INDEX%, LBR$C_READ)

	IF (ST% AND 1%) = 0%
	THEN
 !		CALL ENTR_3MESSAGE(SCOPE, "Unable to initilize library", 0%)
		LIBR_EXTRACTVAR = ST%
		EXIT FUNCTION
	END IF

200	!
	! Open the library function
	!
	ST% = LBR$OPEN(LR.INDEX%, LIB_NAME$, , ".TLB")

	IF (ST% AND 1%) = 0%
	THEN
		LIBR_EXTRACTVAR = ST%
		GOTO CloseLibrary
	END IF

300	!
	! Search for key in file
	!
	ST% = LBR$LOOKUP_KEY(LR.INDEX%, KEY_NAME$, TXRFA)

	IF (ST% AND 1%) = 0%
	THEN
		LIBR_EXTRACTVAR = ST%
		GOTO CloseLibrary
	END IF

400	!
	! Open up output file
	!
	TEMP_TEXT$ = ""

500	!
	! Copy over text
	!
 Loop:
	TEXT$ = SPACE$(132%)
	ST% = LBR$GET_RECORD(LR.INDEX%, TEXT$)

	IF (ST% AND 1%) = 1%
	THEN
		TEMP% = LEN(TRM$(TEXT$))
		IF MID(TEXT$, TEMP%, 1%) = "+"
		THEN
			!
			! There was a "+" meaning append this line to next.
			!
			TEMP_TEXT$ = TEMP_TEXT$ + LEFT(TEXT$, TEMP% - 1%)
		ELSE
			IF MID(TEXT$, TEMP%, 1%) = "-"
			THEN
				!
				! There was a "-" meaning the end of the line.
				!
				TEMP_TEXT$ = TEMP_TEXT$ + &
					LEFT(TEXT$, TEMP% - 1%) + '13'C
			ELSE
				!
				! Must not have been put there with the
				! LIN_INSERT_TEXT function.  Handle anyway.
				!
				TEMP_TEXT$ = TEMP_TEXT$ + &
					LEFT(TEXT$, TEMP%) + '13'C
			END IF
		END IF
		GOTO Loop
	END IF

	OUT_TEXT$ = TEMP_TEXT$

1000	!
	! Close library file
	!
 CloseLibrary:
	ST% = LBR$CLOSE(LR.INDEX%)

	END FUNCTION
